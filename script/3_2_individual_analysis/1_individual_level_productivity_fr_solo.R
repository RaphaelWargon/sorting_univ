rm(list = ls())

gc()
#install.packages('devtools')
library('pacman')
p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'dplyr','magrittr','tidyr','ggplot2'
       ,'binsreg',
       'DescTools',
       'cowplot',
       'did',
       'MatchIt',
       'boot'#,
       #'DIDmultiplegt',
       #"DIDmultiplegtDYN"#,'didimputation'
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}


inputpath <- "D:\\panel_fr_res\\data\\panel_smoothed.parquet"


ds <- open_dataset(inputpath) %>%
  filter(
    last_year-entry_year >2
    & entry_year >=1965 
    #& year >= 2003
    #& entry_year <=2003
    & !(is.na(field))&!is.na(city) & !is.na(type) 
  )%>% select(-inst_set_this_year)
ds <- as.data.table(ds)


reweight_cols <-  c('publications_raw', 
                    'citations_raw',
                    'nr_source_top_5pct_raw',
                    'nr_source_top_10pct_raw',
                    "nr_source_top_20pct_raw",
                    "nr_source_btm_50pct_raw",
                    "nr_source_mid_40pct_raw"
)

ds <- ds %>%
  .[, (str_replace(reweight_cols, '_raw','_reweight')) := lapply(.SD, function(x) x/n_inst_y), .SDcols = reweight_cols] %>%
  .[, n_lt_global := n_distinct(author_id), by = c('merged_inst_id', 'year')] %>%
  .[, prod_inst_2003:= max(sum(citations_reweight)/n_lt_global * as.numeric(year == 2003)), by = 'merged_inst_id'] %>%
  .[, size_2003:= max(n_lt_global * as.numeric(year == 2003)), by = 'merged_inst_id'] %>%
  .[, prod_au_first_2y := max(citations_reweight * as.numeric( as.numeric(as.character(year))<= entry_year +2 )), by = 'author_id']


ds <- ds %>%
  .[, ':='(prod_inst_n_tile = cut(prod_inst_2003, unique(quantile(unique(ds[, list(merged_inst_id, prod_inst_2003)])$prod_inst_2003,
                                                                  probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>%
  .[, ':='(prod_au_n_tile = cut(prod_au_first_2y, unique(quantile(unique(ds[, list(merged_inst_id, prod_au_first_2y)])$prod_au_first_2y,
                                                                  probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>%
  
  .[, ':='(size_n_tile = cut(size_2003, unique(quantile(unique(ds[, list(merged_inst_id, size_2003)])$size_2003,
                                                        probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>%
  .[, ":="(prod_inst_n_tile = ifelse(is.na(prod_inst_n_tile), 0, prod_inst_n_tile),
           prod_au_n_tile = ifelse(is.na(prod_au_n_tile), 0, prod_au_n_tile),
           size_n_tile = ifelse(is.na(size_n_tile), 0, size_n_tile)
  )]



inst <-  as.data.table(open_dataset("D:\\panel_fr_res\\inst_fr.parquet"))
gc()

orsay_to_recode <- c('I3019441195','I4210109968','I4210162675','I4210097733','I4210151467','I4210115729','I2799857465','I4210121618','I4210118056')

sample_df <- ds %>%
  ## without the brits
  .[country == "FR" & year >= 2003] %>%
  #### Checking that there are enough observations for each individual or author :
  .[, ':='(n_inst_id_sample = n_distinct(merged_inst_id)), by = 'author_id'] %>%
  .[, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
           n_authors_sample = n_distinct(author_id),
           n_y_sample_inst = n_distinct(year),
           n_obs_inst = .N), by = c('merged_inst_id','field')]%>%
  .[, acces_rce := ifelse(merged_inst_id %in% orsay_to_recode, "2010", acces_rce)] %>%
  .[, date_first_idex := ifelse(merged_inst_id %in% orsay_to_recode, 2012, date_first_idex)] %>%
  .[, fusion_date := ifelse(merged_inst_id %in% orsay_to_recode, 2019, fusion_date)] %>%
  .[, n_y_in_sample_au := n_distinct(year), by = 'author_id']%>%
  .[ n_authors_w_several_inst > 0 # ensure that the author and firm are part of the connected set
     & n_y_in_sample_au >=2 & n_authors_sample >1 & n_y_sample_inst>1] %>% #remove labs that are too poorly measured
  .[, ":="(acces_rce = ifelse(is.na(acces_rce),0,acces_rce),
           date_first_idex = ifelse(is.na(date_first_idex),0,date_first_idex),
           fusion_date = ifelse(is.na(fusion_date),0,fusion_date))] %>%
  .[, ':='(n_obs_au = .N), by = "author_id"]%>%
  .[, ':='(n_au_inst_id_field_y = .N), by = c('merged_inst_id', 'field','year')] %>%
  .[, min_n_au_inst_id_field_y := min(n_au_inst_id_field_y), by = c('merged_inst_id', "field")]%>%
  .[, pub_04_07 := sum(as.numeric(year > 2004 & year <= 2007) * publications_raw ), by = 'author_id'] %>%
  # .[pub_04_07 >=2] %>%
  .[, n_lt := n_distinct(author_id), by = c('merged_inst_id','field','year')] %>%
  .[, merged_inst_id_field := paste0(merged_inst_id, '_', field)] %>%
  .[, fusion_date := fifelse(fusion_date =="2023", "0", as.character(fusion_date))] %>%
  .[ , ':='(fusion_date = as.factor(fusion_date),
            date_first_idex = as.factor(date_first_idex),
            acces_rce = as.factor(acces_rce),
            year = as.factor(year))]

gc()
rm(ds)
gc()
length(unique(sample_df$author_id)) #386536
nrow(unique(sample_df[, list(merged_inst_id, field)])) #43576


ggplot(unique(sample_df[, list(author_id, n_obs_au,entry_year)]))+geom_density(aes(x=n_obs_au, group = entry_year,color = entry_year))



cities <- fread('D:\\panel_fr_res\\data\\v_commune_2025.csv')

# Staggered design regression to estimate treatment effects ---------------

####### Prepare dataset for staggered design regression

table(unique(sample_df[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$fusion_date)


sample_df_reg <- sample_df %>%
  .[, year_n := as.numeric(as.character(year))] %>%
  .[!(acces_rce %in%  c(2014, 2015))
    & !(date_first_idex %in% c(2014))
    & !(fusion_date %in% c(2012,2019))
    & !(str_detect(idex, "annulee"))] %>%
  .[, pub_04_07 := sum(as.numeric(year_n > 2004 & year_n <= 2007) * publications_raw ), by = 'author_id'] %>%
  .[pub_04_07 >=2] %>%
  .[str_count(field, ',')<=1]%>%
  .[, n_lt := n_distinct(author_id), by = c('merged_inst_id','field','year')] %>%
  .[, merged_inst_id_field := paste0(merged_inst_id, '_', field)] %>%
  .[, merged_inst_id_domain := paste0(merged_inst_id, '_', domain)] %>%
  .[, fusion_date := fifelse(fusion_date =="2023", "0", as.character(fusion_date))] %>%
  #.[year != "2020"] %>%
  .[ , ':='(fusion_date = as.factor(fusion_date),
            date_first_idex = as.factor(date_first_idex),
            acces_rce = as.factor(acces_rce),
            year = as.factor(year))]%>%
  .[, city:= case_when(city == "Saint-Etienne" ~ 'Saint-Étienne',
                       city == "St-Malo" ~ "Saint-Malo",
                       .default  = city
  )] %>% .[, gender := case_when(gender == 'M' ~0,
                                 gender == 'F' ~ 1)]
gc()

sample_df_reg <- merge(sample_df_reg, cities %>% .[, city:=LIBELLE], by ='city', allow.cartesian = TRUE)%>%
  .[!is.na(LIBELLE)]

outcomes <- c('publications_raw', 'publications_reweight',
              'citations_raw','citations_reweight',
              'nr_source_top_5pct_raw', 'nr_source_top_5pct_reweight',
              'nr_source_top_10pct_raw', 'nr_source_top_10pct_reweight'
)

sample_df_reg <-sample_df_reg %>%   .[, (outcomes) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes]
#fwrite(sample_df_reg, "D:\\panel_fr_res\\data\\sample_df_reg.csv" )
sample_df_reg <- fread( "D:\\panel_fr_res\\data\\sample_df_reg.csv" )

length(unique(sample_df_reg$author_id)) #128947
nrow(unique(sample_df_reg[, list(merged_inst_id, field)])) #30583
nrow(unique(sample_df_reg[, list(merged_inst_id, domain)])) #10391

table(unique(sample_df_reg[, list(merged_inst_id, domain, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df_reg[, list(merged_inst_id, domain, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df_reg[, list(merged_inst_id, domain, acces_rce,date_first_idex,fusion_date)])$fusion_date)

gc()
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))

save_path = paste0("D:\\panel_fr_res\\results\\productivity\\treatment_by_treatment\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

# Alternative : estimate treatments separately ----------------------------

all_treatments <- c('acces_rce','date_first_idex','fusion_date')
sample_df_reg <- sample_df_reg %>%
  .[, ':='(entry_cohort = floor(entry_year/5)*5) ]

unit_cols <- c('merged_inst_id','domain', 'name', 'author_id','author_name', "merged_inst_id_domain",
               'acces_rce', 'date_first_idex','fusion_date', 'field',
               'city', 'DEP', 'REG','type','public','ecole','cnrs', "entry_cohort",
               'size_n_tile','prod_inst_n_tile',
               'entry_year', 'prod_au_n_tile', 'gender'
)

test_data <- sample_df_reg %>%
#  .[type %in% c('education','facility','government')] %>%
  .[!str_detect(domain, ',')]

units <- unique(test_data[, ..unit_cols])
list_es_solo <- list()

trend_controls_to_test <- list( NULL,
                               c('field', 'entry_cohort','cnrs', 'city') ,
                               c('field', 'entry_cohort','cnrs', 'city', 'gender'),     
                               c('field', 'entry_cohort','cnrs', 'city', 'prod_au_n_tile'),     
                               c('field', 'entry_cohort','cnrs', 'city', 'gender', 'prod_au_n_tile'),     
                               c('field', 'entry_cohort','cnrs', 'city', 'prod_inst_n_tile'),                                
                               c('field', 'entry_cohort','cnrs', 'city', 'prod_au_n_tile', 'prod_inst_n_tile'),
                               c('field', 'entry_cohort','cnrs', 'city', 'gender', 'prod_au_n_tile', 'prod_inst_n_tile')
                               
                                )

for(trend_ctrl in trend_controls_to_test){
list_es_solo[[paste0(trend_ctrl, collapse = '_')]] <- compute_separate_estimates(treatments = c("acces_rce",'date_first_idex'),
                                   outcomes = c('publications_reweight','citations_reweight','nr_source_top_5pct_reweight','nr_source_top_10pct_reweight',
                                                'avg_rank_source_raw'),
                                   data = test_data,
                                   w_matching = TRUE, matching_variables = c('entry_cohort','city','field'),
                                   trend_controls = trend_ctrl,
                                   plot_event_study = TRUE,
                                   save_event_study = TRUE, save_path = save_path, type = "feols"
)

gc()

}

saveRDS(list_es_solo, paste0(save_path, 'all_regressions_matched_entry_cohort_city_field.rds'))

outcomes <- c('publications_reweight','citations_reweight','nr_source_top_5pct_reweight','nr_source_top_10pct_reweight',
              'avg_rank_source_raw')
agg_stag <- map(outcomes, \(outcome)
                map(c("acces_rce",'date_first_idex'), \(treatment)
                    map(1:length(list_es_solo), \(spec)
                        list_es_solo[[spec]][[treatment]][[outcome]][["table_agg"]]
                    )
                )
) |>
  unlist(recursive = FALSE) |>
  unlist(recursive = FALSE) |>   # one extra unlist to flatten the treatment level
  rbindlist() %>% distinct()

pre_mean <- map(outcomes, \(outcome)
                map(c("acces_rce",'date_first_idex'), \(treatment)
                    map(1:length(list_es_solo), \(spec)
                        list_es_solo[[spec]][[treatment]][[outcome]][["pre_mean"]]
                    )
                )
) |>
  unlist(recursive = FALSE) |>
  unlist(recursive = FALSE) 

n_obs <- map(outcomes, \(outcome)
                map(c("acces_rce",'date_first_idex'), \(treatment)
                    map(1:length(list_es_solo), \(spec)
                        list_es_solo[[spec]][[treatment]][[outcome]][["n_obs"]]
                    )
                )
) |>
  unlist(recursive = FALSE) |>
  unlist(recursive = FALSE) #|>   # one extra unlist to flatten the treatment level


r_2 <- map(outcomes, \(outcome)
             map(c("acces_rce",'date_first_idex'), \(treatment)
                 map(1:length(list_es_solo), \(spec)
                     list_es_solo[[spec]][[treatment]][[outcome]][["pseudo_r_2"]]
                 )
             )
) |>
  unlist(recursive = FALSE) |>
  unlist(recursive = FALSE) #|>   # one extra unlist to flatten the treatment level

make_stargazer_like_table_dt(unique(agg_stag_solo %>% .[treat == "acces_rce"] %>%
                                      .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             pre_mean = pre_mean,
                             n_obs = n_obs_solo$acces_rce,
                             r_2 = r_2_solo$acces_rce,
                             var_order = c('publications_reweight','citations_reweight','nr_source_top_10pct_reweight','nr_source_top_5pct_reweight'), 
                             drop_unlisted_vars = TRUE,
                             save_path = 'D:\\panel_fr_res\\productivity_results\\individual\\agg_prod_acces_rce_solo.tex'
)



make_stargazer_like_table_dt(unique(agg_stag_solo %>% .[treat == "date_first_idex"] %>%
                                      .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             pre_mean = pre_mean_solo$date_first_idex,
                             n_obs = n_obs_solo$date_first_idex,
                             r_2 = r_2_solo$date_first_idex,
                             var_order = c('publications_reweight','citations_reweight','nr_source_top_10pct_reweight','nr_source_top_5pct_reweight'), 
                             drop_unlisted_vars = TRUE,
                             save_path = 'D:\\panel_fr_res\\productivity_results\\individual\\agg_prod_date_first_idex_solo.tex'
)


all_coefs <- as.data.table(list_es_solo[[treat]][[var]][['no_ctrl']]$coeftable, keep.rownames = TRUE)%>%
  .[, d := str_extract(rn, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
  # .[, d := str_extract(var, '(?<=year[0-9]{4}:)[a-z_]+(?=[0-9])|^[a-z_]+(?=[0-9]{4}:year)')]%>%
  .[, g := str_extract(rn, paste0('(?<=' , d, '_)[0-9]{4}')) ] %>%
  .[, year := str_extract(rn, '(?<=year::)[0-9]{4}')] %>%
  # .[, year := str_extract(var, '(?<=year)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] %>%
  .[, ':='(est = Estimate,
           std = `Std. Error`)]
for( d_plot in c('acces_rce')){
  for(g_plot in sort(unique( (all_coefs %>% .[d==d_plot])$g )) ){
    p <- ggplot(all_coefs %>% .[d==d_plot & abs(t)<7 & g == g_plot])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[d_plot]], ' for cohort ', g_plot))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    print(p)
  }}
saveRDS(list_es_solo, file = "D:\\panel_fr_res\\productivity_results\\individual\\regressions_treatment_by_treatment")
