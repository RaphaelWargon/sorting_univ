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
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))


inputpath <- "D:\\panel_fr_res\\data\\panel_au_year_fr.parquet"

save_path = paste0("D:\\panel_fr_res\\results\\productivity_au\\all_treatments\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

ds <- open_dataset(inputpath) %>%
  filter(
    last_year-entry_year >2
    & entry_year >=1965 
    #& !(acces_rce_0_1y %in%  c(2014, 2015))
    #& !(date_first_idex_0_1y %in% c(2014))
    #& !(fusion_date_0_1y %in% c(2012,2016,2019))
    & year >= 2003
    & entry_year <=2003
  )
nrow(ds)

ds <- as.data.table(ds)
gc()
table(ds$control_0_1y)

sample_df_reg <- ds %>%
  .[, year_n := as.numeric(as.character(year))] %>%
  .[, ever_in_idex_annulee := max(as.numeric(str_detect(idex_set, "annulee") )), by = 'author_id'] %>%
  .[ever_in_idex_annulee==0] %>%
  .[, pub_04_07 := sum(as.numeric(year_n > 2004 & year_n <= 2007) * publications_raw ), by = 'author_id'] %>%
  .[, cit_04_07 := sum(as.numeric(year_n > 2004 & year_n <= 2007) * citations_raw ), by = 'author_id'] %>%
  .[pub_04_07 >=2] %>%
  .[str_count(field, ',')<=1]%>%
  #.[year != "2020"] %>%
  .[ , ':='(idn = as.numeric(str_remove(author_id, 'A')))]
gc()

sample_df_reg %>% .[year >=2003] %>% .[, .N, by = 'author_id'] %>% .[, .N, by = "N"]

test <- sample_df_reg %>%
  .[str_detect(author_name, 'Aghion')]

outcomes <- c('publications_raw',
              'citations_raw',
              'nr_source_top_5pct_raw', 
              'nr_source_top_10pct_raw',
              'nr_source_top_20pct_raw',
              'nr_source_mid_40pct_raw',
              'nr_source_btm_50pct_raw',
              colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), "new")]
)

sample_df_reg <-sample_df_reg %>%   .[, (outcomes) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes] %>%
  .[, ':='(entry_cohort = floor(entry_year/5)*5) ]


sample_df_reg <- sample_df_reg %>%
  .[, ':='(pub_n_tile = cut(pub_04_07, unique(quantile(unique(sample_df_reg[, list(author_id, pub_04_07)])$pub_04_07,
                                                       probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, ':='(cit_n_tile = cut(cit_04_07, unique(quantile(unique(sample_df_reg[, list(author_id, cit_04_07)])$cit_04_07,
                                                       probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, min_cnrs := min(ifelse(in_cnrs==1, year, NA), na.rm =T), by ='author_id'] %>%
  .[, min_cnrs := ifelse(!is.na(min_cnrs),min_cnrs, 0)]%>%
  .[, ':='(interact_rce_idex_0_1y =  ifelse(acces_rce_0_1y != 0 & date_first_idex_0_1y != 0,
                                            pmax(as.numeric(as.character(acces_rce_0_1y)), 
                                                 as.numeric(as.character(date_first_idex_0_1y))), 0 ),
           interact_rce_idex_0_3y =  ifelse(acces_rce_0_3y != 0 & date_first_idex_0_3y != 0,
                                            pmax(as.numeric(as.character(acces_rce_0_3y)), 
                                                 as.numeric(as.character(date_first_idex_0_3y))), 0 ),
           interact_rce_idex_0_5y =  ifelse(acces_rce_0_5y != 0 & date_first_idex_0_5y != 0,
                                            pmax(as.numeric(as.character(acces_rce_0_5y)), 
                                                 as.numeric(as.character(date_first_idex_0_5y))), 0 ),
           interact_rce_idex_2_3y =  ifelse(acces_rce_2_3y != 0 & date_first_idex_2_3y != 0,
                                            pmax(as.numeric(as.character(acces_rce_2_3y)), 
                                                 as.numeric(as.character(date_first_idex_2_3y))), 0 ),
           interact_rce_idex_2_5y =  ifelse(acces_rce_2_5y != 0 & date_first_idex_2_5y != 0,
                                            pmax(as.numeric(as.character(acces_rce_2_5y)), 
                                                 as.numeric(as.character(date_first_idex_2_5y))), 0 ),
           
           interact_rce_idex_itt_2005 =  ifelse(ITT_acces_rce_2005 != 0 & ITT_date_first_idex_2005 != 0,
                                            pmax(as.numeric(as.character(ITT_acces_rce_2005)), 
                                                 as.numeric(as.character(ITT_date_first_idex_2005))), 0 ),
           
           interact_rce_idex_itt_2006 =  ifelse(ITT_acces_rce_2006 != 0 & ITT_date_first_idex_2006 != 0,
                                                pmax(as.numeric(as.character(ITT_acces_rce_2006)), 
                                                     as.numeric(as.character(ITT_date_first_idex_2006))), 0 ),
           
           interact_rce_idex_itt_2007 =  ifelse(ITT_acces_rce_2007 != 0 & ITT_date_first_idex_2007 != 0,
                                                pmax(as.numeric(as.character(ITT_acces_rce_2007)), 
                                                     as.numeric(as.character(ITT_date_first_idex_2007))), 0 ),
           
           interact_rce_idex_itt_2008 =  ifelse(ITT_acces_rce_2008 != 0 & ITT_date_first_idex_2008 != 0,
                                                pmax(as.numeric(as.character(ITT_acces_rce_2008)), 
                                                     as.numeric(as.character(ITT_date_first_idex_2008))), 0 ),
           
           interact_rce_idex_itt_2009 =  ifelse(ITT_acces_rce_2009 != 0 & ITT_date_first_idex_2009 != 0,
                                                pmax(as.numeric(as.character(ITT_acces_rce_2009)), 
                                                     as.numeric(as.character(ITT_date_first_idex_2009))), 0 )
           
           
  )]

fwrite(sample_df_reg, "D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" )
rm(ds)
gc()

sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 
sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #158674
gc()


unit_cols <- c("author_id", "domain","field", "subfield","gender", "entry_year","last_year",
               "entry_cohort", "pub_04_07","cit_04_07","min_cnrs","pub_n_tile",'min_cnrs' ,
               'acces_rce','date_first_idex','fusion_date','interact_rce_idex','cit_n_tile'
)

# Stayers sample ----------------------------------------------------------

stayers <- sample_df_reg %>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce)),by= 'author_id'] %>%
  .[all_chg == 0] %>%
  .[,':='(acces_rce  = acces_rce_0_1y,
          date_first_idex =date_first_idex_0_1y,
          fusion_date = fusion_date_0_1y,
          interact_rce_idex = interact_rce_idex_0_1y,
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile)
  )] %>%
  .[, ':='(date_first_idex = ifelse(acces_rce ==0, date_first_idex, 0 ))]%>%
  .[!str_detect(inst_id_set, ',')] %>%
  .[all_acces_rce ==0 | acces_rce!=0] 

stayers %>%
  .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce'), .SD= c('publications_raw','citations_raw','in_acces_rce','retired')]%>%
  ggplot() + geom_line(aes(x=year, y = in_acces_rce, color = factor(acces_rce)))

test <- stayers %>% .[acces_rce ==0 & in_acces_rce == 1] %>% .[, list(inst_id_set)] %>% distinct()

table(stayers$acces_rce)
table(stayers$acces_rce, stayers$date_first_idex)
table(stayers$acces_rce, stayers$interact_rce_idex)
table(stayers$date_first_idex, stayers$interact_rce_idex)


list_g <- make_list_g(stayers, c("acces_rce", "date_first_idex", "fusion_date", "interact_rce_idex"))

formula_elements <- c()
formula_w_interactions <- c()
for(d in names(list_g)){
  for(g_i in list_g[[d]]){
    varname =paste0(d, '_', g_i)
    print(varname)
    ref = as.character(as.numeric(g_i)-1)
    stayers[[varname]] <- as.numeric((stayers[[paste0(d)]] == g_i))
    if(!str_detect(d, 'interact')){
      formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    }
    formula_w_interactions <- c(formula_w_interactions, paste0(varname, ' + i(year,', varname, ',ref=',ref,')') )
  }
}



test <- compute_all_estimates(outcomes = c('publications_raw', 
                                           'citations_raw'
                                           ,'total_new_phrase_comb_reuse','nr_source_top_5pct_raw'
),
data = stayers %>% .[cit_04_07 >2],
w_matching = TRUE, 
matching_variables = c('entry_year','field','pub_n_tile','cit_n_tile'),
#w_matching = FALSE,
id_vars = c('author_id'),
trend_controls = #NU
  c('in_cnrs','in_ecole'),
plot_event_study = TRUE,
#save_event_study = TRUE, save_path = save_path, 
type = "feols",
formula_elements = formula_w_interactions,
peer_effects = FALSE
)




# All  -----------------------------------------------------------------
type_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), 'in_type')] 



all_df_reg <- sample_df_reg %>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce),
           all_idex = sum(in_date_first_idex),
           all_retired = sum(as.numeric(year >last_year))
  ),by= 'author_id'] %>%
  .[, (paste0('all_', type_cols)) := lapply(.SD, sum), by = 'author_id', .SDcols = type_cols]%>%
  .[all_in_type_company ==0 & all_in_type_archive ==0 & all_in_type_other == 0
   & !is.na(field)
  ] %>%
  .[,':='(acces_rce  = ITT_acces_rce_2007,
          date_first_idex =ITT_date_first_idex_2007,
          fusion_date = ITT_fusion_date_2007,
          interact_rce_idex = interact_rce_idex_itt_2007,
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile)
  )] %>%
  .[, has_pub := as.numeric(publications_raw >0)] %>%
  #.[ (acces_rce == acces_rce_0_1y)
  #   &(date_first_idex == date_first_idex_0_1y)
  #   ]%>%
 # .[, ':='(date_first_idex = ifelse(acces_rce ==0, date_first_idex, 0 ))]%>%
  #.[!str_count(inst_id_set, ',')>2] %>%
  .[!(acces_rce %in% 2013:2015) & !(date_first_idex %in% c(2013,2014))
    & !(interact_rce_idex %in% c(2013,2014))
    # & (fusion_date ==0)
  ] %>%
  .[, inst_id_obs := .N, by = c('inst_id_set')] %>%
  .[, ':='(min_inst_id_obs = min(inst_id_obs),
           n_obs = .N,
           max_nr_inst_id = max(str_count(inst_id_set,','))
  ), by = 'author_id'] %>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce), 0, acces_rce),
           date_first_idex = ifelse(is.na(date_first_idex), 0, date_first_idex),
           fusion_date = ifelse(is.na(fusion_date), 0, fusion_date),
           interact_rce_idex = ifelse(is.na(interact_rce_idex), 0, interact_rce_idex)
           )
    
  ] %>% .[fusion_date<=2020] %>%
  .[, n_lt := .N, by = c('inst_id_set','field')]

table((all_df_reg %>% .[, .N, by = 'author_id'])$N)
gc()

all_df_reg %>%
  .[, has_pub := as.numeric(publications_raw >0)] %>%
  .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce'), 
    .SD= c('publications_raw','change_af','citations_raw','in_acces_rce','retired','has_pub'
    )]%>%
  ggplot() + geom_line(aes(x=year, y = has_pub, color = factor(acces_rce)))

nrow(all_df_reg[str_count(inst_id_set, ',')>0])/nrow(all_df_reg)

gc()

list_g <- make_list_g(all_df_reg, c("acces_rce", "date_first_idex", "fusion_date", "interact_rce_idex"))

formula_elements <- c()
formula_w_interactions <- c()
for(d in names(list_g)){
  for(g_i in list_g[[d]]){
    varname =paste0(d, '_', g_i)
    print(varname)
    ref = as.character(as.numeric(g_i)-2)
    all_df_reg[[varname]] <- as.numeric((all_df_reg[[paste0(d)]] == g_i))
    if(!str_detect(d, 'interact')){
      formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    }
    formula_w_interactions <- c(formula_w_interactions, paste0(varname, ' + i(year,', varname, ',ref=',ref,')') )
  }
}

table(all_df_reg$acces_rce,all_df_reg$date_first_idex)
table(all_df_reg$interact_rce_idex,all_df_reg$date_first_idex)

table( unique(all_df_reg %>% 
                .[, list(acces_rce, interact_rce_idex, inst_id_set)])
       $acces_rce,unique(all_df_reg %>% 
                           .[, list(acces_rce,interact_rce_idex, inst_id_set)])$interact_rce_idex)



gc()

sample_separate_rce <- all_df_reg %>%
  .[, ':='(yearn = as.numeric(as.character(year)),
           acces_rce = as.numeric(as.character(acces_rce)),
           idn = as.numeric(str_remove(author_id, "A")),
           d1 = as.numeric(str_detect(domain, "1")),
           d2 = as.numeric(str_detect(domain, "2")),
           d3 = as.numeric(str_detect(domain, "3")),
           d4 = as.numeric(str_detect(domain, "4"))
  )] %>% .[ date_first_idex == 0] %>%
  .[, n_field := n_distinct(author_id), by = 'field'] %>%
  .[n_field >=50] %>%
  .[, ':='(city_2007 = paste(ifelse(year ==2007, DEP_set, ''), collapse = ""), 
    inst_2007 = paste(ifelse(year ==2007, inst_id_set, ''), collapse = "")
    ), by = 'author_id'] %>%
  .[, ':='(N = n_distinct(author_id)), by = 'city_2007'] %>% .[N>=20]

ggplot(sample_separate_rce %>%
         .[, .(N = n_distinct(author_id)), by = 'city_2007'])+
  geom_density(aes(x=(N)))

gc()
table( unique(sample_separate_rce[, list(acces_rce, author_id)])$acces_rce)
test_cs <- att_gt(yname = 'publications_raw',
                  tname = 'yearn',
                  idname = 'idn',
                  gname = 'acces_rce',
                  data = sample_separate_rce
                    
                  # allow_unbalanced_panel = TRUE,
                  # faster_mode = FALSE,
                  , base_period = "universal"
                  ,xformla = ~ entry_year + subfield  +pub_n_tile + cit_n_tile + city_2007
                  ,control_group = 'nevertreated'
                    
)

ggdid(aggte(test_cs, type =  "dynamic", na.rm = T))

test_all <- compute_all_estimates(outcomes = c(#'publications_raw', 
  # "has_pub"
  'publications_raw',#'citations_raw', 'nr_source_top_5pct_raw',
  "total_new_phrase_comb_reuse"#,'semantic_distance'
),
data = all_df_reg 
,w_matching = TRUE, matching_variables = c('entry_year','field','pub_n_tile','cit_n_tile'),
#,w_matching = FALSE,
id_vars = c('author_id'),
trend_controls =
  #NULL,
  c('in_cnrs','in_ecole'
    #,'in_type_education','in_type_facility','in_type_government','in_type_company','in_type_archive','in_type_nonprofit'
    ,'entry_year','field'
    ,'pub_n_tile', 'cit_n_tile'
    #,'city_set'
    #, 'inst_id_set'
  ),
plot_event_study = TRUE,
save_event_study = FALSE,
#save_event_study = TRUE, save_path = save_path, 
type = "feols",
formula_elements = formula_w_interactions,
peer_effects = FALSE
)

test_2<-as.data.table(test_all$publications_raw$regression$coeftable, keep.rownames = T) %>%
  .[, var:=rn] %>% .[,est :=Estimate] %>% .[,std:=`Std. Error`] %>%.[, rn :=NULL] %>%
  .[ str_detect(var, '(?<=[0-9]:)[a-z_]')]%>%
  .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
  # .[, d := str_extract(var, '(?<=year[0-9]{4}:)[a-z_]+(?=[0-9])|^[a-z_]+(?=[0-9]{4}:year)')]%>%
  .[, g := str_extract(var, paste0('(?<=' , d, '_)[0-9]{4}')) ] %>%
  .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
  # .[, year := str_extract(var, '(?<=year)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] #%>% .[, t := as.character(t)]

for(treat in unique(test_2$d)){
  print(treat)
  event_study_plot <- ggplot(test_2 %>% .[d ==treat])+
    geom_point(aes(x= as.factor(t), y = est, color = g))+
    geom_errorbar(aes(x=as.factor(t), ymin = est -1.96*std, ymax=est+1.96*std, color = g))+
    geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  print(event_study_plot)
}

