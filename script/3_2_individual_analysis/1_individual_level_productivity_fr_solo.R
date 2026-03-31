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


inputpath <- "D:\\panel_fr_res\\panel_smoothed.parquet"


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
                    'nr_source_top_10pct_raw'
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



cities <- fread('D:\\panel_fr_res\\v_commune_2025.csv')

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
  .[, fusion_date := fifelse(fusion_date =="2023", "0", as.character(fusion_date))] %>%
  #.[year != "2020"] %>%
  .[ , ':='(fusion_date = as.factor(fusion_date),
            date_first_idex = as.factor(date_first_idex),
            acces_rce = as.factor(acces_rce),
            year = as.factor(year))]%>%
  .[, city:= case_when(city == "Saint-Etienne" ~ 'Saint-Étienne',
                       city == "St-Malo" ~ "Saint-Malo",
                       .default  = city
  )] 
gc()

sample_df_reg <- merge(sample_df_reg, cities %>% .[, city:=LIBELLE], by ='city', allow.cartesian = TRUE)%>%
  .[!is.na(LIBELLE)]

outcomes <- c('publications_raw', 'publications_reweight',
              'citations_raw','citations_reweight',
              'nr_source_top_5pct_raw', 'nr_source_top_5pct_reweight',
              'nr_source_top_10pct_raw', 'nr_source_top_10pct_reweight'
)

sample_df_reg <-sample_df_reg %>%   .[, (outcomes) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes]
fwrite(sample_df_reg, "D:\\panel_fr_res\\sample_df_reg.csv" )
sample_df_reg <- fread( "D:\\panel_fr_res\\sample_df_reg.csv" )

length(unique(sample_df_reg$author_id)) #129095
nrow(unique(sample_df_reg[, list(merged_inst_id, field)])) #30708
table(unique(sample_df_reg[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df_reg[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df_reg[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$fusion_date)


# Write specification ---------------------------------------------------


fe_min = ' | merged_inst_id^domain + author_id + year'

fe_large = paste0(  ' | merged_inst_id^domain + '
                    ,'year +author_id '
                    ,'+ type^year '
                    ,'+ gender^year'
                    ,'+ public^year'
                    ,'+ ecole^year'
                    ,'+ cnrs^year'
                    ,'+ field^year'
                    ,'+ entry_year^year '
                    ,'+ prod_au_n_tile^year'
                    ,'+ prod_inst_n_tile^year'
                    ,'+ size_n_tile^year'
                    ,'+ REG^year'
                    ,'+ n_inst_y^year'
                    
)
gc()


# Alternative : estimate treatments separately ----------------------------

p_load('did', 'MatchIt')
all_treatments <- c('acces_rce','date_first_idex','fusion_date')

unit_cols <- c('merged_inst_id','domain', 'name', 'author_id','author_name', 
               'acces_rce', 'date_first_idex','fusion_date',
               'city', 'DEP', 'REG','type','public','ecole','cnrs',
               'size_n_tile','prod_inst_n_tile',
               'entry_year', 'prod_au_n_tile', 'gender'
)

list_es_solo <- list(acces_rce = list(), date_first_idex = list())

for(treat in c('acces_rce', 'date_first_idex')){
  
  sample_separate <- sample_df_reg
  
  for(d in all_treatments[all_treatments != treat]){
    sample_separate <- sample_separate[sample_separate[[d]] == 0]
  }
  sample_separate <- sample_separate[!str_detect(domain, ',') & !is.na(REG)]
  
  ## Remove excluded values
  if(treat == 'acces_rce'){sample_separate <- sample_separate %>% .[acces_rce != 2015]}
  
  treatment_values <- sort(unique(sample_separate[sample_separate[[treat]] !=0][[treat]]))
  
  formula_elements <- c()
  for(g_i in treatment_values){
    print(paste0(treat, ': ', g_i))
    varname =paste0(treat, '_', g_i)
    ref = as.character(as.numeric(g_i)-1)
    sample_separate[[varname]] <- as.numeric((sample_separate[[treat]] == g_i))
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  }  
  length(formula_elements)
  
  formula_no_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_min, ' + subclass^year'))
  formula_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_large, '+ subclass^year'))
  
  units <- unique(sample_separate[, ..unit_cols]) %>%
    .[, treat := as.numeric(acces_rce != 0 | date_first_idex!=0 | fusion_date != 0)]
  print(table(units[[treat]]))
  match_units <- matchit(treat ~
                           entry_year + domain
                         ,data = units
                         ,method = "exact",
  )
  matched_units <- match.data(match_units)
  print(table(matched_units[[treat]]))
  
  matched_units <- matched_units %>%
    .[, list(merged_inst_id, domain, author_id, subclass)]
  
  
  for(var in setdiff(outcomes[str_detect(outcomes, 'reweight')], names(list_es_solo[[treat]])) ){
    
    list_es_solo[[treat]][[var]] <- list()
    
    sample_separate$y <- sample_separate[[var]]
    
    sample_separate <- sample_separate %>%
      .[, ':='(y_lt = sum(y)), by = c('merged_inst_id',"domain", 'year')] %>%
      .[, y_minus_i_lt := (y_lt - y)/(1-n_lt)]
    
    matched_data <- merge(sample_separate, matched_units, by = c('merged_inst_id','domain','author_id'))
    
    
    start_time <- Sys.time()
    es_stag <- fepois(formula_no_ctrl
                      , data = matched_data
                      ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                      ,cluster = c('merged_inst_id', 'domain','author_id')
    ) 
    time_taken <- Sys.time() - start_time
    print(time_taken)
    gc()
    list_es_solo[[treat]][[var]][['no_ctrl']] <- es_stag
    
    start_time <- Sys.time()
    es_stag_w_ctrl <- fepois(formula_ctrl,
                             , data = matched_data 
                             ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                             ,cluster = c('merged_inst_id', 'domain','author_id')
    ) 
    time_taken <- Sys.time()-start_time
    gc()
    print(time_taken)
    list_es_solo[[treat]][[var]][['ctrl']] <- es_stag_w_ctrl
  }
  
}



source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))

agg_stag_solo <- data.table(treat = '', est = 0, std = 0, t= 0, pvalue = 0, pvalue_pretrend= 0, type = '', comparison_group = '',  var = '', ctrl = '') %>% .[treat != '']
agg_stag_by_t_solo <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0,  t= 0, n = '', comparison_group = '', var = '',  ctrl = '') %>% .[treatment != '']

agg_stag_by_g_solo <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0, g = 0, n = '', comparison_group = '', var = '',  ctrl = '') %>% .[treatment != '']

gc()

pre_mean_solo <- list() 

for(treat in c('acces_rce','date_first_idex')){
  no_ctrl_path = paste0("D:\\panel_fr_res\\productivity_results\\individual\\heterogeneity\\",treat, "\\no_ctrl\\")
  ctrl_path = paste0("D:\\panel_fr_res\\productivity_results\\individual\\heterogeneity\\",treat, "\\ctrl\\")
  if (!file.exists(no_ctrl_path)){
    dir.create(no_ctrl_path, recursive = TRUE)
  }
  if (!file.exists(ctrl_path)){
    dir.create(ctrl_path, recursive = TRUE)
  }
  
  sample_separate <- sample_df_reg
  
  for(d in all_treatments[all_treatments != treat]){
    sample_separate <- sample_separate[sample_separate[[d]] == 0]
  }
  sample_separate <- sample_separate[!str_detect(domain, ',') & !is.na(REG)]
  
  ## Remove excluded values
  if(treat == 'acces_rce'){sample_separate <- sample_separate %>% .[acces_rce != 2015]}
  
  treatment_values <- sort(unique(sample_separate[sample_separate[[treat]] !=0][[treat]]))
  
  formula_elements <- c()
  for(g_i in treatment_values){
    print(paste0(treat, ': ', g_i))
    varname =paste0(treat, '_', g_i)
    ref = as.character(as.numeric(g_i)-1)
    sample_separate[[varname]] <- as.numeric((sample_separate[[treat]] == g_i))
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  }  
  length(formula_elements)
  
  units <- unique(sample_separate[, ..unit_cols]) %>%
    .[, treat := as.numeric(acces_rce != 0 | date_first_idex!=0 | fusion_date != 0)]
  print(table(units[[treat]]))
  match_units <- matchit(treat ~
                           entry_year + domain
                         ,data = units
                         ,method = "exact",
  )
  matched_units <- match.data(match_units)
  print(table(matched_units[[treat]]))
  
  matched_units <- matched_units %>%
    .[, list(merged_inst_id, domain, author_id, subclass)]
  
  
  matched_data <-   merge(sample_separate, matched_units, by = c('merged_inst_id','domain','author_id'))
  for(var in names(list_es_solo[[treat]])){
    
    pre_mean_solo[[treat]][[var]] <- round(mean((sample_separate[as.numeric(as.character(year)) < 2009])[[var]], na.rm =T),2)
    agg_stag_no_ctrl <- agg_effects(list_es_solo[[treat]][[var]][['no_ctrl']], matched_data, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_solo <- rbind(agg_stag_solo, agg_stag_no_ctrl)
    agg_stag_by_t_no_ctrl <- agg_effect_het(list_es_solo[[treat]][[var]][['no_ctrl']], matched_data, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_by_t_solo <- rbind(agg_stag_by_t_solo, agg_stag_by_t_no_ctrl)
    for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
      p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(no_ctrl_path,var, '_', treat, '_by_t',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    agg_stag_by_g_no_ctrl <- agg_effect_het(list_es_solo[[treat]][[var]][['no_ctrl']], matched_data, by  ='g')%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_by_g_solo <- rbind(agg_stag_by_g_solo, agg_stag_by_g_no_ctrl)
    for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
      p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= g, y = est))+
        geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('First treatment period')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(no_ctrl_path,var, '_', treat, '_by_g',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    
    
    
    
    agg_stag_ctrl <- agg_effects(list_es_solo[[treat]][[var]][['ctrl']], matched_data, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    
    agg_stag_solo <- rbind(agg_stag_solo, agg_stag_ctrl)
    agg_stag_by_t_ctrl <- agg_effect_het(list_es_solo[[treat]][[var]][['ctrl']], matched_data, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    agg_stag_by_t_solo <- rbind(agg_stag_by_t_solo, agg_stag_by_t_ctrl)
    
    for(treat in unique(agg_stag_by_t_ctrl$treatment)){
      p <- ggplot(agg_stag_by_t_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(ctrl_path,var, '_', treat, '_by_t',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    
    agg_stag_by_g_ctrl <- agg_effect_het(list_es_solo[[treat]][[var]][['ctrl']], matched_data, by  ='g')%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    agg_stag_by_g_solo <- rbind(agg_stag_by_g_solo, agg_stag_by_g_ctrl)
    
    for(treat in unique(agg_stag_by_g_ctrl$treatment)){
      p <- ggplot(agg_stag_by_g_ctrl %>% .[treatment %in% c(treat) ])+
        geom_point(aes(x= g, y = est))+
        geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
        geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Treatment cohort')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(ctrl_path,var, '_', treat, '_by_g',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
  }
}
n_obs_solo <- list()
r_2_solo <- list()
for(treat in c('acces_rce','date_first_idex')){
  for(var in names(list_es)){
    n_obs_solo[[treat]][[ paste0(var, " | ", fe_min)]] <- list_es_solo[[treat]][[var]][['no_ctrl']]$nobs
    n_obs_solo[[treat]][[ paste0(var, " | ", fe_large)]] <- list_es_solo[[treat]][[var]][['ctrl']]$nobs
    r_2_solo[[treat]][[ paste0(var, " | ", fe_min)]] <-   round(list_es_solo[[treat]][[var]][['no_ctrl']]$pseudo_r2, 5)
    r_2_solo[[treat]][[ paste0(var, " | ", fe_large)]] <- round(list_es_solo[[treat]][[var]][['ctrl']]$pseudo_r2   , 5)
    #r_2[[ paste0(var, " | ", fe_min)]] <-   round(r2(list_es[[var]][['no_ctrl']])[['r2']], 5)
    #r_2[[ paste0(var, " | ", fe_large)]] <- round(r2(list_es[[var]][['ctrl']])[['r2']]   , 5)
  }
}


make_stargazer_like_table_dt(unique(agg_stag_solo %>% .[treat == "acces_rce"] %>%
                                      .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             pre_mean = pre_mean_solo$acces_rce,
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
