rm(list = ls())
gc()
library('pacman')
p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot',
       'MatchIt')
wins_vars <- function(x, pct_level = 0.025){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/etwfe_functions.R'))


# I) Loading data ------------------------------------------------------------



inputpath <- "D:\\panel_fr_res\\inst_level_flows.parquet"

ds <- open_dataset(inputpath) %>%
  filter(!is.na(type_r) & !is.na(type_s) & year >=2003) %>%
  group_by(merged_inst_id_r, merged_inst_id_s) %>%
  mutate(max_pair = max(total)) %>%
  ungroup() %>%
  filter(entry_year_dyad <= 2003 & max_pair >0)
gc()
ds <- as.data.table(ds)
nrow(ds)

colnames(ds)

ds <- ds %>%
  .[, size_r := max(stayers_w), by = c('merged_inst_id_r','domain','year') ] %>%
  .[, size_s := max(stayers_w), by = c('merged_inst_id_s','domain','year') ]%>%
  .[, size_r_2003 := sum(as.numeric(year== 2003)*size_r), by = c('merged_inst_id_r','domain') ] %>%
  .[, size_s_2003 := sum(as.numeric(year== 2003)*size_s), by = c('merged_inst_id_s','domain') ]
ds <- ds %>%
  .[, quant_size_r_2003 := cut(size_r_2003, unique(quantile(unique(ds[, list(merged_inst_id_r, domain, size_r_2003)])$size_r_2003,
                                                            probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE)
  ] %>%
  .[, quant_size_s_2003 := cut(size_s_2003, unique(quantile(unique(ds[, list(merged_inst_id_s, domain, size_s_2003)])$size_s_2003,
                                                            probs = c(0,0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE)
  ] %>%
  .[, ':='(quant_size_r_2003 = fifelse(is.na(quant_size_r_2003), 0, quant_size_r_2003),
           quant_size_s_2003 = fifelse(is.na(quant_size_s_2003), 0, quant_size_s_2003)
  )]%>%
  .[!is.na(merged_inst_id_s) & !is.na(merged_inst_id_r) & !is.na(domain)] %>%
  .[, unit := paste0(merged_inst_id_s, '_to_',merged_inst_id_r, '_', domain)] %>%
  .[, n_obs := n_distinct(year), by = unit] 
gc()


# Preparing data ----------------------------------------------------------


ds_clean <- ds %>%
  .[, fusion_date_r := fifelse(fusion_date_r == "2023", "0", as.character(fusion_date_r) )]%>%
  .[, fusion_date_s := fifelse(fusion_date_s == "2023", "0", as.character(fusion_date_s) )] %>%
  .[, ':='(fusion_date_s = fifelse(is.na(fusion_date_s), "0", as.character(fusion_date_s) ),
           fusion_date_r = fifelse(is.na(fusion_date_r), "0", as.character(fusion_date_r) ),
           date_first_idex_s = fifelse(is.na(date_first_idex_s), "0", as.character(date_first_idex_s) ),
           date_first_idex_r = fifelse(is.na(date_first_idex_r), "0", as.character(date_first_idex_r) ),
           acces_rce_s = fifelse(is.na(acces_rce_s), '0', as.character(acces_rce_s) ),
           acces_rce_r = fifelse(is.na(acces_rce_r), '0', as.character(acces_rce_r) ),
           city_r = fifelse(merged_inst_id_r == 'abroad','abroad', city_r),
           city_s = fifelse(merged_inst_id_s == 'abroad','abroad', city_s),
           main_topic_r = fifelse(merged_inst_id_r == 'abroad','abroad', main_topic_r),
           main_topic_s = fifelse(merged_inst_id_r == 'abroad','abroad', main_topic_s),
           secteur_r = fifelse(merged_inst_id_r == 'abroad','abroad', secteur_r),
           secteur_s = fifelse(merged_inst_id_r == 'abroad','abroad', secteur_s),
           type_fr_r = fifelse(merged_inst_id_r == 'abroad','abroad', type_fr_r),
           type_fr_s = fifelse(merged_inst_id_r == 'abroad','abroad', type_fr_s)
  )] %>%
  .[, ':='(city_s = fifelse(merged_inst_id_s == 'entry','entry', city_s),
           main_topic_s = fifelse(merged_inst_id_r == 'entry','entry', main_topic_s),
           secteur_s = fifelse(merged_inst_id_r == 'entry','entry', secteur_s),
           type_fr_s = fifelse(merged_inst_id_r == 'entry','entry', type_fr_s)
  )]
gc()
outcomes <-c('movers', colnames(ds_clean)[str_detect(colnames(ds_clean), 'movers_w|exit')])
controls <- c('type_r','type_s','city_r','city_s','secteur_r','secteur_s', 'ecole_r','ecole_s','cnrs_s','cnrs_r','quant_size_r_2003','quant_size_s_2003')

cols_to_keep <-c("merged_inst_id_r",'merged_inst_id_s','domain','year', 'unit',
                 'acces_rce_r','acces_rce_s','date_first_idex_r','date_first_idex_s',
                 'fusion_date_r','fusion_date_s',
                 outcomes,controls
)

ds_clean <- ds_clean[, ..cols_to_keep]
rm(ds)
gc()


ds_clean <- ds_clean %>%
  .[, ':='(
    acces_rce_a_s = fifelse(acces_rce_s != '0' & merged_inst_id_r == 'abroad', acces_rce_s, "0"),
    acces_rce_a_r = fifelse(acces_rce_r != '0' & merged_inst_id_s == 'abroad', acces_rce_r, "0"),
    
    date_first_idex_a_s = fifelse(date_first_idex_s != '0' & merged_inst_id_r == 'abroad', date_first_idex_s, "0"),
    date_first_idex_a_r = fifelse(date_first_idex_r != '0' & merged_inst_id_s == 'abroad', date_first_idex_r, "0"),
    
    fusion_date_a_s = fifelse(fusion_date_s != '0' & merged_inst_id_r == 'abroad', fusion_date_s, "0"),
    fusion_date_a_r = fifelse(fusion_date_r != '0' & merged_inst_id_s == 'abroad', fusion_date_r, "0"),
    
    acces_rce_e_r = fifelse(acces_rce_r != '0' & merged_inst_id_s == 'entry', acces_rce_r, "0"),
    date_first_idex_e_r = fifelse(date_first_idex_r != '0' & merged_inst_id_s == 'entry', acces_rce_r, "0"),
    fusion_date_e_r = fifelse(fusion_date_r != '0' & merged_inst_id_s == 'entry', acces_rce_r, "0")
    
  )]



# Solo treatment estimation -----------------------------------------------


p_load('did', 'MatchIt')
all_treatments <- c('acces_rce','date_first_idex','fusion_date')

unit_cols <- c("merged_inst_id_r",'merged_inst_id_s', 'unit',
               'domain',# 'name_r','name_s', 
               'acces_rce_r', 'date_first_idex_r','fusion_date_r',
               'acces_rce_s', 'date_first_idex_s','fusion_date_s',
               
               'city_r', 'city_s', 'type_r','type_s',#'public_r','public_s',
               'ecole_r','ecole_s',#'cnrs_r', 'cnrs_s',
               "quant_size_r_2003","quant_size_s_2003",'secteur_s','secteur_r'
)
fe_min <- '| year + merged_inst_id_r^domain  +merged_inst_id_s^domain + unit'#fe_large <- paste0(fe_min, '+ type_s^year + type_r^year + public_s^year + public_r^year + city_s^year + city_r^year')
#fe_large <- paste0(fe_min,  '+', paste0(paste0(controls, '^year'), collapse = ' + '))
fe_large <-  "| year + merged_inst_id_r^domain  +merged_inst_id_s^domain + unit+type_r^year + domain^year + type_s^year + type_s^type_r^year + city_r^year + city_s^year  + cnrs_r^year + cnrs_s^year + cnrs_s^cnrs_r^year + ecole_r^year + ecole_s^year + ecole_s^ecole_r^year"# + quant_size_r_2003^year + quant_size_s_2003^year+quant_size_s_2003^quant_size_r_2003^year"

list_es_solo <- list(acces_rce = list(), date_first_idex = list(), fusion_date = list())

agg_stag_solo <- data.table(treat = '', est = 0, std = 0, t= 0, pvalue = 0, var = '', ctrl = '') %>% .[treat != '']
agg_stag_by_t_solo <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0,  t= 0,var = '',  ctrl = '') %>% .[treatment != '']
agg_stag_by_g_solo <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0, g = 0,var = '',  ctrl = '') %>% .[treatment != '']

gc()

pre_mean_solo <- list() 

for(d in c('acces_rce',
           'date_first_idex'#,
           # "fusion_date"
)){
  
  no_ctrl_path = paste0("D:\\panel_fr_res\\lab_results\\heterogeneity\\",d, "\\no_ctrl\\")
  ctrl_path = paste0("D:\\panel_fr_res\\lab_results\\heterogeneity\\",d, "\\ctrl\\")
  if (!file.exists(no_ctrl_path)){
    dir.create(no_ctrl_path, recursive = TRUE)
  }
  if (!file.exists(ctrl_path)){
    dir.create(ctrl_path, recursive = TRUE)
  }
  
  sample_separate <- ds_clean
  
  for(treat in all_treatments[all_treatments != d]){
    sample_separate <- sample_separate[sample_separate[[paste0(treat,'_s')]] == 0]
    sample_separate <- sample_separate[sample_separate[[paste0(treat,'_r')]] == 0]
    
  }
  sample_separate <- sample_separate[!str_detect(domain, ',')]
  
  ## Remove excluded values
  if(d == 'acces_rce'){sample_separate <- sample_separate %>% .[acces_rce_r != 2015 &acces_rce_s != 2015 ]}
  
  treatment_values_s <- sort(unique(sample_separate[sample_separate[[paste0(d, '_s')]] !=0][[paste0(d, '_s')]]))
  treatment_values_r <- sort(unique(sample_separate[sample_separate[[paste0(d, '_r')]] !=0][[paste0(d, '_r')]]))
  
  formula_elements <- c()
  for(g_i in treatment_values_s){
    print(paste0(d, ': ', g_i))
    varname =paste0(d, '_s_g', g_i)
    ref = as.character(as.numeric(g_i)-1)
    sample_separate[[varname]] <- as.numeric((sample_separate[[paste0(d, '_s')]] == g_i)
                                             & (sample_separate[['merged_inst_id_r']] != "abroad")
                                             
    )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    
    varname_abroad =paste0(d, '_a_s_g', g_i)
    sample_separate[[varname_abroad]] <- as.numeric((sample_separate[[paste0(d, '_s')]] == g_i)
                                                    & (sample_separate[['merged_inst_id_r']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
    
  }
  for(g_j in treatment_values_s){
    print(paste0(d, ': ', g_j))
    varname =paste0(d, '_r_g', g_j)
    ref = as.character(as.numeric(g_j)-1)
    sample_separate[[varname]] <- as.numeric((sample_separate[[paste0(d, '_r')]] == g_j)
                                             & (sample_separate[['merged_inst_id_s']] != "abroad")
                                             & (sample_separate[['merged_inst_id_s']] != "entry")
    )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    
    varname_abroad =paste0(d, '_a_r_g', g_j)
    sample_separate[[varname_abroad]] <- as.numeric((sample_separate[[paste0(d, '_r')]] == g_j)
                                                    & (sample_separate[['merged_inst_id_s']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
    varname_entry =paste0(d, '_e_r_g', g_j)
    sample_separate[[varname_entry]] <- as.numeric((sample_separate[[paste0(d, '_r')]] == g_j)
                                                   & (sample_separate[['merged_inst_id_s']] == "entry")
    )
    formula_elements <- c(formula_elements, paste0(varname_entry, ' + i(year,', varname_entry, ',ref=',ref,')'))
    
    
  }    
  
  units <- unique(sample_separate[, ..unit_cols]) %>%
    .[, treat := as.numeric(acces_rce_r != 0 | date_first_idex_r !=0 | fusion_date_r != 0
                            |acces_rce_s != 0 | date_first_idex_s !=0 | fusion_date_s != 0 )]
  print(table(units[[paste0(d, '_r')]], units[[paste0(d, '_s')]]))
  match_units <- matchit(treat ~
                           domain + type_r + type_s
                         ,data = units
                         ,method = "exact",
  )
  matched_units <- match.data(match_units)
  print(table(matched_units[[paste0(d, '_r')]], matched_units[[paste0(d, '_s')]]))
  
  matched_units <- matched_units %>%
    .[, list(unit, subclass)]
  
  matched_data <- merge(sample_separate, matched_units, by = c('unit'))
  
  gc()
  
  for(var in setdiff(outcomes[str_detect(outcomes, '_w')], names(list_es_solo[[d]])) ){
    var_path_no_ctrl= paste0(no_ctrl_path, var, '\\')
    if (!file.exists(var_path_no_ctrl)){
      dir.create(var_path_no_ctrl, recursive = TRUE)
    }
    var_path_ctrl= paste0(ctrl_path, var, '\\')
    if (!file.exists(var_path_ctrl)){
      dir.create(var_path_ctrl, recursive = TRUE)
    }
    
    formula_no_ctrl <- as.formula(paste0(var, '~ ',  paste0(formula_elements, collapse= '+'), fe_min))#, ' + subclass^year'))
    formula_ctrl <- as.formula(paste0(var, ' ~ ',  paste0(formula_elements, collapse= '+'), fe_large))#, '+ subclass^year'))
    
    
    list_es_solo[[d]][[var]] <- list()
    
    
    
    start_time <- Sys.time()
    es_stag <- fepois(formula_no_ctrl
                      , data = matched_data
                      ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                      ,cluster = c('unit')
    ) 
    time_taken <- Sys.time() - start_time
    print(time_taken)
    gc()
    list_es_solo[[d]][[var]][['no_ctrl']] <- es_stag
    
    start_time <- Sys.time()
    es_stag_w_ctrl <- fepois(formula_ctrl,
                             , data = matched_data 
                             ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                             ,cluster = c('unit')
    ) 
    time_taken <- Sys.time()-start_time
    gc()
    print(time_taken)
    list_es_solo[[d]][[var]][['ctrl']] <- es_stag_w_ctrl
    
    
    saveRDS(list_es_solo, file = "D:\\panel_fr_res\\lab_results\\all_regressions_treatment_by_treatment.rds")
    pre_mean_solo[[treat]][[var]] <- round(mean((sample_separate[as.numeric(as.character(year)) < 2009])[[var]], na.rm =T),2)
    
    agg_stag_no_ctrl <- agg_etwfe(list_es_solo[[d]][[var]][['no_ctrl']], matched_data, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_solo <- rbind(agg_stag_solo, agg_stag_no_ctrl)
    agg_stag_by_t_no_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['no_ctrl']], matched_data, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_by_t_solo <- rbind(agg_stag_by_t_solo, agg_stag_by_t_no_ctrl)
    for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
      p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_no_ctrl,var, '_', treat, '_by_t',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    agg_stag_by_g_no_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['no_ctrl']], matched_data, by  ='g')%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_by_g_solo <- rbind(agg_stag_by_g_solo, agg_stag_by_g_no_ctrl)
    for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
      p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= g, y = est))+
        geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('First treatment period')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_no_ctrl,var, '_', treat, '_by_g',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    
    
    
    
    agg_stag_ctrl <- agg_etwfe(list_es_solo[[d]][[var]][['ctrl']], matched_data, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    
    agg_stag_solo <- rbind(agg_stag_solo, agg_stag_ctrl)
    agg_stag_by_t_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['ctrl']], matched_data, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    agg_stag_by_t_solo <- rbind(agg_stag_by_t_solo, agg_stag_by_t_ctrl)
    
    for(treat in unique(agg_stag_by_t_ctrl$treatment)){
      p <- ggplot(agg_stag_by_t_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_ctrl,var, '_', treat, '_by_t',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    
    agg_stag_by_g_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['ctrl']], matched_data, by  ='g')%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    agg_stag_by_g_solo <- rbind(agg_stag_by_g_solo, agg_stag_by_g_ctrl)
    
    for(treat in unique(agg_stag_by_g_ctrl$treatment)){
      p <- ggplot(agg_stag_by_g_ctrl %>% .[treatment %in% c(treat) ])+
        geom_point(aes(x= g, y = est))+
        geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
        geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Treatment cohort')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_ctrl,var, '_', treat, '_by_g',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
  }
}


#resave
for(d in names(list_es_solo)){
  no_ctrl_path = paste0("D:\\panel_fr_res\\lab_results\\heterogeneity\\",d, "\\no_ctrl\\")
  ctrl_path = paste0("D:\\panel_fr_res\\lab_results\\heterogeneity\\",d, "\\ctrl\\")
  if (!file.exists(no_ctrl_path)){
    dir.create(no_ctrl_path, recursive = TRUE)
  }
  if (!file.exists(ctrl_path)){
    dir.create(ctrl_path, recursive = TRUE)
  }
  
  sample_separate <- ds_clean
  
  for(treat in all_treatments[all_treatments != d]){
    sample_separate <- sample_separate[sample_separate[[paste0(treat,'_s')]] == 0]
    sample_separate <- sample_separate[sample_separate[[paste0(treat,'_r')]] == 0]
    
  }
  sample_separate <- sample_separate[!str_detect(domain, ',')]
  
  ## Remove excluded values
  if(d == 'acces_rce'){sample_separate <- sample_separate %>% .[acces_rce_r != 2015 &acces_rce_s != 2015 ]}
  
  treatment_values_s <- sort(unique(sample_separate[sample_separate[[paste0(d, '_s')]] !=0][[paste0(d, '_s')]]))
  treatment_values_r <- sort(unique(sample_separate[sample_separate[[paste0(d, '_r')]] !=0][[paste0(d, '_r')]]))
  
  formula_elements <- c()
  for(g_i in treatment_values_s){
    print(paste0(d, ': ', g_i))
    varname =paste0(d, '_s_g', g_i)
    ref = as.character(as.numeric(g_i)-1)
    sample_separate[[varname]] <- as.numeric((sample_separate[[paste0(d, '_s')]] == g_i)
                                             & (sample_separate[['merged_inst_id_r']] != "abroad")
                                             
    )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    
    varname_abroad =paste0(d, '_a_s_g', g_i)
    sample_separate[[varname_abroad]] <- as.numeric((sample_separate[[paste0(d, '_s')]] == g_i)
                                                    & (sample_separate[['merged_inst_id_r']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
    
  }
  for(g_j in treatment_values_s){
    print(paste0(d, ': ', g_j))
    varname =paste0(d, '_r_g', g_j)
    ref = as.character(as.numeric(g_j)-1)
    sample_separate[[varname]] <- as.numeric((sample_separate[[paste0(d, '_r')]] == g_j)
                                             & (sample_separate[['merged_inst_id_s']] != "abroad")
                                             & (sample_separate[['merged_inst_id_s']] != "entry")
    )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    
    varname_abroad =paste0(d, '_a_r_g', g_j)
    sample_separate[[varname_abroad]] <- as.numeric((sample_separate[[paste0(d, '_r')]] == g_j)
                                                    & (sample_separate[['merged_inst_id_s']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
    varname_entry =paste0(d, '_e_r_g', g_j)
    sample_separate[[varname_entry]] <- as.numeric((sample_separate[[paste0(d, '_r')]] == g_j)
                                                   & (sample_separate[['merged_inst_id_s']] == "entry")
    )
    formula_elements <- c(formula_elements, paste0(varname_entry, ' + i(year,', varname_entry, ',ref=',ref,')'))
    
    
  }    
  
  units <- unique(sample_separate[, ..unit_cols]) %>%
    .[, treat := as.numeric(acces_rce_r != 0 | date_first_idex_r !=0 | fusion_date_r != 0
                            |acces_rce_s != 0 | date_first_idex_s !=0 | fusion_date_s != 0 )]
  print(table(units[[paste0(d, '_r')]], units[[paste0(d, '_s')]]))
  match_units <- matchit(treat ~
                           domain + type_r + type_s
                         ,data = units
                         ,method = "exact",
  )
  matched_units <- match.data(match_units)
  print(table(matched_units[[paste0(d, '_r')]], matched_units[[paste0(d, '_s')]]))
  
  matched_units <- matched_units %>%
    .[, list(unit, subclass)]
  
  matched_data <- merge(sample_separate, matched_units, by = c('unit'))
  
  gc()
  
  
  for(var in names(list_es_solo[[d]])){
    print(paste0(d, ': ', var))
    var_path_no_ctrl= paste0(no_ctrl_path, var, '\\')
    if (!file.exists(var_path_no_ctrl)){
      dir.create(var_path_no_ctrl, recursive = TRUE)
    }
    var_path_ctrl= paste0(ctrl_path, var, '\\')
    if (!file.exists(var_path_ctrl)){
      dir.create(var_path_ctrl, recursive = TRUE)
    }
    
    agg_stag_no_ctrl <- agg_etwfe(list_es_solo[[d]][[var]][['no_ctrl']], matched_data, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_solo <- rbind(agg_stag_solo, agg_stag_no_ctrl)
    agg_stag_by_t_no_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['no_ctrl']], matched_data, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_by_t_solo <- rbind(agg_stag_by_t_solo, agg_stag_by_t_no_ctrl)
    for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
      p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_no_ctrl,var, '_', treat, '_by_t',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    agg_stag_by_g_no_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['no_ctrl']], matched_data, by  ='g')%>%
      .[, var := var] %>% .[, ctrl := 'None']
    agg_stag_by_g_solo <- rbind(agg_stag_by_g_solo, agg_stag_by_g_no_ctrl)
    for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
      p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= g, y = est))+
        geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('First treatment period')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_no_ctrl,var, '_', treat, '_by_g',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    
    
    
    
    agg_stag_ctrl <- agg_etwfe(list_es_solo[[d]][[var]][['ctrl']], matched_data, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    
    agg_stag_solo <- rbind(agg_stag_solo, agg_stag_ctrl)
    agg_stag_by_t_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['ctrl']], matched_data, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    agg_stag_by_t_solo <- rbind(agg_stag_by_t_solo, agg_stag_by_t_ctrl)
    
    for(treat in unique(agg_stag_by_t_ctrl$treatment)){
      p <- ggplot(agg_stag_by_t_ctrl %>% .[treatment %in% c(treat)])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_ctrl,var, '_', treat, '_by_t',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    
    agg_stag_by_g_ctrl <- agg_etwfe_het(list_es_solo[[d]][[var]][['ctrl']], matched_data, by  ='g')%>%
      .[, var := var] %>% .[, ctrl := fe_large]
    agg_stag_by_g_solo <- rbind(agg_stag_by_g_solo, agg_stag_by_g_ctrl)
    
    for(treat in unique(agg_stag_by_g_ctrl$treatment)){
      p <- ggplot(agg_stag_by_g_ctrl %>% .[treatment %in% c(treat) ])+
        geom_point(aes(x= g, y = est))+
        geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
        geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Treatment cohort')+ ylab('Estimate and 95% CI')+
        theme_bw()
      pdf(paste0(var_path_ctrl,var, '_', treat, '_by_g',".pdf"))
      print(p)
      dev.off() 
      rm(p)
    }
    gc()
    
  }}

n_obs_solo <- list()
r_2_solo <- list()
for(d in names(list_es_solo)){
  n_obs_solo[[d]] <- list()
  r_2_solo[[d]] <- list()
  for(var in names(list_es_solo[[d]])){
    n_obs_solo[[d]][[ paste0(var, " | ", fe_min)]] <- list_es_solo[[d]][[var]][['no_ctrl']]$nobs
    n_obs_solo[[d]][[ paste0(var, " | ", fe_large)]] <- list_es_solo[[d]][[var]][['ctrl']]$nobs
    r_2_solo[[d]][[ paste0(var, " | ", fe_min)]] <-   round(list_es_solo[[d]][[var]][['no_ctrl']]$pseudo_r2, 5)
    r_2_solo[[d]][[ paste0(var, " | ", fe_large)]] <- round(list_es_solo[[d]][[var]][['ctrl']]$pseudo_r2   , 5)
  }
  make_stargazer_like_table_dt(unique(agg_stag_solo %>% .[str_detect(treat, d)] %>%
                                        .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                               var_map = dict_vars, 
                               treat_map = dict_vars, 
                               # var_order = outcomes, 
                               pre_mean = pre_mean[[d]],
                               n_obs = n_obs_solo[[d]],
                               r_2 = r_2_solo[[d]],
                               drop_unlisted_vars = TRUE,
                               save_path = paste0('D:\\panel_fr_res\\lab_results\\agg_mobility_solo_', d, '.tex')
  )
  
}


# Estimating all treatments as one for multiply treated institutions --------


sample_acces_rce_idex <- ds_clean[!str_detect(domain, ',') & !(acces_rce_r %in% c(2014,2015)) & !(acces_rce_s %in% c(2014,2015))] %>%
  .[, any_treatment_s := as.numeric(acces_rce_s !=0 | date_first_idex_s != 0 | fusion_date_s !=0)] %>%
  .[, any_treatment_r := as.numeric(acces_rce_r !=0 | date_first_idex_r != 0 | fusion_date_r !=0)] %>%
  .[, ':='(treated_s = as.numeric(acces_rce_s != 0 & date_first_idex_s != 0 & fusion_date_s == 0),
           treated_r = as.numeric(acces_rce_r != 0 & date_first_idex_r != 0 & fusion_date_r == 0))
    ] %>%
  .[(any_treatment_s == 0 & any_treatment_r == 0) |
      (treated_r & (any_treatment_s ==0 |treated_s ==1) )
    | (treated_s & (any_treatment_r ==0 |treated_r ==1) )

      ] 
  
gc()
d <- 'acces_rce'
## first treatment is acces_rce
treatment_values_s <- sort(unique(sample_acces_rce_idex[sample_acces_rce_idex[[paste0(d, '_s')]] !=0][[paste0(d, '_s')]]))
treatment_values_r <- sort(unique(sample_acces_rce_idex[sample_acces_rce_idex[[paste0(d, '_r')]] !=0][[paste0(d, '_r')]]))

formula_elements <- c()
for(g_i in treatment_values_s){
  print(paste0(d, ': ', g_i))
  varname =paste0(d, '_s_g', g_i)
  ref = as.character(as.numeric(g_i)-1)
  sample_acces_rce_idex[[varname]] <- as.numeric((sample_acces_rce_idex[[paste0(d, '_s')]] == g_i)
                                           & (sample_acces_rce_idex[['merged_inst_id_r']] != "abroad")
                                           
  )
  formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  
  varname_abroad =paste0(d, '_a_s_g', g_i)
  sample_acces_rce_idex[[varname_abroad]] <- as.numeric((sample_acces_rce_idex[[paste0(d, '_s')]] == g_i)
                                                  & (sample_acces_rce_idex[['merged_inst_id_r']] == "abroad")
  )
  formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
  
  
}
for(g_j in treatment_values_s){
  print(paste0(d, ': ', g_j))
  varname =paste0(d, '_r_g', g_j)
  ref = as.character(as.numeric(g_j)-1)
  sample_acces_rce_idex[[varname]] <- as.numeric((sample_acces_rce_idex[[paste0(d, '_r')]] == g_j)
                                           & (sample_acces_rce_idex[['merged_inst_id_s']] != "abroad")
                                           & (sample_acces_rce_idex[['merged_inst_id_s']] != "entry")
  )
  formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  
  varname_abroad =paste0(d, '_a_r_g', g_j)
  sample_acces_rce_idex[[varname_abroad]] <- as.numeric((sample_acces_rce_idex[[paste0(d, '_r')]] == g_j)
                                                  & (sample_acces_rce_idex[['merged_inst_id_s']] == "abroad")
  )
  formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
  
  varname_entry =paste0(d, '_e_r_g', g_j)
  sample_acces_rce_idex[[varname_entry]] <- as.numeric((sample_acces_rce_idex[[paste0(d, '_r')]] == g_j)
                                                 & (sample_acces_rce_idex[['merged_inst_id_s']] == "entry")
  )
  formula_elements <- c(formula_elements, paste0(varname_entry, ' + i(year,', varname_entry, ',ref=',ref,')'))
  
  
}    

units <- unique(sample_acces_rce_idex[, ..unit_cols]) %>%
  .[, treat := as.numeric(acces_rce_r != 0 | date_first_idex_r !=0 | fusion_date_r != 0
                          |acces_rce_s != 0 | date_first_idex_s !=0 | fusion_date_s != 0 )]
print(table(units[[paste0(d, '_r')]], units[[paste0(d, '_s')]]))
match_units <- matchit(treat ~
                         domain + type_r + type_s
                       ,data = units
                       ,method = "exact",
)
matched_units <- match.data(match_units)
print(table(matched_units[[paste0(d, '_r')]], matched_units[[paste0(d, '_s')]]))

matched_units <- matched_units %>%
  .[, list(unit, subclass)]

matched_data <- merge(sample_acces_rce_idex, matched_units, by = c('unit'))

gc()

for(var in c('movers_w') ){

  formula_no_ctrl <- as.formula(paste0(var, '~ ',  paste0(formula_elements, collapse= '+'), fe_min, ' + subclass^year'))
  formula_ctrl <- as.formula(paste0(var, ' ~ ',  paste0(formula_elements, collapse= '+'), fe_large, '+ subclass^year'))
  
  
  start_time <- Sys.time()
  es_stag <- fepois(formula_no_ctrl
                    , data = matched_data
                    ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                    ,cluster = c('unit')
  ) 
  time_taken <- Sys.time() - start_time
  print(time_taken)
  gc()

  start_time <- Sys.time()
  es_stag_w_ctrl <- fepois(formula_ctrl,
                           , data = matched_data 
                           ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                           ,cluster = c('unit')
  ) 
  time_taken <- Sys.time()-start_time
  gc()
  print(time_taken)
}
agg_stag_by_t_no_ctrl <- agg_etwfe_het(es_stag, matched_data, by  ='t', t_limit = 5)%>%
  .[, var := var] %>% .[, ctrl := 'None']
for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
  p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
    geom_point(aes(x= t, y = est))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
    geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  print(p)
  rm(p)
}
gc()
agg_stag_by_t_ctrl <- agg_etwfe_het(es_stag_w_ctrl, matched_data, by  ='t', t_limit = 5)%>%
  .[, var := var] %>% .[, ctrl := 'None']
for(treat in unique(agg_stag_by_t_ctrl$treatment)){
  p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
    geom_point(aes(x= t, y = est))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
    geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  print(p)
  rm(p)
}
gc()
