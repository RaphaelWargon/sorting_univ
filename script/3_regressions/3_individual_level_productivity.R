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

sample_df_peers <- fread("E:\\panel_fr_res\\sample_df.csv") 
gc()

length(unique(sample_df_peers$author_id)) #265916
nrow(unique(sample_df_peers[, list(merged_inst_id, field)])) #2868


ggplot(unique(sample_df_peers[, list(author_id, n_obs_au,entry_year)][n_obs_au<20]))+geom_density(aes(x=n_obs_au, group = entry_year,color = entry_year))

# Staggered design regression to estimate treatment effects ---------------

####### Prepare dataset for staggered design regression

table(unique(sample_df_peers[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df_peers[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df_peers[, list(merged_inst_id, field, acces_rce,date_first_idex,fusion_date)])$fusion_date)


sample_df_reg <- sample_df_peers %>%
  .[!(acces_rce %in%  c(2015))
    & !(date_first_idex %in% c(2014))] %>%
  .[, pub_04_07 := sum(as.numeric(year > 2004 & year <= 2007) * publications_raw ), by = 'author_id'] %>%
  #.[pub_04_07 >=2] %>%
  .[, n_lt := n_distinct(author_id), by = c('merged_inst_id','field','year')] %>%
  .[, merged_inst_id_field := paste0(merged_inst_id, '_', field)] %>%
  .[, fusion_date := fifelse(fusion_date =="2023", "0", as.character(fusion_date))] %>%
  .[year != "2020"] %>%
  .[ , ':='(fusion_date = as.factor(fusion_date),
            date_first_idex = as.factor(date_first_idex),
            acces_rce = as.factor(acces_rce),
            year = as.factor(year))]

length(unique(sample_df_reg$author_id)) #90515
nrow(unique(sample_df_reg[, list(merged_inst_id, field)])) #2829

ggplot(sample_df_reg %>% .[, .(total_publis = sum(publications_raw)), by = 'author_id'])+
  geom_density(aes(x=log(total_publis)))


#rm(sample_df_peers)

list_g = list( "acces_rce" = sort(unique(sample_df_reg[acces_rce !=0]$acces_rce))
               ,"date_first_idex" = sort(unique(sample_df_reg[date_first_idex !=0]$date_first_idex))
               , "fusion_date" = sort(unique(sample_df_reg[fusion_date !=0]$fusion_date))
)
gc()


formula_elements <- c()
for(d in c('acces_rce'#
           , 'date_first_idex', 'fusion_date'#,'rce_idex'
)){
  for(g_i in list_g[[d]]){
    print(paste0(d, ': ', g_i))
    varname =paste0(d, '_', g_i)
    ref = as.character(as.numeric(g_i)-1)
    sample_df_reg[[varname]] <- as.numeric((sample_df_reg[[paste0(d)]] == g_i))
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  }}  
length(formula_elements)

gc()


fe_min = ' | merged_inst_id_field + author_id + year'
fe_large = paste0(  ' | merged_inst_id_field + '
                    ,'year +author_id '
                    ,'+ type^year '
                    ,'+ gender^year'
                    ,'+ public^year'
                    ,'+ ecole^year'
                    ,'+ cnrs^year'
                    ,'+ field^year'
                    ,'+ entry_year^year '
                    ,'+ city^year'
                    
)
gc()

formula_no_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_min))
formula_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_large))
list_es = list()

agg_stag <- data.table(treat = '', est = 0, std = 0, t= 0, pvalue = 0, pvalue_pretrend= 0, type = '',  var = '', ctrl = '') %>% .[treat != '']
agg_stag_by_t <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0,  t= 0, n = '', var = '',  ctrl = '') %>% .[treatment != '']
agg_stag_by_g <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0, g = 0, n = '', var = '',  ctrl = '') %>% .[treatment != '']

dict_vars <- c('acces_rce'= 'University autonomy',
               'date_first_idex'='Received an IDEX',
               'fusion_date'= "Merged establishment",
               "publications_raw" = 'Publications',
               "citations_raw"='Citations',
               "publications" = 'Publications',
               "citations"='Citations',
               "avg_publications" = 'Average publications',
               "avg_citations"='Average citations',
               "n_au" = "Number of researchers",
               "nr_source_btm_50pct_raw"="Bottom 50% journal publications",
               "nr_source_mid_40pct_raw"="Middle 40% journal publications",
               "nr_source_top_20pct_raw"="Top 20% journal publications", 
               "nr_source_top_10pct_raw" ="Top 10% journal publications", 
               "nr_source_top_5pct_raw" ="Top 5% journal publications",
               "nr_source_btm_50pct"="Bottom 50% journal publications",
               "nr_source_mid_40pct"="Middle 40% journal publications",
               "nr_source_top_20pct"="Top 20% journal publications", 
               "nr_source_top_10pct" ="Top 10% journal publications", 
              "nr_source_top_5pct" ="Top 5% journal publications",
               "type" = 'Institution Type',
               "city"=  'City',
               'cnrs'='CNRS',
               'public'='Public Status',
               'ecole'='Grande Ecole status',
               'main_topic'='Main field',
               "|merged_inst_id"= 'Institution',
               " |merged_inst_id"= 'Institution',
               "|merged_inst_id_field"= 'Institution $\\times$ field',
               " |merged_inst_id"= 'Institution $\\times$ field'
)
gc()

outcomes <- c('publications_raw',
  'publications'
  ,'citations', 'citations_raw',
  'nr_source_top_5pct', 'nr_source_top_5pct_raw'
)
for(var in outcomes){
  no_ctrl_path = "E:\\panel_fr_res\\productivity_results\\individual\\no_ctrl\\"
  ctrl_path = "E:\\panel_fr_res\\productivity_results\\individual\\ctrl\\"
  if (!file.exists(no_ctrl_path)){
    dir.create(no_ctrl_path, recursive = TRUE)
  }
  if (!file.exists(ctrl_path)){
    dir.create(ctrl_path, recursive = TRUE)
  }
  
  list_es[[var]] <- list()
  
  sample_df_reg$y <- sample_df_reg[[var]]

  sample_df_reg <- sample_df_reg %>%
  .[, ':='(y_lt = sum(y)), by = c('merged_inst_id',"field", 'year')] %>%
  .[, y_minus_i_lt := (y_lt - y)/(1-n_lt)]


  start_time <- Sys.time()
  es_stag <- fepois(formula_no_ctrl
                  , data = sample_df_reg
                  ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                  ,cluster = c('merged_inst_id',"field",'author_id')
  ) 
  time_taken <- Sys.time() - start_time
  print(time_taken)
  gc()
  list_es[[var]][['no_ctrl']] <- es_stag
  
  start_time <- Sys.time()
  es_stag_w_ctrl <- fepois(formula_ctrl,
                          , data = sample_df_reg 
                          ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                          ,cluster = c('author_id','merged_inst_id_field')
  ) 
  time_taken <- Sys.time()-start_time
  gc()
  print(time_taken)
  list_es[[var]][['ctrl']] <- es_stag_w_ctrl
}


source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))

for(var in outcomes){
  
  agg_stag_no_ctrl <- agg_effects(list_es[[var]][['no_ctrl']], sample_df_reg)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag <- rbind(agg_stag, agg_stag_no_ctrl)
  agg_stag_by_t_no_ctrl <- agg_effect_het(list_es[[var]][['no_ctrl']], sample_df_reg, by  ='t')%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_no_ctrl)
  for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat) & abs(t)<=7])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(no_ctrl_path, var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  agg_stag_by_g_no_ctrl <- agg_effect_het(list_es[[var]][['no_ctrl']], sample_df_reg, by  ='g')%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag_by_g <- rbind(agg_stag_by_g, agg_stag_by_g_no_ctrl)
  for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
    p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= g, y = est))+
      geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('First treatment period')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(no_ctrl_path, var, '_', treat , "_", 'by_g',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  
  
  
  
  agg_stag_ctrl <- agg_effects(list_es[[var]][['ctrl']], sample_df_reg, t_limit =7)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  
  agg_stag <- rbind(agg_stag, agg_stag_ctrl)
  agg_stag_by_t_ctrl <- agg_effect_het(list_es[[var]][['ctrl']], sample_df_reg, by  ='t')%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_ctrl)
  
  for(treat in unique(agg_stag_by_t_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_ctrl %>% .[treatment %in% c(treat) & abs(t)<=7])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
      geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(ctrl_path,var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  
  agg_stag_by_g_ctrl <- agg_effect_het(list_es[[var]][['ctrl']], sample_df_reg, by  ='g')%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  agg_stag_by_g <- rbind(agg_stag_by_g, agg_stag_by_g_ctrl)
  
  for(treat in unique(agg_stag_by_g_ctrl$treatment)){
    p <- ggplot(agg_stag_by_g_ctrl %>% .[treatment %in% c(treat) ])+
      geom_point(aes(x= g, y = est))+
      geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Treatment cohort')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(ctrl_path,var, '_', treat , "_", 'by_g',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
}



pre_mean <- list() 
for(var in names(list_es)){
  pre_mean[[var]] <- round(mean((sample_df_reg[year < 2009])[[var]], na.rm =T),2)
}
n_obs <- list()
r_2 <- list()
for(var in names(list_es)){
  n_obs[[ paste0(var, " | ", fe_min)]] <- list_es[[var]][['no_ctrl']]$nobs
  n_obs[[ paste0(var, " | ", fe_large)]] <- list_es[[var]][['ctrl']]$nobs
  r_2[[ paste0(var, " | ", fe_min)]] <-   round(list_es[[var]][['no_ctrl']]$pseudo_r2, 5)
  r_2[[ paste0(var, " | ", fe_large)]] <- round(list_es[[var]][['ctrl']]$pseudo_r2   , 5)
}

make_stargazer_like_table_dt(agg_stag %>%
                               .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)], 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             var_order = c('publications','citations','nr_source_top_5pct'), 
                             drop_unlisted_vars = TRUE,
                             save_path = 'E:\\panel_fr_res\\productivity_results\\individual\\agg_prod.tex'
)


make_stargazer_like_table_dt(agg_stag %>%
                               .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)], 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             var_order = c('publications_raw','citations_raw','nr_source_top_5pct_raw'), 
                             drop_unlisted_vars = TRUE,
                             save_path = 'E:\\panel_fr_res\\productivity_results\\individual\\agg_prod_raw.tex'
)

# Cohort heterogeneity -----------------------------------------------------------

all_coefs <- as.data.table(list_es[[var]][['no_ctrl']]$coeftable, keep.rownames = TRUE)%>%
  .[, d := str_extract(rn, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
  # .[, d := str_extract(var, '(?<=year[0-9]{4}:)[a-z_]+(?=[0-9])|^[a-z_]+(?=[0-9]{4}:year)')]%>%
  .[, g := str_extract(rn, paste0('(?<=' , d, '_)[0-9]{4}')) ] %>%
  .[, year := str_extract(rn, '(?<=year::)[0-9]{4}')] %>%
  # .[, year := str_extract(var, '(?<=year)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] %>%
  .[, ':='(est = Estimate,
           std = `Std. Error`)]
for( d_plot in c('acces_rce','date_first_idex','fusion_date')){
for(g_plot in sort(unique( (all_coefs %>% .[d==d_plot])$g )) ){
p <- ggplot(all_coefs %>% .[d==d_plot & abs(t)<7 & g == g_plot])+
  geom_point(aes(x= t, y = est))+
  geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
  geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
  labs(title = paste0('Treatment: ', dict_vars[[d_plot]], ' for cohort ', g_plot))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
  theme_bw()
print(p)
}}

saveRDS(list_es, file = "E:\\panel_fr_res\\productivity_results\\individual\\all_regressions.rds")

# Heterogeneity -----------------------------------------------------------


alpha_hats <- unique(sample_df_reg$alpha_hat)

sample_df_reg <- sample_df_reg %>% .[, quantile_au_fe := cut(alpha_hat, breaks = quantile(alpha_hats, c(0:4/4), include.lowest = TRUE),
                                                             labels = lapply(1:4, as.character))]
list_es = list()

formula_het_no_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + quantile_au_fe*(', 
                                          paste0(formula_elements, collapse= '+'), ')', fe_min))
formula_het_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + quantile_au_fe*(', 
                                       paste0(formula_elements, collapse= '+'), ')',
                                       fe_large))
sample_df_reg$y <- sample_df_reg[["citations"]]

sample_df_reg <- sample_df_reg %>%
  .[, ':='(y_lt = sum(y)), by = c('inst_id_field','year')] %>%
  .[, y_minus_i_lt := (y_lt - y)/(1-n_lt)]

gc()
start_time <- Sys.time()
es_stag_het <- fepois(formula_het_no_ctrl
                  , data = sample_df_reg
                  ,cluster = c('inst_id','author_id')
) 
time_taken <- Sys.time() - start_time
print(time_taken)
gc()
coefs <- as.data.table(es_stag_het$coeftable, keep.rownames = TRUE)
coefs$var <- coefs$rn
coefs <- coefs %>%
  .[ str_detect(var, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
  .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
  .[, g := str_extract(var, '(?<=_)[0-9]+$')] %>%
  .[, quant := str_extract(var, '(?<=quantile_au_fe)[0-9]')] %>%
  .[, quant := fifelse(is.na(quant), 'global', quant)] %>%
  .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] 

agg_stag_no_ctrl <- agg_effect_het(es_stag_het, by = 'quant', sample_df_reg)%>%
  .[, var := var] %>% .[, ctrl := 'None']

p <- ggplot(agg_stag_no_ctrl %>% .[treatment %in% c(treat)])+
  geom_point(aes(x= quant, y = est))+
  geom_errorbar(aes(x=quant, ymin = est -1.96*std, ymax=est+1.96*std))+
  geom_hline(aes(yintercept = 0))+
  labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Quantile')+ ylab('Estimate and 95% CI')+
  theme_bw()
p
for(treat in names(list_g)){
  print(ggplot(coefs %>% .[d %in% c(treat) & `Std. Error` <100])+
  geom_point(aes(x= t, y = Estimate, color = quant))+
  geom_errorbar(aes(x=t, ymin = Estimate -1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`, color = quant))+
  geom_hline(aes(yintercept = 0))+
  labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
  theme_bw())
  print(ggplot(agg_stag_no_ctrl %>% .[treatment %in% c(treat)])+
          geom_point(aes(x= quant, y = est))+
          geom_errorbar(aes(x=quant, ymin = est -1.96*std, ymax=est+1.96*std))+
          geom_hline(aes(yintercept = 0))+
          labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Quantile')+ ylab('Estimate and 95% CI')+
          theme_bw())
  }



