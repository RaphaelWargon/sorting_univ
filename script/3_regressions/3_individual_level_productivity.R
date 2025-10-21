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
       'boot',
       'DIDmultiplegt',
       "DIDmultiplegtDYN"#,'didimputation'
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

sample_df_peers <- fread("E:\\panel_fr_res\\sample_df_reg.csv") 
gc()

length(unique(sample_df_peers$author_id))
length(unique(sample_df_peers$inst_id_field))


# Staggered design regression to estimate treatment effects ---------------

####### Prepare dataset for staggered design regression

table(unique(sample_df_peers[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df_peers[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df_peers[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$fusion_date)


sample_df_reg <- sample_df_peers %>%
  .[!(acces_rce %in%  c(2015))
    & !(date_first_idex %in% c(2014))] %>%
  .[, log_publications := log(publications)] %>%
  .[, n_lt := n_distinct(author_id), by = c('inst_id_field','year')] %>%
  .[, fused_inst_id := fifelse(is.na(fused_inst_id), inst_id, fused_inst_id)] %>%
  .[, fused_inst_id_field := paste0(fused_inst_id, '_', field)]

list_g = list( "acces_rce" = sort(unique(sample_df_reg[acces_rce !=0]$acces_rce))
               ,"date_first_idex" = sort(unique(sample_df_reg[date_first_idex !=0]$date_first_idex))
               , "fusion_date" = sort(unique(sample_df_reg[fusion_date !=0]$fusion_date))
)
gc()

fe_min = ' | fused_inst_id_field + author_id + year'
fe_large = paste0(  ' | fused_inst_id_field + '
                    ,'year +author_id '
                    ,'+ type^year '
                    ,'+ gender^year'
                    ,'+ public^year'
                    ,'+ ecole^year'
                    ,'+ cnrs^year'
                    ,'+ field^year'
                    #,'+ entry_year_year '
                    #,'+ city_year'
                    
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

formula_no_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + ', paste0(formula_elements, collapse= '+'), fe_min))
formula_ctrl <- as.formula(paste0( 'y ~ y_minus_i_lt + ', paste0(formula_elements, collapse= '+'), fe_large))
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
               "|fused_inst_id"= 'Institution',
               " |fused_inst_id"= 'Institution',
               "|fused_inst_id_field"= 'Institution $\\times$ field',
               " |fused_inst_id"= 'Institution $\\times$ field'
)
gc()

for(var in c('publications','citations','publications_raw', 'citations_raw',
             'nr_source_top_5pct', 'nr_source_top_5pct_raw'
             )
    ){
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
  .[, ':='(y_lt = sum(y)), by = c('inst_id_field','year')] %>%
  .[, y_minus_i_lt := (y_lt - y)/(1-n_lt)]


  start_time <- Sys.time()
  es_stag <- fepois(formula_no_ctrl
                  , data = sample_df_reg
                  ,cluster = c('inst_id','author_id')
  ) 
  time_taken <- Sys.time() - start_time
  print(time_taken)
  gc()
  list_es[[var]][['no_ctrl']] <- es_stag

  agg_stag_no_ctrl <- agg_effects(es_stag, sample_df_reg)%>%
  .[, var := var] %>% .[, ctrl := 'None']

  agg_stag <- rbind(agg_stag, agg_stag_no_ctrl)
  agg_stag_by_t_no_ctrl <- agg_effect_het(es_stag, sample_df_reg, by  ='t')%>%
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
  
  agg_stag_by_g_no_ctrl <- agg_effect_het(es_stag, sample_df_reg, by  ='g')%>%
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
  
  
  start_time <- Sys.time()
  es_stag_w_ctrl <- fepois(formula_ctrl,
                          , data = sample_df_reg 
                          ,cluster = c('author_id','inst_id')
  ) 
  time_taken <- Sys.time()-start_time
  gc()
  time_taken
  list_es[[var]][['ctrl']] <- es_stag_w_ctrl
  
  agg_stag_ctrl <- agg_effects(es_stag_w_ctrl, sample_df_reg)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  agg_stag <- rbind(agg_stag, agg_stag_ctrl)
  agg_stag_by_t_ctrl <- agg_effect_het(es_stag_w_ctrl, sample_df_reg, by  ='t')%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_ctrl)
  
  
  for(treat in unique(agg_stag_by_t_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_ctrl %>% .[treatment %in% c(treat) & abs(t)<=7])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(ctrl_path,var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  agg_stag_by_g_ctrl <- agg_effect_het(es_stag_w_ctrl, sample_df_reg, by  ='g')%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  agg_stag_by_g <- rbind(agg_stag_by_g, agg_stag_by_g_ctrl)
  
  for(treat in unique(agg_stag_by_g_ctrl$treatment)){
    p <- ggplot(agg_stag_by_g_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= g, y = est))+
      geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('First treatment period')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(ctrl_path,var, '_', treat , "_", 'by_g',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }

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

# Exploratory -----------------------------------------------------------

test <- unique(sample_df_reg %>% .[, list(fused_inst_id, inst_id,name, acces_rce, date_first_idex, fusion_date)] ) 

nrow(sample_df_peers %>% .[inst_id == "I68947357"])
test2 <- as.data.table(open_dataset(inputpath) %>% select(fused_inst_id, inst_id,name, acces_rce, date_first_idex, fusion_date))
test2 <- unique(test2)

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
