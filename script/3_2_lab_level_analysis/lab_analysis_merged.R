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


# I) Loading data ------------------------------------------------------------

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/etwfe_functions.R'))


inputpath <- "E:\\panel_fr_res\\inst_level_flows.parquet"

ds <- open_dataset(inputpath) %>%
  filter(!is.na(type_r) & !is.na(type_s) & year >=2003) %>%
  group_by(merged_inst_id_r, merged_inst_id_s) %>%
  mutate(max_pair = max(total), entry_year_pair = min(year)) %>%
  ungroup() %>%
  filter(entry_year_pair <= 2003 & max_pair >0)
gc()
ds <- as.data.table(ds)
nrow(ds)


ds <- ds %>%
  .[,size_s := max(as.numeric(merged_inst_id_s == merged_inst_id_r)*stayers, na.rm = T), by = c('merged_inst_id_s','year')]%>%
  .[,size_r := max(as.numeric(merged_inst_id_s == merged_inst_id_r)*stayers, na.rm = T), by = c('merged_inst_id_r','year')] %>%
  .[, size_r := ifelse(size_r <= 0, sum(stayers_w), size_r),by = c('merged_inst_id_r','year') ]%>%
  .[, size_s := ifelse(size_s <= 0, sum(stayers_w), size_s),by = c('merged_inst_id_s','year') ] %>%
  .[!is.na(merged_inst_id_s) & !is.na(merged_inst_id_r)] %>%
  .[, unit := paste0(merged_inst_id_s, '_to_',merged_inst_id_r)] %>%
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
  )]
gc()

ds_clean <- ds_clean %>%
  .[, ':='(
    acces_rce_a_s = fifelse(acces_rce_s != '0' & merged_inst_id_r == 'abroad', acces_rce_s, "0"),
    acces_rce_a_r = fifelse(acces_rce_r != '0' & merged_inst_id_s == 'abroad', acces_rce_r, "0"),
    
    date_first_idex_a_s = fifelse(date_first_idex_s != '0' & merged_inst_id_r == 'abroad', date_first_idex_s, "0"),
    date_first_idex_a_r = fifelse(date_first_idex_r != '0' & merged_inst_id_s == 'abroad', date_first_idex_r, "0"),
    
    fusion_date_a_s = fifelse(fusion_date_s != '0' & merged_inst_id_r == 'abroad', fusion_date_s, "0"),
    fusion_date_a_r = fifelse(fusion_date_r != '0' & merged_inst_id_s == 'abroad', fusion_date_r, "0")
  )]


list_g = list("acces_rce" = list( i = sort(unique(ds_clean[acces_rce_s !=0]$acces_rce_s)),
                                  j = sort(unique(ds_clean[acces_rce_r !=0]$acces_rce_r)))
              
              ,"date_first_idex" = list( i = sort(unique(ds_clean[date_first_idex_s !=0]$date_first_idex_s)),
                                         j =   sort(unique(ds_clean[date_first_idex_r !=0]$date_first_idex_r)))
              
              , "fusion_date" = list( i = sort(unique(ds_clean[fusion_date_s !=0]$fusion_date_s)),
                                      j = sort(unique(ds_clean[fusion_date_r !=0]$fusion_date_r)))
)
gc()
formula_elements <- c()
for(d in names(list_g) ){
  for(g_i in list_g[[d]][['i']]){
    print(paste0(d, ': ', g_i))
    varname =paste0(d, '_s_g', g_i)
    ref = as.character(as.numeric(g_i)-1)
    ds_clean[[varname]] <- as.numeric((ds_clean[[paste0(d, '_s')]] == g_i)
                                      & (ds_clean[['merged_inst_id_r']] != "abroad")
                                      )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))

    varname_abroad =paste0(d, '_a_s_g', g_i)
    ds_clean[[varname_abroad]] <- as.numeric((ds_clean[[paste0(d, '_s')]] == g_i)
                                      & (ds_clean[['merged_inst_id_r']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
  }
  for(g_j in list_g[[d]][['j']]){
    print(paste0(d, ': ', g_j))
    varname =paste0(d, '_r_g', g_j)
    ref = as.character(as.numeric(g_j)-1)
    ds_clean[[varname]] <- as.numeric((ds_clean[[paste0(d, '_r')]] == g_j)
                                      & (ds_clean[['merged_inst_id_s']] != "abroad")
                                      )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    
    varname_abroad =paste0(d, '_a_r_g', g_j)
    ds_clean[[varname_abroad]] <- as.numeric((ds_clean[[paste0(d, '_r')]] == g_j)
                                      & (ds_clean[['merged_inst_id_s']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
  }
  
  
  }  
length(formula_elements)
gc()

rm(ds)
gc()

# Regression --------------------------------------------------------------


list_es <- list()

outcomes <- c("movers_w", 
              "movers_w_foreign_entrant", "movers_w_junior", 'movers_w_medium', 'movers_w_senior')
for(var in outcomes){
list_es[[var]] <- list()
var_path = paste0("E:\\panel_fr_res\\lab_results\\full_no_fe\\", var)
if (!file.exists(var_path)){
  dir.create(var_path, recursive = TRUE)
}

fe_min <- '| year + merged_inst_id_r  +merged_inst_id_s + unit'
fe_large <- paste0(fe_min, '+ type_s^year + type_r^year + public_s^year + public_r^year + city_s^year + city_r^year')
start_time <- Sys.time()

es_stag <- fepois( as.formula(paste0(var, ' ~ ', paste0(formula_elements, collapse= '+'), fe_min))
                   , data = ds_clean,
                   mem.clean = TRUE, lean = TRUE, fixef.tol = 1E-2
                 ,cluster = c('unit')
) 

time_taken <- Sys.time() - start_time
time_taken

list_es[[var]][['no_ctrl']] <- es_stag


start_time <- Sys.time()

es_stag_ctrl <- fepois( as.formula(paste0(var, ' ~ ', paste0(formula_elements, collapse= '+'), fe_large))
                        , data = ds_clean,
                        mem.clean = TRUE, lean = TRUE, fixef.tol = 1E-2
                        ,cluster = c('unit')
) 

time_taken <- Sys.time() - start_time
time_taken

list_es[[var]][['ctrl']] <- es_stag_ctrl


}
saveRDS(list_es, file = "E:\\panel_fr_res\\lab_results\\all_regressions.rds")
list_es <- readRDS("E:\\panel_fr_res\\lab_results\\all_regressions.rds")

agg_stag <- data.table(treat = '', est = 0, std = 0, t= 0, pvalue = 0, var = '', ctrl = '') %>% .[treat != '']
agg_stag_by_t <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0,  t= 0,var = '',  ctrl = '') %>% .[treatment != '']
agg_stag_by_g <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0, g = 0,var = '',  ctrl = '') %>% .[treatment != '']

for( var in outcomes){
  print(var)
  var_path_no_ctrl= paste0("E:\\panel_fr_res\\lab_results\\full_no_fe\\", var, '\\')
  if (!file.exists(var_path_no_ctrl)){
    dir.create(var_path_no_ctrl, recursive = TRUE)
  }
  var_path_ctrl= paste0("E:\\panel_fr_res\\lab_results\\ctrl\\", var, '\\')
  if (!file.exists(var_path_ctrl)){
    dir.create(var_path_ctrl, recursive = TRUE)
  }
  
  excluded_vars <- names(list_es[[var]][["ctrl"]]$coefficients)[str_detect(names(list_es[[var]][["ctrl"]]$coefficients),
                                                                           paste0('acces_rce_r_g2014|acces_rce_s_g2014|acces_rce_a_r_g2014|acces_rce_a_s_g2014',
                                                                           "|date_first_idex_r_g2014|date_first_idex_s_g2014|date_first_idex_a_r_g2014|date_first_idex_a_s_g2014",
                                                                           "|fusion_date_r_g2012|fusion_date_s_g2012|fusion_date_a_r_g2012|fusion_date_a_s_g2012",
                                                                           '|2020')
                                                                           
                                                                           )]
  for(excluded_var in excluded_vars){
  list_es[[var]][["no_ctrl"]]$coefficients[[excluded_var]] <- NA
  list_es[[var]][["ctrl"]]$coefficients[[excluded_var]] <- NA
  
}

  agg_stag_no_ctrl <- agg_etwfe(list_es[[var]][['no_ctrl']], ds_clean, t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag <- rbind(agg_stag, agg_stag_no_ctrl)
  agg_stag_by_t_no_ctrl <- agg_etwfe_het(list_es[[var]][['no_ctrl']], ds_clean, by  ='t', t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_no_ctrl)
  for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(var_path_no_ctrl, var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  agg_stag_by_g_no_ctrl <- agg_etwfe_het(list_es[[var]][['no_ctrl']], ds_clean, by  ='g')%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag_by_g <- rbind(agg_stag_by_g, agg_stag_by_g_no_ctrl)
  for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
    p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= g, y = est))+
      geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('First treatment period')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(var_path_no_ctrl, var, '_', treat , "_", 'by_g',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  
  
  agg_stag_ctrl <- agg_etwfe(list_es[[var]][["ctrl"]], data = ds_clean, t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  
  agg_stag <- rbind(agg_stag, agg_stag_ctrl)
  agg_effect_by_t_ctrl <- agg_etwfe_het(list_es[[var]][["ctrl"]], 't', data = ds_clean, t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  gc()
  
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_effect_by_t_ctrl)%>%
    .[, var := var] %>% .[, ctrl := fe_large]

  
  for(treat in unique(agg_effect_by_t_ctrl$treatment)){
    p <- ggplot(agg_effect_by_t_ctrl %>% .[treatment %in% c(treat) ])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
      geom_vline(aes(xintercept = "-1"), linetype = 'dashed')+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(var_path_ctrl, var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  
  agg_effect_by_g_ctrl <- agg_etwfe_het(list_es[[var]][["ctrl"]], 'g', data = ds_clean)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  gc()
  agg_stag_by_g <- rbind(agg_stag_by_g, agg_effect_by_g_ctrl)
  
  for(treat in unique(agg_effect_by_g_ctrl$treatment)){
    p <- ggplot(agg_effect_by_g_ctrl %>% .[treatment %in% c(treat) ])+
      geom_point(aes(x= g, y = est))+
      geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Treatment cohort')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(var_path_ctrl, var, '_', treat , "_", 'by_g',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
}



pre_mean <- list()
for(var in names(list_es)){
  pre_mean[[var]] <- round(mean((ds_clean[year < 2009] %>%
                                   filter(!!as.symbol(var) < Inf & !!as.symbol(var) >-Inf))[[var]], na.rm =T),2)
}
n_obs <- list()
r_2 <- list()
for(var in names(list_es)){
  n_obs[[ paste0(var, " | ", fe_min)]] <- list_es[[var]][['no_ctrl']]$nobs
  n_obs[[ paste0(var, " | ", fe_large)]] <- list_es[[var]][['ctrl']]$nobs
  r_2[[ paste0(var, " | ", fe_min)]] <-   round(list_es[[var]][['no_ctrl']]$pseudo_r2, 5)
  r_2[[ paste0(var, " | ", fe_large)]] <- round(list_es[[var]][['ctrl']]$pseudo_r2   , 5)
}

make_stargazer_like_table_dt(unique(agg_stag%>%
                                      .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                            # var_order = outcomes, 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             drop_unlisted_vars = TRUE,
                             save_path = 'E:\\panel_fr_res\\lab_results\\agg_mobility.tex'
)


# Heterogeneity by cohort -------------------------------------------------



coefs <-  as.data.table(list_es[['movers_w']][['ctrl']]$coeftable, keep.rownames = TRUE)
colnames(coefs) <-c("var",'est', 'std', 'tval','p')
coefs <- coefs %>%
  .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[rs]_)')]%>%
  .[!is.na(est) & !is.na(d)]%>%
  .[, type := case_when(str_detect(var, '_s_g') ~ 'delta',
                        str_detect(var, '_r_g')~'gamma',
                        .default = '')] %>%
  .[, g := str_extract(var, '(?<=g)[0-9]{1,4}')] %>%
  .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] %>%
  .[year != "2020"] %>%
  .[, treat := ifelse(type!='',
                      paste0(type, '_', d), d)]
p <- ggplot(coefs %>% .[treat == "gamma_acces_rce" & abs(t)<=7 & g == 2012])+
  geom_point(aes(x= t, y = est))+
  geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
  geom_vline(aes(xintercept = -1), linetype = 'dashed')+geom_hline(aes(yintercept = 0))+
  labs(title = '2012')+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
  theme_bw()+  theme(plot.title = element_text(hjust = 0.5))
p
var_path_ctrl= paste0("E:\\panel_fr_res\\lab_results\\ctrl\\", 'movers_w', '\\')

pdf(paste0(var_path_ctrl, "movers_w", '_', "gamma_acces_rce" , "_", 'g2012',".pdf"))
print(p)
dev.off() 
rm(p)



p <- ggplot(coefs %>% .[treat == "gamma_acces_rce" & abs(t)<=7 & g == 2015])+
  geom_point(aes(x= t, y = est))+
  geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
  geom_vline(aes(xintercept = -1), linetype = 'dashed')+geom_hline(aes(yintercept = 0))+
  labs(title = paste0("2015"))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
  theme_bw()+  theme(plot.title = element_text(hjust = 0.5))
p
var_path_ctrl= paste0("E:\\panel_fr_res\\lab_results\\ctrl\\", 'movers_w', '\\')

pdf(paste0(var_path_ctrl, "movers_w", '_', "gamma_acces_rce" , "_", 'g2015',".pdf"))
print(p)
dev.off() 
rm(p)

