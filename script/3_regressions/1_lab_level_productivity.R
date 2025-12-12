rm(list = ls())
gc()
#install.packages('devtools')
library('pacman')

#install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')
#install.packages("DIDmultiplegt", force = TRUE)
#install.packages("DIDmultiplegtDYN", force = TRUE)
#devtools::install_github("CdfInnovLab/didImputation", force = TRUE)

library(didImputation)

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'dplyr','magrittr','tidyr'
       ,'binsreg',
       'DescTools',
       'cowplot',
       'DIDmultiplegt',
       "DIDmultiplegtDYN"
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}


inputpath <- "E:\\panel_fr_res\\inst_pub_y.parquet"
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))
ds <- open_dataset(inputpath) 
ds <- as.data.table(ds) %>%
  .[, ":="(first_y_lab = min(year, na.rm = T),
           n_obs = n_distinct(year), 
           min_n_au = min(n_au, na.rm = TRUE) ), by = c('merged_inst_id','field')] %>%
  .[, paris := fifelse(city == 'Paris', 1, 0)] %>%
  .[,":="(#idex = ifelse(is.na(idex), 'no_idex', idex),
    acces_rce = ifelse(is.na(acces_rce), "0", acces_rce),
    date_first_idex  = ifelse(is.na(date_first_idex ), "0", date_first_idex),
    fusion_date = ifelse(is.na(fusion_date), "0", fusion_date))]%>%
  .[, t := year-2007] 

gc()

cols_to_wins <- c('n_au', "publications_raw","citations_raw","nr_source_btm_50pct_raw",
                  "nr_source_mid_40pct_raw","nr_source_top_20pct_raw","nr_source_top_10pct_raw",
                  "nr_source_top_5pct_raw")

ds_clean <- ds %>%
  #.[is.na(fusion_date) | fusion_date >=2017] %>%

  #.[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins] %>%
  .[year >=1997  & year < 2020& !is.na(field)] %>%
  .[, fusion_date := fifelse(fusion_date == "2023", "0", fusion_date)] %>%
  .[str_count(field, ',')<2 ] %>%
  .[, n_au := fifelse(is.na(n_au), 0, n_au)] %>%
 # .[first_y_lab <= 2003 ] %>%
  .[, ':='(avg_publications = fifelse(n_au >0, publications_raw/n_au, 0),
           avg_citations =    fifelse(n_au >0, citations_raw/n_au, 0))] %>%
  .[acces_rce != "2014" & date_first_idex != "2014" & fusion_date != "2012"]%>%
  .[, ":="(n_au_2003 = max(as.numeric(year == 1997)*n_au ),
           min_n_au = min(n_au),
           avg_publications_2003 = max(as.numeric(year == 1997)*avg_publications ),
           avg_citations_2003 = max(as.numeric(year == 1997)*avg_citations ),
           last_year_lab = max(year)
           ), by = c('merged_inst_id','field')]%>%
  .[first_y_lab <= 1997 & n_au_2003 >0 
    ]


summary(ds_clean$n_obs)
summary(ds_clean$n_au)

summary(ds_clean$min_n_au)

summary(ds$n_au)

gc()
nrow(unique(ds_clean[, list(merged_inst_id, field)])) #10812
table(unique(ds_clean[, list(merged_inst_id, field, acces_rce)])$acces_rce)
table(unique(ds_clean[, list(merged_inst_id, field, date_first_idex)])$date_first_idex)
table(unique(ds_clean[, list(merged_inst_id, field, fusion_date)])$fusion_date)

table(unique(ds_clean[, list(merged_inst_id, acces_rce)])$acces_rce)
table(unique(ds_clean[, list(merged_inst_id, date_first_idex)])$date_first_idex)
table(unique(ds_clean[, list(merged_inst_id, fusion_date)])$fusion_date)


test<-unique(ds_clean[, list(merged_inst_id, field, fusion_date, n_au_2003, avg_publications_2003,min_n_au)])[min_n_au <5]
test<-unique(ds_clean[, list(merged_inst_id, field, fusion_date, year, publications_raw, n_au_2003, avg_publications_2003)])[merged_inst_id == "I68947357" & field =="24"]

list_g = list( "acces_rce" = sort(unique(ds_clean[acces_rce !=0]$acces_rce))
               ,"date_first_idex" = sort(unique(ds_clean[date_first_idex !=0]$date_first_idex))
               , "fusion_date" = sort(unique(ds_clean[fusion_date !=0]$fusion_date))
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
    ds_clean[[varname]] <- as.numeric((ds_clean[[paste0(d)]] == g_i))
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  }}  
length(formula_elements)


fe_min = ' |merged_inst_id^field + year'
fe_large = paste0(  ' |merged_inst_id^field + '
                    ,'year '
                    ,'+ type^year '
                    ,'+ public^year'
                    ,'+ ecole^year'
                    ,'+ cnrs^year'
                    ,'+ field^year'
                    ,'+ city^year'
                    ,'+ n_au_2003^year'
                    ,'+ avg_publications_2003^year'

)
gc()

list_es = list()

agg_stag <- data.table(treat = '', est = 0, std = 0, t= 0, pvalue = 0, pvalue_pretrend= 0, type = '',  var = '', ctrl = '') %>% .[treat != '']
agg_stag_by_t <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0,  t= 0, n = '', var = '',  ctrl = '') %>% .[treatment != '']
agg_stag_by_g <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0, g = 0, n = '', var = '',  ctrl = '') %>% .[treatment != '']

dict_vars <- c('acces_rce'= 'University autonomy',
               'date_first_idex'='Received an IDEX',
               'fusion_date'= "Merged establishment",
               "publications_raw" = 'Publications',
               "citations_raw"='Citations',
               "avg_publications" = 'Average publications',
               "avg_citations"='Average citations',
               "n_au" = "Number of researchers",
               "nr_source_btm_50pct_raw"="Bottom 50% journal publications",
               "nr_source_mid_40pct_raw"="Middle 40% journal publications",
               "nr_source_top_20pct_raw"="Top 20% journal publications", 
               "nr_source_top_10pct_raw" ="Top 10% journal publications", 
               "nr_source_top_5pct_raw" ="Top 5% journal publications",
               "type" = 'Institution Type',
               "city"=  'City',
               'cnrs'='CNRS',
               'public'='Public Status',
               'ecole'='Grande Ecole status',
               'main_topic'='Main field',
               "|merged_inst_id"= 'Institution',
               " |merged_inst_id"= 'Institution',
                 "n_au_2003" = 'Number of authors in 2003'
)
outcomes <- c('n_au',
              "publications_raw","citations_raw","nr_source_top_5pct_raw",
              'avg_publications',
              'avg_citations')

#rm(ds)
gc()


for(var in outcomes){
  
  no_ctrl_path = "E:\\panel_fr_res\\productivity_results\\labs\\no_ctrl\\"
  ctrl_path = "E:\\panel_fr_res\\productivity_results\\labs\\ctrl\\"
  if (!file.exists(no_ctrl_path)){
    dir.create(no_ctrl_path, recursive = TRUE)
  }
  if (!file.exists(ctrl_path)){
    dir.create(ctrl_path, recursive = TRUE)
  }
  
  
  

list_es[[var]] <- list()

start_time <- Sys.time()
es_stag <- fepois( as.formula(paste0(var, ' ~ ', paste0(formula_elements, collapse= '+'), fe_min))
                  , data = ds_clean
                  ,mem.clean = TRUE,lean = TRUE, fixef.tol = 1E-2,
                  ,cluster = c('merged_inst_id', 'field')
) 
time_taken <- Sys.time() - start_time
print(time_taken)

list_es[[var]][['no_ctrl']] <- es_stag

agg_stag_no_ctrl <- agg_effects(es_stag, ds_clean, t_limit =7)%>%
  .[, var := var] %>% .[, ctrl := 'None']

agg_stag <- rbind(agg_stag, agg_stag_no_ctrl)
agg_stag_by_t_no_ctrl <- agg_effect_het(es_stag, ds_clean, by  ='t')%>%
  .[, var := var] %>% .[, ctrl := 'None']
agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_no_ctrl)


for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
  p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat) & abs(t)<=7])+
    geom_point(aes(x= t, y = est))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
    geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  pdf(paste0(no_ctrl_path, var, '_', treat , "_", 'by_t',".pdf"))
  print(p)
  dev.off() 
  rm(p)
}
gc()

agg_stag_by_g_no_ctrl <- agg_effect_het(es_stag, ds_clean, by  ='g')%>%
  .[, var := var] %>% .[, ctrl := 'None']
agg_stag_by_g <- rbind(agg_stag_by_g, agg_stag_by_g_no_ctrl)


for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
  p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat) ])+
    geom_point(aes(x= g, y = est))+
    geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
    geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  pdf(paste0(no_ctrl_path, var, '_', treat , "_", 'by_g',".pdf"))
  print(p)
  dev.off()
  rm(p)
}
gc()



start_time <- Sys.time()
es_stag_ctrl <- fepois( as.formula(paste0(var, ' ~ ', paste0(formula_elements, collapse= '+'), fe_large))
                   , data = ds_clean,
                   ,mem.clean = TRUE,lean = TRUE, fixef.tol = 1E-2,
                   ,cluster = c('merged_inst_id', 'field')
) 
time_taken <- Sys.time() - start_time
print(time_taken)

list_es[[var]][['ctrl']] <- es_stag


agg_stag_ctrl <- agg_effects(es_stag_ctrl, ds_clean, t_limit =7)%>%
  .[, var := var] %>% .[, ctrl := fe_large]

agg_stag <- rbind(agg_stag, agg_stag_ctrl)
agg_stag_by_t_ctrl <- agg_effect_het(es_stag_ctrl, ds_clean, by  ='t')%>%
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

agg_stag_by_g_ctrl <- agg_effect_het(es_stag_ctrl, ds_clean, by  ='g')%>%
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
                             var_order = outcomes, 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             drop_unlisted_vars = TRUE,
                             save_path = 'E:\\panel_fr_res\\productivity_results\\labs\\agg_att_lab_productivity_table.tex'
                             )

make_stargazer_like_table_dt(unique(agg_stag%>%
                                      .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             var_order = c('n_au','avg_publications','avg_citations'), 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             drop_unlisted_vars = TRUE,
                             save_path = 'E:\\panel_fr_res\\productivity_results\\labs\\agg_att_lab_productivity_table_short.tex'
)

#data_acces_rce <- ds_clean %>% .[group %in% c(0,1)] %>%
#  .[, g := fifelse(acces_rce ==0, Inf, as.numeric(acces_rce) - 2008)] %>%
#  .[year %in% 1997:2020 ] %>%
#  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]
#acces_rce_units <- unique(data_acces_rce[, list(inst_id,name,group,type_fr,type)])
#table(data_acces_rce$g) 
#ggplot(data_acces_rce %>%
#         .[, .N, by = c('g','year')])+
#  geom_line(aes(x=year,y=N,color= as.factor(g)))
#
#ggplot(data_acces_rce %>%
#         .[, .N, by = c('g','first_y_lab')])+
#  geom_point(aes(x=first_y_lab,y=N,color= as.factor(g), group = as.factor(g)))
#
#ggplot(data_acces_rce %>%
#         .[, lapply(.SD, mean, na.rm= T), by = c('g','year'), .SDcols = cols_to_wins])+
#  geom_line(aes(x=year,y=citations_raw,color= as.factor(g)))
#
#feols_acces_rce <- didImputation(y0 = citations_raw ~ 0 | inst_id + t + cnrs_t+pub_2002_t +first_y_lab_t+type_t#+main_topic_t #+city_t
#                                 ,cohort = "g",
#                                 data = data_acces_rce %>% 
#                                   .[inst_id != 'I4210159570'] %>%
#                                   .[, ':='(log_citations = log(citations_raw),
#                                                                    cnrs_t = paste0(cnrs,'_',t),
#                                                                    type_t = paste0(type,'_',t),
#                                                                    city_t = paste0(city,'_',t),
#                                                                    main_topic_t = paste0(main_topic,'_',t),
#                                                                    universite_t= paste0(main_topic, '_', t),
#                                                                    first_y_lab_t= paste0(main_topic, '_', t),
#                                                                    pub_2002_t = paste0(pub_2002, '_', t)
#                                                                    )])
#
#plot(feols_acces_rce)

