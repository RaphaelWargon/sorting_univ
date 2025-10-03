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


inputpath <- "D:\\panel_fr_res\\inst_pub_y.parquet"

ds <- open_dataset(inputpath)
ds <- as.data.table(ds)
gc()

ds_clean <- ds %>%
  .[is.na(fusion_date) | fusion_date >=2017] %>%
  .[,":="(idex = ifelse(is.na(idex), 'no_idex', idex),
         acces_rce = ifelse(is.na(acces_rce), 0, acces_rce),
         date_first_idex  = ifelse(is.na(date_first_idex ), 0, date_first_idex),
         fusion_date = ifelse(is.na(fusion_date), 0, fusion_date),
         fused_inst_id = ifelse(is.na(fused_inst_id), inst_id, fused_inst_id))]%>%
  .[, group := case_when(acces_rce == 0 & date_first_idex == 0  ~ 0,
                           acces_rce >0 & date_first_idex == 0 ~1,
                           acces_rce >0 & date_first_idex >0 ~2,
                           acces_rce ==0 & date_first_idex >0 ~3) ] %>%
  .[, t := year-2007] %>%
  .[,':='(first_y_lab = min(year, na.rm = T),
          pub_2002 = max(as.numeric(year ==2002)*publications_raw, na.rm=T),
          cit_2002 = max(as.numeric(year ==2002)*citations_raw, na.rm=T)
  ), by = 'inst_id' ]
gc()
table(ds_clean$group)
cols_to_wins <- c("publications_raw","citations_raw","nr_source_btm_50pct_raw",
                  "nr_source_mid_40pct_raw","nr_source_top_20pct_raw","nr_source_top_10pct_raw",
                  "nr_source_top_5pct_raw","pub_2002","cit_2002")
data_acces_rce <- ds_clean %>% .[group %in% c(0,1)] %>%
  .[, g := fifelse(acces_rce ==0, Inf, as.numeric(acces_rce) - 2008)] %>%
  .[year %in% 1997:2020 ] %>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]
acces_rce_units <- unique(data_acces_rce[, list(inst_id,name,group,type_fr,type)])
table(data_acces_rce$g) 
ggplot(data_acces_rce %>%
         .[, .N, by = c('g','year')])+
  geom_line(aes(x=year,y=N,color= as.factor(g)))

ggplot(data_acces_rce %>%
         .[, .N, by = c('g','first_y_lab')])+
  geom_point(aes(x=first_y_lab,y=N,color= as.factor(g), group = as.factor(g)))

ggplot(data_acces_rce %>%
         .[, lapply(.SD, mean, na.rm= T), by = c('g','year'), .SDcols = cols_to_wins])+
  geom_line(aes(x=year,y=citations_raw,color= as.factor(g)))

feols_acces_rce <- didImputation(y0 = citations_raw ~ 0 | inst_id + t + cnrs_t+pub_2002_t +first_y_lab_t+type_t#+main_topic_t #+city_t
                                 ,cohort = "g",
                                 data = data_acces_rce %>% 
                                   .[inst_id != 'I4210159570'] %>%
                                   .[, ':='(log_citations = log(citations_raw),
                                                                    cnrs_t = paste0(cnrs,'_',t),
                                                                    type_t = paste0(type,'_',t),
                                                                    city_t = paste0(city,'_',t),
                                                                    main_topic_t = paste0(main_topic,'_',t),
                                                                    universite_t= paste0(main_topic, '_', t),
                                                                    first_y_lab_t= paste0(main_topic, '_', t),
                                                                    pub_2002_t = paste0(pub_2002, '_', t)
                                                                    )])

plot(feols_acces_rce)

