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


sample_df_reg <- fread( "D:\\panel_fr_res\\sample_df_reg.csv" )

save_path = paste0("D:\\panel_fr_res\\results\\roy_model\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}




df_reg_roy_sep <- sample_df_reg %>%
   #.[date_first_idex==0] %>%
  .[, in_acces_rce := fifelse(acces_rce == 0, 0, 1)] %>%
  .[, ':='(inst_set = paste(sort(unique(inst_id)), collapse = ","),
           max_in_acces_rce = max(in_acces_rce)
           ), by = c("author_id", "year")] %>%
  .[, new_af := str_detect(lag(inst_set, order_by = year), inst_id), by = 'author_id'] %>%
  .[, chg_af := as.numeric(inst_set != lag(inst_set, order_by = year)), by = 'author_id'] %>%
  .[, (paste0('lag_', c('new_af','chg_af','in_acces_rce') )):= lapply(.SD, shift, n= 1, type = 'lag'), by = 'author_id',
    .SDcols = c('new_af','chg_af','in_acces_rce')] 
  

test_feols <- feols(in_acces_rce ~ sunab(acces_rce, year, 2009)#+ lag_new_af + lag_chg_af
                    |
                      author_id + inst_id #+ entry_year^year + domain^year + cnrs^year + city^year
                    ,data = df_reg_roy_sep
                      )
iplot(test_feols)

ggplot(df_reg_roy_sep %>%
         .[, .(in_acces_rce = sum(in_acces_rce),
               out_acces_rce = sum(1-in_acces_rce)), by= c('year')])+
  geom_line(aes(x=year, y = in_acces_rce))+
  geom_line(aes(x=year, y = out_acces_rce), linetype = 'dashed')
