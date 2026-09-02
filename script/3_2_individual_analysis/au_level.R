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
       'etwfe',
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
#source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))


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
fwrite(sample_df_reg, "C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 
sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #146091
gc()


unit_cols <- c("author_id", "domain","field", "subfield","gender", "entry_year","last_year",
               "entry_cohort", "pub_04_07","cit_04_07","min_cnrs","pub_n_tile",'min_cnrs' ,
               'acces_rce','date_first_idex','fusion_date','interact_rce_idex','cit_n_tile'
)
outcomes_to_keep <- c('publications_raw', 'citations_raw','total_new_phrase_comb_reuse','nr_source_top_5pct_raw')

type_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), 'in_type')] 


# Sample for main specification -------------------------------------------


sample_df_reg <- sample_df_reg %>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce),
           all_idex = sum(in_date_first_idex),
           all_retired = sum(as.numeric(year >last_year))
  ),by= 'author_id'] %>%
  .[, (paste0('all_', type_cols)) := lapply(.SD, sum), by = 'author_id', .SDcols = type_cols]%>%
  .[all_in_type_company ==0 & all_in_type_archive ==0 & all_in_type_other == 0
   & !is.na(field)
  ] 
sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #44575
gc()


df_reg <- sample_df_reg %>%
  .[,':='(acces_rce  = ITT_acces_rce_2007,
          date_first_idex =ITT_date_first_idex_2007,
          fusion_date = ITT_fusion_date_2007,
          interact_rce_idex = ifelse(ITT_acces_rce_2007 != 0 & ITT_date_first_idex_2007 != 0,
                                     pmin(as.numeric(as.character(ITT_acces_rce_2007)), 
                                          as.numeric(as.character(ITT_date_first_idex_2007))), 0 ),
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile),
          has_pub = as.numeric(publications_raw >0)
  )] %>%
  .[, inst_set_2007 := paste( ifelse(year == 2007, inst_id_set, ''), ''), by = 'author_id'] %>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce), 0, acces_rce),
           date_first_idex = ifelse(is.na(date_first_idex), 0, date_first_idex),
           fusion_date = ifelse(is.na(fusion_date), 0, fusion_date),
           interact_rce_idex = ifelse(is.na(interact_rce_idex), 0, interact_rce_idex),
           
           treatment = case_when(interact_rce_idex!=0 ~"interact_rce_idex",
                          acces_rce!=0 ~"acces_rce",
                          date_first_idex!=0 ~"date_first_idex",
                          .default = 'control')
  )
  ] %>%
  .[!(acces_rce %in% 2013:2015) & !(date_first_idex %in% c(2013,2014))
    & !(interact_rce_idex %in% c(2013,2014))
     & (fusion_date <=2020)
  ]

table((df_reg %>% .[, .N, by = 'author_id'])$N)
gc()

df_reg %>%
  .[, has_pub := as.numeric(publications_raw >0)] %>%
  .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce'), 
    .SD= c('publications_raw','change_af','citations_raw','in_acces_rce','retired','has_pub'
    )]%>%
  ggplot() + geom_line(aes(x=year, y = has_pub, color = factor(acces_rce)))

nrow(df_reg[str_count(inst_id_set, ',')>0])/nrow(df_reg)

gc()

table(df_reg$treatment)
gc()

set.seed(1)
auth <- unique(df_reg$author_id)
keep <- sample(auth, length(auth) * 0.05)

list_est <- list()
for(treat in c('acces_rce','date_first_idex','interact_rce_idex')){
  
  d_sep <- df_reg[treatment %in% c("control", treat) & author_id %in% keep]
  list_est[[treat]] <- etwfe(
    fml    = citations_raw ~ entry_year + field + pub_n_tile,
    tvar   = "year",
    gvar   = treat,
    data   = d_sep,
    #ivar   = "author_id",
    gref   = 0,
    cgroup = "never",
    family = "poisson",
    vcov   = ~ inst_set_2007
  )
  
}
lapply(list_est, emfx, type = "event")     # event study
plot(emfx(list_est$acces_rce, type = "event", compress = TRUE))
plot(emfx(list_est$date_first_idex, type = "event", compress = TRUE))
plot(emfx(list_est$interact_rce_idex, type = "event", compress = TRUE))



