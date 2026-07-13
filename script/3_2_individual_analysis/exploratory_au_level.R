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


inputpath <- "D:\\panel_fr_res\\data\\panel_au_year.parquet"

save_path = paste0("D:\\panel_fr_res\\results\\productivity_au\\all_treatments\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

ds <- open_dataset(inputpath) %>%
  filter(
    last_year-entry_year >2
    & entry_year >=1965 
    & !(acces_rce_0_1y %in%  c(2014, 2015))
    & !(date_first_idex_0_1y %in% c(2014))
    & !(fusion_date_0_1y %in% c(2012,2016,2019))
    & year >= 2003
    #& entry_year <=2003
  )
nrow(ds)

ds <- as.data.table(ds)
gc()
table(ds$control_0_1y)

sample_df_reg <- ds %>%
  .[, year_n := as.numeric(as.character(year))] %>%
  .[!str_detect(idex_set, "annulee")] %>%
  .[, pub_04_07 := sum(as.numeric(year_n > 2004 & year_n <= 2007) * publications_raw ), by = 'author_id'] %>%
  .[, cit_04_07 := sum(as.numeric(year_n > 2004 & year_n <= 2007) * citations_raw ), by = 'author_id'] %>%
  .[pub_04_07 >=2] %>%
  .[str_count(field, ',')<=1]%>%
  #.[year != "2020"] %>%
  .[ , ':='(idn = as.numeric(str_remove(author_id, 'A')))]
gc()

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
                                                 as.numeric(as.character(date_first_idex_2_5y))), 0 )
           
           )]

fwrite(sample_df_reg, "D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" )
rm(ds)
gc()

sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 
#sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #104656
gc()


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
  .[!str_detect(inst_id_set, ',')] %>%
  .[all_acces_rce ==0 | acces_rce!=0] 

stayers %>%
  .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce'), .SD= c('publications_raw','citations_raw','in_acces_rce','retired')]%>%
  ggplot() + geom_line(aes(x=year, y = in_acces_rce, color = factor(acces_rce)))

test <- stayers %>% .[acces_rce ==0 & in_acces_rce == 1] %>% .[, list(inst_id_set)] %>% distinct()

table(stayers$acces_rce)
table(stayers$acces_rce, stayers$date_first_idex)


unit_cols <- c("author_id", "domain","field", "subfield","gender", "entry_year","last_year",
               "entry_cohort", "pub_04_07","cit_04_07","min_cnrs","pub_n_tile",'min_cnrs' ,
               'acces_rce','date_first_idex','fusion_date','interact_rce_idex','cit_n_tile'
               )

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


test <- compute_all_estimates(outcomes = c('retired','publications_raw', 'citations_raw'
                                           ),
                              data = stayers %>% .[pub_04_07>2],
                              w_matching = TRUE, matching_variables = c('entry_cohort','domain','pub_n_tile', 'cit_n_tile'),
                              #w_matching = FALSE,
                              id_vars = c('author_id'),
                              trend_controls = #NULL,
                              c('in_cnrs','in_ecole','entry_cohort','domain'),
                              plot_event_study = TRUE,
                              #save_event_study = TRUE, save_path = save_path, 
                              type = "feols",
                              formula_elements = formula_w_interactions,
                              peer_effects = FALSE
)




# Movers  -----------------------------------------------------------------



# Old ---------------------------------------------------------------------


trt_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), '[0-9]y')]
df_reg <- sample_df_reg %>%
  .[ #acces_rce_5y != 0 & 
    date_first_idex_0_1y == 0
    & !acces_rce_2_3y %in% 2013:2015
    & !str_detect(domain, ',')
    #& first_year_dgds >= 2005
    #& (first_year_dgds <= acces_rce_5y| acces_rce_5y ==0)
  ] %>%
  .[, (trt_cols ) := lapply(.SD, as.numeric), .SDcols = trt_cols] 

table(unique(df_reg[,list(author_id,acces_rce_2_3y)])$acces_rce_2_3y)

test_did <- did::att_gt(yname = 'citations_raw',
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'acces_rce_2_3y',
                        data = df_reg #%>% .[!str_detect(field, ',') & !field %in% c('15', '18','29','30','34','35')]
                        ,
                        allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        
                       # xformla = ~ entry_cohort + domain 

                        #+ inst_id
                        , base_period = 'universal'
                        ,control_group = 'nevertreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))
gc()
ggdid(test_did, ncol = 3)

test_did <- did::att_gt(yname = 'citations_raw',
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'acces_rce_0_1y',
                        data = df_reg #%>% .[acces_rce_0_1y!=0]
                          ,
                        #allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,

                        xformla = ~ entry_cohort + domain + min_cnrs + pub_n_tile #+ inst_id
                        , base_period = 'universal'
                        ,control_group = 'notyettreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))
gc()
ggdid(test_did, ncol = 3)


test_did <- did::att_gt(yname = "total_new_phrase_comb_reuse",
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'acces_rce_0_1y',
                        data = df_reg #%>% .[acces_rce_0_1y!=0]
                        ,
                        allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        
                        #xformla = ~ entry_cohort + field + pub_04_07 #+ inst_id
                        , base_period = 'universal'
                        ,control_group = 'nevertreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))
gc()
ggdid(test_did, ncol = 3)



test_did <- did::att_gt(yname = "in_acces_rce",
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'acces_rce_2_3y',
                        data = df_reg
                        ,allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        
                        # xformla = ~ entry_year + domain + min_cnrs + pub_n_tile   #+ inst_id
                        , base_period = 'universal'
                        ,control_group = 'notyettreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE #, min_e = -6, max_e = 8
            ))
gc()
ggdid(test_did, ncol = 3)




test_did <- did::att_gt(yname = "in_universite",
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'acces_rce_0_1y',
                        data = df_reg
                        ,
                        #allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        
                        xformla = ~ entry_cohort + domain +min_cnrs #+ inst_id
                        , base_period = 'universal'
                        ,control_group = 'notyettreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE, min_e = -6, max_e = 8))
gc()
ggdid(test_did, ncol = 3)



test_did <- did::att_gt(yname = 'in_acces_rce',
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'acces_rce_1y',
                        data = df_reg
                        ,
                        allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        xformla = ~ entry_cohort + domain  #+ prod_au_n_tile + prod_inst_n_tile + REG#+ inst_id
                        , base_period = 'universal'
                        ,control_group = 'notyettreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))
gc()
ggdid(test_did, ncol = 3)


df_reg <- sample_df_reg %>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            #+ as.numeric(contrats_et_prestations_de_recherche_hors_anr)
                            #+ as.numeric(subventions_de_la_region)
                            # + as.numeric(subventions_union_europeenne)
                            
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[ date_first_idex_0_1y == 0 & date_first_idex_2_3y == 0
   & acces_rce_0_1y %in% 0:2012
    & !str_detect(domain, ',')
    #& first_year_dgds >= 2005
    #& (first_year_dgds <= acces_rce_5y| acces_rce_5y ==0)
  ] %>%
  .[, (paste0('lag_', mobility_cols)):= lapply(.SD, shift, n= 1, type = 'lag'), by = 'author_id',
    .SDcols = mobility_cols] %>%
  #.[, (paste0('lag_', mobility_cols)):=lapply(.SD, replace_na), .SDcols = (paste0('lag_', mobility_cols))]%>%
  .[, acces_rce := ifelse(acces_rce_0_1y!= 0, 1, 0)] %>%
  .[, same_status := ifelse((acces_rce_0_1y !=0 & in_acces_rce ==1)|
                                (acces_rce_0_1y ==0& in_acces_rce==0), 1,0)]%>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce)),by= 'author_id'] %>%
  .[all_chg==0 & (acces_rce==1 | all_acces_rce ==0 )]

summary(df_reg[!is.na(ratio_subv_propre)]$ratio_subv_propre)

test_feols <- feols(as.formula(paste0('publications_raw ~ sunab(acces_rce_0_1y, year, 0)'
                                      #, '+', paste0( paste0('lag_', mobility_cols), collapse = ' + ')
                                      ,'| author_id + year '
                                      # ,'+ entry_cohort^year'
                                      # ,'+ field^year'
                                      #,'+ min_cnrs^year'
                                      #,'+ city_set^year'
                                      #,'+ pub_n_tile^year'
                                      #,'+ cit_n_tile^year'
))
,data = df_reg %>% .[acces_rce_0_1y != 2013]
, cluster = 'author_id'
)

iplot(test_feols)



# clean test for roy model ------------------------------------------------
mobility_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), '^in_|af$')]
replace_na <- function(x) ifelse(is.na(x), 0, x)
setorder(sample_df_reg, author_id, year)
sample_separate_acces_rce <-  sample_df_reg %>% 
  .[ date_first_idex_0_1y == 0 & in_type_company ==0 & in_type_healthcare==0 & 
       (acces_rce_0_1y !=0) ] %>%
  .[, (paste0('lag_', mobility_cols)):= lapply(.SD, shift, n= 1, type = 'lag'), by = 'author_id',
    .SDcols = mobility_cols] %>%
  .[!acces_rce_0_1y %in% 2013:2015] %>%
  .[, same_status_move := ifelse(in_acces_rce == lag_in_acces_rce , 1, 0)] %>%
  .[, move_trt_trt := ifelse(in_acces_rce == 1 & lag_in_acces_rce ==1, 1, 0)] %>%
  .[, move_trt_ctrl := ifelse(in_acces_rce ==0 & lag_in_acces_rce == 1, 1, 0)] %>%
  .[, move_ctrl_trt := ifelse(in_acces_rce == 1 & lag_in_acces_rce ==0, 1, 0)] %>%
  .[, move_ctrl_ctrl := ifelse(in_acces_rce ==0 & lag_in_acces_rce == 0, 1, 0)]  %>%
  .[acces_rce_0_1y!=0] %>%
  .[inst_id_set != '']
test_feols <- feols(as.formula(paste0('in_universite ~ sunab(acces_rce_0_1y, year, 2009)'
                                   #   ,"+ lag_new_af"
                   #, '+', paste0( paste0('lag_', mobility_cols), collapse = ' + ')
                    ,'| author_id + year'
                   #,'+ entry_year^year'
                   #,'+ domain^year'
                   #,'+ min_cnrs^year'
                   #,'+ city_set^year'
                   #,'+ pub_n_tile^year'
                   #,'+ cit_n_tile^year'
                   ))
                    ,data = sample_df_reg
                   , cluster = 'author_id'
                    , family = "binomial"
                    )

iplot(test_feols)


test <- df_reg %>% .[in_type_company==0]%>% .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce_0_1y'), .SD= unique(c(outcomes, mobility_cols))] %>%
  .[, acces_rce :=as.factor(acces_rce_0_1y)]

ggplot(test[!acces_rce_0_1y%in%2013:2015 ][, t:=year-acces_rce_0_1y])+geom_line(aes(x=year, y=citations_raw, color=acces_rce))



²test <- sample_df_reg %>% 
  .[acces_rce_0_1y==0 & in_acces_rce == 1 & year ==2009]

test1 <- sample_df_reg %>% .[author_id=="A5000067707"]%>%
  .[, list(author_id, year, in_acces_rce, inst_id_set)]
# let's get there later ---------------------------------------------------



make_list_g <- function(df, cols) {
  setNames(
    lapply(cols, function(col) {
      sort(unique(df[get(col) != 0][[col]]))
    }),
    cols
  )
}

list_g <- make_list_g(sample_df_reg, c("acces_rce_au", "date_first_idex_au", "fusion_date_au", "interact_rce_idex_au"))

formula_elements <- c()
formula_w_interactions <- c()
for(d in names(list_g)){
  for(g_i in list_g[[d]]){
    varname =paste0(d, '_', g_i)
    print(varname)
    ref = as.character(as.numeric(g_i)-1)
    sample_df_reg[[varname]] <- as.numeric((sample_df_reg[[paste0(d)]] == g_i))
    if(!str_detect(d, 'interact')){
      formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    }
    formula_w_interactions <- c(formula_w_interactions, paste0(varname, ' + i(year,', varname, ',ref=',ref,')') )
  }
}
test <- compute_all_estimates(outcomes = c('citations_raw'),
                              data = sample_df_reg,
                             # w_matching = TRUE, matching_variables = c('entry_cohort','city','field'),
                              w_matching = FALSE,
                              id_vars = c('author_id','inst_id_domain'),
                              trend_controls = NULL,
                              #plot_event_study = TRUE,
                              #save_event_study = TRUE, save_path = save_path, type = "feols",
                              formula_elements = formula_elements
)



# grant_regressions -------------------------------------------------------


