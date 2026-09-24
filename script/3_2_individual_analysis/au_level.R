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
       "stargazer",
       'MatchIt',
       'WeightIt',
       'cobalt',
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
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))


inputpath <- "D:\\panel_fr_res\\data\\panel_au_year_fr.parquet"

save_path = paste0("D:\\panel_fr_res\\results\\productivity_au\\all_treatments\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

ds <- open_dataset(inputpath) %>%
  filter(
    last_year-entry_year >2
    & last_year >= 2003
    & entry_year >=1965 
    #& !(acces_rce_0_1y %in%  c(2014, 2015))
    #& !(date_first_idex_0_1y %in% c(2014))
    #& !(fusion_date_0_1y %in% c(2012,2016,2019))
    & entry_year <=2003
  )
nrow(ds)

ds <- as.data.table(ds) #7322301
gc()
 
n_au <- length(unique(ds$author_id)) #251105

fields_by_number <- ds %>%
  .[, .(N = n_distinct(author_id)), by = 'field']
table(fields_by_number$N)

fields_to_keep <- fields_by_number[N>=0.001*n_au]$field

sample_df_reg <- ds %>%
  .[, year_n := as.numeric(as.character(year))] %>%
  .[, ever_in_idex_annulee := max(as.numeric(str_detect(idex_set, "annulee") )), by = 'author_id'] %>%
  .[field %in% fields_to_keep] %>%
  .[ , ':='(idn = as.integer(factor(author_id)))]
gc()
length(unique(sample_df_reg$author_id)) #190249

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
              colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), "new") & !str_detect(colnames(sample_df_reg), "wins") ]
)

outcomes_wins <- ifelse(
  grepl('raw', outcomes),
  gsub('raw', 'wins', outcomes),
  paste0(outcomes, '_wins')
)

selection_period= 2000:2002

sample_df_reg <-sample_df_reg %>%   .[, (outcomes_wins) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes] %>%
  .[, ':='(entry_cohort = floor(entry_year/5)*5) ] %>%
  .[, ':='(pub_selection = sum(as.numeric(year %in% selection_period)*publications_raw),
           cit_selection = sum(as.numeric(year %in% selection_period)*citations_raw)
           ), by= 'author_id'] %>%
  .[pub_selection>0]
length(unique(sample_df_reg$author_id)) #141449

sample_df_reg <- sample_df_reg %>%
  .[, ':='(pub_n_tile = cut(pub_selection, unique(quantile(unique(sample_df_reg[, list(author_id, pub_selection)])$pub_selection,
                                                       probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, ':='(cit_n_tile = cut(cit_selection, unique(quantile(unique(sample_df_reg[, list(author_id, cit_selection)])$cit_selection,
                                                       probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, min_cnrs := min(ifelse(in_cnrs==1, year, NA), na.rm =T), by ='author_id'] %>%
  .[, min_cnrs := ifelse(!is.na(min_cnrs),min_cnrs, 0)] %>%
  .[year >=2003]

sample_df_reg %>% .[, .(N=n_distinct(author_id)), by='min_cnrs']
fwrite(sample_df_reg, "D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" )
rm(ds)
gc()
fwrite(sample_df_reg, "C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 
sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #141449
gc()


unit_cols <- c("author_id", "domain","field", "subfield","gender", "entry_year","last_year",
               "entry_cohort", "pub_selection","cit_selection","min_cnrs","pub_n_tile",'min_cnrs' ,
               'acces_rce','date_first_idex','fusion_date','interact_rce_idex','cit_n_tile'
)
outcomes_to_keep <- c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins')

type_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), 'in_type')] 


# Sample for main specification -------------------------------------------


sample_df_reg <- sample_df_reg %>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce),
           all_idex = sum(in_date_first_idex),
           all_retired = sum(as.numeric(year >last_year))
  ),by= 'author_id'] %>%
  .[, (paste0('all_', type_cols)) := lapply(.SD, sum, na.rm =TRUE), by = 'author_id', .SDcols = type_cols]%>%
  .[all_in_type_company ==0 &  all_in_type_healthcare == 0] 

sample_df_reg  %>% .[, list(author_id)] %>% distinct() %>% count() #104794
gc()


df_reg <- sample_df_reg %>%
  .[,':='(acces_rce       = as.integer(ITT_acces_rce_2007),
          date_first_idex = as.integer(ITT_date_first_idex_2007),
          fusion_date     = as.integer(ITT_fusion_date_2007),
          interact_rce_idex = ifelse(ITT_acces_rce_2007 != 0 & ITT_date_first_idex_2007 != 0,
                                     pmin(as.integer(as.character(ITT_acces_rce_2007)), 
                                          as.integer(as.character(ITT_date_first_idex_2007))), 0 ),
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile),
          has_pub = as.numeric(publications_raw >0)
  )] %>%
  .[, inst_set_2007 := inst_id_set[year == 2007][1], by = 'author_id'] %>%
  .[, city_set_2007 := city_set[year == 2007][1], by = 'author_id'] %>%
  .[, DEP_set_2007 := DEP_set[year == 2007][1], by = 'author_id'] %>%
  .[, REG_set_2007 := REG_set[year == 2007][1], by = 'author_id'] %>%
  .[, cnrs_2007 := as.integer(min_cnrs > 0 & min_cnrs <= 2007)] %>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce), 0, as.integer(acces_rce)),
           date_first_idex = ifelse(is.na(date_first_idex), 0, as.integer(date_first_idex)),
           fusion_date = ifelse(is.na(fusion_date), 0, as.integer(fusion_date)),
           interact_rce_idex = ifelse(is.na(interact_rce_idex), 0, as.integer(interact_rce_idex)),
           idn = as.integer(factor(idn)),
           year_n = as.integer(year_n),
           treatment = case_when(interact_rce_idex!=0 ~"interact_rce_idex",
                          acces_rce!=0 ~"acces_rce",
                          date_first_idex!=0 ~"date_first_idex",
                          .default = 'control')
  )
  ] %>%
  .[!(acces_rce %in% 2013:2015) & !(date_first_idex %in% c(2013,2014))
    & !(interact_rce_idex %in% c(2013,2014))
     #& (fusion_date <=2020)
    & (ever_in_idex_annulee ==0)
  ]
df_reg  %>% .[, list(author_id)] %>% distinct() %>% count() #69143
gc()

df_reg %>%
  .[, has_pub := as.numeric(publications_raw >0)] %>%
  .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce'), 
    .SD= c('publications_raw','change_af','citations_wins','in_acces_rce','retired','has_pub'
    )]%>%
  ggplot() + geom_line(aes(x=year, y = citations_wins, color = factor(acces_rce)))

nrow(df_reg[str_count(inst_id_set, ',')>0])/nrow(df_reg)

gc()

df_reg %>%  .[, .(N =n_distinct(author_id)), by = "treatment" ]

gc()
list_est <- list()

controls <- c('entry_cohort','pub_n_tile','cit_n_tile')

field_dummies <- c()
all_fields <- sort((df_reg %>% .[, list(field)] %>% distinct() %>% separate_rows(field, sep= ',') %>% distinct())$field)
for(field in all_fields){
  print(field)
  field_var <- paste0('f_', field)
  df_reg[[field_var]] <- as.numeric(str_detect(df_reg$field, field))
  field_dummies <- c(field_dummies, field_var)
}

outcomes_to_keep <- c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins', 'nr_source_top_10pct_wins',
                      'new_phrase_comb_reuse','publications_raw','citations_raw')

for(treat in c('acces_rce','date_first_idex',
               'interact_rce_idex'
)){
  print(paste0("Computing the loop for: ", dict_vars[[treat]]))
  list_est[[treat]] <- list()
  for(outcome in outcomes_to_keep){
    
    cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                       "treatment", controls, field_dummies)
    
    
    start_time_treat <- Sys.time()
    d_sep <- df_reg[treatment %in% c("control", treat) #& author_id %in% keep
    ] %>%
      .[, ":="(entry_cohort = as.factor(entry_cohort),
               domain = as.factor(domain),
               pub_n_tile = as.factor(pub_n_tile),
               cit_n_tile = as.factor(cit_n_tile),
               inst_set_2007 = as.factor(inst_set_2007),
               field = factor(field),
               treatment = as.numeric(as.character(get(treat))) )] %>% #.[entry_year %in% 1985:2003] %>%
      .[, ..cols_to_keep]
    
    
    
    
    print(paste0('Estimating for outcome: ', outcome))
    start_time_est_outcome <- Sys.time()
  list_est[[treat]][[outcome]] <- list()
  
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "treatment",
                         data = d_sep 
                         ,xformla = as.formula(paste0('~',
                                                      paste0(c(controls, field_dummies), collapse = '+')))
                         ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
  )
  
  list_est[[treat]][[outcome]]$regression <- es_stag
  
  print(paste0("Finished the estimation for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  x_lim <- c(min(d_sep$year)-min(es_stag$group),  max(d_sep$year)-max(es_stag$group))
  
  start_time_plot <- Sys.time()
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                        min_e = x_lim[1], max_e = x_lim[2])
  list_est[[treat]][[outcome]]$aggte_dyn <- es_aggte_dyn
  plot <- ggdid(es_aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
    geom_vline(xintercept = -0.5, colour = 'firebrick')+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
  print(plot_print + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
  list_est[[treat]][[outcome]]$plot <- plot_print
  
  print(paste0("Finished the plot for ", outcome, ' in:'))
  print(Sys.time()-start_time_plot)
  
  print(paste0("Finished for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  gc()
  }
  print(paste0("Finished the loop for ", dict_vars[[treat]], ' in:'))
  print(Sys.time()-start_time_treat)
}

dir.create(file.path(save_path, "estimates"), showWarnings = FALSE)

for(treat in c('acces_rce','date_first_idex','interact_rce_idex')){
  for(outcome in outcomes_to_keep){
   print(paste0('Saving for: ', treat, ' ', outcome))
    saveRDS(list_est[[treat]][[outcome]],
            file.path(save_path, "estimates", paste0(treat, "__", outcome, ".rds")),
            compress = FALSE)
    
  }
}


make_data_table_aggt <- function(treat, x) {
  list_to_get <- list_est[[treat]][[x]]
  #print(names(list_to_get))
  if("aggte_dyn" %in% names(list_to_get) ){
    #print('retrieving')
  a <- list_to_get[["aggte_dyn"]]
  }
  else{  
    #print("computing")
    a <- aggte(list_to_get[["regression"]], type = "dynamic", na.rm = TRUE)
    
  }
  
  data.table(
    treat      = treat,
    var        = x,
    event_time = a$egt,
    att        = a$att.egt,
    se         = a$se.egt
  ) %>%
    .[, `:=`(
    ci_lo = att - a$crit.val.egt * se,
    ci_hi = att + a$crit.val.egt * se
  )]
}

all_coef <- rbindlist(lapply(names(list_est), function(tr)
  rbindlist(lapply(names(list_est[[tr]]), function(v)
    make_data_table_aggt(tr, v)))))

for(treat in c('acces_rce','date_first_idex',
               'interact_rce_idex'
)){ 
  
  d_sep <- df_reg[treatment %in% c("control", treat) #& author_id %in% keep
  ] %>%
    .[, ":="(entry_cohort = as.factor(entry_cohort),
             domain = as.factor(domain),
             pub_n_tile = as.factor(pub_n_tile),
             cit_n_tile = as.factor(cit_n_tile),
             inst_set_2007 = as.factor(inst_set_2007),
             field = factor(field),
             treatment = as.numeric(as.character(get(treat))) )] %>%
    .[, treat := as.numeric(treatment !=0)]
  
  
  W <- weightit(as.formula(paste("treat ~", paste(c(controls, field_dummies), collapse = "+"))),
              data = d_sep, method = "glm", estimand = "ATT")

  # love plot: standardized mean differences before/after weighting
  love.plot(W, abs = TRUE, thresholds = c(m = 0.1),
            var.order = "unadjusted", stars = "std")
  
  # propensity score overlap
  bal_plot <- bal.plot(W, var.name = "prop.score", which = "both", type = "histogram", mirror = TRUE, colors = c('firebrick','steelblue'))
  print(bal_plot)
  ggsave(plot = bal_plot,  filename = file.path(save_path, "estimates", path = paste0(treat ,'_', "balance_plot.png")))
}

list_aggte <- list()
for(treat in c('acces_rce','date_first_idex',
               'interact_rce_idex'
)){ 
  list_aggte[[treat]] <- list()
  for(outcome in outcomes_to_keep){
  print(paste0(treat, "__", outcome, ".rds"))
    
  res  <- readRDS(file.path(save_path, "estimates", paste0(treat, "__", outcome, ".rds")))

  es_stag <- res$regression

  list_aggte[[treat]][[outcome]] <- aggte(es_stag, type = 'simple')
  
  }
}
list_tables <- list()

get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}
for(treat in c('acces_rce','date_first_idex',
               'interact_rce_idex'
)){ 
  res  <- readRDS(file.path(save_path, "estimates", paste0(treat, "__", outcome, ".rds")))
  
  es_stag <- res$regression
  
  agg_simple <- list_aggte[[treat]][[outcome]]
  att <- agg_simple$overall.att
  se  <- agg_simple$overall.se
  z   <- att / se
  p   <- 2 * (1 - pnorm(abs(z)))
  stars <- get_stars(p)
  
  # data underlying this att_gt run
  d <- es_stag$DIDparams$data
  
  # number of units (all units in the estimation sample)
  n_units <- length(unique(d$idn))
  
  # pre-treatment average of the outcome, among eventually-treated units,
  # in periods before their treatment year (treatment == 0 group timing var excluded)
  treated_ids <- unique(d$idn[d$treatment != 0])
  pre_data <- d[d$idn %in% treated_ids & d$year_n < d$treatment, ]
  pre_mean <- mean(pre_data[[outcome]], na.rm = TRUE)
  
  rows[[outcome]] <- data.frame(
    Outcome       = dict_vars[[outcome]],
    ATT           = round(att, 3),
    Stars         = stars,
    SE            = round(se, 3),
    PreTreatMean  = round(pre_mean, 3),
    N_units       = n_units,
    stringsAsFactors = FALSE
  )
}


### Save all the plots

for(treat in names(list_est) ){
  
  for(outcome in names(list_est[[treat]])){
    to_save <- list_est[[treat]][[outcome]]$plot
    
    print(to_save + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
    ggsave(plot = to_save, filename = paste0(save_path, 
                                             paste0(c(treat, outcome), collapse = '_'),
                                             '.png'
                                             ))
  }
}

list_est$acces_rce$publications_wins$plot


ggdid(test_did, ncol= 2)  

start_time <- Sys.time()
test <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
              min_e = x_lim[1], max_e = x_lim[2], cband = TRUE, bstrap = TRUE, biters = 1)
print(Sys.time()-start_time)



# Heterogeneity by dependency on grants -----------------------------------

classification_heterogeneity <- unique(df_reg %>%
  .[, y_classification := min(ifelse(!is.na(anr_investissements_d_avenir), year, NA), na.rm = T),
    by = 'inst_id_set'] %>%
  .[y_classification == year] %>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[, list(inst_id_set, y_classification, ratio_subv_propre)])

summary(classification_heterogeneity)

quantiles_ratio_subv_propre = quantile(classification_heterogeneity$ratio_subv_propre, probs = c(0.33, 0.66), na.rm =T)

classification_heterogeneity <- classification_heterogeneity %>%
  .[, quantile_ratio_subv_propre := case_when(ratio_subv_propre <= quantiles_ratio_subv_propre [[1]] ~ '1',
                                              ratio_subv_propre <= quantiles_ratio_subv_propre [[2]] ~ '2',
                                              ratio_subv_propre > quantiles_ratio_subv_propre [[2]] ~ '3',
                                              )] %>%
  .[, ":="(inst_set_2007 = inst_id_set,
           inst_id_set = NULL) ]

df_reg <- merge(df_reg,
                 classification_heterogeneity, by = 'inst_set_2007', all.x = TRUE)


list_es_by_ratio_subv_propre <- list()
gc()

for(quant_value in c("1","2","3")){
  print(quant_value)
  start_time_quant = Sys.time()
  list_es_by_ratio_subv_propre[[quant_value]] <- list()
for(treat in c('acces_rce','date_first_idex',
               'interact_rce_idex'
)){
  print(paste0("Computing the loop for: ", dict_vars[[treat]]))
  list_es_by_ratio_subv_propre[[quant_value]][[treat]] <- list()
  for(outcome in outcomes_to_keep){
    
    cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                       "treatment", controls, field_dummies)
    
    
    start_time_treat <- Sys.time()
    d_sep <- df_reg[treatment %in% c("control", treat) 
                    & (quantile_ratio_subv_propre == quant_value | treatment == 'control')
    ] %>%
      .[, ":="(entry_cohort = as.factor(entry_cohort),
               domain = as.factor(domain),
               pub_n_tile = as.factor(pub_n_tile),
               cit_n_tile = as.factor(cit_n_tile),
               inst_set_2007 = as.factor(inst_set_2007),
               field = factor(field),
               treatment = as.numeric(as.character(get(treat))) )] %>% #.[entry_year %in% 1985:2003] %>%
      .[, ..cols_to_keep]
    
    
    
    
    print(paste0('Estimating for outcome: ', outcome))
    start_time_est_outcome <- Sys.time()
    list_es_by_ratio_subv_propre[[quant_value]][[treat]][[outcome]] <- list()
    
    es_stag <- did::att_gt(yname = outcome,
                           tname = 'year_n',
                           idname = 'idn',
                           gname = "treatment",
                           data = d_sep 
                           ,xformla = as.formula(paste0('~',
                                                        paste0(c(controls, field_dummies), collapse = '+')))
                           ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
    )
    
    list_es_by_ratio_subv_propre[[quant_value]][[treat]][[outcome]]$regression <- es_stag
    
    print(paste0("Finished the estimation for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    
    x_lim <- c(min(d_sep$year)-min(es_stag$group),  max(d_sep$year)-max(es_stag$group))
    
    start_time_plot <- Sys.time()
    es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                          min_e = x_lim[1], max_e = x_lim[2])
    list_es_by_ratio_subv_propre[[quant_value]][[treat]][[outcome]]$aggte_dyn <- es_aggte_dyn
    plot <- ggdid(es_aggte_dyn)
    plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
      geom_vline(xintercept = -0.5, colour = 'firebrick')+
      theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
    print(plot_print + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
    list_es_by_ratio_subv_propre[[quant_value]][[outcome]]$plot <- plot_print
    
    print(paste0("Finished the plot for ", outcome, ' in:'))
    print(Sys.time()-start_time_plot)
    
    print(paste0("Finished for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    
    gc()
  }
  print(paste0("Finished the loop for ", dict_vars[[treat]], ' in:'))
  print(Sys.time()-start_time_treat)
}
  print(paste0("Finished the loop for quantile ", quant_value, ' in:'))
  print(Sys.time()-start_time_quant)
  
}

saveRDS(list_es_by_ratio_subv_propre, paste0(save_path, 'cs_estimates_by_quant_nt_entry_cohort_pub_cit_tile_field.rds'))


list_es_by_ratio_subv_propre <- readRDS(paste0(save_path, 'cs_estimates_by_quant_nt_entry_cohort_pub_cit_tile_field.rds'))


### Alternative specification :
list_etwfe <- list()
match_variables <- c('entry_cohort','field','cnrs_2007','pub_n_tile','cit_n_tile'
                     )
for(treat in c('acces_rce','date_first_idex','interact_rce_idex'
)){
  start_time <- Sys.time()
  list_etwfe[[treat]] <- list()
  
  for(outcome in c('publications_wins') #outcomes_to_keep
      ){
    list_etwfe[[treat]][[outcome]] <- list()
    cols_to_keep <- c(outcome, 
                      "year", "author_id", "inst_set_2007", 
                      "treatment", "acces_rce", "date_first_idex", "fusion_date", "interact_rce_idex",
                      match_variables
                      )
  d_sep <- df_reg[treatment %in% c("control", treat) #& author_id %in% keep
      ] %>%
    .[, ":="(entry_cohort = as.factor(entry_cohort),
             domain = as.factor(domain),
             pub_n_tile = as.factor(pub_n_tile),
             cit_n_tile = as.factor(cit_n_tile),
             inst_set_2007 = as.factor(inst_set_2007)
             
    )] %>% #.[entry_year %in% 1985:2003] %>%
    .[, ..cols_to_keep] %>%
    .[, treat_binary := as.numeric(treatment != 'control')]
  
  
  d_sep_match <- match.data(matchit(as.formula(paste0('treat_binary ~ ',
                                           paste0(match_variables, collapse = ' + ')))
                                    ,data = d_sep
                                    ,method = "exact")) 
  
  print(d_sep_match %>%
          .[, .(N =n_distinct(author_id)), by = "treatment" ])
  
  
  etwfe_est <- etwfe(
    fml    = as.formula(paste0(outcome, '~ 0')),
    tvar   = "year",
    gvar   = treat,
    data   = d_sep_match,
    #ivar   = "author_id",
    #gref   = 0,
    cgroup = "never",
    family = "poisson",
    vcov   = ~ inst_set_2007,
    weights = ~weights
  )
  print(Sys.time()- start_time)
  list_etwfe[[treat]][[outcome]][['regression']] <- etwfe_est
  start_time <- Sys.time()
  
  event_study <- emfx(etwfe_est, type = "event")
  print(plot(event_study))
  list_etwfe[[treat]][[outcome]][['plot']] <- event_study
  print(Sys.time()- start_time)
  }
}
uniqueN(df_reg[author_id %in% keep]$inst_set_2007)   # at 5%
uniqueN(df_reg$inst_set_2007)                        # at 100%
table(d_sep$entry_cohort, d_sep$entry_year)

emfx(list_est$acces_rce, type = "event", vcov =FALSE)
emfx(list_est[[treat]], type = "calendar")   # ATT by calendar year
emfx(list_est[[treat]], type = "group")      # ATT by adoption cohort


lapply(list_est, emfx, type = "event")     # event study
plot(emfx(list_est$acces_rce, type = "event", compress = TRUE))
plot(emfx(list_est$date_first_idex, type = "event", compress = TRUE))
plot(emfx(list_est$interact_rce_idex, type = "event", compress = TRUE))
