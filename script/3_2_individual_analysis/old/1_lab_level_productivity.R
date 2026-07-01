rm(list = ls())
gc()
#install.packages('devtools')
library('pacman')

#install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')
#install.packages("DIDmultiplegt", force = TRUE)
#install.packages("DIDmultiplegtDYN", force = TRUE)
#devtools::install_github("CdfInnovLab/didImputation", force = TRUE)

#library(didImputation)

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'dplyr','magrittr','tidyr'
       ,'binsreg',
       'DescTools',
       'cowplot',
       'DIDmultiplegt',
       "DIDmultiplegtDYN","did",
       'MatchIt'
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

inputpath <- "D:\\panel_fr_res\\data\\inst_pub_y.parquet"
inputpath_cities <- 'D:\\panel_fr_res\\data\\v_commune_2025.csv'
if(!exists(inputpath)){
  inputpath <- 'C:\\Users\\rapha\\Desktop\\inst_pub_y.parquet'
  inputpath_cities <- 'C:\\Users\\rapha\\Desktop\\v_commune_2025.csv'
}

source(paste0(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), '/agg_effects.R'))
ds <- open_dataset(inputpath) 
ds <- as.data.table(ds) %>%
  .[, ":="(n_obs = n_distinct(year), 
           min_n_au = min(n_au, na.rm = TRUE) ), by = c('inst_id')] %>%
  .[, paris := fifelse(city == 'Paris', 1, 0)] %>%
  .[, idex :=sapply(idex, paste, collapse = ", ")]%>%
  .[, parent_id :=sapply(parent_id, paste, collapse = ", ")] %>%
  .[,":="(#idex = ifelse(is.na(idex), 'no_idex', idex),
    acces_rce = ifelse(is.na(acces_rce), "0", acces_rce),
    date_first_idex  = ifelse(is.na(date_first_idex ), "0", date_first_idex),
    fusion_date = ifelse(is.na(fusion_date), "0", fusion_date))]%>%
  # recode last errors
  .[, ':='(acces_rce=  case_when(str_detect(parent_id, "I4405256580") | inst_id == 'I4405256580' ~"2010",
                                 str_detect(parent_id, "I126425946") | inst_id == 'I126425946' ~"2010",
                                 str_detect(parent_id, "I4405253280") | inst_id == 'I4405253280' ~"2010",
                                 .default = acces_rce
                                 ),
           date_first_idex=  case_when(str_detect(parent_id, "I4405256580") | inst_id == 'I4405256580' ~"2016",
                                 .default = date_first_idex
           ),
           idex =  case_when(str_detect(parent_id, "I4405256580") | inst_id == 'I4405256580' ~'isite_annule_bfc',
                             .default = idex
           )
             )
    
  ]%>%
  .[, t := year-2007] 

gc()

cities_recoded = c('Saint-Aubin' = '91','Saint-Pierre' = '974','Bagneux' = '92','Bièvres' = '91','Castres' = '81',
                   "Changé" = "53",'Chappes' = "63","Châtillon" = "92",'Floirac' = "33","Francheville"= "21",
                   "Genay" = "69","La Garde" = "83","La Rochelle" = '17',"Léry" = '21',"Massy" = "91",
                   "Mérignac" = "33","Montreuil" = '93',"Monts" = "37","Nozay" = '91',"Olivet" = "45",
                   "Rochefort" = '17',"Saint-Aubin-sur-Mer" = '14',"Saint-Denis" = "93","Saint-Gilles" = "35",
                   "Saint-Grégoire" = "35","Saint-Louis" = "68","Saint-Maurice" = "94","Saint-Nazaire" = "44",
                   "Saint-Pierre" = "974",'Saint-Priest' = "34", "Sainte-Hélène" = "73","Senlis" = "60","Valence" = "26",
                   "Vernon" = "27","Villepinte" = "93"
)

cities <-unique(fread(inputpath_cities) %>% .[TYPECOM == "COM"] %>%
                   .[, list(LIBELLE,REG,DEP)])%>%
  .[,LIBELLE := case_when(LIBELLE == "Saint-Denis" & DEP == "974" ~ 'Saint-Denis_Réunion',
                         .default = LIBELLE)]
cities <- cities[is.na(cities_recoded[LIBELLE]) | DEP == cities_recoded[LIBELLE]]

cols_to_wins <- c('n_au', "publications_raw","citations_raw","nr_source_btm_50pct_raw",
                  "nr_source_mid_40pct_raw","nr_source_top_20pct_raw","nr_source_top_10pct_raw",
                  "nr_source_top_5pct_raw")

ds_clean <- ds %>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins] %>%
  .[year >=1997  & year < 2020] %>%
  #.[, fusion_date := fifelse(fusion_date == "2023", "0", fusion_date)] %>%
  .[, n_au := fifelse(is.na(n_au), 0, n_au)] %>%
  .[, ':='(avg_publications = fifelse(n_au >0, publications_raw/n_au, 0),
           avg_citations =    fifelse(n_au >0, citations_raw/n_au, 0))] %>%
  .[acces_rce != "2014" & date_first_idex != "2014" & fusion_date != "2012"]%>%
  .[, ":="(max_n_au_2003 = max(as.numeric(year <= 2003)*n_au ),
           min_n_au = min(n_au),
           avg_publications_2003 = max(as.numeric(year == 2003)*avg_publications ),
           avg_citations_2003 = max(as.numeric(year == 2003)*avg_citations ),
           publications_2003 = max(as.numeric(year == 2003)*publications_raw ),
           citations_2003 = max(as.numeric(year == 2003)*citations_raw ),
           nr_source_top_5pct_raw_2003 = max(as.numeric(year == 2003)*nr_source_top_5pct_raw ),
           last_year_lab = max(year)
  ), by = c('inst_id')]%>%
  .[first_year_lab <= 2003  & !is.na(type_fr) & year%in%2003:2020] %>%
  .[, city:= case_when(city == "Saint-Etienne" ~ 'Saint-Étienne',
                       city == "St-Malo" ~ "Saint-Malo",
                       city == "Saint-Denis" & str_detect(parent_id, "I24715933") ~ 'Saint-Denis_Réunion',
                       .default  = city
  )] 

ds_clean <- merge(ds_clean, cities %>% .[, city:=LIBELLE], by ='city', all.x = TRUE)%>%
  .[!is.na(LIBELLE) ]

ds_clean <- ds_clean %>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[, quartile_n_au_2003 := cut(max_n_au_2003,
                                unique(quantile(unique(ds_clean[, list(inst_id, max_n_au_2003)])$max_n_au_2003,
                                                probs = c(0.0,0.25,0.5,0.75,1) )
                                ), include.lowest = T
                                , labels = FALSE
  )] %>%
  .[, quartile_avg_pub_2003 := cut(avg_publications_2003,
                                   unique(quantile(unique(ds_clean[, list(inst_id, avg_publications_2003)])$avg_publications_2003,
                                                   probs = c(0.0,0.25,0.5,0.75,1) )
                                   ), include.lowest = T
                                   , labels = FALSE
  )] %>%
  
  
  .[, n_inst_city := n_distinct(inst_id), by = 'city'] %>%
  .[, any_treatment := ifelse(acces_rce != "0" | date_first_idex != '0' | fusion_date != "0", 1, 0)] %>%
  .[, cnrs :=  fifelse(is.na(cnrs), 0, cnrs)] %>%
  .[, ':='(idn = as.numeric(paste0(str_remove(inst_id, 'I'), '0', str_remove(domain,','))),
           yearn = as.numeric(as.character(year)),
           acces_rce = as.numeric(as.character(acces_rce)),
           date_first_idex = as.numeric(as.character(date_first_idex)),
           fusion_date = as.numeric(as.character(fusion_date))
  )]
gc()

examiner <- unique(ds_clean %>%
                     .[, list(inst_id, name, city, type, cnrs,
                              first_year_lab, acces_rce, date_first_idex, fusion_date,
                              max_n_au_2003)])


examiner_total <- unique(ds %>%
                     .[, list(inst_id, name, city, type, cnrs,
                              first_year_lab, acces_rce, date_first_idex, fusion_date)])

examiner_ctrl <- examiner[acces_rce==0&date_first_idex==0&fusion_date ==0 & type != "company"]
summary(ds_clean$n_obs)
summary(ds_clean$n_au)

summary(ds_clean$min_n_au)

summary(ds$n_au)

gc()
nrow(unique(ds_clean[, list(inst_id)])) #2996
table(unique(ds_clean[, list(inst_id, acces_rce)])$acces_rce)
table(unique(ds_clean[, list(inst_id, date_first_idex)])$date_first_idex)
table(unique(ds_clean[, list(inst_id, fusion_date)])$fusion_date)

# matching ----------------------------------------------------------------
unit_cols <- c('inst_id','domain','name',
               
               'acces_rce','date_first_idex','fusion_date','any_treatment',
               
               
               'first_year_lab','type', 'city','REG','DEP',
               'n_inst_city',
               'type_fr','secteur',
               
               'max_n_au_2003','avg_publications_2003','avg_citations_2003','publications_2003','citations_2003',
               'quartile_n_au_2003','quartile_avg_pub_2003'
               )

units <- unique(ds_clean  %>%
                  .[, ..unit_cols] %>% 
                  .[ type %in% c('government','facility','education')]) %>% 
  .[, ':='(REG = as.character(REG),DEP = as.character(DEP))]


units_rce <- units[date_first_idex ==0 & acces_rce %in% 0:2012]
table(units_rce$acces_rce)

match_rce <- matchit(any_treatment ~
                         type  + domain + REG 
                        + quartile_n_au_2003
                       # + quartile_avg_pub_2003
  
  ,units_rce,method = 'exact'
)
matched_units_rce <- as.data.table(match.data(match_rce)) %>%
  .[, acces_rce_ctrl := max(acces_rce), by = "subclass"]
  
table(matched_units_rce$acces_rce,matched_units_rce$acces_rce_ctrl)

matched_rce <- merge(ds_clean %>%
                       .[, inst_id_domain := paste0(inst_id,domain)],
                      matched_units_rce%>%
                        .[, list(inst_id, domain, subclass)], by = c('inst_id','domain'))
matched_rce  %>% .[] %>%
  .[, .(mean_n_au = mean(avg_citations, na.rm=TRUE)), 
             by = .(year, any_treatment)] %>%
  ggplot(aes(year, mean_n_au, color = factor(any_treatment))) +
  geom_line()


summary(matched_rce$ratio_subv_propre)
test <- fepois(citations_raw~ sunab(acces_rce,year, 0) | inst_id_domain + year + subclass^year,
              matched_rce %>% .[ratio_subv_propre<=0.14 | is.na(ratio_subv_propre) ],
              cluster = 'inst_id_domain'
             # ,weights = (matched_rce %>% .[ratio_subv_propre >=0.14 | is.na(ratio_subv_propre) ])$max_n_au_2003
              )
iplot(test)

test <- fepois(avg_citations~ sunab(acces_rce,year, 0) | inst_id_domain + year + subclass^year,
              matched_rce,
              cluster = 'inst_id_domain'
              ,weights = matched_rce$max_n_au_2003
)
iplot(test)


test_agg <- fepois(c(n_au, publications_raw,
                     citations_raw,
                     avg_publications,
                     avg_citations)~ sunab(acces_rce,year, -1) | inst_id_domain + year + subclass^year,
               matched_rce  ,
               cluster = 'inst_id_domain'
              # ,weights = matched_rce$max_n_au_2003
)
iplot(test_agg)
pretest <- lapply(test_agg, wald, keep = "year::-[0-9]")

pretest
lapply(test_agg, iplot, ref.line = -1)

agg_results <- lapply(test_agg, aggregate, agg = "att")
agg_results

# Effet par cohorte (agrégé across périodes)
lapply(test_agg, aggregate, agg = "cohort")

# regs --------------------------------------------------------------------



test_did <- did::att_gt(yname = 'avg_citations',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = 'acces_rce',
                        data = ds_clean %>%
                          .[!acces_rce %in% 2013:2015 & date_first_idex ==0   ] %>%
                          .[year %in% 2003:2020 & type %in% c('education','facility','government')
                            #& type_fr !='undefined'
                          ],
                        # allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        xformla = ~ REG + type + domain + cnrs + quartile_n_au_2003 + quartile_avg_pub_2003 + paris + secteur + type_fr 
                        , base_period = 'universal'
                        ,control_group = 'nevertreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))
gc()
ggdid(test_did, ncol = 2)



test_did <- did::att_gt(yname = 'avg_citations',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = 'acces_rce',
                        data = ds_clean %>%
                          .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                                                      as.numeric(anr_hors_investissements_d_avenir)
                                                    + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
                          )/(
                            as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
                          .[!acces_rce %in% 2013:2015 & date_first_idex ==0   ] %>%
                          .[year %in% 2003:2020 & type %in% c('education','facility','government')
                            & type_fr !='undefined'
                            & (is)
                          ],
                        # allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        xformla = ~ REG + type + domain + cnrs + quartile_n_au_2003 + quartile_avg_pub_2003 + paris + secteur + type_fr 
                        , base_period = 'universal'
                        ,control_group = 'nevertreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))



test_did <- did::att_gt(yname = 'n_au',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = 'acces_rce',
                        data = ds_clean %>%
                          .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                                                      as.numeric(anr_hors_investissements_d_avenir)
                                                    + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
                          )/(
                            as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
                          .[!acces_rce %in% 2013:2015 & date_first_idex ==0   ] %>%
                          .[year %in% 2003:2020 & type %in% c('education','facility','government')
                          ]
                        ,
                        # allow_unbalanced_panel = TRUE,
                        # faster_mode = FALSE,
                        xformla = ~ type + domain + cnrs  + quartile_n_au_2003 + quartile_avg_pub_2003  + secteur + type_fr 
                        , base_period = 'universal'
                        ,control_group = 'notyettreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))


table(ds_clean$type_fr)

test<-unique(ds_clean[, list(inst_id, fusion_date, n_au_2003, avg_publications_2003,min_n_au)])[min_n_au <5]
test<-unique(ds_clean[, list(inst_id, fusion_date, year, publications_raw, n_au_2003, avg_publications_2003)])[inst_id == "I68947357" & field =="24"]

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


fe_min = ' |inst_id + year'
fe_large = paste0(  ' |inst_id + '
                    ,'year '
                    ,'+ type^year '
                    #,'+ public^year'
                    #,'+ ecole^year'
                    ,'+ cnrs^year'
                    #,'+ field^year'
                    ,'+ capital^year'
                    ,'+ main_topic^year'
                    ,'+ city^year'
                    ,'+ quartile_n_au_2003^year'
                    ,'+ quartile_cit_2003^year'
                    ,'+ quartile_pub_2003^year'
                    ,'+ quartile_top5pct_2003^year'
                    #,'+ quartile_avg_pub_2003^year'
                    #,'+ n_au_2003^year'
                    #,'+ avg_publications_2003^year'
                    
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
               "|inst_id"= 'Institution',
               " |inst_id"= 'Institution',
               "n_au_2003" = 'Number of authors in 2003'
)
outcomes <- c('n_au',
              "publications_raw","citations_raw","nr_source_top_5pct_raw",
              "nr_source_top_10pct_raw",
              'avg_publications',
              'avg_citations'
              )

#rm(ds)
gc()


for(var in outcomes){
  
  no_ctrl_path = "D:\\panel_fr_res\\productivity_results\\labs_enfr\\no_ctrl\\"
  ctrl_path = "D:\\panel_fr_res\\productivity_results\\labs_enfr\\ctrl\\"
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
                     ,mem.clean = TRUE,lean = TRUE, fixef.tol = 1E-4,
                     ,cluster = c('inst_id')
  ) 
  time_taken <- Sys.time() - start_time
  print(time_taken)
  
  list_es[[var]][['no_ctrl']] <- es_stag
  
  agg_stag_no_ctrl <- agg_effects(es_stag, ds_clean, t_limit =5)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  
  agg_stag <- rbind(agg_stag, agg_stag_no_ctrl)
  agg_stag_by_t_no_ctrl <- agg_effect_het(es_stag, ds_clean, by  ='t',t_limit =5)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_no_ctrl)
  
  
  for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
      geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(no_ctrl_path, var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  
  agg_stag_by_g_no_ctrl <- agg_effect_het(es_stag, ds_clean, by  ='g',t_limit =5)%>%
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
                          ,mem.clean = TRUE,lean = TRUE, fixef.tol = 1E-4,
                          ,cluster = c('inst_id')
  ) 
  time_taken <- Sys.time() - start_time
  print(time_taken)
  
  list_es[[var]][['ctrl']] <- es_stag
  
  
  agg_stag_ctrl <- agg_effects(es_stag_ctrl, ds_clean, t_limit =5)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  
  agg_stag <- rbind(agg_stag, agg_stag_ctrl)
  agg_stag_by_t_ctrl <- agg_effect_het(es_stag_ctrl, ds_clean, by  ='t', t_limit =5)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_ctrl)
  
  for(treat in unique(agg_stag_by_t_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
      geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
      theme_bw()
    pdf(paste0(ctrl_path,var, '_', treat , "_", 'by_t',".pdf"))
    print(p)
    dev.off() 
    rm(p)
  }
  gc()
  
  agg_stag_by_g_ctrl <- agg_effect_het(es_stag_ctrl, ds_clean, by  ='g',t_limit =5)%>%
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
saveRDS(list_es, file = "D:\\panel_fr_res\\productivity_results\\labs\\all_regressions.rds")


pre_mean <- c()
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
                             save_path = 'D:\\panel_fr_res\\productivity_results\\labs\\agg_att_lab_productivity_table.tex'
)

make_stargazer_like_table_dt(unique(agg_stag%>%
                                      .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)]), 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             var_order = c('n_au','publications_raw','citations_raw', 'nr_source_top_10pct_raw'), 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             drop_unlisted_vars = TRUE,
                             save_path = 'D:\\panel_fr_res\\productivity_results\\labs\\agg_att_lab_productivity_table_short.tex'
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

