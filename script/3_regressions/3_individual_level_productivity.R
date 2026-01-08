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
  .[pub_04_07 >=2] %>%
  .[, n_lt := n_distinct(author_id), by = c('merged_inst_id','field','year')] %>%
  .[, merged_inst_id_field := paste0(merged_inst_id, '_', field)] %>%
  .[, fusion_date := fifelse(fusion_date =="2023", "0", as.character(fusion_date))] %>%
  .[year != "2020"] %>%
  .[ , ':='(fusion_date = as.factor(fusion_date),
            date_first_idex = as.factor(date_first_idex),
            acces_rce = as.factor(acces_rce),
            year = as.factor(year))]

length(unique(sample_df_reg$author_id)) #90515
nrow(unique(sample_df_reg[, list(merged_inst_id, field)])) #2827

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
              "| merged_inst_id_field"= 'Institution $\\times$ field',
              " | merged_inst_id_field"= 'Institution $\\times$ field',
              "author_id"= 'Author',              "year"= 'Year',  "entry_year"= 'Entry year',
              "field"= 'Field',              "gender"= 'Gender',
              " |merged_inst_id"= 'Institution $\\times$ field'
)
gc()

outcomes <- c('publications_raw', 'publications',
              'citations_raw','citations',
              'nr_source_top_5pct_raw', 'nr_source_top_5pct',
              'nr_source_top_10pct_raw', 'nr_source_top_10pct'
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
  
  agg_stag_no_ctrl <- agg_effects(list_es[[var]][['no_ctrl']], sample_df_reg, t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag <- rbind(agg_stag, agg_stag_no_ctrl)
  agg_stag_by_t_no_ctrl <- agg_effect_het(list_es[[var]][['no_ctrl']], sample_df_reg, by  ='t', t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := 'None']
  agg_stag_by_t <- rbind(agg_stag_by_t, agg_stag_by_t_no_ctrl)
  for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
    p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat)])+
      geom_point(aes(x= t, y = est))+
      geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
      geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
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
  
  
  
  
  agg_stag_ctrl <- agg_effects(list_es[[var]][['ctrl']], sample_df_reg, t_limit = 5)%>%
    .[, var := var] %>% .[, ctrl := fe_large]
  
  agg_stag <- rbind(agg_stag, agg_stag_ctrl)
  agg_stag_by_t_ctrl <- agg_effect_het(list_es[[var]][['ctrl']], sample_df_reg, by  ='t', t_limit = 5)%>%
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
  pre_mean[[var]] <- round(mean((sample_df_reg[as.numeric(as.character(year)) < 2009])[[var]], na.rm =T),2)
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

make_stargazer_like_table_dt(agg_stag %>%
                               .[, ctrl := ifelse(ctrl == 'None' | ctrl == '|year', fe_min, ctrl)], 
                             var_map = dict_vars, 
                             treat_map = dict_vars, 
                             pre_mean = pre_mean,
                             n_obs = n_obs,
                             r_2 = r_2,
                             var_order = outcomes, 
                             drop_unlisted_vars = TRUE,
                             save_path = 'E:\\panel_fr_res\\productivity_results\\individual\\agg_alt_var.tex'
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
list_es <- readRDS("E:\\panel_fr_res\\productivity_results\\individual\\all_regressions.rds")
# Heterogeneity -----------------------------------------------------------

list_all_idex <- unlist(lapply(unique((sample_df_reg$idex)), str_split, pattern = ","))
list_all_idex <- list_all_idex[list_all_idex !='']
formula_elements_by_idex <- c()
for(idex_occ in list_all_idex){
    print(idex_occ)
    varname =idex_occ
    ref = as.character(as.numeric(idex_dict_dates[[idex_occ]])-1)
    sample_df_reg[[varname]] <- as.numeric(str_detect(sample_df_reg[['idex']], idex_occ))
    formula_elements_by_idex <- c(formula_elements_by_idex, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
  } 
length(formula_elements_by_idex)
gc()

list_es_het_by_idex <- list()
outcomes_idex <- c('publications_raw','citations_raw','nr_source_top_5pct_raw')
formula_het_by_idex <- as.formula(paste0( 'y ~ y_minus_i_lt + ',  
                                          paste0(c(formula_elements[!str_detect(formula_elements, "idex")],
                                                   formula_elements_by_idex), collapse= '+'), 
                                          fe_large)) 


for(var in outcomes_idex){
  sample_df_reg$y <- sample_df_reg[[var]]
  
  sample_df_reg <- sample_df_reg %>%
    .[, ':='(y_lt = sum(y)), by = c('merged_inst_id',"field", 'year')] %>%
    .[, y_minus_i_lt := (y_lt - y)/(1-n_lt)]
  
  
  list_es_het_by_idex[[var]] <- list()

  start_time <- Sys.time()
  es_stag_w_ctrl <- fepois(formula_het_by_idex,
                           , data = sample_df_reg 
                           ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                           ,cluster = c('author_id','merged_inst_id_field')
  ) 
  time_taken <- Sys.time()-start_time
  gc()
  print(time_taken)
  list_es_het_by_idex[[var]][['ctrl']] <- es_stag_w_ctrl
}
saveRDS(list_es_het_by_idex, file = "E:\\panel_fr_res\\productivity_results\\individual\\regressions_by_idex.rds")

agg_stag_idex <- rbind(as.data.table(agg_effects_idex(list_es_het_by_idex[["publications_raw"]][['ctrl']], sample_df_reg, t_limit = 5)) %>% .[, var := "publications_raw"],
                       as.data.table(agg_effects_idex(list_es_het_by_idex[["citations_raw"]][['ctrl']], sample_df_reg, t_limit = 5))%>% .[, var := "citations_raw"],
                       as.data.table(agg_effects_idex(list_es_het_by_idex[["nr_source_top_5pct_raw"]][['ctrl']], sample_df_reg, t_limit = 5))%>% .[, var := "nr_source_top_5pct_raw"])

idex_names_to_plot <-c('multi_idex' = "Several IDEX",
                       
                       "idex_bordeaux" = 'Bordeaux',
                       "idex_lyon_2012" = "Lyon (2012)",      
                       "idex_muse" = "MUSE (Montpellier)"            ,
                       "isite_lille"= "Lille (Isite)"          , 
                       "idex_future"= 'Future (East Paris)'         ,  
                       "idex_paris_cite" = 'Paris Cité (2018)'      ,
                       "idex_paris_cite_2012" = 'Paris Cité (2012)'  , 
                       "idex_sorbonne_univ"    = 'Sorbonne Université' ,
                       "idex_aix_marseille"  = 'Aix Marseille' , 
                       "isite_cap_20_25"   = "CAP 20-25 (Auvergne)" ,   
                       "idex_2_grenoble"   = "Grenoble"   ,
                       "idex_psl"          = "PSL"   , 
                       "idex_paris_saclay" = "Paris Saclay"   , 
                         "isite_next"    = "Next (Isite, Nantes)"  ,      
                       "idex_2_cote_azur" = "Côte d'Azur"    ,
                       "idex_hesam"       = "HESAM (Paris, art)"    ,
                       "idex_psi"        = "PSI (Cergy)"     , 
                       "isite_lorraine"  =  "Lorraine (Isite)" ,   
                       "isite_e2s" = "E2S (Isite, Pau)",  
                       "idex_annulee_lyon" = 'Lyon',
                       "idex_annulee_toulouse" = "Toulouse",
                       'isite_annule_bfc' = 'BFC (Besançon)'
                       
)

idex_names_to_plot <- idex_names_to_plot[c('multi_idex', names(sort(idex_dict_dates))[!names(sort(idex_dict_dates))
        %in% c("idex_annulee_lyon","idex_annulee_toulouse",'isite_annule_bfc','multi_idex')
        & names(sort(idex_dict_dates)) %in% names(idex_names_to_plot)
        ],
                     "idex_annulee_lyon",
                     "idex_annulee_toulouse",
                     'isite_annule_bfc')]
apply_map <- function(x, m) {
  if (is.null(m)) return(x)
  if (is.list(m)) m <- unlist(m, use.names = TRUE)
  if (is.null(names(m)) || any(is.na(names(m)))) stop("var_map/treat_map must be named.")
  idx <- match(x, names(m)); out <- x; repl <- !is.na(idx); out[repl] <- unname(m[idx[repl]]); out
}
colors <- c("multi"   = "firebrick3",
            "2011"    = "steelblue1",
            "2012"    = "steelblue2",
            "2013"    = "steelblue3",
            "2016"    = "steelblue",
            "2017"    = "steelblue4",
            
            "2018"    = "black",
            "annulee" = "grey30"
)

agg_stag_idex <- agg_stag_idex %>% .[!treat %in% c('acces_rce','fusion_date')] %>%
  .[, idex := apply_map(treat, idex_names_to_plot) ] %>%
  .[, idex := factor(idex, levels = idex_names_to_plot)] %>%
  .[, date := apply_map(treat, idex_dict_dates) ] %>%
  .[, label_color := case_when(treat == "multi_idex" ~"multi",
                              str_detect(treat, "annule") ~"annulee",
                              .default = date)] 
  

p <- ggplot(agg_stag_idex %>%
              .[var == "publications_raw"  & !str_detect(treat, 'lorraine') ])+
      geom_point(aes(x= idex, y = est, color = label_color))+
      geom_errorbar(aes(x=idex, ymin = est -1.96*std, ymax=est+1.96*std, color = label_color))+
  scale_color_manual(values =colors)+
      geom_vline(aes(xintercept = 1.5), linetype = "dashed")+
      geom_vline(aes(xintercept = 19.5), linetype = "dashed")+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Effect by IDEX on publications '))+xlab('IDEX name')+ ylab('Estimate and 95% CI')+
      theme_bw()+theme(axis.text.x = element_text(angle =45, hjust =1),legend.position = "None")+
  ggpubr::geom_bracket(xmin = 19.5, xmax = 22.5, y.position = 1, label = "Cancelled")
pdf(paste0(ctrl_path, 'publications_raw_by_idex',".pdf"))
print(p)
dev.off()
    
p <- ggplot(agg_stag_idex %>%
                  .[var == "citations_raw" & !str_detect(treat, 'lorraine') ])+
      geom_point(aes(x= idex, y = est, color = label_color))+
      geom_errorbar(aes(x=idex, ymin = est -1.96*std, ymax=est+1.96*std, color = label_color))+
      scale_color_manual(values =colors)+
      geom_vline(aes(xintercept = 1.5), linetype = "dashed")+
      geom_vline(aes(xintercept = 19.5), linetype = "dashed")+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Effect by IDEX on citations '))+xlab('IDEX name')+ ylab('Estimate and 95% CI')+
      theme_bw()+theme(axis.text.x = element_text(angle =45, hjust =1),legend.position = "None")+
      ggpubr::geom_bracket(xmin = 19.6, xmax = 22.5, y.position = 1, label = "Cancelled")
pdf(paste0(ctrl_path, 'citations_raw_by_idex',".pdf"))
print(p)
dev.off()

    
p <- ggplot(agg_stag_idex %>%
                  .[var == "nr_source_top_5pct_raw" & !str_detect(treat, 'bordeaux')])+
      geom_point(aes(x= idex, y = est, color = label_color))+
      geom_errorbar(aes(x=idex, ymin = est -1.96*std, ymax=est+1.96*std, color = label_color))+
      scale_color_manual(values =colors)+
      geom_vline(aes(xintercept = 1.5), linetype = "dashed")+
      geom_vline(aes(xintercept = 19.5), linetype = "dashed")+
      geom_hline(aes(yintercept = 0))+
      labs(title = paste0('Effect by IDEX on publications in top 5% cited journals '))+xlab('IDEX name')+ ylab('Estimate and 95% CI')+
      theme_bw()+theme(axis.text.x = element_text(angle =45, hjust =1),legend.position = "None")+
      ggpubr::geom_bracket(xmin = 19.6, xmax = 22.5, y.position = 1, label = "Cancelled")
pdf(paste0(ctrl_path, 'nr_source_top_5pct_raw_by_idex',".pdf"))
print(p)
dev.off()
