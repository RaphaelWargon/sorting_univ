rm(list = ls())
gc()

library('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot','npregfast','np')
sample <- fread( "E:\\panel_fr_res\\test_with_fixed_effects.csv") %>% #test_with_fixed_effects.csv
  .[
    , ':='(rank_au_akm_norm = rank_au_akm/max(rank_au_akm, na.rm = T),
           rank_inst_akm_norm = rank_inst_akm/max(rank_inst_akm, na.rm = T),
           rank_colab_akm_norm = rank_colab_akm/max(rank_colab_akm, na.rm = T)
    ), by = 'main_field'
  ]
sample <- sample%>%
  .[, inst_id_set:=paste0(unique(list(inst_id)), collapse = ","), by = c('author_id','year') ]%>%
  .[, lag_inst_id_set := lag(inst_id_set, order_by = year), by = author_id] %>%
  .[, entrant := ifelse(!str_detect(lag_inst_id_set, inst_id) | is.na(lag_inst_id_set), 1,0)]
gc()

sample <- sample %>%
  .[, future_colleagues_fe := sum( as.numeric(entrant ==0)*alpha_hat )/sum(as.numeric(entrant==0)),
    by = c('inst_id_field','year')] %>%
  .[, future_colleagues_rank := frank(future_colleagues_fe)] %>%
  .[, future_colleagues_rank_norm := future_colleagues_rank /max(future_colleagues_rank, na.rm= T)] %>%
  .[, alt_inst_fe := future_colleagues_fe + fixef_inst_akm ] %>%
  .[, alt_inst_fe_rank := frank(future_colleagues_fe)] %>%
  .[, alt_inst_fe_rank_norm := alt_inst_fe_rank/max(alt_inst_fe_rank, na.rm= T)] 


scanr_oa<- as.data.table(open_dataset('E:\\scanR\\oa_scanr_id.parquet'))

etudiants <- fread('E:\\scanR\\fr-esr-atlas_regional-effectifs-d-etudiants-inscrits_agregeables.csv')

etudiants <- etudiants %>%
  rename(secteur = `secteur d'établissement`)%>%
  rename(type = `type d'établissement`) %>%
  mutate(scanr_id=str_replace(scanr, "https://scanr.enseignementsup-recherche.gouv.fr/entite/",''))%>%
  select(scanr_id, secteur,type, starts_with('Effectif'))%>%
  pivot_longer(cols = starts_with('Effectif'), names_to = 'year', values_to = "n_student") %>%
  mutate(year = str_replace(year, "Effectifs d'étudiants inscrits ", ""))

scanr_oa <- merge(scanr_oa,etudiants )

ggplot(scanr_oa %>%
         .[, .(n_student = sum(n_student, na.rm=T)), by = c('year','type')])+
  geom_line(aes(x=year,y=n_student, color = as.factor(type), group =as.factor(type)))
