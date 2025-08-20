sample <- fread( "D:\\panel_fr_res\\test_with_fixed_effects.csv") 

sample <-   sample[
  , ':='(rank_au_akm = rank_au_akm/max(rank_au_akm, na.rm = T),
         rank_inst_akm = rank_inst_akm/max(rank_inst_akm, na.rm = T)
  ), by = 'main_field']



# Densities ---------------------------------------------------------------

xy_level_pre <-  sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] %>%
  .[, list(author_id, inst_id_field, rank_inst_akm,rank_au_akm, uni_pub)]
xy_level_pre <- unique(xy_level_pre)

density_match_pre <- np::npudensbw(~ rank_inst_akm + rank_au_akm ,
                                   xy_level_pre,
                                   bwmethod = "normal-reference")
density_match_pre <- npudens(density_match_pre) 

# Threshold functions -----------------------------------------------------



y_level_pre <- sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] %>%
  .[, .(   min_rank_au_akm = min(rank_au_akm, na.rm = T),
          max_rank_au_akm = max(rank_au_akm, na.rm = T),
          sd_rank_au_akm =   sd(rank_au_akm, na.rm = T),
          mean_rank_au_akm = mean(rank_au_akm, na.rm =T),
          min_fixef_au_akm = min(fixef_au_akm, na.rm = T),
          max_fixef_au_akm = max(fixef_au_akm, na.rm = T),
          sd_fixef_au_akm =   sd(fixef_au_akm, na.rm = T),
          mean_fixef_au_akm = mean(fixef_au_akm, na.rm =T),
          has_rank_90th_akm = max(fifelse(rank_au_akm >=0.9,1,0), na.rm = T),
          has_rank_50th_akm = max(fifelse(rank_au_akm >=0.5,1,0), na.rm = T),
          n_authors = n_distinct(author_id)
  ),
  by = c('inst_id','main_field', 'uni_pub',
         'rank_inst_akm','fixef_inst_akm',
         'n_authors_sample')
  ] %>%
  .[,uni_pub:=as.factor(uni_pub)]%>%
  .[min_rank_au_akm !=Inf & min_rank_au_akm!=-Inf & !is.na(rank_inst_akm) & !is.na(uni_pub)]

np_reg <- frfast( min_rank_au_akm ~ rank_inst_akm + uni_pub
                 , p= 2, seed = 12, 
                 data = y_level_pre,
                 nboot = 100, weights = n_authors)
plot(np_reg)
pred <- predict(np_reg, newdata= y_level_pre %>%
                  .[, list(rank_inst_akm,uni_pub)] %>%
                  .[, uni_pub := as.factor(uni_pub)])  

y_level_pre$prediction <- (as.numeric(y_level_pre$uni_pub)*pred$Level_1$Estimation[,1]
                           +(1-as.numeric(y_level_pre$uni_pub))*pred$Level_0$Estimation[,1])


ggplot(y_level_pre)+
  geom_point(aes(x=rank_inst_akm, y = prediction))


