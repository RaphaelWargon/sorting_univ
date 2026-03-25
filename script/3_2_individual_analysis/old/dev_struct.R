rm(list = ls())
gc()

library('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot','npregfast','np', 'KernSmooth')
sample <- fread( "D:\\panel_fr_res\\test_with_fixed_effects.csv") %>% #test_with_fixed_effects.csv
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



# Densities ---------------------------------------------------------------

xy_level_pre <-  sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] %>%
  .[, list(author_id, inst_id_field, alt_inst_fe_rank_norm,rank_au_akm_norm, uni_pub)] %>%
  .[!is.na(alt_inst_fe_rank_norm)]

xy_level_pre <- unique(xy_level_pre)

density_match_pre <- bkde2D(xy_level_pre[, list(alt_inst_fe_rank_norm,rank_au_akm_norm)],
                                   bandwidth = c(0.01,0.01))
image(density_match_pre$x1,density_match_pre$x2, density_match_pre$fhat)



xy_level_post <-  sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 2010:2020
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] %>%
  .[, list(author_id, inst_id_field, alt_inst_fe_rank_norm,rank_au_akm_norm, uni_pub)] %>%
  .[!is.na(alt_inst_fe_rank_norm)]

xy_level_post <- unique(xy_level_post)

density_match_post <- bkde2D(xy_level_post[, list(alt_inst_fe_rank_norm,rank_au_akm_norm)],
                            bandwidth = c(0.01,0.01))
image(density_match_post$x1,density_match_post$x2, density_match_post$fhat)

# Threshold functions -----------------------------------------------------



y_level_pre <- sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] %>%
  .[, .(   min_rank_au_akm = min(rank_au_akm_norm, na.rm = T),
          max_rank_au_akm = max(rank_au_akm_norm, na.rm = T),
          sd_rank_au_akm =   sd(rank_au_akm_norm, na.rm = T),
          mean_rank_au_akm = mean(rank_au_akm_norm, na.rm =T),
          n_authors = n_distinct(author_id)
  ),
  by = c('inst_id','main_field', 'uni_pub','year',
         'alt_inst_fe_rank_norm',
         'n_authors_sample')
  ] %>%
  .[,uni_pub:=as.factor(uni_pub)]%>%
  .[min_rank_au_akm !=Inf & min_rank_au_akm!=-Inf & !is.na(alt_inst_fe_rank_norm) & !is.na(uni_pub)]

np_reg_x_bar <- frfast( min_rank_au_akm ~ alt_inst_fe_rank_norm + uni_pub
                 , p= 2, seed = 12, 
                 data = y_level_pre,
                 nboot = 100, weights = n_authors)
plot(np_reg_x_bar)
pred <- predict(np_reg, newdata= y_level_pre %>%
                  .[, list(alt_inst_fe_rank_norm,uni_pub)] %>%
                  .[, uni_pub := as.factor(uni_pub)])  

y_level_pre$x_bar <- (as.numeric(y_level_pre$uni_pub)*pred$Level_1$Estimation[,1]
                           +(1-as.numeric(y_level_pre$uni_pub))*pred$Level_0$Estimation[,1])


ggplot(y_level_pre)+
  geom_point(aes(x=alt_inst_fe_rank_norm, y = x_bar, color = uni_pub))



x_level_pre <- sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
    & year == entry_year
  ] %>%
  .[, .(   min_rank_inst_akm = min(alt_inst_fe_rank_norm, na.rm = T),
           max_rank_inst_akm = max(alt_inst_fe_rank_norm, na.rm = T)
  ),
  by = c('author_id','main_field', 'uni_pub','rank_au_akm_norm')
  ] %>%
  .[,uni_pub:=as.factor(uni_pub)]%>%
  .[min_rank_inst_akm !=Inf & min_rank_inst_akm!=-Inf  & !is.na(uni_pub)]

np_reg_y_bar <- frfast( min_rank_inst_akm ~ rank_au_akm_norm + uni_pub
                  , p= 2, seed = 12, 
                  data = x_level_pre,
                  nboot = 100)
plot(np_reg_y_bar)
pred <- predict(np_reg, newdata= x_level_pre %>%
                  .[, list(rank_au_akm_norm,uni_pub)] %>%
                  .[, uni_pub := as.factor(uni_pub)])  

x_level_pre$y_bar_pub <- pred$Level_1$Estimation[,1]
x_level_pre$y_bar_priv <- pred$Level_0$Estimation[,1]


ggplot(x_level_pre)+
  geom_point(aes(x=rank_au_akm_norm, y = y_bar_pub), color= 'darkblue')+
  geom_point(aes(x=rank_au_akm_norm, y = y_bar_priv))
  



# transitions -------------------------------------------------------------

transitions_pre <-  sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] %>%
  .[, lag_uni_pub := lag(uni_pub, order_by = year), by = author_id] %>%
  .[, lag_y := lag(alt_inst_fe_rank_norm, order_by = year), by = author_id] %>%
  .[entrant == 1] %>%
  .[, list(author_id, inst_id_field, alt_inst_fe_rank_norm,rank_au_akm_norm, uni_pub, lag_y, lag_uni_pub)] %>%
  .[!is.na(alt_inst_fe_rank_norm) & !is.na(lag_y)] %>%
  .[, ':='(n_transitions = .N), by = c('lag_uni_pub','uni_pub')]%>%
  .[, ':='(lag_y = round(lag_y,2),alt_inst_fe_rank_norm = round(alt_inst_fe_rank_norm, 2), 
           rank_au_akm_norm = round(rank_au_akm_norm, 2))] %>%
  .[, .(min_new_rank = min(alt_inst_fe_rank_norm), n_transitions = first(n_transitions)), by = c("rank_au_akm_norm","lag_y",'uni_pub','lag_uni_pub')]

gc()

unique(transitions_pre[, list(uni_pub,lag_uni_pub,n_transitions)])

transitions_pub_priv <- transitions_pre[lag_uni_pub==1 & uni_pub==0]
transitions_pub_pub <- transitions_pre[lag_uni_pub==1 & uni_pub==1]
transitions_priv_priv <- transitions_pre[lag_uni_pub==0 & uni_pub==0]
transitions_priv_pub <- transitions_pre[lag_uni_pub==0 & uni_pub==1]

library(mgcv)

pre_fit_pub_priv <- gam(min_new_rank ~ te(rank_au_akm_norm, lag_y), data = transitions_pub_priv)
pre_fit_pub_pub <- gam(min_new_rank ~ te(rank_au_akm_norm, lag_y), data = transitions_pub_pub)
pre_fit_priv_priv <- gam(min_new_rank ~ te(rank_au_akm_norm, lag_y), data = transitions_priv_priv)
pre_fit_priv_pub <- gam(min_new_rank ~ te(rank_au_akm_norm, lag_y), data = transitions_priv_pub)



# Testing the calculation for pre -------------------------

sample_pre <- sample %>%
  .[,n_obs_au := .N, by = author_id] %>%
  .[year %in% 1997:2006
    & n_obs_au >=10
    & n_authors_sample >= 5
  ] 



pred <- predict(np_reg_x_bar, newdata= sample_pre %>%
                  .[, list(alt_inst_fe_rank_norm,uni_pub)] %>%
                  .[, uni_pub := as.factor(uni_pub)])  
sample_pre$x_bar <- (as.numeric(sample_pre$uni_pub)*pred$Level_1$Estimation[,1]
                      +(1-as.numeric(sample_pre$uni_pub))*pred$Level_0$Estimation[,1])
pred <- predict(np_reg_y_bar, newdata= sample_pre %>%
                  .[, list(rank_au_akm_norm,uni_pub)] %>%
                  .[, uni_pub := as.factor(uni_pub)])  

sample_pre$y_bar_pub <- pred$Level_1$Estimation[,1]
sample_pre$y_bar_priv <- pred$Level_0$Estimation[,1]

gc()

sample_pre$y_bar_new_priv <- (as.numeric(sample_pre$uni_pub ==1)* predict(pre_fit_pub_priv, newdata = copy(sample_pre) %>%
                                      .[, ':='(lag_uni_pub =uni_pub, uni_pub=0,lag_y=alt_inst_fe_rank_norm) ])+
                                as.numeric(sample_pre$uni_pub ==0)* predict(pre_fit_priv_priv, newdata = copy(sample_pre) %>%
                                                                              .[, ':='(lag_uni_pub =uni_pub, uni_pub=0,lag_y=alt_inst_fe_rank_norm) ])
)

sample_pre$y_bar_new_pub <- (as.numeric(sample_pre$uni_pub ==1)* predict(pre_fit_pub_pub, newdata = copy(sample_pre) %>%
                                                                            .[, ':='(lag_uni_pub =uni_pub, uni_pub=1,lag_y=alt_inst_fe_rank_norm) ])+
                                as.numeric(sample_pre$uni_pub ==0)* predict(pre_fit_priv_pub, newdata = copy(sample_pre) %>%
                                                                              .[, ':='(lag_uni_pub =uni_pub, uni_pub=1,lag_y=alt_inst_fe_rank_norm) ])
)
gc()

prob_kde_rect <- function(k, x1, x2, y1, y2) {
  dx <- diff(k$x)[1]; dy <- diff(k$y)[1]
  ix <- which(k$x >= x1 & k$x <= x2)
  iy <- which(k$y >= y1 & k$y <= y2)
  if (length(ix) == 0 || length(iy) == 0) return(0)
  sum(k$z[iy, ix]) * dx * dy
}
sample_pre$max_y_priv <- 0
sample_pre$max_y_pub <- 0

for(y in 1997:2006){
  print(y)
    all_ys <- subset(sample_pre, year == get('y',envir = parent.frame())) %>%
      .[, list(alt_inst_fe_rank_norm, uni_pub, x_bar)]
    sample_pre$max_y_priv <-( as.numeric(sample_pre$year ==y) * sapply(sample_pre$rank_au_akm_norm, function(t) {
      v <- max(all_ys$alt_inst_fe_rank_norm[ all_ys$x_bar >= t&all_ys$ uni_pub==0 ], na.rm =T)
    })
   +  sample_pre$max_y_priv )
    sample_pre$max_y_pub <-( as.numeric(sample_pre$year ==y) * sapply(sample_pre$rank_au_akm_norm, function(t) {
      v <- max(all_ys$alt_inst_fe_rank_norm[ all_ys$x_bar >= t& all_ys$uni_pub==1 ], na.rm =T)
    })
    +  sample_pre$max_y_pub )
    
    }
