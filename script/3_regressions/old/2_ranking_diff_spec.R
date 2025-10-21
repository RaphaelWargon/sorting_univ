rm(list = ls())
gc()

library('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot','npregfast')
sample <- fread( "E:\\panel_fr_res\\test_with_fixed_effects.csv") %>% #test_with_fixed_effects.csv
   .[
    , ':='(rank_au_akm_norm = rank_au_akm/max(rank_au_akm, na.rm = T),
           rank_inst_akm_norm = rank_inst_akm/max(rank_inst_akm, na.rm = T)
           ), by = 'main_field'
  ]
sample <- sample %>%
  .[, inst_id_set:=paste0(unique(list(inst_id)), collapse = ","), by = c('author_id','year') ]%>%
  .[, lag_inst_id_set := lag(inst_id_set, order_by = year), by = author_id] %>%
  .[, entrant := ifelse(!str_detect(lag_inst_id_set, inst_id) | is.na(lag_inst_id_set), 1,0)]
gc()


ranking_data <- sample[,n_obs_au := .N, by = author_id]%>%
                       .[year >=2000 & year<=2020 & country == "FR" #& n_y_in_sample >10
                       & rank_au_akm_norm >0.025 & rank_au_akm_norm < 0.975
                       & rank_inst_akm_norm >0.025 & rank_inst_akm_norm < 0.975
                       & n_obs_au >=10
                       & entrant == 1
                       & n_obs_univ >=100
                  #    & inst_type!= 'company'
                  ] %>%
                  .[
  , ':='(rank_au_akm = (rank_au_akm-min(rank_au_akm, na.rm = T))/(max(rank_au_akm, na.rm = T)-min(rank_au_akm, na.rm = T)),
         #rank_au_logsup =(rank_au_logsup-min(rank_au_logsup, na.rm = T))/(max(rank_au_logsup, na.rm = T)-min(rank_au_logsup, na.rm = T)),
         rank_inst_akm_norm = (rank_inst_akm_norm-min(rank_inst_akm_norm, na.rm = T))/(max(rank_inst_akm_norm, na.rm = T)-min(rank_inst_akm_norm, na.rm = T))
         #rank_inst_logsup_norm = rank_inst_logsup_norm/max(rank_inst_logsup_norm, na.rm = T)
         ), by = 'main_field'
] %>%
 .[, n_obs := .N, by = 'inst_id_field']%>%
                    .[, 
  .(   min_rank_au_akm = min(rank_au_akm, na.rm = T),
       max_rank_au_akm = max(rank_au_akm, na.rm = T),
       sd_rank_au_akm =   sd(rank_au_akm, na.rm = T),
       mean_rank_au_akm = mean(rank_au_akm, na.rm =T),
       min_fixef_au_akm = min(fixef_au_akm, na.rm = T),
       max_fixef_au_akm = max(fixef_au_akm, na.rm = T),
       sd_fixef_au_akm =   sd(fixef_au_akm, na.rm = T),
       mean_fixef_au_akm = mean(fixef_au_akm, na.rm =T),
       has_rank_90th_akm = max(fifelse(rank_au_akm >=0.9,1,0), na.rm = T),
       has_rank_50th_akm = max(fifelse(rank_au_akm >=0.5,1,0), na.rm = T),
       #min_rank_au_logsup =  min(rank_au_logsup, na.rm = T),
       #max_rank_au_logsup =  max(rank_au_logsup, na.rm = T),
       #sd_rank_au_logsup =   sd(rank_au_logsup, na.rm = T),
       #mean_rank_au_logsup = mean(rank_au_logsup, na.rm =T),
       #has_rank_90th_logsup = max(fifelse(rank_au_logsup >=0.9,1,0), na.rm = T),
       #has_rank_50th_logsup = max(fifelse(rank_au_logsup >=0.5,1,0), na.rm = T),
       n_authors = n_distinct(author_id))
  , by =c('inst_id','year','main_field', 'inst_name','inst_type','uni_pub','cnrs','fused',
          'rank_inst_akm_norm','fixef_inst_akm'
          #,'rank_inst_logsup_norm','fixef_inst_logsup'
          ,'n_authors_sample','n_obs')]%>%
  .[, post := fifelse(year >=2009,1,0)]


# number of observations --------------------------------------------------------------------
ggplot(unique(ranking_data[, .(inst_id,rank_inst_akm_norm,n_obs, inst_type, uni_pub)]))+
  geom_point(aes(x=log(n_obs), y = rank_inst_akm_norm))


# desc stats --------------------------------------------------------------
to_plot <- ranking_data %>%
  .[, uni_pub := fifelse(uni_pub ==1, "University", "Other")]


p <- binsreg(y=mean_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
        data = to_plot[year <= 2007]
        ,weights = n_authors,
        by = uni_pub, randcut = 1,
        bycolors = c('black','steelblue'),
        bysymbols = c(16,15))



p  <- p$bins_plot+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of average author fixed-effect')

save_plot("E:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre.png", p)


p1 <- binsreg(y=mean_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
             data = to_plot[, uni_pub_post := case_when(uni_pub =="University" & year >2008 ~ 'University - post',
                                                        uni_pub =="University" & year<=2008 ~ 'University - pre',
                                                        uni_pub =="Other" & year >2008 ~ 'Other - post',
                                                        uni_pub =="Other" & year<=2008 ~ 'Other - pre'
                                                        )][uni_pub == 'Other']
             ,weights = n_authors,
             by = uni_pub_post, randcut = 1,
             bycolors = c('grey81','black','steelblue1',"steelblue4"),
             bysymbols = c(16,16, 15,15)
            )



p1  <- p1$bins_plot+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of average author fixed-effect')+
  ylim(c(0.38,0.6))+geom_abline(intercept = 0, slope = 1) 

p1
p2 <- binsreg(y=mean_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
             data = to_plot[, uni_pub_post := case_when(uni_pub =="University" & year >2008 ~ 'University - post',
                                                        uni_pub =="University" & year<=2008 ~ 'University - pre',
                                                        uni_pub =="Other" & year >2008 ~ 'Other - post',
                                                        uni_pub =="Other" & year<=2008 ~ 'Other - pre'
             )][uni_pub == 'University']
             ,weights = n_authors,
             by = uni_pub_post, randcut = 1,
             bycolors = c('steelblue1',"steelblue4"),
             bysymbols = c(16,16, 15,15)
            )



p2  <- p2$bins_plot+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of average author fixed-effect')+
  ylim(c(0.38,0.6))+geom_abline(intercept = 0, slope = 1) 

p2
p12 <- plot_grid(p1,p2)
p12
save_plot("E:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre_post.png", p12)


# ranking regressions -----------------------------------------------------

sub_formula_ranking_mean <- paste0("mean_rank_au_akm ~"
                                   ,"i(year, rank_inst_akm_norm, 2008)"
                                   ,"+i(year, uni_pub, 2008) "
                                   ,"+i(year, uni_pub*rank_inst_akm_norm, 2008)")
sub_formula_ranking_mean_agg <- str_replace_all(str_replace_all(sub_formula_ranking_mean, 'year', 'post'), "2008", "0")
fe_minimal <- c('inst_id^main_field + year')
fe_controls <- c(fe_minimal, 'main_field^year'
                 , 'inst_type^year'
                 , 'cnrs^year','fused^year'
)

formula_ranking_1 = paste0(sub_formula_ranking_mean, '|',
                           paste0(fe_minimal,collapse = '+'))
formula_ranking_1_ct = paste0(sub_formula_ranking_mean, '|',
                           paste0(fe_controls,collapse = '+'))
formula_ranking_1_agg = paste0(sub_formula_ranking_mean_agg, '|',
                           paste0(fe_minimal,collapse = '+'))
formula_ranking_1_ct_agg = paste0(sub_formula_ranking_mean_agg, '|',
                               paste0(fe_controls,collapse = '+'))

test_ranking_1 <- feols(as.formula(formula_ranking_1),
                      ranking_data
                      , weights = (ranking_data
                                   
                                   )$n_authors
                      )
iplot(test_ranking_1, main = 'Overall ranking')
iplot(test_ranking_1,i.select = 2, main = 'Effect on avg author rank of uni')
iplot(test_ranking_1,i.select = 3, main = 'Ranking for universities')

etable(test_ranking_1)
test_ranking_1_agg <- feols(as.formula(formula_ranking_1_agg),
                            ranking_data, weights = ranking_data$n_authors)


formula_ranking_1_controls = paste0("mean_rank_au_akm ~"
                         ,"i(year, rank_inst_akm_norm, 2008)"
                         ,"+i(year, uni_pub, 2008) "
                         ,"+i(year, uni_pub*rank_inst_akm_norm, 2008)"
                         ,"|"
                         , "inst_id^main_field+ year"
                          , '+ main_field^year'
                          , '+ inst_type^year'
                         , '+ cnrs^year + fused^year'
)

test_ranking_1_ct <- feols(as.formula(formula_ranking_1_ct),
                        ranking_data
                        , weights = (ranking_data
                                     
                        )$n_authors
)
iplot(test_ranking_1_ct, main = 'Overall ranking')
iplot(test_ranking_1_ct,i.select = 2, main = 'Effect on avg author rank of uni')
iplot(test_ranking_1_ct,i.select = 3, main = 'Ranking for universities')

pdf("E:\\panel_fr_res\\productivity_results\\ranking_avg_overall.pdf")
iplot(test_ranking_1_ct, main = "Effect of institution-rank on avg. author rank")
dev.off()

pdf("E:\\panel_fr_res\\productivity_results\\uni_pub_au_rank.pdf")
iplot(test_ranking_1_ct,i.select = 2, main = "Effect of being a public university on avg. author rank")
dev.off()


pdf("E:\\panel_fr_res\\productivity_results\\ranking_avg_uni.pdf")
iplot(test_ranking_1_ct,i.select = 3, main = "Effect of institution-rank for public university on avg. author rank")
dev.off()

etable(test_ranking_1_ct)
test_ranking_1_ct_agg <- feols(as.formula(formula_ranking_1_ct_agg)
  ,ranking_data, weights = ranking_data$n_authors)
sub_formula_ranking_log <-  paste0("log(mean_rank_au_akm) ~"
                                   ,"i(year, log(rank_inst_akm_norm), 2008)"
                                   ,"+i(year, uni_pub, 2008) "
                                   ,"+i(year, uni_pub*log(rank_inst_akm_norm), 2008)")
sub_formula_ranking_log_agg <- str_replace_all(str_replace_all(sub_formula_ranking_log, 'year', 'post'), "2008", "0")
formula_ranking_2 = paste0(sub_formula_ranking_log, '|',
                           paste0(fe_minimal,collapse = '+'))
formula_ranking_2_ct = paste0(sub_formula_ranking_log, '|',
                              paste0(fe_controls,collapse = '+'))
formula_ranking_2_agg = paste0(sub_formula_ranking_log_agg, '|',
                               paste0(fe_minimal,collapse = '+'))
formula_ranking_2_ct_agg = paste0(sub_formula_ranking_log_agg, '|',
                                  paste0(fe_controls,collapse = '+'))



test_ranking_2 <- feols(as.formula(formula_ranking_2),
                      ranking_data, weights = ranking_data$n_authors)
iplot(test_ranking_2)
iplot(test_ranking_2,i.select = 2)
iplot(test_ranking_2,i.select = 3)

etable(test_ranking)
test_ranking_2_agg <- feols(as.formula(formula_ranking_2_agg)
  ,ranking_data[, post:=as.numeric(year>=2009)], weights = ranking_data$n_authors)


test_ranking_2_ct <- feols(as.formula(formula_ranking_2_ct),
                        ranking_data, weights = ranking_data$n_authors)


pdf("E:\\panel_fr_res\\productivity_results\\log_ranking_avg_overall.pdf")
iplot(test_ranking_2_ct, main = "Effect of institution-rank on log avg. author rank")
dev.off()

pdf("E:\\panel_fr_res\\productivity_results\\log_uni_pub_au_rank.pdf")
iplot(test_ranking_2_ct,i.select = 2, main = "Effect of being a public university on log avg. author rank")
dev.off()


pdf("E:\\panel_fr_res\\productivity_results\\log_ranking_avg_uni.pdf")
iplot(test_ranking_2_ct,i.select = 3, main = "Effect of institution-rank for public university on log avg. author rank")
dev.off()

etable(test_ranking)
test_ranking_2_ct_agg <- feols( as.formula(formula_ranking_2_ct_agg)
  ,ranking_data[, post:=as.numeric(year>=2009)], weights = ranking_data$n_authors)


etable(test_ranking_1_agg, test_ranking_1_ct_agg,test_ranking_2_agg, test_ranking_2_ct_agg
       ,file = "E:\\panel_fr_res\\productivity_results\\ranking.tex",replace= TRUE
       )


# localisme ---------------------------------------------------------------

inputpath <- "E:\\panel_fr_res\\panel_smoothed_w_theses.parquet"
ds <- open_dataset(inputpath) %>%
  filter(#all_y_in_FR >= (last_year-entry_year +1)/4
    last_year-entry_year >2
    & entry_year >=1965 
    & year >= 1997
    
  ) %>%
  select(author_id,year,
         n_phd_students, in_supervisor_inst, 
         in_referee_inst,in_jury_inst, thesis_year #, inst_set_this_year
  )
ds <- as.data.table(ds)%>%
  .[!is.na(thesis_year)]

length(unique(ds$author_id))
sample_localism <- merge(sample,
                         ds, by = c('author_id','year')) %>%
  .[entrant == 1]

p<-ggplot(sample_localism %>%
         .[, .(in_supervisor_inst = mean(in_supervisor_inst)),  by = c('uni_pub','year')] %>%
           .[, uni_pub := ifelse(uni_pub == 1, 'University','Outside ') ])+
  geom_line(aes(x=year, y = in_supervisor_inst, color = as.factor(uni_pub)))+  
  scale_color_manual(values = c('black','steelblue'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab("Prob. to be in thesis supervisor's institution")
p
save_plot("E:\\panel_fr_res\\desc_stats\\in_supervisor_inst.png", p)

reg_local <- feglm(in_supervisor_inst~
                     i(year, uni_pub, 2008)
                   + i(year, uni_pub*rank_inst_akm_norm, 2008)
                   + i(year, rank_inst_akm_norm, 2008)
                   + i(year, rank_au_akm_norm, 2008)
                   + i(year, uni_pub*rank_au_akm_norm, 2008)
                   
                   |
                     inst_id_field + year
                  # + main_field^year + inst_type^year
                   , data = sample_localism
                  , family = 'binomial'
                   )
iplot(reg_local, main = '1')
iplot(reg_local, i.select = 2)
iplot(reg_local, i.select = 3)2
iplot(reg_local, i.select = 4)
iplot(reg_local, i.select = 5)


reg_local <- feglm(c(in_supervisor_inst,in_referee_inst,in_jury_inst)~
                     i(post, uni_pub, 0)
                   + i(post, uni_pub*rank_inst_akm_norm, 0)
                   + i(post, rank_inst_akm_norm, 0)
                   |
                     inst_id_field + year
                   #+ main_field^year + inst_type^year
                   #+ thesis_year^year
                   , data = sample_localism[, post := as.numeric(year >2008)],
                   family = 'binomial'
)

reg_local_ctrls <- feglm(c(in_supervisor_inst,in_referee_inst,in_jury_inst)~
                     i(post, uni_pub, 0)
                   + i(post, uni_pub*rank_inst_akm_norm, 0)
                   + i(post, rank_inst_akm_norm, 0)
                   |
                     inst_id_field + year
                   + main_field^year + inst_type^year
                   + thesis_year^year + cnrs^year
                   , data = sample_localism[, post := as.numeric(year >2008)]
                   ,                   family = 'binomial'
)

etable(reg_local, reg_local_ctrls)


etable(reg_local, reg_local_ctrls,
       file =  "E:\\panel_fr_res\\lab_results\\localism.tex",
       replace = TRUE
       )

