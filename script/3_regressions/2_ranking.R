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

#sample$alpha_hat <- sample$alpha_hat.x
sample <- sample %>%
  .[, future_colleagues_fe := sum( as.numeric(entrant ==0)*alpha_hat )/sum(as.numeric(entrant==0)),
    by = c('inst_id_field','year')] %>%
  .[, future_colleagues_rank := frank(future_colleagues_fe)] %>%
  .[, future_colleagues_rank_norm := future_colleagues_rank /max(future_colleagues_rank, na.rm= T)] %>%
  .[, alt_inst_fe := future_colleagues_fe + fixef_inst_akm ] %>%
  .[, alt_inst_fe_rank := frank(future_colleagues_fe)] %>%
  .[, alt_inst_fe_rank_norm := alt_inst_fe_rank/max(alt_inst_fe_rank, na.rm= T)] 
  
  
ggplot(sample %>% .[citations_raw>0]%>%
         .[, ':='(future_colleagues_rank_norm = round(future_colleagues_rank_norm, 2),
                  rank_au_akm_norm = round(rank_au_akm_norm, 2)
                  )] %>%
         .[, .(citations_raw = mean(citations_raw),
               rank_colab_akm = mean(rank_colab_akm), N = .N
               ), by = c('future_colleagues_rank_norm','rank_au_akm_norm')])+
  geom_raster(aes(x=future_colleagues_rank_norm, y= rank_au_akm_norm, fill = log(N)))+
  scale_fill_gradientn(colors = c('white','pink', 'firebrick','orange','yellow','green','aquamarine','blue','purple','black'))

ggplot(sample %>% .[citations_raw>0]%>%
         .[, ':='(rank_inst_akm_norm = round(rank_inst_akm_norm, 2),
                  rank_au_akm_norm = round(rank_au_akm_norm, 2)
         )] %>%
         .[, .(citations_raw = mean(citations_raw),
               rank_colab_akm = mean(rank_colab_akm), N = .N
         ), by = c('rank_inst_akm_norm','rank_au_akm_norm')])+
  geom_raster(aes(x=rank_inst_akm_norm, y= rank_au_akm_norm, fill = log(N)))+
  scale_fill_gradientn(colors = c('white','pink', 'firebrick','orange','yellow','green','aquamarine','blue','purple','black'))


ggplot(sample %>% .[citations_raw>0]%>%
         .[, ':='(rank_inst_akm_norm = round(rank_inst_akm_norm, 2),
                  rank_au_akm_norm = round(rank_au_akm_norm, 2)     
           )] %>%
         .[, .(citations_raw = mean(citations_raw),
               rank_colab_akm_norm = mean(rank_colab_akm_norm)
         ), by = c('rank_inst_akm_norm','rank_au_akm_norm')])+
  geom_raster(aes(x=rank_inst_akm_norm, y= rank_au_akm_norm, fill = rank_colab_akm_norm))+
  scale_fill_gradientn(colors = c('white','pink', 'firebrick','orange','yellow','green','aquamarine','blue','purple','black'))

ggplot(sample %>% .[citations_raw>0]%>%
         .[, ':='(rank_inst_akm_norm = round(rank_inst_akm_norm, 2),
                  rank_au_akm_norm = round(rank_au_akm_norm, 2),
                  rank_colab_akm_norm = round(rank_colab_akm_norm, 2)
         )] %>%
         .[, .(citations_raw = mean(citations_raw), N = .N
         ), by = c('rank_colab_akm_norm','rank_au_akm_norm')])+
  geom_raster(aes(x=rank_colab_akm_norm, y= rank_au_akm_norm, fill = log(N)))+
  scale_fill_gradientn(colors = c('white','pink', 'firebrick','orange','yellow','green','aquamarine','blue','purple','black'))


ggplot(sample %>% .[citations_raw>0]%>%
         .[, ':='(rank_inst_akm_norm = round(rank_inst_akm_norm, 2),
                  rank_au_akm_norm = round(rank_au_akm_norm, 2),
                  rank_colab_akm_norm = round(rank_colab_akm_norm, 2)
         )] %>%
         .[, .(citations_raw = mean(citations_raw),
               rank_au_akm_norm = mean(rank_au_akm_norm)
         ), by = c('rank_colab_akm_norm','rank_inst_akm_norm')])+
  geom_raster(aes(x=rank_colab_akm_norm, y= rank_inst_akm_norm, fill = rank_au_akm_norm))+
  scale_fill_gradientn(colors = c('white','pink', 'firebrick','orange','yellow','green','aquamarine','blue','purple','black'))



plot(sort(log(unique(sample[, list(inst_id, n_obs_univ)])$n_obs_univ)))

test_estim <- feols(log(citations) ~ rank_inst_akm_norm*prive*post + rank_au_akm_norm*prive*post
                    |year + main_field
                 , data = sample)
summary(test_estim)  



unique(sample[name == 'Paris School of Economics'][, list(inst_id, rank_au_akm_norm, rank_inst_akm_norm, n_authors_sample, main_field,n_authors_w_several_inst)])

ggplot(sample[name == 'Paris School of Economics' & year %in% c(2005, 2016) & main_field == "econ"])+
  geom_density(aes(x=rank_au_akm_norm, color = as.factor(year)), alpha = 0.5)

ggplot(sample[year %in% c(2005, 2016) & main_field == "econ"])+
  geom_point(aes(x=rank_au_akm_norm, y=alpha_hat, color = interaction(year, uni_pub)), alpha = 0.5)+
  geom_vline(aes(xintercept= 0.025))+geom_vline(aes(xintercept= 0.975))
  #geom_smooth(aes(x=rank_inst_akm, y=rank_au_akm, color = interaction(year, uni_pub)), alpha = 0.5)

ggplot(sample[year %in% c(2005, 2016)# & main_field == "econ"
              ])+
  geom_point(aes(x=fixef_inst_akm, y=alpha_hat, color = interaction(year, uni_pub)), alpha = 0.5)+
  geom_smooth(aes(x=fixef_inst_akm, y=alpha_hat, color = interaction(year, uni_pub)), alpha = 0.5, formula = y ~x)

ggplot(sample[year %in% c(2005, 2016)])+
  geom_density(aes(x=fixef_inst_akm, fill = as.factor(uni_pub)))
unique(sample[,list(author_id, n_y_in_sample)])[, .N, by = 'n_y_in_sample'][order(n_y_in_sample)]

ggplot()+
  geom_line(aes(x=fixef_inst_akm, y=rank_inst_akm), color ="darkred" ,
            data = unique(sample[, list(inst_id_field, rank_inst_akm, fixef_inst_akm)]) %>%
              .[, ':='(rank_inst_akm = rank_inst_akm/max(rank_inst_akm, na.rm= T),
                       fixef_inst_akm = (fixef_inst_akm - min(fixef_inst_akm, na.rm =T))/(max(fixef_inst_akm, na.rm =T)-min(fixef_inst_akm, na.rm = T))
                       )]
              )+
  geom_line(aes(x=avg_alpha_i_bar, y=rank_colab_akm), color ="darkblue" ,
            data = unique(sample[, list(avg_alpha_i_bar, rank_colab_akm)]) %>%
              .[avg_alpha_i_bar<Inf & avg_alpha_i_bar >-Inf] %>%
              .[, ':='(rank_colab_akm = rank_colab_akm/max(rank_colab_akm, na.rm= T),
                       avg_alpha_i_bar = (avg_alpha_i_bar - min(avg_alpha_i_bar, na.rm =T))/(max(avg_alpha_i_bar, na.rm =T)-min(avg_alpha_i_bar, na.rm = T))
              )]
  )+
  geom_line(aes(x=alpha_hat, y=rank_au_akm/max(rank_au_akm)) ,color = "darkgreen", 
            data = unique(sample[, list(author_id, rank_au_akm, alpha_hat)])%>%
              .[, ':='(rank_au_akm = rank_au_akm/max(rank_au_akm, na.rm= T),
                       alpha_hat = (alpha_hat - min(alpha_hat, na.rm =T))/(max(alpha_hat, na.rm =T)-min(alpha_hat, na.rm = T))
              )]
            
            
            )
  
save_plot("D:\\panel_fr_res\\productivity_results\\distrib_fe.png", plot=last_plot())

ggplot()+
  geom_line(aes(x=rank_inst_akm, y=min_rank), color ="darkred" ,
            data = sample %>%
              .[rank_inst_akm <Inf & rank_inst_akm >-Inf & !is.na(rank_inst_akm)] %>%
              .[, rank_inst_akm := round(rank_inst_akm/max(rank_inst_akm, na.rm =T), 2)]%>%
              .[, .(min_rank =min(rank_au_akm_norm, na.rm = T)), by = c('inst_id_field','rank_inst_akm')] %>%
              .[, .(min_rank = mean(min_rank)), by = rank_inst_akm]
  )+
  geom_line(aes(x=rank_au_akm, y=min_rank), color ="darkgreen" ,
            data = sample %>%
              .[rank_au_akm <Inf & rank_au_akm >-Inf & !is.na(rank_au_akm)] %>%
              .[, rank_au_akm := round(rank_au_akm/max(rank_au_akm, na.rm =T), 2)]%>%
              .[, .(min_rank =min(rank_inst_akm_norm, na.rm = T)), by = c('author_id','rank_au_akm')] %>%
              .[, .(min_rank = mean(min_rank)), by = rank_au_akm]
  )
save_plot("D:\\panel_fr_res\\productivity_results\\threshold_functions.png", plot=last_plot())

ggplot()+
  geom_line(aes(x=future_colleagues_rank_norm, y=min_rank), color ="darkred" ,
            data = sample %>%
              .[future_colleagues_rank_norm <Inf & future_colleagues_rank_norm >-Inf & !is.na(future_colleagues_rank_norm)] %>%
              .[, future_colleagues_rank_norm := round(future_colleagues_rank_norm/max(future_colleagues_rank_norm, na.rm =T), 2)]%>%
              .[, .(min_rank =min(rank_au_akm_norm, na.rm = T)), by = c('inst_id_field','future_colleagues_rank_norm')] %>%
              .[, .(min_rank = mean(min_rank)), by = future_colleagues_rank_norm]
  )+
  geom_line(aes(x=rank_au_akm, y=min_rank), color ="darkgreen" ,
            data = sample %>%
              .[rank_au_akm <Inf & rank_au_akm >-Inf & !is.na(rank_au_akm)] %>%
              .[, rank_au_akm := round(rank_au_akm/max(rank_au_akm, na.rm =T), 2)]%>%
              .[, .(min_rank =min(future_colleagues_rank_norm, na.rm = T)), by = c('author_id','rank_au_akm')] %>%
              .[, .(min_rank = mean(min_rank)), by = rank_au_akm]
  )



save_plot("D:\\panel_fr_res\\productivity_results\\threshold_functions_alt.png", plot=last_plot())




# sorting pure ------------------------------------------------------------



ranking_data <- sample[,n_obs_au := .N, by = author_id]%>%
                       .[year >=2000 & year<=2020# & country == "FR" #& n_y_in_sample >10
                       & rank_au_akm_norm >0.025 & rank_au_akm_norm < 0.975
                       & rank_inst_akm_norm >0.025 & rank_inst_akm_norm < 0.975
                       & n_obs_au >=10
                       & n_authors_sample >= 5
                       & entrant == 1
                       & n_obs_univ >=100
                  #    & inst_type!= 'company'
                  ] %>%
                  .[
  , ':='(fixef_au_akm = alpha_hat,
         fixef_colab_akm = avg_alpha_i_bar,
    rank_au_akm = (rank_au_akm-min(rank_au_akm, na.rm = T))/(max(rank_au_akm, na.rm = T)-min(rank_au_akm, na.rm = T)),
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
       
     min_rank_colab_akm = min(rank_colab_akm, na.rm = T),
     max_rank_colab_akm = max(rank_colab_akm, na.rm = T),
     sd_rank_colab_akm =   sd(rank_colab_akm, na.rm = T),
     mean_rank_colab_akm = mean(rank_colab_akm, na.rm =T),
     min_fixef_colab_akm = min(fixef_colab_akm, na.rm = T),
     max_fixef_colab_akm = max(fixef_colab_akm, na.rm = T),
     sd_fixef_colab_akm =   sd(fixef_colab_akm, na.rm = T),
     mean_fixef_colab_akm = mean(fixef_colab_akm, na.rm =T),
     has_rank_90th_akm = max(fifelse(rank_colab_akm >=0.9,1,0), na.rm = T),
     has_rank_50th_akm = max(fifelse(rank_colab_akm >=0.5,1,0), na.rm = T),
            
       #min_rank_au_logsup =  min(rank_au_logsup, na.rm = T),
       #max_rank_au_logsup =  max(rank_au_logsup, na.rm = T),
       #sd_rank_au_logsup =   sd(rank_au_logsup, na.rm = T),
       #mean_rank_au_logsup = mean(rank_au_logsup, na.rm =T),
       #has_rank_90th_logsup = max(fifelse(rank_au_logsup >=0.9,1,0), na.rm = T),
       #has_rank_50th_logsup = max(fifelse(rank_au_logsup >=0.5,1,0), na.rm = T),
       n_authors = n_distinct(author_id))
  , by =c('inst_id','year','main_field', 'name','type','uni_pub','cnrs','fused',
          'rank_inst_akm_norm','fixef_inst_akm','idex'
          ,'alt_inst_fe_rank_norm','alt_inst_fe','future_colleagues_fe',"future_colleagues_rank_norm"
          #,'rank_inst_logsup_norm','fixef_inst_logsup'
          ,'n_authors_sample','n_obs')]%>%
  .[, post := fifelse(year >=2009,1,0)]

unique(ranking_data[name == 'Paris School of Economics'][, list(inst_id, rank_inst_akm_norm, main_field)])

test <- unique(ranking_data[main_field == 'econ'][, list(inst_id, name, type, rank_inst_akm_norm, main_field, n_authors_sample)])

gc()
#rm(sample)
gc()


test <- sample[author_name == "Philippe Aghion"]


test <- unique(ranking_data[rank_inst_akm_norm >=0.9*max(rank_inst_akm_norm, na.rm = T), by = 'main_field'][, 
.(fixef_au_akm = max(max_rank_au_akm)), by = 
list(inst_id,name, main_field,rank_inst_akm_norm)])
to_plot <- ranking_data %>%
  .[, uni_pub := fifelse(uni_pub ==1, "University", "Other")]



# number of observations --------------------------------------------------------------------
ggplot(unique(ranking_data[][, .(inst_id,rank_inst_akm_norm,n_obs, type, uni_pub)]))+
  geom_point(aes(x=log(n_obs), y = rank_inst_akm_norm))
p <- binsreg(y=mean_rank_au_akm, x= mean_rank_colab_akm, # w = ~main_field,
             data = to_plot[year <= 2007]
             ,weights = n_authors,
             by = uni_pub, randcut = 1,
             bycolors = c('black','steelblue'),
             bysymbols = c(16,15))
p  <- p$bins_plot+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  labs(title = '')+xlab('Rank of fixed-effect - Colleagues')+ylab('Rank of average author fixed-effect')
p
save_plot("D:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre_peer.png", p)



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
p
save_plot("D:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre.png", p)


# desc stats --------------------------------------------------------------


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
p
save_plot("D:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre.png", p)


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
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of average author fixed-effect')
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
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of average author fixed-effect')#+
  #ylim(c(0.38,0.6))+geom_abline(intercept = 0, slope = 1) 

p2
p12 <- plot_grid(p1,p2)
p12
save_plot("D:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre_post.png", p12)


p1 <- binsreg(y=min_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
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
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of minimum author fixed-effect')


p1
p2 <- binsreg(y=min_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
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
  labs(title = '')+xlab('Rank of fixed-effect - Institution')+ylab('Rank of minimum author fixed-effect')#+


p2
p12 <- plot_grid(p1,p2)
p12
save_plot("D:\\panel_fr_res\\productivity_results\\ranking_min_binsreg_pre_post.png", p12)




p1 <- binsreg(y=mean_rank_au_akm, x= mean_rank_colab_akm, # w = ~main_field,
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
  labs(title = '')+xlab('Rank of fixed-effect - Colleagues')+ylab('Rank of average author fixed-effect')


p1
p2 <- binsreg(y=mean_rank_au_akm, x= mean_rank_colab_akm, # w = ~main_field,
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
  labs(title = '')+xlab('Rank of fixed-effect - Colleagues')+ylab('Rank of average author fixed-effect')#+
#ylim(c(0.38,0.6))+geom_abline(intercept = 0, slope = 1) 

p2
p12 <- plot_grid(p1,p2)
p12
save_plot("D:\\panel_fr_res\\productivity_results\\ranking_binsreg_pre_post_peers.png", p12)



ggplot(ranking_data[min_rank_au_akm<Inf  
                   # & n_obs >5 & n_obs <=1000
                    & entrant == 1
                     & ( n_authors <10000 & n_authors >=5) 
                     # &inst_type %in% c('company','facility'
                     #                   ,'education','government'
                     #                   ,'nonprofit'
                     #                   ) 
                    #& year %in% c(2005,2015)
                    #& main_field == 'econ'
                   ]%>%
         .[, 
           uni_pub_post_cnrs := paste0(ifelse(uni_pub ==1, 'uni_','not_uni_')
                                       , ifelse(post==1, 'post_','pre_'),ifelse(cnrs==1, 'crns','noncnrs'))]
       )+
  geom_point(aes(x= rank_inst_akm_norm, y=min_rank_au_akm, size =n_authors, color = uni_pub_post_cnrs ))+
  geom_smooth(aes(x= rank_inst_akm_norm, y=min_rank_au_akm, size =n_authors, color =uni_pub_post_cnrs),
              fullrange = F, alpha =0.2)+
  scale_color_manual(values= c('goldenrod3','skyblue4','goldenrod2','skyblue1',
                               'firebrick4','seagreen3','firebrick1','seagreen2'))

ggplot(ranking_data[min_rank_au_akm<Inf  
                    # & n_obs >5 & n_obs <=1000
                    & entrant == 1
                    & ( n_authors <10000 & n_authors >=5) 
                    # &inst_type %in% c('company','facility'
                    #                   ,'education','government'
                    #                   ,'nonprofit'
                    #                   ) 
                    #& year %in% c(2005,2015)
                    #& main_field == 'econ'
]%>%
  .[, 
    uni_pub_post_cnrs := paste0(ifelse(uni_pub ==1, 'uni_','not_uni_')
                                , ifelse(post==1, 'post_','pre_'),ifelse(cnrs==1, 'crns','noncnrs'))]
)+
  geom_point(aes(x= rank_inst_akm_norm, y=mean_rank_au_akm, size =n_authors, color = uni_pub_post_cnrs ))+
  geom_smooth(aes(x= rank_inst_akm_norm, y=mean_rank_au_akm, size =n_authors, color =uni_pub_post_cnrs),
              fullrange = F, alpha =0.2)+
  scale_color_manual(values= c('goldenrod3','skyblue4','goldenrod2','skyblue1',
                               'firebrick4','seagreen3','firebrick1','seagreen2'))



ggplot(ranking_data[min_rank_au_akm<Inf  
                    & n_obs >=50
                    & entrant == 1
                    #& ( n_authors <5000 & n_authors >=10) 
                    &inst_type %in% c('company','facility'
                                      ,'education','government'
                                      ,'nonprofit'
                    ) 
                    # & main_field == 'bioc'
]%>%
  .[, 
    uni_pub_post_cnrs := paste0(ifelse(uni_pub ==1, 'uni_','not_uni_')
                                , ifelse(post==1, 'post_','pre_'),ifelse(cnrs==1, 'crns','noncnrs'))]%>%
  .[, .(min_rank_au_akm = mean(min_rank_au_akm, na.rm = T),
        n_authors = mean(n_authors)), by = c('uni_pub_post_cnrs','rank_inst_akm_norm','inst_id','post')]
)+
  geom_point(aes(x= rank_inst_akm_norm, y=min_rank_au_akm, size =n_authors, color = uni_pub_post_cnrs ))+
  geom_smooth(aes(x= rank_inst_akm_norm, y=min_rank_au_akm, size =n_authors, color =uni_pub_post_cnrs),
              fullrange = F, alpha =0.1)+
  scale_color_manual(values= c('goldenrod3','skyblue4','goldenrod2','skyblue1',
                               'firebrick4','seagreen4','firebrick1','seagreen2'))


binsreg(y=mean_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
        data = ranking_data[min_rank_au_akm<Inf & entrant == 1 &
                              inst_type %in% c('education','government','facility')][, 
uni_pub_post_cnrs := paste0(ifelse(uni_pub ==1, 'uni_','not_uni_')
                            , ifelse(post==1, 'post_','pre_'),ifelse(cnrs==1, 'crns','noncnrs'))], 
        weights = n_authors,
        by = uni_pub_post_cnrs, randcut = 1,
bycolors = c('goldenrod3','skyblue4','goldenrod2','skyblue1', 'firebrick4','seagreen4','firebrick1','seagreen1'),
bysymbols = c(3,18,3,18,15,16,15,16),
polyreg = 3)


binsreg(y=mean_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
        data = ranking_data[min_rank_au_akm<Inf & entrant == 1 &
                              inst_type %in% c('education','government','facility')][, 
                                                                                     uni_pub_post_cnrs := paste0(ifelse(uni_pub ==1, 'uni_','not_uni_')
                                                                                                                 , ifelse(post==1, 'post_','pre_'),ifelse(cnrs==1, 'crns','noncnrs'))], 
        weights = n_authors,
        by = uni_pub_post_cnrs, randcut = 1,
        bycolors = c('goldenrod3','skyblue4','goldenrod2','skyblue1', 'firebrick4','seagreen4','firebrick1','seagreen1'),
        bysymbols = c(3,18,3,18,15,16,15,16),
        polyreg = 3)



binsreg(y=min_rank_au_logsup, x= rank_inst_logsup_norm, # w = main_field,
        data = ranking_data[min_rank_au_akm<Inf & 
                              inst_type %in% c('education','government','facility')
                            & entrant == 1
                            ][, 
                                                                                     uni_pub_post_cnrs := paste0(ifelse(uni_pub ==1, 'uni_','not_uni_')
                                                                                                                 , ifelse(post==1, 'post_','pre_'),ifelse(cnrs==1, 'crns','noncnrs'))], 
        weights = n_authors,
        by = uni_pub_post_cnrs, randcut = 1,
        bycolors = c('goldenrod3','skyblue4','goldenrod2','skyblue1', 'firebrick4','seagreen4','firebrick1','seagreen1'),
        bysymbols = c(3,18,3,18,15,16,15,16),
        polyreg = 3)


binsreg(y=max_rank_au_akm, x= rank_inst_akm,  w = main_field,
        data = ranking_data[min_rank_au_logsup<Inf & 
                              inst_type %in% c('education','government','facility')][, uni_pub_post := paste0(ifelse(uni_pub ==1, 'treat_', 
                                                                                                                     ifelse(cnrs == 1, 'cnrs_', 'ctrl_')), 
                                                                                                              ifelse(post == 1, 'post','pre'))], weights = n_authors,
        by = uni_pub_post, polyreg = 1, randcut = 1)
binsreg(y=mean_rank_au_akm, x= rank_inst_akm, w = main_field,
        data = ranking_data[min_rank_au_logsup<Inf & 
                              inst_type %in% c('education','government','facility')][, uni_pub_post := paste0(ifelse(uni_pub ==1, 'treat_', 
                                                                                                                     ifelse(cnrs == 1, 'cnrs_', 'ctrl_')), 
                                                                                                              ifelse(post == 1, 'post','pre'))], weights = n_authors,
        by = uni_pub_post, polyreg = 1, randcut = 1)

test <- ranking_data[year == 2005 & inst_type == "education"]

ggplot(ranking_data[ inst_type != "company" & main_field == 'chem'])+
  geom_jitter(aes(x = rank_inst_akm, y = min_rank_au_akm, color= interaction(uni_pub, cnrs), size = n_authors_sample))

ggplot(ranking_data[year == 2019& main_field == 'chem'])+
  geom_point(aes(x = rank_inst_akm, y = fixef_inst_akm, color= interaction(uni_pub, cnrs)))

# validation ? -----------------------------------------------------------

validate_spec = ranking_data[year >=2000 & year<=2006 
                  & uni_pub == 1
]

validate_spec[min_rank_au_akm>0.7 & rank_inst_akm <0.3]

ggplot(validate_spec[min_rank_au_akm <Inf & year == 2005]) +
  geom_point(aes(x = rank_inst_akm_norm, y = min_rank_au_akm, color = main_field, size = n_authors))

ggplot(validate_spec[mean_rank_au_akm <Inf & year == 2005]) +
  geom_point(aes(x = rank_inst_akm_norm, y = mean_rank_au_akm, color = main_field, size = n_authors))

ggplot(ranking_data[min_rank_au_akm <Inf & uni_pub == 1 & cnrs == 0 & !is.na(main_field)
                    & rank_inst_akm_norm < 0.99 & rank_inst_akm_norm >0.01 & n_authors >1]) +
  geom_point(aes(x = rank_inst_akm_norm, y = min_rank_au_akm), color= "firebrick", alpha = 0.8)+
  geom_point(aes(x = rank_inst_akm_norm, y = mean_rank_au_akm), color= "gold", alpha = 0.8)+
  geom_point(aes(x = rank_inst_akm_norm, y = max_rank_au_akm), color= "seagreen", alpha = 0.8)
ggplot(validate_spec[min_rank_au_akm <Inf]) +
  geom_jitter(aes(x = rank_inst_akm_norm, y = max_rank_au_akm))
ggplot(validate_spec) +
  geom_jitter(aes(x = rank_inst_akm_norm, y = sd_rank_au_akm))

binsreg(y=min_rank_au_akm, x= rank_inst,  w = main_field,
        data = validate_spec[min_rank_au_akm <Inf], nbins = 50, weights = n_authors)
binsreg(y=max_rank_au_akm, x= rank_inst,  w = main_field,
        data = validate_spec[max_rank_au_akm >-Inf], nbins = 50, weights = n_authors)
binsreg(y=mean_rank_au_akm, x= rank_inst, w = main_field,
        data = validate_spec, nbins = 50, weights = n_authors)


# test np estimation ------------------------------------------------------
ranking_data <- ranking_data[, uni_pub_post := case_when(cnrs == 0 & uni_pub == 0 & post == 0 & type != 'company'~0,
                                                         cnrs == 0 & uni_pub == 0 & post == 1 & type != 'company'~1,
                                                         cnrs == 0 & uni_pub == 1 & post == 0 ~2,
                                                         cnrs == 0 & uni_pub == 1 & post == 1 ~3,
                                                         cnrs == 1 & uni_pub == 0 & post == 0 ~4,
                                                         cnrs == 1 & uni_pub == 0 & post == 1 ~5,
                                                         cnrs == 1 & uni_pub == 1 & post == 0 ~6,
                                                         cnrs == 1 & uni_pub == 1 & post == 1 ~7,
                                                         cnrs == 0 & uni_pub == 0 & post == 0 & type == 'company'~8,
                                                         cnrs == 0 & uni_pub == 0 & post == 1 & type == 'company'~9
                                                         
                                                         )][!is.na(uni_pub_post)]
np_reg <- frfast((min_rank_au_akm) ~ (rank_inst_akm_norm) + uni_pub_post, #+ main_field + inst_type,
                p= 2, seed = 12, data = ranking_data[min_rank_au_akm <Inf
                                                     & !is.na(main_field)
                                                     & n_authors >1 ],
                nboot = 100, weights = n_authors)
plot(np_reg, fac = 3,CIcol = 'blue', CItype = "l",der = 0)
plot(np_reg, fac = 2,CIcol = 'blue', CItype = "l",der = 0)
plot(np_reg, fac = 0,CIcol = 'blue', CItype = "l",der = 0)

plot(np_reg,fac=1, CIcol = 'blue', CItype = "l",diffwith = 0,der = 0)
plot(np_reg,fac=3, CIcol = 'blue', CItype = "l",diffwith = 2,der = 0)
plot(np_reg,fac=3, CIcol = 'blue', CItype = "l",diffwith = 0,der = 0)

plot(np_reg,fac=5, CIcol = 'blue', CItype = "l",diffwith = 4,der = 0)
plot(np_reg,fac=7, CIcol = 'blue', CItype = "l",diffwith = 6,der = 0)
plot(np_reg,fac=9, CIcol = 'blue', CItype = "l",diffwith = 8,der = 0)
plot(np_reg,fac=2, CIcol = 'blue', CItype = "l",diffwith = 0,der = 0)
plot(np_reg,fac=2, CIcol = 'blue', CItype = "l",diffwith = 0,der = 0)


np_reg <- frfast((min_rank_au_logsup) ~ (rank_inst_logsup_norm) + post + main_field + inst_type,
                 p= 2, seed = 12, data = ranking_data[min_rank_au_logsup <Inf & uni_pub == 1 & cnrs == 0 & !is.na(main_field)],
                 nboot = 100, weights = n_authors)
plot(np_reg, CIcol = 'blue', CItype = "l", post = 0, der = 0)

np_reg <- frfast(min_rank_au_logsup ~ rank_inst_logsup+ post + main_field+ inst_type,
                 p= 2, seed = 12, data = ranking_data[min_rank_au_logsup <Inf & uni_pub == 0 & cnrs == 0],
                 nboot = 100, weights = n_authors)
plot(np_reg, CIcol = 'blue', CItype = "l",post = 0,der = 0)

np_reg <- frfast(min_rank_au_logsup ~ rank_inst_logsup+ post + main_field +inst_type,
                 p= 2, seed = 12, data = ranking_data[min_rank_au_logsup <Inf & uni_pub == 0 & cnrs == 1],
                 nboot = 100, weights = n_authors)
plot(np_reg, CIcol = 'blue', CItype = "l",der = 0)


ggplot(ranking_data[, .(min_rank_au_akm=mean(min_rank_au_akm)), by = c('uni_pub','cnrs','year')])+
  geom_line(aes(x=year, y=min_rank_au_akm, color = interaction(uni_pub,cnrs)))


# ranking regressions -----------------------------------------------------

formula_ranking = paste0("c(mean_rank_au_akm,min_rank_au_akm) ~"
                        ,"i(year, alt_inst_fe_rank_norm, 2008)"
                        ,"+i(year, uni_pub, 2008) "
                        ,"+i(year, uni_pub*alt_inst_fe_rank_norm, 2008)"
                        ,"+i(year, has_idex, 2008) "
                        ,"+i(year, has_idex*alt_inst_fe_rank_norm, 2008)"
                        ,"+i(year, has_idex*uni_pub, 2008) "
                        ,"+i(year, has_idex*uni_pub*alt_inst_fe_rank_norm, 2008)"
                        
                        
                        ,"|"
                         , "inst_id^main_field+ year"
                        , '+ main_field^year'
                        , '+ type^year'
                        )

test_ranking <- feols(as.formula(formula_ranking),
                      ranking_data%>%
                        .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
                      , weights = (ranking_data
                                   
                                   )$n_authors
                      )
iplot(test_ranking, main = 'Overall ranking')
iplot(test_ranking,i.select = 2, main = 'Effect on avg author rank of uni')
iplot(test_ranking,i.select = 3, main = 'Ranking for universities')
iplot(test_ranking,i.select = 4, main = 'Effect on avg author rank of idex')
iplot(test_ranking,i.select = 5, main = 'Ranking for idex')
iplot(test_ranking,i.select = 6, main = 'Effect on avg author rank of uni * idex')
iplot(test_ranking,i.select = 7, main = 'Ranking for universities * idex')



etable(test_ranking)
test_ranking <- feols(as.formula(str_replace_all(str_replace_all(formula_ranking, 'year', 'post'), "2008", "0")),
                      ranking_data, weights = ranking_data$n_authors)
etable(test_ranking)

etable(test_ranking, keep =c( 'uni_pub','idex') 
       ,file = "D:\\panel_fr_res\\productivity_results\\ranking.tex", replace = TRUE
       )



formula_ranking = paste0("c(log(mean_rank_au_akm),log(min_rank_au_akm)) ~"
                         ,"i(year, alt_inst_fe_rank_norm, 2008)"
                         ,"+i(year, uni_pub, 2008) "
                         ,"+i(year, uni_pub*alt_inst_fe_rank_norm, 2008)"
                         ,"+i(year, has_idex, 2008) "
                         ,"+i(year, has_idex*alt_inst_fe_rank_norm, 2008)"
                         ,"+i(year, has_idex*uni_pub, 2008) "
                         ,"+i(year, has_idex*uni_pub*alt_inst_fe_rank_norm, 2008)"
                         
                         ,"|"
                         , "inst_id^main_field+ year"
                         , '+ main_field^year'
                         , '+ type^year'
)

test_ranking <- feols(as.formula(formula_ranking),
                      ranking_data, weights = ranking_data$n_authors)
iplot(test_ranking, main = 'Overall ranking')
iplot(test_ranking,i.select = 2, main = 'Effect on avg author rank of uni')
iplot(test_ranking,i.select = 3, main = 'Ranking for universities')
iplot(test_ranking,i.select = 4, main = 'Effect on avg author rank of idex')
iplot(test_ranking,i.select = 5, main = 'Ranking for idex')
iplot(test_ranking,i.select = 6, main = 'Effect on avg author rank of uni * idex')
iplot(test_ranking,i.select = 7, main = 'Ranking for universities * idex')

etable(test_ranking)
test_ranking <- feols(as.formula(str_replace_all(str_replace_all(formula_ranking, 'year', 'post'), "2008", "0")),
                      ranking_data[, post:=as.numeric(year>=2009)], weights = ranking_data$n_authors)
etable(test_ranking)

etable(test_ranking, keep =c( 'uni_pub','idex') 
       ,file = "D:\\panel_fr_res\\productivity_results\\ranking_log.tex", replace = TRUE
)


