rm(list = ls())
gc()
library('arrow')
library('data.table')
library('fixest')
library('tidyverse')
library('npregfast')
library('binsreg')
sample <- fread( "E:\\panel_fr_res\\test_with_fixed_effects.csv") %>% #test_with_fixed_effects.csv
   .[
    , ':='(rank_au_akm_norm = rank_au_akm/max(rank_au_akm, na.rm = T),
           rank_au_logsup_norm = rank_au_logsup/max(rank_au_logsup, na.rm =T),
           rank_inst_akm_norm = rank_inst_akm/max(rank_inst_akm, na.rm = T),
           rank_inst_logsup_norm = rank_inst_logsup/max(rank_inst_logsup, na.rm = T)
           ), by = 'main_field'
  ]
sample <- sample %>%
  .[, inst_id_set:=paste0(unique(list(inst_id)), collapse = ","), by = c('author_id','year') ]%>%
  .[, lag_inst_id_set := lag(inst_id_set, order_by = year), by = author_id] %>%
  .[, entrant := ifelse(!str_detect(lag_inst_id_set, inst_id) | is.na(lag_inst_id_set), 1,0)]
gc()

test <- unique(sample[country == 'FR'][rank_inst_akm >=0.9*max(rank_inst_akm, na.rm = T), by = 'main_field'][, 
    list(inst_id,inst_name, main_field,rank_inst_akm_norm)])[order(main_field,rank_inst_akm_norm)]

unique(sample[inst_name == 'Paris School of Economics'][, list(inst_id, rank_au_akm_norm, rank_inst_akm_norm, n_authors_sample, main_field,n_authors_w_several_inst)])

ggplot(sample[inst_name == 'Paris School of Economics' & year %in% c(2005, 2016) & main_field == "econ"])+
  geom_density(aes(x=rank_au_akm_norm, color = as.factor(year)), alpha = 0.5)

ggplot(sample[year %in% c(2005, 2016) & main_field == "econ"])+
  geom_point(aes(x=rank_au_akm_norm, y=fixef_au_akm, color = interaction(year, uni_pub)), alpha = 0.5)+
  geom_vline(aes(xintercept= 0.025))+geom_vline(aes(xintercept= 0.975))
  #geom_smooth(aes(x=rank_inst_akm, y=rank_au_akm, color = interaction(year, uni_pub)), alpha = 0.5)

ggplot(sample[year %in% c(2005, 2016)# & main_field == "econ"
              ])+
  geom_point(aes(x=fixef_inst_akm, y=fixef_au_akm, color = interaction(year, uni_pub)), alpha = 0.5)+
  geom_smooth(aes(x=fixef_inst_akm, y=fixef_au_akm, color = interaction(year, uni_pub)), alpha = 0.5, formula = y ~x)

ggplot(sample[year %in% c(2005, 2016)])+
  geom_density(aes(x=fixef_inst_akm, fill = as.factor(uni_pub)))
unique(sample[,list(author_id, n_y_in_sample)])[, .N, by = 'n_y_in_sample'][order(n_y_in_sample)]

ranking_data <- sample[,n_obs_au := .N, by = author_id]%>%
                       .[year >=2000 & year<=2020 & country == "FR" #& n_y_in_sample >10
                       & rank_au_akm_norm >0.025 & rank_au_akm_norm < 0.975
                       & rank_inst_akm_norm >0.025 & rank_inst_akm_norm < 0.975
                       & n_obs_au >=10
                  #    & inst_type!= 'company'
                  ] %>%
                  .[
  , ':='(rank_au_akm = (rank_au_akm-min(rank_au_akm, na.rm = T))/(max(rank_au_akm, na.rm = T)-min(rank_au_akm, na.rm = T)),
         rank_au_logsup =(rank_au_logsup-min(rank_au_logsup, na.rm = T))/(max(rank_au_logsup, na.rm = T)-min(rank_au_logsup, na.rm = T)),
         rank_inst_akm_norm = (rank_inst_akm_norm-min(rank_inst_akm_norm, na.rm = T))/(max(rank_inst_akm_norm, na.rm = T)-min(rank_inst_akm_norm, na.rm = T)),
         rank_inst_logsup_norm = rank_inst_logsup_norm/max(rank_inst_logsup_norm, na.rm = T)), by = 'main_field'
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
       min_rank_au_logsup =  min(rank_au_logsup, na.rm = T),
       max_rank_au_logsup =  max(rank_au_logsup, na.rm = T),
       sd_rank_au_logsup =   sd(rank_au_logsup, na.rm = T),
       mean_rank_au_logsup = mean(rank_au_logsup, na.rm =T),
       has_rank_90th_logsup = max(fifelse(rank_au_logsup >=0.9,1,0), na.rm = T),
       has_rank_50th_logsup = max(fifelse(rank_au_logsup >=0.5,1,0), na.rm = T),
       n_authors = n_distinct(author_id))
  , by =c('inst_id','year','main_field', 'inst_name','inst_type','uni_pub','cnrs', "entrant",
          'rank_inst_akm_norm','rank_inst_logsup_norm','fixef_inst_akm','fixef_inst_logsup','n_authors_sample','n_obs')]%>%
  .[, post := fifelse(year >=2009,1,0)]

unique(ranking_data[inst_name == 'Paris School of Economics'][, list(inst_id, rank_inst_akm_norm, main_field)])

test <- unique(ranking_data[main_field == 'econ'][, list(inst_id, inst_name, inst_type, rank_inst_akm_norm, main_field, n_authors_sample)])

gc()
#rm(sample)
gc()


test <- sample[author_name == "Philippe Aghion"]


test <- unique(ranking_data[rank_inst_akm_norm >=0.9*max(rank_inst_akm_norm, na.rm = T), by = 'main_field'][, 
.(fixef_au_akm = max(max_rank_au_akm)), by = 
list(inst_id,inst_name, main_field,rank_inst_akm_norm)])



# number of observations --------------------------------------------------------------------
ggplot(unique(ranking_data[entrant==1][, .(inst_id,rank_inst_akm_norm,n_obs, inst_type, uni_pub)]))+
  geom_point(aes(x=log(n_obs), y = rank_inst_akm_norm))


# desc stats --------------------------------------------------------------



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


binsreg(y=min_rank_au_akm, x= rank_inst_akm_norm, # w = ~main_field,
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
  geom_point(aes(x = rank_inst_akm, y = min_rank_au_akm, color = main_field, size = n_authors))

ggplot(validate_spec[mean_rank_au_akm <Inf & year == 2005]) +
  geom_point(aes(x = rank_inst_akm, y = mean_rank_au_akm, color = main_field, size = n_authors))

ggplot(validate_spec[min_rank_au_logsup <Inf]) +
  geom_jitter(aes(x = rank_inst_logsup, y = min_rank_au_logsup, ))

ggplot(ranking_data[min_rank_au_akm <Inf & uni_pub == 1 & cnrs == 0 & entrant==0& !is.na(main_field)
                    & rank_inst_akm_norm < 0.99 & rank_inst_akm_norm >0.01 & n_authors >1]) +
  geom_point(aes(x = rank_inst_akm_norm, y = min_rank_au_akm), color= "firebrick", alpha = 0.8)+
  geom_point(aes(x = rank_inst_akm_norm, y = mean_rank_au_akm), color= "gold", alpha = 0.8)+
  geom_point(aes(x = rank_inst_akm_norm, y = max_rank_au_akm), color= "seagreen", alpha = 0.8)
ggplot(validate_spec[min_rank_au_akm <Inf]) +
  geom_jitter(aes(x = rank_inst, y = max_rank_au_akm))
ggplot(validate_spec) +
  geom_jitter(aes(x = rank_inst, y = sd_rank_au_akm))

binsreg(y=min_rank_au_akm, x= rank_inst,  w = main_field,
        data = validate_spec[min_rank_au_akm <Inf], nbins = 50, weights = n_authors)
binsreg(y=max_rank_au_akm, x= rank_inst,  w = main_field,
        data = validate_spec[max_rank_au_akm >-Inf], nbins = 50, weights = n_authors)
binsreg(y=mean_rank_au_akm, x= rank_inst, w = main_field,
        data = validate_spec, nbins = 50, weights = n_authors)


# test np estimation ------------------------------------------------------
ranking_data <- ranking_data[, uni_pub_post := case_when(cnrs == 0 & uni_pub == 0 & post == 0 & inst_type != 'company'~0,
                                                         cnrs == 0 & uni_pub == 0 & post == 1 & inst_type != 'company'~1,
                                                         cnrs == 0 & uni_pub == 1 & post == 0 ~2,
                                                         cnrs == 0 & uni_pub == 1 & post == 1 ~3,
                                                         cnrs == 1 & uni_pub == 0 & post == 0 ~4,
                                                         cnrs == 1 & uni_pub == 0 & post == 1 ~5,
                                                         cnrs == 1 & uni_pub == 1 & post == 0 ~6,
                                                         cnrs == 1 & uni_pub == 1 & post == 1 ~7,
                                                         cnrs == 0 & uni_pub == 0 & post == 0 & inst_type == 'company'~8,
                                                         cnrs == 0 & uni_pub == 0 & post == 1 & inst_type == 'company'~9
                                                         
                                                         )][!is.na(uni_pub_post)]
np_reg <- frfast((min_rank_au_akm) ~ (rank_inst_akm_norm) + uni_pub_post, #+ main_field + inst_type,
                p= 2, seed = 12, data = ranking_data[min_rank_au_akm <Inf
                                                     & !is.na(main_field)
                                                     & n_authors >1 & entrant == 1],
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

formula_ranking = paste0("log(mean_rank_au_akm) ~ i(year, uni_pub*(log(rank_inst_akm_norm) +log(rank_inst_akm_norm)^2), 2007) +i(year, uni_pub, 2007) + log(rank_inst_akm_norm)*as.factor(year)|"
                         , "inst_id^main_field+main_field^year+inst_type^year+ cnrs^year")

test_ranking <- feols(as.formula(formula_ranking),
                      ranking_data[entrant == 0], weights = ranking_data[entrant == 0]$n_authors)
iplot(test_ranking)
iplot(test_ranking,i.select = 2)
etable(test_ranking)
test_ranking <- feols(as.formula(str_replace(str_replace(formula_ranking, 'year', 'post'), "2007", "0")),
                      ranking_data, weights = ranking_data$n_authors)
etable(test_ranking, keep =c( 'uni_pub'))

simpler_formula <- paste0("mean_rank_au_akm ~i(year, uni_pub*(rank_inst_akm_norm +rank_inst_akm_norm^2), 2007) +  i(year, uni_pub, 2007) + rank_inst_akm_norm*as.factor(year)|"
                          , "inst_id^main_field+main_field^year+inst_type^year+ cnrs^year ")
test_ranking <- feols(as.formula(simpler_formula),
                      ranking_data, weights = ranking_data$n_authors)
iplot(test_ranking)

iplot(test_ranking,i.select = 2)

