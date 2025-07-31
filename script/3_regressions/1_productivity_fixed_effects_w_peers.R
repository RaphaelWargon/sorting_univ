rm(list = ls())
gc()
library('pacman')

p_load('arrow'
,'data.table'
,'fixest'
,'tidyverse'
,'binsreg',
'DescTools',
'cowplot')
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}


inputpath <- "E:\\panel_fr_res\\panel_smoothed_w_theses.parquet"

#inputpath <- "C:\\Users\\rapha\\Desktop\\panel_smoothed.parquet"
#
ds <- open_dataset(inputpath) 
ds$schema$names

ds <- open_dataset(inputpath) %>%
  filter(#all_y_in_FR >= (last_year-entry_year +1)/4
          last_year-entry_year >2
         & entry_year >=1965 
         & year >= 1997

  ) %>%
  select(author_id, author_name, year,
         entry_year,
         inst_id, name, type, main_field,
         publications_raw,citations_raw,  
         publications, citations,
         avg_rank_source_raw,nr_source_btm_50pct_raw,
         nr_source_mid_40pct_raw, nr_source_top_20pct_raw,nr_source_top_10pct_raw,nr_source_top_5pct_raw,
         avg_rank_source,nr_source_btm_50pct,
         nr_source_mid_40pct, nr_source_top_20pct,nr_source_top_10pct,nr_source_top_5pct,
         period_inst, uni_pub, cnrs,type, fused, idex, main_topic, city, prive, public, ecole, 
         n_inst_y,
         n_phd_students, in_supervisor_inst, 
         in_referee_inst,in_jury_inst, thesis_year #, inst_set_this_year
         )
ds <- as.data.table(ds)

ds <- unique(ds)
#nrow(unique(ds[,list(author_id)])) #197840 w min_period 2 min_y_f 1/2
#  245495 w min_period 3 min_y_f 1/4

gc()
unique(ds[, list(author_id,entry_year)][])[, .N, by = 'entry_year'][order(entry_year)]


unique(sample_df[name == 'Paris School of Economics'][, list(inst_id, n_authors_sample, main_field,main_field_recoded,max_field, field_value)])
unique(sample_df[name == 'Paris School of Business'][, list(inst_id, n_authors_sample, main_field,main_field_recoded,max_field, field_value)])

unique(sample_df[name == 'Paris School of Economics' & is.na(main_field) ][, list(inst_id, author_id,author_name,main_field_recoded)])

unique(sample_df[author_name == 'Philippe Aghion' ][, list(inst_id,name, author_id,author_name,main_field_recoded)])

unique(sample_df[str_detect(name, 'Université Paris 1')|
            str_detect(name, 'Panthéon-Sorbonne')][, list(inst_id, name, n_authors_sample, main_field,max_field,main_field_recoded, field_value)])
unique(sample_df[str_detect(name, 'Université Paris 1')|
            str_detect(name, 'Panthéon-Sorbonne')&main_field_recoded !='medi'][, list(author_id, author_name, n_authors_sample, main_field,max_field,main_field_recoded, field_value)])


##
inst_to_exclude <- c('I4210159570')
cols_to_wins <- colnames(ds)[4:19]
sample_df <- ds[entry_year >=1965 
             & entry_year <=2006
             & year >= 1997
             & citations >0
             #& inst_type %in% c('facility','education')
             #& country == 'FR'
             & !(inst_id %in% inst_to_exclude) 
             & !(is.na(main_field))
             & !(inst_id %in% c('archive',"other"))
             ]%>%
  .[, ':='(n_inst_id_sample = n_distinct(inst_id),
           main_field = first(main_field, na_rm = TRUE),
           inst_id = ifelse(is.na(inst_id), 
                            lag(inst_id, order_by = year), inst_id)), by = 'author_id'] %>%
  .[, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
           n_authors_sample = n_distinct(author_id) ), by = c('inst_id','main_field')]%>%
  .[,n_by_field := n_distinct(author_id), by = 'main_field']%>%
  .[, n_y_in_sample := n_distinct(year), by = 'author_id']%>%
  .[n_authors_w_several_inst > 0 & n_y_in_sample >=2] %>%
  .[,max_field := dplyr::first(ifelse(n_authors_sample == max(n_authors_sample* as.numeric(!is.na(main_field)) ) & !is.na(main_field), main_field, NA), na_rm = T),
    by = "inst_id"] %>%
  .[,field_value := n_authors_sample/n_distinct(author_id),by = 'inst_id']%>%
  .[,max_field_value := max(field_value), by = 'inst_id'] %>%
  .[,main_field_recoded := ifelse( ((field_value <0.1| n_authors_sample<=5) ) | is.na(main_field), max_field, main_field)]%>%
  .[, main_field := main_field_recoded]%>%
  .[,n_authors_sample := n_distinct(author_id), by = c('inst_id','main_field')]%>%
  #.[n_authors_sample >= 5]%>%
  .[ , ':='(n_y_in_sample = n_distinct(year),
            n_obs_au = .N), by = c('author_id','main_field')]%>%
 # .[n_obs_au >=3]%>%
  .[,inst_id_field := paste0(inst_id, main_field)] %>%
  .[, entry_cohort := fifelse(entry_year <= 2000, floor(entry_year/5)*5, entry_year)]%>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]%>%
  .[, n_obs_univ := .N, by = 'inst_id']%>%
  .[, ":="(avg_rank_source_raw = ifelse(is.na(avg_rank_source_raw),0,avg_rank_source_raw),
           nr_source_top_10pct = ifelse(is.na(nr_source_top_10pct),0,nr_source_top_10pct),
           nr_source_top_5pct = ifelse(is.na(nr_source_top_5pct),0,nr_source_top_5pct),
           nr_source_top_10pct_raw = ifelse(is.na(nr_source_top_10pct_raw),0,nr_source_top_10pct_raw),
           nr_source_top_5pct_raw = ifelse(is.na(nr_source_top_5pct_raw),0,nr_source_top_5pct_raw)
           )] %>%
  .[, normalized_avg_rank_source_raw :=avg_rank_source_raw/max(avg_rank_source_raw), by = c('year','main_field') ]
#rm(ds)
gc()

nrow(unique(sample_df[, list(author_id)]))#169926 authors

sample_df <- unique(sample_df[, ':='(log_cit_w_p =log(citations/publications),
                               log_log_cit_w_p = log(log(citations/publications)),
                               log_cit_w_p_raw = log(citations_raw/publications_raw),
                               log_log_cit_w_p_raw = log(log(citations_raw/publications_raw))
                        )])
#summary(sample_df)

to_plot <- sample_df[, .(log_cit_w_p = mean(log_cit_w_p, na.rm =T),
                         citations = mean(citations, na.rm = T),
                                        citations_raw = mean(citations_raw, na.rm = T),
                                        publications = mean(publications_raw, na.rm=T), 
                                        avg_rank_source_raw = mean(avg_rank_source_raw*publications_raw, na.rm=T), 
                                        nr_source_top_5pct = mean(nr_source_top_5pct, na.rm=T))
                                       ,by= c('uni_pub','year')]


ggplot(to_plot)+
  geom_line(aes(x=year, y= publications, color =interaction(uni_pub)))+
  scale_color_manual(values = c('seagreen','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average publications')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)

p <- ggplot(to_plot)+
  geom_line(aes(x=year, y= avg_rank_source_raw, color =interaction(uni_pub)), show.legend = FALSE)+
  scale_color_manual(values = c('black','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average journal-quality weighted publications')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)
p
save_plot("E:\\panel_fr_res\\desc_stats\\avg_rankw_pub.png", p)


p <- ggplot(to_plot)+
  geom_line(aes(x=year, y= log(citations), color =interaction(uni_pub)))+
  scale_color_manual(values = c('black','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average citations')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)
p
save_plot("E:\\panel_fr_res\\desc_stats\\avg_cit.png", p)

test <- sample_df[inst_id == "I4210144005"]
sample_df[log_cit_w_p == -Inf][, list(citations, publications)]

sample_df <- sample_df %>%
  .[, colab := paste0(sort(unique((author_id))), collapse =','), by = c('inst_id_field','year')]%>%
  .[, colab := str_replace_all(str_remove_all(str_remove(colab, author_id), ',$|^,'), ',{2,}', ',')]

test <- sample_df %>%
  .[, .N, by = 'colab']

ggplot(sample_df[, .N, by = colab])+
  geom_histogram(aes(x=log(N)))
# regressions -------------------------------------------------------------
formula_res <- paste0('log(citations)~ 1 + i(year, uni_pub, 2008)',
                  '+ i(year, has_idex*uni_pub, 2008)',
                  '+ i(year, has_idex*(1-uni_pub), 2008)',
                  '| '
                  ,' type^year '
                  ,'+ cnrs^year'
                  ,'+ fused^year'
                  ,'+ city^year'
                  ,'+ main_field^entry_cohort^year '
)
reg_to_residualize <- feols(as.formula(formula_res)
                            ,data = sample_df %>%
                              .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
)
gc()

sample_df$y <- residuals(reg_to_residualize)

ggplot(sample_df)+geom_density(aes(x=y))

gc()


formula <- paste0('y~ 1',
                  '| ',
                  ' author_id + inst_id_field^year '
)
test_brutal <- feols(as.formula(formula)
                     ,data = sample_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
                       )
gc()

sample_df <- sample_df %>%
  .[, alpha_hat := fixef(test_brutal)$author_id[author_id]]
gc()
ggplot(unique(sample_df[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))

sample_df <- sample_df %>%
  .[, ":="( sum_alpha_hat = sum(alpha_hat),
            n_colab = n_distinct(author_id)
            ),
    by = c('inst_id_field','year')] %>%
  .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
ggplot(sample_df)+geom_density(aes(x=avg_alpha_i_bar))

formula_qual <- paste0('y~ avg_alpha_i_bar',
                  '| ',
                  ' author_id + inst_id_field^year '
)
test_qual <- feols(as.formula(formula_qual)
                     ,data = sample_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
)
gc()

est_gamma <- test_qual$coeftable
est_gamma$i <- 1
est_gamma
est_diff_alpha <- data.table(NA,1)
names(est_diff_alpha) <- c('diff','i')
for(i in 2:10){
  print(paste0('Process for i=', i))
  sample_df <- sample_df %>%
    .[, ':='(alpha_hat_i_minus_1 = alpha_hat,
      alpha_hat = fixef(test_qual)$author_id[author_id],
      y_i = y- est_gamma[[i-1,1]]*avg_alpha_i_bar )]
  est_diff_i <- data.table(unique(sample_df[,list(author_id,alpha_hat_i_minus_1, alpha_hat)])[,.(mean((alpha_hat-alpha_hat_i_minus_1)**2, na.rm =T))][[1,1]],
                           i)
  colnames(est_diff_i) <- colnames(est_diff_alpha)
  est_diff_alpha<- rbind(est_diff_alpha, est_diff_i)
  formula <- paste0('y_i~ 1',
                    '| ',
                    ' author_id + inst_id_field^year '
  )
  test_brutal <- feols(as.formula(formula)
                       ,data = sample_df %>%
                         .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
  )
  gc()
  
  sample_df <- sample_df %>%
    .[, alpha_hat := fixef(test_brutal)$author_id[author_id]]
  gc()
  ggplot(unique(sample_df[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))
  
  sample_df <- sample_df %>%
    .[, ":="( sum_alpha_hat = sum(alpha_hat),
              n_colab = n_distinct(author_id)
    ),
    by = c('inst_id_field','year')] %>%
    .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
  ggplot(sample_df)+geom_density(aes(x=avg_alpha_i_bar))
  
  formula_qual <- paste0('y~ avg_alpha_i_bar',
                         '| ',
                         ' author_id + inst_id_field^year '
  )
  test_qual <- feols(as.formula(formula_qual)
                     ,data = sample_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
  )
  gc()
  est_gamma_i <-  cbind(test_qual$coeftable, i)
  colnames(est_gamma_i) <- colnames(est_gamma)
  est_gamma <-rbind(est_gamma,est_gamma_i)
  
}
  

cp <- ggplot(est_gamma)+
  geom_point(aes(x=as.factor(i), y = Estimate))+
  geom_line(aes (x=i, y = Estimate))+
  geom_errorbar(aes(x=i, ymin = Estimate -1.96*`Std. Error`,
ymax = Estimate + 1.96*`Std. Error`))+ylim(0.08, 0.18)+
  xlab('Number of iterations')+ylab('')+
  theme_minimal()
cp

save_plot("E:\\panel_fr_res\\productivity_results\\convergence_estimate.png",cp)


cp_aufe <- ggplot(est_diff_alpha[i>1])+
  geom_point(aes(x=as.factor(i), y = diff))+
  geom_line(aes(x=i-1, y= diff))+
  xlab('Number of iterations')+ylab('')+
  theme_minimal()
cp_aufe
save_plot("E:\\panel_fr_res\\productivity_results\\convergence_fe.png",cp)


sample_df <- sample_df %>%
  .[, ':='(alpha_hat_i_minus_1 = alpha_hat,
           alpha_hat = fixef(test_qual)$author_id[author_id],
           y_i = y- est_gamma[[5,1]]*avg_alpha_i_bar )]

ggplot(unique(sample_df[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))

formula <- paste0('y_final~ 1 + i(year, uni_pub, 2008)',
       '+ i(year, has_idex*uni_pub, 2008)',
       '+ i(year, has_idex*(1-uni_pub), 2008)',
       '+ i(year, prive, 2008)',
       '+ alpha_hat',
       '|',
       ' inst_id_field + year '
       ,'+ type^year '
       ,'+ ecole^year'
       ,'+ cnrs^year'
       ,'+ fused^year'
       ,'+ main_field^entry_year^year '
       ,'+ city^year'
)
test_brutal <- feols(as.formula(formula)
                     ,data = sample_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )] %>%
                       .[, y_final := log(citations)- est_gamma[[5,1]]*avg_alpha_i_bar ]
)
gc()

iplot(test_brutal,i.select = 1)
iplot(test_brutal,i.select = 2)
iplot(test_brutal,i.select = 3)
iplot(test_brutal,i.select = 4)

pdf("E:\\panel_fr_res\\productivity_results\\effect_uni_pub_citations.pdf")
iplot(test_brutal, main = 'Effect of being in a PU on log citations')
dev.off()
pdf("E:\\panel_fr_res\\productivity_results\\effect_idex_citations.pdf")
iplot(test_brutal,i.select = 2, main = 'Effect of being in an IDEX-receiving university on log citations')
dev.off()
pdf("E:\\panel_fr_res\\productivity_results\\effect_idex_uni_pub_citations.pdf")
iplot(test_brutal,i.select = 3, main = 'Effect of being in an IDEX-receiving PU on log citations')
dev.off()
pdf("E:\\panel_fr_res\\productivity_results\\effect_private_citations.pdf")
iplot(test_brutal,i.select = 4, main = 'Effect of being in a private institution on log citations')
dev.off()


summary(test_brutal)


#
#formula_agg <- paste0('c(publications_raw, avg_rank_source_raw*publications_raw, citations_raw, nr_source_top_5pct_raw)',
#                  '~ 1 + i(post, uni_pub, 0)| ',
#                  ' author_id + inst_id_field '
#                  ,'+ inst_type^year '
#                  ,'+ cnrs^year'
#                  ,'+ fused^year'
#                  ,'+ main_field^year '
#                  ,'+entry_cohort^year'
#)
#
formula_agg <- str_replace_all(formula, '\\(year','\\(post')
formula_agg <- str_replace_all(formula_agg, '\\d{4}\\)','0\\)')

agg_prod <- feols(as.formula(formula_agg)
                  ,data = sample_df %>%
                    .[,post := as.numeric(year >=2010)])
gc()
etable(agg_prod)
etable(agg_prod,drop = c('alpha_hat'),
       file = "E:\\panel_fr_res\\productivity_results\\agg_prod.tex", replace=TRUE)



fixef_brutal <- fixef(test_brutal#, fixef.iter =  5000
)
gc()


fixef_ds_inst_akm <-as.data.table(list(names(fixef_brutal$inst_id_field),fixef_brutal$inst_id_field))
colnames(fixef_ds_inst_akm) <- c('inst_id_field','fixef_inst_akm')
fixef_ds_inst_akm <- fixef_ds_inst_akm[, ":="(rank_inst_akm =frank(fixef_inst_akm))]
ggplot(fixef_ds_inst_akm)+geom_line(aes(x=rank_inst_akm,y=fixef_inst_akm))
ggplot(fixef_ds_inst_akm)+geom_density(aes(x=fixef_inst_akm))

fixef_ds_inst <- merge(fixef_ds_inst_akm[, ":="(rank_inst_akm =frank(fixef_inst_akm))],
                       unique(sample_df[, list(inst_id_field, name,n_obs_univ)]), by = 'inst_id_field')
test <- fixef_ds_inst[str_detect(inst_id_field, 'econ')][, 
                                                                           rank_inst_akm := rank_inst_akm/max(rank_inst_akm, na.rm = T)]

test[inst_id_field %in% c("I57995698econ", 'I2802331213econ',
                          "I4210092408econ",'I4210144888econ')]
unique(sample_df[, list(author_id,author_name, main_field, n_obs_au,alpha_hat)])[author_name=='Philippe Aghion']
unique(sample_df[, list(author_id,author_name, main_field, n_obs_au,alpha_hat)])[author_name=='Stanislas Dehaene']

test2 <- unique(sample_df[, list(author_id,author_name, main_field, n_obs_au,alpha_hat)])[str_detect(main_field, 'econ')][, ":="(rank_au_akm =frank(alpha_hat))][, 
                                                     rank_au_akm := rank_au_akm/max(rank_au_akm, na.rm = T)]
test2[author_name=='Philippe Aghion']


sample_df <- merge(sample_df, fixef_ds_inst_akm, by ='inst_id_field', all.x = T) 

rank_au <- unique(sample_df[, list(author_id, alpha_hat)]) %>%
  .[, rank_au_akm := frank(alpha_hat)]
sample_df <- merge(sample_df, rank_au[, list(author_id, rank_au_akm)], by ='author_id', all.x = T) 

sample_df <- sample_df %>%
  .[, rank_colab_akm:=frank(avg_alpha_i_bar) ]

# Save results ------------------------------------------------------------
fwrite(sample_df, "E:\\panel_fr_res\\test_with_fixed_effects.csv")
gc()
