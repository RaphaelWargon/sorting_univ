rm(list = ls())
gc()
library('pacman')

#install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')
#install.packages("DIDmultiplegt", force = TRUE)

library(fwildclusterboot)

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot', 'fwildclusterboot','DIDmultiplegt')
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}


inputpath <- "E:\\panel_fr_res\\panel_smoothed.parquet"


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
  select(author_id, author_name, #gender,
         year,
         entry_year,
         inst_id, name, type, field, subfield, domain, entry_country,
         publications_raw,citations_raw,  
         publications, citations,
         avg_rank_source_raw,nr_source_btm_50pct_raw,
         nr_source_mid_40pct_raw, nr_source_top_20pct_raw,nr_source_top_10pct_raw,nr_source_top_5pct_raw,
         avg_rank_source,nr_source_btm_50pct,
         nr_source_mid_40pct, nr_source_top_20pct,nr_source_top_10pct,nr_source_top_5pct,
         period_inst, uni_pub, cnrs,type, acces_rce,
         idex, first_y_inst_period, date_first_idex,
         fused_inst_id, fusion_date,
         main_topic, city, prive, public, ecole, 
         n_inst_y, first_y_inst_period, 
         # n_phd_students, in_supervisor_inst, 
         # in_referee_inst,in_jury_inst, thesis_year #, inst_set_this_year
  )
ds <- as.data.table(ds)

ds <- unique(ds)
#nrow(unique(ds[,list(author_id)])) #197840 w min_period 2 min_y_f 1/2
#  245495 w min_period 3 min_y_f 1/4

gc()
unique(ds[, list(author_id,entry_year)][])[, .N, by = 'entry_year'][order(entry_year)]

##
inst_to_exclude <- c('I4210159570')
cols_to_wins <- colnames(ds)[4:19]
sample_df <- ds[entry_year >=1965 
                & entry_year <=2003
                & year >= 1997
                & citations >0
                #& inst_type %in% c('facility','education')
                #& country == 'FR'
                & !(inst_id %in% inst_to_exclude) 
                & !(is.na(field))
                & !(inst_id %in% c('archive',"other"))
]%>%
  .[, ':='(n_inst_id_sample = n_distinct(inst_id),
           field = first(field, na_rm = TRUE),
           inst_id = ifelse(is.na(inst_id), 
                            lag(inst_id, order_by = year), inst_id)), by = 'author_id'] %>%
  .[, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
           n_authors_sample = n_distinct(author_id) ), by = c('inst_id','field')]%>%
  .[,n_by_field := n_distinct(author_id), by = 'field']%>%
  .[, n_y_in_sample := n_distinct(year), by = 'author_id']%>%
  .[n_authors_w_several_inst > 0 & n_y_in_sample >=2] %>%
  .[,max_field := dplyr::first(ifelse(n_authors_sample == max(n_authors_sample* as.numeric(!is.na(field)) ) & !is.na(field), field, NA), na_rm = T),
    by = "inst_id"] %>%
  .[,field_value := n_authors_sample/n_distinct(author_id),by = 'inst_id']%>%
  .[,max_field_value := max(field_value), by = 'inst_id'] %>%
  .[,field_recoded := ifelse( ((field_value <0.1| n_authors_sample<=5) ) | is.na(field), max_field, field)]%>%
  .[, field := field_recoded]%>%
  .[,n_authors_sample := n_distinct(author_id), by = c('inst_id','field')]%>%
  #.[n_authors_sample >= 5]%>%
  .[ , ':='(n_y_in_sample = n_distinct(year),
            n_obs_au = .N), by = c('author_id','field')]%>%
  # .[n_obs_au >=3]%>%
  .[,inst_id_field := paste0(inst_id, field)] %>%
  .[, entry_cohort := fifelse(entry_year <= 2000, floor(entry_year/5)*5, entry_year)]%>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]%>%
  .[, n_obs_univ := .N, by = 'inst_id']%>%
  .[, ":="(avg_rank_source_raw = ifelse(is.na(avg_rank_source_raw),0,avg_rank_source_raw),
           nr_source_top_10pct = ifelse(is.na(nr_source_top_10pct),0,nr_source_top_10pct),
           nr_source_top_5pct = ifelse(is.na(nr_source_top_5pct),0,nr_source_top_5pct),
           nr_source_top_10pct_raw = ifelse(is.na(nr_source_top_10pct_raw),0,nr_source_top_10pct_raw),
           nr_source_top_5pct_raw = ifelse(is.na(nr_source_top_5pct_raw),0,nr_source_top_5pct_raw)
  )] %>%
  .[, normalized_avg_rank_source_raw :=avg_rank_source_raw/max(avg_rank_source_raw), by = c('year','field') ]
#rm(ds)
gc()


nrow(unique(sample_df[, list(author_id)]))#146189 authors

sample_df <- unique(sample_df[, ':='(log_citations = log(citations),
                                     log_cit_w_p =log(citations/publications),
                                     log_log_cit_w_p = log(log(citations/publications)),
                                     log_cit_w_p_raw = log(citations_raw/publications_raw),
                                     log_log_cit_w_p_raw = log(log(citations_raw/publications_raw))
)])
#summary(sample_df)

# desc stats --------------------------------------------------------------


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
save_plot("D:\\panel_fr_res\\desc_stats\\avg_rankw_pub.png", p)


p <- ggplot(to_plot)+
  geom_line(aes(x=year, y= log(citations), color =interaction(uni_pub)))+
  scale_color_manual(values = c('black','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average citations')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)
p
save_plot("D:\\panel_fr_res\\desc_stats\\avg_cit.png", p)

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

formula_res <- paste0('log_citations~ 1 ',
                      '| '
                      ,' type^year '
                      ,'+ cnrs^year'
                      ,'+ city^year'
                      ,'+idex^year'
                      ,'+uni_pub^year'
                      ,'+acces_rce^year'
                      ,'+date_first_idex^year'
                      ,'+fusion_date^year'
                      ,'+ field^entry_cohort^year '
)
reg_to_residualize <- feols(as.formula(formula_res)
                            ,data = sample_df %>%
                              .[, ':='(acces_rce = ifelse(is.na(acces_rce), 0, acces_rce),
                                       date_first_idex = ifelse(is.na(date_first_idex), 0, date_first_idex),
                                       fusion_date = ifelse(is.na(fusion_date), 0, fusion_date))]%>%
                              .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )]
)
gc()

sample_df$y <- residuals(reg_to_residualize)

ggplot(sample_df)+geom_density(aes(x=y))

gc()


## Bootstrap iterations author-level -------------------------------------



set.seed(123)
n_au <- sample_df %>% .[, .(n= n_distinct(author_id)), by = 'field'] %>% .[n >100]
all_au <- merge(unique(sample_df[, list(author_id, field)]), n_au, by = 'field')
gc()
for(bootstrap_iteration in 1:1000){

print(sprintf('Bootstrap iteration n° %s', bootstrap_iteration))
list_au_btsp <- data.table('author_id' = '', 'field'= '', 'w'= 0)
for(field_ in unique(n_au$field)){
n <- as.integer(n_au[field == field_][,2])
#print(field_)
#print(n)
sampling_weights <- sample(
  x = c(, 1L),
  size = n,
  replace = TRUE
)

list_au <-  data.table(cbind((all_au[field==field_])$author_id,(all_au[field==field_])$field, sampling_weights))
colnames(list_au) <- colnames(list_au_btsp)
list_au_btsp <- rbind(list_au_btsp, list_au)
}
list_au_btsp <- list_au_btsp[author_id!='']

btstp_iteration_df <- merge(sample_df, list_au_btsp,by= c('author_id','field'), all.x = T) %>%
  .[, w := as.numeric(w)]
print(sprintf('%s observations', nrow(btstp_iteration_df)))

formula <- paste0('y~ 1',
                  '| ',
                  ' author_id + inst_id_field^year '
)
test_brutal <- feols(as.formula(formula)
                     ,data = btstp_iteration_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )],
                     weights = btstp_iteration_df$w
)
gc()

btstp_iteration_df <- btstp_iteration_df %>%
  .[, alpha_hat := fixef(test_brutal)$author_id[author_id]]
gc()
ggplot(unique(btstp_iteration_df[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))

btstp_iteration_df <- btstp_iteration_df %>%
  .[, ":="( sum_alpha_hat = sum(alpha_hat),
            n_colab = n_distinct(author_id)
  ),
  by = c('inst_id_field','year')] %>%
  .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
ggplot(btstp_iteration_df)+geom_density(aes(x=avg_alpha_i_bar))

formula_qual <- paste0('y~ avg_alpha_i_bar',
                       '| ',
                       ' author_id + inst_id_field^year '
)
test_qual <- feols(as.formula(formula_qual)
                   ,data = btstp_iteration_df %>%
                     .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )],
                   weights = btstp_iteration_df$w
)
gc()

est_gamma <- test_qual$coeftable
est_gamma$i <- 1
est_gamma$bootstrap_iteration <- bootstrap_iteration
est_diff_alpha <- data.table(NA,1,bootstrap_iteration)
names(est_diff_alpha) <- c('diff','i','boostrap_iteration')
for(i in 2:10){
  print(paste0('Process for i=', i))
  btstp_iteration_df <- btstp_iteration_df %>%
    .[, ':='(alpha_hat_i_minus_1 = alpha_hat,
             alpha_hat = fixef(test_qual)$author_id[author_id],
             y_i = y- est_gamma[[i-1,1]]*avg_alpha_i_bar )]
  est_diff_i <- data.table(unique(btstp_iteration_df[,list(author_id,alpha_hat_i_minus_1, alpha_hat)])[,.(mean((alpha_hat-alpha_hat_i_minus_1)**2, na.rm =T))][[1,1]],
                           i, bootstrap_iteration)
  colnames(est_diff_i) <- colnames(est_diff_alpha)
  est_diff_alpha<- rbind(est_diff_alpha, est_diff_i)
  formula <- paste0('y_i~ 1',
                    '| ',
                    ' author_id + inst_id_field^year '
  )
  test_brutal <- feols(as.formula(formula)
                       ,data = btstp_iteration_df %>%
                         .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )],
                       weights = btstp_iteration_df$w
  )
  gc()
  
  btstp_iteration_df <- btstp_iteration_df %>%
    .[, alpha_hat := fixef(test_brutal)$author_id[author_id]]
  gc()
  ggplot(unique(btstp_iteration_df[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))
  
  btstp_iteration_df <- btstp_iteration_df %>%
    .[, ":="( sum_alpha_hat = sum(alpha_hat),
              n_colab = n_distinct(author_id)
    ),
    by = c('inst_id_field','year')] %>%
    .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
  ggplot(btstp_iteration_df)+geom_density(aes(x=avg_alpha_i_bar))
  
  formula_qual <- paste0('y~ avg_alpha_i_bar',
                         '| ',
                         ' author_id + inst_id_field^year '
  )
  test_qual <- feols(as.formula(formula_qual)
                     ,data = btstp_iteration_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )],
                     weights = btstp_iteration_df$w
  )
  gc()
  est_gamma_i <-  cbind(test_qual$coeftable, i, bootstrap_iteration)
  colnames(est_gamma_i) <- colnames(est_gamma)
  est_gamma <-rbind(est_gamma,est_gamma_i)
  
}
if(bootstrap_iteration ==1){

all_est_gamma <- est_gamma
all_est_alpha<- est_diff_alpha

boot_fe_au<- btstp_iteration_df[,list(author_id, alpha_hat)]
boot_fe_au$bootstrap_iteration <- bootstrap_iteration
}
else{
  all_est_gamma <- rbind(all_est_gamma, est_gamma)
  all_est_alpha <- rbind(all_est_alpha, est_diff_alpha)
  
  boot_fe_au_i <- btstp_iteration_df[,list(author_id, alpha_hat)]
  boot_fe_au_i$bootstrap_iteration <- bootstrap_iteration
  boot_fe_au <- rbind(boot_fe_au,boot_fe_au_i)
}
}


pt_est <- as.numeric(mean((copy(all_est_gamma)%>%
                             .[i==10])$Estimate))
cp <- ggplot(copy(all_est_gamma)%>%
               .[i==10])+
  geom_density(aes(x= Estimate))+
  #ylim(0.08, 0.18)+
  xlab('Estimates')+ylab('')+
  geom_vline(aes(xintercept = pt_est  ))+
  theme_minimal()
cp


cp <- ggplot(all_est_gamma)+
  geom_point(aes(x=as.factor(i), y = Estimate))+
#ylim(0.08, 0.18)+
  xlab('Number of iterations')+ylab('')+
  theme_minimal()+
  geom_hline(aes(yintercept =pt_est), linetype = "dashed")
cp

save_plot("D:\\panel_fr_res\\productivity_results\\convergence_estimate.png",cp)


cp_aufe <- ggplot(all_est_alpha[i>1])+
  geom_point(aes(x=as.factor(i), y = diff))+
  #geom_line(aes(x=i-1, y= diff))+
  xlab('Number of iterations')+ylab('')+
  theme_minimal()
cp_aufe
save_plot("D:\\panel_fr_res\\productivity_results\\convergence_fe.png",cp_aufe)

boot_fe_au <- boot_fe_au %>%
  .[, diff := alpha_hat -lag(alpha_hat, order_by = bootstrap_iteration), by = 'author_id']


cp_aufe <- ggplot(copy(boot_fe_au)[bootstrap_iteration >1])+
  geom_histogram(aes(x=diff), binwidth = 1)+
  #geom_line(aes(x=i-1, y= diff))+
  xlab('diff between iterations')+ylab('')+
  theme_minimal()
cp_aufe

mean(abs(all_est_gamma$`t value`[1]) <abs(all_est_gamma$`t value`[2:100]))

all_est_gamma_test <- all_est_gamma[i==10]


t_test <-mean(abs( all_est_gamma_test$Estimate[1] /sd(all_est_gamma_test$Estimate))<abs( all_est_gamma_test$Estimate[2:100]/sd(all_est_gamma_test$Estimate)))


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
                  ,'+ field^entry_year^year '
                  ,'+ city^year'
)
test_brutal <- feols(as.formula(formula)
                     ,data = sample_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )] %>%
                       .[, y_final := log(citations)- est_gamma[[5,1]]*avg_alpha_i_bar ]%>%
                       .[entry_year <= 2003 & year > 2003]
)
gc()

iplot(test_brutal,i.select = 1)
iplot(test_brutal,i.select = 2)
iplot(test_brutal,i.select = 3)
iplot(test_brutal,i.select = 4)

pdf("D:\\panel_fr_res\\productivity_results\\effect_uni_pub_citations.pdf")
iplot(test_brutal, main = 'Effect of being in a PU on log citations')
dev.off()
pdf("D:\\panel_fr_res\\productivity_results\\effect_idex_citations.pdf")
iplot(test_brutal,i.select = 2, main = 'Effect of being in an IDEX-receiving institution on log citations')
dev.off()
pdf("D:\\panel_fr_res\\productivity_results\\effect_idex_uni_pub_citations.pdf")
iplot(test_brutal,i.select = 3, main = 'Effect of being in an IDEX-receiving PU on log citations')
dev.off()
pdf("D:\\panel_fr_res\\productivity_results\\effect_private_citations.pdf")
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
#                  ,'+ field^year '
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
       file = "D:\\panel_fr_res\\productivity_results\\agg_prod.tex", replace=TRUE)

formula <- paste0('y_final~ 1',
                  '+ entry_abroad*(',
                  '+ i(post, uni_pub,               0)',
                  '+ i(post, has_idex*uni_pub,      0)',
                  '+ i(post, has_idex*(1-uni_pub),  0)',
                  '+ i(post, prive,                 0)',
                  ')',
                  '+ alpha_hat',
                  '|',
                  ' inst_id_field + year '
                  ,'+ type^year '
                  ,'+ ecole^year'
                  ,'+ cnrs^year'
                  ,'+ fused^year'
                  ,'+ field^entry_year^year '
                  ,'+ city^year'
)


agg_prod <- feols(as.formula(formula)
                  ,data = sample_df %>%
                    .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )] %>%
                    .[, y_final := log(citations)- est_gamma[[5,1]]*avg_alpha_i_bar ]%>%
                    .[entry_year <= 2003 & year > 2003] %>%
                    .[, entry_abroad := as.numeric(!str_detect(entry_country, 'FR'))]%>%
                    .[, post := as.numeric(year >= 2007)]
)
gc()
etable(agg_prod,drop = c('alpha_hat'),
       file = "D:\\panel_fr_res\\productivity_results\\agg_prod_étranger.tex", replace=TRUE)


etable(agg_prod)


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
unique(sample_df[, list(author_id,author_name, field, n_obs_au,alpha_hat)])[author_name=='Philippe Aghion']
unique(sample_df[, list(author_id,author_name, field, n_obs_au,alpha_hat)])[author_name=='Stanislas Dehaene']

test2 <- unique(sample_df[, list(author_id,author_name, field, n_obs_au,alpha_hat)])[str_detect(field, 'econ')][, ":="(rank_au_akm =frank(alpha_hat))][, 
                                                                                                                                                       rank_au_akm := rank_au_akm/max(rank_au_akm, na.rm = T)]
test2[author_name=='Philippe Aghion']


sample_df <- merge(sample_df, fixef_ds_inst_akm, by ='inst_id_field', all.x = T) 

rank_au <- unique(sample_df[, list(author_id, alpha_hat)]) %>%
  .[, rank_au_akm := frank(alpha_hat)]
sample_df <- merge(sample_df, rank_au[, list(author_id, rank_au_akm)], by ='author_id', all.x = T) 

sample_df <- sample_df %>%
  .[, rank_colab_akm:=frank(avg_alpha_i_bar) ]

# Save results ------------------------------------------------------------
fwrite(sample_df, "D:\\panel_fr_res\\test_with_fixed_effects.csv")
gc()
