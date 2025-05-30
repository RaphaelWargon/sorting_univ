
fixef_ds_au_akm <-as.data.table(list(names(fixef_brutal$author_id),
                                     fixef_brutal$author_id))
colnames(fixef_ds_au_akm) <- c('author_id','fixef_au_akm')
fixef_ds_au_akm <-fixef_ds_au_akm[, ":="(rank_au_akm =frank(fixef_au_akm))]
fixef_ds_inst_akm <-as.data.table(list(names(fixef_brutal$inst_id_field),fixef_brutal$inst_id_field))
colnames(fixef_ds_inst_akm) <- c('inst_id_field','fixef_inst_akm')
fixef_ds_inst_akm <- fixef_ds_inst_akm[, ":="(rank_inst_akm =frank(fixef_inst_akm))]

fixef_ds_au <- merge(fixef_ds_au_akm[, ":="(rank_au_akm =frank(fixef_au_akm))],
                     unique(sample_df[, list(author_id,author_name, main_field, n_obs_au)]), by = 'author_id', how = 'left')
fixef_ds_inst <- merge(fixef_ds_inst_akm[, ":="(rank_inst_akm =frank(fixef_inst_akm))],
                       unique(sample_df[, list(inst_id_field, inst_name, country,n_obs_univ)]), by = 'inst_id_field')
test <- fixef_ds_inst[str_detect(inst_id_field, 'econ') & country == 'FR'][, 
rank_inst_akm := rank_inst_akm/max(rank_inst_akm, na.rm = T)]

test[inst_id_field %in% c("I57995698econ", 'I2802331213econ',
                          "I4210092408econ",'I4210144888econ')]
fixef_ds_au[author_name=='Philippe Aghion']
test2 <- fixef_ds_au[str_detect(main_field, 'econ')][, 
rank_au_akm := rank_au_akm/max(rank_au_akm, na.rm = T)]
test2[author_name=='Philippe Aghion']

ggplot(fixef_ds_au)+
  geom_density_2d(aes(x=log(n_obs_au), y = rank_au_akm))

ggplot(fixef_ds_inst)+
  geom_point(aes(x=log(n_obs_univ), y = rank_inst_akm))
test_inst <- sample_df[log_cit_w_p>-Inf & inst_id_field %in% c("I57995698econ", 'I2802331213econ',
                                     "I4210092408econ",'I4210144888econ')][,
n_inst := n_distinct(inst_id), by = c('author_id','year')][,
lapply(.SD, mean, na.rm = T), by = c('inst_name','year'), 
.SDcols = c('n_inst','publications','publications_raw',"avg_rank_source_raw",
            'citations','citations_raw','n_obs_au','n_obs_univ',
            'log_cit_w_p','log_cit_w_p_raw')]

ggplot(test_inst)+
  geom_line(aes(x=year,y=publications_raw*avg_rank_source_raw, color = inst_name))
 


ggplot(sample_df[entrant == 1 & log_cit_w_p>-Inf][,
              lapply(.SD, mean, na.rm = T), by = c('uni_pub','year'), 
              .SDcols = c('n_inst','publications','publications_raw',
                          'citations','citations_raw','n_obs_au','n_obs_univ',
                          'log_cit_w_p','log_cit_w_p_raw')])+
  geom_line(aes(x=year,y=n_obs_au, color = as.factor(uni_pub)))

ggplot(unique(sample_df[author_id == 'A5000051438'][, list(author_id, year, publications, citations, citations_raw,publications_raw)]))+
  geom_line(aes(x=year,y=publications_raw))



sample_df <- merge(sample_df, fixef_ds_au_akm, by ='author_id', all.x = T) 
sample_df <- merge(sample_df, fixef_ds_inst_akm, by ='inst_id_field', all.x = T) 

sample_df <- sample %>% .[
  , ':='(rank_au_akm_norm = rank_au_akm/max(rank_au_akm, na.rm = T),
         rank_inst_akm_norm = rank_inst_akm/max(rank_inst_akm, na.rm = T)
  ), by = 'main_field'
]  %>%
  .[, inst_id_set:=paste0(unique(list(inst_id)), collapse = ","), by = c('author_id','year') ]%>%
  .[, lag_inst_id_set := lag(inst_id_set, order_by = year), by = author_id] %>%
  .[, entrant := ifelse(!str_detect(lag_inst_id_set, inst_id) | is.na(lag_inst_id_set), 1,0)]
gc()

cutoffs <- as.vector(quantile(unique(sample_df$n_authors_sample_df), probs = c(0.005,0.995)))
to_reg <- sample_df%>%
  .[, ":="(lag_pub = lag(publications, order_by = year),
           lag_cit = lag(citations, order_by = year),
           lag_fe_inst = lag(rank_inst_akm, order_by = year)#,
          # lag_n_inst = lag(n_inst, order_by = year)
  ), by = 'author_id']%>%
  .[ 
    entrant ==1
    #& n_authors_sample >= 50 & n_authors_sample <= 600
    &entry_year <=2007
  #  & !(cnrs ==1 & uni_pub==1)
    #&inst_type %in% c('facility','education')
    & country == 'FR'
    ] %>%
  #.[n_authors_sample >= cutoffs[1] & n_authors_sample<=cutoffs[2]
  #  ] %>%
  .[, post := fifelse(year >=2008, 1, 0)]  %>%
  .[, uni_pub:= fifelse(inst_id %in% c('I57995698','I4210092408'), 1, uni_pub)]%>%
  .[, cnrs:= fifelse(inst_id %in% c('I57995698','I3018756631'), 1, cnrs)]
  #.[, in_supervisor_inst:= as.integer(in_supervisor_inst)]
  # ranking_data[n_authors_sample >= 10& n_authors_sample <=2000 & entrant == 0]




ggplot(to_reg[!is.na(rank_au_akm_norm) & !is.na(rank_inst_akm_norm)][, .(ranking = cor(rank_au_akm_norm, rank_inst_akm_norm)), by = c('inst_type','uni_pub','year')])+
  geom_line(aes(x=year,y=ranking, color = interaction(inst_type, uni_pub)))
ggplot(to_reg[!is.na(rank_au_akm_norm) & !is.na(rank_inst_akm_norm)][, .(ranking = cor(rank_au_akm_norm, rank_inst_akm_norm)), by = c('uni_pub','year')])+
  geom_line(aes(x=year,y=ranking, color = as.factor(uni_pub)))
ggplot(to_reg[!is.na(rank_au_akm_norm) & !is.na(rank_inst_akm_norm)][, .(ranking = cor(rank_au_akm_norm, rank_inst_akm_norm)), by = c('uni_pub','cnrs','year')])+
  geom_line(aes(x=year,y=ranking, color = interaction(uni_pub, cnrs)))

ggplot(to_reg[!is.na(rank_au_akm_norm) & !is.na(rank_inst_akm_norm)][, 
.(in_supervisor_inst = mean(in_supervisor_inst, na.rm=T)), by = c('uni_pub','year')])+
  geom_line(aes(x=year,y=in_supervisor_inst, color = as.factor(uni_pub)))

plot(sort(unique(sample_df$n_authors_sample)))
plot(sort(unique(to_reg$n_authors_sample)))



sub_formula = paste0("rank_au_akm_norm ~ i(year, uni_pub*rank_inst_akm_norm, 2007)"
                     ,"+i(year, uni_pub, 2007)"
                     , "+i(year, rank_inst_akm_norm, 2007)"
                     ,'+lag_pub + lag_cit + lag_fe_inst '
                     #,"+i(year, uni_pub*in_supervisor_inst, 2007)"
                     #,"+i(year, in_supervisor_inst, 2007)"
                     , "|"
)
fe = paste0("inst_id^main_field "
            , "+year^inst_type"
            , '+cnrs^year+fused^year'
            #, '+entry_cohort^main_field^year'
)
formula_ranking = paste0(sub_formula,fe
)
test_ranking <- feols(as.formula(formula_ranking),
                      data =   to_reg 
                   #  ,weights =  to_reg$n_authors_sample 
                      )
iplot(test_ranking, i.select =1, main = "uni_pub*rank_inst_akm_norm")
iplot(test_ranking,i.select = 2, main = "uni_pub")
iplot(test_ranking,i.select = 3, main = "rank_inst_akm_norm")

#plot(sort(test_ranking$residuals))
iplot(test_ranking,i.select = 4, main = "uni_pub*in_supervisor_inst")
iplot(test_ranking,i.select = 5, main ="in_supervisor_inst")

etable(test_ranking)
formula_agg = paste0(str_replace_all(str_replace_all(sub_formula, 'year', 'post'), "2007", "0")
, fe)

test_ranking <- feols(as.formula(formula_agg),
                       data =    to_reg
                  #    ,weights = to_reg$n_authors
                      )
etable(test_ranking)




# test w supervisor -------------------------------------------------------


sub_formula = paste0("c(in_supervisor_inst, in_referee_inst, in_jury_inst) ~ "
                     ,"+i(year, uni_pub, 2007)"
                     ,'+lag_pub + lag_cit + lag_fe_inst '
                     #,"+i(year, uni_pub*in_supervisor_inst, 2007)"
                     #,"+i(year, in_supervisor_inst, 2007)"
                     , "|"
)
formula_ranking = paste0(sub_formula,fe
)

test_ranking <- feols(as.formula(formula_ranking),
                      data =   to_reg 
                      #,weights =  to_reg$n_authors 
)
iplot(test_ranking, i.select =1)

etable(test_ranking)
formula_agg = paste0(str_replace_all(str_replace_all(sub_formula, 'year', 'post'), "2007", "0")
                     , fe)

test_ranking <- feols(as.formula(formula_agg),
                      data =    to_reg
                      #    ,weights = to_reg$n_authors
)
etable(test_ranking)


