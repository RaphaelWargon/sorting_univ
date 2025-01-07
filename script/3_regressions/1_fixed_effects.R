rm(list = ls())
gc()
library('arrow')
library('data.table')
library('fixest')
library('tidyverse')
library('binsreg')

inputpath <- "E:\\panel_fr_res\\panel_smoothed.parquet"
ds <- open_dataset(inputpath) 
ds$schema$names

ds <- open_dataset(inputpath) %>%
  filter(all_y_in_FR >= (last_year-entry_year +1)/4
         & last_year-entry_year >2
  ) %>%
  select(author_id, author_name, year, period_total,
         publications_raw,citations_raw,  
         publications, citations, entry_year,country, 
         inst_id, inst_name, inst_type, main_field,
         parent, period_inst, uni_pub, cnrs, fused, 
         n_inst_y#, inst_set_this_year
         )
ds <- as.data.table(ds)

#nrow(unique(ds[,list(author_id)])) #197840 w min_period 2 min_y_f 1/2
#  245495 w min_period 3 min_y_f 1/4

gc()
#fields <- c("agri","arts","bioc", "busi","chem", "comp",
#            "deci", "dent","eart", "econ","ener", "engi",
#            "envi", "heal","immu", "mate","math", "medi",
#            "neur", "nurs","phar", "phys","psyc", "soci","vete"    
#)
#
ds <- ds[,
         n_inst_id_sample := n_distinct(inst_id), by = 'author_id'
][, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
         n_authors_sample = n_distinct(author_id) ), by = c('inst_id','main_field')][,
      n_by_field := n_distinct(author_id), by = 'main_field'][,
      n_y_in_sample := n_distinct(year), by = 'author_id'][,
      max_field := dplyr::first(ifelse(n_authors_sample == max(n_authors_sample), main_field, NA), na_rm = T),by = "inst_id"][,
       field_value := n_authors_sample/n_distinct(author_id),by = 'inst_id'][,
        max_field_value := max(field_value), by = 'inst_id'][,
        main_field_recoded := ifelse(field_value <0.025 | is.na(main_field), max_field, main_field)]
gc()

unique(ds[inst_name == 'Paris School of Economics'][, list(inst_id, n_authors_sample, main_field,main_field_recoded,max_field, field_value)])
unique(ds[inst_name == 'Paris School of Economics' & is.na(main_field) ][, list(inst_id, author_id,author_name,main_field_recoded)])


unique(ds[str_detect(inst_name, 'Université Paris 1')|
            str_detect(inst_name, 'Panthéon-Sorbonne')][, list(inst_id, inst_name, n_authors_sample, main_field,max_field,main_field_recoded, field_value)])

counts_inst <- unique(ds[, list(inst_id, main_field, n_authors_sample)])[, .N, by = n_authors_sample]

counts_inst <- unique(ds[, list(inst_id, main_field)])[, .N, by = inst_id]
summary(counts_inst)
counts_inst <- unique(ds[, list(inst_id, main_field, n_authors_w_several_inst)])[, .N, by = n_authors_w_several_inst]
counts_au <- unique(ds[, list(author_id, main_field, n_y_in_sample)])[, .N, by = n_y_in_sample]

unique(ds[ n_y_in_sample>=5& main_field == "econ"][, author_name])

nrow(ds[n_inst_id_sample ==1])

nrow(ds[n_authors_w_several_inst == 0])
nrow(ds[n_inst_id_sample == 1 & n_authors_w_several_inst > 1])
nrow(ds[n_inst_id_sample > 1 & n_authors_w_several_inst == 1])
nrow(ds[n_inst_id_sample > 1 & n_authors_w_several_inst > 1])


#count_by_fields <- unique(ds[n_authors_w_several_inst > 1 
#            & n_authors_sample > 10])[, list(author_id,main_field)][, .N, by = main_field]


unique(ds[, list(author_id,entry_year)][])[, .N, by = 'entry_year'][order(entry_year)]



##
sample <- ds[n_authors_w_several_inst > 0
             & n_y_in_sample >=2
             #  (n_authors_sample > 100 | country == 'FR')
             & entry_year >=1965 
             & year >= 1997
             & citations >0]%>%
  .[,main_field_recoded := ifelse(max_field_value>=0.4 | is.na(main_field), max_field, main_field_recoded)]%>%
  .[, main_field := main_field_recoded]%>%
  .[,n_authors_sample := n_distinct(author_id), by = c('inst_id','main_field')]%>%
 # .[n_authors_sample >= 5]%>%
  .[ , ':='(n_y_in_sample = n_distinct(year),
          n_obs_au = .N), by = 'author_id']%>%
  .[n_obs_au >=3]%>%
  .[,inst_id_field := paste0(inst_id, main_field)] %>%
  .[, entry_cohort := fifelse(entry_year <= 2000, floor(entry_year/5)*5, entry_year)]%>%
  .[, n_obs_univ := .N, by = 'inst_id']
#rm(ds)
gc()

nrow(unique(sample[, list(author_id)]))#135494 authors

sample <- unique(sample[, ':='(log_cit_w_p =log(citations/publications),
                               log_log_cit_w_p = log(log(citations/publications)),
                               log_cit_w_p_raw = log(citations_raw/publications_raw),
                               log_log_cit_w_p_raw = log(log(citations_raw/publications_raw))
                        )])
#summary(sample)

to_plot <- sample[log_cit_w_p>-Inf][, .(log_cit_w_p = mean(log_cit_w_p, na.rm =T)), by= c('uni_pub','year','cnrs')]

ggplot(to_plot)+
  geom_point(aes(x=year, y= log_cit_w_p, color =interaction(uni_pub, cnrs)))

test <- sample[inst_id == "I4210144005"]
sample[log_cit_w_p == -Inf][, list(citations, publications)]
# regressions -------------------------------------------------------------
formula <- paste0('log_cit_w_p~ 1 | ',
                  ' author_id + inst_id_field +',
                  ' inst_type^year + country^year + as.factor(cnrs)^as.factor(uni_pub)^year + main_field^entry_cohort^year + fused^year'
                  #    paste0(fields, collapse = '^entry_cohort^year +'), '^entry_year^year'
)
test_brutal <- feols(as.formula(formula) #+ inst_id^author_id
                     ,data = sample)
gc()

fixef_brutal <- fixef(test_brutal#, fixef.iter =  5000
)
plot(fixef_brutal)
gc()

formula_logsup <- paste0('log_log_cit_w_p~ 1 | ',
                         ' author_id + inst_id_field +',
                         ' inst_type^year + country^year + cnrs^year+ uni_pub^year + main_field^entry_cohort^year+fused^year'
                         #paste0(fields, collapse = '^entry_cohort^year +'), '^entry_year^year'
)
test_logsup <- feols(as.formula(formula_logsup) #+ fusion^year#+ inst_id^author_id
                     ,data = sample)
gc()

fixef_logsup <- fixef(test_logsup#, fixef.iter =  5000
)


fixef_ds_au_akm <-as.data.table(list(names(fixef_brutal$author_id),
                                     fixef_brutal$author_id))
colnames(fixef_ds_au_akm) <- c('author_id','fixef_au_akm')
fixef_ds_au_logsup <-as.data.table(list(names(fixef_logsup$author_id),
                                        fixef_logsup$author_id))
colnames(fixef_ds_au_logsup) <- c('author_id','fixef_au_logsup')

fixef_ds_au <- merge(fixef_ds_au_akm, fixef_ds_au_logsup, by ='author_id', all.x = TRUE)
fixef_ds_au <- fixef_ds_au[, ":="(rank_au_akm =frank(fixef_au_akm),
                                  rank_au_logsup = frank(fixef_au_logsup))]
rm(fixef_ds_au_akm, fixef_ds_au_logsup)
gc()

ggplot(fixef_ds_au)+
  geom_density(aes(x=fixef_au_akm), color = 'firebrick')+
  geom_density(aes(x= fixef_au_logsup), color = 'seagreen')

ggplot(fixef_ds_au)+
  geom_line(aes(x=rank_au_akm, y =fixef_au_akm), color = 'firebrick')+
  geom_line(aes(x=rank_au_logsup, y =fixef_au_logsup), color = 'seagreen')

#fixef_ds_inst <-as.data.table(list(names(fixef_brutal$parent_id),fixef_brutal$parent_id))
#colnames(fixef_ds_inst) <- c('parent_id','fixef_in')
#fixef_ds_inst[, rank_inst :=frank(fixef_in)]

fixef_ds_inst_akm <-as.data.table(list(names(fixef_brutal$inst_id_field),fixef_brutal$inst_id_field))
colnames(fixef_ds_inst_akm) <- c('inst_id_field','fixef_inst_akm')

fixef_ds_inst_logsup <-as.data.table(list(names(fixef_logsup$inst_id_field),fixef_logsup$inst_id_field))
colnames(fixef_ds_inst_logsup) <- c('inst_id_field','fixef_inst_logsup')

fixef_ds_inst <- merge(fixef_ds_inst_akm, fixef_ds_inst_logsup, by ='inst_id_field', all.x = TRUE)
fixef_ds_inst <- fixef_ds_inst[, ":="(rank_inst_akm =frank(fixef_inst_akm),
                                      rank_inst_logsup = frank(fixef_inst_logsup))]
rm(fixef_ds_inst_akm, fixef_ds_inst_logsup)
gc()


ggplot(fixef_ds_inst)+
  geom_density(aes(x=fixef_inst_akm), color = 'firebrick')+
  geom_density(aes(x= fixef_inst_logsup), color = 'seagreen')

ggplot(fixef_ds_inst)+
  geom_line(aes(x=rank_inst_akm, y =fixef_inst_akm), color = 'firebrick')+
  geom_line(aes(x=rank_inst_logsup, y =fixef_inst_logsup), color = 'seagreen')


gc()

#test <- -fixef_brutal$`country^year`

summary(test_brutal)

sample <- merge(sample, fixef_ds_au, by ='author_id', all.x = T) 
sample <- merge(sample, fixef_ds_inst, by ='inst_id_field', all.x = T) 

gc()

test <- unique(sample[main_field == "econ" & country == "FR"][, 
 rank_inst_akm := rank_inst_akm/max(rank_inst_akm, na.rm = T)][, 
 .(cit = sum(citations_raw)/n_distinct(author_id)), by = 
   list(inst_id,inst_name, main_field,rank_inst_akm,fixef_inst_akm)])

ggplot(test)+
  geom_point(aes(x=rank_inst_akm,y=cit))+geom_smooth(aes(x=rank_inst_akm,y=cit))

test2 <- unique(sample[str_detect(main_field, "econ") & country == "FR"][,
n_inst := .N, by = c('author_id','year')][, 
rank_au_akm := rank_au_akm/max(rank_au_akm, na.rm = T)][, 
.(cit = sum(citations_raw),
  cit_w = sum(citations_raw/n_inst)/sum(publications_raw/n_inst)), by = 
list(author_id, author_name, main_field,rank_au_akm,fixef_au_akm)])
ggplot(test2)+
  geom_point(aes(x=rank_au_akm,y=cit_w))+geom_smooth(aes(x=rank_au_akm,y=cit_w))


test2 <- sample[inst_id == "I57995698"| inst_id == 'I2802331213'][,
  n_inst := n_distinct(inst_id), by = c('author_id','year')][,
lapply(.SD, mean, na.rm = T), by = c('inst_name','year'), .SDcols = c('fixef_au_akm','rank_au_akm','n_inst',
                                                                      'publications','publications_raw','citations','citations_raw',
                                                                      'n_obs_au','n_obs_univ')]


ggplot(test2)+
  geom_line(aes(x=year,y=citations_raw, color = inst_name))

fixef_ds_au <- merge(fixef_ds_au, unique(sample[, list(author_id, n_obs_au)]))

ggplot(fixef_ds_au)+
  geom_smooth(aes(x= log(n_obs_au), y = rank_au_akm), color = 'firebrick')+
  geom_smooth(aes(x= log(n_obs_au), y = rank_au_logsup), color = 'seagreen')


ggplot(unique(sample[, .(author_id, rank_au_akm,rank_au_logsup, n_obs_au)]))+
  geom_point(aes(x= log(n_obs_au), y = rank_au_akm), color = 'firebrick')+
  geom_point(aes(x= log(n_obs_au), y = rank_au_logsup), color = 'seagreen')

# Save results ------------------------------------------------------------
fwrite(sample, "E:\\panel_fr_res\\test_with_fixed_effects.csv")
gc()
