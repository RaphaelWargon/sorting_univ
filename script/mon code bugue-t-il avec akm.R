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
  filter(all_y_in_FR >= (last_year-entry_year +1)/2
         & last_year-entry_year >1
         ) %>%
  select(author_id, author_name, year, period_total,
         publications_raw,citations_raw,  
         publications, citations, entry_year,country, 
         inst_id, inst_name, inst_type, main_field,
         parent, period_inst, uni_pub, cnrs, fused, 
         #agri,arts,bioc, busi,chem, comp,
         #deci, dent,eart, econ,ener, engi,
         #envi, heal,immu, mate,math, medi,
         #neur, nurs,phar, phys,psyc, soci,vete, 
         n_inst_y   )
ds <- as.data.table(ds)

#nrow(unique(ds[,list(author_id)])) 197840

gc()
fields <- c("agri","arts","bioc", "busi","chem", "comp",
            "deci", "dent","eart", "econ","ener", "engi",
            "envi", "heal","immu", "mate","math", "medi",
            "neur", "nurs","phar", "phys","psyc", "soci","vete"    
)

ds <- ds[,
  n_inst_id_sample := n_distinct(inst_id), by = 'author_id'
][, ':='(n_authors_w_several_inst = sum(ifelse(n_inst_id_sample >1, 1, 0)),
         n_authors_sample = n_distinct(author_id) ), by = c('inst_id','main_field')][,
          n_by_field := n_distinct(author_id), by = 'main_field'][n_by_field >1000][,
    n_y_in_sample := n_distinct(year), by = 'author_id'
          ]
gc()

counts_inst <- unique(ds[, list(inst_id, main_field, n_authors_sample)])[, .N, by = n_authors_sample]
counts_inst <- unique(ds[, list(inst_id, main_field, n_authors_w_several_inst)])[, .N, by = n_authors_w_several_inst]
counts_au <- unique(ds[, list(author_id, main_field, n_inst_id_sample)])[, .N, by = n_inst_id_sample]

nrow(ds[n_inst_id_sample ==1])

nrow(ds[n_authors_w_several_inst == 0])
nrow(ds[n_inst_id_sample == 1 & n_authors_w_several_inst > 1])
nrow(ds[n_inst_id_sample > 1 & n_authors_w_several_inst == 1])
nrow(ds[n_inst_id_sample > 1 & n_authors_w_several_inst > 1])


#count_by_fields <- unique(ds[n_authors_w_several_inst > 1 
#            & n_authors_sample > 10])[, list(author_id,main_field)][, .N, by = main_field]


unique(ds[, list(author_id,entry_year)][])[, .N, by = 'entry_year'][order(entry_year)]

ds <- ds[, entry_cohort := fifelse(entry_year <= 2000, floor(entry_year/5)*5, entry_year)]

unique(ds[, list(author_id,entry_cohort)][])[, .N, by = 'entry_cohort'][order(entry_cohort)]


##
sample <- ds[n_authors_w_several_inst > 1
             & n_y_in_sample >2
             #  (n_authors_sample > 100 | country == 'FR')
             & entry_cohort >=1965 
             & year >= 1997
               ]
# regressions -------------------------------------------------------------
formula <- paste0('log(citations/publications)~ 1 | ',
                      ' author_id + inst_id +',
                        ' inst_type^year + country^year + cnrs^year+ uni_pub^year + main_field^year'
                #    paste0(fields, collapse = '^entry_cohort^year +'), '^entry_year^year'
                  )
test_brutal <- feols(as.formula(formula) #+ fusion^year#+ inst_id^author_id
                     ,data = sample)
gc()

fixef_brutal <- fixef(test_brutal#, fixef.iter =  5000
                      )
plot(fixef_brutal)
gc()

formula_logsup <- paste0('log(log(citations/publications)+1)~ 1 | ',
                  ' author_id + inst_id +',
                  ' inst_type^year + country^year + cnrs^year+ uni_pub^year + main_field^year'
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

fixef_ds_inst_akm <-as.data.table(list(names(fixef_brutal$inst_id),fixef_brutal$inst_id))
colnames(fixef_ds_inst_akm) <- c('inst_id','fixef_inst_akm')

fixef_ds_inst_logsup <-as.data.table(list(names(fixef_logsup$inst_id),fixef_logsup$inst_id))
colnames(fixef_ds_inst_logsup) <- c('inst_id','fixef_inst_logsup')

fixef_ds_inst <- merge(fixef_ds_inst_akm, fixef_ds_inst_logsup, by ='inst_id', all.x = TRUE)
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
ds <- merge(ds, fixef_ds_au, by ='author_id', all.x = T) 
ds <- merge(ds, fixef_ds_inst, by ='inst_id', all.x = T) 

gc()


# Save results ------------------------------------------------------------
fwrite(ds, "E:\\panel_fr_res\\test_with_fixed_effects.csv")
gc()
ds <- fread( "E:\\panel_fr_res\\test_with_fixed_effects.csv")

test <- unique(ds[ country == 'FR'
                 & main_field=="econ"][
  , .(sum_cit = sum(citations_raw)), by =c("inst_id", "inst_type", "rank_inst_akm",
                                           "rank_inst_logsup", "inst_name",'parent')
])[order(inst_type, rank_inst_akm)][, 
        ':='(rank_inst_akm = ((rank_inst_akm - min(rank_inst_akm))/(max(rank_inst_akm)-min(rank_inst_akm))),
             rank_inst_logsup = ((rank_inst_logsup - min(rank_inst_logsup))/(max(rank_inst_logsup)-min(rank_inst_logsup))))]
# I57995698 pse


  #c('A5026928510','A5084984675','A5037232892',"A5074250972","A5015565298","A5090091703","A5087461705", #"A5074190434") piketty philippe blanchard tirole duflo zucman c.antonin k. schubert
# A5110429533 new for JT
test_2 <- unique(ds[year >=2000#& country == 'FR'
  & (str_detect(main_field, "econ") |str_detect(main_field, "soci") )
  #& main_field == "econ"
  ][,
.(sum_cit = sum(citations_raw)), by = c('author_id','main_field', 'rank_au_akm','author_name')][
  order(rank_au_akm)])[, rank_au := ((rank_au_akm - min(rank_au_akm, na.rm = T))/(max(rank_au_akm, na.rm= T)-min(rank_au_akm,na.rm = T)))]

test_famous <- test_2[author_id %in% c('A5026928510','A5084984675','A5037232892',"A5074250972","A5015565298","A5090091703","A5087461705")]

ds[author_id == 'A5074190434' ]


# validation ? -----------------------------------------------------------

validate_spec = ds[year >=2000 & year<=2006 
                          & country == "FR"& uni_pub == 1
][n_y_in_sample >10][, ':='(rank_au_akm = rank_au_akm/max(rank_au_akm, na.rm = T),
         rank_inst = rank_inst_akm/max(rank_inst_akm, na.rm = T) ), by = 'main_field'
][, .(min_rank_au_akm = min(rank_au_akm, na.rm = T),
       max_rank_au_akm = max(rank_au_akm, na.rm = T),
       sd_rank_au_akm = sd(rank_au_akm, na.rm = T),
       mean_rank_au_akm = mean(rank_au_akm, na.rm =T),
       n_authors = n_distinct(author_id),
       has_rank_90th = max(fifelse(rank_au_akm >=0.9,1,0), na.rm = T),
       has_rank_50th = max(fifelse(rank_au_akm >=0.5,1,0), na.rm = T)), by =c('inst_id','main_field','rank_inst')][
         n_authors >10]

validate_spec[min_rank_au_akm>0.7 & rank_inst <0.3]

ggplot(validate_spec[min_rank_au_akm <Inf]) +
  geom_jitter(aes(x = rank_inst, y = min_rank_au_akm))
ggplot(validate_spec[max_rank_au_akm >-Inf]) +
  geom_jitter(aes(x = rank_inst, y = mean_rank_au_akm))
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


# ranking regressions -----------------------------------------------------

ranking_data <- ds[year >=2000 & year<=2020 & country == "FR" 
                                                    ][
                      , ':='(rank_au_akm = rank_au_akm/max(rank_au_akm, na.rm = T),
                             rank_au_logsup = rank_au_logsup/max(rank_au_logsup, na.rm =T),
                             rank_inst_akm = rank_inst_akm/max(rank_inst_akm, na.rm=T),
                             rank_inst_logsup = rank_inst_logsup/max(rank_inst_logsup, na.rm = T)), by = 'main_field'
                    ][, 
                   ":="(min_rank_au_akm = min(rank_au_akm, na.rm = T),
                        max_rank_au_akm = max(rank_au_akm, na.rm = T),
                         sd_rank_au_akm =   sd(rank_au_akm, na.rm = T),
                       mean_rank_au_akm = mean(rank_au_akm, na.rm =T),
                      has_rank_90th_akm = max(fifelse(rank_au_akm >=0.9,1,0), na.rm = T),
                      has_rank_50th_akm = max(fifelse(rank_au_akm >=0.5,1,0), na.rm = T),
                   min_rank_au_logsup =  min(rank_au_logsup, na.rm = T),
                   max_rank_au_logsup =  max(rank_au_logsup, na.rm = T),
                    sd_rank_au_logsup =   sd(rank_au_logsup, na.rm = T),
                  mean_rank_au_logsup = mean(rank_au_logsup, na.rm =T),
                 has_rank_90th_logsup = max(fifelse(rank_au_logsup >=0.9,1,0), na.rm = T),
                 has_rank_50th_logsup = max(fifelse(rank_au_logsup >=0.5,1,0), na.rm = T),
                  n_authors = n_distinct(author_id))
, by =c('inst_id','year','main_field', 'inst_name','inst_type','uni_pub','cnrs','rank_inst_akm','rank_inst_logsup')][, post := fifelse(year >=2009,1,0)]
#to_plot <- ranking_data %>% .[, min_rank_au := mean(min_rank_au), by = c('year','univ_pub')]
#ggplot(to_plot)+
#  geom_line(aes(x = year, y = min_rank_au, color = as.factor(univ_pub)))


formula_ranking = paste0("min_rank_au_akm ~ i(year, uni_pub*rank_inst_akm, 2007) + rank_inst_akm*as.factor(year)|"
          , "inst_id^main_field+main_field^year+inst_type^year")

test_ranking <- feols(as.formula(formula_ranking),
                      ranking_data, weights = ranking_data$n_authors)
iplot(test_ranking)
etable(test_ranking)
test_ranking <- feols(as.formula(str_replace(str_replace(formula_ranking, 'year', 'post'), "2007", "0")),
                      ranking_data, weights = ranking_data$n_authors)
etable(test_ranking, keep =c( 'univ_pub'))


ggplot(ds[!is.na(rank_au) & !is.na(rank_inst)][country == "FR"
          ][, .(cor = cor(rank_au,rank_inst)), by = 'year'][year >=1950])+
  geom_point(aes(x=year, y=cor))


ggplot(unique(ds[entry_year >1949
  #&main_field == "econ"
  ][, list(author_id, fixef_au, rank_au, entry_year,main_field)])[,
    .(rank_au = mean(rank_au)), by = c('entry_year','main_field')])+
  geom_point(aes(x=entry_year, y=rank_au, color = main_field))+
  geom_smooth((aes(x=entry_year, y=rank_au, color = main_field)))



ggplot(ds[country == "FR" & parent_type == "education"][, .(cor = cor(fixef_au,fixef_in)), by = c('year','univ_pub')])+
  geom_point(aes(x=year, y=cor, color= as.factor(univ_pub)))

ggplot(ds[country == "FR"][, min_fixef_in := min(rank_inst), by =c('author_id','year')][,
             .(cor = cor(rank_au,min_fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


ggplot(ds[country == "FR" & year <=2020 & year >=2000][, min_fixef_au := min(rank_au), by =c('inst_id','year')][,
 .(cor = cor(min_fixef_au,rank_inst)), by = c('year','univ_pub','main_field')])+
  geom_point(aes(x=year, y=cor, color= univ_pub))


ggplot(ds[country == "DE"][, min_fixef_in := min(rank_inst), by =c('author_id','year')][,
                                                                                        .(cor = cor(rank_au,min_fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


ggplot(ds[country == "FR" & year == 2005][, min_fixef_in := min(fixef_in), by =c('author_id','year')])+
  geom_jitter(aes(x=fixef_au, y=min_fixef_in))

ggplot(ds[country == "FR" & year %in% c(2002, 2007, 2012, 2017)][, min_fixef_in := min(fixef_in), by =c('author_id','year')])+
  geom_point(aes(x=fixef_au, y=min_fixef_in, color = as.factor(year)))+
  geom_smooth(aes(x=fixef_au, y=min_fixef_in, color = as.factor(year)),method = lm)

gc()


ggplot(ds[country == "FR" & year %in% c(2002, 2007, 2012, 2017)][, min_rank_au := min(rank_au), by =c('inst_id','year')])+
  geom_point(aes(x=min_rank_au, y=rank_inst, color = as.factor(year)))+
  geom_smooth(aes(x=min_rank_au, y=rank_inst, color = as.factor(year)),method = lm)

gc()


ggplot(ds[country == "FR" & str_detect(parent_name, 'niversit') 
          & year %in% c(2007, 2012)][, min_fixef_in := min(fixef_in), by =c('author_id','year')])+
  geom_point(aes(x=fixef_au, y=min_fixef_in, color = interaction(year, type)))+
  geom_smooth(aes(x=fixef_au, y=min_fixef_in, color = interaction(year, type)),method = lm)

gc()


ggplot(ds[country == "FR" & parent_type %in% c('education','government','facility','company')
          & year %in% c(2007, 2012)][, 
              ":="(min_rank_au = min(rank_au, na.rm = T),
                   n_authors = n_distinct(author_id)), by =c('parent_id','year')][
                     n_authors >5
                   ])+
  geom_point(aes(y=min_rank_au, x=rank_inst, color = interaction(year, parent_type)))+
  geom_smooth(aes(y=min_rank_au, x=rank_inst, color = interaction(year,parent_type)),method = lm)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0,1)+xlim(0,1)


binsreg(y= min_rank_au, x= rank_inst, data =
          ds[year <2009][country == "FR" & parent_type %in% c('government','education','facility','company')][, ":="(min_rank_au = min(rank_au, na.rm = T)), by =c('inst_id')]
, by = parent_type)


binsreg(y= min_rank_au, x= rank_inst, data =
          ds[year <2009][country == "FR" & inst_type %in% c('government','education','facility','company')][, 
":="(min_rank_au = min(rank_au, na.rm = T),
     mean_rank_au = mean(rank_au, na.rm =T)), by =c('inst_id')][,
     mean_rank_au := (mean_rank_au-min(mean_rank_au))/(max(mean_rank_au)-min(mean_rank_au))]
        , by = inst_type)

binsreg(y= min_rank_au, x= rank_inst, data =
          ds[year >2009][country == "FR" & inst_type %in% c('gouvernment','education','facility')][, 
        ":="(min_rank_au = min(rank_au, na.rm = T)), by =c('inst_id')][,
        univ_pub_type := paste0(univ_pub, ',', inst_type)]
        , by = univ_pub_type)


binsreg(y= min_rank_au, x= rank_inst, data =
          ds[year <2009][country == "FR" & inst_type %in% c('gouvernment','education','facility')][, 
           ":="(min_rank_au = min(rank_au, na.rm = TRUE)), by =c('inst_id')][,
           univ_pub_type := paste0(univ_pub, ',', inst_type)]
        , by = univ_pub_type)

binsreg(y= min_rank_au, x= rank_inst, data =
          ds[year >2000][,post := ifelse(year <2009,0,1)][country == "FR" & inst_type %in% c('education','government','facility')][, 
                                        ":="(min_rank_au = min(rank_au, na.rm = T)
                                            ), by =c('inst_id', 'post')][, parent_type_year := paste0(univ_pub, "_",post)]
        , by =parent_type_year,
        polyreg =1 )



binsreg(y= rank_au, x= rank_inst, data =
          ds)


binsreg(y= rank_au, x= rank_inst, data =
          unique(ds[, list(rank_au, rank_inst, author_id, inst_id, univ_pub, inst_type, rank_au)])[,
          univ_pub_type := paste0(univ_pub, ',', inst_type)],
        by = univ_pub_type)



binsreg(y= rank_au, x= rank_inst, data =
          unique(ds[,post:=ifelse(year>=2009,1,0)]
                 [, list(rank_au, rank_inst, post, author_id, inst_id, univ_pub, inst_type, rank_au)])[,
        univ_pub_type_post := paste0(univ_pub, '_', inst_type, '_post_', post)][
          inst_type %in% c('facility','government','company','education')
        ],
        by = univ_pub_type_post)


binsreg(y= min_rank_inst, x= rank_au, data =
          ds[, post := ifelse(year < 2009, 0, 1)][country == "FR" & parent_type %in% c('education','government','facility','company')
             ][, 
                            ":="(min_rank_inst = min(rank_inst, na.rm = T)
                                ), by =c('author_id','post','univ_pub')][
                            ], by = post
)
ggplot(ds[country == "FR" & type %in% c('education','government','facility','company')
          & year %in% c(2007, 2012)][, min_rank_inst := min(rank_inst), by =c('rank_au','year')])+
  geom_smooth(aes(x=rank_au, y=min_rank_inst, color = interaction(year,type)),method = lm)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0,1)+xlim(0,1)




ggplot(ds[country == "FR" & type %in% c('education','government','facility','company')
          & year %in% c(2007, 2012)][, min_rank_inst := min(rank_inst), by =c('rank_au','year')])+
  geom_smooth(aes(x=rank_au, y=min_rank_inst, color = interaction(year,type)),method = lm)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0,1)+xlim(0,1)


ggplot(ds[country == "FR" & parent_type %in% c('education','government','facility','company')
          & year %in% c(2007, 2012)][, 
                                     ":="(min_rank_au = min(rank_au, na.rm = T),
                                          n_authors = n_distinct(author_id)), by =c('parent_id','year')][
                                            n_authors >5
                                          ])+
  geom_point(aes(y=rank_au, x=rank_inst, color = interaction(year, parent_type)))+
  geom_smooth(aes(y=rank_au, x=rank_inst, color = interaction(year,parent_type)),method = lm)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0,1)+xlim(0,1)



ggplot(ds[, max_fixef_in := max(fixef_in), by =c('author_id','year')][,
                                                                      .(cor = cor(fixef_au,max_fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


ggplot(ds[, min_fixef_au := min(fixef_au), by =c('inst_id','year')][,
   .(cor = cor(min_fixef_au,fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))

ggplot(ds[, max_fixef_au := max(fixef_au), by =c('inst_id','year')][,
                                                                    .(cor = cor(max_fixef_au,fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


ggplot(ds[, med_fixef_au := median(fixef_au), by =c('inst_id','year')][,
                                                                    .(cor = cor(med_fixef_au,fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))
gc()

ggplot(ds[, med_fixef_au := mean(fixef_au), by =c('inst_id','year')][,
                                                                       .(cor = cor(med_fixef_au,fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))

ggplot(ds[year == 2005][, .(fixef_au_min = min(fixef_au)), by = 'fixef_in'])+
  geom_jitter(aes(x=fixef_in, y=fixef_au_min))

ggplot(ds[year == 2005][, .(fixef_inst_min = min(fixef_in)), by = 'fixef_au'])+
  geom_jitter(aes(x=fixef_au, y=fixef_inst_min))


ggplot(ds[year == entry_year][, .(fixef_inst_min = min(fixef_in)), by = c('fixef_au','entry_year')])+
  geom_jitter(aes(x=fixef_au, y=fixef_inst_min, color= entry_year))
gc()
