rm(list = ls())
gc()
library('arrow')
library('data.table')
library('fixest')
library('tidyverse')
library('binsreg')

inputpath <- "D:\\panel_fr_res\\panel_non_sphericised.parquet"
inputpath <- "C:\\Users\\rapha\\Desktop\\panel_non_sphericised.parquet"
ds <- open_dataset(inputpath) %>%
  #filter(last_year-entry_year >= 5 | entry_year >=2015) %>%
  select(author_id,year,
         publications_raw,citations_raw, 
         publications, citations, entry_year,country, 
         inst_id, inst_type, display_name, main_field,
         parent_id, parent_name, parent_type, period_inst, 
         art, biol,busi,chem,comp,econ,engi,envi,
         geog,geol,hist,mate,math,medi,phil,
         phys,poli,psyc,soci)
ds <- as.data.table(ds)
gc()

fields <- c("art","biol","busi","chem","comp","econ","engi","envi",
            "geog","geol","hist","mate","math","medi","phil",
            "phys","poli","psyc","soci")

recoding <- c('I118618916'="I899635006",
              "I36085230" = "I899635006",
              "I177483745" = "I899635006",
              "I4210103002"="I198244214",
              "I4210143836" = "I198244214",
              "I184646667"="I39804081",
              "I102197404"="I277688954",
              'I7171862'= "I2279609970",
              "I59807433"= "I2279609970",
              "I2800379142"= "I4210154111",
              "I3123023596"="I56067802",
              "I4210095130"="I208215962")
ds <- ds[, parent_id := ifelse(!parent_id %in% names(recoding), parent_id,
                               recoding[parent_id]
                               )]
ds <- unique(ds)
liste_institutions <- unique(ds[country == 'FR'][,
                    list(inst_id, inst_type, display_name, parent_id, parent_name, parent_type)])

list_fused <- c("I21491767","I198244214","I4210142324",
                "I899635006","I899635006","I39804081",
                "I201841394","I4210154111","I204730241","I15057530",
                "I277688954","I68947357","I2279609970","I19894307")

list_non_fr_non_uni_pub <- c('I3131573726','I4210086079',
                             "I24240610","I16465266",'I35298706',
                             "I4210134562","I185839726","I4210143169",
                             "I4210127465","I10342815",
                             "I57206974","I193291145","I4210163862","I124357947",
                             "I97565354")

universites_publiques <- liste_institutions[parent_type == 'education'] %>%
  .[str_detect(tolower(parent_name), 'universit') & !str_detect(tolower(parent_name), 'catholi')
  &  ! parent_id %in% list_non_fr_non_uni_pub ] %>%
  .[, list(parent_id, parent_type, parent_name)] %>%
  unique() %>% .[, fusion := as.numeric(parent_id %in% list_fused) ]


ds <- merge(ds,
            unique(universites_publiques[, univ_pub := 1][, list(univ_pub, parent_id, fusion)]),
            all.x = TRUE)
ds <- ds[, ':='(fusion = ifelse(is.na(fusion),0, fusion),
                univ_pub = ifelse(is.na(univ_pub), 0, univ_pub))]
gc()
cross_fixed <- ds[,
  n_inst_id_sample := n_distinct(inst_id), by = 'author_id'
][, ':='(n_authors_w_several_inst = sum(ifelse(n_inst_id_sample >1, 1, 0)),
         n_authors_sample = n_distinct(author_id) ), by = 'inst_id'][,
          n_by_field := n_distinct(author_id), by = fields][n_by_field >100]
gc()

nrow(cross_fixed[n_inst_id_sample ==1])

nrow(cross_fixed[n_authors_w_several_inst == 0])
nrow(cross_fixed[n_inst_id_sample == 1 & n_authors_w_several_inst > 1])
nrow(cross_fixed[n_inst_id_sample > 1 & n_authors_w_several_inst == 1])
nrow(cross_fixed[n_inst_id_sample > 1 & n_authors_w_several_inst > 1])


count_by_fields <- unique(cross_fixed[n_authors_w_several_inst > 1 
            & n_authors_sample > 10])[, list(author_id,main_field)][, .N, by = main_field]


# regressions -------------------------------------------------------------
formula <- paste0('log(citations_raw/publications_raw+1) ~ 1 | ',
                      ' author_id + inst_id +',
                        #' inst_type^year + parent_type^year + country^year + univ_pub^year +',
                    paste0(fields, collapse = '^entry_year^year +'), '^entry_year^year'
                  )
test_brutal <- feols(as.formula(formula) #+ fusion^year#+ inst_id^author_id
                     ,data =cross_fixed[n_authors_w_several_inst > 1 
                                       #  (n_authors_sample > 100 | country == 'FR')
                                        & country == 'FR'
                                        ] )
gc()

fixef_brutal <- fixef(test_brutal#, fixef.iter =  5000
                      )
plot(fixef_brutal)
gc()


  
fixef_ds_au <-as.data.table(list(names(fixef_brutal$author_id),fixef_brutal$author_id))
colnames(fixef_ds_au) <- c('author_id','fixef_au')
fixef_ds_au[, rank_au :=frank(fixef_au)]

ggplot(fixef_ds_au)+
  geom_density(aes(x=fixef_au))

ggplot(fixef_ds_au)+
  geom_line(aes(x=rank_au, y =fixef_au))

#fixef_ds_inst <-as.data.table(list(names(fixef_brutal$parent_id),fixef_brutal$parent_id))
#colnames(fixef_ds_inst) <- c('parent_id','fixef_in')
#fixef_ds_inst[, rank_inst :=frank(fixef_in)]

fixef_ds_inst <-as.data.table(list(names(fixef_brutal$inst_id),fixef_brutal$inst_id))
colnames(fixef_ds_inst) <- c('inst_id','fixef_in')
fixef_ds_inst[, rank_inst :=frank(fixef_in)]

ggplot(fixef_ds_inst)+
  geom_density(aes(x=fixef_in))
ggplot(fixef_ds_inst)+
  geom_line(aes(x=rank_inst, y =fixef_in))

gc()

#test <- -fixef_brutal$`country^year`

summary(test_brutal)
ds <- merge(ds, fixef_ds_au, by ='author_id') 
ds <- merge(ds, fixef_ds_inst, by ='inst_id') 

gc()

ds <- ds[, ':='(rank_au = (rank_au-min(rank_au))/(max(rank_au)-min(rank_au))), by = c('year')][,
         ":="(rank_inst = (rank_inst-min(rank_inst))/(max(rank_inst)- min(rank_inst)) ), by = c('country','inst_type','year')]
gc()

#ds <- ds[, ':='(fixef_au = NULL, fixef_in = NULL, rank_au = NULL, rank_inst =NULL)]
paste0(fields, collapse = '+')
test <- unique(ds[year == 2018 & country == 'FR'
                  & econ  == 1 & biol+busi+chem+comp+engi+envi+geog+geol+hist+mate+math+medi+phil+phys+poli+psyc+soci ==0][
  , .(sum_cit = sum(citations_raw)), by =c("inst_id", "inst_type", "rank_inst", "display_name",'parent_name')
])[order(inst_type, rank_inst)]
  #c('A5026928510','A5084984675','A5037232892',"A5074250972","A5015565298","A5090091703","A5087461705") piketty philippe blanchard tirole duflo zucman c.antonin

test_2 <- unique(ds[year == 2018 & country == 'FR'
  & str_detect(main_field, "econ")
  ][,
.(sum_cit = sum(citations)), by = c('author_id','rank_au','year',fields)][order(rank_au)])

ggplot(ds[!is.na(rank_au) & !is.na(rank_inst)][country == "FR"
          ][, .(cor = cor(rank_au,rank_inst)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


ggplot(unique(ds[entry_year >1949
  &main_field == "chem"][, list(author_id, fixef_au, rank_au, entry_year)])[,
    .(rank_au = mean(rank_au)), by = 'entry_year'])+
  geom_point(aes(x=entry_year, y=rank_au))



ggplot(ds[country == "FR" & parent_type == "education"][, .(cor = cor(fixef_au,fixef_in)), by = c('year','univ_pub')])+
  geom_point(aes(x=year, y=cor, color= as.factor(univ_pub)))

ggplot(ds[country == "FR"][, min_fixef_in := min(rank_inst), by =c('author_id','year')][,
             .(cor = cor(rank_au,min_fixef_in)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


ggplot(ds[country == "FR"][, min_fixef_au := min(rank_au), by =c('inst_id','year')][,
 .(cor = cor(min_fixef_au,rank_inst)), by = 'year'])+
  geom_point(aes(x=year, y=cor))


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
          ds[,post := ifelse(year <2009,0,1)][country == "FR" & inst_type %in% c('education','government','facility')][, 
                                        ":="(min_rank_au = min(rank_au, na.rm = T)
                                            ), by =c('inst_id', 'post')][, parent_type_year := paste0(univ_pub, "_",post)]
        , by =parent_type_year)



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
