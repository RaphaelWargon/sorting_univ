rm(list = ls())
gc()
library('arrow')
library('data.table')
library('fixest')
library('tidyverse')
library('binsreg')

inputpath <- "D:\\panel_fr_res\\panel_non_sphericised.parquet"
ds <- open_dataset(inputpath) %>%
  #filter(last_year-entry_year >= 5 | entry_year >=2015) %>%
  select(author_id,year,
         publications_raw,citations_raw, 
         publications, citations, entry_year,country, 
         inst_id, inst_type, display_name,
         parent_id, parent_name, parent_type, period_inst)
ds <- as.data.table(ds)
gc()


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
cross_fixed <- ds[, n_authors_sample := n_distinct(author_id), by = 'parent_id'][,
                                                                                 n_inst_id := n_distinct(parent_id), by = 'author_id'
]



ggplot(unique(cross_fixed[, list(inst_id, n_authors_sample, inst_type, country)]))+
  geom_density(aes(x = log(n_authors_sample), color = inst_type), alpha = 0.5)


# author-level desc stats -------------------------------------------------
collect_set <- function(col){
  return(str_c(sort(unique(col)), collapse = ','))
}
au_level <- ds[,
               .(lapply(.SD,
                        collect_set
               )
               ,
               univ_pub = max(univ_pub),
               entry_year = first(entry_year),
               publications = sum(publications),
               citations = sum(citations)
               )
               , by = c('author_id','year')
               , .SDcols = c('country','inst_id','display_name','inst_type',
                             'parent_type','parent_id','parent_name','fusion','n_inst_id')
               ]


# lab-level desc stats ----------------------------------------------------



test_transitions <- ds[country =='FR'][, 
 last_inst_type := lag(inst_type, order_by = year), by = 'author_id'][
inst_type != last_inst_type][
                                           ,.N, by = c('year','inst_type','last_inst_type')][
                                             inst_type %in% c('government','company','facility','education')][
                                               last_inst_type %in% c('government','company','facility','education')]

ggplot(test_transitions) +
  geom_line(aes(x= year, y = N, color = interaction(inst_type,last_inst_type)))



ggplot(ds[country =='FR'][, .(N=n_distinct(author_id)), by= c('year', 'inst_type')]) +
  geom_line(aes(x= year, y = N, color= inst_type))
