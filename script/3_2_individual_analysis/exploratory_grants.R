units_au_grants <- unique(sample_df_reg%>%
                            .[, ':='(first_year_dgds = ifelse(first_year_dgds %in% 2005:2026, first_year_dgds, 0),
                                     first_year_ods = ifelse(first_year_ods %in% 2005:2026, first_year_ods, 0)
                                     )]%>%
                            .[, ':='(ever_dgds = as.numeric(first_year_dgds != 0),
                                     ever_ods = as.numeric(first_year_ods != 0),
                                     ever_erc = as.numeric(first_year_erc != 0)
                                     )]%>%
                            .[, .(  publications_pre = mean(publications* ifelse( year <= 2007, 1, NA ), na.rm = T),
                                       publications_post = mean(publications * ifelse( year >= 2016, 1, NA ) , na.rm = T ),
                                       citations_pre = mean(citations* ifelse( year <= 2007, 1, NA ) , na.rm = T),
                                       citations_post =mean(citations * ifelse( year >= 2016, 1, NA ) , na.rm = T),
                                       entry_year = first(entry_year),
                                       ever_dgds = max(ever_dgds), ever_ods = max(ever_ods), ever_erc = max(ever_erc)
                                       
), by=  c('author_id', 'first_year_dgds','first_year_ods', 'first_year_erc')]) 

table(unique(units_au_grants[, list(author_id, ever_dgds)])$ever_dgds)
table(unique(units_au_grants[, list(author_id, ever_ods)])$ever_ods)
table(unique(units_au_grants[, list(author_id, ever_erc)])$ever_erc)
table(unique(units_au_grants[, list(author_id, first_year_erc)])$first_year_erc)

test <- unique(units_au_grants[, list(author_id, ever_erc)][ever_erc==1])

summary(units_au_grants)




ever_labex <- sample_df_reg %>%
  .[, ever_labex := max(as.numeric(str_detect(grant_id, "LABX") )), by = 'author_id'] %>%
  #.[ever_labex ==1] %>%
  .[, first_year_labex := min(ifelse(str_detect(grant_id, "LABX"), year, NA ), na.rm = T ), 
    by = 'author_id']
gc()
ever_labex_facilities <- unique(ever_labex %>%
                                  .[ever_labex == 1]%>%
                                  .[year == first_year_labex & type %in% c("facility",'government')]%>%
                                  .[, .(first_y_labex_facility = min(year)), by= c('inst_id', 'domain', 'name','type')])
table(ever_labex_facilities$first_y_labex_facility)
gc()
ever_labex <- merge(ever_labex,
                    ever_labex_facilities %>%
                      .[, list(inst_id, domain, first_y_labex_facility)] %>%
                      .[, ever_labex_facility_level := 1],
                    by = c('inst_id','domain'), all.x = TRUE
                    )%>%
  .[, ':='(yearn = as.numeric(as.character(year)),
           idn = as.numeric(str_remove(author_id, "A")),
           d1 = as.numeric(str_detect(domain, "1")),
           d2 = as.numeric(str_detect(domain, "2")),
           d3 = as.numeric(str_detect(domain, "3")),
           d4 = as.numeric(str_detect(domain, "4"))
           )] %>%
  .[, chg_af := as.numeric(inst_id != lag(inst_id, order_by = year)), by = 'author_id'] %>%
  .[, ':='(sum_chg_af = sum(chg_af, na.rm =T),
           max_resp= max(resp, na.rm =T),
           max_award_au_total = max(award_au_total, na.rm =T),
           ever_labex_facility_au = min(ifelse(ever_labex_facility_level, year, NA ), na.rm = T ),
           first_y_labex_facility = min(first_y_labex_facility, na.rm = T)
           ), by = 'author_id']%>%
  .[, treat_labex := ifelse(ever_labex_facility_au>first_y_labex_facility,ever_labex_facility_au ,first_y_labex_facility)] %>%
  .[, treat_labex := ifelse(is.na(treat_labex), 0, treat_labex)]
summary(ever_labex$sum_chg_af)
length(unique(ever_labex[resp == 1]$author_id))

gc()

length(unique(ever_labex$inst_id))
length(unique(ever_labex$author_id))
table(ever_labex$first_year_labex)
table(ever_labex$ever_labex_facility_au)
table(ever_labex$treat_labex)
table(ever_labex$treat_labex, ever_labex$domain)

summary(unique(ever_labex[max_award_au_total>0][, list(max_award_au_total)] )$max_award_au_total)
reg_labex <- ever_labex %>%
  .[, treat_labex := ifelse(treat_labex ==Inf, 0, treat_labex)]%>%
  .[!(treat_labex %in% c(2019)) 
    #max_award_au_total>=50000 & 
    #sum_chg_af <=5 # &
    & max_resp>0 
  ]
table(reg_labex$treat_labex, reg_labex$domain)

table(reg_labex$treat_labex)
test_did <- did::att_gt(yname = 'citations',
                tname = 'yearn',
                idname = 'idn',
                gname = 'treat_labex',
                data = reg_labex,
               # allow_unbalanced_panel = TRUE,
               # faster_mode = FALSE,
                xformla = ~ entry_cohort + d1 + d2 +d3 + d4 -1  #+ prod_au_n_tile + prod_inst_n_tile + REG#+ inst_id
                ,control_group = 'nevertreated'
                )
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))
gc()

ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE, min_e = -6, max_e = 8))


ggdid(test_did, ncol = 3)



stacked_reg_labex_df <- sample_df_reg %>%
  .[, ever_labex := max(as.numeric(str_detect(grant_id, "LABX") )), by = 'author_id'] %>%
  #.[ever_labex ==1] %>%
  .[, ':='(first_year_labex = min(ifelse(str_detect(grant_id, "LABX"), year, NA ), na.rm = T ),
           first_year_dgds = min(ifelse(first_year_dgds>0, first_year_dgds, NA) , na.rm = T ),
           first_year_ods = min(ifelse(first_year_ods>0, first_year_ods, NA) , na.rm = T )
  ), by = 'author_id'] %>%
  .[, first_year_labex := ifelse(first_year_labex == Inf, 0, first_year_labex)] %>%
  .[, first_year_dgds := ifelse(first_year_dgds == Inf, 0, first_year_dgds)] %>%
  .[, first_year_ods := ifelse(first_year_ods == Inf, 0, first_year_ods)] %>%
  .[, ':='(max_resp= max(resp, na.rm =T)
  ), by = 'author_id']%>%
  .[first_year_labex != 0 | ( (first_year_dgds != 0 | first_year_ods !=0)
                              & max_resp >0 & first_year_labex==0) ] 


table(unique(stacked_reg_labex_df[, list(author_id, first_year_labex, first_year_dgds)])$first_year_labex, 
      unique(stacked_reg_labex_df[, list(author_id, first_year_labex, first_year_dgds)])$first_year_dgds)

event_studies_labex <-  list()
for( year_treatment in c(2011:2012)){
  df_reg <- stacked_reg_labex_df %>%
    .[, treat := ifelse(first_year_labex == year_treatment, 1, 0)] %>%
    .[treat == 1 | ( (first_year_dgds == year_treatment |
                        first_year_ods == year_treatment)
                     & date_first_idex != 0 & acces_rce != 0
                       )  ] %>%
    .[, t := year - year_treatment]%>%
    .[year >=2005]
  
  df_reg <- match.data(matchit(treat ~entry_cohort + domain + prod_au_n_tile ,
                               df_reg))
  print(table(unique(df_reg[, list(treat, author_id)])$treat))
event_studies_labex[[as.character(year_treatment)]] <- feols(citations ~ i(t, treat, 0) | domain  + year
              ,data  =df_reg,
              cluster = 'author_id'
              )
}
iplot((event_studies_labex))
stacked_reg_labex_df %>% .[, .(mean(award_au_total)), by = "treat"]

# equipex -----------------------------------------------------------------

ever_equipx <- sample_df_reg %>%
  .[, ever_equipx := max(as.numeric(str_detect(grant_id_inst, "EQPX") & type == "facility")), by = 'author_id'] %>%
  .[ever_equipx ==1] %>%
  .[, first_year_equipx := min(ifelse(str_detect(grant_id_inst, "EQPX") & type == "facility", year, NA ), na.rm = T ), by = 'author_id'] %>%
  .[, ':='(yearn = as.numeric(as.character(year)),
           idn = as.numeric(str_remove(author_id, "A")),
           d1 = as.numeric(str_detect(domain, "1")),
           d2 = as.numeric(str_detect(domain, "2")),
           d3 = as.numeric(str_detect(domain, "3")),
           d4 = as.numeric(str_detect(domain, "4"))
  )] %>%
  .[, inst_set := paste(sort(unique(inst_id)), collapse = ","), by = c("author_id", "year")] %>%
  .[, chg_af := as.numeric(inst_set != lag(inst_set, order_by = year)), by = 'author_id'] %>%
  .[, chg_af_relax := 1-as.numeric(str_detect(inst_set, lag(inst_set, order_by = year))
                                 | str_detect(lag(inst_set, order_by = year), inst_set)), by = 'author_id'] %>%
  .[, ':='(sum_chg_af = sum(chg_af, na.rm =T),
           sum_chg_af_relax = sum(chg_af_relax, na.rm =T),
           max_resp= max(resp, na.rm =T),
           max_award_au_total = max(award_au_total, na.rm =T)
  ), by = 'author_id'] %>%
  .[sum_chg_af <=2] %>%
  .[type == "facility"]
summary(ever_equipx$sum_chg_af)
length(unique(ever_equipx[resp == 1]$author_id))

gc()

length(unique(ever_equipx$inst_id))
length(unique(ever_equipx$author_id))
table(ever_equipx$first_year_equipx)
summary(unique(ever_equipx[max_award_au_total>0][, list(max_award_au_total)] )$max_award_au_total)
  
test_did <- did::att_gt(yname = 'nr_source_top_10pct',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = 'first_year_equipx',
                        data = ever_equipx %>%
                          .[#award_au_total>=234536 & 
                            #max_resp>=1
                            ],
                        allow_unbalanced_panel = TRUE,
                        faster_mode = FALSE,
                         #xformla = ~ entry_cohort #+ d1 + d2 +d3 + d4 -1 #+ prod_au_n_tile + prod_inst_n_tile + REG#+ inst_id
                        ,control_group = 'notyettreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))

gc()
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE,  min_e = -5, max_e=5))

aggte(test_did, type = 'dynamic', na.rm = TRUE, min_e = 1, max_e=5)

ggdid(test_did, ncol = 3)


###############################################

ever_personal_grant <- sample_df_reg %>%
  .[, inst_set := paste(sort(unique(inst_id)), collapse = ","), by = c("author_id", "year")] %>%
  .[, chg_af := as.numeric(inst_set != lag(inst_set, order_by = year)), by = 'author_id'] %>%
   .[, ":="(resp_date = min(ifelse(resp >= 1, year, NA), na.rm = T),
            max_resp= max(resp, na.rm =T),
            first_year_dgds = min(ifelse(first_year_dgds >2000& first_year_dgds < 2026, first_year_dgds, NA),na.rm = T),
           # first_year_ods = min(ifelse(first_year_ods >2000 & first_year_ods < 2026, first_year_ods, NA),na.rm = T),
           sum_chg_af = sum(chg_af, na.rm =T),
            max_award_au_total = max(award_au_total, na.rm =T)
            
            ), by = "author_id"] %>%
    .[, ':='(yearn = as.numeric(as.character(year)),
           idn = as.numeric(str_remove(author_id, "A")),
           d1 = as.numeric(str_detect(domain, "1")),
           d2 = as.numeric(str_detect(domain, "2")),
           d3 = as.numeric(str_detect(domain, "3")),
           d4 = as.numeric(str_detect(domain, "4"))
  )]%>% 
  .[, first_year_dgds:= ifelse(!is.na(first_year_dgds) | first_year_dgds != Inf,first_year_dgds, 0)] %>%
  .[ !is.na(first_year_dgds) & first_year_dgds != Inf &# yearn >= first_year_dgds- 5 & yearn <= first_year_dgds+5  &
                                      (max_award_au_total>0 | first_year_dgds == 0)
     ] #%>% .[str_count(field, ',')< 1]

summary(ever_personal_grant$resp_date)
table(unique(ever_personal_grant[, list(author_id, first_year_dgds)])$first_year_dgds)
summary(unique(ever_personal_grant[max_award_au_total>0][, list(max_award_au_total)] )$max_award_au_total)

summary(ever_personal_grant$citations)

length(unique(ever_personal_grant$inst_id))
length(unique(ever_personal_grant$author_id))

gc()
test <- ever_personal_grant %>%
  .[ date_first_idex ==0
     & acces_rce !=0
     & sum_chg_af <=1
     #(max_award_au_total>= 1000000    ) & 
     #| first_year_dgds == 0
     # & max_resp >0 #&  
     & first_year_dgds >acces_rce
  ]

test_did <- did::att_gt(yname = 'publications',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = "first_year_dgds",
                        data = ever_personal_grant %>%
                          .[ #date_first_idex ==0
                             #& acces_rce !=0
                            #  sum_chg_af <=6
                            # & (max_award_au_total>= 1000000    )
                            # | first_year_dgds == 0
                            # & max_resp >0 #&  
                            # & first_year_dgds >acces_rce
                          ]
                        , base_period = "universal"
                        , allow_unbalanced_panel = TRUE
                        ,xformla = ~ #acces_rce + date_first_idex + fusion_date +
                          entry_cohort + domain + prod_au_n_tile + prod_inst_n_tile+ REG#+ inst_id
                        ,control_group = 'notyettreated'
)
ggdid(test_did, ncol = 3)

aggte(test_did, type = "dynamic",min_e = 3, max_e = 9)
ggdid(aggte(test_did, type = "dynamic",min_e = -5, max_e = 6))
gc()


ggdid(aggte(test_did, type = "dynamic"))

# MASSIVE GRANTS ----------------------------------------------------------


test_did <- did::att_gt(yname = 'citations',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = "first_year_dgds",
                        data = ever_personal_grant %>%
                          .[(max_award_au_total> 2000000 
                             | first_year_dgds == 0
                             | first_year_dgds %in% 2006:2014)
                            ]
                        , base_period = "universal"
                       #, allow_unbalanced_panel = TRUE
                        ,xformla = ~ acces_rce + date_first_idex + fusion_date +
                         entry_year + domain #+ type + prod_au_n_tile #+ prod_inst_n_tile#+ city#+ inst_id
                        ,control_group = 'notyettreated'
)
ggdid(test_did, ncol = 3)

aggte(test_did, type = "dynamic",min_e = 3, max_e = 9)

ggdid(aggte(test_did, type = "dynamic",min_e = -5, max_e = 10))
ggdid(aggte(test_did, type = 'group'))
gc()


# erc access --------------------------------------------------------------


###############################################

ever_terc <- sample_df_reg %>%
  .[, ever_terc := max(as.numeric(str_detect(grant_id, "TERC") |str_detect(grant_id, "-ERC")  )), by = 'author_id'] %>%
  #.[ever_labex ==1] %>%
  .[, first_year_terc := min(ifelse(str_detect(grant_id, "TERC") |str_detect(grant_id, "-ERC") , year, NA ), na.rm = T ), 
    by = 'author_id'] %>%
  .[, ":="(resp_date = min(ifelse(resp >= 1, year, NA), na.rm = T),
           max_resp= max(resp, na.rm =T),
           first_year_terc = ifelse(first_year_terc==Inf, 0, first_year_terc),
           max_award_au_total = max(award_au_total, na.rm =T)
           
  ), by = "author_id"] %>%
  .[, ':='(yearn = as.numeric(as.character(year)),
           idn = as.numeric(str_remove(author_id, "A")),
           d1 = as.numeric(str_detect(domain, "1")),
           d2 = as.numeric(str_detect(domain, "2")),
           d3 = as.numeric(str_detect(domain, "3")),
           d4 = as.numeric(str_detect(domain, "4"))
  )]
summary(ever_terc$resp_date)
table(unique(ever_terc[, list(author_id, first_year_terc)])$first_year_terc)
summary(unique(ever_terc[max_award_au_total>0][, list(max_award_au_total)] )$max_award_au_total)

summary(ever_personal_grant$citations)

length(unique(ever_personal_grant$inst_id))
length(unique(ever_personal_grant$author_id))

gc()


test_did <- did::att_gt(yname = 'new_phrase',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = "first_year_terc",
                        data = ever_terc %>%
                          .[#first_year_terc>0 
                            #(max_award_au_total>= 1000000    ) & 
                            #| first_year_dgds == 0
                            #max_resp >0 
                            first_year_terc == 2017 |first_year_terc == 2018 | first_year_terc ==0
                          ]
                        , base_period = "universal"
                        #, allow_unbalanced_panel = TRUE
                        #,xformla = ~ #acces_rce + date_first_idex + fusion_date +
                        #  entry_cohort#  + field  #+ type + prod_au_n_tile + prod_inst_n_tile#+ DEP#+ inst_id
                        ,control_group = 'nevertreated'
)
ggdid(test_did, ncol = 3)

aggte(test_did, type = "dynamic",min_e = 3, max_e = 9)
ggdid(aggte(test_did, type = "dynamic",min_e = -5, max_e = 6))


# ever erc ----------------------------------------------------------------

ever_erc <- sample_df_reg %>%
  .[, ever_erc := max(as.numeric(first_year_erc !=0)), by = 'author_id'] %>%
  .[, treat_erc := min(ifelse(first_year_erc >0, first_year_erc, NA), na.rm =T), by = 'author_id'] %>%
  #.[ever_labex ==1] %>%
  .[, ":="(resp_date = min(ifelse(resp >= 1, year, NA), na.rm = T),
           max_resp= max(resp, na.rm =T),
           first_year_dgds = min(ifelse(first_year_dgds >2000& first_year_dgds < 2026, first_year_dgds, NA),na.rm = T),
           # first_year_ods = min(ifelse(first_year_ods >2000 & first_year_ods < 2026, first_year_ods, NA),na.rm = T),
           max_award_au_total = max(award_au_total, na.rm =T)
           
  ), by = "author_id"] %>%
  .[, ':='(yearn = as.numeric(as.character(year)),
           idn = as.numeric(str_remove(author_id, "A")),
           d1 = as.numeric(str_detect(domain, "1")),
           d2 = as.numeric(str_detect(domain, "2")),
           d3 = as.numeric(str_detect(domain, "3")),
           d4 = as.numeric(str_detect(domain, "4"))
  )]
summary(ever_erc$resp_date)
table(unique(ever_erc[, list(author_id, treat_erc)])$treat_erc)
summary(unique(ever_erc[max_award_au_total>0][, list(max_award_au_total)] )$max_award_au_total)

summary(ever_erc$citations)

length(unique(ever_erc$inst_id))
length(unique(ever_erc$author_id))

gc()


test_did <- did::att_gt(yname = 'citations',
                        tname = 'yearn',
                        idname = 'idn',
                        gname = "treat_erc",
                        data = ever_erc %>%
                          .[
                            #(max_award_au_total>= 1000000    ) & 
                            #| first_year_dgds == 0
                            # max_resp >0 &  
                            # first_year_dgds %in% 2009:2019
                          ]
                        , base_period = "universal"
                        , allow_unbalanced_panel = TRUE
                        #,xformla = ~ #acces_rce + date_first_idex + fusion_date +
                        #  domain #+ field  #+ type + prod_au_n_tile + prod_inst_n_tile#+ DEP#+ inst_id
                        ,control_group = 'notyettreated'
)
ggdid(test_did, ncol = 3)

aggte(test_did, type = "dynamic",min_e = 3, max_e = 9)
ggdid(aggte(test_did, type = "dynamic",min_e = -5, max_e = 6))



