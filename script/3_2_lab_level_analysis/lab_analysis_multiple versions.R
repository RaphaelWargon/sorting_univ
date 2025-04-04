
agg_noctrls <- feols(movers_w ~ 
                    i(post, uni_pub_r*,                                        0)
                  + i(post, uni_pub_s,                                         0) 
                  + i(post, uni_pub_r*uni_pub_s,                               0) 
                  #+ i(post, uni_pub_r*abroad_s,                                0)
                  #+ i(post, uni_pub_s*abroad_r,                                0)
                 # + i(post, uni_pub_r*entrant,                                 0)
                  + i(post, has_idex_r*(1-uni_pub_r),                          0)
                  + i(post, has_idex_s*(1-uni_pub_s),                          0)
                  + i(post, has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s), 0)
                  #+ i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                 0)
                  #+ i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                 0)
                  #+ i(post, has_idex_r*(1-uni_pub_r)*entrant,                  0)
                  + i(post, has_idex_r*uni_pub_r,                              0)
                  + i(post, has_idex_s*uni_pub_s,                              0)
                  + i(post, has_idex_r*uni_pub_r*has_idex_s*uni_pub_s,         0)
                  #+ i(post, has_idex_r*uni_pub_r*abroad_s,                     0)
                  #+ i(post, has_idex_s*uni_pub_s*abroad_r,                     0)
                  #+ i(post, has_idex_r*uni_pub_r*entrant,                      0)
                  #+ size_r*as.factor(year)+size_s*as.factor(year)
                  | 
                    inst_id_receiver + inst_id_sender + year
                  + inst_id_receiver^inst_id_sender
                  #+ cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                  #+ fused_r^year + fused_s^year + fused_r^fused_s^year
                  #+ type_r_year + type_s_year + type_s_type_r_year
                  #+ main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                  , data = reg_df %>%
                    .[, post := as.numeric(year >2008)]%>%
                    .[year >= 2003 &
                        type_r %in% c('facility') 
                      & type_s %in% c('facility')]
                  ,cluster = c('inst_id_sender','inst_id_receiver')
                  
)

agg_noctrls_now <- feols(movers ~ 
                       i(post, uni_pub_r,                                         0)
                     + i(post, uni_pub_s,                                         0) 
                     + i(post, uni_pub_r*uni_pub_s,                               0) 
                     #+ i(post, uni_pub_r*abroad_s,                                0)
                     #+ i(post, uni_pub_s*abroad_r,                                0)
                     # + i(post, uni_pub_r*entrant,                                 0)
                     + i(post, has_idex_r*(1-uni_pub_r),                          0)
                     + i(post, has_idex_s*(1-uni_pub_s),                          0)
                     + i(post, has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s), 0)
                     #+ i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                 0)
                     #+ i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                 0)
                     #+ i(post, has_idex_r*(1-uni_pub_r)*entrant,                  0)
                     + i(post, has_idex_r*uni_pub_r,                              0)
                     + i(post, has_idex_s*uni_pub_s,                              0)
                     + i(post, has_idex_r*uni_pub_r*has_idex_s*uni_pub_s,         0)
                     #+ i(post, has_idex_r*uni_pub_r*abroad_s,                     0)
                     #+ i(post, has_idex_s*uni_pub_s*abroad_r,                     0)
                     #+ i(post, has_idex_r*uni_pub_r*entrant,                      0)
                     #+ size_r*as.factor(year)+size_s*as.factor(year)
                     | 
                       inst_id_receiver + inst_id_sender + year
                     + inst_id_receiver^inst_id_sender
                     #+ cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                     #+ fused_r^year + fused_s^year + fused_r^fused_s^year
                     #+ type_r_year + type_s_year + type_s_type_r_year
                     #+ main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                     , data = reg_df %>%
                       .[, post := as.numeric(year >2008)]%>%
                       .[year >= 2003 &
                           type_r %in% c('facility') 
                         & type_s %in% c('facility')]
                     ,cluster = c('inst_id_sender','inst_id_receiver')
                     
)
agg_ctrls <- feols(movers_w ~ 
                       i(post, uni_pub_r,                                         0)
                     + i(post, uni_pub_s,                                         0) 
                     + i(post, uni_pub_r*uni_pub_s,                               0) 
                     #+ i(post, uni_pub_r*abroad_s,                                0)
                     #+ i(post, uni_pub_s*abroad_r,                                0)
                     # + i(post, uni_pub_r*entrant,                                 0)
                     + i(post, has_idex_r*(1-uni_pub_r),                          0)
                     + i(post, has_idex_s*(1-uni_pub_s),                          0)
                     + i(post, has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s), 0)
                     #+ i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                 0)
                     #+ i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                 0)
                     #+ i(post, has_idex_r*(1-uni_pub_r)*entrant,                  0)
                     + i(post, has_idex_r*uni_pub_r,                              0)
                     + i(post, has_idex_s*uni_pub_s,                              0)
                     + i(post, has_idex_r*uni_pub_r*has_idex_s*uni_pub_s,         0)
                     #+ i(post, has_idex_r*uni_pub_r*abroad_s,                     0)
                     #+ i(post, has_idex_s*uni_pub_s*abroad_r,                     0)
                     #+ i(post, has_idex_r*uni_pub_r*entrant,                      0)
                     #+ size_r*as.factor(year)+size_s*as.factor(year)
                     | 
                       inst_id_receiver + inst_id_sender + year
                     + inst_id_receiver^inst_id_sender
                     + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                     + fused_r^year + fused_s^year + fused_r^fused_s^year
                     #+ type_r_year + type_s_year + type_s_type_r_year
                     + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                     , data = reg_df %>%
                       .[, post := as.numeric(year >2008)]%>%
                       .[year >= 2003 &
                           type_r %in% c('facility') 
                         & type_s %in% c('facility')]
                     ,cluster = c('inst_id_sender','inst_id_receiver')
                     
)
agg_ctrls_now <- feols(movers ~ 
                     i(post, uni_pub_r,                                         0)
                   + i(post, uni_pub_s,                                         0) 
                   + i(post, uni_pub_r*uni_pub_s,                               0) 
                   #+ i(post, uni_pub_r*abroad_s,                                0)
                   #+ i(post, uni_pub_s*abroad_r,                                0)
                   # + i(post, uni_pub_r*entrant,                                 0)
                   + i(post, has_idex_r*(1-uni_pub_r),                          0)
                   + i(post, has_idex_s*(1-uni_pub_s),                          0)
                   + i(post, has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s), 0)
                   #+ i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                 0)
                   #+ i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                 0)
                   #+ i(post, has_idex_r*(1-uni_pub_r)*entrant,                  0)
                   + i(post, has_idex_r*uni_pub_r,                              0)
                   + i(post, has_idex_s*uni_pub_s,                              0)
                   + i(post, has_idex_r*uni_pub_r*has_idex_s*uni_pub_s,         0)
                   #+ i(post, has_idex_r*uni_pub_r*abroad_s,                     0)
                   #+ i(post, has_idex_s*uni_pub_s*abroad_r,                     0)
                   #+ i(post, has_idex_r*uni_pub_r*entrant,                      0)
                   + size_r*as.factor(year)+size_s*as.factor(year)
                   | 
                     inst_id_receiver + inst_id_sender + year
                   + inst_id_receiver^inst_id_sender
                   + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                   + fused_r^year + fused_s^year + fused_r^fused_s^year
                   #+ type_r_year + type_s_year + type_s_type_r_year
                   + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                   , data = reg_df %>%
                     .[, post := as.numeric(year >2008)]%>%
                     .[year >= 2003 &
                         type_r %in% c('facility') 
                       & type_s %in% c('facility')]
                   ,cluster = c('inst_id_sender','inst_id_receiver')
                   
)
etable(agg_noctrls, agg_noctrls_now, agg_ctrls, agg_ctrls_now, keep = 'post')
gc()
etable(agg_noctrls, agg_noctrls_now, agg_ctrls, agg_ctrls_now, keep = 'post', 
       file = "E:\\panel_fr_res\\lab_results\\lab_mobility_agg_within_france_facility_ctrls.tex",
       replace = TRUE)




# abroad ------------------------------------------------------------------
abroad_agg_noctrl <- feols(movers_w ~ 
                    + i(post, uni_pub_r*abroad_s,                               0)
                    + i(post, uni_pub_s*abroad_r,                               0)
                    + i(post, uni_pub_r*entrant,                                0)
                    + i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                0)
                    + i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                0)
                    + i(post, has_idex_r*(1-uni_pub_r)*entrant,                 0)
                    + i(post, has_idex_r*uni_pub_r*abroad_s,                    0)
                    + i(post, has_idex_s*uni_pub_s*abroad_r,                    0)
                    + i(post, has_idex_r*(uni_pub_r)*entrant,                   0)
                    #+ size_r*as.factor(year)+size_s*as.factor(year)
                    | 
                      inst_id_receiver + inst_id_sender + year
                    + inst_id_receiver^inst_id_sender
                    #+ cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                    #+ fused_r^year + fused_s^year + fused_r^fused_s^year
                    #+ ecole_r^year + ecole_s^year + ecole_r^ecole_s^year
                    # + type_r_year + type_s_year + type_s_type_r_year
                    # + public_r^year + public_s^year
                    # + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                    #  +city_r^year + city_s^year + city_r^city_s^year
                    , data = reg_df %>%
                      .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)]
                    %>% .[#year >= 2003 & 
                      type_r %in% c('facility','abroad') 
                      & type_s %in% c('facility','abroad','entrant')
                    ]
                    #,weights = reg_df$size_r + reg_df$size_r
                    ,cluster = c('inst_id_sender','inst_id_receiver')
)
abroad_agg_ctrl <- feols(movers_w ~ 
                           + i(post, uni_pub_r*abroad_s,                               0)
                           + i(post, uni_pub_s*abroad_r,                               0)
                           + i(post, uni_pub_r*entrant,                                0)
                           + i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                0)
                           + i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                0)
                           + i(post, has_idex_r*(1-uni_pub_r)*entrant,                 0)
                           + i(post, has_idex_r*uni_pub_r*abroad_s,                    0)
                           + i(post, has_idex_s*uni_pub_s*abroad_r,                    0)
                           + i(post, has_idex_r*(uni_pub_r)*entrant,                   0)
                           + size_r*as.factor(year)+size_s*as.factor(year)
                           | 
                             inst_id_receiver + inst_id_sender + year
                           + inst_id_receiver^inst_id_sender
                           + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                           + fused_r^year + fused_s^year + fused_r^fused_s^year
                           + ecole_r^year + ecole_s^year + ecole_r^ecole_s^year
                           # + type_r_year + type_s_year + type_s_type_r_year
                            + public_r^year 
                            + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                             +city_r^year 
                           , data = reg_df %>%
                             .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)]
                           %>% .[#year >= 2003 & 
                             type_r %in% c('facility','abroad') 
                             & type_s %in% c('facility','abroad','entrant')
                           ]
                           #,weights = reg_df$size_r + reg_df$size_r
                           ,cluster = c('inst_id_sender','inst_id_receiver')
)

abroad_agg_noctrl_now <- feols(movers ~ 
                             + i(post, uni_pub_r*abroad_s,                               0)
                           + i(post, uni_pub_s*abroad_r,                               0)
                           + i(post, uni_pub_r*entrant,                                0)
                           + i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                0)
                           + i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                0)
                           + i(post, has_idex_r*(1-uni_pub_r)*entrant,                 0)
                           + i(post, has_idex_r*uni_pub_r*abroad_s,                    0)
                           + i(post, has_idex_s*uni_pub_s*abroad_r,                    0)
                           + i(post, has_idex_r*(uni_pub_r)*entrant,                   0)
                           #+ size_r*as.factor(year)+size_s*as.factor(year)
                           | 
                             inst_id_receiver + inst_id_sender + year
                           + inst_id_receiver^inst_id_sender
                           #+ cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                           #+ fused_r^year + fused_s^year + fused_r^fused_s^year
                           #+ ecole_r^year + ecole_s^year + ecole_r^ecole_s^year
                           # + type_r_year + type_s_year + type_s_type_r_year
                           # + public_r^year + public_s^year
                           # + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                           #  +city_r^year + city_s^year + city_r^city_s^year
                           , data = reg_df %>%
                             .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)]
                           %>% .[#year >= 2003 & 
                             type_r %in% c('facility','abroad') 
                             & type_s %in% c('facility','abroad','entrant')
                           ]
                           #,weights = reg_df$size_r + reg_df$size_r
                           ,cluster = c('inst_id_sender','inst_id_receiver')
)
abroad_agg_ctrl_now <- feols(movers ~ 
                         + i(post, uni_pub_r*abroad_s,                               0)
                         + i(post, uni_pub_s*abroad_r,                               0)
                         + i(post, uni_pub_r*entrant,                                0)
                         + i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                0)
                         + i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                0)
                         + i(post, has_idex_r*(1-uni_pub_r)*entrant,                 0)
                         + i(post, has_idex_r*uni_pub_r*abroad_s,                    0)
                         + i(post, has_idex_s*uni_pub_s*abroad_r,                    0)
                         + i(post, has_idex_r*(uni_pub_r)*entrant,                   0)
                         + size_r*as.factor(year)+size_s*as.factor(year)
                         | 
                           inst_id_receiver + inst_id_sender + year
                         + inst_id_receiver^inst_id_sender
                         + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                         + fused_r^year + fused_s^year + fused_r^fused_s^year
                         + ecole_r^year + ecole_s^year + ecole_r^ecole_s^year
                         # + type_r_year + type_s_year + type_s_type_r_year
                         + public_r^year 
                         + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                         +city_r^year 
                         , data = reg_df %>%
                           .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)]
                         %>% .[#year >= 2003 & 
                           type_r %in% c('facility','abroad') 
                           & type_s %in% c('facility','abroad','entrant')
                         ]
                         #,weights = reg_df$size_r + reg_df$size_r
                         ,cluster = c('inst_id_sender','inst_id_receiver')
)


etable(test_feols)


etable(abroad_agg_noctrl, abroad_agg_noctrl_now, abroad_agg_ctrl, abroad_agg_ctrl_now, keep = 'post', 
       file = "E:\\panel_fr_res\\lab_results\\lab_mobility_agg_abroad_facility_ctrls.tex",
       replace = TRUE)


abroad_ctrl_now <- feols(movers ~ 
                             + i(year, uni_pub_r*abroad_s,                               2008)
                             + i(year, uni_pub_s*abroad_r,                               2008)
                             + i(year, uni_pub_r*entrant,                                2008)
                             + i(year, has_idex_r*(1-uni_pub_r)*abroad_s,                2008)
                             + i(year, has_idex_s*(1-uni_pub_s)*abroad_r,                2008)
                             + i(year, has_idex_r*(1-uni_pub_r)*entrant,                 2008)
                             + i(year, has_idex_r*uni_pub_r*abroad_s,                    2008)
                             + i(year, has_idex_s*uni_pub_s*abroad_r,                    2008)
                             + i(year, has_idex_r*(uni_pub_r)*entrant,                   2008)
                             + size_r*as.factor(year)+size_s*as.factor(year)
                             | 
                               inst_id_receiver + inst_id_sender + year
                             + inst_id_receiver^inst_id_sender
                             + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                             + fused_r^year + fused_s^year + fused_r^fused_s^year
                             + ecole_r^year + ecole_s^year + ecole_r^ecole_s^year
                             # + type_r_year + type_s_year + type_s_type_r_year
                             + public_r^year 
                             + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                             +city_r^year 
                             , data = reg_df %>%
                               .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)]
                             %>% .[#year >= 2003 & 
                               type_r %in% c('facility','abroad') 
                               & type_s %in% c('facility','abroad','entrant')
                             ]
                             #,weights = reg_df$size_r + reg_df$size_r
                             ,cluster = c('inst_id_sender','inst_id_receiver')
)
iplot(abroad_ctrl_now, i.select =1)
iplot(abroad_ctrl_now, i.select =2)
iplot(abroad_ctrl_now, i.select =3)
