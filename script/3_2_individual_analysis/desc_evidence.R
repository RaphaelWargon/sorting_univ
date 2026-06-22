desc_test <- ds %>%
  .[, ':='(avg_publications = fifelse(n_au >0, publications_raw/n_au, 0),
           avg_citations =    fifelse(n_au >0, citations_raw/n_au, 0))] %>%
  .[, ":="(n_au_2003 = max(as.numeric(year == 2003)*n_au ),
           min_n_au = min(n_au),
           avg_publications_2003 = max(as.numeric(year == 2003)*avg_publications ),
           avg_citations_2003 = max(as.numeric(year == 2003)*avg_citations ),
           publications_2003 = max(as.numeric(year == 2003)*publications_raw ),
           citations_2003 = max(as.numeric(year == 2003)*citations_raw ),
           nr_source_top_5pct_raw_2003 = max(as.numeric(year == 2003)*nr_source_top_5pct_raw ),
           last_year_lab = max(year)
  ), by = c('inst_id')]%>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[!is.na(ratio_subv_propre)] %>%
  .[, log_ratio_subv_propre := log(ratio_subv_propre)]%>%
  .[ratio_subv_propre<=1 & n_au >50  & type_fr == "Université" & type == 'education'] %>%
  .[, inst_id_domain := paste0(inst_id,'_',domain)]
summary(desc_test$ratio_subv_propre)
gc()

test <- unique(desc_test[,list(name)])

test_fit <- fepois(avg_citations ~ ratio_subv_propre + ratio_subv_propre^2 + ratio_subv_propre^3 | year + inst_id_domain + domain^year, 
                  desc_test,
                  weights = desc_test$n_au,
                  cluster = 'inst_id_domain'
                  )
etable(test_fit)
desc_test <- desc_test %>% .[!inst_id_domain %in% test_fit$fixef_removed$inst_id_domain]
desc_test$predicted = predict(test_fit)
summary(desc_test$n_au)
p <- ggplot(desc_test)+
  geom_point(aes(x=ratio_subv_propre, y = log(avg_citations+1), color = "obs", size = n_au),alpha = 0.2, stroke = 0)+
  geom_point(aes(x=ratio_subv_propre, y = log(predicted+1), color = "pred", size = n_au),alpha = 0.1, stroke = 0)+
  geom_smooth(aes(x=ratio_subv_propre, y = log(avg_citations+1), color ='fit',weight = n_au) )+
  scale_color_manual(
    name   = NULL,
    values = c(obs = 'steelblue', fit = 'steelblue4', pred = 'grey60'),
    labels = c(obs = "Log Citations", fit = "LOESS fit", pred = 'Predicted values')
  ) +
  labs(
    x        = "Ratio grants / all budget",
    y        = "Log average citations"
  ) +
  scale_size_continuous(name = 'Active authors')+
  theme_minimal() +
  theme(
    plot.title         = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey40", size = 11, margin = margin(b = 12)),
    legend.position    = "right",
    legend.justification = "left",
    legend.margin      = margin(b = 8),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.4),
    axis.title         = element_text(color = "grey30"),
    plot.margin        = margin(16, 20, 16, 16)
  )
p  


# Author level ------------------------------------------------------------

desc_test_au <- sample_df_reg %>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[!is.na(ratio_subv_propre)] %>%
  .[, log_ratio_subv_propre := log(ratio_subv_propre)]%>%
  .[ratio_subv_propre<=1 ] %>%
  .[, log_ratio_subv_propre := log(as.numeric(ratio_subv_propre)) ] %>% 
  .[, log_cit_raw := log(citations_raw+1)]
summary(desc_test_au$ratio_subv_propre)
gc()
fit_au <- fepois(citations_raw ~ ratio_subv_propre  | year + author_id + domain^year + inst_id_set^year + in_ecole^year, 
                 desc_test_au,
                   cluster = 'author_id'
)
etable(fit_au)

desc_test_au <- desc_test_au %>% .[!author_id %in% fit_au$fixef_removed$author_id]
desc_test_au$predicted = predict(fit_au)


p <- ggplot(desc_test_au)+
  geom_point(aes(x=ratio_subv_propre, y = log(citations_raw+1), color = "obs"),alpha = 0.2, stroke = 0)+
  #geom_point(aes(x=ratio_subv_propre, y = log(predicted+1), color = "pred"),alpha = 0.1, stroke = 0)+
  geom_smooth(aes(x=ratio_subv_propre, y = log(citations_raw+1), color ='fit') )+
  scale_color_manual(
    name   = NULL,
    values = c(obs = 'steelblue', fit = 'steelblue4', pred = 'grey60'),
    labels = c(obs = "Log Citations", fit = "LOESS fit", pred = 'Predicted values')
  ) +
  labs(
    x        = "Ratio grants / all budget",
    y        = "Log average citations"
  ) +
  theme_minimal() +
  theme(
    plot.title         = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey40", size = 11, margin = margin(b = 12)),
    legend.position    = "right",
    legend.justification = "left",
    legend.margin      = margin(b = 8),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.4),
    axis.title         = element_text(color = "grey30"),
    plot.margin        = margin(16, 20, 16, 16)
  )
p  

res_log_cit <- 

binsreg(x=log_ratio_subv_propre,
        y = log_cit_raw,
        w= ~ as.factor(year) + as.factor(author_id),
        data = desc_test_au %>% .[log_ratio_subv_propre>-Inf] %>% .[!str_detect(inst_id_set,',')]
        
        ,
        polyreg = 2,
        nbins = 50
        )

