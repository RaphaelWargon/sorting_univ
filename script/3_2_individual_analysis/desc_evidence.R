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
lim <- quantile(desc_test$ratio_subv_propre, probs = c(0.99))
lim
desc_test <- desc_test[ratio_subv_propre<=lim]
gc()

test <- unique(desc_test[,list(name)])

test_fit <- fepois(avg_citations ~ ratio_subv_propre+ratio_subv_propre^2| year + domain^year,
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


est <- binsreg(x=ratio_subv_propre,
               y = citations_raw,
               w =~ domain + as.factor(year),
               #ci = c(2,0),
               data = desc_test %>% .[, domain_year := paste0(domain,as.character(year))],cluster = inst_id,
               
               nbins = 40,
               polyreg = 2,
               polyreggrid   = 40,
               polyregcigrid = 40)   # <-- enable polyreg CI grid points

result <- est$data.plot$`Group Full Sample`

fig <- ggplot() +
  
  # Dots
  geom_point(data = result$data.dots,
             aes(x = x, y = fit),
             color = "steelblue", size = 2) +
  
  # Polyreg line
  geom_line(data = result$data.poly,
            aes(x = x, y = fit),
            color = "steelblue4", linewidth = 0.7)+
  
  # Polyreg line
  geom_ribbon(data = result$data.polyci,
              aes(x = x, ymin = polyci.l, ymax = polyci.r),
              fill = "steelblue4", alpha = 0.1) +
  labs(title = '', x='Research grants to total ratio',
       y = 'Total citations')+
  theme_bw()


print(fig)
ggsave(filename = "D:\\panel_fr_res\\results\\productivity_au\\naive_grant_graph_inst_level.png" , plot = fig, height = 5, width = 9)
ggsave(filename = "C:\\Users\\rapha\\Desktop\\naive_grant_graph_inst_level.png" , plot = fig, height = 5, width = 9)

# Author level ------------------------------------------------------------

desc_au <- sample_df_reg %>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
                            #+ as.numeric(subventions_de_la_region)
                           # + as.numeric(subventions_union_europeenne)
                            
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[!is.na(ratio_subv_propre)] %>%
  .[, log_ratio_subv_propre := log(ratio_subv_propre)]%>%
  .[ratio_subv_propre<=1 ] %>%
  .[, log_ratio_subv_propre := log(as.numeric(ratio_subv_propre)) ] %>% 
  .[, log_cit_raw := log(citations_raw+1)]
summary(desc_au$ratio_subv_propre)
sqrt(var(desc_au$ratio_subv_propre))
lim <- quantile(desc_au$ratio_subv_propre, probs = c(0.99))
lim
desc_au<-   desc_au %>% .[ratio_subv_propre<=lim & ratio_subv_propre>0]
gc()
fit_au <- fepois(c(publications_raw, citations_raw, nr_source_top_10pct_raw,
                   total_new_phrase_comb_reuse) ~ ratio_subv_propre+ratio_subv_propre^2|
                   year + author_id  + domain^year 
                 ,desc_au
                   ,cluster = 'author_id'
)
etable(fit_au,dict = c('ratio_subv_propre' = 'Research grants to total ratio',
                       'citations_raw' = 'Total citations',
                       'publications_raw' = 'Total publications',
                       'nr_source_top_10pct_raw' = "Top 10% journal publications",
                       'total_new_phrase_comb_reuse' = 'New phrases',
                       'author_id' = 'Researcher ID', 'year' = 'Year',
                       'domain' = 'Domain'),
       
      file = "D:\\panel_fr_res\\results\\productivity_au\\naive_grant_regression.tex")

resid_au_year <- fepois(citations_raw ~ 1|
                          year + author_id  + domain^year 
                        ,desc_au
                        ,cluster = 'author_id'
)

desc_au <- desc_au %>% .[!author_id %in% resid_au_year$fixef_removed$author_id]
desc_au$resid = resid_au_year$residuals
#p <- ggplot(desc_au)+
#  geom_point(aes(x=ratio_subv_propre, y = log(citations_raw+1), color = "obs"),alpha = 0.2, stroke = 0)+
#  #geom_point(aes(x=ratio_subv_propre, y = log(predicted+1), color = "pred"),alpha = 0.1, stroke = 0)+
#  geom_smooth(aes(x=ratio_subv_propre, y = log(citations_raw+1), color ='fit') )+
#  scale_color_manual(
#    name   = NULL,
#    values = c(obs = 'steelblue', fit = 'steelblue4', pred = 'grey60'),
#    labels = c(obs = "Log Citations", fit = "LOESS fit", pred = 'Predicted values')
#  ) +
#  labs(
#    x        = "Ratio grants / all budget",
#    y        = "Log average citations"
#  ) +
#  theme_minimal() +
#  theme(
#    plot.title         = element_text(face = "bold", size = 16, margin = margin(b = 4)),
#    plot.subtitle      = element_text(color = "grey40", size = 11, margin = margin(b = 12)),
#    legend.position    = "right",
#    legend.justification = "left",
#    legend.margin      = margin(b = 8),
#    panel.grid.minor   = element_blank(),
#    panel.grid.major   = element_line(color = "grey92", linewidth = 0.4),
#    axis.title         = element_text(color = "grey30"),
#    plot.margin        = margin(16, 20, 16, 16)
#  )
#p  


est <- binsreg(x=ratio_subv_propre,
               y = citations_raw,
               w =~ domain + as.factor(year),
               #ci = c(2,0),
               data = desc_au %>% .[, domain_year := paste0(domain,as.character(year))],cluster = author_id,
               
               polyreg = 2,
               polyreggrid   = 40,
               polyregcigrid = 40)   # <-- enable polyreg CI grid points

result <- est$data.plot$`Group Full Sample`

fig <- ggplot() +
  
  # Dots
  geom_point(data = result$data.dots,
             aes(x = x, y = fit),
             color = "steelblue", size = 2) +
  
  # Polyreg line
  geom_line(data = result$data.poly,
            aes(x = x, y = fit),
            color = "steelblue4", linewidth = 0.7)+
  
  # Polyreg line
  geom_ribbon(data = result$data.polyci,
              aes(x = x, ymin = polyci.l, ymax = polyci.r),
              fill = "steelblue4", alpha = 0.1) +
  labs(title = '', x='Research grants to total ratio',
         y = 'Total citations')+
  theme_bw()


print(fig)
ggsave(filename = "D:\\panel_fr_res\\results\\productivity_au\\naive_grant_graph.png" , plot = fig, height = 5, width = 9)


