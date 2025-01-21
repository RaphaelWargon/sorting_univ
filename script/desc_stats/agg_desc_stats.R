cols_to_wins<- c('publications','citations','nr_source_top_5pct')

agg_by_type_y <- ds %>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]%>%
  .[, type := case_when(#inst_type == 'company' ~ 'Privé',
                        #uni_pub == 1 & cnrs == 1 ~ "UMR Université et CRNS",
                        uni_pub == 1 ~ "Université",
                       # cnrs== 1 ~ "CNRS hors UMR",
                       # inst_type == "education" ~ "Autre ESR",
                        .default = 'Autre'
  ) ] %>%
  .[, .(n_authors = n_distinct(author_id),
        n_pub = sum(publications, na.rm = T),
        n_citations = sum(citations, na.rm = T),
        nr_source_top_5pct = sum(nr_source_top_5pct, na.rm = T),
        avg_pub = mean(publications, na.rm = T),
        avg_citations = mean(citations, na.rm = T),
        avg_nr_source_top_5pct = mean(nr_source_top_5pct, na.rm = T)
        
        ), by = c('type','year')]

ggplot(agg_by_type_y)+
  geom_line(aes(x=year, y = n_authors, color = type))+
  theme_classic()+
  scale_color_manual(values = c("slateblue",'firebrick'))+
  labs(title = "Number of researchers active per year")

ggplot(agg_by_type_y)+
  geom_line(aes(x=year, y = n_pub, color = type))+
  theme_classic()+
  scale_color_manual(values = c("slateblue",'firebrick'))+
  labs(title = "Total number of publications")+
  xlab('Year')+ylab('')


ggplot(agg_by_type_y)+
  geom_line(aes(x=year, y = nr_source_top_5pct, color = type))+
  theme_classic()+
  scale_color_manual(values = c("slateblue",'firebrick'))+
  labs(title = "Total number of publications")+
  xlab('Year')+ylab('')

