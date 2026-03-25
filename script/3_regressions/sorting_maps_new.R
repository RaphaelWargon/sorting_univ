p_load('sf')

map_path <- "C:\\Users\\common\\misc\\commune_carte\\communes-20220101.shp"
#map_path <- "C:\\Users\\common\\misc\\FRA_adm\\FRA_adm3.shp"

map <- read_sf(map_path)
map$city = map$nom


map_path_dpt <- "C:\\Users\\common\\misc\\departements-20180101-shp\\departements-20180101.shp"

map_dpt <- read_sf(map_path_dpt)
map$city = map$nom

to_plot_map <- merge(sample_df_reg %>%
      .[, city := case_when(city =='Sophia Antipolis' ~"Antibes",
                            city =='Cergy-Pontoise' ~"Cergy",
                            city =='Clichy' ~"Clichy-sous-Bois",
                            city =='La Defense' ~"Courbevoie",
                            city =='Saint-Etienne' ~"Saint-Étienne",
                            city =='St-Malo' ~"Saint-Malo",
                            .default = city)] %>%
      .[year ==2006] %>%
    .[, .(mean_cit = mean(citations_reweight),
          med_cit = median(citations_reweight),
         N= n_distinct(author_id),
         n_inst = n_distinct(merged_inst_id),
         n_inst_field = n_distinct(merged_inst_id_field)
             ), by = 'city' ],
  map, by = 'city', all.x=T)
gc()
to_plot_map <- to_plot_map %>%
  .[as.numeric(substring(insee, 1, 2))<97]

to_plot_map_dpt <- merge(to_plot_map %>%
                           .[mean_cit <500] %>% 
                           .[, code_insee :=substring(insee, 1, 2)]%>%
                           .[, geometry := NULL] %>%
                           .[, lapply(.SD, mean, na.rm =T), by = c('code_insee'),
                             .SDcols = c('mean_cit','median_cit','N','n_inst')],
                         map_dpt %>%
                           dplyr::select(code_insee, geometry), by ='code_insee')


plot_n_inst <- ggplot(to_plot_map_dpt)+
  geom_sf(aes(fill = log(n_inst), geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51), ) +
  scale_fill_gradient(low = "azure", high = "grey3",name = 'Log # Inst')+
  theme_minimal() + xlab('') + ylab('')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()
  )

ggsave(plot = plot_n_inst, path = "C:/Users/rapha/Desktop/productivity_results",
       filename = 'map_n_inst.png')
plot_n_au <- ggplot(to_plot_map_dpt)+
  geom_sf(aes(fill = log(N), geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51), ) +
  scale_fill_gradient(low = "azure", high = "steelblue4",name = 'Log # researchers')+
  theme_minimal() + xlab('') + ylab('')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()
  )
ggsave(plot = plot_n_au, path = "C:/Users/rapha/Desktop/productivity_results",
       filename = 'map_n_au.png')


plot_mean_cit <- ggplot(to_plot_map_dpt )+
  geom_sf(aes(fill = mean_cit, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51), ) +
  scale_fill_gradient(low = "azure", high = "firebrick4",name = 'Average citations')+
  theme_minimal() + xlab('') + ylab('')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()
  )
plot_mean_cit
ggsave(plot = plot_mean_cit, path = "C:/Users/rapha/Desktop/productivity_results",
       filename = 'map_mean_cit.png')
