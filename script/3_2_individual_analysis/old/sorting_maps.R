p_load('sf')

map_path <- "C:\\Users\\common\\misc\\commune_carte\\communes-20220101.shp"
#map_path <- "C:\\Users\\common\\misc\\FRA_adm\\FRA_adm3.shp"

map <- read_sf(map_path)
map$city = map$nom


map_path_dpt <- "C:\\Users\\common\\misc\\departements-20180101-shp\\departements-20180101.shp"

map_dpt <- read_sf(map_path_dpt)
map$city = map$nom

test <- merge(unique(sample[,list(city)]),
            as.data.table(map)[, list(city)][, matched := 1], all.x=T)

test[,.N, by = 'matched']
test2 <- test[is.na(matched)]

as.data.table(map)[str_detect(nom, 'Clichy')][, list(nom)]

to_plot_map <- merge(sample %>%
      .[, city := case_when(city =='Sophia Antipolis' ~"Antibes",
                            city =='Cergy-Pontoise' ~"Cergy",
                            city =='Clichy' ~"Clichy-sous-Bois",
                            city =='La Defense' ~"Courbevoie",
                            city =='Saint-Etienne' ~"Saint-Étienne",
                            city =='St-Malo' ~"Saint-Malo",
                            .default = city)] %>%
      .[year ==2006] %>%
    .[, .(fixef_au_akm = mean(alpha_hat, na.rm =T),
         fixef_inst_akm = mean(fixef_inst_akm, na.rm =T),
         future_colleagues_fe = mean(future_colleagues_fe, na.rm =T),
         alt_inst_fe = mean(alt_inst_fe, na.rm =T),

         rank_au_akm_norm = mean(rank_au_akm_norm, na.rm =T),
         rank_inst_akm_norm = mean(rank_inst_akm_norm, na.rm =T),
         future_colleagues_rank_norm = mean(future_colleagues_rank_norm, na.rm =T),
         alt_inst_fe_rank_norm = mean(alt_inst_fe_rank_norm, na.rm =T),
         
         sorting_base = cor(rank_au_akm_norm,rank_inst_akm_norm),
         sorting_other = cor(rank_au_akm_norm,alt_inst_fe_rank_norm),
         N= n_distinct(author_id),
         n_inst = n_distinct(inst_id_field)
             ), by = 'city' ],
  map, by = 'city', all.x=T)
gc()

to_plot_map <- to_plot_map %>%
  .[as.numeric(substring(insee, 1, 2))<97]


to_plot_map_dpt <- merge(to_plot_map %>%
  .[, code_insee :=substring(insee, 1, 2)]%>%
  .[, geometry := NULL] %>%
    .[, lapply(.SD, mean, na.rm =T), by = c('code_insee'),
      .SDcols = c("fixef_au_akm",
                  "fixef_inst_akm",
                  "future_colleagues_fe",
                  "alt_inst_fe",
                  "rank_au_akm_norm",
                  "rank_inst_akm_norm",
                  "future_colleagues_rank_norm",
                  "alt_inst_fe_rank_norm",'sorting_base','sorting_other','N','n_inst')],
  map_dpt %>%
    dplyr::select(code_insee, geometry), by ='code_insee')
 

ggplot(to_plot_map_dpt)+
  geom_sf(aes(fill = sorting_base, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "pink4")

ggplot(to_plot_map_dpt)+
  geom_sf(aes(fill = sorting_other, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "pink4")


ggplot(to_plot_map_dpt)+
  geom_sf(aes(fill = log(N), geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "goldenrod4")


ggplot(to_plot_map_dpt)+
  geom_sf(aes(fill = log(N), geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "black")

ggplot(to_plot_map_dpt %>%
         .[!is.na(fixef_au_akm)])+
  geom_sf(aes(fill = rank_au_akm_norm, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "steelblue4")


ggplot(to_plot_map_dpt %>%
         .[!is.na(fixef_inst_akm)])+
  geom_sf(aes(fill = rank_inst_akm_norm, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "darkgreen")


ggplot(to_plot_map_dpt %>%
         .[!is.na(alt_inst_fe)])+
  geom_sf(aes(fill = alt_inst_fe_rank_norm, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "white", high = "firebrick")


ggplot(to_plot_map )+
  geom_sf(aes(fill = rank_au_akm_norm, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "steelblue4")

ggplot(to_plot_map )+
  geom_sf(aes(fill = alt_inst_fe_rank_norm, geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "white", high = "firebrick")

