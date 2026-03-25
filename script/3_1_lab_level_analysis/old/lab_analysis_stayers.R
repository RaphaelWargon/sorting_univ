rm(list = ls())
gc()
library('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot')
wins_vars <- function(x, pct_level = 0.025){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}


inputpath <- "E:\\panel_fr_res\\unilateral_inst_level_flows.parquet"

ds_unilat <- as.data.table(open_dataset(inputpath)) %>%
  .[, ':='(first_y_lab = min(year), last_y_lab = max(year)), by ='inst_id']

test <- unique(ds_unilat[first_y_lab >2003][,  list(name, inst_id,type_fr,type)][type!='company'])


all_combinations <- CJ(
  inst_id = unique(ds_unilat$inst_id),
  year = seq(min(ds_unilat$year), max(ds_unilat$year))
)
reg_df_unilat <- merge(ds_unilat, all_combinations, by = c("inst_id", "year"), all.x=TRUE, all.y = TRUE)
ggplot(reg_df_unilat %>% .[, .(N = n_distinct(inst_id)), by = 'year'])+
  geom_line(aes(x=year, y= N))

cols_to_fill_down <-c("name","city","fused","uni_pub", "cnrs", "type","main_topic",
                        "idex", "type_fr", "secteur", "prive",'first_y_lab','last_y_lab')
cols_to_win <- colnames(ds_unilat)[str_detect(colnames(ds_unilat),'stayer|total|mover')]

reg_df_unilat <- reg_df_unilat %>%
  .[, (cols_to_fill_down) := lapply(.SD, \(x)
                                      zoo::na.locf(zoo::na.locf(x, na.rm = FALSE), fromLast = TRUE)),
    by = inst_id, .SDcols = cols_to_fill_down] %>%
  .[, ':='( has_idex = ifelse(!is.na(idex ) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  ),
            idex_annulee = ifelse(!is.na(idex ) & str_detect(idex, 'annulee'), 1, 0  )
  )] %>%
  .[, (cols_to_win):= lapply(.SD, wins_vars), .SDcols = cols_to_win]%>% 
  .[, size_03 := max(total*as.numeric(year==2003), na.rm=T), by = inst_id ] %>%
  .[ first_y_lab <=2003 & first_y_lab <= year & year >= 1997] 

ggplot(reg_df_unilat  %>% .[, .(N = n_distinct(inst_id)), by = 'year'])+
  geom_line(aes(x=year, y= N))

ggplot(unique(reg_df_unilat  %>% .[, .(inst_id, size_03, uni_pub)]))+
  geom_histogram(aes(x=size_03, fill = as.factor(uni_pub)), position='dodge')

#reg_df_unilat <- reg_df_unilat %>% .[year >= 2003]

p <-ggplot(reg_df_unilat%>%
             .[!(inst_id == 'abroad')# & first_y_lab <=2003
             ] %>%
             .[!is.na(uni_pub ) ] %>%
             .[, ':='(uni_pub = fifelse(is.na(uni_pub ), 0, uni_pub )
             )]%>%
             .[, move := case_when(uni_pub ==1  ~ 'Inside public universities',
                                   uni_pub ==0 ~ 'Outside public universities',
                                   .default = 'With several affilitions')]%>%
             .[, lapply(.SD, mean, na.rm = T), by = c('move','year'),.SDcols = cols_to_win] 
)+
  geom_line(aes(x=year,y=total_w, color = move))+
  scale_color_manual(values = c('steelblue','black'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

reg_df_subset <- reg_df_unilat[inst_id != 'I1294671590'] %>%
  .[type%in%c('facility','university','other','government') 
   # & main_topic %in% c(
   # "Engineering",
   # "Computer Science",                            
   # "Biochemistry, Genetics and Molecular Biology",
   # "Agricultural and Biological Sciences",        
   # #"Social Sciences",
   # #"Economics, Econometrics and Finance",         
   # "Medicine",
   # "Earth and Planetary Sciences",                
   # "Environmental Science",
   # "Chemical Engineering",                        
   # "Mathematics",
   # "Materials Science",                           
   # "Physics and Astronomy", 
   # "Chemistry",                                   
   #  "Neuroscience", 
   # #"Arts and Humanities",                         
   # #"Business, Management and Accounting", 
   # "Energy",                                      
   # #"Psychology", 
   # "Immunology and Microbiology",                 
   # #"", 
   # "Pharmacology, Toxicology and Pharmaceutics",  
   # #"Decision Sciences", 
   #  "Nursing" 
   # )
    ] %>%.[size_03 <= 600& size_03 >= 20]
table(unique(reg_df_subset[, list(inst_id, uni_pub)])$uni_pub)

hist(unique(reg_df_subset[, list(inst_id, size_03)])$size_03)

ggplot(unique(reg_df_subset[, list(inst_id, uni_pub, size_03)]))+
  geom_histogram(aes(x=size_03, fill = as.factor(uni_pub)))
outcomes_dict <- c(
  "total_w"                  =     'Number of researchers',
  "total_w_junior"           =     'Number of junior researcher',
  "total_w_senior"           =     'Number of senior researcher',
  "total_w_medium"           =     'Number of medium researcher',
  "total_w_foreign_entrant"  =     'Number of foreign entrant',
  
  "stayers_w"                  =     'Continuing researchers',
  "stayers_w_junior"           =     'Continuing junior researcher',
  "stayers_w_senior"           =     'Continuing senior researcher',
  "stayers_w_medium"           =     'Continuing medium researcher',
  "stayers_w_foreign_entrant"  =     'Continuing foreign entrant',
  
  "movers_w"                  =     'Total flows',
  "movers_w_junior"           =     'Junior researcher flows',
  "movers_w_senior"           =     'Senior researcher flows',
  "movers_w_medium"           =     'Medium researcher flows',
  "movers_w_own_entrant_r"    =     'Exiting from entry institution',
  "movers_w_own_entrant_s"    =     'Exiting from entry institution',
  "movers_w_foreign_entrant"  =     'Foreign entrant flows',
  
  "uni_pub"                   =      ' in universities',
  "has_idex"                  =      ' in Idex',
  "has_idex*uni_pub"          =      ' in Idex public universities',
  "cnrs"                      =      ' in CNRS',   
  "cnrs*uni_pub"              =      ' in public universities with CNRS',
  "fused"                     =      ' in fused HEI',   
  "fused*uni_pub"             =      ' in fused public universities'
)

formula_event_study <-paste0('~',
                            " i(year, uni_pub, 2007)",
                            "+ i(year, has_idex, 2007)  ",
                            "+i(year, has_idex*uni_pub, 2007)",
                            "+i(year, fused, 2007)",
                            #"+ i(year, fused*uni_pub, 2007)",
                            "+i(year, cnrs, 2007)"
                           # ,"+ i(year, cnrs*uni_pub, 2007)"
)


fe_large <- "| inst_id + year+type^year +ecole^year + prive^year+city^year+ size_03^year+ main_topic^year"
fe_min <- '| inst_id +year'
list_event_study <- list()
list_event_study_simple <- list()

outcomes <- colnames(ds_unilat %>% dplyr::select(starts_with('total_w')))
outcomes <- outcomes[outcomes %in% names(outcomes_dict)]
for(var in outcomes){

  event_study_var_simple <- fepois(as.formula(paste0(var, formula_event_study, fe_min))
                           , data = reg_df_subset
                           ,cluster = c('inst_id')
  )
  list_event_study_simple[[var]] <- event_study_var_simple
  
  var_path = paste0("E:\\panel_fr_res\\lab_results\\unilat\\simple\\", var)
  if (!file.exists(var_path)){
    dir.create(var_path, recursive = TRUE)
  }
  formula_normalized = str_replace_all(as.character(list_event_study_simple[[var]]$fml)[3], '\\n|\\s', '')
  list_i_variables = str_extract_all(formula_normalized, pattern = '(?<=i\\(year\\,)[A-z\\s*\\(\\)\\d-]*(?=\\,)')[[1]]
  for(i_select in 1:str_count(formula_normalized, pattern = 'i\\(')){
    pdf(paste0(var_path, '\\', str_replace_all(list_i_variables[[i_select]], '\\W', '_')[[1]], ".pdf"))
    iplot(list_event_study_simple[[var]], i.select=i_select, 
          main =  paste0(outcomes_dict[[var]],outcomes_dict[[list_i_variables[[i_select]]]])
    )
    dev.off()
    iplot(list_event_study_simple[[var]], i.select=i_select, 
          main =  paste0(outcomes_dict[[var]],outcomes_dict[[list_i_variables[[i_select]]]])
    )
  }
  event_study_var <- fepois(as.formula(paste0(var, formula_event_study, fe_large))
                           , data = reg_df_subset
                           ,cluster = c('inst_id')
  )
  list_event_study[[var]] <- event_study_var
  
  var_path = paste0("E:\\panel_fr_res\\lab_results\\unilat\\ctrl\\", var)
  if (!file.exists(var_path)){
    dir.create(var_path, recursive = TRUE)
  }
  formula_normalized = str_replace_all(as.character(list_event_study[[var]]$fml)[3], '\\n|\\s', '')
  list_i_variables = str_extract_all(formula_normalized, pattern = '(?<=i\\(year\\,)[A-z\\s*\\(\\)\\d-]*(?=\\,)')[[1]]
  for(i_select in 1:str_count(formula_normalized, pattern = 'i\\(')){
    pdf(paste0(var_path, '\\', str_replace_all(list_i_variables[[i_select]], '\\W', '_')[[1]], ".pdf"))
    iplot(list_event_study[[var]], i.select=i_select, 
          main =  paste0(outcomes_dict[[var]],outcomes_dict[[list_i_variables[[i_select]]]])
    )
    dev.off()
    iplot(list_event_study[[var]], i.select=i_select, 
          main =  paste0(outcomes_dict[[var]],outcomes_dict[[list_i_variables[[i_select]]]])
    )
  }
  
}



ggplot(reg_df_unilat %>%
         .[, type_reg := case_when(
           has_idex*uni_pub*cnrs==1 ~"IDEX uni CNRS",
           has_idex*uni_pub ==1 ~ "IDEX uni",
           uni_pub ==1 ~ "uni",
           has_idex*cnrs==1~"IDEX CNRS",
           uni_pub*cnrs==1 ~"uni CNRS",
           has_idex==1 ~"IDEX",
           cnrs==1 ~"CNRS",
           .default = 'none'
           )] %>%
         .[, lapply(.SD, median, na.rm=T), by = c('type_reg','year'),.SD = cols_to_win])+
  geom_line(aes(x=year, y=total_w,colour = type_reg))


ggplot(reg_df_unilat %>%
         .[, type_reg := case_when(
           has_idex*uni_pub*cnrs==1 ~"IDEX uni CNRS",
           has_idex*uni_pub ==1 ~ "IDEX uni",
           uni_pub ==1 ~ "uni",
           has_idex*cnrs==1 ~"IDEX CNRS",
           uni_pub*cnrs==1 ~"uni CNRS",
           has_idex==1 ~"IDEX",
           cnrs==1 ~"CNRS",
           .default = 'none'
         )] %>%
         .[, .N, by = c('type_reg','year')])+
  geom_line(aes(x=year, y=N,colour = type_reg))


# maps --------------------------------------------------------------------
p_load('sf')

map_path <- "C:\\Users\\common\\misc\\commune_carte\\communes-20220101.shp"
#map_path <- "C:\\Users\\common\\misc\\FRA_adm\\FRA_adm3.shp"

map <- read_sf(map_path)
map$city = map$nom


map_path_dpt <- "C:\\Users\\common\\misc\\departements-20180101-shp\\departements-20180101.shp"

map_dpt <- read_sf(map_path_dpt)
map$city = map$nom



to_plot_map <- merge(reg_df_unilat %>%
                       .[, city := case_when(city =='Sophia Antipolis' ~"Antibes",
                                             city =='Cergy-Pontoise' ~"Cergy",
                                             city =='Clichy' ~"Clichy-sous-Bois",
                                             city =='La Defense' ~"Courbevoie",
                                             city =='Saint-Etienne' ~"Saint-Étienne",
                                             city =='St-Malo' ~"Saint-Malo",
                                             .default = city)] %>%
                       .[year %in% c(2003,2006, 2017)] %>%
                       .[, lapply(.SD, sum, na.rm=T), by = c('city','year')
                         , .SDcols = outcomes ],
                     map, by = 'city', all.x=T)
gc()

to_plot_map <- to_plot_map %>%
  .[as.numeric(substring(insee, 1, 2))<97]


to_plot_map_dpt <- merge(to_plot_map %>%
                           .[, code_insee :=substring(insee, 1, 2)]%>%
                           .[, geometry := NULL] %>%
                           .[, lapply(.SD, sum, na.rm =T), by = c('code_insee','year'),
                             .SDcols = outcomes],
                         map_dpt %>%
                           dplyr::select(code_insee, geometry), by ='code_insee')



ggplot(to_plot_map_dpt[year ==2017])+
  geom_sf(aes(fill = log(total_w), geometry = geometry))+
  coord_sf(xlim = c(-5,10), ylim=c(42,51)) +
  scale_fill_gradient(low = "azure", high = "pink4")

