rm(list = ls())
gc()
#install.packages('devtools')
library('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'dplyr','magrittr','tidyr','ggplot2'
       ,'binsreg',
       'DescTools',
       'cowplot',
       'boot',
       'DIDmultiplegt',
       "DIDmultiplegtDYN"#,'didimputation'
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

inputpath <- "D:\\panel_fr_res\\panel_smoothed.parquet"

#inputpath <- "C:\\Users\\rapha\\Desktop\\panel_smoothed.parquet"
#
#ds <- open_dataset(inputpath) 
#ds$schema$names

ds <- open_dataset(inputpath) %>%
  filter(
    last_year-entry_year >2
    & entry_year >=1965 
    & year >= 1997
    #& entry_year <=2003
    & citations >0
    & !(is.na(field))&!is.na(city) & !is.na(cnrs) & !is.na(type) 
  )%>% select(-inst_set_this_year)
ds <- as.data.table(ds)
ds %>% .[, .N, by = c('author_id','merged_inst_id','year','city')]%>%.[, .N, by ='N']

ggplot(ds %>%
         .[, .(N= n_distinct(author_id)), by = "entry_year"])+geom_col(aes(x= entry_year, y = N) )
ggplot(ds %>%
         .[, .(N= n_distinct(author_id)), by = c('year',"entry_year")])+
  geom_col(aes(x= year, y = N, fill = entry_year) )

#nrow(unique(ds[,list(author_id)])) #313767

gc()

##
#inst_to_exclude <- c('I4210159570')
cols_to_wins <- c("publications_raw","avg_rank_source_raw","nr_source_top_5pct_raw"  ,"nr_source_top_10pct_raw" ,"nr_source_mid_40pct_raw","nr_source_top_20pct_raw" ,"nr_source_btm_50pct_raw","citations_raw"
                  ,"publications","avg_rank_source","nr_source_top_5pct"  ,"nr_source_top_10pct" ,"nr_source_mid_40pct","nr_source_top_20pct" ,"nr_source_btm_50pct","citations"
                  )

sample_df <- ds %>%
  #### Checking that there are enough observations for each individual or author :
  .[, ':='(n_inst_id_sample = n_distinct(merged_inst_id)), by = 'author_id'] %>%
  .[, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
           n_authors_sample = n_distinct(author_id),
           n_y_sample_inst = n_distinct(year)), by = c('merged_inst_id','field')]%>%
  .[, n_y_in_sample_au := n_distinct(year), by = 'author_id']%>%
  .[ n_authors_w_several_inst > 0 # ensure that the author and firm are part of the connected set
     & n_y_in_sample_au >=2 & n_authors_sample >1 & n_y_sample_inst>1] %>% #remove labs that are too poorly measured
  
 # ##### recode main field for wrong affiliation
 # .[,max_field := dplyr::first(ifelse(n_authors_sample == max(n_authors_sample* as.numeric(!is.na(field)) ) & !is.na(field), field, NA), na_rm = T),
 #   by = "merged_inst_id"] %>%
 # .[,field_value := n_authors_sample/n_distinct(author_id),by = 'merged_inst_id']%>%
 # .[,max_field_value := max(field_value), by = 'merged_inst_id'] %>%
 # .[,field_recoded := ifelse( ((field_value <0.02| n_authors_sample<=10) ) | is.na(field), max_field, field)]

  ### 
  .[, log_citations:=log(citations)] %>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]%>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce),0,acces_rce),
           date_first_idex = ifelse(is.na(date_first_idex),0,date_first_idex),
           fusion_date = ifelse(is.na(fusion_date),0,fusion_date)
  )]
#rm(ds)
gc()

sample_df <- sample_df %>% 
  .[, ':='(n_obs_au = .N), by = "author_id"]%>%
  .[n_obs_au >1 & n_inst_y <=5] %>%
  .[, ':='(n_au_inst_id_field_y = .N), by = c('merged_inst_id', 'field','year')] %>%
  .[, min_n_au_inst_id_field_y := min(n_au_inst_id_field_y), by = c('merged_inst_id', "field")]%>%
  .[ min_n_au_inst_id_field_y >1 & n_y_sample_inst >= 20] %>%
  .[!(acces_rce %in%  c(2015))
    & !(date_first_idex %in% c(2014))] %>%
  .[, pub_04_07 := sum(as.numeric(year > 2004 & year <= 2007) * publications_raw ), by = 'author_id'] %>%
  .[pub_04_07 >=2] %>%
  .[, n_lt := n_distinct(author_id), by = c('merged_inst_id','field','year')] %>%
  .[, merged_inst_id_field := paste0(merged_inst_id, '_', field)] %>%
  .[, fusion_date := fifelse(fusion_date =="2023", "0", as.character(fusion_date))] %>%
  .[year != "2020"] %>%
  .[ , ':='(fusion_date = as.factor(fusion_date),
            date_first_idex = as.factor(date_first_idex),
            acces_rce = as.factor(acces_rce),
            year = as.factor(year))]


gc()

ggplot(unique(sample_df[, list(author_id, n_inst_y)]))+
  geom_density(aes(x=n_inst_y))

length(unique(sample_df$author_id)) #265916
nrow(unique(sample_df[, list(merged_inst_id, field)])) #2868
length(unique(sample_df$merged_inst_id)) #966

ggplot(sample_df %>% 
         .[, .(n =n_distinct(year)), by ='merged_inst_id'])+geom_histogram(aes(x= n) )


#fwrite(sample_df, "E:\\panel_fr_res\\sample_df.csv")

# Arcidiacono peer effects ------------------------------------------------

####### Choose outcome variable
sample_df$y <- sample_df$log_citations
sample_df$y_i <- sample_df$y

############ First iteration of algorithm

####### First step : initialize the guess for author FE
formula_first_step <- paste0(' y_i ~ 1 ',
                             '| '
                             ,"author_id + merged_inst_id_field^year"
                             ,'+ type^year '
                             ,'+ cnrs^year'
                             ,'+ gender^year'
                             ,'+ city^year'
                             ,'+idex^year'
                             ,'+uni_pub^year'
                             ,'+acces_rce^year'
                             ,'+date_first_idex^year'
                             ,'+fusion_date^year'
                             ,'+ field^entry_year^year '
)

first_step <- feols(as.formula(formula_first_step)
                    ,data = sample_df
)
gc()

sample_df_peers <- sample_df %>%
  .[, alpha_hat := fixef(first_step)$author_id[author_id]]

####### Guess average peer FE from first guess
sample_df_peers <- sample_df_peers%>%
  .[, ":="( sum_alpha_hat = sum(alpha_hat, na.rm = TRUE),
            n_colab = n_distinct(author_id)
  ),
  by = c('merged_inst_id_field','year')] %>%
  .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
gc()
ggplot(unique(sample_df_peers[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))
ggplot(sample_df_peers)+geom_density(aes(x=avg_alpha_i_bar))
ggplot(sample_df_peers)+geom_density(aes(x=log(n_colab+1)))


####### Second step : Initialize the guess for peer effects
formula_second_step <- paste0('y~ avg_alpha_i_bar',
                              '| '
                              ,"author_id + merged_inst_id_field^year"
                              ,'+ type^year '
                              ,'+ cnrs^year'
                              ,'+ gender^year'
                              ,'+ city^year'
                              ,'+idex^year'
                              ,'+uni_pub^year'
                              ,'+acces_rce^year'
                              ,'+date_first_idex^year'
                              ,'+fusion_date^year'
                              ,'+ field^entry_year^year '
)


second_step <- feols(as.formula(formula_second_step)
                     ,data =sample_df_peers)
gc()

est_gamma <- as.data.table(second_step$coeftable)
est_gamma$i <- 1
est_gamma
est_diff_alpha <- data.table(NA,1)
names(est_diff_alpha) <- c('diff','i')

########## Loop for further iterations

for(i in 2:10){
  print(paste0('Process for i=', i))
  
  ######## Use estimate to adjust y for peer effects
  sample_df_peers <- sample_df_peers %>%
    .[, ':='(alpha_hat_i_minus_1 = alpha_hat,
             alpha_hat = fixef(second_step)$author_id[author_id],
             y_i = y- est_gamma[[i-1,1]]*avg_alpha_i_bar )]
  
  ### Look at convergence of author FE
  est_diff_i <- data.table(unique(sample_df[,list(author_id,alpha_hat_i_minus_1, alpha_hat)])[,.(mean((alpha_hat-alpha_hat_i_minus_1)**2, na.rm =T))][[1,1]],
                           i)
  colnames(est_diff_i) <- colnames(est_diff_alpha)
  est_diff_alpha<- rbind(est_diff_alpha, est_diff_i)
  
  ## Rerun first step
  first_step <- feols(as.formula(formula_first_step)
                      ,data = sample_df_peers
  )
  gc()
  
  sample_df_peers <- sample_df_peers %>%
    .[, alpha_hat := fixef(first_step)$author_id[author_id]]
  gc()
  
  sample_df_peers <- sample_df_peers %>%
    .[, ":="( sum_alpha_hat = sum(alpha_hat, na.rm = TRUE)
    ),
    by = c('merged_inst_id_field','year')] %>%
    .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
  ggplot(sample_df)+geom_density(aes(x=avg_alpha_i_bar))
  
  ### Rerun second step
  second_step <- feols(as.formula(formula_second_step)
                       ,data =sample_df_peers)
  gc()
  
  ### Save estimate value
  est_gamma_i <-  cbind(second_step$coeftable, i)
  colnames(est_gamma_i) <- colnames(est_gamma)
  est_gamma <-rbind(est_gamma,est_gamma_i)
  
}


###### Plot the convergence of estimators

cp <- ggplot(est_gamma)+
  geom_point(aes(x=as.factor(i), y = Estimate))+
  geom_line(aes (x=i, y = Estimate))+
  #geom_errorbar(aes(x=i, ymin = Estimate -1.96*`Std. Error`,
  #  ymax = Estimate + 1.96*`Std. Error`))+ylim(0.08, 0.18)+
  xlab('Number of iterations')+ylab('')+labs(title = 'Point estimate for peer effects')+
  theme_minimal()
cp

save_plot("E:\\panel_fr_res\\productivity_results\\convergence_estimate.png",cp)


cp_aufe <- ggplot(est_diff_alpha[i>1])+
  geom_point(aes(x=as.factor(i), y = log(diff)))+
  geom_line(aes(x=i-1, y=  log(diff)))+
  xlab('Number of iterations')+ylab('')+labs(title = 'Log sum of square differences compared to FE estimates from previous step')+
  theme_minimal()
cp_aufe
save_plot("E:\\panel_fr_res\\productivity_results\\convergence_fe.png",cp_aufe)


list_alpha_hat <- unique(sample_df_peers %>% .[, list(author_id)]) %>%
  .[,alpha_hat := fixef(second_step)$author_id[author_id]]
fwrite(list_alpha_hat, "E:\\panel_fr_res\\calibration\\list_alpha_hat.csv")



sample_df_peers <- sample_df %>%
  .[, alpha_hat := fixef(first_step)$author_id[author_id]]

####### Guess average peer FE from first guess
sample_df_peers <- sample_df_peers%>%
  .[, ":="( sum_alpha_hat = sum(alpha_hat, na.rm = TRUE),
            n_colab = n_distinct(author_id)
  ),
  by = c('merged_inst_id_field','year')] %>%
  .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ] %>%
  .[, y_i := log_citations - est_gamma[[8,1]]*avg_alpha_i_bar-alpha_hat ] %>%
  .[, Period :=case_when(year %in% 2003:2006 ~ "2003-2006",
                         year %in% 2016:2019 ~ "2016-2019",
                         .default = NA)]
  
gc()
fwrite(sample_df_peers, "E:\\panel_fr_res\\sample_df_peers.csv")
sample_df_peers <- fread("D:\\panel_fr_res\\sample_df_peers.csv")

to_plot <- unique(sample_df_peers[, list(author_id, Period, alpha_hat)]) %>%
  .[!is.na(Period)]

p <- ggplot(to_plot)+
  geom_density(aes(x=alpha_hat, color = Period))+  
  scale_color_manual(values = c('steelblue','firebrick'))+
  xlab('Researcher FE')+ylab('')+labs(title = 'Density for the period')+
  theme_minimal()
p
save_plot("E:\\panel_fr_res\\productivity_results\\density_alpha.png",p)

##### Estimating 



list_g = list( "acces_rce" = sort(unique(sample_df_peers[acces_rce !=0]$acces_rce))
               ,"date_first_idex" = sort(unique(sample_df_peers[date_first_idex !=0]$date_first_idex))
               , "fusion_date" = sort(unique(sample_df_peers[fusion_date !=0]$fusion_date))
)
gc()


formula_elements <- c()
for(d in c('acces_rce'#
           , 'date_first_idex', 'fusion_date'#,'rce_idex'
)){
  for(g_i in list_g[[d]]){
    print(paste0(d, ': ', g_i))
    varname =paste0(d, '_', g_i)
    #ref = as.character(as.numeric(g_i)-1)
    sample_df_peers[[varname]] <- as.numeric((sample_df_peers[[paste0(d)]] == g_i))
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ')'))
  }}  
length(formula_elements)

gc()
fe_large = paste0(  ' | merged_inst_id_field + '
                    ,'year '
                    ,'+ type^year '
                    ,'+ gender^year'
                    ,'+ public^year'
                    ,'+ ecole^year'
                    ,'+ cnrs^year'
                    ,'+ field^year'
                    ,'+ entry_year^year '
                    ,'+ city^year'
                    
)
gc()

formula_ctrl <- as.formula(paste0( 'y_i ~ ',  paste0(formula_elements, collapse= '+'), fe_large))
list_es <- list()
for(period in c("2003-2006", "2016-2019")){
  start_time <- Sys.time()
  es_stag_w_ctrl <- feols(formula_ctrl,
                           , data = sample_df_peers %>% .[Period == period] 
                           ,mem.clean = TRUE,fixef.tol = 1E-8
                           ,cluster = c('author_id','merged_inst_id_field')
  ) 
  time_taken <- Sys.time()-start_time
  gc()
  print(time_taken)
  list_es[[period]] <- es_stag_w_ctrl
}

saveRDS(list_es, file = "E:\\panel_fr_res\\productivity_results\\individual\\regressions_for_calibration.rds")
list_es <- readRDS("D:\\panel_fr_res\\productivity_results\\individual\\regressions_for_calibration.rds")

fe_distrib <- rbind(as.data.table(fixef( list_es[["2003-2006"]] )$merged_inst_id_field, keep.rownames = TRUE) %>%
                      .[, period := "2003-2006"],
                    as.data.table(fixef( list_es[["2016-2019"]] )$merged_inst_id_field, keep.rownames = TRUE) %>%
                    .[, period := "2016-2019"]
                    )
colnames(fe_distrib) <- c('merged_inst_id_field','lambda_hat','Period')

p <- ggplot(fe_distrib)+
  geom_density(aes(x=lambda_hat, color = Period))+  
  scale_color_manual(values = c('steelblue','firebrick'))+
  xlab('Institution X Field FE')+ylab('')+labs(title = 'Density for the period')+
  theme_minimal()
p
save_plot("E:\\panel_fr_res\\productivity_results\\density_lambda.png",p)

sample_for_calibration <- merge(sample_df_peers %>%
                                  .[, incumbent := fifelse(as.numeric(as.character(lag(year, order_by = year)))==
                                                                                     as.numeric(as.character(year))-1, 1, 0),
                                    by = c('merged_inst_id_field','author_id')] %>%
                                  .[, ":="(sum_alpha_hat_incumbents = sum( as.numeric(incumbent == 1)*alpha_hat, na.rm= T ),
                                           n_incumbents = sum(incumbent, na.rm = T)
                                           ), by = c('merged_inst_id_field','year')] %>%
                                  .[, avg_alpha_i_incumbents := sum_alpha_hat_incumbents/n_incumbents],
                        fe_distrib, by = c('merged_inst_id_field','Period'))


fwrite(sample_for_calibration, "E:\\panel_fr_res\\sample_for_calibration.csv")

sample_for_calibration <- fread("D:\\panel_fr_res\\sample_for_calibration.csv")

p <- ggplot(sample_for_calibration)+
  geom_density(aes(x=lambda_hat + avg_alpha_i_incumbents, color = Period))+  
  scale_color_manual(values = c('steelblue','firebrick'))+
  xlab('Institution X Field + Average Peer FE')+ylab('')+labs(title = 'Density for the period')+
  theme_minimal()
p
save_plot("E:\\panel_fr_res\\productivity_results\\density_lambda_plus_peers.png",p)


all_au <- unique(sample_for_calibration %>%
                   .[, list(author_id, gender, entry_year, field, x, Period)])%>%
  .[, year := ifelse(Period == "2003-2006", "2005", "2017")]
fwrite(all_au, "E:\\panel_fr_res\\calibration\\list_x.csv")


all_inst <- unique(as.data.table(sample_for_calibration %>%
                                   .[, y := mean(y, na.rm =T), by = c('Period', "merged_inst_id_field")] %>%
                                   select(merged_inst_id_field, type, public, ecole, cnrs, 
                                          city, Period, y, 
                                          contains('acces_rce'), contains('date_first_idex'), contains('fusion_date')) ))%>%
  .[, year := ifelse(Period == "2003-2006", "2005", "2017")]
fwrite(all_inst, "E:\\panel_fr_res\\calibration\\list_y.csv")

##### Get empirical match density
p_load("MASS","plotly",'mgcv')
sample_for_calibration <- sample_for_calibration %>%
  .[, ':='(y = lambda_hat + avg_alpha_i_incumbents
           ,x = alpha_hat) ]  

data_estimation <- sample_for_calibration[Period == "2003-2006"][!is.na(x) & !is.na(y)]

cutoffs_x <-  as.vector(quantile(data_estimation$x, c(0.01, 0.99)))
cutoffs_y <-  as.vector(quantile(data_estimation$y, c(0.01, 0.99)))

#data_estimation <- data_estimation[ (x>= cutoffs_x[1] & x<=cutoffs_x[2])
#                                    & (y>= cutoffs_y[1] & y<=cutoffs_y[2])]

ggplot(data_estimation)+geom_density(aes(x=log_citations, color = as.factor(public)))


model_03_06_private <- mgcv::bam(log_citations ~ te(x,y, k = c(20,20)),
                                 data = data_estimation[public == 0],
                                 method = "fREML",
                                 discrete = TRUE
                                 )
model_03_06_public <- mgcv::bam(log_citations ~  te(x,y, k = c(20,20)),
                                 data = data_estimation[public == 1],
                                 method = "fREML",
                                 discrete = TRUE
)

nx <- 100
ny <- 100
x_grid <- seq(min(data_estimation$x), max(data_estimation$x), length.out = nx)
y_grid <- seq(min(data_estimation$y), max(data_estimation$y), length.out = ny)

grid <- expand.grid(x = x_grid, y = y_grid)

grid$z_hat_private <- predict(model_03_06_private, newdata = grid)
grid$z_hat_public <- predict(model_03_06_public, newdata = grid)

# 3. Reshape predictions into matrix for plotly
z_mat_private <- matrix(grid$z_hat_private, nrow = nx, ncol = ny, byrow = FALSE)
z_mat_public <- matrix(grid$z_hat_public, nrow = nx, ncol = ny, byrow = FALSE)

# 4. Plot with plotly
fig <- plot_ly() %>%
  add_surface(
    x = x_grid,
    y = y_grid,
    z = z_mat_private,
    colorscale = "Blues",    # first color
    opacity    = 0.5,
    name       = "Model 1",
    showscale  = FALSE
  ) %>%
  add_surface(
    x = x_grid,
    y = y_grid,
    z = z_mat_public,
    colorscale = "Reds",     # second color
    opacity    = 0.5,
    name       = "Model 2",
    showscale  = FALSE
  ) %>%
  layout(
    legend = list(
      orientation = "h",
      x = 0.1,
      y = 1.05
    ),
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)")
    )
  )
fig
write.csv(
  data.frame(grid  ),
  "E:\\panel_fr_res\\calibration\\f_x_y_03_06.csv",
  row.names = FALSE
)


density_03_06_private <- kde2d(data_estimation[public == 0]$x, data_estimation[public == 0]$y, n = 100)
density_03_06_public <- kde2d(data_estimation[public == 1]$x, data_estimation[public == 1]$y, n = 100)

fig <- plot_ly() %>%
  add_surface(
    x = density_03_06_private$x,
    y = density_03_06_private$y,
    z = density_03_06_private$z,
    colorscale = "Blues",    # first color
    opacity    = 0.5,
    name       = "Model 1",
    showscale  = FALSE
  ) %>%
  add_surface(
    x = density_03_06_public$x,
    y = density_03_06_public$y,
    z = density_03_06_public$z,
    colorscale = "Reds",    # first color
    opacity    = 0.5,
    name       = "Model 1",
    showscale  = FALSE
  ) %>%
  layout(
    legend = list(
      orientation = "h",
      x = 0.1,
      y = 1.05
    ),
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)")
    )
  )
fig
density_03_06_private_to_save <- as.data.frame(density_03_06_private$z, keep.rownames =  TRUE) %>%
  mutate(x= row_number()) %>% 
  pivot_longer(cols = contains('V'), names_to = "y", names_prefix = "V", values_to = 'density_private') %>%
  mutate(y = as.integer(y))
density_03_06_private_to_save$x <- density_03_06_private$x[density_03_06_private_to_save$x]
density_03_06_private_to_save$y <- density_03_06_private$y[density_03_06_private_to_save$y]


write.csv(
  density_03_06_private_to_save,
  "D:\\panel_fr_res\\calibration\\density_03_06_private.csv",
  row.names = FALSE
)



density_03_06_public_to_save <- as.data.frame(density_03_06_public$z, keep.rownames =  TRUE) %>%
  mutate(x= row_number()) %>% 
  pivot_longer(cols = contains('V'), names_to = "y", names_prefix = "V", values_to = 'density_public') %>%
  mutate(y = as.integer(y))
density_03_06_public_to_save$x <- density_03_06_public$x[density_03_06_public_to_save$x]
density_03_06_public_to_save$y <- density_03_06_public$y[density_03_06_public_to_save$y]


write.csv(
  density_03_06_public_to_save,
  "D:\\panel_fr_res\\calibration\\density_03_06_public.csv",
  row.names = FALSE
)
  