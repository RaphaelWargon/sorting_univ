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
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

inputpath <- "E:\\panel_fr_res\\panel_smoothed.parquet"

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
  .[ min_n_au_inst_id_field_y >1 & n_y_sample_inst >= 20] 

gc()

ggplot(unique(sample_df[, list(author_id, n_inst_y)]))+
  geom_density(aes(x=n_inst_y))

length(unique(sample_df$author_id)) #265916
nrow(unique(sample_df[, list(merged_inst_id, field)])) #2868
length(unique(sample_df$merged_inst_id)) #966

ggplot(sample_df %>% 
         .[, .(n =n_distinct(year)), by ='merged_inst_id'])+geom_histogram(aes(x= n) )


fwrite(sample_df, "E:\\panel_fr_res\\sample_df.csv")
# desc stats --------------------------------------------------------------


to_plot <- sample_df %>%
  .[, uni_pub := fifelse(acces_rce == '0', 0, 1) ] %>% 
  .[, .(citations = mean(citations, na.rm = T),
                         citations_raw = mean(citations_raw, na.rm = T),
                         publications = mean(publications_raw, na.rm=T), 
                         avg_rank_source_raw = mean(avg_rank_source_raw*publications_raw, na.rm=T), 
                         nr_source_top_5pct = mean(nr_source_top_5pct, na.rm=T))
                     ,by= c('uni_pub','year')]
ggplot(to_plot)+
  geom_line(aes(x=year, y= publications, color =interaction(uni_pub)))+
  scale_color_manual(values = c('seagreen','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average publications')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)

p <- ggplot(to_plot)+
  geom_line(aes(x=year, y= avg_rank_source_raw, color =interaction(uni_pub)), show.legend = FALSE)+
  scale_color_manual(values = c('black','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average journal-quality weighted publications')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)
p
save_plot("E:\\panel_fr_res\\desc_stats\\avg_rankw_pub.png", p)


p <- ggplot(to_plot)+
  geom_line(aes(x=year, y= log(citations), color =interaction(uni_pub)))+
  scale_color_manual(values = c('black','steelblue'))+
  theme_bw()+labs(title = '', color = 'Affected by LRU')+xlab('Year') + ylab('Average citations')+
  geom_vline(xintercept = 2007, linetype = 'dashed')+
  geom_vline(xintercept = 2009)
p
save_plot("E:\\panel_fr_res\\desc_stats\\avg_cit.png", p)

# regressions -------------------------------------------------------------

# Arcidiacono peer effects ------------------------------------------------

####### Choose outcome variable
sample_df$y <- sample_df$log_citations
sample_df$y_i <- sample_df$y

############ First iteration of algorithm

####### First step : initialize the guess for author FE
formula_first_step <- paste0(' y_i ~ 1 ',
                             '| '
                             ,"author_id + inst_id_field_year"
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
  by = c('inst_id_field_year')] %>%
  .[, avg_alpha_i_bar := (sum_alpha_hat-alpha_hat)/(n_colab-1) ]
gc()
ggplot(unique(sample_df_peers[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))
ggplot(sample_df_peers)+geom_density(aes(x=avg_alpha_i_bar))
ggplot(sample_df_peers)+geom_density(aes(x=log(n_colab+1)))


####### Second step : Initialize the guess for peer effects
formula_second_step <- paste0('y~ avg_alpha_i_bar',
                              '| '
                              ,"author_id + inst_id_field_year"
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

for(i in 2:5){
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
    .[, ":="( sum_alpha_hat = sum(alpha_hat, na.rm = TRUE),
              n_colab = n_distinct(author_id)
    ),
    by = c('inst_id_field','year')] %>%
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
  xlab('Number of iterations')+ylab('')+
  theme_minimal()
cp

save_plot("E:\\panel_fr_res\\productivity_results\\convergence_estimate.png",cp)


cp_aufe <- ggplot(est_diff_alpha[i>1])+
  geom_point(aes(x=as.factor(i), y = diff))+
  geom_line(aes(x=i-1, y= diff))+
  xlab('Number of iterations')+ylab('')+
  theme_minimal()
cp_aufe
save_plot("E:\\panel_fr_res\\productivity_results\\convergence_fe.png",cp_aufe)

sample_df_peers <- sample_df_peers %>%
  .[year >=2003] %>%
  .[, ':='(alpha_hat_i_minus_1 = alpha_hat,
           alpha_hat = fixef(second_step)$author_id[author_id],
           y_i = log_citations- est_gamma[[5,1]]*avg_alpha_i_bar- alpha_hat )] 

fwrite(sample_df_peers, "E:\\panel_fr_res\\sample_df_reg.csv")