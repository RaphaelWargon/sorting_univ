rm(list = ls())
gc()
#install.packages('devtools')
library('pacman')

install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')
#install.packages("DIDmultiplegt", force = TRUE)
#install.packages("DIDmultiplegtDYN", force = TRUE)
#devtools::install_github("CdfInnovLab/didImputation", force = TRUE)


library("didImputation")

p_load('arrow'
,'data.table'
,'fixest'
,'tidyverse'
,'dplyr','magrittr','tidyr','ggplot2'
,'binsreg',
'DescTools',
'cowplot',
'DIDmultiplegt',
"DIDmultiplegtDYN"#,'didimputation'
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

setwd('U')
inputpath <- "panel_smoothed.parquet"

inputpath <- "E:\\panel_fr_res\\panel_smoothed.parquet"

#inputpath <- "C:\\Users\\rapha\\Desktop\\panel_smoothed.parquet"
#
#ds <- open_dataset(inputpath) 
#ds$schema$names

ds <- open_dataset(inputpath) %>%
  filter(#all_y_in_FR >= (last_year-entry_year +1)/4
          last_year-entry_year >2
         & entry_year >=1965 
         & year >= 1997
         & entry_year >=1965 
         #& entry_year <=2003
         & year >= 1997
         & citations >0
         #& inst_type %in% c('facility','education')
         #& country == 'FR'
         #& !(inst_id %in% inst_to_exclude) 
         & !(is.na(field))
         & !(inst_id %in% c('archive',"other"))
         &!is.na(city) & !is.na(cnrs) & !is.na(type) & !is.na(uni_pub) & !is.na(idex)
  ) %>%
  select(author_id, author_name, gender, year,
         entry_year,
         inst_id, name, type, field, subfield, domain, entry_country,
         publications_raw,citations_raw,  
         publications, citations,
         avg_rank_source_raw,nr_source_btm_50pct_raw,
         nr_source_mid_40pct_raw, nr_source_top_20pct_raw,nr_source_top_10pct_raw,nr_source_top_5pct_raw,
         avg_rank_source,nr_source_btm_50pct,
         nr_source_mid_40pct, nr_source_top_20pct,nr_source_top_10pct,nr_source_top_5pct,
         period_inst, uni_pub, cnrs,type, acces_rce,
         idex, first_y_inst_period, date_first_idex,
         fused_inst_id, fusion_date,
         main_topic, city, prive, public, ecole, 
         n_inst_y, first_y_inst_period, 
        # n_phd_students, in_supervisor_inst, 
        # in_referee_inst,in_jury_inst, thesis_year #, inst_set_this_year
         )
ds <- as.data.table(ds)

ds <- unique(ds)

ggplot(ds %>%
         .[, .(N= n_distinct(author_id)), by = "entry_year"])+geom_col(aes(x= entry_year, y = N) )
ggplot(ds %>%
         .[, .(N= n_distinct(author_id)), by = c('year',"entry_year")])+
  geom_col(aes(x= year, y = N, fill = entry_year) )

#nrow(unique(ds[,list(author_id)])) #313767

gc()

##
#inst_to_exclude <- c('I4210159570')
cols_to_wins <- colnames(ds)[4:19]
sample_df <- ds %>%
  
   #### Checking that there are enough observations for each individual or author :
  .[, ':='(n_inst_id_sample = n_distinct(inst_id)), by = 'author_id'] %>%
  .[, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
           n_authors_sample = n_distinct(author_id),
           n_y_sample_inst = n_distinct(year)), by = c('inst_id','field')]%>%
  .[, n_y_in_sample_au := n_distinct(year), by = 'author_id']%>%
  .[ n_authors_w_several_inst > 0 # ensure that the author and firm are part of the connected set
     & n_y_in_sample_au >=2 & n_authors_sample >1 & n_y_sample_inst>1] %>% #remove labs that are too poorly measured
  
  ##### recode main field for wrong affiliation
  .[,max_field := dplyr::first(ifelse(n_authors_sample == max(n_authors_sample* as.numeric(!is.na(field)) ) & !is.na(field), field, NA), na_rm = T),
    by = "inst_id"] %>%
  .[,field_value := n_authors_sample/n_distinct(author_id),by = 'inst_id']%>%
  .[,max_field_value := max(field_value), by = 'inst_id'] %>%
  .[,field_recoded := ifelse( ((field_value <0.1| n_authors_sample<=5) ) | is.na(field), max_field, field)]%>%
  .[, field := field_recoded]%>%
  .[,inst_id_field := paste0(inst_id, field)] %>%
  
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
  .[, inst_id_field_year := paste0(inst_id_field,'_',year)]%>%
  .[, ':='(n_obs_au = .N), by = "author_id"]%>%
  .[n_obs_au >1] %>%
  .[, ':='(n_au_inst_id_field_y = .N), by = c('inst_id_field_year')] %>%
  .[, min_n_au_inst_id_field_y := min(n_au_inst_id_field_y), by = 'inst_id_field']%>%
  .[ min_n_au_inst_id_field_y >1]

gc()

length(unique(sample_df$author_id)) #290305
length(unique(sample_df$inst_id_field)) #2545
length(unique(sample_df$inst_id)) #1739

ggplot(sample_df %>% 
         .[, .(n =n_distinct(year)), by ='inst_id'])+geom_histogram(aes(x= n) )


# desc stats --------------------------------------------------------------


to_plot <- sample_df[, .(citations = mean(citations, na.rm = T),
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

fwrite(sample_df_peers, "E:\\panel_fr_res\\sample_df_reg.csv")
gc()

# Staggered design regression to estimate treatment effects ---------------

####### Prepare dataset for staggered design regression

sample_df_peers <- sample_df_peers %>%
  .[year >=2003] %>%
  .[, ':='(alpha_hat_i_minus_1 = alpha_hat,
           alpha_hat = fixef(second_step)$author_id[author_id],
           y_i = log_citations- est_gamma[[5,1]]*avg_alpha_i_bar- alpha_hat )] 

table(unique(sample_df_peers[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df_peers[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df_peers[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$fusion_date)



sample_df_reg <- sample_df_peers %>%
  .[!(acces_rce %in%  c(2015))
    & !(date_first_idex %in% c(2014))
  #  & !(fusion_date %in% c(2011:2015))
  ]#%>% # Keep only treatment values for which there are enough observations
  #.[n_y_sample_inst >=20]
table(unique(sample_df_reg[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$acces_rce)
table(unique(sample_df_reg[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$date_first_idex)
table(unique(sample_df_reg[, list(inst_id_field, acces_rce,date_first_idex,fusion_date)])$fusion_date)


ggplot(unique(sample_df_reg[, .N, by = c('acces_rce','year')][acces_rce != '0']))+
  geom_line(aes(x=year, y= N, color = acces_rce))
ggplot(unique(sample_df_reg[, .N, by = c('date_first_idex','year')][date_first_idex != '0']))+
  geom_line(aes(x=year, y= N, color = as.factor(date_first_idex)))
ggplot(unique(sample_df_reg[, .N, by = c('acces_rce','date_first_idex','year')][date_first_idex != '0']))+
  geom_line(aes(x=year, y= N, color = interaction(acces_rce,date_first_idex)))

ggplot(unique(sample_df[, list(author_id, alpha_hat)]))+geom_density(aes(x=alpha_hat))
gc()


list_g = list( "acces_rce" = sort(unique(sample_df_reg[acces_rce !=0]$acces_rce))
              ,"date_first_idex" = sort(unique(sample_df_reg[date_first_idex !=0]$date_first_idex))
              , "fusion_date" = sort(unique(sample_df_reg[fusion_date !=0]$fusion_date))
)
gc()
treatvars <- c()
for(d in c('acces_rce'#
           , 'date_first_idex', 'fusion_date'#,'rce_idex'
)){
  for(g_i in list_g[[d]]){
    print(paste0(d, ': ', g_i))
    for(y in unique(sample_df_reg$year)){
      varname = paste0('D_i_', d, '_g', as.character(g_i), '_y', as.character(y))
      sample_df_reg[[varname]] <- as.numeric( (sample_df_reg[[paste0(d)]] == g_i)
                                          & (sample_df_reg[["year"]] == y))
      treatvars <- c(treatvars, varname)
    }
  }
}  
length(treatvars)

var  = 'y_i'
formula_event_study_stag <- paste0('~', paste0(treatvars, collapse = "+"))
fe_min = ' | year'
fe_large = paste0(  ' |'
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
ggplot(sample_df_reg)+geom_density(aes(x=y_i))

start_time <- Sys.time()
es_stag <- feols(as.formula(paste0( "y_i",
                                   formula_event_study_stag))
                 , data = sample_df_reg
                 ,cluster = c('inst_id','author_id')
) 
time_taken <- Sys.time() - start_time
time_taken
gc()
all_coefs <- as.data.table(es_stag$coeftable, keep.rownames = TRUE)
all_coefs <- all_coefs %>%
  .[, d := str_extract(rn, '(?<=i_|j_)[a-z_]*(?=_g)')] %>%
  .[, type := case_when(str_detect(rn, '_i_') ~ 'delta',
                        str_detect(rn, 'j')~'gamma',
                        .default = '')] %>%
  .[, g := str_extract(rn, '(?<=g)[0-9]{1,4}')] %>%
  .[, year := str_extract(rn, '(?<=y)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] %>%
  .[, treat := ifelse(type!='',
                      paste0(type, '_', d), d)]

w <- sample_df_reg %>%
  .[, .(n  = .N), by = c("acces_rce", "year") ] %>% 
  .[,year := as.character(year)]
colnames(w) <- c('g','year', 'n')
all_coefs <- merge(all_coefs, w, by= c('g','year'))
ggplot(all_coefs %>% .[ abs(t)<=7])+
  geom_point(aes(x= t, y = Estimate, color = g))+
  geom_errorbar(aes(x=t, ymin = Estimate -1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`, color = g))+
  geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
  theme_bw()

ggplot(all_coefs) +geom_line(aes(x=n,y=`Std. Error`))

agg_stag <- agg_effects(es_stag, sample_df)
agg_stag
agg_stag_by_t <- agg_effect_het(es_stag, sample_df, by  ='t')


for(treat in unique(agg_stag_by_t$treatment)){
  p <- ggplot(agg_stag_by_t %>% .[treatment %in% c(treat) & abs(t)<=7])+
    geom_point(aes(x= t, y = est, shape = treatment))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std, linetype = treatment))+
    geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
    labs(title = paste0('Treatment: ', treat))+
    theme_bw()
  print(p)
}
gc()
agg_stag_by_g <- agg_effect_het(es_stag, sample_df, by  ='g')

for(treat in unique(agg_stag_by_g$treatment)){
  p <- ggplot(agg_stag_by_g %>% .[treatment %in% c(treat)])+
    geom_point(aes(x= g, y = est))+
    geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
    geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
    labs(title = paste0('Treatment: ', treat))+
    theme_bw()
  print(p)
}


start_time <- Sys.time()
es_stag_w_ctrl <- feols(as.formula(paste0(var, formula_event_study_stag, fe_large))
                      , data = sample_df_reg
                      ,cluster = c('author_id','inst_id')
) 
time_taken <- Sys.time()-start_time
gc()
time_taken

agg_stag <- agg_effects(es_stag_w_ctrl, sample_df)
agg_stag
agg_stag_by_t <- agg_effect_het(es_stag_w_ctrl, sample_df, by  ='t')


for(treat in unique(agg_stag_by_t$treatment)){
  p <- ggplot(agg_stag_by_t %>% .[treatment %in% c(treat) & t %in% -7:7])+
    geom_point(aes(x= t, y = est, shape = treatment))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std, linetype = treatment))+
    geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
    labs(title = paste0('Treatment: ', treat))+
    theme_bw()
  print(p)
}
gc()
agg_stag_by_g <- agg_effect_het(es_stag_w_ctrl, sample_df, by  ='g')

for(treat in unique(agg_stag_by_g$treatment)){
  p <- ggplot(agg_stag_by_g %>% .[treatment %in% c(treat)])+
    geom_point(aes(x= g, y = est))+
    geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std))+
    geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
    labs(title = paste0('Treatment: ', treat))+
    theme_bw()
  print(p)
}


all_coefs <- as.data.table(es_stag_w_ctrl$coeftable, keep.rownames = TRUE)
all_coefs <- all_coefs %>%
  .[, d := str_extract(rn, '(?<=i_|j_)[a-z_]*(?=_g)')] %>%
  .[, type :="beta"] %>%
  .[, g := str_extract(rn, '(?<=g)[0-9]{1,4}')] %>%
  .[, year := str_extract(rn, '(?<=y)[0-9]{4}')] %>%
  .[, t := as.numeric(year)-as.numeric(g)] %>%
  .[, treat := ifelse(type!='',
                      paste0(type, '_', d), d)]

ggplot(all_coefs %>% .[ treat == 'beta_fusion_date' & abs(t)<=7])+
  geom_point(aes(x= t, y = Estimate, color = g))+
  geom_errorbar(aes(x=t, ymin = Estimate -1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`, color = g))+
  geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
  theme_bw()
for(g_ in unique(all_coefs[treat == 'beta_date_first_idex']$g) ){
  print(ggplot(all_coefs %>% .[ treat == 'beta_date_first_idex' & abs(t)<=7 & g == g_])+
          geom_point(aes(x= t, y = Estimate, color = g))+
          geom_errorbar(aes(x=t, ymin = Estimate -1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`, color = g))+
          geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
          theme_bw())
}
}for(g_ in unique(all_coefs[treat == 'beta_acces_rce']$g) ){
print(ggplot(all_coefs %>% .[ treat == 'beta_acces_rce' & abs(t)<=7 & g == g_])+
  geom_point(aes(x= t, y = Estimate, color = g))+
  geom_errorbar(aes(x=t, ymin = Estimate -1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`, color = g))+
  geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
  theme_bw())

}

p_load("JuliaConnectoR",'dqrng','gtools')
library(fwildclusterboot)
set.seed(123)
dqrng::dqset.seed(123)
bootstrap_errors <- boottest(es_stag_w_ctrl, clustid = c('inst_id','author_id'),B =999,
                             #fe = as.formula('~ inst_id_field'),
                             param = formula_event_study_stag,nthreads = 1, engine = "WildBootTests.jl") 




pdf("D:\\panel_fr_res\\productivity_results\\effect_uni_pub_citations.pdf")
iplot(test_brutal, main = 'Effect of being in a PU on log citations')
dev.off()
pdf("D:\\panel_fr_res\\productivity_results\\effect_idex_citations.pdf")
iplot(test_brutal,i.select = 2, main = 'Effect of being in an IDEX-receiving institution on log citations')
dev.off()
pdf("D:\\panel_fr_res\\productivity_results\\effect_idex_uni_pub_citations.pdf")
iplot(test_brutal,i.select = 3, main = 'Effect of being in an IDEX-receiving PU on log citations')
dev.off()
pdf("D:\\panel_fr_res\\productivity_results\\effect_private_citations.pdf")
iplot(test_brutal,i.select = 4, main = 'Effect of being in a private institution on log citations')
dev.off()


summary(test_brutal)

#
#formula_agg <- paste0('c(publications_raw, avg_rank_source_raw*publications_raw, citations_raw, nr_source_top_5pct_raw)',
#                  '~ 1 + i(post, uni_pub, 0)| ',
#                  ' author_id + inst_id_field '
#                  ,'+ inst_type^year '
#                  ,'+ cnrs^year'
#                  ,'+ fused^year'
#                  ,'+ field^year '
#                  ,'+entry_cohort^year'
#)
#
formula_agg <- str_replace_all(formula, '\\(year','\\(post')
formula_agg <- str_replace_all(formula_agg, '\\d{4}\\)','0\\)')

agg_prod <- feols(as.formula(formula_agg)
                  ,data = sample_df %>%
                    .[,post := as.numeric(year >=2010)])
gc()
etable(agg_prod)
etable(agg_prod,drop = c('alpha_hat'),
       file = "D:\\panel_fr_res\\productivity_results\\agg_prod.tex", replace=TRUE)

formula <- paste0('y_final~ 1',
                  '+ entry_abroad*(',
                  '+ i(post, uni_pub,               0)',
                  '+ i(post, has_idex*uni_pub,      0)',
                  '+ i(post, has_idex*(1-uni_pub),  0)',
                  '+ i(post, prive,                 0)',
                  ')',
                  '+ alpha_hat',
                  '|',
                  ' inst_id_field + year '
                  ,'+ type^year '
                  ,'+ ecole^year'
                  ,'+ cnrs^year'
                  ,'+ fused^year'
                  ,'+ field^entry_year^year '
                  ,'+ city^year'
)


agg_prod <- feols(as.formula(formula)
                     ,data = sample_df %>%
                       .[, has_idex := ifelse(!is.na(idex) & idex != 'no_idex' & !str_detect(idex, 'annulee'), 1, 0  )] %>%
                       .[, y_final := log(citations)- est_gamma[[5,1]]*avg_alpha_i_bar ]%>%
                       .[entry_year <= 2003 & year > 2003] %>%
                       .[, entry_abroad := as.numeric(!str_detect(entry_country, 'FR'))]%>%
                       .[, post := as.numeric(year >= 2007)]
)
gc()
etable(agg_prod,drop = c('alpha_hat'),
       file = "D:\\panel_fr_res\\productivity_results\\agg_prod_étranger.tex", replace=TRUE)


etable(agg_prod)


fixef_brutal <- fixef(test_brutal#, fixef.iter =  5000
)
gc()


fixef_ds_inst_akm <-as.data.table(list(names(fixef_brutal$inst_id_field),fixef_brutal$inst_id_field))
colnames(fixef_ds_inst_akm) <- c('inst_id_field','fixef_inst_akm')
fixef_ds_inst_akm <- fixef_ds_inst_akm[, ":="(rank_inst_akm =frank(fixef_inst_akm))]
ggplot(fixef_ds_inst_akm)+geom_line(aes(x=rank_inst_akm,y=fixef_inst_akm))
ggplot(fixef_ds_inst_akm)+geom_density(aes(x=fixef_inst_akm))

fixef_ds_inst <- merge(fixef_ds_inst_akm[, ":="(rank_inst_akm =frank(fixef_inst_akm))],
                       unique(sample_df[, list(inst_id_field, name,n_obs_univ)]), by = 'inst_id_field')
test <- fixef_ds_inst[str_detect(inst_id_field, 'econ')][, 
                                                                           rank_inst_akm := rank_inst_akm/max(rank_inst_akm, na.rm = T)]

test[inst_id_field %in% c("I57995698econ", 'I2802331213econ',
                          "I4210092408econ",'I4210144888econ')]
unique(sample_df[, list(author_id,author_name, field, n_obs_au,alpha_hat)])[author_name=='Philippe Aghion']
unique(sample_df[, list(author_id,author_name, field, n_obs_au,alpha_hat)])[author_name=='Stanislas Dehaene']

test2 <- unique(sample_df[, list(author_id,author_name, field, n_obs_au,alpha_hat)])[str_detect(field, 'econ')][, ":="(rank_au_akm =frank(alpha_hat))][, 
                                                     rank_au_akm := rank_au_akm/max(rank_au_akm, na.rm = T)]
test2[author_name=='Philippe Aghion']


sample_df <- merge(sample_df, fixef_ds_inst_akm, by ='inst_id_field', all.x = T) 

rank_au <- unique(sample_df[, list(author_id, alpha_hat)]) %>%
  .[, rank_au_akm := frank(alpha_hat)]
sample_df <- merge(sample_df, rank_au[, list(author_id, rank_au_akm)], by ='author_id', all.x = T) 

sample_df <- sample_df %>%
  .[, rank_colab_akm:=frank(avg_alpha_i_bar) ]

# Save results ------------------------------------------------------------
fwrite(sample_df, "D:\\panel_fr_res\\test_with_fixed_effects.csv")
gc()
