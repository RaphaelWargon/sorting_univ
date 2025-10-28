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


# I) Loading data ------------------------------------------------------------



inputpath <- "E:\\panel_fr_res\\inst_level_flows.parquet"

ds <- open_dataset(inputpath) %>%
  filter(!is.na(type_r) & !is.na(type_s) & year >=2003) %>%
  group_by(merged_inst_id_r, merged_inst_id_s) %>%
  mutate(max_pair = max(total), entry_year_pair = min(year)) %>%
  ungroup() %>%
  filter(entry_year_pair <= 2003 & max_pair >0)
gc()
ds <- as.data.table(ds)
nrow(ds)


ds <- ds %>%
  .[,size_s := max(as.numeric(merged_inst_id_s == merged_inst_id_r)*stayers, na.rm = T), by = c('merged_inst_id_s','year')]%>%
  .[,size_r := max(as.numeric(merged_inst_id_s == merged_inst_id_r)*stayers, na.rm = T), by = c('merged_inst_id_r','year')] %>%
  .[, size_r := ifelse(size_r <= 0, sum(stayers_w), size_r),by = c('merged_inst_id_r','year') ]%>%
  .[, size_s := ifelse(size_s <= 0, sum(stayers_w), size_s),by = c('merged_inst_id_s','year') ] %>%
  .[!is.na(merged_inst_id_s) & !is.na(merged_inst_id_r)] %>%
  .[, unit := paste0(merged_inst_id_s, '_to_',merged_inst_id_r)] %>%
  .[, n_obs := n_distinct(year), by = unit] 
gc()


test <- ds %>% .[movers_w >1000]

cols_to_summarize <- colnames(ds)[str_detect(colnames(ds), 'total|stayer|mover')]

p <-ggplot(ds%>%
             .[#merged_inst_id_s != 'I1294671590' & merged_inst_id_r != "I1294671590"
               
               !(merged_inst_id_s == 'abroad'  & merged_inst_id_r == 'abroad') 
               & !(merged_inst_id_s== 'entrant' # & merged_inst_id_r == 'abroad'
               ) & merged_inst_id_r != merged_inst_id_s
             ] %>%
             .[, ':='(uni_pub_r = fifelse(is.na(acces_rce_r) | acces_rce_r == "0", 0, 1),
                      uni_pub_s = fifelse(is.na(acces_rce_s) | acces_rce_s == "0", 0, 1)
             )]%>%
             #.[type_s %in% c('facility','entrant') & type_r == 'facility']%>%
             .[, lapply(.SD, sum, na.rm = T), by = c('uni_pub_r','uni_pub_s','year'),.SDcols = cols_to_summarize] %>%
             .[, move := case_when(uni_pub_r ==1 & uni_pub_s ==1 ~ 'Inside public universities',
                                   uni_pub_r ==0 &  uni_pub_s ==1~ 'From university to outside',
                                   uni_pub_r ==1 &  uni_pub_s ==0~ 'From outside to university',
                                   uni_pub_r ==0 &  uni_pub_s ==0~ 'From outside to outside',
                                   .default = 'From outside to outside')]
)+
  geom_line(aes(x=year,y=movers_w, color = move), linewidth = 0.5)+
  scale_color_manual(values = c('black','seagreen','firebrick','steelblue'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\total_flows.png", p)


p <-ggplot(ds%>%
             .[#merged_inst_id_s != 'I1294671590' & merged_inst_id_r != "I1294671590"
               
               !(merged_inst_id_s == 'abroad'  & merged_inst_id_r == 'abroad') 
               & !(merged_inst_id_s== 'entrant' # & merged_inst_id_r == 'abroad'
               ) & merged_inst_id_r != merged_inst_id_s
             ] %>%
             .[, ':='(uni_pub_r = fifelse(is.na(acces_rce_r) | acces_rce_r == "0", 0, 1),
                      uni_pub_s = fifelse(is.na(acces_rce_s) | acces_rce_s == "0", 0, 1)
             )]%>%
             #.[type_s %in% c('facility','entrant') & type_r == 'facility']%>%
             .[, lapply(.SD, mean, na.rm = T), by = c('uni_pub_r','uni_pub_s','year'),.SDcols = cols_to_summarize] %>%
             .[, move := case_when(uni_pub_r ==1 & uni_pub_s ==1 ~ 'Inside public universities',
                                   uni_pub_r ==0 &  uni_pub_s ==1~ 'From university to outside',
                                   uni_pub_r ==1 &  uni_pub_s ==0~ 'From outside to university',
                                   uni_pub_r ==0 &  uni_pub_s ==0~ 'From outside to outside',
                                   .default = 'From outside to outside')]
)+
  geom_line(aes(x=year,y=movers_w, color = move), linewidth = 0.5)+
  scale_color_manual(values = c('black','seagreen','firebrick','steelblue'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\avg_flows.png", p)





# Preparing data ----------------------------------------------------------


ds_clean <- ds %>%
  .[, fusion_date_r := fifelse(fusion_date_r == "2023", "0", fusion_date_r)]%>%
  .[, fusion_date_s := fifelse(fusion_date_s == "2023", "0", fusion_date_s)] %>%
  .[, ':='(fusion_date_s = fifelse(is.na(fusion_date_s), "0", fusion_date_s),
           fusion_date_r = fifelse(is.na(fusion_date_r), "0", fusion_date_r),
           date_first_idex_s = fifelse(is.na(date_first_idex_s), "0", date_first_idex_s),
           date_first_idex_r = fifelse(is.na(date_first_idex_r), "0", date_first_idex_r),
           acces_rce_s = fifelse(is.na(acces_rce_s), '0', acces_rce_s),
           acces_rce_r = fifelse(is.na(acces_rce_r), '0', acces_rce_r),
           city_r = fifelse(merged_inst_id_r == 'abroad','abroad', city_r),
           city_s = fifelse(merged_inst_id_s == 'abroad','abroad', city_s),
           main_topic_r = fifelse(merged_inst_id_r == 'abroad','abroad', main_topic_r),
           main_topic_s = fifelse(merged_inst_id_r == 'abroad','abroad', main_topic_s),
           secteur_r = fifelse(merged_inst_id_r == 'abroad','abroad', secteur_r),
           secteur_s = fifelse(merged_inst_id_r == 'abroad','abroad', secteur_s),
           type_fr_r = fifelse(merged_inst_id_r == 'abroad','abroad', type_fr_r),
           type_fr_s = fifelse(merged_inst_id_r == 'abroad','abroad', type_fr_s)
  )]
gc()

list_g = list("acces_rce" = list( i = sort(unique(ds_clean[acces_rce_s !=0]$acces_rce_s)),
                                  j = sort(unique(ds_clean[acces_rce_r !=0]$acces_rce_r)))
              
              ,"date_first_idex" = list( i = sort(unique(ds_clean[date_first_idex_s !=0]$date_first_idex_s)),
                                         j =   sort(unique(ds_clean[date_first_idex_r !=0]$date_first_idex_r)))
              
              , "fusion_date" = list( i = sort(unique(ds_clean[fusion_date_s !=0]$fusion_date_s)),
                                      j = sort(unique(ds_clean[fusion_date_r !=0]$fusion_date_r)))
)
gc()
formula_elements <- c()
for(d in names(list_g) ){
  for(g_i in list_g[[d]][['i']]){
    print(paste0(d, ': ', g_i))
    varname =paste0(d, '_s_g', g_i)
    ref = as.character(as.numeric(g_i)-1)
    ds_clean[[varname]] <- as.numeric((ds_clean[[paste0(d, '_s')]] == g_i)
                                      & (ds_clean[['merged_inst_id_r']] != "abroad")
                                      )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))

    varname_abroad =paste0(d, '_a_s_g', g_i)
    ds_clean[[varname_abroad]] <- as.numeric((ds_clean[[paste0(d, '_s')]] == g_i)
                                      & (ds_clean[['merged_inst_id_r']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
  }
  for(g_j in list_g[[d]][['j']]){
    print(paste0(d, ': ', g_j))
    varname =paste0(d, '_r_g', g_j)
    ref = as.character(as.numeric(g_j)-1)
    ds_clean[[varname]] <- as.numeric((ds_clean[[paste0(d, '_r')]] == g_j)
                                      & (ds_clean[['merged_inst_id_s']] != "abroad")
                                      )
    formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
    
    varname_abroad =paste0(d, '_a_r_g', g_j)
    ds_clean[[varname_abroad]] <- as.numeric((ds_clean[[paste0(d, '_r')]] == g_j)
                                      & (ds_clean[['merged_inst_id_s']] == "abroad")
    )
    formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
    
  }
  
  
  }  
length(formula_elements)
gc()

rm(ds)
gc()
# Regression --------------------------------------------------------------
var <- "movers_w"

fe_min <- '| year + merged_inst_id_r  +merged_inst_id_s + unit'
start_time <- Sys.time()

es_stag <- fepois( as.formula(paste0(var, ' ~ ', paste0(formula_elements, collapse= '+'), fe_min))
                   , data = ds_clean
                 ,cluster = c('unit')
) 

time_taken <- Sys.time() - start_time
time_taken

agg_stag <- agg_etwfe(es_stag, data = ds_clean)

agg_effect_by_t <- agg_etwfe_het(es_stag, 't', data = ds_clean)
gc()

for(treat in unique(agg_effect_by_t$treatment)){
  p <- ggplot(agg_effect_by_t %>% .[treatment %in% c(treat) & t %in% -7:7])+
    geom_point(aes(x= t, y = est, shape = treatment))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std, linetype = treatment))+
    geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
    labs(title = paste0('Treatment: ', treat))+
    theme_bw()
  print(p)
}
gc()


