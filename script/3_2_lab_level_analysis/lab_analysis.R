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


inputpath <- "D:\\panel_fr_res\\inst_level_flows.parquet"

ds <- as.data.table(open_dataset(inputpath))
gc()
ds <- ds %>%
  .[, type_r := first(ifelse(type_s != 'abroad' | inst_id_receiver == 'abroad', type_r, NA), na_rm = TRUE), by = 'inst_id_receiver'] %>%
  .[, ":="(uni_pub_r = pmax(uni_pub_r, universite_r), 
           uni_pub_s = pmax(uni_pub_s, universite_s))]
unique(ds[is.na(type_r)][, list(inst_id_receiver, name_r)])

ds[,.N, by = c('type_r','type_s','uni_pub_s','uni_pub_r')][order(-N)][N>100]

ds[,.N, by = c('type_fr_r','type_fr_s','uni_pub_s','uni_pub_r')][order(-N)][N>100]


cols_to_summarize <- colnames(ds)[str_detect(colnames(ds), 'total|stayer|mover')]

ggplot(ds%>%
         .[year >=1997] %>%
         .[year >=1997 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')
           & !(inst_id_sender == 'entrant' & inst_id_receiver == 'abroad')  ]%>%
         .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') | (!is.na(uni_pub_s) | inst_id_sender == 'abroad') &  
             (inst_id_sender!= 'entrant')] %>%
         .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
                  uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s)
         )]%>%
         # .[type_s == 'facility' & type_r == 'facility']%>%
         .[, lapply(.SD, sum, na.rm = T), by = c('uni_pub_r','uni_pub_s','year'),.SDcols = cols_to_summarize] %>%
         .[, move := case_when(uni_pub_r ==1 & uni_pub_s ==1 ~ 'Inside public universities',
                               uni_pub_r ==0 &  uni_pub_s ==1~ 'From university to outside',
                               uni_pub_r ==1 &  uni_pub_s ==0~ 'From outside to university',
                               uni_pub_r ==0 &  uni_pub_s ==0~ 'From outside to outside')]
       )+
  geom_line(aes(x=year,y=movers_w, color = move))

p <-ggplot(ds%>%
         .[!is.na(type_r) & year >=1997 &
             !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad') 
           & !(inst_id_sender== 'entrant' # & inst_id_receiver == 'abroad'
               ) 
           ] %>%
         .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') | (!is.na(uni_pub_s) | inst_id_sender == 'abroad')] %>%
         .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
                  uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s)
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


p <-ggplot(ds%>%
             .[!is.na(type_r) & year >=1997 
                & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad') 
               & !(inst_id_sender== 'entrant' & inst_id_receiver == 'abroad'
               ) 
             ] %>%
             .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') | (!is.na(uni_pub_s) | inst_id_sender == 'abroad')] %>%
             .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
                      uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s)
             )]%>%
             #.[type_s %in% c('facility','entrant') & type_r == 'facility']%>%
             .[, lapply(.SD, mean, na.rm = T), by = c('uni_pub_r','uni_pub_s','year'),.SDcols = cols_to_summarize] %>%
             .[, move := case_when(uni_pub_r ==1 & uni_pub_s ==1 ~ 'Inside public universities',
                                   uni_pub_r ==0 &  uni_pub_s ==1~ 'From university to outside',
                                   uni_pub_r ==1 &  uni_pub_s ==0~ 'From outside to university',
                                   uni_pub_r ==0 &  uni_pub_s ==0~ 'From outside to outside',
                                   .default = 'From outside to outside')]
)+
  geom_line(aes(x=year,y=movers_w_medium, color = move), linewidth = 0.5)+
  scale_color_manual(values = c('black','seagreen','firebrick','steelblue'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\avg_flows_abroad_entrant.png", p)



p <-ggplot(ds%>%
         .[year >=1997 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')
           & !(inst_id_sender== 'entrant' | inst_id_receiver == 'abroad'
           #    & inst_id_receiver == 'abroad'
           )] %>%
         .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') | (!is.na(uni_pub_s) | inst_id_sender == 'abroad')] %>%
         .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
                  uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s)
         )]%>%
          .[inst_id_sender == inst_id_receiver]%>%
         .[, move := case_when(uni_pub_r ==1 & uni_pub_s ==1 ~ 'Inside public universities',
                               uni_pub_r ==0 &  uni_pub_s ==0~ 'Outside public universities',
                               .default = 'With several affilitions')]%>%
         .[, lapply(.SD, sum, na.rm = T), by = c('move','year'),.SDcols = cols_to_summarize] 
)+
  geom_line(aes(x=year,y=total_w, color = move))+
  scale_color_manual(values = c('steelblue','black'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p
save_plot("E:\\panel_fr_res\\desc_stats\\total_authors.png", p)

    

test <- unique(ds%>%
  .[is.na(type_r)]%>%
  .[, list(inst_id_sender)]
)


cols_to_wins <- cols_to_summarize
reg_df <-  ds%>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins, by = c('type_r','type_s')]%>%
  .[, size_s:= ifelse(inst_id_sender == 'entrant', 
                      sum(total_w), size_s), by= c('inst_id_sender','year')]%>%
  .[year >=2000 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')] %>%
  .[inst_id_receiver != inst_id_sender] %>%
  #.[inst_id_sender!='abroad' & inst_id_receiver != 'abroad'
  #  
  # & type_r == 'facility' & type_s == 'facility'
  #  ] %>%
  .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') 
    & (!is.na(uni_pub_s) | inst_id_sender %in% c('abroad','entrant'))
    & !(inst_id_sender == 'entrant' & inst_id_receiver == 'abroad')
    ] %>%
  .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
           uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s),
           cnrs_r = fifelse(inst_id_receiver =="abroad", 0, cnrs_r),
           cnrs_s = fifelse(inst_id_sender %in% c("abroad",'entrant'), 0, cnrs_s),
           fused_r = ifelse(inst_id_receiver =="abroad", 0, fused_r),
           fused_s = ifelse(inst_id_sender %in% c("abroad",'entrant'), 0, fused_s),
           type_r = case_when(inst_id_receiver =='abroad'~ 'abroad', 
                              .default = type_r),
           type_s = case_when(inst_id_sender =='abroad' ~ 'abroad',
                              inst_id_sender == 'entrant' ~'entrant',
                              .default = type_s),
           city_r = case_when(inst_id_receiver =='abroad'~ 'abroad', 
                              .default = city_r),
           city_s = case_when(inst_id_sender =='abroad' ~ 'abroad',
                              inst_id_sender == 'entrant' ~'entrant',
                              .default = city_s)
           
           
  )] %>%
  .[, size_fe := n_distinct(paste0(inst_id_receiver, inst_id_sender)), by =c('uni_pub_r','uni_pub_s','type_r','type_s') ] %>%
  .[size_fe >=250 & size_r >=5 & size_s >=5] %>%
  .[ !is.na(type_s) & !is.na(type_r)
     & type_s %in% c('facility','abroad',"entrant"#,'government','company'
                   ) & type_r %in% c('facility','abroad'#,'government','company'
                                     )
    # &  (uni_pub_r + cnrs_r <2) &  (uni_pub_s + cnrs_s <2) 
    # &  !str_detect(parent_r, inst_id_sender) & !str_detect(parent_s, inst_id_receiver)
     ] %>%
  .[, entry_year_r := min(year), by = inst_id_receiver] %>%
  .[, entry_year_s := min(year), by = inst_id_sender] %>%
  .[, entry_year_pair := 
      ifelse(entry_year_r<=entry_year_s, entry_year_s, entry_year_r),  by = c('inst_id_sender','inst_id_receiver')] %>%
  .[, unit := paste0(inst_id_sender, '_to_',inst_id_receiver)] %>%
  .[, n_obs := n_distinct(year), by = unit]

nrow(unique(reg_df[, list(unit)]))

all_combinations <- CJ(
  unit = unique(reg_df$unit),
  year = seq(min(reg_df$year), max(reg_df$year))
)

reg_df <- reg_df[all_combinations, on = .(unit, year)]
setorder(reg_df, unit, year)

cols_to_fill_down_r <-c( "name_r","city_r","fused_r", "uni_pub_r"         
                      ,"cnrs_r","type_r","main_topic_r","topic_share_r","size_r",
                       "entry_year_r", "ecole_r",'universite_r','public_r','prive_r','idex_r'
                      )
cols_to_fill_down_s <-c(  "name_s","city_s","fused_s","uni_pub_s", "cnrs_s"            
                         ,"type_s","topic_share_s","size_s", "main_topic_s",   
                        "entry_year_s", "ecole_s",'universite_s','public_s','prive_s','idex_s'
                        )

unit_cols <- c("inst_id_sender", "inst_id_receiver",'entry_year_pair','n_obs')
cols_to_fill_0 <- cols_to_summarize
reg_df <- reg_df %>%
  .[, (unit_cols) := lapply(.SD, zoo::na.locf, na.rm = FALSE), 
    by = unit, .SDcols =unit_cols] %>%
  .[year >= entry_year_pair] %>%
  .[, (cols_to_fill_down_r) := lapply(.SD, zoo::na.locf), 
             by = inst_id_receiver, .SDcols = cols_to_fill_down_r] %>%
  .[, (cols_to_fill_down_s) := lapply(.SD, zoo::na.locf), 
    by = inst_id_sender, .SDcols = cols_to_fill_down_s] %>%
  .[, (cols_to_fill_0) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
    .SDcols = cols_to_fill_0]%>%
  .[, ':='(abroad_r = as.numeric(inst_id_receiver=='abroad'),
           abroad_s = as.numeric(inst_id_sender=='abroad'),
           type_r_year = paste0(type_r, '_',year),
           type_s_year = paste0(type_s, '_',year),
           main_topic_s = ifelse(inst_id_sender %in% c('abroad','entrant'), 'undefined', main_topic_s ),
           main_topic_r = ifelse(inst_id_receiver =='abroad', 'undefined', main_topic_r),
           type_s_type_r_year = paste0(type_s, '_',type_r, '_', year)
  )] %>%
  .[, ':='( has_idex_r = ifelse(!is.na(idex_r) & idex_r != 'no_idex' & !str_detect(idex_r, 'annulee'), 1, 0  ),
            has_idex_s = ifelse(!is.na(idex_s) & idex_s != 'no_idex' & !str_detect(idex_s, 'annulee'), 1, 0  ),
            idex_annulee_r = ifelse(!is.na(idex_r) & str_detect(idex_r, 'annulee'), 1, 0  ),
            idex_annulee_s = ifelse(!is.na(idex_s) & str_detect(idex_s, 'annulee'), 1, 0  )
  )]
rm(ds,all_combinations)
gc()

reg_df[, .(n_obs = n_distinct(paste0(inst_id_receiver, inst_id_sender)),
           n_moves = sum(movers_w)), by = c('uni_pub_r','uni_pub_s')]

reg_df[, .(n_obs = n_distinct(paste0(inst_id_receiver, inst_id_sender)),
           n_moves = sum(movers_w)), by = c('uni_pub_r','uni_pub_s','type_r','type_s')] %>%
  .[order(n_moves)]


reg_df[, .N, by = c('inst_id_sender','inst_id_receiver')][, .N, by = 'N'][order(N)]

#unique(reg_df[uni_pub_s == 0 & uni_pub_r == 0 & cnrs_s + cnrs_r >0][, list(name_r, cnrs_r, name_s,cnrs_s, parent_r, parent_s)])
list_feols <- list()
outcomes <- colnames(reg_df)[str_detect(colnames(reg_df), 'movers_w')]
for(var in outcomes){

test_feols <- feols(as.formula(paste0(var, ' ~ '
                    ,'  i(year, uni_pub_r,                                        2008) '
                    ,'+ i(year, uni_pub_s,                                        2008) '
                    ,'+ i(year, uni_pub_r*uni_pub_s,                              2008) '
                    ,'+ i(year, uni_pub_r*abroad_s,                               2008)'
                    ,'+ i(year, uni_pub_s*abroad_r,                               2008)'
                    ,'+ i(year, uni_pub_r*entrant,                                2008)'
                    ,'+ i(year, has_idex_r*(1-uni_pub_r),                         2008)'
                    ,'+ i(year, has_idex_s*(1-uni_pub_s),                         2008)'
                    ,'+ i(year, has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s),2008)'
                    ,'+ i(year, has_idex_r*(1-uni_pub_r)*abroad_s,                2008)'
                    ,'+ i(year, has_idex_s*(1-uni_pub_s)*abroad_r,                2008)'
                    ,'+ i(year, has_idex_r*uni_pub_r,                             2008)'
                    ,'+ i(year, has_idex_s*uni_pub_s,                             2008)'
                    ,'+ i(year, has_idex_r*uni_pub_r*has_idex_s*uni_pub_s,        2008)'
                    ,'+ i(year, has_idex_r*uni_pub_r*abroad_s,                    2008)'
                    ,'+ i(year, has_idex_s*uni_pub_s*abroad_r,                    2008)'
                    
                    ,"+ size_r*as.factor(year)+size_s*as.factor(year)"
                    ,"|" 
                  ,"    inst_id_receiver + inst_id_sender + year"
                  ,"  + inst_id_receiver^inst_id_sender"
                  ,"  + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year"
                  ,"  + fused_r^year + fused_s^year + fused_r^fused_s^year"
                  ,"  + ecole_r^year + ecole_s^year + ecole_r^ecole_s^year"
                  ,"   + type_r_year + type_s_year + type_s_type_r_year"
                  #,"   + public_r^year + public_s^year"
                  ,"   + main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year"
                  ))
                   #  +city_r^year + city_s^year + city_r^city_s^year
                    , data = reg_df %>%
                      .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)]
                   %>% .[year >= 2003 
                         &  type_r %in% c('facility','university','abroad') 
                                      & type_s %in% c('facility','entrant','abroad','university')
                         ]
                    #,weights = reg_df$size_r + reg_df$size_r
                    ,cluster = c('inst_id_sender','inst_id_receiver')
                    )
list_feols[[var]] <- test_feols
gc()

}

outcomes_dict <- c("movers_w"                  =     'Total flows',
                   "movers_w_junior"           =     'Junior researcher flows',
                   "movers_w_senior"           =     'Senior researcher flows',
                   "movers_w_medium"           =     'Medium researcher flows',
                   "movers_w_own_entrant_r"    =     'Returning to entry institution',
                   "movers_w_own_entrant_s"    =     'Exiting from entry institution',
                   "movers_w_foreign_entrant"  =     'Foreign entrant flows',
                   
                   "uni_pub_r"            =     ' into universities',
                   "uni_pub_s"            =     ' away from universities',
                   "uni_pub_r*uni_pub_s"  =     ' between two universities',
                   "uni_pub_r*abroad_s"   =     ' to universities from abroad',
                   "uni_pub_s*abroad_r"   =     ' abroad from universities',
                   "uni_pub_r*entrant"    =     ', entrants to universities',
                   
                   
                   "has_idex_r*(1-uni_pub_r)"                           =      ' into Idex',
                   "has_idex_s*(1-uni_pub_s)"                           =      ' away from Idex',
                   "has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s)"  =      ' between two Idex',   
                   "has_idex_r*(1-uni_pub_r)*abroad_s"                  =      ' to Idex from abroad',     
                   "has_idex_s*(1-uni_pub_s)*abroad_r"                  =      ' abroad from Idex',
                   
                   "has_idex_r*uni_pub_r"                        =      ' into public universities Idex',
                   "has_idex_s*uni_pub_s"                        =      ' away from public universities Idex',                          
                   "has_idex_r*uni_pub_r*has_idex_s*uni_pub_s"   =      ' between two public universities Idex',  
                   "has_idex_r*uni_pub_r*abroad_s"               =      ' to public universities Idex from abroad',                   
                   "has_idex_s*uni_pub_s*abroad_r"               =      ' abroad from idex public universities Idex'            
                   
)
for(var in names(list_feols)){

  var_path = paste0("E:\\panel_fr_res\\lab_results\\", var)
    if (!file.exists(var_path)){
      dir.create(var_path, recursive = TRUE)
    }
  formula_normalized = str_replace_all(as.character(list_feols[[var]]$fml)[3], '\\n|\\s', '')
  list_i_variables = str_extract_all(formula_normalized, pattern = '(?<=i\\(year\\,)[A-z\\s*\\(\\)\\d-]*(?=\\,)')[[1]]
  for(i_select in 1:str_count(formula_normalized, pattern = 'i\\(')){
    pdf(paste0(var_path, '\\', str_replace_all(list_i_variables[[i_select]], '\\W', '_')[[1]], "_facility_wUMR.pdf"))
    iplot(list_feols[[var]], i.select=i_select, 
                 main =  paste0(outcomes_dict[[var]],outcomes_dict[[list_i_variables[[i_select]]]])
                )
    dev.off()
  }
  
  
  
}



test_feols_fe <- fixef(test_feols)

###
test_feols_fe$`type_s_type_r_year`


pdf("E:\\panel_fr_res\\lab_results\\uni_pub_r_facility_wUMR.pdf")
iplot(test_feols, i.select=1, main = 'Universities as destination')
dev.off()
pdf("E:\\panel_fr_res\\lab_results\\uni_pub_s_facility_wUMR.pdf")
iplot(test_feols, i.select=2, main = 'Universities as origin')
dev.off()

pdf("E:\\panel_fr_res\\lab_results\\uni_pub_rs_facility_wUMR.pdf")
iplot(test_feols, i.select=3, main = 'Between-university flows')
dev.off()
pdf("E:\\panel_fr_res\\lab_results\\uni_pub_r_abroad_s_facility_wUMR.pdf")
iplot(test_feols, i.select=4, main = 'Flows to universities from abroad')
dev.off()

pdf("E:\\panel_fr_res\\lab_results\\uni_pub_s_abroad_r_facility_wUMR.pdf")
iplot(test_feols, i.select=5, main = 'Flows abroad from universities')
dev.off()


pdf("E:\\panel_fr_res\\lab_results\\uni_pub_r_entrant_s_facility_wUMR.pdf")
iplot(test_feols, i.select=6, main = 'Flow of entrants to universities')
dev.off()


test_agg <- feols(movers_w ~ 
                    i(post, uni_pub_r*(1-entrant),                             0)
                  + i(post, uni_pub_s,                                         0) 
                  + i(post, uni_pub_r*uni_pub_s,                               0) 
                  #+ i(post, uni_pub_r*abroad_s,                                0)
                  #+ i(post, uni_pub_s*abroad_r,                                0)
                  + i(post, uni_pub_r*entrant,                                 0)
                  + i(post, has_idex_r*(1-uni_pub_r),                          0)
                  + i(post, has_idex_s*(1-uni_pub_s),                          0)
                  + i(post, has_idex_r*(1-uni_pub_r)*has_idex_s*(1-uni_pub_s), 0)
                  + i(post, has_idex_r*(1-uni_pub_r)*abroad_s,                 0)
                  + i(post, has_idex_s*(1-uni_pub_s)*abroad_r,                 0)
                  + i(post, has_idex_r*(1-uni_pub_r)*entrant,                  0)
                  + i(post, has_idex_r*uni_pub_r,                              0)
                  + i(post, has_idex_s*uni_pub_s,                              0)
                  + i(post, has_idex_r*uni_pub_r*has_idex_s*uni_pub_s,         0)
                  + i(post, has_idex_r*uni_pub_r*abroad_s,                     0)
                  + i(post, has_idex_s*uni_pub_s*abroad_r,                     0)
                  + i(post, has_idex_r*uni_pub_r*entrant,                      0)
                  #+ size_r*as.factor(year)+size_s*as.factor(year)
                  | 
                    inst_id_receiver + inst_id_sender + year
                  + inst_id_receiver^inst_id_sender
                  #+ cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                  #+ fused_r^year + fused_s^year + fused_r^fused_s^year
                  #+ type_r_year + type_s_year + type_s_type_r_year
                  #+ main_topic_r^year + main_topic_s^year + main_topic_s^main_topic_r^year
                  , data = reg_df %>%
                    .[, entrant:=fifelse(inst_id_sender=='entrant', 1, 0)] %>%
                    .[, post := as.numeric(year >2008)]%>%
                    .[year >= 2003 &
                        type_r %in% c('facility') 
                      & type_s %in% c('facility','entrant')]
                  ,cluster = c('inst_id_sender','inst_id_receiver')
                  
                  )
etable(test_agg)
gc()


etable(test_agg, file = "E:\\panel_fr_res\\lab_results\\lab_mobility_agg")

  