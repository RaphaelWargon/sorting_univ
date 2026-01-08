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

#source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), 'etwfe_functions.R'))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/etwfe_functions.R'))


inputpath <- "D:\\panel_fr_res\\inst_level_flows.parquet"

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
ds <- ds %>%
  .[, fusion_date_r := fifelse(fusion_date_r == "2023", "0", as.character(fusion_date_r) )]%>%
  .[, fusion_date_s := fifelse(fusion_date_s == "2023", "0", as.character(fusion_date_s) )] %>%
  .[, ':='(fusion_date_s = fifelse(is.na(fusion_date_s), "0", as.character(fusion_date_s) ),
           fusion_date_r = fifelse(is.na(fusion_date_r), "0", as.character(fusion_date_r) ),
           date_first_idex_s = fifelse(is.na(date_first_idex_s), "0", as.character(date_first_idex_s) ),
           date_first_idex_r = fifelse(is.na(date_first_idex_r), "0", as.character(date_first_idex_r) ),
           acces_rce_s = fifelse(is.na(acces_rce_s), '0', as.character(acces_rce_s) ),
           acces_rce_r = fifelse(is.na(acces_rce_r), '0', as.character(acces_rce_r) ),
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



cols_to_summarize <- colnames(ds)[str_detect(colnames(ds), 'total|stayer|mover')]

p <-ggplot(ds%>%
             .[#merged_inst_id_s != 'I1294671590' & merged_inst_id_r != "I1294671590"
               
               !merged_inst_id_s == 'abroad'  & !merged_inst_id_r == 'abroad'
               & !(merged_inst_id_s== 'entrant' # & merged_inst_id_r == 'abroad'
               ) & merged_inst_id_r != merged_inst_id_s
             ] %>%
             .[, ':='(any_treatment_r = fifelse(is.na(acces_rce_r) | acces_rce_r == "0"
                                                &is.na(date_first_idex_r) | date_first_idex_r == "0"
                                                &is.na(fusion_date_r) | fusion_date_r == "0", 0, 1),
                      any_treatment_s = fifelse(is.na(acces_rce_s) | acces_rce_s == "0"
                                                &is.na(date_first_idex_s) | date_first_idex_s == "0"
                                                &is.na(fusion_date_s) | fusion_date_s == "0", 0, 1)
             )]%>%
             #.[type_s %in% c('facility','entrant') & type_r == 'facility']%>%
             .[, lapply(.SD, sum, na.rm = T), by = c('any_treatment_r','any_treatment_s','year'),.SDcols = cols_to_summarize] %>%
             .[, move := case_when(any_treatment_r ==1 &  any_treatment_s ==1 ~ 'Between treated institutions',
                                   any_treatment_r ==0 &  any_treatment_s ==1~ 'From treated to control',
                                   any_treatment_r ==1 &  any_treatment_s ==0~ 'From control to treated',
                                   any_treatment_r ==0 &  any_treatment_s ==0~ 'From control to control',
                                   .default = 'From outside to outside')]
)+
  geom_line(aes(x=year,y=movers_w, color = move), linewidth = 0.5)+
  scale_color_manual(values = c("steelblue",'black','seagreen','firebrick'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("D:\\panel_fr_res\\desc_stats\\total_flows.png", p)


p <-ggplot(ds%>%
             .[#merged_inst_id_s != 'I1294671590' & merged_inst_id_r != "I1294671590"
               
               !(merged_inst_id_s == 'abroad'  & merged_inst_id_r == 'abroad') 
               & !(merged_inst_id_s== 'entrant' # & merged_inst_id_r == 'abroad'
               ) & merged_inst_id_r != merged_inst_id_s
             ] %>%
             .[, ':='(any_treatment_r = fifelse(is.na(acces_rce_r) | acces_rce_r == "0"
                                                &is.na(date_first_idex_r) | date_first_idex_r == "0"
                                                &is.na(fusion_date_r) | fusion_date_r == "0", 0, 1),
                      any_treatment_s = fifelse(is.na(acces_rce_s) | acces_rce_s == "0"
                                                &is.na(date_first_idex_s) | date_first_idex_s == "0"
                                                &is.na(fusion_date_s) | fusion_date_s == "0", 0, 1)
             )]%>%
             #.[type_s %in% c('facility','entrant') & type_r == 'facility']%>%
             .[, lapply(.SD, mean, na.rm = T), by = c('any_treatment_r','any_treatment_s','year'),.SDcols = cols_to_summarize] %>%
             .[, move := case_when(any_treatment_r ==1 &  any_treatment_s ==1 ~ 'Between treated institutions',
                                   any_treatment_r ==0 &  any_treatment_s ==1~ 'From treated to control',
                                   any_treatment_r ==1 &  any_treatment_s ==0~ 'From control to treated',
                                   any_treatment_r ==0 &  any_treatment_s ==0~ 'From control to control',
                                   .default = 'From outside to outside')]
)+
  geom_line(aes(x=year,y=movers_w, color = move), linewidth = 0.5)+
  scale_color_manual(values = c("steelblue",'black','seagreen','firebrick'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("D:\\panel_fr_res\\desc_stats\\avg_flows.png", p)


p <- ggplot(ds %>% 
              .[merged_inst_id_r != 'abroad' & year < 2020] %>%
              .[merged_inst_id_r == merged_inst_id_s] %>%
              .[, any_treatment := fifelse(acces_rce_r == "0" & date_first_idex_r == "0"
                                           &fusion_date_r == '0', "Control","Treated")] %>%
              .[, .(N = sum(total_w, na.rm =T)) ,by = c('any_treatment','year')]
)+
  geom_line(aes(x=year,y=N, color = any_treatment, group = any_treatment), linewidth = 0.5)+
  scale_color_manual(values = c("black","steelblue"))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')+ylim(c(0, 47000))
p

save_plot("E:\\panel_fr_res\\desc_stats\\total_authors.png", p)



# Descriptive tables ------------------------------------------------------

desc_stats_nobs <- ds %>%
  .[, post:= as.numeric(year >= 2008)] %>%
  .[, ':='(any_treatment = fifelse( (is.na(acces_rce_r) | acces_rce_r == "0") 
                                    & (is.na(date_first_idex_r) | date_first_idex_r == "0")
                                    & (is.na(fusion_date_r) | fusion_date_r == "0") , 0, 1)
  )]%>%
  .[merged_inst_id_r != "abroad" & merged_inst_id_s != "abroad"] %>%
  group_by(any_treatment) %>% 
  summarise(count_r= n_distinct(merged_inst_id_r)) %>%
  left_join(ds %>%
              .[, post:= as.numeric(year >= 2008)] %>%
              .[, ':='(any_treatment = fifelse( (is.na(acces_rce_s) | acces_rce_s == "0")
                                                &(is.na(date_first_idex_s) | date_first_idex_s == "0")
                                                &(is.na(fusion_date_s) | fusion_date_s == "0"), 0, 1)
              )]%>%
              .[merged_inst_id_r != "abroad" & merged_inst_id_s != "abroad"] %>%
              group_by(any_treatment) %>% 
              summarise(count_s= n_distinct(merged_inst_id_s)) 
            ,by = c('any_treatment'))

sanity_check <- unique(ds %>%
                         .[, ':='(any_treatment = ifelse( (is.na(acces_rce_s) & acces_rce_s == "0")
                                                          &(is.na(date_first_idex_s) | date_first_idex_s == "0")
                                                          &(is.na(fusion_date_s) | fusion_date_s == "0"), 0, 1)
                         )]%>%
                         .[, list(merged_inst_id_s,any_treatment, acces_rce_s, date_first_idex_s, fusion_date_s)])
table(sanity_check$any_treatment)


cols_to_summarize <- c("movers_w","movers_w_foreign_entrant",
                       "movers_w_junior", "movers_w_medium","movers_w_senior"
                       )

desc_stats_table <- ds %>%
  .[, post:= as.numeric(year >= 2008)] %>%
    .[, ':='(any_treatment_r = fifelse(is.na(acces_rce_r) | acces_rce_r == "0"
                                       |is.na(date_first_idex_r) | date_first_idex_r == "0"
                                       |is.na(fusion_date_r) | fusion_date_r == "0", 0, 1),
             any_treatment_s = fifelse(is.na(acces_rce_s) | acces_rce_s == "0"
                                       |is.na(date_first_idex_s) | date_first_idex_s == "0"
                                       |is.na(fusion_date_s) | fusion_date_s == "0", 0, 1)
    )]%>%
    .[, move := case_when(any_treatment_r ==1 &  any_treatment_s ==1 ~ 'Between treated institutions',
                          any_treatment_r ==0 &  any_treatment_s ==1~ 'From treated to control',
                          any_treatment_r ==1 &  any_treatment_s ==0~ 'From control to treated',
                          any_treatment_r ==0 &  any_treatment_s ==0~ 'From control to control',
                          .default = 'From outside to outside')]%>%
    
    .[merged_inst_id_r != "abroad" & merged_inst_id_s != "abroad"] %>%
  group_by(move, post) %>%
  summarise(
    across(cols_to_summarize, min, na.rm = T, .names = {"min_VAR{col}"}),
    across(cols_to_summarize, mean, na.rm = T, .names = {"mean_VAR{col}"}),
    across(cols_to_summarize, median, na.rm = T, .names = {"median_VAR{col}"}),
    across(cols_to_summarize, sd, na.rm = T, .names = {"sd_VAR{col}"}),
    across(cols_to_summarize, max, na.rm = T, .names = {"max_VAR{col}"})
            ) %>%
  pivot_longer(3:27) %>%
  mutate(value = round(value, 2)) %>%
  mutate(var = str_extract(name, "(?<=VAR).*")) %>%
  mutate(stat = str_extract(name, ".*(?=_VAR)")) %>%
  select(-name) %>% pivot_wider(names_from = stat, values_from = value)


colspec <- paste0("l", paste(rep("c", 6), collapse = ""))

bodylines <- list()

for(value in unique(desc_stats_table$move)){
  
  lines <- c(paste0('\\textbf{',value,'}', paste0(rep('&', 5), collapse = ' '), " \\\\"),
             "\\midrule",
             "\\addlinespace",
             paste0('Prior to the reform ', paste0(rep('&', 5), collapse = ' '), " \\\\")
  )
  bodylines_spec_pre <-desc_stats_table %>%
    filter(move== value & post == 0)
  for(i in 1:nrow(bodylines_spec_pre)){
    lines <- c(lines,
               paste0(paste0(c(dict_vars[as.character(bodylines_spec_pre[i,3])],
                               as.character(bodylines_spec_pre[i,4]),
                               as.character(bodylines_spec_pre[i,5]),
                               as.character(bodylines_spec_pre[i,6]),
                               as.character(bodylines_spec_pre[i,7]),
                               as.character(bodylines_spec_pre[i,8]) ),
                             collapse = " & "),
                      " \\\\"
                               
                      )
    )}
    
    lines <- c(lines,
               "\\addlinespace",
               paste0('After the reform ', paste0(rep('&', 5), collapse = ' '), " \\\\")
               )
    bodylines_spec_post<-desc_stats_table %>%
      filter(move== value & post == 1)
    for(i in 1:nrow(bodylines_spec_post)){
      lines <- c(lines,
                 paste0(paste0(c(dict_vars[as.character(bodylines_spec_post[i,3])],
                                 as.character(bodylines_spec_post[i,4]),
                                 as.character(bodylines_spec_post[i,5]),
                                 as.character(bodylines_spec_post[i,6]),
                                 as.character(bodylines_spec_post[i,7]),
                                 as.character(bodylines_spec_post[i,8]) ),
                               collapse = " & "),
                        " \\\\"
                 )
      )
  }
  
  
  bodylines[[value]] <- c(lines,
                          "\\addlinespace",
                          "\\midrule",
                          "\\addlinespace")
}



lines <- c(
  sprintf("\\begin{tabular}{%s}", colspec),
  "\\toprule",
  " & Min. & Mean & Median & SD & Max \\\\",
  unlist(bodylines),
  "\\bottomrule",
  "\\end{tabular}"
)
tex <- paste(lines, collapse = "\n")
dir.create(dirname("D:\\panel_fr_res\\desc_stats\\lab_to_lab_table.tex"), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, con = "D:\\panel_fr_res\\desc_stats\\lab_to_lab_table.tex", useBytes = TRUE)



desc_stats_table <- ds %>%
  .[, post:= as.numeric(year >= 2008)] %>%
  .[, ':='(any_treatment_r = fifelse(is.na(acces_rce_r) | acces_rce_r == "0"
                                     |is.na(date_first_idex_r) | date_first_idex_r == "0"
                                     |is.na(fusion_date_r) | fusion_date_r == "0", 0, 1),
           any_treatment_s = fifelse(is.na(acces_rce_s) | acces_rce_s == "0"
                                     |is.na(date_first_idex_s) | date_first_idex_s == "0"
                                     |is.na(fusion_date_s) | fusion_date_s == "0", 0, 1)
  )]%>%
  .[, move := case_when(any_treatment_r ==1 &  any_treatment_s ==1 ~ 'Between treated institutions',
                        any_treatment_r ==0 &  any_treatment_s ==1~ 'From treated to control',
                        any_treatment_r ==1 &  any_treatment_s ==0~ 'From control to treated',
                        any_treatment_r ==0 &  any_treatment_s ==0~ 'From control to control',
                        .default = 'From outside to outside')]%>%
  
  .[merged_inst_id_r == "abroad" | merged_inst_id_s == "abroad"] %>%
  group_by(move, post) %>%
  summarise(
    across(cols_to_summarize, min, na.rm = T, .names = {"min_VAR{col}"}),
    across(cols_to_summarize, mean, na.rm = T, .names = {"mean_VAR{col}"}),
    across(cols_to_summarize, median, na.rm = T, .names = {"median_VAR{col}"}),
    across(cols_to_summarize, sd, na.rm = T, .names = {"sd_VAR{col}"}),
    across(cols_to_summarize, max, na.rm = T, .names = {"max_VAR{col}"})
  ) %>%
  pivot_longer(3:27) %>%
  mutate(value = round(value, 2)) %>%
  mutate(var = str_extract(name, "(?<=VAR).*")) %>%
  mutate(stat = str_extract(name, ".*(?=_VAR)")) %>%
  select(-name) %>% pivot_wider(names_from = stat, values_from = value)



colspec <- paste0("l", paste(rep("c", 6), collapse = ""))

bodylines <- list()

for(value in unique(desc_stats_table$move)){
  
  lines <- c(paste0('\\textbf{',value,'}', paste0(rep('&', 5), collapse = ' '), " \\\\"),
             "\\midrule",
             "\\addlinespace",
             paste0('Prior to the reform ', paste0(rep('&', 5), collapse = ' '), " \\\\")
  )
  bodylines_spec_pre <-desc_stats_table %>%
    filter(move== value & post == 0)
  for(i in 1:nrow(bodylines_spec_pre)){
    lines <- c(lines,
               paste0(' & ', 
                      paste0(c(dict_vars[as.character(bodylines_spec_pre[i,3])],
                               as.character(bodylines_spec_pre[i,4]),
                               as.character(bodylines_spec_pre[i,5]),
                               as.character(bodylines_spec_pre[i,6]),
                               as.character(bodylines_spec_pre[i,7]),
                               as.character(bodylines_spec_pre[i,8]) ),
                             collapse = " & "),
                      " \\\\"
                      
               )
    )}
  
  lines <- c(lines,
             "\\addlinespace",
             paste0('After the reform ', paste0(rep('&', 5), collapse = ' '), " \\\\")
  )
  bodylines_spec_post<-desc_stats_table %>%
    filter(move== value & post == 1)
  for(i in 1:nrow(bodylines_spec_post)){
    lines <- c(lines,
               paste0(' & ', 
                      paste0(c(dict_vars[as.character(bodylines_spec_post[i,3])],
                               as.character(bodylines_spec_post[i,4]),
                               as.character(bodylines_spec_post[i,5]),
                               as.character(bodylines_spec_post[i,6]),
                               as.character(bodylines_spec_post[i,7]),
                               as.character(bodylines_spec_post[i,8]) ),
                             collapse = " & "),
                      " \\\\"
               )
    )
  }
  
  
  bodylines[[value]] <- c(lines,
                          "\\addlinespace",
                          "\\midrule",
                          "\\addlinespace")
}



lines <- c(
  sprintf("\\begin{tabular}{%s}", colspec),
  "\\toprule",
  " & Min. & Mean & Median & SD & Max \\\\",
  unlist(bodylines),
  "\\bottomrule",
  "\\end{tabular}"
)
tex <- paste(lines, collapse = "\n")
dir.create(dirname("D:\\panel_fr_res\\desc_stats\\lab_to_lab_table_abroad.tex"), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, con = "D:\\panel_fr_res\\desc_stats\\lab_to_lab_table_abroad.tex", useBytes = TRUE)
