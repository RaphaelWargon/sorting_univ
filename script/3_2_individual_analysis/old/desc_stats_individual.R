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
       'boot'#,
       #'DIDmultiplegt',
       #"DIDmultiplegtDYN"#,'didimputation'
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

sample_df_peers <- fread("E:\\panel_fr_res\\sample_df.csv") 
gc()


sample_df_reg <- sample_df_peers %>%
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


p <- ggplot(sample_df_peers %>% 
              .[year < 2020] %>%
              .[, any_treatment := fifelse(acces_rce == "0" & date_first_idex == "0"
                                           &fusion_date == '0', "Control","Treated")] %>%
              .[, .(N = mean(publications_raw, na.rm =T)) ,by = c('any_treatment','year')]
)+
  geom_line(aes(x=year,y=N, color = any_treatment, group = any_treatment), linewidth = 0.5)+
  scale_color_manual(values = c("black","steelblue"))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\avg_publications.png", p)



p <- ggplot(sample_df_peers %>% 
              .[year < 2020] %>%
              .[, any_treatment := fifelse(acces_rce == "0" & date_first_idex == "0"
                                           &fusion_date == '0', "Control","Treated")] %>%
              .[, .(N = mean(citations_raw, na.rm =T)) ,by = c('any_treatment','year')]
)+
  geom_line(aes(x=year,y=N, color = any_treatment, group = any_treatment), linewidth = 0.5)+
  scale_color_manual(values = c("black","steelblue"))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\avg_citations.png", p)



p <- ggplot(sample_df_peers %>% 
              .[year < 2020] %>%
              .[, any_treatment := fifelse(acces_rce == "0" & date_first_idex == "0"
                                           &fusion_date == '0', "Control","Treated")] %>%
              .[, .(N = mean(nr_source_top_5pct_raw, na.rm =T)) ,by = c('any_treatment','year')]
)+
  geom_line(aes(x=year,y=N, color = any_treatment, group = any_treatment), linewidth = 0.5)+
  scale_color_manual(values = c("black","steelblue"))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\avg_nr_source_top_5pct.png", p)


p <- ggplot(sample_df_peers %>% 
              .[year < 2020] %>%
              .[, any_treatment := fifelse(acces_rce == "0" & date_first_idex == "0"
                                           &fusion_date == '0', "Control","Treated")] %>%
              .[, .(N = mean(avg_rank_source, na.rm =T)) ,by = c('any_treatment','year')]
)+
  geom_line(aes(x=year,y=N, color = any_treatment, group = any_treatment), linewidth = 0.5)+
  scale_color_manual(values = c("black","steelblue"))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\avg_rank_source.png", p)





p <- ggplot(unique(sample_df_peers %>% 
                     .[acces_rce != 0 & year %in%2004:2007] %>%
                     .[, list(merged_inst_id, field, domain, acces_rce, n_au_inst_id_field_y,year)])%>%
              .[, .(N = mean(n_au_inst_id_field_y, na.rm =T)) ,by = c('acces_rce',"domain")] %>%
              .[nchar(domain)<3] %>% .[, domain_char := case_when(domain == '1' ~ "Life Sciences",
                                                                  domain == '2' ~ "Social Sciences",
                                                                  domain == '3' ~ "Physical Sciences",
                                                                  domain == '4' ~ "Health Sciences"
              )]
)+
  geom_col(aes(x=acces_rce,y=N, fill = domain_char), linewidth = 0.5, position = "dodge")+
  scale_fill_manual(values = c("steelblue","aquamarine3","brown","goldenrod"))+
  #geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 10))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\characteristics_acces_rce.png", p)




p <- ggplot(unique(sample_df_peers %>% 
                     .[date_first_idex != 0 & year %in%2004:2007] %>%
                     .[, list(merged_inst_id, field, domain, date_first_idex, n_au_inst_id_field_y,year)])%>%
              .[, .(N = mean(n_au_inst_id_field_y, na.rm =T)) ,by = c('date_first_idex',"domain")] %>%
              .[nchar(domain)<3] %>% .[, domain_char := case_when(domain == '1' ~ "Life Sciences",
                                                                  domain == '2' ~ "Social Sciences",
                                                                  domain == '3' ~ "Physical Sciences",
                                                                  domain == '4' ~ "Health Sciences"
              )]
)+
  geom_col(aes(x=date_first_idex,y=N, fill = domain_char), linewidth = 0.5, position = "dodge")+
  scale_fill_manual(values = c("steelblue","aquamarine3","brown","goldenrod"))+
  #geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 10))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\characteristics_date_first_idex.png", p)


p <- ggplot(unique(sample_df_peers %>% 
                     .[fusion_date != 0 & year %in%2004:2007] %>%
                     .[, list(merged_inst_id, field, domain, fusion_date, n_au_inst_id_field_y,year)])%>%
              .[, .(N = mean(n_au_inst_id_field_y, na.rm =T)) ,by = c('fusion_date',"domain")] %>%
              .[nchar(domain)<3] %>% .[, domain_char := case_when(domain == '1' ~ "Life Sciences",
                                                                  domain == '2' ~ "Social Sciences",
                                                                  domain == '3' ~ "Physical Sciences",
                                                                  domain == '4' ~ "Health Sciences"
              )]
)+
  geom_col(aes(x=fusion_date,y=N, fill = domain_char), linewidth = 0.5, position = "dodge")+
  scale_fill_manual(values = c("steelblue","aquamarine3","brown","goldenrod"))+
  #geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 10))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("E:\\panel_fr_res\\desc_stats\\characteristics_fusion_date.png", p)


p <- ggplot(unique(sample_df_peers %>% 
                     .[acces_rce == 0 & fusion_date == 0 & date_first_idex == 0& year %in%2004:2007] %>%
                     .[, list(merged_inst_id, field, domain, type, n_au_inst_id_field_y,year)])%>%
              .[, .(N = mean(n_au_inst_id_field_y, na.rm =T)) ,by = c('type',"domain")] %>%
              .[nchar(domain)<3] %>% .[, domain_char := case_when(domain == '1' ~ "Life Sciences",
                                                                  domain == '2' ~ "Social Sciences",
                                                                  domain == '3' ~ "Physical Sciences",
                                                                  domain == '4' ~ "Health Sciences"
              )]
)+
  geom_col(aes(x=type,y=N, fill = domain_char), linewidth = 0.5, position = "dodge")+
  scale_fill_manual(values = c("steelblue","aquamarine3","brown","goldenrod"))+
  #geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 10))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

save_plot("D:\\panel_fr_res\\desc_stats\\characteristics_never_treated.png", p)



cols_to_summarize <-  c('publications_raw',
                        'citations_raw',
                        'nr_source_top_5pct_raw'
)

desc_stats_table <- sample_df_reg %>%
  .[, post := as.numeric(year %in% 1997:2003)]%>%
  .[, any_treatment := fifelse(acces_rce == "0" & date_first_idex == "0"
                               &fusion_date == '0', "Control","Treated")] %>%
  group_by(any_treatment, post) %>%
  summarise(
    across(cols_to_summarize, min, na.rm = T, .names = {"min_VAR{col}"}),
    across(cols_to_summarize, mean, na.rm = T, .names = {"mean_VAR{col}"}),
    across(cols_to_summarize, median, na.rm = T, .names = {"median_VAR{col}"}),
    across(cols_to_summarize, sd, na.rm = T, .names = {"sd_VAR{col}"}),
    across(cols_to_summarize, max, na.rm = T, .names = {"max_VAR{col}"})
  ) %>%
  pivot_longer(3:17) %>%
  mutate(value = round(value, 2)) %>%
  mutate(var = str_extract(name, "(?<=VAR).*")) %>%
  mutate(stat = str_extract(name, ".*(?=_VAR)")) %>%
  select(-name) %>% pivot_wider(names_from = stat, values_from = value)



dict_vars <- c('acces_rce'= 'University autonomy',
               'date_first_idex'='Received an IDEX',
               'fusion_date'= "Merged establishment",
               "publications_raw" = 'Publications',
               "citations_raw"='Citations',
               "publications" = 'Publications',
               "citations"='Citations',
               "avg_publications" = 'Average publications',
               "avg_citations"='Average citations',
               "n_au" = "Number of researchers",
               "nr_source_btm_50pct_raw"="Bottom 50% journal publications",
               "nr_source_mid_40pct_raw"="Middle 40% journal publications",
               "nr_source_top_20pct_raw"="Top 20% journal publications", 
               "nr_source_top_10pct_raw" ="Top 10% journal publications", 
               "nr_source_top_5pct_raw" ="Top 5% journal publications",
               "nr_source_btm_50pct"="Bottom 50% journal publications",
               "nr_source_mid_40pct"="Middle 40% journal publications",
               "nr_source_top_20pct"="Top 20% journal publications", 
               "nr_source_top_10pct" ="Top 10% journal publications", 
               "nr_source_top_5pct" ="Top 5% journal publications",
               "type" = 'Institution Type',
               "city"=  'City',
               'cnrs'='CNRS',
               'public'='Public Status',
               'ecole'='Grande Ecole status',
               'main_topic'='Main field',
               "|merged_inst_id"= 'Institution',
               " |merged_inst_id"= 'Institution',
               "|merged_inst_id_field"= 'Institution $\\times$ field',
               "| merged_inst_id_field"= 'Institution $\\times$ field',
               " | merged_inst_id_field"= 'Institution $\\times$ field',
               "author_id"= 'Author',              "year"= 'Year',  "entry_year"= 'Entry year',
               "field"= 'Field',              "gender"= 'Gender',
               " |merged_inst_id"= 'Institution $\\times$ field'
)
gc()


colspec <- paste0("l", paste(rep("c", 6), collapse = ""))

bodylines <- list()

for(value in unique(desc_stats_table$any_treatment)){
  
  lines <- c(paste0('\\textbf{',value,'}', paste0(rep('&', 5), collapse = ' '), " \\\\"),
             "\\midrule",
             "\\addlinespace",
             paste0('Prior to the reform ', paste0(rep('&', 5), collapse = ' '), " \\\\")
  )
  bodylines_spec_pre <-desc_stats_table %>%
    filter(any_treatment== value & post == 0)
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
    filter(any_treatment== value & post == 1)
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
dir.create(dirname("D:\\panel_fr_res\\desc_stats\\indiv_table.tex"), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, con = "D:\\panel_fr_res\\desc_stats\\indiv_table.tex", useBytes = TRUE)


