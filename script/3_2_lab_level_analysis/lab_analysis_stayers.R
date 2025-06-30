ds_unilat <- ds %>%
  filter(inst_id_sender == inst_id_receiver) %>%
  dplyr::select(inst_id_receiver,year, ends_with('_r'), starts_with('stayer'), starts_with('total')) %>%
  distinct() %>% group_by(inst_id_receiver) %>%
  mutate(first_y_lab = min(year), last_y_lab = max(year)) %>%
  ungroup()

ds_unilat<- as.data.table(ds_unilat)



all_combinations <- CJ(
  inst_id_receiver = unique(ds_unilat$inst_id_receiver),
  year = seq(min(ds_unilat$year), max(ds_unilat$year))
)
reg_df <- ds_unilat[all_combinations, on = .(inst_id_receiver, year)]
cols_to_fill_down_r <-c( "name_r","city_r","fused_r", "uni_pub_r"         
                         ,"cnrs_r","type_r","main_topic_r","topic_share_r","size_r"
                        , "ecole_r",'universite_r','public_r','prive_r','idex_r', 'first_y_lab','last_y_lab'
)
cols_to_win <- colnames(ds_unilat)[str_detect(colnames(ds_unilat),'stayer|total')]
reg_df <- reg_df %>%
  .[, (cols_to_fill_down_r) := lapply(.SD, function(x) zoo::na.locf(x, na.rm = FALSE)), 
    by = inst_id_receiver, .SDcols = cols_to_fill_down_r] %>%
  .[, ':='( has_idex_r = ifelse(!is.na(idex_r) & idex_r != 'no_idex' & !str_detect(idex_r, 'annulee'), 1, 0  ),
            idex_annulee_r = ifelse(!is.na(idex_r) & str_detect(idex_r, 'annulee'), 1, 0  )
  )] %>%
  .[, (cols_to_win):= lapply(.SD, wins_vars), .SDcols = cols_to_win]

ggplot(reg_df %>% .[ first_y_lab <=2003] %>% .[, .(N = n_distinct(inst_id_receiver)), by = 'year'])+
  geom_line(aes(x=year, y= N))


ggplot(reg_df %>% .[ first_y_lab <=2003] %>% .[, .(N = n_distinct(inst_id_receiver)), by = 'last_y_lab'])+
  geom_line(aes(x=last_y_lab, y= N))


p <-ggplot(reg_df_subset%>%
             .[year >=1997 & !(inst_id_receiver == 'abroad')# & first_y_lab <=2003
             ] %>%
             .[!is.na(uni_pub_r) ] %>%
             .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r)
             )]%>%
             .[, move := case_when(uni_pub_r ==1  ~ 'Inside public universities',
                                   uni_pub_r ==0 ~ 'Outside public universities',
                                   .default = 'With several affilitions')]%>%
             .[, lapply(.SD, mean, na.rm = T), by = c('move','year'),.SDcols = cols_to_win] 
)+
  geom_line(aes(x=year,y=total_w_foreign_entrant, color = move))+
  scale_color_manual(values = c('steelblue','black'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 8))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p

reg_df_subset <- reg_df[year >= 2003 & inst_id_receiver != 'I1294671590'] %>%
  .[type_r%in%c('facility') ]
table(reg_df_subset$uni_pub_r)
test_feols <- feols( total_w ~
                       i(year, uni_pub_r, 2007) +
                       i(year, has_idex_r, 2007) + 
                       i(year, has_idex_r*uni_pub_r, 2007)
                     + i(year, fused_r, 2007)
                     | 
                       inst_id_receiver + year^main_topic_r +cnrs_r^year +ecole_r^year+prive_r^year+first_y_lab^year
                    ,reg_df_subset )
iplot(test_feols)
iplot(test_feols, i.select=2)
iplot(test_feols, i.select=3)
iplot(test_feols, i.select=4)

