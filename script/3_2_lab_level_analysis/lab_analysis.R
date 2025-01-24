rm(list = ls())
gc()
library('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools')
wins_vars <- function(x, pct_level = 0.025){
  if(is.numeric(x)){
    Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    #winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}


inputpath <- "E:\\panel_fr_res\\inst_level_flows.parquet"

ds <- as.data.table(open_dataset(inputpath))

unique(ds[is.na(uni_pub_s)& inst_id_sender !='abroad'][, list(inst_id_sender)])

ds <- ds %>%
  .[, size_r:=max(total * as.numeric(inst_id_sender == inst_id_receiver)), by= c('inst_id_receiver','year')]%>%
  .[, size_s:=max(total * as.numeric(inst_id_sender == inst_id_receiver)), by= c('inst_id_sender','year')]

cols_to_summarize <- c("total", "stayers", "movers","total_w","stayers_w","movers_w")

ggplot(ds%>%
         .[year >=1997] %>%
         .[year >=1997 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')] %>%
         .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') | (!is.na(uni_pub_s) | inst_id_sender == 'abroad')] %>%
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
         .[year >=1997 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')] %>%
         .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') | (!is.na(uni_pub_s) | inst_id_sender == 'abroad')] %>%
         .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
                  uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s)
         )]%>%
       # .[type_s == 'facility' & type_r == 'facility']%>%
         .[, lapply(.SD, mean, na.rm = T), by = c('uni_pub_r','uni_pub_s','year'),.SDcols = cols_to_summarize] %>%
         .[, move := case_when(uni_pub_r ==1 & uni_pub_s ==1 ~ 'Inside public universities',
                               uni_pub_r ==0 &  uni_pub_s ==1~ 'From university to outside',
                               uni_pub_r ==1 &  uni_pub_s ==0~ 'From outside to university',
                               uni_pub_r ==0 &  uni_pub_s ==0~ 'From outside to outside',
                               .default = 'From outside to outside')]
)+
  geom_line(aes(x=year,y=movers, color = move), linewidth = 0.5)+
  scale_color_manual(values = c('black','seagreen','firebrick','steelblue'))+
  geom_vline(aes(xintercept = 2007), linetype ='dashed')+ geom_vline(aes(xintercept = 2009))+
  theme_bw()+ 
  theme(legend.title = element_blank(),legend.position = 'bottom',legend.box.margin = margin(),legend.text = element_text(size = 6))+
  #guides(color = guide_legend(nrow=2,byrow=TRUE))+
  labs(title = '')+xlab('Year')+ylab('')
p
library('cowplot')
save_plot("E:\\panel_fr_res\\desc_stats\\avg_flows.png", p)


p <-ggplot(ds%>%
         .[year >=1997 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')] %>%
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
save_plot("E:\\panel_fr_res\\desc_stats\\total_authors.png", p)

    

test <- unique(ds%>%
  .[is.na(type_r)]%>%
  .[, list(inst_id_sender)]
)
cols_to_wins <- c('total','movers','stayers','movers_w','stayers_w','total_w')
reg_df <-  ds%>%
  .[year >=2000 & !(inst_id_sender == 'abroad' & inst_id_receiver == 'abroad')] %>%
  .[inst_id_receiver != inst_id_sender] %>%
  .[, (cols_to_wins) := lapply(.SD, wins_vars, pct_level =0.025) , .SDcols = cols_to_wins]%>%
  .[#inst_id_sender!='abroad' & inst_id_receiver != 'abroad'
    #
   # & type_r == 'facility' & type_s == 'facility'
   !(type_r %in% c('other','archive','healthcare')) & !(type_s %in% c('other','archive','healthcare'))
    ] %>%
  .[(!is.na(uni_pub_r) | inst_id_receiver == 'abroad') & (!is.na(uni_pub_s) | inst_id_sender == 'abroad')] %>%
  .[, ':='(uni_pub_r = fifelse(is.na(uni_pub_r), 0, uni_pub_r),
           uni_pub_s = fifelse(is.na(uni_pub_s), 0, uni_pub_s),
           cnrs_r = fifelse(inst_id_receiver =="abroad", 0, cnrs_r),
           cnrs_s = fifelse(inst_id_sender =="abroad", 0, cnrs_s),
           fused_r = ifelse(inst_id_receiver =="abroad", 0, fused_r),
           fused_s = ifelse(inst_id_sender =="abroad", 0, fused_s),
           type_r = fifelse(inst_id_receiver =='abroad', 'abroad', type_r),
           type_s = fifelse(inst_id_sender =='abroad', 'abroad', type_s)
           
  )] %>%
  .[, size_fe := n_distinct(paste0(inst_id_receiver, inst_id_sender)), by =c('uni_pub_r','uni_pub_s','type_r','type_s') ] %>%
  .[size_fe >=100 & size_r >=5 & size_s >=5] %>%
  .[, ':='(abroad_r = as.numeric(inst_id_receiver=='abroad'),
           abroad_s = as.numeric(inst_id_sender=='abroad'),
           type_r_year = paste0(type_r, '_',year),
           type_s_year = paste0(type_s, '_',year),
           type_s_type_r_year = paste0(type_r, '_',type_s, '_', year)
           )]  %>%
  .[ type_s %in% c('facility'#,'abroad'
                   ) & type_r %in% c('facility'#,'abroad'
                                     )
     &  (uni_pub_r + cnrs_r <2) &  (uni_pub_s + cnrs_s <2) ]
reg_df[, .(n_distinct(paste0(inst_id_receiver, inst_id_sender))), by = c('uni_pub_r','uni_pub_s')]

reg_df[, .(n_distinct(paste0(inst_id_receiver, inst_id_sender))), by = c('uni_pub_r','uni_pub_s','type_r','type_s')]
#unique(reg_df[uni_pub_s == 0 & uni_pub_r == 0 & cnrs_s + cnrs_r >0][, list(name_r, cnrs_r, name_s,cnrs_s, parent_r, parent_s)])
test_feols <- feols(movers_w ~ 
                      i(year, uni_pub_r, 2008) +
                      i(year, uni_pub_s, 2008)+ 
                      i(year, uni_pub_r*uni_pub_s, 2008) 
                    + i(year, uni_pub_r*abroad_s, 2008)
                    + i(year, uni_pub_s*abroad_r, 2008)
                    + size_r*as.factor(year)+size_s*as.factor(year)
                    | 
                      inst_id_receiver + inst_id_sender + year
                    + inst_id_receiver^inst_id_sender
                    + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                  #  + fused_r^year + fused_s^year
                  # + type_r_year + type_s_year + type_s_type_r_year
                    , data = reg_df
                 #   ,cluster = c('inst_id_sender','inst_id_receiver')
                    )
iplot(test_feols, i.select=1, main = 'Universities as destination')
iplot(test_feols, i.select=2, main = 'Universities as origin')
iplot(test_feols, i.select=3, main = 'Between-university flows')
iplot(test_feols, i.select=4, main = 'Flows to universities from abroad')
iplot(test_feols, i.select=5, main = 'Flows abroad from universities')

test_feols_fe <- fixef(test_feols)

test_feols_fe$`type_s_type_r_year`
###
test

pdf("E:\\panel_fr_res\\lab_results\\uni_pub_r_facility_wUMR.pdf")
iplot(test_feols, i.select=1, main = 'Universities as destination')
dev.off()
pdf("E:\\panel_fr_res\\lab_results\\uni_pub_s_facility_wUMR.pdf")
iplot(test_feols, i.select=2, main = 'Universities as origin')
dev.off()

pdf("E:\\panel_fr_res\\lab_results\\uni_pub_rs_facility_wUMR.pdf")
iplot(test_feols, i.select=3, main = 'Between-university flows')
dev.off()


test_agg <- feols(movers_w ~ 
                    i(post, uni_pub_r,           1) +
                    i(post, uni_pub_s,           1)+ 
                    i(post, uni_pub_r*uni_pub_s, 1) 
                  + i(post, uni_pub_r*abroad_s,  1)
                  + i(post, uni_pub_s*abroad_r,  1)
                  + size_r*as.factor(year)+size_s*as.factor(year)
                  | 
                    inst_id_receiver + inst_id_sender + year
                  + inst_id_receiver^inst_id_sender
                  + cnrs_r^year + cnrs_s^year + cnrs_r^cnrs_s^year
                  + fused_r^year + fused_s^year
                  # + type_r_year + type_s_year + type_s_type_r_year
                  , data = reg_df[,post:=as.numeric(year>=2009)]
                  ,cluster = c('inst_id_sender','inst_id_receiver'))
etable(test_agg)
gc()
  