var <- 'n_au'
test <- ds_clean %>% .[min_n_au >1 & n_au_2003 >=10 & n_au_2003 <= 50
                        & acces_rce != "2015" & !(field %in% c('18','36',"21",'34'))
                       & type %in% c('education','healthcare','facility','government')
                       ] 

test2 <- unique(test[, list(merged_inst_id, name, field, n_au_2003, min_n_au, acces_rce, date_first_idex, fusion_date)])
start_time <- Sys.time()
es_stag <- fepois( as.formula(paste0(var, ' ~ ', paste0(formula_elements, collapse= '+'), '|merged_inst_id_field + year + paris^year + field^year+ type^cnrs^year + n_au_2003^year'))
                   , data = test %>% .[, merged_inst_id_field := paste0(merged_inst_id,field)]
                   ,mem.clean = TRUE,lean = TRUE, fixef.tol = 1E-2,
                   ,cluster = c('merged_inst_id_field')
) 
time_taken <- Sys.time() - start_time
print(time_taken)

agg_stag_no_ctrl <- agg_effects(es_stag, ds_clean, t_limit =7)%>%
  .[, var := var] %>% .[, ctrl := 'None']

print(agg_stag_no_ctrl)

agg_stag_by_t_no_ctrl <- agg_effect_het(es_stag, ds_clean, by  ='t')%>%
  .[, var := var] %>% .[, ctrl := 'None']


for(treat in unique(agg_stag_by_t_no_ctrl$treatment)){
  p <- ggplot(agg_stag_by_t_no_ctrl %>% .[treatment %in% c(treat) & abs(t)<=7])+
    geom_point(aes(x= t, y = est))+
    geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
    geom_vline(aes(xintercept = -1), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  print(p)
  rm(p)
}


gc()

agg_stag_by_g_no_ctrl <- agg_effect_het(es_stag, ds_clean, by  ='g')%>%
  .[, var := var] %>% .[, ctrl := 'None']


for(treat in unique(agg_stag_by_g_no_ctrl$treatment)){
  p <- ggplot(agg_stag_by_g_no_ctrl %>% .[treatment %in% c(treat) ])+
    geom_point(aes(x= g, y = est))+
    geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std), width = 0.5)+
    geom_hline(aes(yintercept = 0))+
    labs(title = paste0('Treatment: ', dict_vars[[treat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
    theme_bw()
  print(p)
  rm(p)
}
gc()

ggplot(test %>%
         .[, .(n_au=mean(n_au)), by = c('acces_rce','year')])+
  geom_line(aes(x=year, y = n_au, color= acces_rce))


ggplot(test %>%
         .[, .(n_au=mean(n_au)), by = c('fusion_date','year')])+
  geom_line(aes(x=year, y = n_au, color= fusion_date))

