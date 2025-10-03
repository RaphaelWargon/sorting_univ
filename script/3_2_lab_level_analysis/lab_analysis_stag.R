reg_df_s <- reg_df %>%
  .[(fusion_date_r ==0 & fusion_date_s ==0) | (fusion_date_r >=2019 & fusion_date_s >=2019)] %>%
  .[year %in%2003:2018  & max_pair >0.1
    ] %>%
  .[, ':='(acces_rce_r = as.character(acces_rce_r), acces_rce_s= as.character(acces_rce_s),
           date_first_idex_r = as.character(date_first_idex_r), date_first_idex_s= as.character(date_first_idex_s)
           )] #%>%
  #.[, ':='( rce_idex_r = ifelse(acces_rce_r != "0" & date_first_idex_r != '0', 
  #                              date_first_idex_r, "0" ),
  #          date_first_idex_r = ifelse(acces_rce_r =="0", date_first_idex_r,"0") ,
  #          rce_idex_s = ifelse(acces_rce_s != "0" & date_first_idex_s != "0", 
  #                              date_first_idex_r, "0" ),
  #          date_first_idex_s = ifelse(acces_rce_s =="0", date_first_idex_s, "0") 
  #            )
  #  ] %>%
  #.[, ':='(acces_rce_r = ifelse(rce_idex_r != "0" & as.numeric(rce_idex_r)>= as.numeric(year), "0", 
  #                              acces_rce_r),
  #         acces_rce_s = ifelse(rce_idex_s != "0" & as.numeric(rce_idex_s)>= as.numeric(year), "0", 
  #                              acces_rce_s)
  #         )]

table(reg_df_s$rce_idex_r, reg_df_s$acces_rce_r)
table(reg_df_s$rce_idex_s, reg_df_s$acces_rce_s)
table(reg_df_s$acces_rce_r, reg_df_s$acces_rce_s)

gc()
list_g = list("acces_rce" = list( i = sort(unique(reg_df_s[acces_rce_s !=0]$acces_rce_s)),
                                j = sort(unique(reg_df_s[acces_rce_r !=0]$acces_rce_r)))
              
              ,"date_first_idex" = list( i = sort(unique(reg_df_s[date_first_idex_s !=0]$date_first_idex_s)),
                                      j =   sort(unique(reg_df_s[date_first_idex_r !=0]$date_first_idex_r)))
              
             #, "rce_idex" = list( i = sort(unique(reg_df_s[rce_idex_s !=0]$rce_idex_s)),
             #                    j = sort(unique(reg_df_s[rce_idex_r !=0]$rce_idex_r)))
             , "fusion_date" = list( i = sort(unique(reg_df_s[fusion_date_s !=0]$fusion_date_s)),
                                  j = sort(unique(reg_df_s[fusion_date_r !=0]$fusion_date_r)))
)
gc()
treatvars <- c()
for(d in c('acces_rce','date_first_idex'#,'rce_idex', 'fusion_date'
           )){
  for(g_i in list_g[[d]]$i){
    print(paste0(d, ': ', g_i))
    for(y in unique(reg_df_s$year)){
      varname = paste0('D_i_', d, '_g', as.character(g_i), '_y', as.character(y))
      reg_df_s[[varname]] <- as.numeric( (reg_df_s[[paste0(d, '_s')]] == g_i)
                                         & (reg_df_s[["year"]] == y)
                                         & (reg_df_s[['inst_id_receiver']] != 'abroad') )
      treatvars <- c(treatvars, varname)
      varname_abroad = paste0('D_i_a_', d, '_g', as.character(g_i), '_y', as.character(y))
      reg_df_s[[varname_abroad]] <- as.numeric( (reg_df_s[[paste0(d, '_s')]] == g_i)
                                                & (reg_df_s[["year"]] == y)
                                                & (reg_df_s[['inst_id_receiver']] == 'abroad'))
      treatvars <- c(treatvars, varname_abroad)
      
    }
  }
  for(g_j in list_g[[d]]$j){
    print(paste0(d, ': ', g_j))
    for(y in unique(reg_df_s$year)){
      varname = paste0('D_j_', d, '_g', as.character(g_i), '_y', as.character(y))
      reg_df_s[[varname]] <- as.numeric( (reg_df_s[[paste0(d, '_r')]] == g_i)
                                         & (reg_df_s[["year"]] == y) 
                                         & (reg_df_s[['inst_id_sender']] != 'abroad'))
      treatvars <- c(treatvars, varname)
      varname_abroad = paste0('D_j_a_', d, '_g', as.character(g_i), '_y', as.character(y))
      reg_df_s[[varname_abroad]] <- as.numeric( (reg_df_s[[paste0(d, '_r')]] == g_i)
                                                & (reg_df_s[["year"]] == y)
                                                & (reg_df_s[['inst_id_sender']] == 'abroad'))
      treatvars <- c(treatvars, varname_abroad)
    }
  }
}  
length(treatvars)
#colSums(reg_df_s %>% select(all_of(treatvars)))
formula_event_study_stag <- paste0('~', paste0(treatvars, collapse = "+"))

var = 'movers_w'
gc()


es_stag <- feols(as.formula(paste0(var, formula_event_study_stag, fe_large))
                         , data = reg_df_s
                         #,weights = reg_df$size_r + reg_df$size_r
                         ,cluster = c('unit')
) 

gc()


agg_effect <- agg_etwfe(es_stag, t_limit = 7)

gc()

agg_effect_by_t <- agg_etwfe_het(es_stag, 't')
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
agg_effect_by_g <- agg_etwfe_het(es_stag, 'g')

for(treat in unique(agg_effect_by_t$treatment)){
  p <- ggplot(agg_effect_by_g %>% .[treatment %in% c(treat)])+
    geom_point(aes(x= g, y = est, shape = treatment))+
    geom_errorbar(aes(x=g, ymin = est -1.96*std, ymax=est+1.96*std, linetype = treatment))+
    geom_vline(aes(xintercept = 0.5), color = 'red')+geom_hline(aes(yintercept = 0), linetype = "dashed")+
    labs(title = paste0('Treatment: ', treat))+
    theme_bw()
  print(p)
}

gc()

