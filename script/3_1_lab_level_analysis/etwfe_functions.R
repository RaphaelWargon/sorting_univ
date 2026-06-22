agg_etwfe <- function(stag_model, data, R = 0, t_limit = 0){
  coefs <- as.data.table(stag_model$coefficients, keep.rownames = TRUE)
  colnames(coefs) <-c("var",'est')
  coefs <- coefs %>%
    .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[rs]_)')]%>%
    .[!is.na(est) & !is.na(d)]%>%
    .[, type := case_when(str_detect(var, '_s_g') ~ 'delta',
                          str_detect(var, '_r_g')~'gamma',
                          .default = '')] %>%
    .[, g := str_extract(var, '(?<=g)[0-9]{1,4}')] %>%
    .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
    .[, t := as.numeric(year)-as.numeric(g)] %>%
    .[, treat := ifelse(type!='',
                        paste0(type, '_', d), d)]
  
  if(t_limit !=0){
    coefs <- coefs[, t := case_when(t > t_limit ~ t_limit +1,
                                    t < -t_limit ~ -t_limit -1,
                                    .default = t)]
  }
  
  all_treatments_in_reg = unique(coefs$treat)
  etwfe_agg <- data.table(treat = '', est = 0, std = 0, t = 0, pvalue = 0) %>% .[treat != '']
  for(trt in all_treatments_in_reg){
    print(trt)
    tryCatch({
    to_sum <- coefs %>% .[treat == trt & g!="0" & t>0] 
    var = paste0(str_remove(to_sum$d[[1]], '^a_'), ifelse(str_detect(trt, 'delta'), '_s','_r'))
    if(str_detect(trt, '_a')){
      w <- data %>%
        .[merged_inst_id_s == 'abroad' | merged_inst_id_r == 'abroad'] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>% .[,year := as.character(year)]
    }
    if(str_detect(trt, '_e')){
      w <- data %>%
        .[merged_inst_id_s == 'entry' ] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)]
    }
    if(!str_detect(trt, '_a') & !str_detect(trt, '_e')) {
      w <- data %>%
        .[merged_inst_id_s != 'abroad' & merged_inst_id_r != 'abroad'] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>% .[,year := as.character(year)]
    }
    colnames(w) <- c('g', 'year', 'w')
    to_sum <- merge(to_sum %>%
                      .[, ':='(g =as.character(g),
                               year = as.character(year))],
                    w%>%
                      .[, ':='(g =as.character(g),
                               year = as.character(year))], by = c('g','year') )%>%
      .[, w:= w/sum(w)]
    aggte = sum(to_sum$w * to_sum$est)
    cov_effect <- vcov(stag_model)[to_sum$var, to_sum$var]
    aggte_se <- sqrt(t(to_sum$w)%*% cov_effect %*% to_sum$w)[1,1]
    aggte_t <- (aggte-R)/aggte_se
    pval = dt(aggte_t, degrees_freedom(stag_model, type = 't'))
    etwfe_agg = rbind(etwfe_agg, data.table(treat = trt, est = aggte, std = aggte_se, t = aggte_t,
                                            pvalue = pval) )
    },
    error = function(cond) {
      message(conditionMessage(cond))
      NA
    }
    )
  }
 

  return(etwfe_agg)
}

agg_etwfe_het <- function(stag_model, data, by = 't', t_limit = 0){
  coefs <- as.data.table(stag_model$coefficients, keep.rownames = TRUE)
  colnames(coefs) <-c("var",'est')
  coefs <- coefs %>%
    # .[ str_detect(var, '(?<=[0-9]:)[a-z_]+(?=_[ars]_)')]%>%
    .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[rs]_)')]%>%
    .[!is.na(est) & !is.na(d)]%>%
    .[, type := case_when(str_detect(var, '_s_g') ~ 'delta',
                          str_detect(var, '_r_g')~'gamma',
                          .default = '')] %>%
    .[, g := str_extract(var, '(?<=g)[0-9]{1,4}')] %>%
    .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
    .[, t := as.numeric(year)-as.numeric(g)] %>%
    .[, treat := ifelse(type!='',
                        paste0(type, '_', d), d)]
  #coefs$treat <- paste0(coefs$treat, '_', by, coefs[[by]])
  vcov_st_m <- vcov(stag_model)
  all_treatments_in_reg = unique(coefs$treat)
  etwfe_by <- data.table(treat = '', est = 0, std = 0, t_value=0, p_value = 0, by = 0) %>% .[treat != '']
  if(t_limit !=0){
    coefs <- coefs[, t := case_when(t > t_limit ~ t_limit +1,
                                    t < -t_limit ~ -t_limit -1,
                                    .default = t)]
  }
  
  if(by !='t'){
  coefs <- coefs[t >0]  
  }
    for(trt in all_treatments_in_reg){
      tryCatch({
    print(trt)
    start_time_trt = Sys.time()
    to_sum <- coefs %>% .[treat == trt] 
    var = paste0(str_remove(to_sum$d[[1]], '^a_'), ifelse(str_detect(trt, 'delta'), '_s','_r'))
    if(str_detect(trt, '_a')){
      w <- data %>%
        .[merged_inst_id_s == 'abroad' | merged_inst_id_r == 'abroad'] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)]
    }
    if(str_detect(trt, '_e')){
      w <- data %>%
        .[merged_inst_id_s == 'entry' ] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)]
    }
    if(!str_detect(trt, '_a') & !str_detect(trt, '_e')) {
      w <- data %>%
        .[merged_inst_id_s != 'abroad' & merged_inst_id_r != 'abroad' & merged_inst_id_s != 'entry'] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)] 
    }
    colnames(w) <- c('g', 'year', 'w')
    to_sum <- merge(to_sum %>%
                      .[,year:=as.character(year)] %>%
                      .[, g:=as.character(g)],
                    w %>%
                      .[, g :=as.character(g)], by = c('g','year') )
    all_values=  unique(to_sum[[by]])
    for(val in all_values){
      #print(val)
      to_sum_by = to_sum[to_sum[[by]] == val] %>%
        .[, w:= w/sum(w)]
      aggte = sum(to_sum_by$w * to_sum_by$est)
      cov_effect <- vcov_st_m[to_sum_by$var, to_sum_by$var]
      aggte_se <- sqrt(t(to_sum_by$w)%*% cov_effect %*% to_sum_by$w)[1,1]
      aggte_t <- (aggte)/aggte_se
      pval = dt(aggte_t, degrees_freedom(stag_model, type = 't'))
      etwfe_by = rbind(etwfe_by, data.table(treat = trt, est = aggte, std = aggte_se, t_value = aggte_t, p_value= pval, by = val) )}
  time_trt = Sys.time()-start_time_trt
  if(by =='t'){
    ref = ''
    for( el in min(all_values):max(all_values)){
      if(!el %in% all_values){
        ref = el
      }
    }
    print(ref)
    if(ref != ''){
      etwfe_by = rbind(etwfe_by, data.table(treat = trt, est = 0, std = NA, t_value = 0, p_value= 0, by = ref) )}
  }
      },
  error = function(cond) {
    message(conditionMessage(cond))
    NA
  }
      )
      
    }
  colnames(etwfe_by) <- c('treatment','est','std', 't_value','p_value', by)
  if(t_limit !=0 & by == 't'){
    #print('correction')
    etwfe_by <- etwfe_by[, t := case_when(t > t_limit ~ paste0('>',t_limit),
                                    t < -t_limit ~ paste0('<-',t_limit),
                                    .default = as.character(t)) ]
    etwfe_by$t = factor(etwfe_by$t, levels = c(paste0('<-',t_limit), as.character(-t_limit:t_limit),  paste0('>',t_limit)))
  }
  
  return(etwfe_by)
}



compute_all_estimates_etwfe <- function(outcomes = NULL,
                                  data,
                                  id_vars = c('unit'),
                                  trend_controls = NULL,
                                  w_matching = TRUE,
                                  matching_variables =NULL,
                                  plot_event_study = FALSE,
                                  save_event_study = FALSE,
                                  save_path = '',
                                  type = 'fepois',
                                  comparison_group = 'never-treated',
                                  formula_elements 
){
  list_es_all = list()
  
  units <- unique(data[, ..unit_cols]) %>%
    .[, treat := as.numeric(acces_rce_r != 0 | date_first_idex_r!=0 | fusion_date_r != 0
                            | acces_rce_s != 0 | date_first_idex_r!=0 | fusion_date_r != 0
                            )]
  print(table(units$treat))
  
  formula_ctrl <- paste0( 'y ~ ',  
                          paste0(formula_elements, collapse= '+'), 
                          '|  year + merged_inst_id_r^domain  +merged_inst_id_s^domain +',
                          paste0(id_vars, collapse = '+')
  )
  if(!is.null(trend_controls)){
    formula_ctrl <- paste0(formula_ctrl, "+",
                           paste0(paste0(trend_controls, '^year'), collapse = '+'))
  }
  
  if(comparison_group == 'never-treated' & w_matching == TRUE){
    
    formula_ctrl <- paste0(formula_ctrl,  '+ subclass^year')
    
    matching_fml = paste0('treat ~ ', paste0(matching_variables, collapse = "+"))
    match_units <- matchit(as.formula(matching_fml)
                           ,data = units
                           ,method = "exact")
    matched_units <- match.data(match_units)
    print(table(matched_units[["treat"]]))
    
    to_keep_in_matched_units <- c(id_vars, "subclass")
    
    matched_units <- matched_units %>%
      .[, ..to_keep_in_matched_units]
    
  }
  if(comparison_group == 'not-yet-treated'){
    data <- data %>%
      .[, treat := as.numeric(acces_rce_r != 0 | date_first_idex_r!=0 | fusion_date_r != 0
                              | acces_rce_s != 0 | date_first_idex_r!=0 | fusion_date_r != 0
      )]
    .[treat == 1]
  }
  for(var in outcomes ){
    
    list_es_all[[var]] <- list()
    
    data$y <- data[[var]]
    

    if(w_matching == TRUE){
      df_reg <- merge(data, matched_units, by = id_vars)
    }
    else{df_reg <- data}
    
    start_time <- Sys.time()
    
    if(type == "fepois"){
      es_stag_w_ctrl <- feglm(as.formula(formula_ctrl),
                               , data = df_reg 
                               ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                               ,cluster = id_vars, family = 'poisson'
      ) }
    if(type == 'feols'){
      es_stag_w_ctrl <- feols(as.formula(formula_ctrl),
                              , data = df_reg 
                              ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                              ,cluster = id_vars
      ) 
      
    }
    time_taken <- Sys.time()-start_time
    gc()
    print(time_taken)
    list_es_all[[var]][['regression']] <- es_stag_w_ctrl
    
    list_es_all[[var]][['table_agg']] <- agg_etwfe(es_stag_w_ctrl, df_reg, t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl :=  paste0( sort(trend_controls), collapse = "_")]
    list_es_all[[var]][['table_agg_by_t']] <-agg_etwfe_het(es_stag_w_ctrl, df_reg, by  ='t', t_limit = 5)%>%
      .[, var := var] %>% .[, ctrl :=  paste0( sort(trend_controls), collapse = "_")]
    list_es_all[[var]][['table_agg_by_g']] <- agg_etwfe_het(es_stag_w_ctrl, df_reg, by  ='g')%>%
      .[, var := var] %>% .[, ctrl :=  paste0( sort(trend_controls), collapse = "_")]
    
    for_pre_mean <- df_reg 
    if(type == "fepois"){
      for(id_var in id_vars){
        for_pre_mean <- for_pre_mean[! (for_pre_mean[[id_var]] %in% es_stag_w_ctrl$fixef_removed[[id_var]]) ]
      }}
    
    list_es_all[[var]][["pre_mean"]] <- round(mean((for_pre_mean[(for_pre_mean[["year"]] %in% c(2000:2009))])[[var]], na.rm =T),2)
    list_es_all[[var]][["n_obs"]] <- es_stag_w_ctrl$nobs
    if(type == "fepois"){
      list_es_all[[var]][["pseudo_r2"]] <- round(es_stag_w_ctrl$pseudo_r2, 5)
    }
    if(type == "feols"){ list_es_all[[var]][["pseudo_r2"]]<- round(r2(es_stag_w_ctrl)[['r2']], 5)}
    
    for(d in unique(list_es_all[[var]][['table_agg']]$treat)){
      print(d)
      event_study_plot <- ggplot(list_es_all[[var]][['table_agg_by_t']] %>% .[treatment ==d])+
        geom_point(aes(x= t, y = est))+
        geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
        geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
        labs(title = paste0('Treatment: ', dict_vars[[d]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
        theme_bw()
      if(plot_event_study == TRUE){
        print(event_study_plot)}
      if(save_event_study == TRUE){
        
        if(w_matching == TRUE){
          save_path_event_study <- paste0(save_path, '\\', d, '\\match_',paste0(sort(matching_variables), collapse = '_') ,'\\fe_',paste0( sort(trend_controls), collapse = "_"), '\\' )
          
        }
        else{
          save_path_event_study <- paste0(save_path, '\\', d, '\\fe_', paste0( sort(trend_controls), collapse = "_"), '\\' )
        }
        if (!file.exists(save_path_event_study)){
          dir.create(save_path_event_study, recursive = TRUE)
        }
        
        
        pdf(paste0(save_path_event_study , var, '_', d, '_by_t',".pdf"))
        print(event_study_plot)
        dev.off() 
      }
      
    }
    
  }
  return(list_es_all)
}




compute_separate_estimates_etwfe <- function(treatments = c('acces_rce','date_first_idex'),
                                       outcomes = NULL,
                                       data,
                                       id_vars = c('unit'),
                                       trend_controls = NULL,
                                       w_matching = TRUE,
                                       matching_variables = NULL,
                                       plot_event_study = FALSE,
                                       save_event_study = FALSE,
                                       save_path = '',
                                       type = 'fepois'
){
  list_es_solo = list()
  
  for(treat in treatments){
    list_es_solo[[treat]] <- list()
    sample_separate <- data
    
    #if(is.null(outcomes)){outcomes <- colnames(data)[str_detect(colnames(data), 'reweight')]}
    all_treatments = c('acces_rce', 'date_first_idex', 'fusion_date')
    
    for(d in all_treatments[all_treatments != treat]){
      sample_separate <- sample_separate[ (sample_separate[[paste0(d, '_r')]] == 0) & (sample_separate[[paste0(d, '_r')]] == 0 )]
    }

    ## Remove excluded values
    if(treat == 'acces_rce'){sample_separate <- sample_separate %>% .[!acces_rce_r %in% c(2013:2015) & ! acces_rce_s %in% c(2013:2015) 
                                                                      ]}
    
    treatment_values <- list( i = sort(unique(sample_separate[sample_separate[[paste0(treat, '_s')]] != 0][[paste0(treat, '_s')]])),
                              j = sort(unique(sample_separate[sample_separate[[paste0(treat, '_r')]] != 0][[paste0(treat, '_r')]])))
    
    formula_elements <- c()
    for(g_i in treatment_values$i){
      print(paste0(treat, ': ', g_i))
      varname =paste0(treat, '_s_g', g_i)
      ref = as.character(as.numeric(g_i)-1)
      sample_separate[[varname]] <- as.numeric((sample_separate[[paste0(treat, '_s')]] == g_i)
                                               & (sample_separate[['merged_inst_id_r']] != "abroad")
                                               & (sample_separate[['merged_inst_id_r']] != "exit")
      )
      
      formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
      
      varname_abroad =paste0(treat, '_a_s_g', g_i)
      sample_separate[[varname_abroad]] <- as.numeric((sample_separate[[paste0(treat, '_s')]] == g_i)
                                                      & (sample_separate[['merged_inst_id_r']] == "abroad")
      )
      formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
      varname_exit =paste0(d, '_e_s_g', g_i)
      ds_clean[[varname_exit]] <- as.numeric((ds_clean[[paste0(d, '_s')]] == g_i)
                                             & (ds_clean[['merged_inst_id_r']] == "exit")
      )
      formula_elements <- c(formula_elements, paste0(varname_exit, ' + i(year,', varname_exit, ',ref=',ref,')'))
      
    } 
    for(g_j in treatment_values$j){
      print(paste0(treat, ': ', g_j))
      varname =paste0(treat, '_r_g', g_j)
      ref = as.character(as.numeric(g_j)-1)
      sample_separate[[varname]] <- as.numeric((sample_separate[[paste0(treat, '_r')]] == g_j)
                                               & (sample_separate[['merged_inst_id_s']] != "abroad")
                                               & (sample_separate[['merged_inst_id_s']] != "entry")
      )
      formula_elements <- c(formula_elements, paste0(varname, ' + i(year,', varname, ',ref=',ref,')'))
      
      varname_abroad =paste0(treat, '_a_r_g', g_j)
      sample_separate[[varname_abroad]] <- as.numeric((sample_separate[[paste0(treat, '_r')]] == g_j)
                                                      & (sample_separate[['merged_inst_id_s']] == "abroad")
      )
      formula_elements <- c(formula_elements, paste0(varname_abroad, ' + i(year,', varname_abroad, ',ref=',ref,')'))
      
      varname_entry =paste0(treat, '_e_r_g', g_j)
      sample_separate[[varname_entry]] <- as.numeric((sample_separate[[paste0(treat, '_r')]] == g_j)
                                                     & (sample_separate[['merged_inst_id_s']] == "entry")
      )
      formula_elements <- c(formula_elements, paste0(varname_entry, ' + i(year,', varname_entry, ',ref=',ref,')'))
    }  
    
    length(formula_elements)
    
    print(formula_elements)
    units <- unique(sample_separate[, ..unit_cols]) %>%
      .[, treat := as.numeric(acces_rce_r != 0 | date_first_idex_r!=0 | fusion_date_r != 0
                              | acces_rce_s != 0 | date_first_idex_r!=0 | fusion_date_r != 0
      )]
    print(table(units[[paste0(treat, '_r')]], units[[paste0(treat, '_s')]]))
    
    formula_ctrl <- paste0( 'y ~ ',  
                            paste0(formula_elements, collapse= '+'), 
                            '|  year + merged_inst_id_r^domain  +merged_inst_id_s^domain +',
                            paste0(id_vars, collapse = '+')
    )
    if(!is.null(trend_controls)){
      formula_ctrl <- paste0(formula_ctrl, "+",
                             paste0(paste0(trend_controls, '^year'), collapse = '+'))
    }
    
    if(w_matching == TRUE){
      
      formula_ctrl <- paste0(formula_ctrl,  '+ subclass^year')
      
      matching_fml = paste0('treat ~ ', paste0(matching_variables, collapse = "+"))
      match_units <- matchit(as.formula(matching_fml)
                             ,data = units
                             ,method = "exact")
      matched_units <- match.data(match_units)
      print(table(matched_units[[paste0(treat, '_r')]], matched_units[[paste0(treat, '_s')]]))
      
      to_keep_in_matched_units <- c(id_vars, "subclass")
      
      matched_units <- matched_units %>%
        .[, ..to_keep_in_matched_units]
      
    }
    for(var in outcomes ){
      
      list_es_solo[[treat]][[var]] <- list()
      
      sample_separate$y <- sample_separate[[var]]
      
      if(w_matching == TRUE){
        df_reg <- merge(sample_separate, matched_units, by = id_vars)
      }
      else{df_reg <- sample_separate}
      
      start_time <- Sys.time()
      
      if(type == "fepois"){
        es_stag_w_ctrl <- fepois(as.formula(formula_ctrl),
                                 , data = df_reg 
                                 ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                                 ,cluster = id_vars
        ) }
      if(type == 'feols'){
        es_stag_w_ctrl <- feols(as.formula(formula_ctrl),
                                , data = df_reg 
                                ,mem.clean = TRUE,lean = TRUE,fixef.tol = 1E-2
                                ,cluster = id_vars
        ) 
        
      }
      time_taken <- Sys.time()-start_time
      gc()
      print(time_taken)
      list_es_solo[[treat]][[var]][['regression']] <- es_stag_w_ctrl
      
      list_es_solo[[treat]][[var]][['table_agg']] <- agg_etwfe(es_stag_w_ctrl, df_reg, t_limit = 5)%>%
        .[, var := var] %>% .[, ctrl :=  paste0( sort(trend_controls), collapse = "_")]
      list_es_solo[[treat]][[var]][['table_agg_by_t']] <- agg_etwfe_het(es_stag_w_ctrl, df_reg, by  ='t', t_limit = 5)%>%
        .[, var := var] %>% .[, ctrl :=  paste0( sort(trend_controls), collapse = "_")]
      list_es_solo[[treat]][[var]][['table_agg_by_g']] <- agg_etwfe_het(es_stag_w_ctrl, df_reg, by  ='g')%>%
        .[, var := var] %>% .[, ctrl :=  paste0( sort(trend_controls), collapse = "_")]
      
      for_pre_mean <- df_reg 
      if(type == "fepois"){
        for(id_var in id_vars){
          for_pre_mean <- for_pre_mean[! (for_pre_mean[[id_var]] %in% es_stag_w_ctrl$fixef_removed[[id_var]]) ]
        }}
      
      list_es_solo[[treat]][[var]][["pre_mean"]] <- round(mean((for_pre_mean[(for_pre_mean[["year"]]<= for_pre_mean[[treat]] | for_pre_mean[[treat]] == 0)])[[var]], na.rm =T),2)
      list_es_solo[[treat]][[var]][["n_obs"]] <- es_stag_w_ctrl$nobs
      if(type == "fepois"){
        list_es_solo[[treat]][[var]][["pseudo_r2"]] <- round(es_stag_w_ctrl$pseudo_r2, 5)
      }
      if(type == "feols"){  list_es_solo[[treat]][[var]][["pseudo_r2"]]<- round(r2(es_stag_w_ctrl)[['r2']], 5)}
      for(subtreat in unique(list_es_solo[[treat]][[var]][['table_agg_by_t']]$treatment)){
        
        event_study_plot <- ggplot(list_es_solo[[treat]][[var]][['table_agg_by_t']] %>% .[treatment == subtreat])+
          geom_point(aes(x= t, y = est))+
          geom_errorbar(aes(x=t, ymin = est -1.96*std, ymax=est+1.96*std))+
          geom_vline(aes(xintercept = "-1"), linetype = "dashed")+geom_hline(aes(yintercept = 0))+
          labs(title = paste0('Treatment: ', dict_vars[[subtreat]]))+xlab('Time to treatment')+ ylab('Estimate and 95% CI')+
          theme_bw()
        if(plot_event_study == TRUE){
          print(event_study_plot)}
        if(save_event_study == TRUE){
          
          if(w_matching == TRUE){
            save_path_event_study <- paste0(save_path, '\\', treat, '\\match_',paste0(sort(matching_variables), collapse = '_') ,'\\fe_',paste0( sort(trend_controls), collapse = "_"), '\\' )
            
          }
          else{
            save_path_event_study <- paste0(save_path, '\\', subtreat, '\\fe_', paste0( sort(trend_controls), collapse = "_"), '\\' )
          }
          if (!file.exists(save_path_event_study)){
            dir.create(save_path_event_study, recursive = TRUE)
          }
          
          
          pdf(paste0(save_path_event_study , var, '_', treat, '_by_t',".pdf"))
          print(event_study_plot)
          dev.off() 
        }}
      
    }
  }
  return(list_es_solo)
}


unit_cols <- c("merged_inst_id_r",'merged_inst_id_s', 'unit',
               'domain',# 'name_r','name_s', 
               'acces_rce_r', 'date_first_idex_r','fusion_date_r',
               'acces_rce_s', 'date_first_idex_s','fusion_date_s',
               "interact_rce_idex_r", 'interact_rce_idex_s',
               'city_r', 'city_s', 'type_r','type_s',#'public_r','public_s',
               'ecole_r','ecole_s',#'cnrs_r', 'cnrs_s',
               "quant_size_r_2003","quant_size_s_2003",'secteur_s','secteur_r'
)


dict_vars <- c("movers_w"                  =     'Total flows',
               "movers_w_junior"           =     'Junior researcher flows',
               "movers_w_senior"           =     'Senior researcher flows',
               "movers_w_medium"           =     'Medium researcher flows',
               "movers_w_own_entrant_r"    =     'Returning to entry institution',
               "movers_w_own_entrant_s"    =     'Exiting from entry institution',
               "movers_w_foreign_entrant"  =     'Foreign entrant flows', 
               
               
               "movers_w_Q1__cit_2y"       =     'Q1 of productivity (first 2y)', 
               "movers_w_Q2__cit_2y"       =     'Q2 of productivity (first 2y)', 
               "movers_w_Q3__cit_2y"       =     'Q3 of productivity (first 2y)', 
               "movers_w_Q4__cit_2y"       =     'Q4 of productivity (first 2y)', 

               
               "acces_rce"= "Administrative autonomy",
               "date_first_idex"= "Received an IDEX",
               "fusion_date"= "Merged establishment",
               
               'delta_acces_rce' = "Administrative autonomy, departures",
               'delta_acces_rce_a' = "Administrative autonomy, departures abroad",
               'delta_acces_rce_e' = "Administrative autonomy, exit",
               'gamma_acces_rce' = "Administrative autonomy, arrivals",
               'gamma_acces_rce_a' = "Administrative autonomy, arrivals from abroad",
               'gamma_acces_rce_e' = "Administrative autonomy, entry",
               
               'delta_date_first_idex' =   "Received an IDEX, departures",
               'delta_date_first_idex_a' = "Received an IDEX, departures abroad",
               'delta_date_first_idex_e' = "Received an IDEX, exit",
               'gamma_date_first_idex' =   "Received an IDEX, arrivals",
               'gamma_date_first_idex_a' = "Received an IDEX, arrivals from abroad",
               'gamma_date_first_idex_e' = "Received an IDEX, entry",
               
               'delta_interact_rce_idex'   = "Interaction autonomy and IDEX, departures",
               'delta_interact_rce_idex_a' = "Interaction autonomy and IDEX, departures abroad",
               'delta_interact_rce_idex_e' = "Interaction autonomy and IDEX, exit",
               'gamma_interact_rce_idex'   = "Interaction autonomy and IDEX, arrivals",
               'gamma_interact_rce_idex_a' = "Interaction autonomy and IDEX, arrivals from abroad",
               'gamma_interact_rce_idex_e' = "Interaction autonomy and IDEX, entry",
               
               'delta_fusion_date' =   "Merged establishment, departures",
               'delta_fusion_date_a' = "Merged establishment, departures abroad",
               'delta_fusion_date_e' = "Merged establishment, exit",

               'gamma_fusion_date' =   "Merged establishment, arrivals",
               'gamma_fusion_date_a' = "Merged establishment, arrivals from abroad",
               'gamma_fusion_date_e' = "Merged establishment, entry",
               
               '| year' = 'Year',
               'merged_inst_id_r' = 'Destination institution',
               'merged_inst_id_s' = 'Origin institution',
               'unit' = 'Institution pair',
               'type_s' = 'Type of origin institution',
               'type_r' = 'Type of destination institution',
               'city_s' = 'City of origin institution',
               'city_r' = 'City of destination institution'
)


make_stargazer_like_table_dt <- function(dt,
                                         digits = 3,
                                         note = NULL,
                                         table_caption = NULL,
                                         table_label = NULL,
                                         save_path = NULL,
                                         pre_mean = NULL,
                                         n_obs = NULL,
                                         r_2 = NULL,
                                         var_map = NULL,
                                         treat_map = NULL,
                                         var_order = NULL,
                                         drop_unlisted_vars = FALSE,
                                         add_stars = TRUE,                    # NEW
                                         star_levels = c(0.1, 0.05, 0.01),    # NEW (ascending)
                                         star_symbols = c("*", "**", "***"),  # NEW
                                         star_note = "*, **, and *** denote significance at the 10%, 5%, and 1% levels, respectively." # NEW
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
  
  req <- c("treat","est","std","t","pvalue","var","ctrl")
  miss <- setdiff(req, names(dt))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  fmt_num <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
  latexify <- function(s) {
    s <- as.character(s)
    s <- gsub("\\\\", "\\\\textbackslash{}", s)
    s <- gsub("([%&_#\\{}\\$\\^~])", "\\\\\\1", s, perl = TRUE)
    s
  }
  apply_map <- function(x, m) {
    if (is.null(m)) return(x)
    if (is.list(m)) m <- unlist(m, use.names = TRUE)
    if (is.null(names(m)) || any(is.na(names(m)))) stop("var_map/treat_map must be named.")
    idx <- match(x, names(m)); out <- x; repl <- !is.na(idx); out[repl] <- unname(m[idx[repl]]); out
  }
  
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
  
  # ---- helper: number of controls in a 'ctrl' string (treats interactions as one) ----
  control_term_count <- function(ctrl_str) {
    if (is.na(ctrl_str)) return(0L)
    s <- tolower(trimws(ctrl_str))
    if (!nzchar(s) || s == "none") return(0L)
    parts <- trimws(unlist(strsplit(s, "\\+")))
    parts <- parts[nzchar(parts)]
    return(length(parts))
  }
  
  # ---------------- Models & ordering ----------------
  dt[, model_id := paste0(var, " | ", ctrl)] 
  dt <- dt %>% .[, d:=str_extract(treat, 'acces_rce|fusion_date|date_first_idex')]
  models <- unique(dt[, .(var, ctrl)])
  
  # Count controls per model for sorting within var
  models[, ctrl_count := vapply(ctrl, control_term_count, FUN.VALUE = integer(1))]
  
  # Apply explicit var order (groups), then order within each var by ctrl_count asc
  if (!is.null(var_order)) {
    if (drop_unlisted_vars) {
      models <- models[var %in% var_order]
      dt     <- dt[var %in% var_order]
    }
    models[, var__rank := fifelse(var %in% var_order, match(var, var_order), .N + 1L)]
    data.table::setorder(models, var__rank, ctrl_count, ctrl)
    models[, var__rank := NULL]
  } else {
    data.table::setorder(models, var, ctrl_count, ctrl)
  }
  
  models[, model_id := paste0(var, " | ", ctrl)]
  model_ids <- models$model_id
  
  # ---------------- Wide blocks ----------------
  W_est <- data.table::dcast(dt, treat ~ model_id, value.var = "est")
  W_se  <- data.table::dcast(dt, treat ~ model_id, value.var = "std")
  W_p   <- data.table::dcast(dt, treat ~ model_id, value.var = "pvalue")

  ensure_cols <- function(W) {
    if (!("treat" %in% names(W))) W[, treat := NA_character_]
    missing <- setdiff(model_ids, setdiff(names(W), "treat"))
    if (length(missing)) W[, (missing) := NA_real_]
    data.table::setcolorder(W, c("treat", model_ids))
    W[order(treat)]
  }
  data.table::setDT(W_est); W_est <- ensure_cols(W_est)
  data.table::setDT(W_se ); W_se  <- ensure_cols(W_se)
  data.table::setDT(W_p  ); W_p   <- ensure_cols(W_p)

  # ---- Stars matrix (same shape as W_est minus 'treat') ----
  make_star <- function(p) {
    if (!add_stars || is.na(p)) return("")
    # pick the *strongest* symbol matching the smallest cutoff satisfied
    sym <- ""
    for (k in rev(seq_along(star_levels))) {
      if (!is.na(p) && p <= star_levels[k]) { sym <- star_symbols[k]; break }
    }
    sym
  }
  W_star <- copy(W_p)
  for (cn in setdiff(names(W_star), "treat")) {
    W_star[[cn]] <- vapply(W_star[[cn]], make_star, FUN.VALUE = character(1))
  }
  W_star <- ensure_cols(W_star)
  
  # ---------------- Labels & body ----------------
  models[, display_var := apply_map(var, var_map)]
  d_all <- unique(dt$d)
  treats_all <-  list()
  for(d_ in d_all){
    treats_all[[d_]] <- unique((dt[d == d_])$treat )
  }

  build_rows_for_treat <- function(tr) {
    e   <- W_est[treat == tr, ..model_ids]
    se  <- W_se [treat == tr, ..model_ids]
    p   <- W_p  [treat == tr, ..model_ids]
    st  <- W_star[treat == tr, ..model_ids]
    
    # estimates with stars appended
    if (nrow(e) == 0) {
      est_with_stars <- rep("", length(model_ids))
    } else {
      est_with_stars <- paste0(fmt_num(unlist(e)), unlist(st))
    }
    to_vec <- function(D) if (nrow(D) == 0) rep("", length(model_ids)) else fmt_num(unlist(D))
    
    lhs <- latexify(unname(treat_map[[tr]] %||% tr))
    lhs <- paste0('\\textbf{', str_to_sentence(str_replace(lhs,  'Administrative autonomy, |Received an IDEX, |Merged establishment, ', '' )),'}')
    line  <- function(lhs, vec) paste(lhs, paste(vec, collapse = " & "), sep = " & ")
    
    c(
      paste0(line(lhs,           est_with_stars), '\\\\'),
      paste0(line("Std. error",  paste0('(',to_vec(se), ')')), '\\\\'),
      "\\addlinespace",
      paste0(line("\\textit{p-value}",     paste0('\\textit{', to_vec(p), '}')), '\\\\'),
     # paste0(line("\\textit{Pretrend p}",  paste0('\\textit{', to_vec(pr), '}')), '\\\\'),
      "\\addlinespace"
    )
  }
  body_lines <- list()
  for(d in d_all){
  body_lines[[d]] <- c( paste0('\\textbf{', treat_map[[d]], '} & ', paste0(rep("", length(model_ids)), collapse = ' & '), '\\\\'),
                        "\\addlinespace" ,
                            unlist(lapply(treats_all[[d]], build_rows_for_treat), use.names = FALSE),
                        "\\midrule"
  )
  }
  body_lines <- unlist(body_lines, use.names = FALSE)
  # ---------------- Controls presence matrix ----------------
  map_control_terms <- function(ctrl_str) {
    if (is.na(ctrl_str) || trimws(ctrl_str) == "" || tolower(trimws(ctrl_str)) == "none") return(character(0))
    parts <- trimws(unlist(strsplit(ctrl_str, "\\+")))
    parts <- parts[nzchar(parts)]
    vapply(parts, function(term) {
      toks <- trimws(unlist(strsplit(term, "\\^")))
      toks <- apply_map(toks, var_map)
      toks <- latexify(toks)
      if (length(toks) > 1L) paste(toks, collapse = " $\\times$ ") else toks
    }, FUN.VALUE = character(1))
  }
  model_ctrl_labels <- lapply(models$ctrl, map_control_terms)
  
  unique_controls <- {
    seen <- character(0)
    for (vec in model_ctrl_labels) for (lab in vec) if (!(lab %in% seen)) seen <- c(seen, lab)
    seen
  }
  
  control_rows <- character(0)
  if (length(unique_controls)) {
    control_rows <- c(control_rows, paste("Controls", paste(rep("", length(model_ids)), collapse = " & "), sep = " & "))
    for (lab in unique_controls) {
      xs <- vapply(model_ctrl_labels, function(v) if (lab %in% v) "Yes" else "", FUN.VALUE = character(1))
      control_rows <- c(control_rows, paste(lab, paste(xs, collapse = " & "), sep = " & "))
    }
  }
  
  # ---------------- Headers ----------------
  r <- rle(models$display_var)
  header_top <- paste(
    c("", mapply(function(lbl, k) sprintf("\\multicolumn{%d}{c}{\\textit{%s}}", k, latexify(lbl)), r$values, r$lengths)),
    collapse = " & "
  )
  pre_means_line= paste0('\\textit{Pre-2009 avg.} & ', paste0(pre_mean[models$var], collapse= '&'))
  n_obs_line= paste0('N & ', paste0(n_obs[models$var], collapse= '&'))
  r_2_line= paste0('Pseudo R2 & ', paste0(r_2[models$var], collapse= '&'))
  
  # ---------------- Assemble LaTeX ----------------
  K <- nrow(models)
  colspec <- paste0("l", paste(rep("c", K), collapse = ""))
  # Notes: append star legend if none provided; or append to your note.
  final_note <- if (add_stars) {
    if (is.null(note) || !nzchar(note)) star_note else paste(note, star_note)
  } else note
  
  lines <- c(
    sprintf("\\begin{tabular}{%s}", colspec),
    "\\toprule",
    paste0(header_top, " \\\\"),
    "\\midrule",
    body_lines,
    "\\midrule",
    if (length(control_rows)) paste0(control_rows, " \\\\") else NULL,
    "\\midrule",
    paste0(pre_means_line, " \\\\"),
    paste0(n_obs_line, " \\\\"),
    paste0(r_2_line, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  tex <- paste(lines[!is.na(lines)], collapse = "\n")
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(tex, con = save_path, useBytes = TRUE)
  }
  tex
}

