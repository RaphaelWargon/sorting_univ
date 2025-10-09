agg_effects <- function(stag_model, data, R = 0, t_limit = 0){
  coefs <- as.data.table(stag_model$coefficients, keep.rownames = TRUE)
  colnames(coefs) <-c("var",'est')
  coefs <- coefs %>%
    .[str_detect(var, '(?<=i_|j_)[a-z_]*(?=_g)')]%>%
    .[, d := str_extract(var, '(?<=i_|j_)[a-z_]*(?=_g)')] %>%
    .[, type := 'beta'] %>%
    .[, g := str_extract(var, '(?<=g)[0-9]{1,4}')] %>%
    .[, year := str_extract(var, '(?<=y)[0-9]{4}')] %>%
    .[, t := as.numeric(year)-as.numeric(g)] %>%
    .[, treat := ifelse(type!='',
                        paste0(type, '_', d), d)]
  
  if(t_limit !=0){
    coefs <- coefs[abs(t)<= t_limit]
  }
  
  all_treatments = unique(coefs$treat)
  etwfe_agg <- data.table(treat = '', est = 0, std = 0, t = 0, pvalue = 0, pvalue_pretrend = 0) %>% .[treat != '']
  for(trt in all_treatments){
    print(trt)
    tryCatch({
      to_sum <- coefs %>% .[treat == trt & g!="0" & t>0] 
      var = to_sum$d[[1]]
      w <- data %>%
          .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)]
      colnames(w) <- c('g', 'year', 'w')
      to_sum <- merge(to_sum %>%
                        .[, ':='(g =as.character(g),
                                 year = as.character(year))],
                      w%>%
                        .[, ':='(g =as.character(g),
                                 year = as.character(year))], by = c('g','year') ) %>%
        .[, w := w/sum(w)]
      aggte = sum(to_sum$w * to_sum$est)
      cov_effect <- vcov(stag_model)[to_sum$var, to_sum$var]
      aggte_se <- sqrt(t(to_sum$w)%*% cov_effect %*% to_sum$w)[1,1]
      aggte_t <- (aggte-R)/aggte_se
      pval = dt(aggte_t, degrees_freedom(es_stag, type = 't'))
      
      to_sum_pre <- coefs %>% .[treat == trt & g!="0" & t<0] 
      var = to_sum_pre$d[[1]]
      w <- data %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)]
      colnames(w) <- c('g', 'year', 'w')
      to_sum_pre <- merge(to_sum_pre %>%
                        .[, ':='(g =as.character(g),
                                 year = as.character(year))],
                      w%>%
                        .[, ':='(g =as.character(g),
                                 year = as.character(year))], by = c('g','year') ) %>%
        .[, w := w/sum(w)]
      aggte_pre = sum(to_sum_pre$w * to_sum_pre$est)
      cov_effect_pre <- vcov(stag_model)[to_sum_pre$var, to_sum_pre$var]
      aggte_se_pre <- sqrt(t(to_sum_pre$w)%*% cov_effect_pre %*% to_sum_pre$w)[1,1]
      aggte_t_pre <- (aggte_pre-R)/aggte_se_pre
      pval_pre = dt(aggte_t_pre, degrees_freedom(es_stag, type = 't'))
      etwfe_agg = rbind(etwfe_agg, data.table(treat = trt, est = aggte, std = aggte_se, t = aggte_t,
                                              pvalue = pval, pvalue_pretrend = pval_pre) )
    },
    error = function(cond) {
      message(conditionMessage(cond))
      NA
    }
    )
  }
  
  return(etwfe_agg)
}

agg_effect_het <- function(stag_model, data, by = 't'){
  coefs <- as.data.table(stag_model$coefficients, keep.rownames = TRUE)
  colnames(coefs) <-c("var",'est')
  coefs <- coefs %>%
    .[str_detect(var, '(?<=i_|j_)[a-z_]*(?=_g)')]%>%
    .[, d := str_extract(var, '(?<=i_|j_)[a-z_]*(?=_g)')] %>%
    .[, type := 'beta'] %>%
    .[, g := str_extract(var, '(?<=g)[0-9]{1,4}')] %>%
    .[, year := str_extract(var, '(?<=y)[0-9]{4}')] %>%
    .[, t := as.numeric(year)-as.numeric(g)] %>%
    .[, treat := ifelse(type!='',
                        paste0(type, '_', d), d)]
  #coefs$treat <- paste0(coefs$treat, '_', by, coefs[[by]])
  vcov_st_m <- vcov(stag_model)
  all_treatments = unique(coefs$treat)
  etwfe_by <- data.table(treat = '', est = 0, std = 0, t_value=0, p_value = 0, by = 0, n = 0) %>% .[treat != '']
  
  if(by !='t'){
    coefs <- coefs[t >0]  
  }
  for(trt in all_treatments){
    print(trt)
    start_time_trt = Sys.time()
    to_sum <- coefs %>% .[treat == trt] 
    var = to_sum$d[[1]]
    w <- data %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)] 
    colnames(w) <- c('g', 'year', 'w')
    to_sum <- merge(to_sum %>%
                      .[,year:=as.character(year)] %>%
                      .[, g:=as.character(g)],
                    w %>%
                      .[, g :=as.character(g)], by = c('g','year') )
    for(val in unique(to_sum[[by]])){
      print(val)
      to_sum_by = to_sum[to_sum[[by]] == val] 
      n_by = sum(to_sum_by$w)
      to_sum_by = to_sum_by %>%
        .[, w:= w/sum(w)]
      aggte = sum(to_sum_by$w * to_sum_by$est)
      cov_effect <- vcov_st_m[to_sum_by$var, to_sum_by$var]
      aggte_se <- sqrt(t(to_sum_by$w)%*% cov_effect %*% to_sum_by$w)[1,1]
      aggte_t <- (aggte)/aggte_se
      pval = dt(aggte_t, degrees_freedom(es_stag, type = 't'))
      etwfe_by = rbind(etwfe_by, data.table(treat = trt, est = aggte, std = aggte_se, t_value = aggte_t, p_value= pval, by = val, n = n_by) )}
    time_trt = Sys.time()-start_time_trt
  }
  colnames(etwfe_by) <- c('treatment','est','std', 't_value','p_value', by, 'n')
  return(etwfe_by)
}
