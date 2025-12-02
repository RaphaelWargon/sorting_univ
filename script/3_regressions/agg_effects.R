agg_effects <- function(stag_model, data, R = 0, t_limit = 0){
  coefs <- as.data.table(stag_model$coefficients, keep.rownames = TRUE)
  colnames(coefs) <-c("var",'est')
  coefs <- coefs %>%
    .[ str_detect(var, '(?<=[0-9]:)[a-z_]+(?=[0-9])')]%>%
    .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
   # .[, d := str_extract(var, '(?<=year[0-9]{4}:)[a-z_]+(?=[0-9])|^[a-z_]+(?=[0-9]{4}:year)')]%>%
    .[, g := str_extract(var, paste0('(?<=' , d, '_)[0-9]{4}')) ] %>%
     .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
   # .[, year := str_extract(var, '(?<=year)[0-9]{4}')] %>%
    .[, t := as.numeric(year)-as.numeric(g)]   
  if(t_limit !=0){
    coefs <- coefs[abs(t)<= t_limit]
  }
  
  all_treatments = unique(coefs$d)
  etwfe_agg <- data.table(treat = '', est = 0, std = 0, t = 0, pvalue = 0, pvalue_pretrend = 0, type = '') %>% .[treat != '']
  for(trt in all_treatments){
    print(trt)
    tryCatch({
      
      ##### SUNAB weights
      to_sum <- coefs %>% .[d == trt & t>0] 
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
      
      
      to_sum_pre <- coefs %>% .[d == trt & t<0] 
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
                                              pvalue = pval, pvalue_pretrend = pval_pre, type = 'sunab') )
      
      
      
      
    },
    error = function(cond) {
      message(conditionMessage(cond))
      NA
    }
    )
  }
  
  return(etwfe_agg)
}





agg_effects_ch <- function(stag_model, data, t_switch = 1, t_comp = -1, R = 0, t_limit = 0){
  coefs <- as.data.table(stag_model$coefficients, keep.rownames = TRUE)
  colnames(coefs) <-c("var",'est')
  coefs <- coefs %>%
    #.[ str_detect(var, 'year[0-9]{4}')]%>%
    #.[, d := str_extract(var, '(?<=year[0-9]{4}:)[a-z_]+(?=[0-9])|^[a-z_]+(?=[0-9]{4}:year)')]%>%
    #.[, g := str_extract(var, paste0('(?<=' , d, ')[0-9]{4}')) ] %>%
    #.[, year := str_extract(var, '(?<=year)[0-9]{4}')] %>%
    .[ str_detect(var, '(?<=[0-9]:)[a-z_]+(?=[0-9])')]%>%
    .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
    .[, g := str_extract(var, paste0('(?<=' , d, '_)[0-9]{4}')) ] %>%
    .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%

    .[, t := as.numeric(year)-as.numeric(g)]   
  
  all_treatments = unique(coefs$d)
  etwfe_agg <- data.table(treat = '', est = 0, std = 0, t = 0, pvalue = 0, type = '') %>% .[treat != '']
  for(trt in all_treatments){
    print(trt)
    tryCatch({
      
      ##### CH weights
      to_sum <- coefs %>% .[d == trt] 
      var = to_sum$d[[1]]
      w <- data %>%
        .[, .(n  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)]
      colnames(w) <- c('g', 'year', 'n')
      to_sum <- merge(to_sum %>%
                        .[, ':='(g =as.character(g),
                                 year = as.character(year))],
                      w%>%
                        .[, ':='(g =as.character(g),
                                 year = as.character(year))], by = c('g','year') ) 
      N_d_D = sum(w[year==g]$n)
      to_sum <- to_sum %>%
        .[, w := case_when(t == t_switch ~  1/N_d_D,
                          t == t_comp ~ -1/N_d_D, ##switch
                          .default = 0
                          ), by ='year'] %>%
        .[, ':='(n_not_yet_treated=sum( (as.numeric(t) <0 )*n ),
                 plus = sum(t ==t_switch ),
                 minus = sum(t == t_comp)), by = 'g'] %>%
      .[, w := ifelse(t<0, - ( plus - minus ) * 1/N_d_D * n/n_not_yet_treated ,w) ] 
      
      aggte = sum(to_sum$w * to_sum$est)
      cov_effect <- vcov(stag_model)[to_sum$var, to_sum$var]
      aggte_se <- sqrt(t(to_sum$w)%*% cov_effect %*% to_sum$w)[1,1]
      aggte_t <- (aggte-R)/aggte_se
      pval = dt(aggte_t, degrees_freedom(es_stag, type = 't'))
      etwfe_agg = rbind(etwfe_agg, data.table(treat = trt, est = aggte, std = aggte_se, t = aggte_t,
                                              pvalue = pval, type = 'ch') )
      
      
      
      
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
    #.[ str_detect(var, 'year[0-9]{4}')]%>%
    #.[, d := str_extract(var, '(?<=year[0-9]{4}:)[a-z_]+(?=[0-9])|^[a-z_]+(?=[0-9]{4}:year)')]%>%
    #.[, g := str_extract(var, paste0('(?<=' , d, ')[0-9]{4}')) ] %>%
    #.[, year := str_extract(var, '(?<=year)[0-9]{4}')] %>%
    .[ str_detect(var, '(?<=[0-9]:)[a-z_]+(?=[0-9])')]%>%
    .[, d := str_extract(var, '(?<=[0-9]:)[a-z_]+(?=_[0-9])')]%>%
    .[, g := str_extract(var, paste0('(?<=' , d, '_)[0-9]{4}')) ] %>%
    .[, year := str_extract(var, '(?<=year::)[0-9]{4}')] %>%
    .[, quant := str_extract(var, '(?<=quantile_au_fe)[0-9]')] %>%
    .[, quant := fifelse(is.na(quant), 'global', quant)] %>%
    .[, t := as.numeric(year)-as.numeric(g)] 
  #coefs$treat <- paste0(coefs$treat, '_', by, coefs[[by]])
  vcov_st_m <- vcov(stag_model)
  all_treatments = unique(coefs$d)
  etwfe_by <- data.table(treat = '', est = 0, std = 0, t_value=0, p_value = 0, by = 0, n = 0) %>% .[treat != '']
  
  if(by !='t'){
    coefs <- coefs[t >0]
  
  }
  for(trt in all_treatments){
    print(trt)
    start_time_trt = Sys.time()
    to_sum <- coefs %>% .[d == trt] 
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
    all_values=  unique(to_sum[[by]])
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
    if(by =='t'){
      ref = ''
      for( el in min(all_values):max(all_values)){
        if(!el %in% all_values){
          ref = el
        }
      }
      print(ref)
      if(ref != ''){
      etwfe_by = rbind(etwfe_by, data.table(treat = trt, est = 0, std = NA, t_value = 0, p_value= 0, by = ref, n = n_by) )}
    }
  }
  colnames(etwfe_by) <- c('treatment','est','std', 't_value','p_value', by, 'n')
  return(etwfe_by)
}


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
  
  req <- c("treat","est","std","t","pvalue","pvalue_pretrend","type","var","ctrl")
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
  W_pr  <- data.table::dcast(dt, treat ~ model_id, value.var = "pvalue_pretrend")
  
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
  data.table::setDT(W_pr ); W_pr  <- ensure_cols(W_pr)
  
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
  treats_all <- sort(unique(dt$treat))
  treat_display <- setNames(apply_map(treats_all, treat_map), treats_all)
  
  build_rows_for_treat <- function(tr) {
    e   <- W_est[treat == tr, ..model_ids]
    se  <- W_se [treat == tr, ..model_ids]
    p   <- W_p  [treat == tr, ..model_ids]
    pr  <- W_pr [treat == tr, ..model_ids]
    st  <- W_star[treat == tr, ..model_ids]
    
    # estimates with stars appended
    if (nrow(e) == 0) {
      est_with_stars <- rep("", length(model_ids))
    } else {
      est_with_stars <- paste0(fmt_num(unlist(e)), unlist(st))
    }
    to_vec <- function(D) if (nrow(D) == 0) rep("", length(model_ids)) else fmt_num(unlist(D))
    
    lhs <- latexify(unname(treat_display[[tr]] %||% tr))
    line  <- function(lhs, vec) paste(lhs, paste(vec, collapse = " & "), sep = " & ")
    
    c(
      paste0(line(paste0("\\textbf{", lhs, "}"),           est_with_stars), '\\\\'),
      paste0(line("Std. error",  paste0('(',to_vec(se), ')')), '\\\\'),
      "\\addlinespace",
      paste0(line("\\textit{p-value}",     paste0('\\textit{', to_vec(p), '}')), '\\\\'),
      paste0(line("\\textit{Pretrend p}",  paste0('\\textit{', to_vec(pr), '}')), '\\\\'),
      "\\addlinespace"
    )
  }
  treats <- unique(dt[order(treat), treat])
  body_lines <- unlist(lapply(treats, build_rows_for_treat), use.names = FALSE)
  
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
  n_obs_line= paste0('N & ', paste0(n_obs[models$model_id], collapse= '&'))
  r_2_line= paste0('Pseudo R2 & ', paste0(r_2[models$model_id], collapse= '&'))
  
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
