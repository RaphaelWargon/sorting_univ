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
  
  all_treatments = unique(coefs$treat)
  etwfe_agg <- data.table(treat = '', est = 0, std = 0, t = 0, pvalue = 0) %>% .[treat != '']
  for(trt in all_treatments){
    print(trt)
    tryCatch({
    to_sum <- coefs %>% .[treat == trt & g!="0" & t>0] 
    var = paste0(str_remove(to_sum$d[[1]], '^a_'), ifelse(str_detect(trt, 'delta'), '_s','_r'))
    if(str_detect(trt, '_a')){
      w <- data %>%
        .[merged_inst_id_s == 'abroad' | merged_inst_id_r == 'abroad'] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>% .[,year := as.character(year)]
    }
    else{
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
  all_treatments = unique(coefs$treat)
  etwfe_by <- data.table(treat = '', est = 0, std = 0, t_value=0, p_value = 0, by = 0) %>% .[treat != '']
  if(t_limit !=0){
    coefs <- coefs[, t := case_when(t > t_limit ~ t_limit +1,
                                    t < -t_limit ~ -t_limit -1,
                                    .default = t)]
  }
  
  if(by !='t'){
  coefs <- coefs[t >0]  
  }
    for(trt in all_treatments){
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
    else{
      w <- data %>%
        .[merged_inst_id_s != 'abroad' & merged_inst_id_r != 'abroad'] %>%
        .[, .(w  = .N), by = c(var, "year") ] %>%
        .[,year := as.character(year)] 
    }
    colnames(w) <- c('g', 'year', 'w')
    to_sum <- merge(to_sum %>%
                      .[,year:=as.character(year)] %>%
                      .[, g:=as.character(g)],w %>%
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


dict_vars <- c("movers_w"                  =     'Total flows',
               "movers_w_junior"           =     'Junior researcher flows',
               "movers_w_senior"           =     'Senior researcher flows',
               "movers_w_medium"           =     'Medium researcher flows',
               "movers_w_own_entrant_r"    =     'Returning to entry institution',
               "movers_w_own_entrant_s"    =     'Exiting from entry institution',
               "movers_w_foreign_entrant"  =     'Foreign entrant flows', 
               "acces_rce"= "University autonomy",
               "date_first_idex"= "Received an IDEX",
               "fusion_date"= "Merged establishment",
               
               'delta_acces_rce' = "University autonomy, departures",
               'delta_acces_rce_a' = "University autonomy, departures abroad",
               'gamma_acces_rce' = "University autonomy, arrivals",
               'gamma_acces_rce_a' = "University autonomy, arrivals from abroad",
               
               'delta_date_first_idex' =   "Received an IDEX, departures",
               'delta_date_first_idex_a' = "Received an IDEX, departures abroad",
               'gamma_date_first_idex' =   "Received an IDEX, arrivals",
               'gamma_date_first_idex_a' = "Received an IDEX, arrivals from abroad",
               
               'delta_fusion_date' =   "Merged establishment, departures",
               'delta_fusion_date_a' = "Merged establishment, departures abroad",
               'gamma_fusion_date' =   "Merged establishment, arrivals",
               'gamma_fusion_date_a' = "Merged establishment, arrivals from abroad",
               
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
    lhs <- paste0('\\textbf{', str_to_sentence(str_replace(lhs,  'University autonomy, |Received IDEX, |Merged establishment, ', '' )),'}')
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

