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
       'did',
       "stargazer",
       'MatchIt',
       'WeightIt',
       'cobalt',
       'etwfe',
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

inputpath <- "D:\\panel_fr_res\\data\\inst_field_year_panel_final.parquet"

save_path = paste0("D:\\panel_fr_res\\results\\productivity_lab\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))

ds <- open_dataset(inputpath) %>%
  filter(first_year_lab <= 2003  )
nrow(ds)
ds <- as.data.table(ds) #524838
gc()

ds <- ds %>%
 .[, idex := vapply(idex, paste, character(1), collapse = ", ")]
nrow(unique(ds[, list(inst_id, domain)])) #12651

df_reg <- ds %>%
  .[, interact_rce_idex := ifelse(date_first_idex >0 & acces_rce >0, pmin(date_first_idex, acces_rce), 0)] %>%
  .[!(acces_rce %in% 2013:2015)
    #& (fusion_date <=2020)
    & (!str_detect(idex, "annule"))
    & !is.na(domain)
  ]%>%
.[, ':='(idn = as.integer(factor(paste0(inst_id, '_', domain))),
           year_n = as.integer(year)
  )] 
gc()

nrow(unique(df_reg[, list(inst_id, domain)])) #11031

selection_period= 2000:2002

df_reg <-df_reg %>%
  .[, ':='(entry_cohort = floor(first_year_lab/5)*5) ] %>%
  .[, ':='(pub_selection = sum(as.numeric(year %in% selection_period)*publications),
           cit_selection = sum(as.numeric(year %in% selection_period)*citations),
           n_au_selection = mean(ifelse(year %in% selection_period & total > 0, total, NA), na.rm = T)
  ), by= 'idn'] %>%
  .[, n_au_selection := ifelse( is.na(n_au_selection) | n_au_selection == Inf | n_au_selection == -Inf, 0, n_au_selection)]

df_reg <- df_reg %>%
  .[n_au_selection < quantile(unique(df_reg[, list(idn, n_au_selection)])$n_au_selection,
                              probs = c(0.99))&
    n_au_selection >=5
    #& type %in% c('facility')
    & !(type %in% c('healthcare','company'))
    # & pub_selection >0
      ]
length(unique(df_reg$idn)) #2130
unique(quantile(unique(df_reg[, list(idn, n_au_selection)])$n_au_selection,
                probs = c(0, 0.25, 0.5, 0.75, 0.9, 1)))

outcomes_to_avg <- c('publications'
                     ,'citations'
                     ,'new_phrase_comb_reuse'
                     ,'nr_source_top_5pct'
                     ,'nr_source_top_10pct'
                     ,'nr_source_top_20pct')
avg_outcomes <- paste0('avg_', outcomes_to_avg)
unique(quantile(unique(df_reg[, list(idn, n_au_selection)])$n_au_selection,
                probs = c(0, 0.25, 0.5, 0.75, 0.9, 1)))
quants_to_cut <- c(0, 0.25, 0.5, 0.75, 1)
# include.lowest (with a dot) is the argument of cut(): 'include_lowest' was silently ignored, so the
# units at the minimum got an NA n-tile and were dropped by did whenever the n-tile was a control.
# n_au_n_tile is now cut on n_au_selection (it was cut on cit_selection with the n_au_selection breaks).
df_reg <- df_reg %>%
  .[, ':='(pub_n_tile = cut(pub_selection, unique(quantile(unique(df_reg[, list(idn, pub_selection)])$pub_selection,
                                                           probs = quants_to_cut)), include.lowest = TRUE, labels = FALSE))
  ] %>% 
  .[, ':='(cit_n_tile = cut(cit_selection, unique(quantile(unique(df_reg[, list(idn, cit_selection)])$cit_selection,
                                                           probs = quants_to_cut)), include.lowest = TRUE, labels = FALSE))
  ] %>% 
  .[, ':='(n_au_n_tile = cut(n_au_selection, unique(quantile(unique(df_reg[, list(idn, n_au_selection)])$n_au_selection,
                                                             probs = quants_to_cut)), include.lowest = TRUE, labels = FALSE))
  ] %>%  
  .[year >=2003] %>%
  .[, (avg_outcomes) := lapply(.SD, function(x){x/total_w}) , .SDcols = outcomes_to_avg] %>%
  .[date_first_idex == 0 | acces_rce != 0] %>%
  .[, treatment := case_when(acces_rce != 0 & date_first_idex == 0 ~ 'acces_rce_plain',
                             acces_rce != 0 & date_first_idex <= 2012 ~ 'first_wave_idex',
                             acces_rce != 0  ~ 'second_wave_idex',
                             .default = 'control'
  ) ] %>%
  .[, acces_rce:=as.integer(acces_rce)] %>%
  .[, ':='(second_wave_idex = ifelse(treatment == 'second_wave_idex', acces_rce,0),
           first_wave_idex = ifelse(treatment == 'first_wave_idex', acces_rce,0),
           acces_rce_plain = ifelse(treatment == 'acces_rce_plain', acces_rce,0))] %>%
  .[, share_women := total_w_women/total_w] %>%
  .[, ecole := as.numeric(type_fr == 'École')]

# n-tiles are categorical controls: stored as character so that did includes one dummy per n-tile
# (in the main regression pub_n_tile used to enter as a linear term)
tile_vars <- c('pub_n_tile', 'cit_n_tile', 'n_au_n_tile')
df_reg[, (tile_vars) := lapply(.SD, as.character), .SDcols = tile_vars]
  
  
table(unique(df_reg[, list(idn, treatment)])$treatment)
table(unique(df_reg[, list(idn, acces_rce)])$acces_rce)

table(unique(df_reg[treatment == 'second_wave_idex'][, list(idn, second_wave_idex)])$second_wave_idex)
table(unique(df_reg[treatment == 'first_wave_idex'][, list(idn, first_wave_idex)])$first_wave_idex)
table(unique(df_reg[treatment == 'acces_rce_plain'][, list(idn, acces_rce_plain)])$acces_rce_plain)


table(unique(df_reg[, list(idn, cnrs)])$cnrs)
table(unique(df_reg[, list(idn, domain)])$domain)
table(unique(df_reg[, list(idn, ecole)])$ecole)


dict_vars <- c(dict_vars,
               'total' = 'Number of researchers',
               'total_entrant' = 'Number of new researchers',
               'total_w' = 'Number of researchers',
               'total_men' = 'Number of male researchers',
               'total_women' = 'Number of female researchers',
               'total_w_men' = 'Number of male researchers',
               'total_w_women' = 'Number of female researchers',
               'share_women' = 'Share of women',
               'nr_coau_under_5y' = 'Publications (recently arrived)',
               'nr_coau_under_10y' = 'Publications (recently arrived)',
               'citations_coau_under_5y' = 'Citations  (recently arrived)',
               'new_phrase_comb_reuse_coau_under_5y' = 'New phrases (recently arrived)',
               'nr_source_top_5pct_raw_coau_under_5y' = "Publications in top 5% cited journals (recently arrived)",
               "semantic_distance_coau_under_5y" = "Semantic distance to existing works (recently arrived)",
               
               'total_junior' = 'Number of junior researchers',
               'total_w_junior' = 'Number of junior researchers',
               'nr_coau_junior_under_5y' = 'Publications by juniors (recently arrived)',
               'nr_coau_junior_under_10y' = 'Publications by juniors (recently arrived)',
               
               'total_senior' = 'Number of senior researchers',
               'total_w_senior' = 'Number of senior researchers',
               'nr_coau_senior_under_5y' = 'Publications by seniors (recently arrived)',
               'nr_coau_senior_under_10y' = 'Publications by seniors (recently arrived)',
               
               'total_medium' = 'Number of mid-career researchers',
               'total_w_medium' = 'Number of mid-career researchers',
               'nr_coau_medium_under_5y' = 'Publications by mid-career (recently arrived)',
               'nr_coau_medium_under_10y' = 'Publications by mid-career (recently arrived)',
               
               'total_foreign_entrant' = 'Number of foreign entrants',
               'total_w_foreign_entrant' = 'Number of foreign entrants',
               
               'nr_coau_foreign_entrant' = 'Publications by foreign entrants',
               'nr_coau_foreign_entrant_under_5y' = 'Publications by foreign entrants (recently arrived)',
               'citations_coau_foreign_entrant_under_5y' = 'Citations by foreign entrants (recently arrived)',
               'new_phrase_comb_reuse_coau_foreign_entrant_under_5y' = 'New phrases by foreign entrants (recently arrived)',
               'nr_source_top_5pct_raw_coau_foreign_entrant_under_5y' = "Publications in top 5% cited journals by foreign entrants (recently arrived)",
               "semantic_distance_coau_foreign_entrant_under_5y" = "Semantic distance to existing works (recently arrived foreign entrants)",
               
               'publications' = 'Publications',
               'citations'= 'Citations',
               'new_phrase_comb_reuse'= "New phrases",
               'nr_source_top_5pct' = "Publications in top 5% cited journals",
               'nr_source_top_10pct' = "Publications in top 10% cited journals",
               'nr_source_top_20pct' = "Publications in top 20% cited journals",
               "semantic_distance" = 'Semantic distance to existing works',
               
               'avg_publications' = 'Avg. Publications',
               'avg_citations'= 'Avg. Citations',
               'avg_new_phrase_comb_reuse'= "Avg. New phrases",
               'avg_nr_source_top_5pct' = "Avg. Publications in top 5% cited journals",
               'avg_nr_source_top_10pct' = "Avg. Publications in top 10% cited journals",
               'avg_nr_source_top_20pct' = "Avg. Publications in top 20% cited journals",

               'acces_rce_plain' = 'Autonomy - No IDEX',
               'first_wave_idex' = 'Autonomy - 1st wave IDEX',
               'second_wave_idex' = 'Autonomy - 2nd wave IDEX'
)

outcomes_to_keep <- c('total',
                      'total_w',
                      'share_women',
                      'total_foreign_entrant', 
                      "total_w_foreign_entrant",
                      'nr_coau_foreign_entrant','nr_coau_foreign_entrant_under_5y',
                      'nr_coau_under_5y', 'citations_coau_under_5y',
                      'total_w_junior','total_w_senior','total_w_medium',
                      'publications','avg_publications',
                      'citations','avg_citations',
                      'nr_source_top_5pct','avg_nr_source_top_5pct',
                      'new_phrase_comb_reuse','avg_new_phrase_comb_reuse')

df_reg <- df_reg %>%
  .[, (outcomes_to_keep) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes_to_keep] 
crit <- qnorm(0.975)   # 1.96 for a 95% CI; change to qnorm(0.95) for 90%
gc()


# Helper functions --------------------------------------------------------
# (identical in the lab-level and the author-level scripts)

var_label <- function(x) {
  if (!is.null(names(dict_vars)) && x %in% names(dict_vars)) dict_vars[[x]] else x
}

get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

clean_msg <- function(x) gsub("[\r\n]+", " ", x)

# Runs f() and returns its value, or the error message if it fails, plus the warnings it raised.
# Errors never propagate: the caller records them and moves on.
safe_call <- function(f) {
  warns <- character(0)
  value <- withCallingHandlers(
    tryCatch(f(), error = function(e) structure(list(message = conditionMessage(e)),
                                                class = 'safe_call_error')),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart('muffleWarning')
    }
  )
  failed <- inherits(value, 'safe_call_error')
  list(value    = if (failed) NULL else value,
       error    = if (failed) value$message else NA_character_,
       warnings = unique(warns))
}

add_step <- function(res, step, stage) {
  res$warnings <- c(res$warnings, step$warnings)
  if (!is.na(step$error)) {
    res$failed_stage <- c(res$failed_stage, stage)
    res$error        <- c(res$error, paste0(stage, ': ', step$error))
  }
  res
}

safe_fwrite <- function(x, file) {
  tryCatch(fwrite(x, file),
           error = function(e) message('Could not write ', file, ' (is it open in another program?): ',
                                       conditionMessage(e)))
}

save_plots_pdf <- function(plots, file, width = 9, height = 6) {
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(invisible(FALSE))
  ok <- tryCatch({ grDevices::pdf(file, width = width, height = height); TRUE },
                 error = function(e) { message('Could not open ', file, ': ', conditionMessage(e)); FALSE })
  if (!ok) return(invisible(FALSE))
  on.exit(grDevices::dev.off())
  for (p in plots) tryCatch(print(p), error = function(e) message('Could not draw a plot: ', conditionMessage(e)))
  invisible(TRUE)
}

collect_results <- function(root, prefix) {
  files <- list.files(root, pattern = paste0('^', prefix, '.*\\.csv$'), recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(data.table())
  rbindlist(lapply(files, fread), fill = TRUE)
}

# Control specifications ---------------------------------------------------

# Aliases let one control token stand for several columns (e.g. a block of dummies)
expand_controls <- function(ctrls) {
  as.character(unlist(lapply(ctrls, function(x) if (!is.null(control_aliases[[x]])) control_aliases[[x]] else x),
                      use.names = FALSE))
}

make_spec_name <- function(ctrls) if (length(ctrls) == 0) 'none' else paste(ctrls, collapse = '__')

# All combinations of the blocks; '' in a block means that the block is left out
build_specs <- function(blocks) {
  grid <- expand.grid(blocks, stringsAsFactors = FALSE)
  ctrl_list <- lapply(seq_len(nrow(grid)), function(i) {
    v <- unlist(grid[i, , drop = TRUE], use.names = FALSE)
    v[v != '']
  })
  spec_names <- vapply(ctrl_list, make_spec_name, character(1))
  ord <- order(lengths(ctrl_list), spec_names)
  tab <- as.data.table(grid)[ord]
  setnames(tab, paste0('ctrl_', names(blocks)))
  tab[, `:=`(spec = spec_names[ord], n_controls = lengths(ctrl_list)[ord])]
  setcolorder(tab, c('spec', 'n_controls'))
  list(table = tab, controls = setNames(ctrl_list[ord], spec_names[ord]))
}

spec_of <- function(ctrls) {
  hit <- names(spec_controls)[vapply(spec_controls, function(v) setequal(v, ctrls), logical(1))]
  if (length(hit) != 1) stop('No unique specification with controls: ', paste(ctrls, collapse = ', '))
  hit
}

make_xformla <- function(ctrl_vars) {
  if (length(ctrl_vars) == 0) return(~1)
  as.formula(paste0('~', paste0(ctrl_vars, collapse = '+')))
}

# Estimation ---------------------------------------------------------------

es_table <- function(a) {
  cv <- a$crit.val.egt
  if (is.null(cv) || length(cv) != 1 || !is.finite(cv)) cv <- qnorm(0.975)
  data.table(event_time = a$egt, att = a$att.egt, se = a$se.egt) %>%
    .[, `:=`(ci_lo = att - cv * se, ci_hi = att + cv * se, crit_val = cv)]
}

plot_es <- function(es, y_title, title = '', subtitle = NULL) {
  ggplot(es, aes(x = event_time, y = att)) +
    geom_hline(yintercept = 0, colour = 'grey60') +
    geom_vline(xintercept = -0.5, colour = 'firebrick') +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2) +
    geom_point() +
    theme_bw() + xlab('Time to treatment') + ylab(y_title) +
    labs(title = title, subtitle = subtitle)
}

# Sample size and pre-treatment mean, on the sample actually used by did (falls back on the input data)
sample_stats <- function(es_stag, data, outcome, pre_mean_fun) {
  d <- es_stag$DIDparams$data
  if (is.null(d) || !all(c('idn', 'year_n', 'g_treat', outcome) %in% names(d))) d <- data
  d <- as.data.frame(d)
  list(n_units = length(unique(d$idn)), pre_mean = pre_mean_fun(d, outcome))
}

# One did::att_gt run + aggregations. Never throws: failures are recorded in the returned list.
estimate_did <- function(data, outcome, ctrl_vars, base_period, allow_unbalanced_panel,
                         clustervar, pre_mean_fun, keep_full, outcome_label) {
  t0  <- Sys.time()
  res <- list(status = 'ok', failed_stage = character(0), error = character(0), warnings = character(0),
              att = NA_real_, se = NA_real_, pre_mean = NA_real_, n_units = NA_integer_,
              es = NULL, full = NULL, run_time = NA_real_)
  finalize <- function(res) {
    res$run_time <- as.numeric(difftime(Sys.time(), t0, units = 'secs'))
    res$warnings <- unique(res$warnings)
    res$status <- if (length(res$failed_stage) == 0) {
      if (is.finite(res$att)) 'ok' else 'no_estimate'
    } else if (is.finite(res$att)) 'partial' else 'error'
    res
  }
  
  step <- safe_call(function() did::att_gt(yname = outcome,
                                           tname = 'year_n',
                                           idname = 'idn',
                                           gname = 'g_treat',
                                           data = data,
                                           xformla = make_xformla(ctrl_vars),
                                           base_period = base_period,
                                           allow_unbalanced_panel = allow_unbalanced_panel,
                                           control_group = 'notyettreated',
                                           clustervars = clustervar))
  res <- add_step(res, step, 'att_gt')
  if (is.null(step$value)) return(finalize(res))
  es_stag <- step$value
  
  step <- safe_call(function() sample_stats(es_stag, data, outcome, pre_mean_fun))
  res  <- add_step(res, step, 'sample_stats')
  if (!is.null(step$value)) { res$n_units <- step$value$n_units; res$pre_mean <- step$value$pre_mean }
  
  step <- safe_call(function() aggte(es_stag, type = 'simple', na.rm = TRUE))
  res  <- add_step(res, step, 'aggte_simple')
  if (!is.null(step$value)) { res$att <- step$value$overall.att; res$se <- step$value$overall.se }
  
  step <- safe_call(function() {
    x_lim <- c(min(data$year_n) - min(es_stag$group), max(data$year_n) - max(es_stag$group))
    aggte(es_stag, type = 'dynamic', na.rm = TRUE, min_e = x_lim[1], max_e = x_lim[2])
  })
  res <- add_step(res, step, 'aggte_dynamic')
  es_aggte_dyn <- step$value
  if (!is.null(es_aggte_dyn)) res$es <- es_table(es_aggte_dyn)
  
  if (keep_full) {
    plot_print <- NULL
    if (!is.null(es_aggte_dyn)) {
      step <- safe_call(function() {
        ggdid(es_aggte_dyn) + scale_colour_manual(values = c("black", 'black')) +
          geom_vline(xintercept = -0.5, colour = 'firebrick') +
          theme_bw() + theme(legend.position = 'none') + xlab('Time to treatment') +
          ylab(outcome_label) + labs(title = '')
      })
      res <- add_step(res, step, 'plot')
      plot_print <- step$value
    }
    res$full <- list(regression = es_stag, aggte_dyn = es_aggte_dyn, plot = plot_print)
  }
  finalize(res)
}

summary_row <- function(res, spec, ctrls, treat, treat_label, outcome) {
  att <- res$att
  se  <- res$se
  p   <- if (is.finite(att) && is.finite(se) && se > 0) 2 * (1 - pnorm(abs(att / se))) else NA_real_
  ci_low  <- att - crit * se
  ci_high <- att + crit * se
  data.table(
    spec         = spec,
    controls     = if (length(ctrls) == 0) 'none' else paste(ctrls, collapse = ' + '),
    treat        = treat,
    Treatment    = treat_label,
    outcome      = outcome,
    Outcome      = var_label(outcome),
    status       = res$status,
    failed_stage = paste(res$failed_stage, collapse = ' | '),
    error        = clean_msg(paste(res$error, collapse = ' | ')),
    ATT          = round(att, 3),
    Stars        = get_stars(p),
    SE           = round(se, 3),
    p_value      = round(p, 4),
    CI_low       = round(ci_low, 3),
    CI_high      = round(ci_high, 3),
    CI           = sprintf("[%.3f, %.3f]", ci_low, ci_high),
    PreTreatMean = round(res$pre_mean, 3),
    N_units      = res$n_units,
    run_time_sec = round(res$run_time, 1),
    n_warnings   = length(res$warnings),
    warnings     = clean_msg(paste(res$warnings, collapse = ' | '))
  )
}

# Estimation sample for one treatment: 'main_treat' uses every unit, the other treatments
# use their own treated units + the control group. g_treat holds the treatment date (0 = never).
analysis_sample <- function(treat, keep_cols, extra_filter = NULL) {
  keep_rows <- if (treat == main_treat) rep(TRUE, nrow(df_reg)) else df_reg$treatment %in% c('control', treat)
  if (!is.null(extra_filter)) keep_rows <- keep_rows & (extra_filter %in% TRUE)
  d <- df_reg[keep_rows, ..keep_cols]
  d[, g_treat := as.integer(get(treat))]
  d[]
}

# Estimates every outcome of one treatment for one control specification, and saves:
#   att_summary__<treat>.csv  (one row per outcome, incl. status and error message; written last,
#                              so its presence means that the block is finished)
#   event_study__<treat>.csv / .pdf
#   estimates/<treat>__<outcome>.rds  (full did objects, by default only for specs in full_object_specs)
run_block <- function(d_analysis, treat, spec, out_dir, extra_cols = list(), subtitle_extra = NULL,
                      save_full = spec %in% full_object_specs) {
  opts      <- analyses[[treat]]
  summary_f <- file.path(out_dir, paste0('att_summary__', treat, '.csv'))
  if (!overwrite_existing && file.exists(summary_f)) {
    cat('  Already estimated, skipped (set overwrite_existing <- TRUE to re-run)\n')
    return(invisible(NULL))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ctrls     <- spec_controls[[spec]]
  ctrl_vars <- expand_controls(ctrls)
  keep_full <- save_full
  if (keep_full) dir.create(file.path(out_dir, 'estimates'), showWarnings = FALSE)
  subtitle  <- paste(c(paste0('Controls: ', if (length(ctrls) == 0) 'none' else paste(ctrls, collapse = ', ')),
                       subtitle_extra), collapse = ' | ')
  
  rows <- list(); es_rows <- list(); plots <- list()
  for (outcome in opts$outcomes) {
    cat(paste0('  Estimating for outcome: ', outcome, ' ... '))
    cols <- unique(c(outcome, 'idn', 'year_n', cluster_var, 'g_treat', ctrl_vars))
    res  <- estimate_did(data = d_analysis[, ..cols], outcome = outcome, ctrl_vars = ctrl_vars,
                         base_period = opts$base_period,
                         allow_unbalanced_panel = opts$allow_unbalanced_panel,
                         clustervar = cluster_var, pre_mean_fun = pre_mean_fun,
                         keep_full = keep_full, outcome_label = var_label(outcome))
    cat(res$status, sprintf('(%.0f s)\n', res$run_time))
    if (length(res$error) > 0) message('    -> ', paste(res$error, collapse = ' | '))
    
    row <- summary_row(res, spec, ctrls, treat, opts$label, outcome)
    for (nm in names(extra_cols)) row[, (nm) := extra_cols[[nm]]]
    rows[[outcome]] <- row
    
    if (!is.null(res$es)) {
      es <- copy(res$es)[, `:=`(spec = spec, treat = treat, outcome = outcome)]
      for (nm in names(extra_cols)) es[, (nm) := extra_cols[[nm]]]
      es_rows[[outcome]] <- es
      plots[[outcome]] <- plot_es(res$es, var_label(outcome),
                                  title = paste0('Treatment: ', opts$label), subtitle = subtitle)
      if (print_plots) print(plots[[outcome]])
    }
    if (keep_full && !is.null(res$full)) {
      saveRDS(res$full, file.path(out_dir, 'estimates', paste0(treat, '__', outcome, '.rds')), compress = FALSE)
    }
    rm(res); gc()
  }
  if (length(es_rows) > 0) {
    safe_fwrite(rbindlist(es_rows, fill = TRUE), file.path(out_dir, paste0('event_study__', treat, '.csv')))
    save_plots_pdf(plots, file.path(out_dir, paste0('event_study__', treat, '.pdf')))
  }
  out <- rbindlist(rows, fill = TRUE)
  fwrite(out, summary_f)
  invisible(out)
}

# Plots --------------------------------------------------------------------

plot_att_points <- function(d, x_var, x_title, y_title, title = '') {
  d <- d[is.finite(ATT)]
  if (nrow(d) == 0) return(NULL)
  ggplot(d, aes(x = .data[[x_var]], y = ATT)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
    geom_hline(yintercept = 0, colour = 'grey10') +
    theme_bw() + theme(legend.position = 'none') + xlab(x_title) + ylab(y_title) + labs(title = title)
}

# Specification curve: ATT (95% CI) of every specification, ranked, with the controls included below
plot_spec_curve <- function(res, out_var, treat_name, treat_label, baseline_spec_name, ctrl_levels, n_total) {
  d <- res[outcome == out_var & treat == treat_name & is.finite(ATT)]
  if (nrow(d) == 0) return(NULL)
  d <- d[order(ATT)][, rank := .I][, is_baseline := spec == baseline_spec_name]
  x_scale <- scale_x_continuous(limits = c(0.5, nrow(d) + 0.5), expand = c(0, 0))
  colours <- scale_colour_manual(values = c(`FALSE` = 'grey20', `TRUE` = 'firebrick'), guide = 'none')
  top <- ggplot(d, aes(x = rank, y = ATT, colour = is_baseline)) +
    geom_hline(yintercept = 0, colour = 'grey50') +
    geom_linerange(aes(ymin = CI_low, ymax = CI_high, alpha = is_baseline)) +
    geom_point(aes(size = is_baseline)) +
    scale_alpha_manual(values = c(`FALSE` = 0.35, `TRUE` = 1), guide = 'none') +
    scale_size_manual(values = c(`FALSE` = 0.7, `TRUE` = 2), guide = 'none') +
    colours + x_scale + theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = NULL, y = 'ATT', title = paste0(var_label(out_var), ' - Treatment: ', treat_label),
         subtitle = sprintf('%d of %d specifications with an estimate (baseline in red)', nrow(d), n_total))
  inc <- d[, .(control = unlist(strsplit(spec, '__', fixed = TRUE))), by = .(rank, is_baseline)] %>%
    .[control != 'none']
  if (nrow(inc) == 0) return(top)
  bottom <- ggplot(inc, aes(x = rank, y = factor(control, levels = rev(ctrl_levels)), colour = is_baseline)) +
    geom_point(shape = 15, size = 0.9) +
    colours + scale_y_discrete(drop = FALSE) + x_scale + theme_bw() +
    labs(x = 'Specifications, ranked by ATT', y = NULL)
  cowplot::plot_grid(top, bottom, ncol = 1, align = 'v', axis = 'lr', rel_heights = c(2, 1))
}


# Control specifications ----------------------------------------------------
# Each block lists mutually exclusive alternatives ('' = block left out); every combination
# across blocks is estimated: 2 x 3 x 3 x 3 x 2 x 2 x 2 = 432 specifications, the first one
# being the specification without any control.
control_blocks <- list(
  domain = c('', 'domain'),
  n_au   = c('', 'n_au_selection', 'n_au_n_tile'),
  pub    = c('', 'pub_selection',  'pub_n_tile'),
  cit    = c('', 'cit_selection',  'cit_n_tile'),
  cnrs   = c('', 'cnrs'),
  type   = c('', 'type'),
  ecole  = c('', 'ecole')
)
control_aliases <- list()   # no control stands for several columns at the lab level

specs         <- build_specs(control_blocks)
spec_table    <- specs$table      # one row per specification, ordered by number of controls
spec_controls <- specs$controls   # named list: specification -> controls
baseline_spec <- spec_of(c('domain', 'type', 'cnrs', 'pub_n_tile'))   # specification used so far

# Settings ------------------------------------------------------------------
specs_to_run       <- spec_table$spec   # e.g. spec_table[n_controls <= 3]$spec to run a subset first
full_object_specs  <- baseline_spec     # specs for which the full did objects are saved (.rds); spec_table$spec = all
overwrite_existing <- FALSE             # FALSE: blocks already saved are skipped, so an interrupted run can be resumed
print_plots        <- FALSE             # event-study plots are always saved in a pdf per specification
cluster_var        <- 'inst_id'
main_treat         <- 'acces_rce'
controls_root      <- file.path(save_path, 'by_controls')

# Pre-treatment mean of the outcome among eventually-treated units (years up to 2007)
pre_mean_fun <- function(d, outcome) {
  treated_ids <- unique(d$idn[d$g_treat != 0])
  mean(d[[outcome]][d$idn %in% treated_ids & d$year_n <= 2007], na.rm = TRUE)
}

outcomes_main  <- outcomes_to_keep
outcomes_waves <- c('total',
                    'total_w',
                    'total_w_women',
                    'total_foreign_entrant', 'nr_coau_foreign_entrant','nr_coau_foreign_entrant_under_5y',
                    'nr_coau_under_5y', 'citations_coau_under_5y',
                    'total_w_junior','total_w_senior','total_w_medium',
                    'publications','avg_publications',
                    'citations','avg_citations',
                    'nr_source_top_5pct','avg_nr_source_top_5pct',
                    'new_phrase_comb_reuse','avg_new_phrase_comb_reuse')

# 1) all units treated for autonomy (units only treated by the IDEX were dropped above)
# 2) one analysis per IDEX wave: treated units of the wave + control group
analyses <- list(
  acces_rce        = list(label = 'All', outcomes = outcomes_main,
                          base_period = 'universal', allow_unbalanced_panel = FALSE),
  acces_rce_plain  = list(label = var_label('acces_rce_plain'), outcomes = outcomes_waves,
                          base_period = 'varying', allow_unbalanced_panel = TRUE),
  first_wave_idex  = list(label = var_label('first_wave_idex'), outcomes = outcomes_waves,
                          base_period = 'varying', allow_unbalanced_panel = TRUE),
  second_wave_idex = list(label = var_label('second_wave_idex'), outcomes = outcomes_waves,
                          base_period = 'varying', allow_unbalanced_panel = TRUE)
)
analyses_to_run <- names(analyses)

keep_cols <- unique(c(outcomes_main, outcomes_waves, 'idn', 'year_n', cluster_var, names(analyses),
                      expand_controls(unique(unlist(spec_controls)))))
missing_cols <- setdiff(keep_cols, names(df_reg))
if (length(missing_cols) > 0) stop('Columns missing from df_reg: ', paste(missing_cols, collapse = ', '))

dir.create(controls_root, recursive = TRUE, showWarnings = FALSE)
fwrite(spec_table, file.path(save_path, 'spec_table.csv'))
cat(sprintf('%d specifications, %d did estimations in total\n', length(specs_to_run),
            length(specs_to_run) * sum(sapply(analyses[analyses_to_run], function(a) length(a$outcomes)))))


# Estimation for every control specification ----------------------------------
# Output: <save_path>/by_controls/<specification>/ (see run_block). A failed estimation is
# recorded in att_summary (status, failed_stage, error) and the loop moves on.

for (treat in analyses_to_run) {
  print(paste0("Computing the loop for: ", analyses[[treat]]$label))
  start_time_treat <- Sys.time()
  d_analysis <- analysis_sample(treat, keep_cols)
  
  for (i in seq_along(specs_to_run)) {
    spec <- specs_to_run[i]
    cat(sprintf('\n[%s] Specification %d/%d: %s\n', treat, i, length(specs_to_run), spec))
    run_block(d_analysis, treat, spec, file.path(controls_root, spec))
  }
  rm(d_analysis); gc()
  print(paste0("Finished the loop for ", analyses[[treat]]$label, ' in:'))
  print(Sys.time() - start_time_treat)
}


# ATT by treatment group, for each specification -----------------------------

treat_levels <- vapply(analyses, `[[`, character(1), 'label')
for (spec in specs_to_run) {
  res_spec <- rbindlist(lapply(names(analyses), function(tr) {
    f <- file.path(controls_root, spec, paste0('att_summary__', tr, '.csv'))
    if (file.exists(f)) fread(f)
  }), fill = TRUE)
  if (nrow(res_spec) == 0) next
  res_spec[, Treatment := factor(Treatment, levels = treat_levels)]
  plots <- lapply(unique(res_spec$outcome), function(o) {
    plot_att_points(res_spec[outcome == o], 'Treatment', 'Treatment group', var_label(o))
  })
  save_plots_pdf(plots, file.path(controls_root, spec, 'att_by_treatment.pdf'))
  if (spec == baseline_spec) for (p in Filter(Negate(is.null), plots)) print(p)
}


# Results across specifications -----------------------------------------------

all_res <- collect_results(controls_root, 'att_summary__')
if (nrow(all_res) > 0) {
  all_res <- merge(spec_table, all_res, by = 'spec')   # current specifications only, adds ctrl_* columns
  setorder(all_res, treat, outcome, n_controls, spec)
  safe_fwrite(all_res, file.path(save_path, 'all_specs_att_summary.csv'))
  safe_fwrite(all_res[status != 'ok'], file.path(save_path, 'all_specs_errors.csv'))
  print(dcast(all_res, treat ~ status, value.var = 'outcome', fun.aggregate = length))
  
  all_es <- collect_results(controls_root, 'event_study__')
  if (nrow(all_es) > 0) safe_fwrite(all_es[spec %in% spec_table$spec], file.path(save_path, 'all_specs_event_study.csv'))
  
  ctrl_levels <- setdiff(unique(unlist(control_blocks, use.names = FALSE)), '')
  for (tr in intersect(names(analyses), unique(all_res$treat))) {
    plots <- lapply(analyses[[tr]]$outcomes, function(o) {
      plot_spec_curve(all_res, o, tr, analyses[[tr]]$label, baseline_spec, ctrl_levels, length(specs_to_run))
    })
    save_plots_pdf(plots, file.path(save_path, paste0('spec_curve__', tr, '.pdf')), width = 11, height = 7)
  }
}
