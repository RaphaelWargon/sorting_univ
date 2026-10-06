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
#source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))


inputpath <- "D:\\panel_fr_res\\data\\panel_au_year_fr.parquet"

save_path = paste0("D:\\panel_fr_res\\results\\productivity_au\\autonomy_waves\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

# What to run ---------------------------------------------------------------
rebuild_cache      <- TRUE   # FALSE: skip the parquet step and read the cached csv directly
run_main_specs     <- TRUE   # DID (autonomy, then IDEX waves) for every control specification
run_balance        <- TRUE   # propensity-score balance diagnostics, baseline controls
run_het_grants     <- TRUE   # heterogeneity by dependency on grants (terciles)
run_het_tresorerie <- TRUE   # heterogeneity by tresorerie (above / below the median)
run_etwfe          <- TRUE   # etwfe, autonomy only

cache_path_d     <- "D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv"
cache_path_local <- "C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv"

if (rebuild_cache) {
  ds <- open_dataset(inputpath) %>%
    filter(
      last_year-entry_year >2
      & last_year >= 2003
      & entry_year >=1965 
      #& !(acces_rce_0_1y %in%  c(2014, 2015))
      #& !(date_first_idex_0_1y %in% c(2014))
      #& !(fusion_date_0_1y %in% c(2012,2016,2019))
      & entry_year <=2003
    )
  nrow(ds)
  
  ds <- as.data.table(ds) #7322301
  gc()
  
  n_au <- length(unique(ds$author_id)) #251105
  
  fields_by_number <- ds %>%
    .[, .(N = n_distinct(author_id)), by = 'field']
  table(fields_by_number$N)
  
  fields_to_keep <- fields_by_number[N>=0.001*n_au]$field
  
  sample_df_reg <- ds %>%
    .[, year_n := as.numeric(as.character(year))] %>%
    .[, ever_in_idex_annulee := max(as.numeric(str_detect(idex_set, "annulee") )), by = 'author_id'] %>%
    .[field %in% fields_to_keep] %>%
    .[ , ':='(idn = as.integer(factor(author_id)))]
  gc()
  length(unique(sample_df_reg$author_id)) #190249
  
  sample_df_reg %>% .[year >=2003] %>% .[, .N, by = 'author_id'] %>% .[, .N, by = "N"]
  
  test <- sample_df_reg %>%
    .[str_detect(author_name, 'Aghion')]
  
  outcomes <- c('publications_raw',
                'citations_raw',
                'nr_source_top_5pct_raw', 
                'nr_source_top_10pct_raw',
                'nr_source_top_20pct_raw',
                'nr_source_mid_40pct_raw',
                'nr_source_btm_50pct_raw',
                colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), "new") & !str_detect(colnames(sample_df_reg), "wins") ]
  )
  
  outcomes_wins <- ifelse(
    grepl('raw', outcomes),
    gsub('raw', 'wins', outcomes),
    paste0(outcomes, '_wins')
  )
  
  selection_period= 2000:2002
  
  sample_df_reg <-sample_df_reg %>%   .[, (outcomes_wins) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes] %>%
    .[, ':='(entry_cohort = floor(entry_year/5)*5) ] %>%
    .[, ':='(pub_selection = sum(as.numeric(year %in% selection_period)*publications_raw),
             cit_selection = sum(as.numeric(year %in% selection_period)*citations_raw)
    ), by= 'author_id'] %>%
    .[pub_selection>0]
  length(unique(sample_df_reg$author_id)) #141449
  
  # Note: 'include_lowest' is ignored by cut() (the argument is include.lowest), so authors at the minimum
  # get an NA n-tile; they are put in a separate category '0' when df_reg is built below (unchanged).
  sample_df_reg <- sample_df_reg %>%
    .[, ':='(pub_n_tile = cut(pub_selection, unique(quantile(unique(sample_df_reg[, list(author_id, pub_selection)])$pub_selection,
                                                             probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
    ] %>% 
    .[, ':='(cit_n_tile = cut(cit_selection, unique(quantile(unique(sample_df_reg[, list(author_id, cit_selection)])$cit_selection,
                                                             probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
    ] %>% 
    .[, min_cnrs := min(ifelse(in_cnrs==1, year, NA), na.rm =T), by ='author_id'] %>%
    .[, min_cnrs := ifelse(!is.na(min_cnrs),min_cnrs, 0)] %>%
    .[year >=2003] 
  
  sample_df_reg %>% .[, .(N=n_distinct(author_id)), by='min_cnrs']
  fwrite(sample_df_reg, cache_path_d)
  rm(ds)
  gc()
  fwrite(sample_df_reg, cache_path_local)
  rm(sample_df_reg)
  gc()
}

sample_df_reg <- fread(cache_path_local)

sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #141449
gc()


unit_cols <- c("author_id", "domain","field", "subfield","gender", "entry_year","last_year",
               "entry_cohort", "pub_selection","cit_selection","min_cnrs","pub_n_tile",'min_cnrs' ,
               'acces_rce','date_first_idex','fusion_date','interact_rce_idex','cit_n_tile'
)
outcomes_to_keep <- c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins')

type_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), 'in_type')] 


# Sample for main specification -------------------------------------------


sample_df_reg <- sample_df_reg %>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce),
           all_idex = sum(in_date_first_idex),
           all_retired = sum(as.numeric(year >last_year))
  ),by= 'author_id'] %>%
  .[, (paste0('all_', type_cols)) := lapply(.SD, sum, na.rm =TRUE), by = 'author_id', .SDcols = type_cols]%>%
  .[all_in_type_company ==0 &  all_in_type_healthcare == 0] 

sample_df_reg  %>% .[, list(author_id)] %>% distinct() %>% count() #104794
gc()


df_reg <- sample_df_reg %>%
  .[,':='(acces_rce       = as.integer(ITT_acces_rce_2007),
          date_first_idex = as.integer(ITT_date_first_idex_2007),
          fusion_date     = as.integer(ITT_fusion_date_2007),
          interact_rce_idex = ifelse(ITT_acces_rce_2007 != 0 & ITT_date_first_idex_2007 != 0,
                                     pmin(as.integer(as.character(ITT_acces_rce_2007)), 
                                          as.integer(as.character(ITT_date_first_idex_2007))), 0 ),
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile),
          has_pub = as.numeric(publications_raw >0)
  )] %>%
  .[, inst_set_2007 := inst_id_set[year == 2007][1], by = 'author_id'] %>%
  .[, city_set_2007 := city_set[year == 2007][1], by = 'author_id'] %>%
  .[, DEP_set_2007 := DEP_set[year == 2007][1], by = 'author_id'] %>%
  .[, REG_set_2007 := REG_set[year == 2007][1], by = 'author_id'] %>%
  .[, cnrs_2007 := as.integer(min_cnrs > 0 & min_cnrs <= 2007)] %>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce), 0, as.integer(acces_rce)),
           date_first_idex = ifelse(is.na(date_first_idex), 0, as.integer(date_first_idex)),
           fusion_date = ifelse(is.na(fusion_date), 0, as.integer(fusion_date)),
           interact_rce_idex = ifelse(is.na(interact_rce_idex), 0, as.integer(interact_rce_idex)),
           idn = as.integer(factor(idn)),
           year_n = as.integer(year_n)
  )
  ] %>%
  .[!(acces_rce %in% 2013:2015)
    & (ever_in_idex_annulee ==0)
  ] %>%
  # Drop people treated by the IDEX but not by autonomy (date_first_idex != 0 & acces_rce == 0),
  # as in the lab-level script
  .[date_first_idex == 0 | acces_rce != 0] %>%
  # Treatment groups as in the lab-level script: autonomy without IDEX, autonomy + 1st / 2nd IDEX wave.
  # The timing of every group is the date of autonomy (acces_rce).
  .[, treatment := case_when(acces_rce != 0 & date_first_idex == 0 ~ 'acces_rce_plain',
                             acces_rce != 0 & date_first_idex <= 2012 ~ 'first_wave_idex',
                             acces_rce != 0  ~ 'second_wave_idex',
                             .default = 'control'
  ) ] %>%
  .[, acces_rce := as.integer(acces_rce)] %>%
  .[, ':='(second_wave_idex = ifelse(treatment == 'second_wave_idex', acces_rce, 0L),
           first_wave_idex  = ifelse(treatment == 'first_wave_idex',  acces_rce, 0L),
           acces_rce_plain  = ifelse(treatment == 'acces_rce_plain',  acces_rce, 0L))]
df_reg  %>% .[, list(author_id)] %>% distinct() %>% count()
gc()

df_reg %>%
  .[, has_pub := as.numeric(publications_raw >0)] %>%
  .[, lapply(.SD, mean, na.rm = T), by = c('year','acces_rce'), 
    .SD= c('publications_raw','change_af','citations_wins','in_acces_rce','retired','has_pub'
    )]%>%
  ggplot() + geom_line(aes(x=year, y = citations_wins, color = factor(acces_rce)))

nrow(df_reg[str_count(inst_id_set, ',')>0])/nrow(df_reg)

gc()

df_reg %>%  .[, .(N =n_distinct(author_id)), by = "treatment" ]
table(unique(df_reg[treatment == 'acces_rce_plain'][, list(author_id, acces_rce_plain)])$acces_rce_plain)
table(unique(df_reg[treatment == 'first_wave_idex'][, list(author_id, first_wave_idex)])$first_wave_idex)
table(unique(df_reg[treatment == 'second_wave_idex'][, list(author_id, second_wave_idex)])$second_wave_idex)

gc()

field_dummies <- c()
all_fields <- sort((df_reg %>% .[, list(field)] %>% distinct() %>% separate_rows(field, sep= ',') %>% distinct())$field)
for(field in all_fields){
  print(field)
  field_var <- paste0('f_', field)
  set(df_reg, j = field_var, value = as.numeric(str_detect(df_reg$field, field)))   # set(): no copy of df_reg
  field_dummies <- c(field_dummies, field_var)
}

# entry cohorts are categories (one dummy per cohort), as in the previous version
df_reg[, entry_cohort := as.character(entry_cohort)]

outcomes_to_keep <- c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins', 'nr_source_top_10pct_wins',
                      'new_phrase_comb_reuse','publications_raw','citations_raw')

dict_vars <- c(dict_vars,
               'acces_rce_plain' = 'Autonomy - No IDEX',
               'first_wave_idex' = 'Autonomy - 1st wave IDEX',
               'second_wave_idex' = 'Autonomy - 2nd wave IDEX')
crit <- qnorm(0.975)   # 1.96 for a 95% CI; change to qnorm(0.95) for 90%


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
# across blocks is estimated: 2 x 3 x 3 x 3 x 2 = 108 specifications, the first one being the
# specification without any control. 'field_dummies' stands for all the f_* field dummies;
# domain and the field dummies are alternatives because fields are nested in domains.
control_blocks <- list(
  cohort = c('', 'entry_cohort'),
  field  = c('', 'domain', 'field_dummies'),
  pub    = c('', 'pub_selection', 'pub_n_tile'),
  cit    = c('', 'cit_selection', 'cit_n_tile'),
  cnrs   = c('', 'cnrs_2007')
)
control_aliases <- list(field_dummies = field_dummies)

specs         <- build_specs(control_blocks)
spec_table    <- specs$table      # one row per specification, ordered by number of controls
spec_controls <- specs$controls   # named list: specification -> controls
baseline_spec <- spec_of(c('entry_cohort', 'field_dummies', 'pub_n_tile', 'cit_n_tile'))   # specification used so far

# Settings ------------------------------------------------------------------
specs_to_run       <- spec_table$spec   # e.g. spec_table[n_controls <= 2]$spec to run a subset first
full_object_specs  <- baseline_spec     # specs for which the full did objects are saved (.rds, large at author level)
overwrite_existing <- FALSE             # FALSE: blocks already saved are skipped, so an interrupted run can be resumed
print_plots        <- FALSE             # event-study plots are always saved in a pdf per specification
cluster_var        <- 'inst_set_2007'
main_treat         <- 'acces_rce'
controls_root      <- file.path(save_path, 'by_controls')

het_specs             <- baseline_spec    # control specifications used in the heterogeneity analyses (specs_to_run = all)
het_treats            <- c('acces_rce', 'acces_rce_plain', 'first_wave_idex', 'second_wave_idex')
het_save_full_objects <- FALSE            # TRUE: also save the full did objects of the heterogeneity analyses

grant_vars              <- c('anr_investissements_d_avenir', 'anr_hors_investissements_d_avenir',
                             'contrats_et_prestations_de_recherche_hors_anr', 'produits_de_fonctionnement_encaissables')
tresorerie_var          <- 'tresorerie'
tresorerie_median_level <- 'institution'  # 'institution': median across treated institution sets (as for the grant
                                          # terciles); 'author': median across treated authors

# Pre-treatment mean of the outcome among treated authors, before their own treatment year
pre_mean_fun <- function(d, outcome) {
  mean(d[[outcome]][d$g_treat != 0 & d$year_n < d$g_treat], na.rm = TRUE)
}

# 1) all people treated for autonomy (people only treated by the IDEX were dropped above)
# 2) one analysis per IDEX wave: treated authors of the wave + control group
analyses <- list(
  acces_rce        = list(label = 'All', outcomes = outcomes_to_keep,
                          base_period = 'varying', allow_unbalanced_panel = FALSE),
  acces_rce_plain  = list(label = var_label('acces_rce_plain'), outcomes = outcomes_to_keep,
                          base_period = 'varying', allow_unbalanced_panel = FALSE),
  first_wave_idex  = list(label = var_label('first_wave_idex'), outcomes = outcomes_to_keep,
                          base_period = 'varying', allow_unbalanced_panel = FALSE),
  second_wave_idex = list(label = var_label('second_wave_idex'), outcomes = outcomes_to_keep,
                          base_period = 'varying', allow_unbalanced_panel = FALSE)
)
analyses_to_run <- names(analyses)

keep_cols <- unique(c(outcomes_to_keep, 'idn', 'year_n', cluster_var, names(analyses),
                      expand_controls(unique(unlist(spec_controls)))))
needed_cols <- c(keep_cols, 'treatment', 'author_id', 'inst_id_set',
                 if (run_het_grants) grant_vars, if (run_het_tresorerie) tresorerie_var)
missing_cols <- setdiff(needed_cols, names(df_reg))
if (length(missing_cols) > 0) stop('Columns missing from df_reg: ', paste(missing_cols, collapse = ', '))

dir.create(controls_root, recursive = TRUE, showWarnings = FALSE)
fwrite(spec_table, file.path(save_path, 'spec_table.csv'))
cat(sprintf('%d specifications, %d did estimations in total\n', length(specs_to_run),
            length(specs_to_run) * sum(sapply(analyses[analyses_to_run], function(a) length(a$outcomes)))))


# Estimation for every control specification ----------------------------------
# Output: <save_path>/by_controls/<specification>/ (see run_block). A failed estimation is
# recorded in att_summary (status, failed_stage, error) and the loop moves on.

if (run_main_specs) {
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


# Balance diagnostics (baseline controls) --------------------------------------

if (run_balance) {
  balance_path <- file.path(save_path, 'balance')
  dir.create(balance_path, recursive = TRUE, showWarnings = FALSE)
  balance_vars <- expand_controls(spec_controls[[baseline_spec]])
  
  for (treat in names(analyses)) {
    d_sep <- analysis_sample(treat, unique(c('idn', names(analyses), balance_vars))) %>%
      .[, treat_bin := as.numeric(g_treat != 0)]
    
    step <- safe_call(function() {
      W <- weightit(as.formula(paste("treat_bin ~", paste(balance_vars, collapse = "+"))),
                    data = d_sep, method = "glm", estimand = "ATT")
      
      # love plot: standardized mean differences before/after weighting
      print(love.plot(W, abs = TRUE, thresholds = c(m = 0.1),
                      var.order = "unadjusted", stars = "std"))
      
      # propensity score overlap
      bal_plot <- bal.plot(W, var.name = "prop.score", which = "both", type = "histogram", mirror = TRUE,
                           colors = c('firebrick','steelblue'))
      print(bal_plot)
      ggsave(plot = bal_plot, filename = file.path(balance_path, paste0(treat, '_', "balance_plot.png")))
    })
    if (!is.na(step$error)) message('Balance diagnostics failed for ', treat, ': ', step$error)
    rm(d_sep); gc()
  }
}


# Heterogeneity: shared runner ----------------------------------------------------
# For each treatment and each group, the sample is: treated authors of the group + the whole control
# group (control authors have no group, i.e. NA, and are kept in every subsample). Treated authors
# whose group is NA (missing financial data) are in no subsample.
# Output: <save_path>/heterogeneity_<name>/<specification>/group_<g>/ + all_att_summary.csv, att_by_group.pdf

run_heterogeneity <- function(group_col, groups, het_name) {
  het_root <- file.path(save_path, paste0('heterogeneity_', het_name))
  for (treat in het_treats) {
    for (g in groups) {
      in_group   <- df_reg$treatment == 'control' | df_reg[[group_col]] %in% g
      d_analysis <- analysis_sample(treat, keep_cols, extra_filter = in_group)
      cat(sprintf('\n[%s | %s = %s] treated authors: %d, control authors: %d\n', treat, het_name, g,
                  uniqueN(d_analysis[g_treat != 0]$idn), uniqueN(d_analysis[g_treat == 0]$idn)))
      for (spec in het_specs) {
        cat(sprintf('Specification: %s\n', spec))
        run_block(d_analysis, treat, spec, file.path(het_root, spec, paste0('group_', g)),
                  extra_cols = list(het_variable = het_name, het_group = g),
                  subtitle_extra = paste0(het_name, ': ', g),
                  save_full = het_save_full_objects)
      }
      rm(d_analysis); gc()
    }
  }
  
  res <- collect_results(het_root, 'att_summary__')
  if (nrow(res) == 0) return(invisible(NULL))
  res <- res[spec %in% het_specs]
  res[, het_group := factor(as.character(het_group), levels = groups)]
  safe_fwrite(res, file.path(het_root, 'all_att_summary.csv'))
  het_es <- collect_results(het_root, 'event_study__')
  if (nrow(het_es) > 0) safe_fwrite(het_es[spec %in% het_specs], file.path(het_root, 'all_event_study.csv'))
  for (sp in unique(res$spec)) {
    plots <- list()
    for (tr in het_treats) for (o in outcomes_to_keep) {
      plots[[paste(tr, o)]] <- plot_att_points(res[spec == sp & treat == tr & outcome == o], 'het_group',
                                               het_name, var_label(o),
                                               title = paste0('Treatment: ', analyses[[tr]]$label))
    }
    save_plots_pdf(plots, file.path(het_root, sp, 'att_by_group.pdf'))
  }
  invisible(res)
}


# Heterogeneity by dependency on grants -----------------------------------

if (run_het_grants) {
  classification_heterogeneity <- unique(df_reg[, c('inst_id_set', 'year', grant_vars), with = FALSE] %>%
    .[, y_classification := suppressWarnings(min(ifelse(!is.na(anr_investissements_d_avenir), year, NA), na.rm = T)),
      by = 'inst_id_set'] %>%
    .[y_classification == year] %>%
    .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                                as.numeric(anr_hors_investissements_d_avenir)
                              + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
    )/(
      as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
    .[, list(inst_id_set, y_classification, ratio_subv_propre)])
  
  print(summary(classification_heterogeneity))
  
  quantiles_ratio_subv_propre = quantile(classification_heterogeneity$ratio_subv_propre, probs = c(0.33, 0.66), na.rm =T)
  
  classification_heterogeneity <- classification_heterogeneity %>%
    .[, quantile_ratio_subv_propre := case_when(ratio_subv_propre <= quantiles_ratio_subv_propre [[1]] ~ '1',
                                                ratio_subv_propre <= quantiles_ratio_subv_propre [[2]] ~ '2',
                                                ratio_subv_propre > quantiles_ratio_subv_propre [[2]] ~ '3',
    )] %>%
    .[, ":="(inst_set_2007 = inst_id_set,
             inst_id_set = NULL) ]
  if (anyDuplicated(classification_heterogeneity$inst_set_2007)) {
    warning('Several grant classifications for the same inst_set_2007: the first one is kept')
    classification_heterogeneity <- unique(classification_heterogeneity, by = 'inst_set_2007')
  }
  
  # added to df_reg by reference (no merge, so that df_reg is not copied)
  df_reg[, `:=`(ratio_subv_propre = NA_real_, quantile_ratio_subv_propre = NA_character_)]
  df_reg[classification_heterogeneity, on = 'inst_set_2007',
         `:=`(ratio_subv_propre = i.ratio_subv_propre, quantile_ratio_subv_propre = i.quantile_ratio_subv_propre)]
  print(unique(df_reg[, .(author_id, treatment, quantile_ratio_subv_propre)]) %>%
          .[, .N, by = .(treatment, quantile_ratio_subv_propre)] %>% .[order(treatment, quantile_ratio_subv_propre)])
  
  run_heterogeneity('quantile_ratio_subv_propre', c('1', '2', '3'), 'grants')
}


# Heterogeneity by tresorerie (above / below the median) --------------------------
# Same construction as for grants: value of the first year in which tresorerie is observed for the
# institution set, attached to authors through their 2007 institution set. tresorerie is NA for the
# control group, so the median is computed among treated units only, and control authors are kept
# in both subsamples. Treated units exactly at the median are in 'below'.

if (run_het_tresorerie) {
  classification_tresorerie <- unique(df_reg[, .(inst_id_set, year, tresorerie_value = as.numeric(get(tresorerie_var)))] %>%
    .[, y_classification := suppressWarnings(min(ifelse(!is.na(tresorerie_value), year, NA), na.rm = T)),
      by = 'inst_id_set'] %>%
    .[y_classification == year] %>%
    .[, list(inst_set_2007 = inst_id_set, y_classification_tresorerie = y_classification, tresorerie_value)])
  if (anyDuplicated(classification_tresorerie$inst_set_2007)) {
    warning('Several tresorerie values for the same inst_set_2007: the first one is kept')
    classification_tresorerie <- unique(classification_tresorerie, by = 'inst_set_2007')
  }
  
  df_reg[, tresorerie_value := NA_real_]
  df_reg[classification_tresorerie, on = 'inst_set_2007', tresorerie_value := i.tresorerie_value]
  
  treated_tresorerie <- if (tresorerie_median_level == 'institution') {
    unique(df_reg[treatment != 'control' & !is.na(tresorerie_value), .(inst_set_2007, tresorerie_value)])$tresorerie_value
  } else {
    unique(df_reg[treatment != 'control' & !is.na(tresorerie_value), .(author_id, tresorerie_value)])$tresorerie_value
  }
  median_tresorerie <- median(treated_tresorerie)
  print(paste0('Median ', tresorerie_var, ' across treated ', tresorerie_median_level, 's: ', median_tresorerie,
               ' (', length(treated_tresorerie), ' ', tresorerie_median_level, 's)'))
  
  df_reg[, tresorerie_group := fcase(treatment == 'control',          NA_character_,
                                     is.na(tresorerie_value),          NA_character_,
                                     tresorerie_value > median_tresorerie, 'above',
                                     default = 'below')]
  print(unique(df_reg[, .(author_id, treatment, tresorerie_group)]) %>%
          .[, .N, by = .(treatment, tresorerie_group)] %>% .[order(treatment, tresorerie_group)])
  
  run_heterogeneity('tresorerie_group', c('below', 'above'), 'tresorerie')
}


### Alternative specification : etwfe, autonomy only ---------------------------

if (run_etwfe) {
  etwfe_path <- file.path(save_path, 'etwfe')
  dir.create(etwfe_path, recursive = TRUE, showWarnings = FALSE)
  list_etwfe <- list()
  match_variables <- c('entry_cohort','field','cnrs_2007','pub_n_tile','cit_n_tile'
  )
  for(treat in c('acces_rce')){   # autonomy only, as in the first DID regression
    start_time <- Sys.time()
    list_etwfe[[treat]] <- list()
    
    for(outcome in c('publications_wins') #outcomes_to_keep
    ){
      list_etwfe[[treat]][[outcome]] <- list()
      cols_to_keep <- c(outcome, 
                        "year", "author_id", "inst_set_2007", 
                        "treatment", "acces_rce", "date_first_idex", "fusion_date", "interact_rce_idex",
                        match_variables
      )
      # every author treated for autonomy (whatever the IDEX status) + the control group,
      # i.e. the same sample as the first DID regression
      d_sep <- df_reg[, ..cols_to_keep] %>%
        .[, ":="(entry_cohort = as.factor(entry_cohort),
                 pub_n_tile = as.factor(pub_n_tile),
                 cit_n_tile = as.factor(cit_n_tile),
                 inst_set_2007 = as.factor(inst_set_2007)
        )] %>%
        .[, treat_binary := as.numeric(get(treat) != 0)]
      
      step <- safe_call(function() {
        d_sep_match <- as.data.table(match.data(matchit(as.formula(paste0('treat_binary ~ ',
                                                                          paste0(match_variables, collapse = ' + ')))
                                                        ,data = d_sep
                                                        ,method = "exact")))
        print(d_sep_match %>%
                .[, .(N =n_distinct(author_id)), by = "treatment" ])
        
        etwfe(
          fml    = as.formula(paste0(outcome, '~ 0')),
          tvar   = "year",
          gvar   = treat,
          data   = d_sep_match,
          #ivar   = "author_id",
          #gref   = 0,
          cgroup = "never",
          family = "poisson",
          vcov   = ~ inst_set_2007,
          weights = ~weights
        )
      })
      if (is.null(step$value)) {
        message('etwfe failed for ', treat, ' / ', outcome, ': ', step$error)
        next
      }
      etwfe_est <- step$value
      print(Sys.time()- start_time)
      list_etwfe[[treat]][[outcome]][['regression']] <- etwfe_est
      start_time <- Sys.time()
      
      step <- safe_call(function() emfx(etwfe_est, type = "event"))
      if (is.null(step$value)) {
        message('emfx failed for ', treat, ' / ', outcome, ': ', step$error)
        next
      }
      event_study <- step$value
      tryCatch(print(plot(event_study)), error = function(e) message('Could not plot: ', conditionMessage(e)))
      list_etwfe[[treat]][[outcome]][['plot']] <- event_study
      tryCatch(fwrite(as.data.table(event_study), file.path(etwfe_path, paste0(treat, '__', outcome, '__event.csv'))),
               error = function(e) message('Could not save the etwfe event study: ', conditionMessage(e)))
      print(Sys.time()- start_time)
    }
  }
  # Other aggregations of the same model:
  # emfx(list_etwfe$acces_rce$publications_wins$regression, type = "calendar")   # ATT by calendar year
  # emfx(list_etwfe$acces_rce$publications_wins$regression, type = "group")      # ATT by adoption cohort
}
