suppressPackageStartupMessages({
  library(data.table)
})
rm(list=ls())
gc()
library(fixest)
library(tidyverse)
make_institution_treatments <- function(
    n_inst = 200,
    seed = 1L,
    # values for single-treatment groups
    val_t1_only = 1,
    val_t2_2011_only = 1,
    val_t2_2012_only = 1,
    val_t2_2013_only = 1,
    # values for multi-treatment groups
    val_t1_multi = 1,
    val_t2_multi = 1
) {
  set.seed(seed)
  
  inst <- data.table(inst_id = 1:n_inst)
  inst[, `:=`(
    cohort_t1 = 0L,   # 0 if never treated
    cohort_t2 = 0L,
    val_t1   = 0,
    val_t2   = 0
  )]
  
  pool <- inst$inst_id
  take_n <- function(n) {
    x <- sample(pool, 10)
    pool <<- setdiff(pool, n)
    x
  }
  
  # --- Only one treatment (50 institutions total) ---
  g_t1_2010 <- take_n(30)
  g_t1_2012 <- take_n(30)
  g_t2_2011 <- take_n(30)
  g_t2_2012 <- take_n(30)
  g_t2_2013 <- take_n(30)
  
  inst[inst_id %in% g_t1_2010, `:=`(cohort_t1 = 2010L, val_t1 = val_t1_only)]
  inst[inst_id %in% g_t1_2012, `:=`(cohort_t1 = 2012L, val_t1 = val_t1_only)]
  inst[inst_id %in% g_t2_2011, `:=`(cohort_t2 = 2011L, val_t2 = val_t2_2011_only)]
  inst[inst_id %in% g_t2_2012, `:=`(cohort_t2 = 2012L, val_t2 = val_t2_2012_only)]
  inst[inst_id %in% g_t2_2013, `:=`(cohort_t2 = 2013L, val_t2 = val_t2_2013_only)]
  
  # --- Several treatments (5 groups x 10 = 50 institutions total) ---
  g_t1_2010_t2_2011 <- take_n(30)
  g_t1_2010_t2_2012 <- take_n(30)
  g_t1_2012_t2_2012 <- take_n(30)
  g_t1_2012_t2_2011 <- take_n(30)
  g_t1_2010_t2_2013 <- take_n(30)
  
  inst[inst_id %in% g_t1_2010_t2_2011, `:=`(cohort_t1 = 2010L, val_t1 = val_t1_multi,
                                            cohort_t2 = 2011L, val_t2 = val_t2_multi)]
  inst[inst_id %in% g_t1_2010_t2_2012, `:=`(cohort_t1 = 2010L, val_t1 = val_t1_multi,
                                            cohort_t2 = 2012L, val_t2 = val_t2_multi)]
  inst[inst_id %in% g_t1_2012_t2_2012, `:=`(cohort_t1 = 2012L, val_t1 = val_t1_multi,
                                            cohort_t2 = 2012L, val_t2 = val_t2_multi)]
  inst[inst_id %in% g_t1_2012_t2_2011, `:=`(cohort_t1 = 2012L, val_t1 = val_t1_multi,
                                            cohort_t2 = 2011L, val_t2 = val_t2_multi)]
  inst[inst_id %in% g_t1_2010_t2_2013, `:=`(cohort_t1 = 2010L, val_t1 = val_t1_multi,
                                            cohort_t2 = 2013L, val_t2 = val_t2_multi)]
  
  inst[, group := fifelse(
    cohort_t1 > 0L & cohort_t2 == 0L, "only_t1",
    fifelse(cohort_t2 > 0L & cohort_t1 == 0L, "only_t2",
            fifelse(cohort_t1 > 0L & cohort_t2 > 0L, "t1_and_t2", "control")
    )
  )]
  
  inst[]
}

assign_institutions_semi_persistent <- function(
    n_ind, years, n_inst,
    p_two_inst = 0.30,
    p_keep_n_affil = 0.80,
    p_keep_inst1 = 0.85,
    p_keep_inst2 = 0.80,
    seed = 1L
) {
  set.seed(seed)
  years <- as.integer(years)
  T <- length(years)
  
  out <- CJ(ind_id = 1:n_ind, year = years)
  setorder(out, ind_id, year)
  
  # init year
  out[year == years[1], n_affil := 1L + rbinom(.N, 1L, p_two_inst)]
  out[year == years[1], inst1 := sample.int(n_inst, .N, replace = TRUE)]
  out[year == years[1] & n_affil == 2L, inst2 := {
    x <- sample.int(n_inst, .N, replace = TRUE)
    same <- (x == inst1)
    while (any(same)) {
      x[same] <- sample.int(n_inst, sum(same), replace = TRUE)
      same <- (x == inst1)
    }
    x
  }]
  
  for (tt in 2:T) {
    y_prev <- years[tt - 1]
    y_cur  <- years[tt]
    
    prev <- out[year == y_prev, .(ind_id, n_affil_prev = n_affil, inst1_prev = inst1, inst2_prev = inst2)]
    cur  <- out[year == y_cur]
    
    keep_n <- rbinom(n_ind, 1L, p_keep_n_affil) == 1L
    n_new  <- 1L + rbinom(n_ind, 1L, p_two_inst)
    cur[, n_affil := ifelse(keep_n, prev$n_affil_prev, n_new)]
    
    keep1 <- rbinom(n_ind, 1L, p_keep_inst1) == 1L
    cur[, inst1 := ifelse(keep1, prev$inst1_prev, sample.int(n_inst, n_ind, replace = TRUE))]
    
    cur[, inst2 := as.integer(NA)]
    idx2 <- which(cur$n_affil == 2L)
    if (length(idx2) > 0) {
      had2_prev <- !is.na(prev$inst2_prev[idx2])
      keep2 <- rep(FALSE, length(idx2))
      keep2[had2_prev] <- rbinom(sum(had2_prev), 1L, p_keep_inst2) == 1L
      
      inst2_candidate <- integer(length(idx2))
      inst2_candidate[keep2] <- prev$inst2_prev[idx2][keep2]
      
      n_redraw <- sum(!keep2)
      if (n_redraw > 0) {
        draws <- sample.int(n_inst, n_redraw, replace = TRUE)
        inst1_for_redraw <- cur$inst1[idx2][!keep2]
        same <- (draws == inst1_for_redraw)
        while (any(same)) {
          draws[same] <- sample.int(n_inst, sum(same), replace = TRUE)
          same <- (draws == inst1_for_redraw)
        }
        inst2_candidate[!keep2] <- draws
      }
      
      same_final <- inst2_candidate == cur$inst1[idx2]
      if (any(same_final)) {
        draws <- sample.int(n_inst, sum(same_final), replace = TRUE)
        inst1_sf <- cur$inst1[idx2][same_final]
        same2 <- (draws == inst1_sf)
        while (any(same2)) {
          draws[same2] <- sample.int(n_inst, sum(same2), replace = TRUE)
          same2 <- (draws == inst1_sf)
        }
        inst2_candidate[same_final] <- draws
      }
      
      cur[idx2, inst2 := inst2_candidate]
    }
    
    out[year == y_cur, `:=`(n_affil = cur$n_affil, inst1 = cur$inst1, inst2 = cur$inst2)]
  }
  
  out[]
}

simulate_panel <- function(
    seed = 1L,
    n_ind = 1000,
    n_inst = 200,
    years = 2005:2024,
    # assignment persistence
    p_two_inst = 0.30,
    p_keep_n_affil = 0.80,
    p_keep_inst1 = 0.85,
    p_keep_inst2 = 0.80,
    # outcome model
    lambda0 = 1.5,
    beta_t1 = 0.0,  # per unit t1_value on log-mean
    beta_t2 = 0.0,  # per unit t2_value on log-mean
    sd_year = 0.15,
    sd_ind  = 0.40,
    sd_inst = 0.30,
    # treatment values
    val_t1_only = 1,
    val_t2_2011_only = 1,
    val_t2_2012_only = 1,
    val_t2_2013_only = 2,
    val_t1_multi = 1,
    val_t2_multi = 1
) {
  set.seed(seed)
  years <- as.integer(years)
  
  inst_tr <- make_institution_treatments(
    n_inst = n_inst,
    seed = seed + 1000L,
    val_t1_only = val_t1_only,
    val_t2_2011_only = val_t2_2011_only,
    val_t2_2012_only = val_t2_2012_only,
    val_t2_2013_only = val_t2_2013_only,
    val_t1_multi = val_t1_multi,
    val_t2_multi = val_t2_multi
  )
  
  year_fe <- data.table(year = years, year_eff = rnorm(length(years), 0, sd_year))
  ind_re  <- data.table(ind_id = 1:n_ind, ind_eff = rnorm(n_ind, 0, sd_ind))
  inst_re <- data.table(inst_id = 1:n_inst, inst_eff = rnorm(n_inst, 0, sd_inst))
  
  # base (wide) assignment with persistence
  base <- assign_institutions_semi_persistent(
    n_ind = n_ind, years = years, n_inst = n_inst,
    p_two_inst = p_two_inst,
    p_keep_n_affil = p_keep_n_affil,
    p_keep_inst1 = p_keep_inst1,
    p_keep_inst2 = p_keep_inst2,
    seed = seed + 2000L
  )
  
  # ---- requested main dataset: one row per (individual, year, institution) ----
  affil1 <- base[, .(ind_id, year, inst_id = inst1)]
  affil2 <- base[n_affil == 2L, .(ind_id, year, inst_id = inst2)]
  panel_long <- rbindlist(list(affil1, affil2), use.names = TRUE)
  setorder(panel_long, ind_id, year, inst_id)
  
  # merge institution cohorts/values + institution RE
  panel_long <- merge(panel_long, inst_tr, by = "inst_id", all.x = TRUE)
  panel_long <- merge(panel_long, inst_re, by = "inst_id", all.x = TRUE)
  
  # merge ind/year effects
  panel_long <- merge(panel_long, ind_re,  by = "ind_id", all.x = TRUE)
  panel_long <- merge(panel_long, year_fe, by = "year",   all.x = TRUE)
  
  # Treatment indicators and realized values at the row level
  panel_long[, t1_on := as.integer(cohort_t1 > 0L & year >= cohort_t1)]
  panel_long[, t2_on := as.integer(cohort_t2 > 0L & year >= cohort_t2)]
  panel_long[, t1_value := fifelse(t1_on == 1L, val_t1, 0)]
  panel_long[, t2_value := fifelse(t2_on == 1L, val_t2, 0)]
  
  # Cohort columns requested (already in data; but ensure naming and 0 if never treated)
  panel_long[, cohort_t1 := as.integer(cohort_t1)]
  panel_long[, cohort_t2 := as.integer(cohort_t2)]
  
  # Outcome: to avoid double-counting an individual's publications when they have 2 institutions,
  # we simulate at the (ind_id, year) level then copy it onto both institution rows.
  # (If you prefer splitting across institutions, tell me: equal split or weighted.)
  ind_year <- unique(panel_long[, .(ind_id, year, ind_eff, year_eff)])
  ind_year <- merge(ind_year,
                    panel_long[, .(
                      inst_eff_mean = mean(inst_eff),
                      t1_value_any = max(t1_value),
                      t2_value_any = max(t2_value)
                    ), by = .(ind_id, year)],
                    by = c("ind_id", "year"),
                    all.x = TRUE)
  
  ind_year[, linpred :=
             log(lambda0) +
             year_eff + ind_eff + inst_eff_mean +
             beta_t1 * t1_value_any +
             beta_t2 * t2_value_any
  ]
  ind_year[, mu := exp(linpred)]
  ind_year[, publications := rpois(.N, lambda = mu)]
  
  panel_long <- merge(
    panel_long,
    ind_year[, .(ind_id, year, publications)],
    by = c("ind_id", "year"),
    all.x = TRUE
  )
  
  # clean column order (includes your requested institution id and cohorts)
  setcolorder(panel_long, c(
    "ind_id", "year", "inst_id",
    "publications",
    "t1_on", "t2_on", "t1_value", "t2_value",
    "cohort_t1", "cohort_t2",
    "val_t1", "val_t2",
    "ind_eff", "year_eff", "inst_eff",
    "group"
  ))
  
  panel_long <- panel_long   %>%
    .[, ':='(to_2010 = as.numeric(cohort_t1 == 2010),
                                      to_2012 = as.numeric(cohort_t1 == 2012),
                                      tt_2011 = as.numeric(cohort_t2 == 2011),
                                      tt_2012 = as.numeric(cohort_t2 == 2012),
                                      tt_2013 = as.numeric(cohort_t2 == 2013)
  )] %>%
    .[, ':='(to =cohort_t1, tt = cohort_t2)]
  
  list(
    panel_ind_year_inst = panel_long[],
    institution_treatments = inst_tr[order(inst_id)],
    effects = list(year_fe = year_fe, ind_re = ind_re, inst_re = inst_re),
    params = list(
      seed = seed, years = years, n_ind = n_ind, n_inst = n_inst,
      p_two_inst = p_two_inst,
      p_keep_n_affil = p_keep_n_affil,
      p_keep_inst1 = p_keep_inst1,
      p_keep_inst2 = p_keep_inst2,
      lambda0 = lambda0,
      beta_t1 = beta_t1, beta_t2 = beta_t2,
      sd_year = sd_year, sd_ind = sd_ind, sd_inst = sd_inst
    )
  )
}

# -------------------
# Example
# -------------------
beta1 <- 0.5 
beta2 <- -0.5
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))

simulate_n_panels <- function(
    n_sim = 100,
    base_seed = 123,
    ...
){
  
  sims <- lapply(1:n_sim, function(i){
    
    sim <- simulate_panel(
      seed = base_seed + i,
        years = 2005:2024,
        n_ind = 2000,
        n_inst = 300,
      
        lambda0 = 2,
        beta_t1 = beta1,
        beta_t2 = beta2
      )
    
    dt <- sim$panel_ind_year_inst
    dt[, sim_id := i]   # track which simulation it belongs to
    
    return(dt)
  })
  
  rbindlist(sims)
}
sim_data <- simulate_n_panels(500)
gc()

regression = list()
for(i in unique(sim_data$sim_id)){
  
  sim_current <- sim_data[sim_id == i]
  regression[[as.character(i)]]  <- fepois(publications ~
                      to_2010 + i(year, to_2010, ref=2009)
                    + to_2012 + i(year, to_2012, ref=2011)
                    + tt_2011 + i(year, tt_2011, ref=2010)
                    + tt_2012 + i(year, tt_2012, ref=2011)
                    + tt_2013 + i(year, tt_2013, ref=2012)
                    | ind_id + year + inst_id,
                    data = sim_current)
  print(i)
}

simulated_est <- rbindlist(lapply(names(regression), function(name) {
  result <- agg_effects(regression[[name]], data = sim_data[sim_id == as.integer(name)])  # Call your function with the correct data
  result[, sim_id := name]  # Add a new column with the name of the list element
}), use.names = TRUE, fill = TRUE) 


ggplot(simulated_est)+
  geom_histogram(aes(x=est, fill=treat)) +
  geom_vline(aes(xintercept = beta1))+geom_vline(aes(xintercept = beta2))
gc()

summary(simulated_est[treat == 'tt'])
summary(simulated_est[treat == 'to'])



ols = list()
for(i in unique(sim_data$sim_id)){
  
  sim_current <- sim_data[sim_id == i]
  ols[[as.character(i)]]  <- fepois(publications ~ i(post, t1_on, 0) +i(post, t2_on, 0)
                                           | ind_id + year + inst_id,
                                           data = sim_current %>% .[,post := ifelse(year>=2010, 1, 0)])
  print(i)
}

simulated_fepois <- rbindlist(lapply(names(regression), function(name) {
  result_ols <- as.data.table(coeftable(ols[[name]]), keep.rownames = T)
  result_ols[, sim_id := name] 
}), use.names = TRUE, fill = TRUE) 

ggplot(simulated_fepois)+
  geom_histogram(aes(x=Estimate, fill=rn)) +
  geom_vline(aes(xintercept = beta1))+geom_vline(aes(xintercept = beta2))
gc()
summary(simulated_fepois[rn == 'post::1:t1_on'])
summary(simulated_fepois[rn == 'post::1:t2_on'])
