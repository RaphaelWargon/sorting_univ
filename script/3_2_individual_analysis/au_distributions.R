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
       'MatchIt',
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
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))
sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 
sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg %>% .[, list(author_id)] %>% distinct() %>% count() #104656
gc()


unit_cols <- c("author_id", "domain","field", "subfield","gender", "entry_year","last_year",
               "entry_cohort", "pub_04_07","cit_04_07","min_cnrs","pub_n_tile",'min_cnrs' ,
               'acces_rce','date_first_idex','fusion_date','interact_rce_idex','cit_n_tile'
)

type_cols <- colnames(sample_df_reg)[str_detect(colnames(sample_df_reg), 'in_type')] 




all_df_reg <- sample_df_reg %>%
  .[, ':='(all_chg = sum(new_af +change_af),
           all_acces_rce = sum(in_acces_rce),
           all_idex = sum(in_date_first_idex),
           all_retired = sum(as.numeric(year >last_year))
  ),by= 'author_id'] %>%
  .[, (paste0('all_', type_cols)) := lapply(.SD, sum), by = 'author_id', .SDcols = type_cols]%>%
  .[all_in_type_company ==0 & all_in_type_archive ==0 & all_in_type_other == 0
    & !is.na(field)
  ] %>%
  .[,':='(acces_rce  = ITT_acces_rce_2007,
          date_first_idex =ITT_date_first_idex_2007,
          fusion_date = ITT_fusion_date_2007,
          interact_rce_idex = interact_rce_idex_itt_2007,
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile)
  )] %>%
  .[, has_pub := as.numeric(publications_raw >0)] %>%
  #.[ (acces_rce == acces_rce_0_1y)
  #   &(date_first_idex == date_first_idex_0_1y)
  #   ]%>%
  # .[, ':='(date_first_idex = ifelse(acces_rce ==0, date_first_idex, 0 ))]%>%
  #.[!str_count(inst_id_set, ',')>2] %>%
  .[!(acces_rce %in% 2013:2015) & !(date_first_idex %in% c(2013,2014))
    & !(interact_rce_idex %in% c(2013,2014))
    # & (fusion_date ==0)
  ] %>%
  .[, inst_id_obs := .N, by = c('inst_id_set')] %>%
  .[, ':='(min_inst_id_obs = min(inst_id_obs),
           n_obs = .N,
           max_nr_inst_id = max(str_count(inst_id_set,','))
  ), by = 'author_id'] %>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce), 0, acces_rce),
           date_first_idex = ifelse(is.na(date_first_idex), 0, date_first_idex),
           fusion_date = ifelse(is.na(fusion_date), 0, fusion_date),
           interact_rce_idex = ifelse(is.na(interact_rce_idex), 0, interact_rce_idex)
  )
  
  ] %>% .[fusion_date<=2020] %>%
  .[, n_lt := .N, by = c('inst_id_set','field')]



# Estimating the distributions --------------------------------------------

citations_pre <- all_df_reg[year %in% 2003:2005 & year <=last_year & in_type_facility ==1 & acces_rce !=0,
                     .(citations = sum(citations_raw),
                       total_new_phrase_comb_reuse = sum(total_new_phrase_comb_reuse)
                       ),
                     by = .(author_id, field)] %>%
  .[, N := .N, by = 'field'] %>%
  .[N>=25] %>%
  .[, ':='(cit_rel = citations / mean(citations),
           new_phrase_rel = total_new_phrase_comb_reuse/mean(total_new_phrase_comb_reuse)), by = field] 

ggplot(citations_pre)+
  geom_point(aes(x=cit_rel, y = new_phrase_rel))

ggplot(citations_pre)+
  geom_density(aes(x=log(cit_rel)))

ggplot(citations_pre)+
  geom_density(aes(x=log(new_phrase_rel)))

# how much mass is at zero, by field -- report this separately
citations_pre[, .(share_zero = mean(citations == 0), n = .N), by = field][order(-share_zero)]

# pooled empirical survival, zeros dropped, ties handled
surv <- citations_pre[citations > 0, .N, by = cit_rel][order(cit_rel)]
surv[, S := (sum(N) - cumsum(N) + N) / sum(N)] %>% 
  .[,group := "pre"]

ggplot(surv, aes(cit_rel, S)) +
  geom_point(size = 0.6, alpha = 0.5) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl") +
  labs(x = "citations / field mean (log)", y = expression(P(X >= x)))


p_load("gamlss")

dat <- data.frame(citations_pre %>%
                    .[, field_offset := mean(citations), by = 'field'])

m0  <- gamlss(citations ~ 1+ offset(field_offset), family = NBI, data = dat, trace = FALSE)
tab <- chooseDist(m0, type = "counts", k = log(nrow(dat)))  # k = log(n) -> BIC
getOrder(tab)                                                # ranked table



x <- citations_pre[cit_rel>0,cit_rel]
library(fitdistrplus); library(actuar)

f_ln   <- fitdist(x, "lnorm")
f_gam  <- fitdist(x, "gamma",   lower = c(0, 0))
f_wei  <- fitdist(x, "weibull")
f_llog <- fitdist(x, "llogis",  start = list(shape = 1, scale = median(x)))
f_par  <- fitdist(x, "pareto",  start = list(shape = 2, scale = 1))
f_burr <- fitdist(x, "burr",    start = list(shape1 = 1, shape2 = 2, rate = 1))

L <- list(f_ln, f_gam, f_wei, f_llog, f_par, f_burr)
nm <- c("lognormal","gamma","weibull","log-logistic","Pareto","Burr")
gofstat(L, fitnames = nm)            # KS, CvM, AD, AIC, BIC in one table

cdfcomp(L, legendtext = nm, xlogscale = TRUE, ylogscale = TRUE)
qqcomp(L,  legendtext = nm, xlogscale = TRUE, ylogscale = TRUE)

citations_post <- all_df_reg %>%
  .[, has_competitive := (as.numeric(au_funded_ANR_ods== 1 | au_funded_ANR_dgds == 1 | au_funded_ERC ==1))] %>%
  .[, inst_has_grant := max(has_competitive), by = c('inst_id_set','year')] %>%
  .[, has_competitive := max(inst_has_grant), by = c('author_id','year')] %>%
  .[year %in% 2015:2020 & year <=last_year & in_type_facility ==1 & acces_rce !=0,
                            .(citations = sum(citations_raw)),
                            by = .(author_id, field,has_competitive)] %>%
  .[, N := .N, by = 'field'] %>%
  .[N>=25] %>%
  .[, cit_rel := citations / mean(citations), by = field]

surv_post <- citations_post[citations > 0, .N, by = c("cit_rel", "has_competitive")][order(cit_rel)]
surv_post[, S := (sum(N) - cumsum(N) + N) / sum(N), by = 'has_competitive'] %>%
  .[, group := ifelse(has_competitive ==1, 'Comp', 'Base')] %>% .[, has_competitive := NULL]

surv_both <- rbind(surv, surv_post)
ggplot(surv_both, aes(cit_rel, S, color = group)) +
  geom_point(size = 0.6, alpha = 0.5) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl") +
  labs(x = "citations / field mean (log)", y = expression(P(X >= x)))


citations_idex <- all_df_reg %>%
  .[, has_competitive := max(in_date_first_idex), by = c('author_id','year')] %>%
  .[year %in% 2015:2020 & year <=last_year & in_type_facility ==1 & in_acces_rce==0,
    .(citations = sum(citations_raw)),
    by = .(author_id, field,has_competitive)] %>%
  .[, N := .N, by = 'field'] %>%
  .[N>=25] %>%
  .[, cit_rel := citations / mean(citations), by = field]

surv_idex <- citations_idex[citations > 0, .N, by = c("cit_rel", "has_competitive")][order(cit_rel)]
surv_idex[, S := (sum(N) - cumsum(N) + N) / sum(N), by = 'has_competitive'] %>%
  .[, group := ifelse(has_competitive ==1, 'Comp', 'Base')] %>% .[, has_competitive := NULL]

ggplot(surv_idex, aes(cit_rel, S, color = group)) +
  geom_point(size = 0.6, alpha = 0.5) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl") +
  labs(x = "citations / field mean (log)", y = expression(P(X >= x)))


# claude code -------------------------------------------------------------

# =============================================================================
# Distributional tests of the grant-allocation model
#   (1) four survival curves: {pre, post} x {control, treated institutions}
#   (2) bootstrap bands, clustered at institution level
#   (3) quantile-ratio plot  -> slope estimates alpha*t2, flat segment = placebo
#   (4) dispersion difference-in-differences on var(log output)
#   (5) individual-level kinked-slope regression
#
# ADAPT THE NAMES IN SECTION 0. Everything else should run unchanged.
# Required columns in `movers`:
#   author_id, field, inst_id, year, citations_raw, treated (institution 0/1)
# =============================================================================

library(data.table)
library(ggplot2)

# ---- 0. Panel construction --------------------------------------------------

PRE  <- 2003:2004          # pre-reform publication years
POST <- 2015:2016          # post-reform publication years

d <- as.data.table(all_df_reg)[year %in% c(PRE, POST) & in_type_facility==1]
d[, period := fifelse(year %in% PRE, "pre", "post")] %>%
  .[, has_competitive := (as.numeric(au_funded_ANR_ods== 1 | au_funded_ANR_dgds == 1 | au_funded_ERC ==1))] 
  

panel <- d[, .(cit = sum(citations_raw)),
           by = .(author_id, field, inst_id_set, has_competitive, period)]

# Field x period normalisation.
# NOTE: this cancels differential citation accumulation but also removes the
# cross-period level. If you want the level (to identify B = T/t_0), normalise
# instead by an EXTERNAL field x publication-year benchmark and use a fixed
# citation window per paper.
panel[, x := cit / mean(cit), by = .(field, period)]

panel[, grp := factor(paste(period, fifelse(has_competitive == 1L, "treated", "control")),
                      levels = c("pre control", "pre treated",
                                 "post control", "post treated"))]

# ---- 1. Survival curves -----------------------------------------------------

surv_fun <- function(x) {
  dt <- data.table(x = x)[x > 0][, .N, by = x][order(x)]
  dt[, S := (sum(N) - cumsum(N) + N) / sum(N)][, .(x, S)]
}

sv <- panel[x > 0, surv_fun(x), by = grp]

p_surv <- ggplot(sv, aes(x, S, colour = grp)) +
  geom_step(direction = "hv") +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl") +
  labs(x = "citations / field mean (log)",
       y = expression(P(X >= x)), colour = NULL) +
  theme_minimal()
print(p_surv)

# ---- 2. Bootstrap bands, clustered on institution ---------------------------

grid <- 10^seq(-2, 1.7, length.out = 60)

boot_surv <- function(dat, B = 300) {
  insts <- unique(dat$inst_id_set)
  out <- matrix(NA_real_, B, length(grid))
  for (b in seq_len(B)) {
    pick <- data.table(inst_id_set = sample(insts, length(insts), replace = TRUE))
    bs   <- merge(pick, dat, by = "inst_id_set", allow.cartesian = TRUE)
    out[b, ] <- vapply(grid, function(g) mean(bs$x >= g), numeric(1))
  }
  data.table(x  = grid,
             lo = apply(out, 2, quantile, 0.025, na.rm = TRUE),
             hi = apply(out, 2, quantile, 0.975, na.rm = TRUE))
}

bands <- panel[x > 0, boot_surv(.SD), by = grp, .SDcols = c("x", "inst_id_set")]

p_bands <- ggplot() +
  geom_ribbon(data = bands, aes(x, ymin = lo, ymax = hi, fill = grp), alpha = 0.20) +
  geom_step(data = sv, aes(x, S, colour = grp), direction = "hv") +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl") +
  labs(x = "citations / field mean (log)",
       y = expression(P(X >= x)), colour = NULL, fill = NULL) +
  theme_minimal()
print(p_bands)

# ---- 3. Quantile-ratio test -------------------------------------------------
# Model: output elasticity in type is 1 below the cutoff and 1 + alpha*t2 above.
# => log[Q_treated(u)/Q_control(u)] is FLAT below the cutoff and rises with
#    slope alpha*t2 against log Q_control(u), in the POST period only.
#    The PRE line is the placebo: it should be flat throughout.

u <- seq(0.05, 0.995, by = 0.005)

qt <- panel[x > 0, .(u = u, q = as.numeric(quantile(x, u, type = 7))), by = grp]
qw <- dcast(qt, u ~ grp, value.var = "q")
setnames(qw, make.names(names(qw)))

qw[, `:=`(ratio_pre  = `pre.treated`  / `pre.control`,
          ratio_post = `post.treated` / `post.control`)]

qq <- rbind(
  qw[, .(u, period = "pre",  base_q = `pre.control`,  ratio = ratio_pre)],
  qw[, .(u, period = "post", base_q = `post.control`, ratio = ratio_post)]
)

p_ratio <- ggplot(qq, aes(base_q, ratio, colour = period)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "control-group quantile (log)",
       y = "treated / control quantile ratio (log)", colour = NULL) +
  theme_minimal()
print(p_ratio)

# Difference-in-differences version: post log-ratio minus pre log-ratio.
did <- qw[, .(u, base_q = `post.control`, did = log(ratio_post) - log(ratio_pre))]

p_did <- ggplot(did, aes(base_q, did)) +
  geom_line() + geom_hline(yintercept = 0, linetype = 2) +
  scale_x_log10() +
  labs(x = "control-group quantile (log)",
       y = "DiD in log quantile ratio") +
  theme_minimal()
print(p_did)

# Slope above a candidate cutoff quantile -> estimate of alpha*t2
CUT_U <- 0.80
summary(lm(log(ratio_post) ~ log(base_q),
           data = qq[period == "post" & u > CUT_U]))

# ---- 4. Dispersion DiD ------------------------------------------------------
# The model predicts treated institutions become MORE UNEQUAL internally,
# not necessarily more productive on average.

vd <- panel[x > 0, .(v = var(log(x)), m = mean(log(x)), n = .N),
            by = .(inst_id_set, has_competitive, period)][n >= 20]
vd[, post := as.integer(period == "post")]

summary(lm(v ~ has_competitive * post, data = vd))   # interaction = dispersion effect
summary(lm(m ~ has_competitive * post, data = vd))   # level effect, for comparison

# ---- 5. Individual-level kinked slope ---------------------------------------
# log(post output) on log(pre output), allowing a kink and a treatment
# interaction. Model: slope 1 below the kink; slope 1 + alpha*t2 above it,
# and the extra steepening should appear only for treated institutions.

w <- dcast(panel, author_id + inst_id + treated ~ period, value.var = "x")
w <- w[pre > 0 & post > 0]
w[, `:=`(lpre = log(pre), lpost = log(post))]

# profile over candidate kink locations, pick the best-fitting one
cands <- quantile(w$lpre, seq(0.5, 0.95, by = 0.01))
rss <- vapply(cands, function(k) {
  ww <- copy(w)[, above := pmax(lpre - k, 0)]
  sum(resid(lm(lpost ~ lpre + above * treated, data = ww))^2)
}, numeric(1))
kink <- cands[which.min(rss)]

w[, above := pmax(lpre - kink, 0)]
fit <- lm(lpost ~ lpre + above * treated, data = w)
summary(fit)
# interpretation:
#   coef(lpre)            should be ~1        (placebo below the cutoff)
#   coef(above:treated)   estimates alpha*t2  (extra steepening where the contest bites)
#   cluster SEs on inst_id before reporting, e.g. with fixest::feols


library(gamlss)
library(gamlss.dist)

make_nll <- function(fam) {
  fo    <- get(fam)()
  pars  <- names(fo$parameters)[unlist(fo$parameters)]
  links <- vapply(pars, function(p) fo[[paste0(p, ".link")]], character(1))
  dfun  <- get(paste0("d", fam))
  inv   <- function(l, x) switch(l, log = exp(x), logit = plogis(x), identity = x, exp(x))
  function(p, y, off) {
    args <- list(x = y, log = TRUE)
    for (i in seq_along(pars))
      args[[pars[i]]] <- inv(links[i], if (pars[i] == "mu") p[i] + off else p[i])
    v <- -sum(do.call(dfun, args))
    if (!is.finite(v)) 1e10 else v
  }
}
  
panel[, off := log(mean(cit)), by = .(field, period)]# field benchmark as exposure
d <- as.data.frame(panel[period == "pre", .(y = as.integer(cit), off)] %>% .[off>0]) %>%
  sample_frac(0.1)

fams <- c("PO","NBI","PIG","SICHEL","BNB",           # no structural zeros
          "ZIP","ZINBI","ZIPIG","ZIBNB",             # + atom at zero
          "ZANBI","ZABNB")                           # hurdle variants

fit_fam <- function(fam, y, off) {
  fo   <- get(fam)()
  pars <- names(fo$parameters)[unlist(fo$parameters)]
  k    <- length(pars)
  o    <- nlminb(c(log(mean(y) + 0.1), rep(-0.5, k - 1)), make_nll(fam), y = y, off = off)
  list(family = fam, pars = pars, par = o$par, k = k,
       logLik = -o$objective,
       AIC = 2 * o$objective + 2 * k,
       BIC = 2 * o$objective + log(length(y)) * k,
       conv = o$convergence)
}

fits <- Filter(Negate(is.null), lapply(fams, function(f)
  tryCatch(fit_fam(f, d$y, d$off), error = function(e) NULL)))

res <- do.call(rbind, lapply(fits, function(f)
  data.frame(family = f$family, k = f$k, logLik = f$logLik,
             AIC = f$AIC, BIC = f$BIC, conv = f$conv)))
res[order(res$BIC), ]
msurv <- function(f, off, grid) {
  fo    <- get(f$family)()
  links <- vapply(f$pars, function(p) fo[[paste0(p, ".link")]], character(1))
  inv   <- function(l, x) switch(l, log = exp(x), logit = plogis(x), identity = x, exp(x))
  pfun  <- get(paste0("p", f$family))
  args  <- list()
  for (i in seq_along(f$pars))
    args[[f$pars[i]]] <- inv(links[i], if (f$pars[i] == "mu") f$par[i] + off else f$par[i])
  vapply(grid, function(g) mean(1 - do.call(pfun, c(list(q = g - 1), args))), numeric(1))
}

off_s <- sample(d$off, min(length(d$off), 5000))
grid  <- unique(round(10^seq(0, log10(max(d$y)), length.out = 60)))

emp <- data.table(x = grid,
                  S = vapply(grid, function(g) mean(d$y >= g), numeric(1)),
                  family = "empirical")

curves <- rbindlist(lapply(fits, function(f) {
  s <- tryCatch(msurv(f, off_s, grid), error = function(e) NULL)
  if (is.null(s)) NULL else data.table(x = grid, S = s, family = f$family)
}))

ggplot(rbind(emp, curves) %>% 
         filter(family %in% c('ZIBNB','ZABNB','ZINBI','NBI','empirical'))
         , aes(x, S, colour = family)) +
  geom_line() +
  scale_x_log10() + scale_y_log10() + annotation_logticks(sides = "bl") +
  labs(x = "citations (log)", y = expression(P(Y >= x)), colour = NULL) +
  theme_minimal()
