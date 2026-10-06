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
df_reg <- df_reg %>%
  .[, ':='(pub_n_tile = cut(pub_selection, unique(quantile(unique(df_reg[, list(idn, pub_selection)])$pub_selection,
                                                           probs = quants_to_cut)), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, ':='(cit_n_tile = cut(cit_selection, unique(quantile(unique(df_reg[, list(idn, cit_selection)])$cit_selection,
                                                           probs = quants_to_cut)), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, ':='(n_au_n_tile = cut(cit_selection, unique(quantile(unique(df_reg[, list(idn, n_au_selection)])$n_au_selection,
                                                            probs = quants_to_cut)), include_lowest = T, labels = FALSE))
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
               
               
               'stayers' = 'Number of stayers',
               'stayers_w'= 'Number of stayers',
               'movers'= 'Number of movers',
               'movers_w'= 'Number of movers',
               
               
               
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
               'total_from_abroad' = 'Number of researchers w. foreign affiliation',
               'total_w_from_abroad' = 'Number of researchers w. foreign affiliation',
               
               
               'stayers_foreign_entrant' = 'Number of foreign entrant stayers',
               'stayers_w_foreign_entrant'= 'Number of foreign entrant stayers',
               'movers_foreign_entrant'= 'Number of foreign entrant movers',
               'movers_w_foreign_entrant'= 'Number of foreign entrant movers',
               
               
               'total_from_t_company' = 'Number of researchers w. previous company affiliation',
               'total_w_from_t_company' = 'Number of researchers  w. previous company affiliation',
               'total_from_privé' = 'Number of researchers w. previous private affiliation',
               'total_w_from_privé' = 'Number of researchers w. previous private affiliation',
               
               
               'exits' = 'Number of retiring researchers',
               'exits_w' = 'Number of retiring researchers',
               'total_entrant' = 'Number of first-year researchers',
               'total_w_entrant' = 'Number of first-year researchers',
               
               'nr_coau_foreign_entrant' = 'Publications by foreign entrants',
               'citations_coau_foreign_entrant' = 'Citations by foreign entrants',
               "nr_coau_from_abroad"  = 'Publications by foreign affiliated',
               'citations_coau_from_abroad' = 'Citations by foreign affiliated',
               
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
               'second_wave_idex' = 'Autonomy - 2nd wave IDEX',
               
               'domain' = 'Domain',
               'pub_n_tile' = 'Quartile of Pub. (selection)',
               'cit_n_tile' = 'Quartile of Cit. (selection)',
               'n_au_n_tile' = 'Quartile of Nb. Researchers (selection)'
               
)
controls = c('domain', 'type','cnrs','pub_n_tile'
)
gc()
est_path  <- file.path(save_path, "estimates")

if (!dir.exists(est_path)) {
  dir.create(est_path, recursive = TRUE)
}

outcomes_to_keep <- c('total','total_w',
                      'stayers','stayers_w',
                      'movers','movers_w',
                      'stayers_foreign_entrant','stayers_w_foreign_entrant',
                      'movers_foreign_entrant','movers_w_foreign_entrant',
                      'share_women',
                      'total_foreign_entrant', 
                      "total_w_foreign_entrant",
                      'nr_coau_foreign_entrant','nr_coau_foreign_entrant_under_5y',
                      "citations_coau_foreign_entrant",
                      "nr_coau_from_abroad",
                      'nr_coau_under_5y', 'citations_coau_under_5y',
                      'total_junior','total_senior','total_medium',
                      'total_w_junior','total_w_senior','total_w_medium',
                      'exits','exits_w',#'total_entrant','total_w_entrant',
                      'total_from_abroad', 'total_w_from_abroad',
                      'total_from_t_company','total_w_from_t_company','total_from_privé','total_w_from_privé',
                      'publications','avg_publications',
                      'citations','avg_citations',
                      'nr_source_top_5pct','avg_nr_source_top_5pct',
                      'new_phrase_comb_reuse','avg_new_phrase_comb_reuse'
                      )

list_est_together <- list()

df_reg <- df_reg %>%
  .[, (outcomes_to_keep) := lapply(.SD, wins_vars, pct_level =0.01) , .SDcols = outcomes_to_keep] 
crit <- qnorm(0.975)   # 1.96 for a 95% CI; change to qnorm(0.95) for 90%

treat <- 'acces_rce'
for(outcome in outcomes_to_keep){
  

  start_time_treat <- Sys.time()

  
  print(paste0('Estimating for outcome: ', outcome))
  start_time_est_outcome <- Sys.time()
  list_est_together[[outcome]] <- list()
  
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "acces_rce",
                         data = df_reg ,
                         base_period = "universal",
                         ,xformla = as.formula(paste0('~',
                                                      paste0(controls, collapse = '+')))
                         ,control_group = 'notyettreated',clustervars = 'inst_id'
  )
  
  list_est_together[[outcome]]$regression <- es_stag
  
  print(paste0("Finished the estimation for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  x_lim <- c(min(df_reg$year)-min(es_stag$group),  max(df_reg$year)-max(es_stag$group))
  
  start_time_plot <- Sys.time()
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                        min_e = x_lim[1], max_e = x_lim[2])
  list_est_together[[outcome]]$aggte_dyn <- es_aggte_dyn
  plot <- ggdid(es_aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
    geom_vline(xintercept = -0.5, colour = 'firebrick')+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
  print(plot_print + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
  
  ggsave(plot = plot_print, filename = file.path(save_path, "estimates", paste0(outcome, ".png")))
  
  
  list_est_together[[outcome]]$plot <- plot_print
  
  print(paste0("Finished the plot for ", outcome, ' in:'))
  print(Sys.time()-start_time_plot)
  
  print(paste0("Finished for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  out <- list(regression = es_stag, aggte_dyn = es_aggte_dyn, plot = plot_print)
  
  saveRDS(out,
          file.path(save_path, "estimates", paste0(outcome, ".rds")),
          compress = FALSE)
  
  rm(out, es_stag, es_aggte_dyn, plot, plot_print); gc()
  
  gc()
}
list_est_together <- list()
for(outcome in outcomes_to_keep){
  list_est_together[[outcome]] <- readRDS(file.path(save_path, "estimates", paste0(outcome, ".rds")))
}


rows <- list()

get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}
for(outcome in names(list_est_together)){
    print(treat)
    print(outcome)
    res  <- list_est_together[[outcome]]
    es_stag <- res$regression
    
    agg_simple <- aggte(es_stag, type = 'simple',na.rm = TRUE)
    att <- agg_simple$overall.att
    se  <- agg_simple$overall.se
    z   <- att / se
    p   <- 2 * (1 - pnorm(abs(z)))
    stars <- get_stars(p)
    ci_low  <- att - crit * se
    ci_high <- att + crit * se
    
    # data underlying this att_gt run
    d <- es_stag$DIDparams$data
    
    # number of units (all units in the estimation sample)
    n_units <- length(unique(d$idn))
    
    # pre-treatment average of the outcome, among eventually-treated units,
    # in periods before their treatment year (treatment == 0 group timing var excluded)
    treated_ids <- unique(d$idn[d$acces_rce != 0])
    pre_data <- d[d$idn %in% treated_ids & d$year_n <= 2007, ]
    pre_mean <- mean(pre_data[[outcome]], na.rm = TRUE)
    
    rows[[outcome]] <- data.frame(
      Outcome      = dict_vars[[outcome]],
      ATT          = round(att, 3),
      Stars        = stars,
      SE           = round(se, 3),
      CI_low       = round(ci_low, 3),
      CI_high      = round(ci_high, 3),
      CI           = sprintf("[%.3f, %.3f]", ci_low, ci_high),  # ready for a table
      PreTreatMean = round(pre_mean, 3),
      N_units      = n_units,
      stringsAsFactors = FALSE
    )
}
res_main <- rbindlist(rows, idcol = "outcome")[, treat := 'All']

fwrite(res_main, file = file.path(est_path, paste0('att_', paste0(controls, collapse = '__'), '.csv') ))
res_main <- fread(file.path(est_path, paste0('att_', paste0(controls, collapse = '__'), '.csv') )) %>%
  .[, `:=`(
  est    = ATT,
  std    = SE,
  pvalue = 2 * pnorm(-abs(ATT / SE)),
  var    = outcome,          # technical key; labels come from var_map
  ctrl   = paste0(controls, collapse = '+'),
  n_obs  = N_units,
  pre_mean = PreTreatMean
)]


make_stargazer_like_table_dt_v2(res_main,
                                         digits = 3,
                                         note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy on the number of researchers in treated institutions per domain and per category: all (1), researchers who first published abroad (2), in the past 5 years (3), between 5 to 15 years prior to each year (4), and above 15 years (5), the number of researchers whose last publication was recorded this year (6) and the share of women (7). Results are obtained by estimation of \\cite{callaway2021difference}, using the never-treated and not-yet-treated units as controls. Each researcher with n>1 affiliations is counted as 1/n researcher at each affiliation. Variables are winsorized at the 1\\% level.',
                                         save_path = file.path(est_path, 'att_mobility_lab_level.tex' ),
                                         var_map = dict_vars,
                                         treat_map = NULL,
                                         var_order = c('total_w','total_w_foreign_entrant', 'total_w_junior','total_w_medium','total_w_senior', 'exits_w','share_women'),
                                         treat_order = NULL,
                                         drop_unlisted_vars = TRUE,
                                         pre_mean_label = "Pre-treatment mean",
                                         n_label = "N (units)",
                                         add_stars = TRUE
                                         )



make_stargazer_like_table_dt_v2(res_main,
                                digits = 3,
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy on the number of researchers in treated institutions per domain and per category: all (1), researchers who first published abroad (2), in the past 5 years (3), between 5 to 15 years prior to each year (4), and above 15 years (5) and the number of researchers whose last publication was recorded this year (6). Results are obtained by estimation of \\cite{callaway2021difference}, using the never-treated and not-yet-treated units as controls. Variables are winsorized at the 1\\% level.',
                                save_path = file.path(est_path, 'att_mobility_lab_level_alt.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('total','total_foreign_entrant', 'total_junior','total_medium','total_senior', 'exits'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)

make_stargazer_like_table_dt_v2(res_main,
                                digits = 3,
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy on the number of researchers researchers whose last affiliation prior to the current one was abroad, weighted by number of affiliations (1) and unweighted (2), and their number of publications (3). Results are obtained by estimation of \\cite{callaway2021difference}, using the never-treated and not-yet-treated units as controls. Variables are winsorized at the 1\\% level.',
                                save_path = file.path(est_path, 'att_mobility_prod_from_abroad.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('total_w_from_abroad','total_from_abroad', 'nr_coau_from_abroad'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)


make_stargazer_like_table_dt_v2(res_main,
                                digits = 3,
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy for treated institutions on total publications (1) and total citations (2) by researchers who became affiliated with the lab in the past 5 years, and total publications (3) and total citations (4) by researchers affiliated with the lab who first published abroad. Results are obtained by estimation of \\cite{callaway2021difference}, using the never-treated and not-yet-treated units as controls. Variables are winsorized at the 1\\% level.',
                                save_path = file.path(est_path, 'att_productivity_newcomers.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('nr_coau_under_5y','citations_coau_under_5y', 'nr_coau_foreign_entrant', 'citations_coau_foreign_entrant'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)



make_stargazer_like_table_dt_v2(res_main,
                                digits = 3,
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy for treated institutions on total publications (1), average publications (2), total citations (3), average citations (4), total publications in the 5\\% most cited journals (5), average publications in the top 5\\% most cited journals (6), number of new phrase combinations weighted by reuse (7) and average number of new phrase combinations weighted by reuse (8) of researchers affiliated with the institution. Results are obtained by estimation of \\cite{callaway2021difference}, using the never-treated and not-yet-treated units as controls. Variables are winsorized at the 1\\% level.',
                                save_path = file.path(est_path, 'att_productivity.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('publications','avg_publications', 'citations', 'avg_citations','nr_source_top_5pct','avg_nr_source_top_5pct','new_phrase_comb_reuse','avg_new_phrase_comb_reuse'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)

make_stargazer_like_table_dt_v2(res_main,
                                digits = 3,
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy for treated institutions on the number of researchers with last affiliation at this institution (1) and who just entered this institution (2), and on the same values for the subset of researchers who first published outside of France. Results are obtained by estimation of \\cite{callaway2021difference}, using the never-treated and not-yet-treated units as controls. Variables are winsorized at the 1\\% level.',
                                save_path = file.path(est_path, 'stayer_mover.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('stayers_w','movers_w', 'stayers_w_foreign_entrant', 'movers_w_foreign_entrant'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)





# Heterogeneity by IDEX wave -----------------------------------------------------------

controls = c('domain'#, #'type'#, 'cnrs'#,'pub_n_tile'
)
gc()


for(treat in c('acces_rce_plain','first_wave_idex',
               'second_wave_idex'
)){
  print(paste0("Computing the loop for: ", dict_vars[[treat]]))
  list_est[[treat]] <- list()
  
  for(outcome in outcomes_to_keep){
    
    cols_to_keep <- c( outcome, "idn", "year_n", 'inst_id',
                       "treatment", controls)
    
    
    start_time_treat <- Sys.time()
    d_sep <- df_reg[treatment %in% c("control", treat) #& author_id %in% keep
    ] %>%
      .[, ":="(entry_cohort = as.factor(entry_cohort),
               domain = as.factor(domain),
               pub_n_tile = as.factor(pub_n_tile),
               cit_n_tile = as.factor(cit_n_tile),
               treatment = as.numeric(as.character(get(treat))) )] %>%
      .[, ..cols_to_keep]
    
    
    
    print(paste0('Estimating for outcome: ', outcome))
    start_time_est_outcome <- Sys.time()
    list_est[[treat]][[outcome]] <- list()
    
    es_stag <- did::att_gt(yname = outcome,
                           tname = 'year_n',
                           idname = 'idn',
                           gname = "treatment",
                           data = d_sep ,
                           allow_unbalanced_panel = T,
                           ,xformla = as.formula(paste0('~',
                                                        paste0(controls, collapse = '+')))
                           ,control_group = 'notyettreated',clustervars = 'inst_id'
    )
    
    list_est[[treat]][[outcome]]$regression <- es_stag
    
    print(paste0("Finished the estimation for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    
    x_lim <- c(min(d_sep$year)-min(es_stag$group),  max(d_sep$year)-max(es_stag$group))
    
    start_time_plot <- Sys.time()
    es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                          min_e = x_lim[1], max_e = x_lim[2])
    list_est[[treat]][[outcome]]$aggte_dyn <- es_aggte_dyn
    plot <- ggdid(es_aggte_dyn)
    plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
      geom_vline(xintercept = -0.5, colour = 'firebrick')+
      theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
    print(plot_print + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
    ggsave(plot = plot_print, filename = file.path(save_path, "estimates", paste0(outcome, treat, ".png")))
    
    list_est[[treat]][[outcome]]$plot <- plot_print
    
    print(paste0("Finished the plot for ", outcome, ' in:'))
    print(Sys.time()-start_time_plot)
    
    print(paste0("Finished for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    out <- list(regression = es_stag, aggte_dyn = es_aggte_dyn, plot = plot_print)
    
    saveRDS(out,
            file.path(save_path, "estimates", paste0(outcome, '_', treat, ".rds")),
            compress = FALSE)
    
    rm(out, es_stag, es_aggte_dyn, plot, plot_print); gc()
    
    gc()
  }
  print(paste0("Finished the loop for ", dict_vars[[treat]], ' in:'))
  print(Sys.time()-start_time_treat)
}


list_est <- list()
for(treat in c('acces_rce_plain','first_wave_idex',
               'second_wave_idex'
)){
  list_est[[treat]] <- list()
for(outcome in outcomes_to_keep){
  list_est[[treat]][[outcome]] <- readRDS( file.path(save_path, "estimates", paste0(outcome, '_', treat, ".rds")))
}
}

rows_het <- list()
crit <- qnorm(0.975)   # 1.96 for a 95% CI; change to qnorm(0.95) for 90%
get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}
for(treat in names(list_est)){ 
  rows_het[[treat]] <- list()
  for(outcome in names(list_est[[treat]])){
  print(treat)
  print(outcome)
  res  <- list_est[[treat]][[outcome]]
  es_stag <- res$regression
  
  agg_simple <- aggte(es_stag, type = 'simple')
  att <- agg_simple$overall.att
  se  <- agg_simple$overall.se
  z   <- att / se
  p   <- 2 * (1 - pnorm(abs(z)))
  stars <- get_stars(p)
  
  ci_low  <- att - crit * se
  ci_high <- att + crit * se
  
  d <- es_stag$DIDparams$data
  n_units <- length(unique(d$idn))
  
  treated_ids <- unique(d$idn[d$treatment != 0])
  pre_data <- d[d$idn %in% treated_ids & d$year_n <= 2007, ]
  pre_mean <- mean(pre_data[[outcome]], na.rm = TRUE)
  
  rows_het[[treat]][[outcome]] <- data.frame(
    Outcome      = dict_vars[[outcome]],
    ATT          = round(att, 3),
    Stars        = stars,
    SE           = round(se, 3),
    CI_low       = round(ci_low, 3),
    CI_high      = round(ci_high, 3),
    CI           = sprintf("[%.3f, %.3f]", ci_low, ci_high),  # ready for a table
    PreTreatMean = round(pre_mean, 3),
    N_units      = n_units,
    stringsAsFactors = FALSE
  )
  }
}

res_het <- rbindlist(
  lapply(names(rows_het), function(tr) {
    rbindlist(rows_het[[tr]], idcol = "outcome")[, treat := dict_vars[[tr]]]
  })
)

res_het <- rbind(res_het, res_main)
for(o in names(rows_het$acces_rce_plain)){
  p <- ggplot(res_het %>% .[outcome == o])+
    geom_point(aes(x = treat, y = ATT)) +
    geom_errorbar(aes(x=treat, ymin=CI_low,ymax=CI_high))+
    geom_hline(yintercept = 0, colour = 'grey10')+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[o]]) + labs(title='')
  
  print(p)
  
}


all_est_t <- list()
for(treat in names(list_est)){ 
  for(outcome in names(list_est[[treat]])){
    print(treat)
    print(outcome)
    aggte_dyn <- list_est[[treat]][[outcome]][["aggte_dyn"]]
    
    att <- aggte_dyn$att.egt
    se  <- aggte_dyn$se.egt
    t <- aggte_dyn$egt
    ci_low  <- att - crit * se
    ci_high <- att + crit * se
    
    d <- aggte_dyn$DIDparams$data
    n_units <- length(unique(d$idn))
    
    treated_ids <- unique(d$idn[d$treatment != 0])
    pre_data <- d[d$idn %in% treated_ids & d$year_n <= 2007, ]
    pre_mean <- mean(pre_data[[outcome]], na.rm = TRUE)
    
    all_est_t[[paste0(treat,outcome)]] <- as.data.frame(
      list(
      Treat        = rep(treat, length(t)),
      Outcome      = rep(outcome, length(t)),
      t            = t,
      ATT          = round(att, 3),
      SE           = round(se, 3),
      CI_low       = round(ci_low, 3),
      CI_high      = round(ci_high, 3),
      PreTreatMean = rep(round(pre_mean, 3), length(t)),
      N_units      = rep(n_units,  length(t))),
      stringsAsFactors = FALSE
    )
  }
}
all_est_t <- rbindlist(all_est_t)
for(o in unique(all_est_t$Outcome)){
  
p <- ggplot(all_est_t %>% .[Outcome == o])+
  geom_point(aes(x = t, y = ATT, color = Treat, shape = Treat), position = position_dodge(0.5)) +
  geom_errorbar(aes(x=t, ymin=CI_low,ymax=CI_high, color = Treat), position = position_dodge(0.5))+
  scale_color_manual(values = c("acces_rce_plain"= 'steelblue4',
                                "first_wave_idex" = "steelblue3" ,
                                "second_wave_idex" = "black" 
                                ), 
                      labels =c("acces_rce_plain"= dict_vars[["acces_rce_plain"]],
                                "first_wave_idex" = dict_vars[["first_wave_idex"]] ,
                                "second_wave_idex" = dict_vars[["second_wave_idex"]] 
                      )
                     )+
  scale_shape_manual(values = c("acces_rce_plain"= 16,
                                "first_wave_idex" = 17 ,
                                "second_wave_idex" = 18 
  ), 
  labels =c("acces_rce_plain"= dict_vars[["acces_rce_plain"]],
            "first_wave_idex" = dict_vars[["first_wave_idex"]] ,
            "second_wave_idex" = dict_vars[["second_wave_idex"]] 
  )
  )+
  geom_hline(yintercept = 0, colour = 'grey10', linetype = 'dashed')+
  geom_vline(xintercept = -0.5, colour = 'firebrick')+
  theme_bw()+theme(legend.position = 'bottom') + xlab('Time to treatment')+ylab(dict_vars[[o]]) + labs(title='')

print(p)
ggsave(plot = p, filename = paste0(save_path, "by_idex_wave_",
                                         paste0(c(treat, o), collapse = '_'),
                                         '.png'
))
}


# Further heterogeneity within first wave ---------------------------------



df_subset_first_wave <- df_reg %>% .[acces_rce==0 | first_wave_idex !=0] %>%
  .[, cit_half := ifelse(pub_n_tile %in% 1:2, 'low','high')]
table(df_subset_first_wave$first_wave_idex)
table(df_subset_first_wave$acces_rce)

controls <- c('domain')
list_by_cit_quantile <- list()
for(outcome in c('total_w','total_w_foreign_entrant','publications','avg_publications',
'citations','avg_citations')){
  list_by_cit_quantile[[outcome]] <- list()
for(quartile in c('low','high')){
  list_by_cit_quantile[[outcome]][[quartile]] <- list()
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "acces_rce",
                         data = df_subset_first_wave %>% .[cit_half == quartile] ,
                         allow_unbalanced_panel = T,
                         ,xformla = as.formula(paste0('~',
                                                      paste0(controls, collapse = '+')))
                         ,control_group = 'notyettreated',clustervars = 'inst_id'
  )
  list_by_cit_quantile[[outcome]][[quartile]]$regression <- es_stag
   
  
  x_lim <- c(min(d_sep$year)-min(es_stag$group),  max(d_sep$year)-max(es_stag$group))
  
  start_time_plot <- Sys.time()
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                        min_e = x_lim[1], max_e = x_lim[2])
  list_by_cit_quantile[[outcome]][[quartile]]$aggte_dyn <- es_aggte_dyn
  list_by_cit_quantile[[outcome]][[quartile]]$aggte <- aggte(es_stag, type = 'simple', na.rm = TRUE, 
                                                             min_e = x_lim[1], max_e = x_lim[2])

  plot <- ggdid(list_by_cit_quantile[[outcome]][[quartile]]$aggte_dyn)
  list_by_cit_quantile[[outcome]][[quartile]]$plot <- plot
}
  y_min <- min(c(list_by_cit_quantile[[outcome]]$low$plot$data$att- list_by_cit_quantile[[outcome]]$low$plot$data$c*list_by_cit_quantile[[outcome]]$low$plot$data$att.se,
                 list_by_cit_quantile[[outcome]]$high$plot$data$att- list_by_cit_quantile[[outcome]]$high$plot$data$c*list_by_cit_quantile[[outcome]]$high$plot$data$att.se))
  y_max <-  max(c(list_by_cit_quantile[[outcome]]$low$plot$data$att+ list_by_cit_quantile[[outcome]]$low$plot$data$c*list_by_cit_quantile[[outcome]]$low$plot$data$att.se,
                  list_by_cit_quantile[[outcome]]$high$plot$data$att+ list_by_cit_quantile[[outcome]]$high$plot$data$c*list_by_cit_quantile[[outcome]]$high$plot$data$att.se))
  for(quartile in c('low','high')){
    plot <- ggdid(list_by_cit_quantile[[outcome]][[quartile]]$aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
    geom_vline(xintercept = -0.5, colour = 'firebrick')+
    ylim(c(y_min, y_max))+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
  print(plot_print)
  ggsave(plot = plot_print, filename = file.path(save_path, "estimates", paste0(outcome, treat, quartile,".png")))
  
  list_by_cit_quantile[[outcome]][[quartile]]$plot <- plot_print
  }
}


