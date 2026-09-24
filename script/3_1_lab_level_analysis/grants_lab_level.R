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

save_path = paste0("D:\\panel_fr_res\\results\\productivity_lab_grants\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

ds <- open_dataset(inputpath) %>%
  filter(first_year_lab <= 2003  )
nrow(ds)
ds <- as.data.table(ds) #524838
gc()

nrow(unique(ds[, list(inst_id, domain)])) #12651

df_reg <- ds %>%
  .[, treat_personal_grant := pmin(first_year_dgds, first_year_erc)] %>%
  .[first_year_ods == 0 & first_year_ods_inst == 0
    & (treat_personal_grant %in% 2005:2025 | treat_personal_grant ==0)
    ] %>%
  .[, ':='(idn = as.integer(factor(paste0(inst_id, '_', domain))),
           year_n = as.integer(year)
           )] 
gc()

selection_period= 2000:2002
 
df_reg <-df_reg %>%
  .[, ':='(entry_cohort = floor(first_year_lab/5)*5) ] %>%
  .[, ':='(pub_selection = sum(as.numeric(year %in% selection_period)*publications),
           cit_selection = sum(as.numeric(year %in% selection_period)*citations),
           n_au_selection = mean(ifelse(year %in% selection_period & total > 0, total, NA), na.rm = T)
  ), by= 'idn'] %>%
  .[, n_au_selection := ifelse( is.na(n_au_selection) | n_au_selection == Inf | n_au_selection == -Inf, 0, n_au_selection)]%>%
  .[pub_selection>0 & type == 'facility']
length(unique(df_reg$idn)) #6888

df_reg <- df_reg %>%
  .[, ':='(pub_n_tile = cut(pub_selection, unique(quantile(unique(df_reg[, list(idn, pub_selection)])$pub_selection,
                                                           probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, ':='(cit_n_tile = cut(cit_selection, unique(quantile(unique(df_reg[, list(idn, cit_selection)])$cit_selection,
                                                           probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[, ':='(n_au_n_tile = cut(cit_selection, unique(quantile(unique(df_reg[, list(idn, n_au_selection)])$n_au_selection,
                                                           probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))), include_lowest = T, labels = FALSE))
  ] %>% 
  .[year >=2003]


outcomes_to_keep <- c('total', 'total_entrant', 'total_foreign_entrant',
                      'nr_coau_under_5y','nr_coau_under_10y','citations_coau_under_5y',
                      'nr_coau_foreign_entrant','nr_coau_foreign_entrant_under_5y',
                      'citations_coau_foreign_entrant_under_5y', 
                      'publications','citations','new_phrase_comb_reuse')

dict_vars <- c('total' = 'Number of researchers',
               'total_entrant' = 'Number of new researchers',
               'total_w' = 'Number of researchers',
               'total_men' = 'Number of male researchers',
               'total_women' = 'Number of female researchers',
               'total_w_men' = 'Number of male researchers',
               'total_w_women' = 'Number of female researchers',

               'nr_coau_under_5y' = 'Publications (recently arrived)',
               'nr_coau_under_10y' = 'Publications (recently arrived)',
               'citations_coau_under_5y' = 'Citations  (recently arrived)',
               'new_phrase_comb_reuse_coau_under_5y' = 'New phrases (recently arrived)',
               'nr_source_top_5pct_raw_coau_under_5y' = "Publications in top 5% cited journals (recently arrived)",
               "semantic_distance_coau_under_5y" = "Semantic distance to existing works (recently arrived)",
               
               
               'nr_coau_junior_under_5y' = 'Publications by juniors (recently arrived)',
               'nr_coau_junior_under_10y' = 'Publications by juniors (recently arrived)',

               'nr_coau_senior_under_5y' = 'Publications by seniors (recently arrived)',
               'nr_coau_senior_under_10y' = 'Publications by seniors (recently arrived)',

               'nr_coau_medium_under_5y' = 'Publications by mid-career (recently arrived)',
               'nr_coau_medium_under_10y' = 'Publications by mid-career (recently arrived)',

               'total_foreign_entrant' = 'Number of foreign entrants',
               'nr_coau_foreign_entrant' = 'Publications by foreign entrants',
               'nr_coau_foreign_entrant_under_5y' = 'Publications by foreign entrants (recently arrived)',
               'citations_coau_foreign_entrant_under_5y' = 'Citations by foreign entrants (recently arrived)',
               'new_phrase_comb_reuse_coau_foreign_entrant_under_5y' = 'New phrases by foreign entrants (recently arrived)',
               'nr_source_top_5pct_raw_coau_foreign_entrant_under_5y' = "Publications in top 5% cited journals by foreign entrants (recently arrived)",
               "semantic_distance_coau_foreign_entrant_under_5y" = "Semantic distance to existing works (recently arrived foreign entrants)",
               
               'publications' = 'Publications',
               'citations'= 'Citations',
               'new_phrase_comb_reuse'= "New phrases",
               'nr_source_top_5pct_raw' = "Publications in top 5% cited journals",
               'nr_source_top_10pct_raw' = "Publications in top 10% cited journals",
               'nr_source_top_20pct_raw' = "Publications in top 20% cited journals",
               "semantic_distance" = 'Semantic distance to existing works'
               
               )
table(unique(df_reg[, list(idn, treat_personal_grant)])$treat_personal_grant)

table(unique(df_reg %>% .[, treat := ifelse(treat_personal_grant == 0, 0, 1)] %>%
               .[, list(idn, treat)])$treat)
table(unique(df_reg %>% .[, treat := ifelse(treat_personal_grant == 0, 0, 1)] %>%
               .[, list(idn, treat, domain)])$treat,
      unique(df_reg %>% .[, treat := ifelse(treat_personal_grant == 0, 0, 1)] %>%
               .[, list(idn, treat, domain)])$domain)

length(unique(df_reg$idn))

controls = c('domain', 'cnrs' ,'entry_cohort','pub_n_tile', 'n_au_n_tile'
             )
gc()
for(outcome in names(dict_vars)){
  print(outcome)
  es_stag <- did::att_gt(yname = outcome, tname = 'year_n', idname = 'idn',
                         gname = "treat_personal_grant", data = df_reg,
                         xformla = as.formula(paste0('~', paste0(controls, collapse = '+'))),
                         control_group = 'notyettreated', clustervars = 'idn')
  
  #x_lim <- c(min(d_sep$year) - min(es_stag$group), max(d_sep$year) - max(es_stag$group))
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, min_e= -10, max_e = 15
  )
  
  plot <- ggdid(es_aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black","black")) +
    geom_vline(xintercept = -0.5, colour = 'firebrick') +
    theme_bw() + theme(legend.position = 'none') +
    xlab('Time to treatment') + ylab(dict_vars[[outcome]]) + 
    labs(title = '')
  
  print(plot_print + labs(title = paste0(outcome)))
  
  ggsave(plot = plot_print, filename = file.path(save_path, "estimates", path = paste0(outcome, ".png")))
  # strip the data from es_stag/plot since it's saved once in base_data.rds
  #es_stag$DIDparams$data <- NULL
  plot_print$data <- NULL     # ggplot no longer self-contained for re-rendering, but fine for viewing
  
  out <- list(regression = es_stag, aggte_dyn = es_aggte_dyn, plot = plot_print)
  
  saveRDS(out,
          file.path(save_path, "estimates", paste0(outcome, ".rds")),
          compress = FALSE)
  
  rm(out, es_stag, es_aggte_dyn, plot, plot_print); gc()
}

# one row per unit: its first observed year (pre-treatment for units att_gt keeps)
base <- df_reg[order(idn, year_n)][!duplicated(idn)]
base[, D := as.integer(treat_personal_grant != 0)]

W <- weightit(as.formula(paste("D ~", paste(controls, collapse = "+"))),
              data = base, method = "glm", estimand = "ATT")

# love plot: standardized mean differences before/after weighting
love.plot(W, abs = TRUE, thresholds = c(m = 0.1),
          var.order = "unadjusted", stars = "std")

# propensity score overlap
bal.plot(W, var.name = "prop.score", which = "both", type = "histogram", mirror = TRUE, colors = c('firebrick','steelblue'))
ggsave( filename = file.path(save_path, "estimates", path = paste0("balance_plot.png")))


all_est <- list()
for(outcome in c('total'
                 ,'publications','citations','new_phrase_comb_reuse',
                 'total_w','nr_coau_under_5y', 'citations_coau_under_5y'
                 )){
      all_est[[outcome]] <- readRDS(
        file.path(save_path, "estimates", paste0(outcome, ".rds")) )
      
      all_est[[outcome]]$aggte_simple <- aggte(all_est[[outcome]]$regression, type = 'simple')
      
      all_est[[outcome]]$pre_mean <- round(mean((df_reg %>% .[year < treat_personal_grant])[[outcome]] ),2)
      
}

fmt_aggte <- function(a, digits = 3) {
  p <- 2 * pnorm(-abs(a$overall.att / a$overall.se))
  stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  c(sprintf(paste0("%.", digits, "f%s"), a$overall.att, stars),
    sprintf(paste0("(%.", digits, "f)"), a$overall.se))
}

tab <- sapply(c('total'
                ,'publications','citations','new_phrase_comb_reuse'), function(o) {
  c(fmt_aggte(all_est[[o]]$aggte_simple),
    format(all_est[[o]]$regression$n, big.mark = ","),   # number of units
    format(all_est[[o]]$pre_mean, big.mark = ",")
    )                                  # comparison group
})

rownames(tab) <- c("ATT", " ", "N", "Pre Treat. Avg.")
colnames(tab) <- unlist(dict_vars[colnames(tab)])       # readable outcome names

stargazer(tab, type = "text")                           # preview

stargazer(tab, type = "latex",
          title = "Average treatment effect on the treated",
          label = "tab:att_simple",
          notes = c("Callaway and Sant'Anna (2021) estimator, simple aggregation.",
                    "Standard errors clustered at the laboratory level in parentheses.",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          out = file.path(save_path, "att_pub.tex"))



tab <- sapply(c('total_w','nr_coau_under_5y', 'citations_coau_under_5y'), function(o) {
                  c(fmt_aggte(all_est[[o]]$aggte_simple),
                    format(all_est[[o]]$regression$n, big.mark = ","),   # number of units
                    format(all_est[[o]]$pre_mean, big.mark = ",")
                  )                                  # comparison group
                })

rownames(tab) <- c("ATT", " ", "N", "Pre Treat. Avg.")
colnames(tab) <- unlist(dict_vars[colnames(tab)])       # readable outcome names

stargazer(tab, type = "text")                           # preview

stargazer(tab, type = "latex",
          title = "Average treatment effect on the treated",
          label = "tab:att_simple_2",
          notes = c("Callaway and Sant'Anna (2021) estimator, simple aggregation.",
                    "Standard errors clustered at the laboratory level in parentheses.",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          out = file.path(save_path, "att_pub_2.tex"))
