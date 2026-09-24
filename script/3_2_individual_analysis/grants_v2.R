
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

#sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )
save_path = paste0("D:\\panel_fr_res\\results\\grant_level_evidence\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}
sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))

# Grant recipient analysis - DGDS -----------------------------------------

sample_dgds_resp <- copy(sample_df_reg) %>%
  .[, ':='(au_ever_dgds = max(au_funded_ANR_dgds),
           au_ever_dgpie = max(au_funded_ANR_ods),
           au_ever_ERC = max(au_funded_ERC),
           year_n = as.integer(year_n),
           treat_personal_grant = min(ifelse(first_year_dgds>=2005 |first_year_erc>=2008 , 
                                                  pmin(first_year_dgds, first_year_erc), NA), na.rm = T)
           ), by = 'author_id'] %>%
  .[, treat_personal_grant := ifelse(is.na(treat_personal_grant) | treat_personal_grant == Inf| treat_personal_grant==-Inf,
                                0, treat_personal_grant)] %>%
  .[, au_ever_pg := as.numeric(( au_ever_dgds == 1  |au_ever_ERC ==1) 
                                 &  (first_year_ods>treat_personal_grant | first_year_ods ==0)
                                 & treat_personal_grant >=2005 & treat_personal_grant <= 2020) ] %>%
  .[, treat_personal_grant := treat_personal_grant*au_ever_pg] %>%
  .[, ':='(lab_funded_dgds = max(au_funded_ANR_dgds),
           lab_funded_dgpie = max(au_funded_ANR_ods),
           lab_funded_ERC = max(au_funded_ERC)), by = c('inst_id_set','year')] %>%
  .[ lab_funded_dgpie == 0 & lab_funded_dgpie ==0 ] %>% 
  .[, first_year_pg_lab := min(ifelse( (lab_funded_dgds ==1 | lab_funded_ERC == 1)
                                        & !str_detect(inst_id_set,',')
                                        & inst_id_set != ""
                                        & in_type_facility == 1
                                        , as.numeric(as.character(year)), NA), na.rm = T),
    by = 'author_id'] %>%
  .[, first_year_pg_lab:=ifelse(is.na(first_year_pg_lab) | first_year_pg_lab==Inf
                                  , 0, first_year_pg_lab)] %>%
  .[, treat_arrived := min(ifelse((lab_funded_dgds ==1 | lab_funded_ERC == 1)
                                  & !str_detect(inst_id_set,',')
                                  & inst_id_set != ""
                                  & new_af == 1
                                  & in_type_facility == 1
                                  , as.numeric(as.character(year)), NA), na.rm = T),
    by = 'author_id'] %>%
  .[, treat_arrived:=ifelse(is.na(treat_arrived) | treat_arrived==Inf, 0, treat_arrived)] %>%
  .[, treatment_group := case_when(treat_personal_grant != 0 ~ 'grant_recipient',
                                   treat_arrived != 0 ~ 'entrant',
                                   first_year_pg_lab !=0 ~'incumbent',
                                   .default = 'control')] %>%
  .[, award_pg := max(ifelse(au_ever_pg == 1 & year == treat_personal_grant, 
                         award_au_total, 0) ), by = 'author_id'] %>%
  .[, award_lab_total := sum(award_pg), by = c('inst_id_set','year')] %>%
  .[, award_lab_treatment := max(ifelse(year == first_year_pg_lab, 
                                        award_lab_total, 0) ), by = 'author_id']
  
gc()
fields_to_keep <- (sample_dgds_resp %>%
  .[, .(N = n_distinct(author_id)), by = 'field'] %>%
  .[N>=0.01*length(unique(sample_dgds_resp$author_id))])$field

sample_dgds_resp <- sample_dgds_resp %>% .[field %in% fields_to_keep] %>%
  .[, idn := as.integer(factor(str_remove(author_id, 'A')))]
gc()

table(unique(sample_dgds_resp[, list(author_id, treatment_group)])$treatment_group)

length(unique(sample_dgds_resp$author_id)) #126904

test_did <- did::att_gt(yname = "publications_raw",
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'treat_personal_grant',
                        data = sample_dgds_resp %>% .[treatment_group %in% c("grant_recipient",'control')]
                        # ,allow_unbalanced_panel = TRUE,
                        # ,faster_mode = FALSE
                         # ,xformla = ~ entry_year + field
                        ,control_group = 'nevertreated'
)
ggdid(aggte(test_did, type = 'dynamic', na.rm = TRUE))

ggdid(test_did, ncol=5)




full_nyt_version <- did::att_gt(yname = "publications_raw",
                        tname = 'year_n',
                        idname = 'idn',
                        gname = 'first_year_dgds',
                        data = sample_dgds_resp %>% .[first_year_dgds !=0]
                        # ,allow_unbalanced_panel = TRUE,
                        # ,faster_mode = FALSE
                         ,xformla = ~ entry_year + field + pub_n_tile
                        ,control_group = 'notyettreated'
)
ggdid(aggte(full_nyt_version, type = 'dynamic', na.rm = TRUE))

dir.create(file.path(save_path, "estimates"), showWarnings = FALSE)

controls <- c('entry_year', 'field','pub_n_tile')
outcomes_to_keep <- c('publications_wins', 'citations_wins',
                      'total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins', 'nr_source_top_10pct_wins')
gc()

id_cols <- c("idn", "year_n", "inst_id_set", "treatment", controls)

for(treat in c('grant_recipient','incumbent','entrant')){
  print(paste0('Treat: ', treat))
  # build d_sep once per treat on the shared covariates (recompute per outcome only the y column)
  d_sep_base <- sample_dgds_resp[treatment_group %in% c("control", treat)] %>%
    .[, ":="(entry_cohort = as.factor(entry_cohort),
             domain = as.factor(domain),
             pub_n_tile = as.factor(pub_n_tile),
             cit_n_tile = as.factor(cit_n_tile),
             inst_id_set = as.factor(inst_id_set),
             field = factor(field),
             treatment = as.numeric(case_when(treatment_group == "grant_recipient"~ treat_personal_grant,
                                              treatment_group == "entrant"~ treat_arrived,
                                              treatment_group == "incumbent"~ first_year_pg_lab,
                                              .default = 0
                                              ) )
             )]
  
  table(unique(d_sep_base[, list(author_id, treatment) ])$treatment)
  # save the shared covariate block ONCE for this treat (not once per outcome)
  saveRDS(d_sep_base[, ..id_cols],
          file.path(save_path, "estimates", paste0(treat, "__base_data.rds")),
          compress = FALSE)
  
  for(outcome in outcomes_to_keep){
    print(outcome)
    d_sep <- d_sep_base[, c(outcome, id_cols), with = FALSE]
    
    es_stag <- did::att_gt(yname = outcome, tname = 'year_n', idname = 'idn',
                           gname = "treatment", data = d_sep,
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
    
    print(plot_print + labs(title = paste0(treat, '__', outcome)))
    
    ggsave(plot = plot_print, filename = file.path(save_path, "estimates", path = paste0(treat, "__", outcome, ".png")))
    # strip the data from es_stag/plot since it's saved once in base_data.rds
    es_stag$DIDparams$data <- NULL
    plot_print$data <- NULL     # ggplot no longer self-contained for re-rendering, but fine for viewing
    
    out <- list(regression = es_stag, aggte_dyn = es_aggte_dyn, plot = plot_print)
    
    saveRDS(out,
            file.path(save_path, "estimates", paste0(treat, "__", outcome, ".rds")),
            compress = FALSE)
    
    rm(out, es_stag, es_aggte_dyn, plot, plot_print, d_sep); gc()
  }
  rm(d_sep_base); gc()
}

make_data_table_aggt <-function(x, list_to_plot = all_version_list){
  aggte_all_version = list_to_plot[[x]][["aggte"]]
  
  aggte_table <- data.table(
    var = x,
    event_time = aggte_all_version$egt,
    att        = aggte_all_version$att.egt,
    se         = aggte_all_version$se.egt,
    crit_val = aggte_all_version$crit.val.egt
  ) %>%
    .[, `:=`(
    ci_lo = att - crit_val * se,
    ci_hi = att + crit_val * se
  )]
  return(aggte_table)
  }

test <- rbindlist(lapply(names(all_version_list), make_data_table_aggt))

names(all_version_list)

ggplot(test %>% .[var %in% c('publications_raw','total_new_phrase') & event_time %in% -10:10])+
  geom_point(aes(x= event_time, y = att, color = var,shape = var), position = position_dodge(0.5))+
  geom_errorbar(aes(x=event_time, ymin = ci_lo, ymax = ci_hi, color = var), position = position_dodge(0.5))+
  theme_bw()+
  scale_color_manual(
    name   = 'Variable',
    values = c('publications_raw' = 'black', 'total_new_phrase' = 'steelblue4'),
    labels = c('publications_raw' = 'Publications', 'total_new_phrase' = 'New phrases')
  ) +
  scale_shape_manual(
    name   = 'Variable',
    values = c('publications_raw' = 16, 'total_new_phrase' = 17),
    labels = c('publications_raw' = 'Publications', 'total_new_phrase' = 'New phrases')
  )+
  xlab('Time to grant arrival')+ylab('')+
  geom_vline(aes(xintercept = -0.5), color = 'firebrick', linetype = 'dashed')+
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 'dashed')
ggsave("C:\\Users\\rapha\\Desktop\\grant_effect_pub_new_phrase.png", width = 8, height = 4.5)



ggplot(test %>% .[var %in% c('citations_raw') & event_time %in% -10:10])+
  geom_point(aes(x= event_time, y = att), position = position_dodge(0.5))+
  geom_errorbar(aes(x=event_time, ymin = ci_lo,ymax=ci_hi), position = position_dodge(0.5))+
  scale_color_manual(values = c('black'))+
  theme_bw()+
  xlab('Time to grant arrival')+ylab('')+
  geom_vline(aes(xintercept = -0.5), color = 'firebrick', linetype = 'dashed')+
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 'dashed')
ggsave("C:\\Users\\rapha\\Desktop\\grant_effect_citations.png", width = 8, height = 4.5 )



ggplot(test %>% .[var %in% c('new_phrase') & event_time %in% -10:10])+
  geom_point(aes(x= event_time, y = att), position = position_dodge(0.5))+
  geom_errorbar(aes(x=event_time, ymin = ci_lo,ymax=ci_hi), position = position_dodge(0.5))+
  scale_color_manual(values = c('black'))+
  theme_bw()+
  xlab('Time to grant arrival')+ylab('')+
  geom_vline(aes(xintercept = -0.5), color = 'firebrick', linetype = 'dashed')+
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 'dashed')
ggsave("C:\\Users\\rapha\\Desktop\\grant_effect_newphrase.png", width = 8, height = 4.5 )

as.data.table(aggte(all_version, type = 'dynamic', na.rm = TRUE))

table(sample_dgds_lab$first_year_dgds_lab)
gc()
#rm(sample_dgds_resp)
gc()



for(treat in c('grant_recipient','incumbent','entrant')){
  print(paste0('Treat: ', treat))
  # build d_sep once per treat on the shared covariates (recompute per outcome only the y column)
  d_sep_base <- sample_dgds_resp[treatment_group %in% c("control", treat)] %>%
    .[, ":="(entry_cohort = as.factor(entry_cohort),
             domain = as.factor(domain),
             pub_n_tile = as.factor(pub_n_tile),
             cit_n_tile = as.factor(cit_n_tile),
             inst_id_set = as.factor(inst_id_set),
             field = factor(field),
             treatment = as.numeric(case_when(treatment_group == "grant_recipient"~ treat_personal_grant,
                                              treatment_group == "entrant"~ treat_arrived,
                                              treatment_group == "incumbent"~ first_year_pg_lab,
                                              .default = 0
             ) )
    )] %>%
   .[, relevant_funding := if (treat == 'grant_recipient') award_pg else award_lab_treatment] %>%
    .[, quant_award := cut(
      relevant_funding,
      breaks = c(-Inf, median(relevant_funding, na.rm = TRUE), Inf),
      labels = c("low", "high")
    ), by = treatment] 
  
  summary(d_sep_base$award_lab_total)
  
  print(table(unique(d_sep_base[, list(author_id, treatment, quant_award) ])$treatment,
        unique(d_sep_base[, list(author_id, treatment, quant_award) ])$quant_award
        ))
  # save the shared covariate block ONCE for this treat (not once per outcome)
  for(quant_value in c('low','high')){
    for(outcome in outcomes_to_keep){
    print(outcome)
    d_sep <- d_sep_base %>%
      .[treatment == "control" | quant_award == quant_value] %>%
      .[, c(outcome, id_cols), with = FALSE]
    
    es_stag <- did::att_gt(yname = outcome, tname = 'year_n', idname = 'idn',
                           gname = "treatment", data = d_sep,
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
    
    print(plot_print + labs(title = paste0(treat, '__', outcome, '__', quant_value)))
    
    #ggsave(plot = plot_print, filename = file.path(save_path, "estimates", path = paste0(treat, "__", outcome, ".png")))
    # strip the data from es_stag/plot since it's saved once in base_data.rds
    #es_stag$DIDparams$data <- NULL
    plot_print$data <- NULL     # ggplot no longer self-contained for re-rendering, but fine for viewing
    
    out <- list(regression = es_stag, aggte_dyn = es_aggte_dyn, plot = plot_print)
    
    saveRDS(out,
            file.path(save_path, "estimates", paste0(treat, "__", outcome, '_funding_', quant_value, ".rds")),
            compress = FALSE)
    
    rm(out, es_stag, es_aggte_dyn, plot, plot_print, d_sep); gc()
    }
  }
  rm(d_sep_base); gc()
}



all_est_by_quant <- list()
for(quant_value in c('low','high')){
  all_est_by_quant[[quant_value]] <- list()
  
for(treat in c('grant_recipient','incumbent','entrant')){
  print(paste0('Treat: ', treat))
  
  all_est_by_quant[[quant_value]][[treat]] <- list()
    for(outcome in c('publications_wins','citations_wins','total_new_phrase_comb_reuse_wins')){
      all_est_by_quant[[quant_value]][[treat]][[outcome]] <- readRDS(
              file.path(save_path, "estimates", paste0(treat, "__", outcome, '_funding_', quant_value, ".rds")))
      
    }
  }
  gc()
}
dyn_dt <- rbindlist(lapply(names(all_est_by_quant), function(q) {
  rbindlist(lapply(names(all_est_by_quant[[q]]), function(tr) {
    rbindlist(lapply(names(all_est_by_quant[[q]][[tr]]), function(o) {
      a <- all_est_by_quant[[q]][[tr]][[o]]$aggte_dyn
      data.table(
        quant_value = q,
        treatment   = tr,
        outcome     = o,
        event_time  = a$egt,
        att         = a$att.egt,
        se          = a$se.egt,
        crit_val    = a$crit.val.egt,
        overall_att = a$overall.att,
        overall_se  = a$overall.se
      )
    }))
  }))
}))

dyn_dt[, `:=`(ci_low  = att - crit_val * se,
              ci_high = att + crit_val * se)]
for(o in c('publications_wins','citations_wins','total_new_phrase_comb_reuse_wins')){
  
  combined_plot <- ggplot(dyn_dt %>% .[outcome == o] %>%
         .[
  , quant_value := factor(quant_value, levels = c('low', 'high'))] )+
  geom_point(aes(x=event_time, y = att, color = treatment),  position = position_dodge(width = 0.5))+
  geom_errorbar(aes(x=event_time, ymin = ci_low, ymax = ci_high, color = treatment), position = position_dodge(width = 0.5))+
  scale_color_manual(
    name   = 'Type of grant recipient',
    values = c('grant_recipient' = 'black', 'entrant' = 'steelblue4',
               'incumbent' = 'forestgreen'),
    labels =  c('grant_recipient' = 'Applicant', 'entrant' = 'Arriving member',
                'incumbent' = 'Incumbent member')
  ) +
  xlab('Time to grant arrival')+ylab(dict_vars[[o]])+
  geom_vline(aes(xintercept = -0.5), color = 'firebrick', linetype = 'dashed')+
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 'dashed') +
  facet_wrap(~quant_value,  labeller = as_labeller(c(low  = 'Below-median funding',
                                                     high = 'Above-median funding')))+
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 11),
        legend.position = 'bottom')
  print(combined_plot)

  ggsave(plot = combined_plot, filename = file.path(save_path, "estimates", path = paste0(o, '__', 'quant_funding', ".png")), height = 6, width = 9)
}

  

