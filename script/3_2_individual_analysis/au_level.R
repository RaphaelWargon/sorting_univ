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
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/agg_effects.R'))


inputpath <- "D:\\panel_fr_res\\data\\panel_au_year_fr.parquet"

save_path = paste0("D:\\panel_fr_res\\results\\productivity_au\\")
if (!file.exists(save_path)){
  dir.create(save_path, recursive = TRUE)
}

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
fwrite(sample_df_reg, "D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" )
rm(ds)
gc()
fwrite(sample_df_reg, "C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

sample_df_reg <- fread("D:\\panel_fr_res\\data\\sample_df_reg_au_level_trt.csv" ) 
sample_df_reg <- fread("C:\\Users\\rapha\\Desktop\\sample_df_reg_au_level_trt.csv" )

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
           year_n = as.integer(year_n),
           treatment = case_when(interact_rce_idex!=0 ~"interact_rce_idex",
                          acces_rce!=0 ~"acces_rce",
                          date_first_idex!=0 ~"date_first_idex",
                          .default = 'control')
  )
  ] %>%
  .[!(acces_rce %in% 2013:2015)
    & (ever_in_idex_annulee ==0)
  ] %>%
  .[date_first_idex == 0 | acces_rce != 0] %>%
  .[, treatment := case_when(acces_rce != 0 & date_first_idex == 0 ~ 'acces_rce_plain',
                             acces_rce != 0 & date_first_idex <= 2012 ~ 'first_wave_idex',
                             acces_rce != 0  ~ 'second_wave_idex',
                             .default = 'control'
  ) ] %>%
  .[, acces_rce:=as.integer(acces_rce)] %>%
  .[, ':='(second_wave_idex = ifelse(treatment == 'second_wave_idex', acces_rce,0),
           first_wave_idex = ifelse(treatment == 'first_wave_idex', acces_rce,0),
           acces_rce_plain = ifelse(treatment == 'acces_rce_plain', acces_rce,0))] 
  
df_reg  %>% .[, list(author_id)] %>% distinct() %>% count() #66771
table(unique(df_reg[, list(idn, treatment)])$treatment)
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

gc()
list_est <- list()

controls <- c('entry_cohort','pub_n_tile','cit_n_tile')

field_dummies <- c()
all_fields <- sort((df_reg %>% .[, list(field)] %>% distinct() %>% separate_rows(field, sep= ',') %>% distinct())$field)
for(field in all_fields){
  print(field)
  field_var <- paste0('f_', field)
  df_reg[[field_var]] <- as.numeric(str_detect(df_reg$field, field))
  field_dummies <- c(field_dummies, field_var)
}

outcomes_to_keep <- c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins', 'nr_source_top_10pct_wins',
                      'new_phrase_comb_reuse','publications_raw','citations_raw', 
                      'nr_source_top_5pct_raw', 'nr_source_top_10pct_raw', 'total_new_phrase_comb_reuse')

list_est_together <- list()
for(outcome in c(outcomes_to_keep)){
    
    cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                       "treatment", controls, field_dummies)
    
    
    print(paste0('Estimating for outcome: ', outcome))
    start_time_est_outcome <- Sys.time()
    list_est_together[[outcome]] <- list()
    
    es_stag <- did::att_gt(yname = outcome,
                           tname = 'year_n',
                           idname = 'idn',
                           gname = "acces_rce",
                           data = df_reg 
                           ,xformla = as.formula(paste0('~',
                                                        paste0(c(controls, field_dummies), collapse = '+')))
                           ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
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
    print(plot_print)
    list_est_together[[outcome]]$plot <- plot_print
    
    print(paste0("Finished the plot for ", outcome, ' in:'))
    print(Sys.time()-start_time_plot)
    
    print(paste0("Finished for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    
    gc()
}


for(outcome in outcomes_to_keep){
  print(paste0('Saving for: ', outcome))
  saveRDS(list_est_together[[outcome]],
          file.path(save_path, "estimates", paste0("acces_rce_all", "__", outcome, ".rds")),
          compress = FALSE)
  
}

list_est_together <- list()

for(outcome in outcomes_to_keep){
  print(paste0('Loading for: ', outcome))
  list_est_together[[outcome]] <- readRDS(file.path(save_path, "estimates", paste0("acces_rce_all", "__", outcome, ".rds")))
  
}


for(outcome in outcomes_to_keep){
  print(list_est_together[[outcome]]$plot)
  ggsave(list_est_together[[outcome]]$plot, filename = file.path(save_path, "estimates", paste0(outcome, ".png")),
         width = 8, height = 6) 
}




rows <- list()
crit <- qnorm(0.975)   # 1.96 for a 95% CI; change to qnorm(0.95) for 90%

get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}
for(outcome in names(list_est_together)){
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

fwrite(res_main, file = file.path(save_path, 'estimates', paste0('att_', paste0(controls, collapse = '__'), '.csv') ))
res_main <- fread(file.path(save_path, 'estimates', paste0('att_', paste0(controls, collapse = '__'), '.csv') )) %>%
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
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy on the 
                                number of publications (1), publications in the 5% most cited journals (2), citations (3), and total number (4) and average number (5) 
                                of new phrase combinations weighed by their reuse, for researchers whose affiliation in 2007 was treated. Results are obtained by estimation of \\cite{callaway2021difference},
                                using the never-treated and not-yet-treated units as controls. Variables are winsorized at the 1\\% level. Standard errors are clustered at the 2007 affiliation level.',
                                save_path = file.path(save_path, 'estimates', 'att_prod.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('publications_wins','nr_source_top_5pct_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','new_phrase_comb_reuse'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)

make_stargazer_like_table_dt_v2(res_main,
                                digits = 3,
                                note = 'This table presents the Average Treatment Effect on the Treated of administrative autonomy on the 
                                number of publications (1), publications in the 5% most cited journals (2), citations (3), and total number
                                of new phrase combinations weighed by their reuse, for researchers whose affiliation in 2007 was treated (4). Results are obtained by estimation of \\cite{callaway2021difference},
                                using the never-treated and not-yet-treated units as controls. Standard errors are clustered at the 2007 affiliation level.',
                                save_path = file.path(save_path, 'estimates', 'att_prod.tex' ),
                                var_map = dict_vars,
                                treat_map = NULL,
                                var_order = c('publications_raw','nr_source_top_5pct_raw', 'citations_raw','total_new_phrase_comb_reuse'),
                                treat_order = NULL,
                                drop_unlisted_vars = TRUE,
                                pre_mean_label = "Pre-treatment mean",
                                n_label = "N (units)",
                                add_stars = TRUE
)
p <- ggdid(list_est_together$publications_wins$regression, ncol = 2)
fv <- names(p$facet$params$facets)[1]           # e.g. "group"
vl <- data.frame(unique(p$data[[fv]]))
names(vl) <- fv
vl$xint <- as.numeric(gsub("[^0-9]", "", as.character(vl[[fv]])))-2003  # keeps the year

plot_pub_by_g <- p +
  geom_vline(data = vl, aes(xintercept = xint), colour = "firebrick") +
  scale_colour_manual(values = c("black", "black")) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Year") + ylab(dict_vars[["publications_wins"]]) +
  labs(title = "")
plot_pub_by_g
ggsave(plot = plot_pub_by_g, file.path(save_path, 'publications_by_g.png'), width = 8, height = 12)


p <- ggdid(list_est_together$citations_wins$regression, ncol = 2)
fv <- names(p$facet$params$facets)[1]           # e.g. "group"
vl <- data.frame(unique(p$data[[fv]]))
names(vl) <- fv
vl$xint <- as.numeric(gsub("[^0-9]", "", as.character(vl[[fv]])))-2003  # keeps the year

plot_cit_by_g <- p +
  geom_vline(data = vl, aes(xintercept = xint), colour = "firebrick") +
  scale_colour_manual(values = c("black", "black")) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Year") + ylab(dict_vars[["citations_wins"]]) +
  labs(title = "")
plot_cit_by_g
ggsave(plot = plot_cit_by_g, file.path(save_path, 'citations_wins_by_g.png'), width = 8, height = 12)


outcomes_for_by_g_graph <- c("publications_wins","citations_wins",
                             "nr_source_top_5pct_wins","total_new_phrase_comb_reuse_wins")
library('purrr')
tidy_aggte <- function(nm) {
  a <- aggte(list_est_together[[nm]]$regression, type = "group")
  data.frame(
    outcome  = dict_vars[[nm]],
    e        = a$egt,
    att      = a$att.egt,
    lower    = a$att.egt - a$crit.val.egt * a$se.egt,
    upper    = a$att.egt + a$crit.val.egt * a$se.egt
  )
}


df_es <- map_dfr(outcomes_for_by_g_graph, tidy_aggte)


p <-ggplot(df_es)+
  geom_point(aes(x= e, y = att))+
  geom_errorbar(aes(x=e, ymin=lower, ymax = upper))+
  facet_wrap(~outcome, scales = "free_y")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()+theme(legend.position = 'none') + xlab('Year of treatment')+ylab("ATT") + labs(title='')
p
ggsave(p, filename = file.path(save_path, 'est_by_g.png'), width = 12, height  = 6)


# Heterogeneity -----------------------------------------------------------

save_path_het = paste0("D:\\panel_fr_res\\results\\productivity_au\\autonomy_waves")
if (!file.exists(save_path_het)){
  dir.create(save_path_het, recursive = TRUE)
}



for(treat in c('acces_rce_plain','first_wave_idex',
               'second_wave_idex'
)){
  print(paste0("Computing the loop for: ", dict_vars[[treat]]))
  list_est[[treat]] <- list()
  for(outcome in outcomes_to_keep){
    
    cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                       "treatment", controls, field_dummies)
    
    
    start_time_treat <- Sys.time()
    d_sep <- df_reg[treatment %in% c("control", treat) #& author_id %in% keep
    ] %>%
      .[, ":="(entry_cohort = as.factor(entry_cohort),
               domain = as.factor(domain),
               pub_n_tile = as.factor(pub_n_tile),
               cit_n_tile = as.factor(cit_n_tile),
               inst_set_2007 = as.factor(inst_set_2007),
               field = factor(field),
               treatment = as.numeric(as.character(get(treat))) )] %>% #.[entry_year %in% 1985:2003] %>%
      .[, ..cols_to_keep]
    
    
    
    
    print(paste0('Estimating for outcome: ', outcome))
    start_time_est_outcome <- Sys.time()
  list_est[[treat]][[outcome]] <- list()
  
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "treatment",
                         data = d_sep 
                         ,xformla = as.formula(paste0('~',
                                                      paste0(c(controls, field_dummies), collapse = '+')))
                         ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
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
  list_est[[treat]][[outcome]]$plot <- plot_print
  
  print(paste0("Finished the plot for ", outcome, ' in:'))
  print(Sys.time()-start_time_plot)
  
  print(paste0("Finished for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  gc()
  }
  print(paste0("Finished the loop for ", dict_vars[[treat]], ' in:'))
  print(Sys.time()-start_time_treat)
}

dir.create(file.path(save_path, "estimates"), showWarnings = FALSE)

for(treat in  c('acces_rce_plain','first_wave_idex',
                'second_wave_idex'
)){
  for(outcome in outcomes_to_keep){
   print(paste0('Saving for: ', treat, ' ', outcome))
    saveRDS(list_est[[treat]][[outcome]],
            file.path(save_path, "estimates", paste0(treat, "__", outcome, ".rds")),
            compress = FALSE)
    
  }
}
list_est <- list()
outcomes_to_keep <-  c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins', 'nr_source_top_10pct_wins',
                       'new_phrase_comb_reuse','publications_raw','citations_raw')

for(treat in  c('acces_rce_plain','first_wave_idex',
                'second_wave_idex'
)){
  list_est[[treat]] <- list()
  for(outcome in outcomes_to_keep){
  list_est[[treat]][[outcome]] <- readRDS(
            file.path(save_path, "estimates", paste0(treat, "__", outcome, ".rds")))
    
  }
}


for(treat in names(list_est) ){
  
  for(outcome in names(list_est[[treat]])){
    to_save <- list_est[[treat]][[outcome]]$plot
    
    print(to_save + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
    ggsave(plot = to_save, filename = paste0(save_path_het, 
                                             paste0(c(treat, outcome), collapse = '_'),
                                             '.png'
                                             ))
  }
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

ggdid(test_did, ncol= 2)  

start_time <- Sys.time()
test <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
              min_e = x_lim[1], max_e = x_lim[2], cband = TRUE, bstrap = TRUE, biters = 1)
print(Sys.time()-start_time)



# Heterogeneity by dependency on grants -----------------------------------

classification_heterogeneity <- unique(df_reg %>%
  .[, y_classification := min(ifelse(!is.na(anr_investissements_d_avenir), year, NA), na.rm = T),
    by = 'inst_id_set'] %>%
  .[y_classification == year] %>%
  .[, ratio_subv_propre := (as.numeric(anr_investissements_d_avenir) + 
                              as.numeric(anr_hors_investissements_d_avenir)
                            + as.numeric(contrats_et_prestations_de_recherche_hors_anr)
  )/(
    as.numeric(produits_de_fonctionnement_encaissables) ) ] %>%
  .[, list(inst_id_set, y_classification, ratio_subv_propre)])

summary(classification_heterogeneity)

quantiles_ratio_subv_propre = quantile(classification_heterogeneity$ratio_subv_propre, probs = c(0.33, 0.66), na.rm =T)

classification_heterogeneity <- classification_heterogeneity %>%
  .[, quantile_ratio_subv_propre := case_when(ratio_subv_propre <= quantiles_ratio_subv_propre [[1]] ~ '1',
                                              ratio_subv_propre <= quantiles_ratio_subv_propre [[2]] ~ '2',
                                              ratio_subv_propre > quantiles_ratio_subv_propre [[2]] ~ '3',
                                              )] %>%
  .[, ":="(inst_set_2007 = inst_id_set,
           inst_id_set = NULL) ]

df_reg <- merge(df_reg,
                 classification_heterogeneity, by = 'inst_set_2007', all.x = TRUE)


list_es_by_ratio_subv_propre <- list()
gc()

for(quant_value in c("1","2","3")){
  print(quant_value)
  start_time_quant = Sys.time()
  list_es_by_ratio_subv_propre[[quant_value]] <- list()
for(treat in c('acces_rce','date_first_idex',
               'interact_rce_idex'
)){
  print(paste0("Computing the loop for: ", dict_vars[[treat]]))
  list_es_by_ratio_subv_propre[[quant_value]][[treat]] <- list()
  for(outcome in outcomes_to_keep){
    
    cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                       "treatment", controls, field_dummies)
    
    
    start_time_treat <- Sys.time()
    d_sep <- df_reg[treatment %in% c("control", treat) 
                    & (quantile_ratio_subv_propre == quant_value | treatment == 'control')
    ] %>%
      .[, ":="(entry_cohort = as.factor(entry_cohort),
               domain = as.factor(domain),
               pub_n_tile = as.factor(pub_n_tile),
               cit_n_tile = as.factor(cit_n_tile),
               inst_set_2007 = as.factor(inst_set_2007),
               field = factor(field),
               treatment = as.numeric(as.character(get(treat))) )] %>% #.[entry_year %in% 1985:2003] %>%
      .[, ..cols_to_keep]
    
    
    
    
    print(paste0('Estimating for outcome: ', outcome))
    start_time_est_outcome <- Sys.time()
    list_es_by_ratio_subv_propre[[quant_value]][[treat]][[outcome]] <- list()
    
    es_stag <- did::att_gt(yname = outcome,
                           tname = 'year_n',
                           idname = 'idn',
                           gname = "treatment",
                           data = d_sep 
                           ,xformla = as.formula(paste0('~',
                                                        paste0(c(controls, field_dummies), collapse = '+')))
                           ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
    )
    
    list_es_by_ratio_subv_propre[[quant_value]][[treat]][[outcome]]$regression <- es_stag
    
    print(paste0("Finished the estimation for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    
    x_lim <- c(min(d_sep$year)-min(es_stag$group),  max(d_sep$year)-max(es_stag$group))
    
    start_time_plot <- Sys.time()
    es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                          min_e = x_lim[1], max_e = x_lim[2])
    list_es_by_ratio_subv_propre[[quant_value]][[treat]][[outcome]]$aggte_dyn <- es_aggte_dyn
    plot <- ggdid(es_aggte_dyn)
    plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
      geom_vline(xintercept = -0.5, colour = 'firebrick')+
      theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
    print(plot_print + labs(title = paste0('Treatment: ', dict_vars[[treat]])))
    list_es_by_ratio_subv_propre[[quant_value]][[outcome]]$plot <- plot_print
    
    print(paste0("Finished the plot for ", outcome, ' in:'))
    print(Sys.time()-start_time_plot)
    
    print(paste0("Finished for ", outcome, ' in:'))
    print(Sys.time()-start_time_est_outcome)
    
    gc()
  }
  print(paste0("Finished the loop for ", dict_vars[[treat]], ' in:'))
  print(Sys.time()-start_time_treat)
}
  print(paste0("Finished the loop for quantile ", quant_value, ' in:'))
  print(Sys.time()-start_time_quant)
  
}

saveRDS(list_es_by_ratio_subv_propre, paste0(save_path, 'cs_estimates_by_quant_nt_entry_cohort_pub_cit_tile_field.rds'))


list_es_by_ratio_subv_propre <- readRDS(paste0(save_path, 'cs_estimates_by_quant_nt_entry_cohort_pub_cit_tile_field.rds'))


### Alternative specification :
list_etwfe <- list()
match_variables <- c('entry_cohort','field','cnrs_2007','pub_n_tile','cit_n_tile'
                     )
for(treat in c('acces_rce','date_first_idex','interact_rce_idex'
)){
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
  d_sep <- df_reg[treatment %in% c("control", treat) #& author_id %in% keep
      ] %>%
    .[, ":="(entry_cohort = as.factor(entry_cohort),
             domain = as.factor(domain),
             pub_n_tile = as.factor(pub_n_tile),
             cit_n_tile = as.factor(cit_n_tile),
             inst_set_2007 = as.factor(inst_set_2007)
             
    )] %>% #.[entry_year %in% 1985:2003] %>%
    .[, ..cols_to_keep] %>%
    .[, treat_binary := as.numeric(treatment != 'control')]
  
  
  d_sep_match <- match.data(matchit(as.formula(paste0('treat_binary ~ ',
                                           paste0(match_variables, collapse = ' + ')))
                                    ,data = d_sep
                                    ,method = "exact")) 
  
  print(d_sep_match %>%
          .[, .(N =n_distinct(author_id)), by = "treatment" ])
  
  
  etwfe_est <- etwfe(
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
  print(Sys.time()- start_time)
  list_etwfe[[treat]][[outcome]][['regression']] <- etwfe_est
  start_time <- Sys.time()
  
  event_study <- emfx(etwfe_est, type = "event")
  print(plot(event_study))
  list_etwfe[[treat]][[outcome]][['plot']] <- event_study
  print(Sys.time()- start_time)
  }
}
uniqueN(df_reg[author_id %in% keep]$inst_set_2007)   # at 5%
uniqueN(df_reg$inst_set_2007)                        # at 100%
table(d_sep$entry_cohort, d_sep$entry_year)

emfx(list_est$acces_rce, type = "event", vcov =FALSE)
emfx(list_est[[treat]], type = "calendar")   # ATT by calendar year
emfx(list_est[[treat]], type = "group")      # ATT by adoption cohort


lapply(list_est, emfx, type = "event")     # event study
plot(emfx(list_est$acces_rce, type = "event", compress = TRUE))
plot(emfx(list_est$date_first_idex, type = "event", compress = TRUE))
plot(emfx(list_est$interact_rce_idex, type = "event", compress = TRUE))



# Never-treated only ------------------------------------------------------



list_est_nt <- list()
for(outcome in c(outcomes_to_keep)){
  
  cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                     "treatment", controls, field_dummies)
  
  
  print(paste0('Estimating for outcome: ', outcome))
  start_time_est_outcome <- Sys.time()
  list_est_nt[[outcome]] <- list()
  
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "acces_rce",
                         data = df_reg 
                         ,xformla = as.formula(paste0('~',
                                                      paste0(c(controls, field_dummies), collapse = '+')))
                         ,control_group = 'nevertreated',clustervars = 'inst_set_2007'
  )
  
  list_est_nt[[outcome]]$regression <- es_stag
  
  print(paste0("Finished the estimation for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  x_lim <- c(min(df_reg$year)-min(es_stag$group),  max(df_reg$year)-max(es_stag$group))
  
  start_time_plot <- Sys.time()
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                        min_e = x_lim[1], max_e = x_lim[2])
  list_est_nt[[outcome]]$aggte_dyn <- es_aggte_dyn
  plot <- ggdid(es_aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
    geom_vline(xintercept = -0.5, colour = 'firebrick')+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
  print(plot_print)
  list_est_nt[[outcome]]$plot <- plot_print
  
  print(paste0("Finished the plot for ", outcome, ' in:'))
  print(Sys.time()-start_time_plot)
  
  saveRDS(list_est_nt[[outcome]],
          file.path(save_path, "estimates", paste0("acces_rce_all_never_treated", "__", outcome, ".rds")),
          compress = FALSE)
  
  print(paste0("Finished for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  gc()
}

#  Alternative years for ITT ------------------------------------------------------
outcomes_to_keep_alt_y <- c('publications_wins','citations_wins','nr_source_top_5pct_wins')
save_path_alt_y <- file.path(save_path, 'alt_y')
if (!file.exists(save_path_alt_y)){
  dir.create(save_path_alt_y, recursive = TRUE)
}

for(alt_y in 2006:2009){

list_est_alt_y[[as.character(alt_y)]] <- list()
df_reg_alt_y <- sample_df_reg %>%
  .[,':='(acces_rce       = as.numeric(get(paste0("ITT_acces_rce_", alt_y))),
          date_first_idex =  as.numeric(get(paste0("ITT_date_first_idex_", alt_y))),
          retired = as.numeric(year >last_year),
          pub_n_tile = ifelse(is.na(pub_n_tile), '0', pub_n_tile),
          cit_n_tile = ifelse(is.na(cit_n_tile), '0', cit_n_tile),
          has_pub = as.numeric(publications_raw >0)
  )] %>%
  .[, inst_set_2007 := inst_id_set[year == alt_y][1], by = 'author_id'] %>%
  .[, city_set_2007 := city_set[year == alt_y][1], by = 'author_id'] %>%
  .[, DEP_set_2007 := DEP_set[year == alt_y][1], by = 'author_id'] %>%
  .[, REG_set_2007 := REG_set[year == alt_y][1], by = 'author_id'] %>%
  .[, cnrs_2007 := as.integer(min_cnrs > 0 & min_cnrs <= alt_y)] %>%
  .[, ":="(acces_rce = ifelse(is.na(acces_rce), 0, as.integer(acces_rce)),
           date_first_idex = ifelse(is.na(date_first_idex), 0, as.integer(date_first_idex)),
           fusion_date = ifelse(is.na(fusion_date), 0, as.integer(fusion_date)),
           interact_rce_idex = ifelse(is.na(interact_rce_idex), 0, as.integer(interact_rce_idex)),
           idn = as.integer(factor(idn)),
           year_n = as.integer(year_n))  ] %>%
  .[!(acces_rce %in% 2013:2015)
    & (ever_in_idex_annulee ==0)
  ] %>%
  .[date_first_idex == 0 | acces_rce != 0]


field_dummies <- c()
all_fields <- sort((df_reg %>% .[, list(field)] %>% distinct() %>% separate_rows(field, sep= ',') %>% distinct())$field)
for(field in all_fields){
  print(field)
  field_var <- paste0('f_', field)
  df_reg_alt_y[[field_var]] <- as.numeric(str_detect(df_reg_alt_y$field, field))
  field_dummies <- c(field_dummies, field_var)
}


for(outcome in c(outcomes_to_keep_alt_y)){
  
  cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                     "treatment", controls, field_dummies)
  
  
  print(paste0('Estimating for outcome: ', outcome))
  start_time_est_outcome <- Sys.time()
  list_est_alt_y[[as.character(alt_y)]][[outcome]] <- list()
  
  if(alt_y<=2007){
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "acces_rce",
                         data = df_reg_alt_y 
                         ,xformla = as.formula(paste0('~',
                                                      paste0(c(controls, field_dummies), collapse = '+')))
                         ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
  )
  }
  else{
    es_stag <- did::att_gt(yname = outcome,
                           tname = 'year_n',
                           idname = 'idn',
                           gname = "acces_rce",
                           data = df_reg_alt_y,
                           anticipation = alt_y - 2007
                           ,xformla = as.formula(paste0('~',
                                                        paste0(c(controls, field_dummies), collapse = '+')))
                           ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
    )
    
  }
  list_est_alt_y[[as.character(alt_y)]][[outcome]]$regression <- es_stag
  
  print(paste0("Finished the estimation for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  x_lim <- c(min(df_reg_alt_y$year)-min(es_stag$group),  max(df_reg_alt_y$year)-max(es_stag$group))
  
  start_time_plot <- Sys.time()
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                        min_e = x_lim[1], max_e = x_lim[2])
  list_est_alt_y[[as.character(alt_y)]][[outcome]]$aggte_dyn <- es_aggte_dyn
  plot <- ggdid(es_aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
    geom_vline(xintercept = -0.5, colour = 'firebrick')+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
  print(plot_print)
  list_est_alt_y[[as.character(alt_y)]][[outcome]]$plot <- plot_print
  
  print(paste0("Finished the plot for ", outcome, ' in:'))
  print(Sys.time()-start_time_plot)
  
  saveRDS(list_est_alt_y[[as.character(alt_y)]][[outcome]],
          file.path(save_path_alt_y, paste0("acces_rce_alt_y", as.character(alt_y), "__", outcome, ".rds")),
          compress = FALSE)
  
  print(paste0("Finished for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  gc()
}
}


rows_itt <- list()
crit <- qnorm(0.975)   # 1.96 for a 95% CI; change to qnorm(0.95) for 90%

get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}
for(alt_y in 2006:2009){
  print(alt_y)
for(outcome in outcomes_to_keep_alt_y){
  print(outcome)
  res  <- readRDS(file.path(save_path_alt_y, paste0("acces_rce_alt_y", as.character(alt_y), "__", outcome, ".rds")))
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
  
  rows_itt[[as.character(alt_y)]][[outcome]] <- data.frame(
    treat        = alt_y,
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

res_itt <- rbindlist(
  lapply(names(rows_itt), function(tr) {
    rbindlist(rows_itt[[tr]], idcol = "outcome")[, treat := tr]
  })
)
p <-ggplot(res_itt)+
  geom_point(aes(x= treat, y = ATT))+
  geom_errorbar(aes(x=treat, ymin=CI_low, ymax = CI_high))+
  facet_wrap(~Outcome, scales = "free_y")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()+theme(legend.position = 'none') + xlab('Year of reference for affiliation')+ylab("ATT") + labs(title='')
p
ggsave(p, filename = file.path(save_path_alt_y, 'est_by_itt_year.png'), width = 12, height  = 6)


df_for_itt_stats <- df_reg %>% 
  .[, inst_set_2009 := inst_id_set[year == 2009][1], by = 'author_id'] %>%
  .[, remain := as.numeric(inst_set_2009 == inst_set_2007) ] %>%
  .[, same_y_treat := as.numeric(acces_rce == ITT_acces_rce_2009 )] %>%
  .[, same_y_treat := ifelse(is.na(same_y_treat), 0, same_y_treat)] %>%
  .[, ':='(treated = max(as.numeric(acces_rce !=0)),
           remain = max(remain),same_y_treat =max(same_y_treat),
           acces_rce_2007 = max(ifelse(in_acces_rce ==1 & year ==2007, 1, NA), na.rm= T),
           acces_rce_2009 = max(ifelse(in_acces_rce ==1 & year ==2009, 1, NA), na.rm= T),
           acces_rce_2020 = max(ifelse(in_acces_rce ==1 & year >=2020, 1, NA), na.rm= T),
           last_year = first(last_year)), by = 'idn'
           ] %>%
  .[, ':='(acces_rce_2007 = ifelse(acces_rce_2007==-Inf | acces_rce_2007 == Inf | is.na(acces_rce_2007), 0, acces_rce_2007),
           acces_rce_2009 = ifelse(acces_rce_2009==-Inf | acces_rce_2009 == Inf | is.na(acces_rce_2009), 0, acces_rce_2009),
           acces_rce_2020 = ifelse(acces_rce_2020==-Inf | acces_rce_2020 == Inf | is.na(acces_rce_2020), 0, acces_rce_2020))] %>%
  .[, remained_in_acces_rce := as.numeric(acces_rce_2007 == 1 & acces_rce_2009 ==1 & remain ==0) ]
print('Nr of people who remain in their affiliation in 2007-2009: ')
print( mean((df_for_itt_stats %>% .[treated == 1])$remain))

print('Nr of people who remain in treated institutions in 2007-2009: ')
print( mean((df_for_itt_stats %>% .[treated == 1])$remained_in_acces_rce))

print('Nr of people who remain in treated institutions with same treatment date in 2007-2009: ')
print( mean((df_for_itt_stats %>% .[treated == 1 & remained_in_acces_rce ==1])$same_y_treat))


print('Nr of control who moved in treated institutions in 2007-2009: ')
print( mean((df_for_itt_stats %>% .[treated == 0])$acces_rce_2009))


print('Nr of treated who remain in treated institutions in 2020-2025: ')
print( mean((df_for_itt_stats %>% .[treated == 1 & last_year >=2020])$acces_rce_2020))



# Idex alone --------------------------------------------------------------



df_reg_idex <- sample_df_reg %>%
  .[,':='(acces_rce       = as.integer(ITT_acces_rce_2007),
          date_first_idex = as.numeric(ITT_date_first_idex_2007),
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
           year_n = as.integer(year_n),
           treatment = case_when(interact_rce_idex!=0 ~"interact_rce_idex",
                                 acces_rce!=0 ~"acces_rce",
                                 date_first_idex!=0 ~"date_first_idex",
                                 .default = 'control')
  )
  ] %>%
  .[!(acces_rce %in% 2013:2015)
    & (ever_in_idex_annulee ==0)
  ] %>%
  .[acces_rce == 0]

table(df_reg_idex$date_first_idex)
table(df_reg_idex$acces_rce)

outcomes_to_keep <- c('publications_wins', 'citations_wins','total_new_phrase_comb_reuse_wins','nr_source_top_5pct_wins', 'nr_source_top_10pct_wins',
                      'new_phrase_comb_reuse','publications_raw','citations_raw', 
                      'nr_source_top_5pct_raw', 'nr_source_top_10pct_raw', 'total_new_phrase_comb_reuse')


field_dummies <- c()
all_fields <- sort((df_reg %>% .[, list(field)] %>% distinct() %>% separate_rows(field, sep= ',') %>% distinct())$field)
for(field in all_fields){
  print(field)
  field_var <- paste0('f_', field)
  df_reg_idex[[field_var]] <- as.numeric(str_detect(df_reg_idex$field, field))
  field_dummies <- c(field_dummies, field_var)
}

list_est_idex <- list()
for(outcome in c(outcomes_to_keep)){
  
  cols_to_keep <- c( outcome, "idn", "year_n", "inst_set_2007", 
                     "treatment", controls, field_dummies)
  
  
  print(paste0('Estimating for outcome: ', outcome))
  start_time_est_outcome <- Sys.time()
  list_est_idex[[outcome]] <- list()
  
  es_stag <- did::att_gt(yname = outcome,
                         tname = 'year_n',
                         idname = 'idn',
                         gname = "date_first_idex",
                         data = df_reg_idex 
                         ,xformla = as.formula(paste0('~',
                                                      paste0(c(controls, field_dummies), collapse = '+')))
                         ,control_group = 'notyettreated',clustervars = 'inst_set_2007'
  )
  
  list_est_idex[[outcome]]$regression <- es_stag
  
  print(paste0("Finished the estimation for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  x_lim <- c(min(df_reg_idex$year)-min(es_stag$group),  max(df_reg_idex$year)-max(es_stag$group))
  
  start_time_plot <- Sys.time()
  es_aggte_dyn <- aggte(es_stag, type = 'dynamic', na.rm = TRUE, 
                        min_e = x_lim[1], max_e = x_lim[2])
  list_est_idex[[outcome]]$aggte_dyn <- es_aggte_dyn
  plot <- ggdid(es_aggte_dyn)
  plot_print <- plot + scale_colour_manual(values = c("black",'black'))+ 
    geom_vline(xintercept = -0.5, colour = 'firebrick')+
    theme_bw()+theme(legend.position = 'none') + xlab('Time to treatment')+ylab(dict_vars[[outcome]]) + labs(title='')
  print(plot_print)
  list_est_idex[[outcome]]$plot <- plot_print
  
  print(paste0("Finished the plot for ", outcome, ' in:'))
  print(Sys.time()-start_time_plot)
  
  print(paste0("Finished for ", outcome, ' in:'))
  print(Sys.time()-start_time_est_outcome)
  
  gc()
}


for(outcome in outcomes_to_keep){
  print(paste0('Saving for: ', outcome))
  saveRDS(list_est_idex[[outcome]],
          file.path(save_path, "estimates", paste0("idex", "__", outcome, ".rds")),
          compress = FALSE)
  

}
