sample_df_reg <- sample_df_reg %>%
  .[, any_treatment := as.numeric(acces_rce != 0 | date_first_idex !=0 | fusion_date != 0)] %>%
  .[!is.na(any_treatment)]%>%
  .[, inst_id := paste0(merged_inst_id, '_', domain)]

fe_min <- '|  year + author_id + author_id^merged_inst_id '
fe_large = paste0(  fe_min
                    #,'+ type^year '
                    #,'+ gender^year'
                    #,'+ public^year'
                    #,'+ ecole^year'
                    ,'+ cnrs^year'
                    ,'+ domain^year'
                    ,'+ entry_year^year'
                    ,'+ prod_au_n_tile^year'
                    ,'+ prod_inst_n_tile^year'
                    #,'+ size_n_tile^year'
                    ,'+ city^year'
                    ,'+ n_inst_y^year'
                    
)
gc()
formula_no_ctrl <- as.formula(paste0( 'any_treatment ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_min))
formula_ctrl <- as.formula(paste0( 'any_treatment ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_large))
list_es = list()

agg_stag <- data.table(treat = '', est = 0, std = 0, t= 0, pvalue = 0, pvalue_pretrend= 0, type = '', comparison_group = '',  var = '', ctrl = '') %>% .[treat != '']
agg_stag_by_t <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0,  t= 0, n = '', comparison_group = '', var = '',  ctrl = '') %>% .[treatment != '']
agg_stag_by_g <- data.table(treatment = '', est = 0, std = 0, t_value = 0, p_value = 0, g = 0, n = '', comparison_group = '', var = '',  ctrl = '') %>% .[treatment != '']

gc()

sample_df_reg$y <- sample_df_reg[["citations_reweight"]]

sample_df_reg <- sample_df_reg %>%
  .[, ':='(y_lt = sum(y)), by = c('merged_inst_id',"field", 'year')] %>%
  .[, y_minus_i_lt := (y_lt - y)/(1-n_lt)]

start_time <- Sys.time()
test_roy_model_no_ctrl <- feglm(formula_no_ctrl, data = sample_df_reg, family = 'binomial',
                        cluster = c('merged_inst_id','author_id')
                        )
time_taken <- Sys.time() -start_time
print(time_taken)
iplot(test_roy_model)

sample_df_reg %>%
  .[, max_any_treatment := max(any_treatment), by = 'author_id']
nrow(sample_df_reg[any_treatment != max_any_treatment])

test <- glm(as.formula(paste0( 'any_treatment ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+')))
  , family= 'binomial', data = sample_df_reg %>% .[!is.na(y_minus_i_lt) & y_minus_i_lt <Inf ])
summary(test)


data_test <- sample_df_reg %>% .[type %in% c('government','education','facility')]
table(data_test$acces_rce)
gc()

fe_min <- '|  year '
fe_large = paste0(  fe_min
                    #,'+ type^year '
                    #,'+ gender^year'
                    #,'+ public^year'
                    #,'+ ecole^year'
                    #,'+ cnrs^year'
                    ,'+ domain^year'
                    ,'+ entry_year^year'
                    ,'+ prod_au_n_tile'
                    ,'+ prod_inst_n_tile'
                    ,'+ size_n_tile'
                    ,'+ REG'
                    #,'+ n_inst_y^year'
                    
)
gc()
formula_no_ctrl <- as.formula(paste0( 'any_treatment ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_min))
formula_ctrl <- as.formula(paste0( 'any_treatment ~ y_minus_i_lt + ',  paste0(formula_elements, collapse= '+'), fe_large))


test_roy <- feglm(formula_ctrl, data = data_test, family = 'binomial',
                  cluster = c('inst_id','author_id')
)
iplot(test_roy)  
gc()



test <- sample_df_reg %>% .[, first_date_cnrs := min( ifelse(cnrs == 1, year, NA), na.rm = TRUE), by = 'author_id'] %>%
  .[!is.na(first_date_cnrs)] %>%
  .[, post_cnrs_entry := as.numeric(year >= first_date_cnrs)]%>%
  .[, .(mean_cnrs = mean(cnrs)), by = c('post_cnrs_entry')]
test
