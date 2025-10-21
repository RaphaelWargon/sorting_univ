rm(list = ls())
gc()
require('pacman')

p_load('arrow'
       ,'data.table'
       ,'fixest'
       ,'tidyverse'
       ,'binsreg',
       'DescTools',
       'cowplot','devtools','cluster',
       'factoextra','tictoc',
       'rblm',
       'fitdistrplus','actuar'
       )
#install_github("tlamadon/rblm")
#library('rblm')
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    #winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}
source(file = "C:/Users/common/projet_3_lru/sorting_univ/script/3_regressions/0_all_blm_utils.R")

source(file = "C:/Users/common/projet_3_lru/sorting_univ/script/3_regressions/0_adapting_blm_functions.R")

inputpath <- "E:\\panel_fr_res\\panel_smoothed_w_theses.parquet"

#inputpath <- "C:\\Users\\rapha\\Desktop\\panel_smoothed.parquet"
#

wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    #Winsorize(x, probs = c(0, 1-pct_level), na.rm = T)
    Winsorize(x, val = quantile(x, probs = c(0, 1-pct_level), na.rm = T))
  } else {x}
}

# Load data ---------------------------------------------------------------


ds <- open_dataset(inputpath) 
ds$schema$names

y_change = 2007

ds <- open_dataset(inputpath) %>%
  filter(#all_y_in_FR >= (last_year-entry_year +1)/4
    last_year-entry_year >2
    & entry_year >=1965 
    & year >= 1997
    
  ) %>%
  dplyr::select(author_id, author_name, year,
         entry_year,
         inst_id, name, type, main_field,
         publications_raw,citations_raw,  
         publications, citations, #country,
         avg_rank_source_raw,nr_source_btm_50pct_raw,
         nr_source_mid_40pct_raw, nr_source_top_20pct_raw,nr_source_top_10pct_raw,nr_source_top_5pct_raw,
         avg_rank_source,nr_source_btm_50pct,
         nr_source_mid_40pct, nr_source_top_20pct,nr_source_top_10pct,nr_source_top_5pct,
         period_inst, uni_pub, cnrs, fused, idex,
         n_inst_y,type_fr, universite, ecole, public, prive,
         n_phd_students, in_supervisor_inst, 
         in_referee_inst,in_jury_inst, thesis_year #, inst_set_this_year
  )
ds <- as.data.table(ds)




# Filter data -------------------------------------------------------------


ds <- ds %>%
  .[, ':='(n_inst_id_sample = n_distinct(inst_id),
           main_field = first(main_field, na_rm = TRUE),
           inst_id = ifelse(is.na(inst_id), 
                            lag(inst_id, order_by = year), inst_id)), by = 'author_id'] %>%
  .[, ':='(n_authors_w_several_inst = n_distinct(ifelse(n_inst_id_sample >1, author_id, 0)),
           n_authors_sample = n_distinct(author_id) ), by = c('inst_id','main_field')]%>%
  .[,n_by_field := n_distinct(author_id), by = 'main_field']%>%
  .[, n_y_in_sample := n_distinct(year), by = 'author_id']%>%
  .[,max_field := dplyr::first(ifelse(n_authors_sample == max(n_authors_sample* as.numeric(!is.na(main_field)) ) & !is.na(main_field), main_field, NA), na_rm = T),
    by = "inst_id"] %>%
  .[,field_value := n_authors_sample/n_distinct(author_id),by = 'inst_id']%>%
  .[,max_field_value := max(field_value), by = 'inst_id'] %>%
  .[,main_field_recoded := ifelse( ((field_value <0.1| n_authors_sample<=5) ) | is.na(main_field), max_field, main_field)] %>%
  .[, main_field := main_field_recoded] %>%
  .[, citations_raw := wins_vars(citations_raw),  by = c('year','main_field')]
gc()

complete_ds <- as.data.table(unique(ds %>%
  .[, list(author_id, entry_year)]) %>%
  dplyr::mutate(year = paste0(seq(2000, 2020), collapse =',')) %>%
  tidyr::separate_rows(year, sep=',') %>%
  dplyr::filter(year >= entry_year) %>%
  dplyr::select(-entry_year))

gc()
ds_for_classification <-merge(complete_ds,
                              ds%>%
                                .[, first_y_lab  := min(year), by = inst_id] %>%
                                .[, inst_id_field := paste0(inst_id, '_', main_field)] %>%
                                .[, list(citations_raw, citations, year,type,entry_year,main_field,
                                         fused,idex,cnrs,uni_pub,first_y_lab,ecole,universite
                                         ,public,prive, author_id, inst_id, inst_id_field, author_name, name,period_inst)] %>%
                                .[,year := as.character(year)],
                              by = c('author_id','year'), all.x = TRUE
                              ) %>%
  tidyr::fill(c(entry_year,main_field,type,
                fused,idex,cnrs,uni_pub,ecole,universite, citations
                ,public,prive, period_inst, author_id, inst_id, inst_id_field, first_y_lab, author_name, name), .direction = 'down')%>%
  mutate(citations_raw = ifelse(is.na(citations_raw), 0, citations_raw))

ds_for_classification <- as.data.table(ds_for_classification) %>%
  .[!is.na(main_field)] %>%
  .[year %in% c(y_change - 5, y_change-6) & entry_year <= y_change-6]%>%
  .[, list(citations_raw, citations, year,type,entry_year,main_field,
           fused,idex,cnrs,uni_pub,first_y_lab,ecole,universite
           ,public,prive, author_id, inst_id, inst_id_field, author_name, name)]

#ds_for_classification <- unique(ds_for_classification)
gc()

ggplot(ds_for_classification)+geom_density(aes(x= log(citations_raw+1), color = year))

p_load('igraph')
edges <- as.vector(t(ds_for_classification[, c("inst_id_field", "author_id")]))
g <- graph(edges, directed = FALSE)
components <- components(g)
firm_components <- split(names(components$membership), components$membership)
first_component <- firm_components$`1`
inst_id_first_component <- first_component[str_detect(first_component, 'I')]

nrow(ds_for_classification %>%
  .[!inst_id_field %in% inst_id_first_component  ])




ds_for_classification <- unique(ds_for_classification %>%
  .[inst_id_field %in% inst_id_first_component])%>%
  .[, ':='(weight = 1/.N), by = c('author_id','year')] 


ggplot(ds_for_classification)+
  geom_density(aes(x= sqrt(citations_raw), color = as.factor(year)))

# Residualise citations for BLM decomposition -----------------------------


residualise_cit_raw <- glm(citations_raw ~ 1 +as.factor(year)*(type+ as.factor(entry_year)+main_field +
                        fused+idex+ cnrs+uni_pub +first_y_lab + ecole + universite + public + prive )
                      , data= ds_for_classification, family = "poisson"
                      ,weights = weight
)

gc()

residualise_cit_smoothed <- glm(citations ~ 1 +as.factor(year)*(type+ as.factor(entry_year)+main_field +
                                                                 fused+idex+ cnrs+uni_pub +first_y_lab + ecole + universite + public + prive )
                           , data= ds_for_classification, family = "poisson"
                           ,weights = weight
)

gc()


ds_for_classification$residualised_cit <- residualise_cit_raw$residuals
ds_for_classification$residualised_cit_smooth <- rstandard(residualise_cit_smoothed, type = "deviance")


ggplot(ds_for_classification)+
  geom_density(aes(x= residualised_cit_smooth, color = as.factor(year)))
summary(ds_for_classification$residualised_cit_smooth)
ggplot(ds_for_classification)+
  geom_histogram(aes(x= residualised_cit_smooth, color = as.factor(year)))


ggplot(ds_for_classification)+
  geom_density(aes(x= citations-residualised_cit_smooth, color = as.factor(year)))
ggplot(ds_for_classification)+
  geom_density(aes(x= citations, color = as.factor(year)))



ggplot(ds_for_classification %>%
         .[,norm_res_cit_sm := 
             (residualised_cit_smooth-min(residualised_cit_smooth, na.rm =T))/(max(residualised_cit_smooth, na.rm =T)-min(residualised_cit_smooth, na.rm =T)), by = 'year' ])+
  geom_density(aes(x= norm_res_cit_sm, color = as.factor(year)))

gc()

fwrite(ds_for_classification, "E:\\panel_fr_res\\ds_classification_blm.csv")
# Build subsets of data for BLM analysis ----------------------------------

ds_for_classification<- fread("E:\\panel_fr_res\\ds_classification_blm.csv")

data1 <- unique(ds_for_classification %>%
  .[year == y_change - 6] %>%
  .[, ":="(y1= residualised_cit_smooth, f1 = inst_id_field, wid = author_id, weight1 = weight, name1= name)] %>%
  .[, list(f1,y1, wid,weight1, name1, author_name)]) 
data2 <- unique(ds_for_classification %>%
  .[year == y_change - 5] %>%
  .[, ":="(y2= residualised_cit_smooth, f2 = inst_id_field, wid = author_id, weight2 = weight, name2 = name)] %>%
  .[, list(f2,y2, wid,weight2, name2,author_name)])



data <- merge(data1, data2, by = c('wid','author_name'), allow.cartesian = FALSE) %>%
  .[ !is.na(f2) & !is.na(f1) & !is.na(y1) & !is.na(y2)] #%>%
  #.[, ':='(y1 = ((y1-min(y1))),
  #         y2 = ((y2-min(y2)))
  #         )]

data[, .(N = (sum(weight2))), by = 'wid']
ds_to_classify <- list()
p_load('actuar')
ggplot(data)+
  geom_density(aes(x = y1), color = "red")+
  geom_density(aes(x = y2), color = "darkgreen")

#distrib <- fitdist(data$y1, "nbinom")
#distrib <- fitdist(data$y1, "pareto", start =  list(shape = 1, scale = 500) )
#distrib2 <- fitdist(data$y1, "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 1))
#cdfcomp(list(distrib, distrib2))

ds_to_classify$sdata <- data[f1==f2]
ds_to_classify$jdata <- data[f1!=f2]


ggplot()+
  geom_density(aes(x = y1), color = "red", data = ds_to_classify$sdata)+
  geom_density(aes(x = y2), color = "darkgreen", data = ds_to_classify$sdata)

ggplot()+
  geom_density(aes(x = y1), color = "red", data = ds_to_classify$jdata)+
  geom_density(aes(x = y2), color = "darkgreen", data = ds_to_classify$jdata)

nf <- length(unique(data1$f1))



# Classify firms ----------------------------------------------------------


group_measures <- grouping.getMeasures(ds_to_classify)
group_classes <- grouping.classify_wsil(group_measures,ksupp = 1:floor(nf/100)#ceiling( (1:(nf/100)^(1/1.3))^1.3)
                                   )


output_path_kclusters = "E:\\panel_fr_res\\models_blm\\firms\\R\\firms_groupings.RData"
save(group_classes, file = output_path_kclusters)
load(output_path_kclusters)

ggplot(group_classes$summary %>%
         pivot_longer(cols = c(Q, silhouette), names_to = "Statistic", values_to =  "values") %>%
         mutate(Statistic = ifelse(Statistic == 'Q', 'WSS', 'Silhouette')))+
  geom_line(aes(x=k, y = values, color = Statistic))+
  scale_color_manual(values= c('aquamarine4', 'steelblue4'))+
  geom_vline(aes(xintercept = group_classes$best_k), linetype = 'dashed')+
  theme_bw()+labs(title = 'K-means clustering results')+ 
  xlab('Number of clusters')+ylab('Value')+theme(legend.box.margin = margin()) 
ggsave(filename = "E:\\panel_fr_res\\models_blm\\firms\\R\\firm_cluster_metrics.png"
       , width= 840*2.5, height = 640*2.5, unit = 'px')

#group_classes <- grouping.classify.once(group_measures,k = 10 #ceiling( (1:(nf/100)^(1/1.3))^1.3)
#                                        )

ds_to_classify <- grouping.append(ds_to_classify, group_classes$all[[10]] )
ds_to_classify$sdata <- ds_to_classify$sdata %>% 
  .[!is.na(j1) & !is.na(j2)]# pb with this author for some reason
ds_to_classify$jdata <- ds_to_classify$jdata %>% 
  .[!is.na(j1) & !is.na(j2)]

ds_to_classify$sdata[, .(sum(weight1)), by= j1]
ds_to_classify$sdata[, .(n_distinct(f1)), by= j1]

ggplot()+
  geom_density(aes(x= y1, color = as.factor(j1) ), data = ds_to_classify$sdata)+
  geom_density(aes(x= y1, color = as.factor(j1) ), ds_to_classify$jdata, linetype = 'dashed')

ggplot()+
  geom_density(aes(x= log(y1+min(y1)+1), color = as.factor(j1) ), data = ds_to_classify$sdata)+
  geom_density(aes(x= log(y1+min(y1)+1), color = as.factor(j1) ), ds_to_classify$jdata, linetype = 'dashed')


ggplot()+
  geom_density(aes(x= log(y1), color = as.factor(j1) ), data = ds_to_classify$sdata)

test <- unique(ds_to_classify$sdata[, list(j1, name1)])

ggplot()+
  geom_density(aes(x= y2, color = as.factor(j2) ), data = ds_to_classify$sdata[y2 >=0])+
  geom_density(aes(x= y2, color = as.factor(j2) ), ds_to_classify$jdata[y2 >=0], linetype = 'dashed')


summary(ds_to_classify$sdata)

#classes <- as.data.table(list(inst_id = names(group_classes$best_cluster),group = group_classes$best_cluster))
#
#classes[, .N, by = group]
#test <- merge(sdata[, inst_id := f1], classes[, group:=as.factor(group)], by = 'inst_id')
#ggplot(test)+
#  geom_density(aes(x= log(y1), fill = group, group = group), alpha = 0.5)
#


# Classify individuals ----------------------------------------------------


ctrl =em.control(sdata_subredraw = FALSE)


#est_interm <- debug_mixt_estimate(ds_to_classify, nk =group_classes$best_k, ctrl)

gc()
ds_to_classify$sdata <- ds_to_classify$sdata %>% .[, ':='(sample =1, x=1,
                                                          y1 = log(y1-min(y1)+1),
                                                          y2 = log(y2-min(y2)+1)
                                                          )]
ds_to_classify$jdata <- ds_to_classify$jdata %>% .[, ':='(y1 = log(y1-min(y1)+1),
                                                          y2 = log(y2-min(y2)+1)
)]

## 12485
#model <- res_mixt$model

all_estimates = list()
n_workers = length(unique(data$wid))
output_path_models = "E:\\panel_fr_res\\models_blm\\workers\\"
rewrite = TRUE
for(nk in 2:2){
  output_model_workers = paste0(output_path_models, 'BLM_', nk, '.RData')
  model_name = paste0('estimates_', nk)
  if(file.exists(output_model_workers) & rewrite == FALSE){
    print(paste0('loading ', as.character(nk)) )
    load(output_model_workers)
    all_estimates[[nk]] <- get(model_name)
    rm(list = model_name)
    gc()
    print('loaded')
  }
 else{
   print(paste0('computing ',  as.character(nk)))
  all_estimates[[nk]] <- m2.mixt.estimate.all_wdist(ds_to_classify, nk = nk, ctrl)
  assign(x = model_name, value = all_estimates[[nk]])
  save(list = model_name, file = output_model_workers)
 }
}

length(all_estimates)
all_likelihoods = c()
for(nk in 2:length(all_estimates)){
  all_likelihoods = c(all_likelihoods, all_estimates[[nk]]$liks)
}

ggplot(as.data.table(list(k = 2:length(all_estimates), likelihood = all_likelihoods)))+
  geom_line(aes(x=k, y= likelihood))

m2.mixt.rdim.pk1(all_estimates[[7]]$model$pk1)
m2.mixt.pplot(est$model$pk1) 
m2.mixt.wplot(est$model$A2) 
m2.mixt.pplot(est$model$dist) 

test <- merge(data, all_estimates[[5]]$model$dist,  by = 'wid')
test <- merge(test,
              as.data.table( list(f1 = names(group_classes$best_cluster), l1= group_classes$best_cluster)), by = 'f1')

#test <- unique(test[max_k== 'k_7'])

test1 <- merge(unique(test[,list(wid,max_k)]),
               unique(ds[, list(author_id, author_name)][, wid:=author_id][, author_id := NULL]))

unique(test[,list(max_k, wid)])[, .N, by = 'max_k']
unique(test[,list(max_k, k_1, k_2, k_3, k_4, k_5, k_6, wid)])[, lapply(.SD, mean,), .SDcols= c('k_1','k_2',"k_3",'k_4','k_5','k_6'), by = "max_k"]

to_plot <- as.data.table(test) %>%
  .[, .(k_1 = sum(k_1*weight1,na.rm =T),
        k_2 = sum(k_2*weight1,na.rm =T),
        k_3 = sum(k_3*weight1,na.rm =T),
        k_4 = sum(k_4*weight1,na.rm =T),
        k_5 = sum(k_5*weight1,na.rm =T),
        k_6 = sum(k_6*weight1,na.rm =T)
        ), by = l1] %>%
  tidyr::pivot_longer(c('k_1','k_2',"k_3",'k_4','k_5','k_6'), values_to = 'value', names_to = 'k')
to_plot <- as.data.table(to_plot) %>%
  .[, value := value/sum(value), by = 'l1'] %>%
  .[, ':='(l1 = as.factor(l1), k = str_replace(k, 'k_', ''))]
ggplot(to_plot
       )+
  geom_col(aes(x=l1, y= value, fill= k), position = 'stack')


ggplot(test[y1>=0] %>%
         dplyr::select(y1, max_k, wid) %>% distinct()) +
  geom_density(aes(x= y1, color = max_k ))

ggplot(test[y1>=0] %>%
         dplyr::select(y1, wid, l1, f1, max_k) %>% distinct() ) +
  geom_density(aes(x= y1, color = as.factor(l1) ))+
  facet_wrap(~max_k)
ggplot(test[y1>=0] %>%
         dplyr::select(y1, wid, l1, f1, max_k) %>% distinct() ) +
  geom_bar(aes(x= y1, color = as.factor(l1) ))+
  facet_wrap(~max_k)


ggplot(test[y1>=0] %>%
         dplyr::select(y1,max_k, wid, l1, f1) %>% distinct() ) +
  geom_raster(aes(x= max_k, y = as.factor(l1), fill = y1 ))+ 
  scale_fill_continuous(type = "viridis") 
ggplot(test[y1>=0] %>%
         dplyr::select(y1,max_k, wid, l1, f1) %>% distinct() ) +
  geom_bin_2d(aes(x= max_k, y = as.factor(l1)))+
  scale_fill_continuous(type = "viridis") 

ggplot(test[y1>=0] %>%
         .[, list(wid,max_k,l1)] %>%
         .[, .(count = log(n_distinct(wid))), by = c('l1','max_k')]
         ) +
  geom_raster(aes(x= max_k, y = as.factor(l1), fill = count ))+ 
  scale_fill_continuous(type = "viridis") 


  est_interacted <- m2.mini.estimate(ds_to_classify$jdata, ds_to_classify$sdata)
#m2.mini.plotw(est, qt = 14, getvals = T) 