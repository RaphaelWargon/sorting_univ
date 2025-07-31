sample_df <- sample_df %>%
  .[, inst_id_set:=paste0(unique(list(inst_id)), collapse = ","), by = c('author_id','year') ]%>%
  .[, ":="(in_private = max(prive, na.rm=T),
           in_public = max(public, na.rm =T),
           in_uni_pub = max(uni_pub, na.rm = T),
           in_cnrs = max(cnrs, na.rm = T)
           ), by = c('author_id','year') ]%>%
  .[, lag_inst_id_set := lag(inst_id_set, order_by = year), by = author_id] %>%
  .[, ":="(in_private_t1= lag(prive, order_by = year),
           in_public_t1= lag(in_public, order_by = year),
           in_uni_pub_t1= lag(in_uni_pub, order_by = year),
           in_cnrs_t1= lag(in_cnrs, order_by = year)
           ), by = author_id] %>%
  .[, entrant := ifelse(!str_detect(lag_inst_id_set, inst_id) | is.na(lag_inst_id_set), 1,0)]

mean( (sample_df %>%
  .[year <=2006] %>% .[, lambda := (1-in_private_t1)*in_private])$lambda, na.rm =T) 
mean( (sample_df %>%
         .[year <=2006] %>% .[, lambda := in_private_t1*(1-in_private)])$lambda, na.rm =T) 

mean( (sample_df %>%
         .[year <=2006] %>% .[, lambda := (1-in_public_t1)*in_public])$lambda, na.rm =T) 
mean( (sample_df %>%
         .[year <=2006] %>% .[, lambda := in_public_t1*(1-in_public)])$lambda, na.rm =T) 

mean( (sample_df %>%
         .[year >=2010] %>% .[, lambda := (1-in_private_t1)*in_private])$lambda, na.rm =T) 
mean( (sample_df %>%
         .[year >=2010] %>% .[, lambda := in_private_t1*(1-in_private)])$lambda, na.rm =T) 

mean( (sample_df %>%
         .[year >=2010] %>% .[, lambda := (1-in_public_t1)*in_public])$lambda, na.rm =T) 
mean( (sample_df %>%
         .[year >=2010] %>% .[, lambda := in_public_t1*(1-in_public)])$lambda, na.rm =T) 


mean( (sample_df %>% .[, lambda := (1-in_private_t1)*in_private])$lambda, na.rm =T) 
mean( (sample_df %>% .[, lambda := in_private_t1*(1-in_private)])$lambda, na.rm =T) 

mean( (sample_df %>% .[, lambda := (1-in_public_t1)*in_public])$lambda, na.rm =T) 
mean( (sample_df %>% .[, lambda := in_public_t1*(1-in_public)])$lambda, na.rm =T) 
mean( sample_df$entrant)
gc()


