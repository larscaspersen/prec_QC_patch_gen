#read CIMIS data, run quality control on it
library(chillR)

weather_info <- read.csv('downloaded_data/cimis_info.csv')
weather <- load_temperature_scenarios(path = 'downloaded_data/cimis/', 
                                      prefix = 'target-station')

#make sure weather data is same organized as weather info
weather <- weather[weather_info$id]

library(weatherQC)
library(tidyverse)

#without spatial regression
# wc_costa_out <- weather_qc_costa(weather_list = weather, weather_info = weather_info,
#                                  aux_list = weather, aux_info = weather_info,
#                  region = 'USA', subregion = 'California')
# 
# chillR::save_temperature_scenarios(wc_costa_out, path = 'qc_results/', prefix = 'costa')
# 
# wc_durre_out <- weather_qc_durre(weather_list = weather, weather_info = weather_info, 
#                                  region = 'USA', subregion = 'California')
# chillR::save_temperature_scenarios(wc_durre_out, path = 'qc_results/', prefix = 'durre')
# 


wc_costa_out <- load_temperature_scenarios(path = 'qc_results/', prefix = 'costa')
wc_durre_out <- load_temperature_scenarios(path = 'qc_results/', prefix = 'durre')

#names(wc_costa_out) <- weather_info$id



#get a table how many were removed in total

#get a table how many removed for each test
#(this is a bit different for costa, because only if at least two tests mark the observation
#gets removed)

#find out how many removed the same

#find out how many align with the quality flag 
#S = errouneous reading, problem in Tmin, Tmax
#R = far outside historical range (>99.8%)
#Y = modertately out of historical range (>96%)

#focus on R and S


#get number of marked outlier
Tmin_costa <- purrr::map_dbl(wc_costa_out, function(x){
  tmin_t1 <- grepl(pattern = '1', x = x$flag_Tmin)
  tmin_t2 <- grepl(pattern = '2', x = x$flag_Tmin)
  tmin_t3 <- grepl(pattern = '3', x = x$flag_Tmin)
  tmin_t4 <- grepl(pattern = '4', x = x$flag_Tmin)
  tmin_t5 <- grepl(pattern = '5', x = x$flag_Tmin)
  
  sum(rowSums(cbind(tmin_t1, tmin_t2, tmin_t3, tmin_t4, tmin_t5)) >= 2)
})

Tmin_durre <- purrr::map_dbl(wc_durre_out, function(x){
  sum(is.na(x$flag_Tmin) == FALSE)
})


Tmax_costa <- purrr::map_dbl(wc_costa_out, function(x){
  tmin_t1 <- grepl(pattern = '1', x = x$flag_Tmax)
  tmin_t2 <- grepl(pattern = '2', x = x$flag_Tmax)
  tmin_t3 <- grepl(pattern = '3', x = x$flag_Tmax)
  tmin_t4 <- grepl(pattern = '4', x = x$flag_Tmax)
  tmin_t5 <- grepl(pattern = '5', x = x$flag_Tmax)
  
  sum(rowSums(cbind(tmin_t1, tmin_t2, tmin_t3, tmin_t4, tmin_t5)) >= 2)
})

Tmax_durre <- purrr::map_dbl(wc_durre_out, function(x){
  sum(is.na(x$flag_Tmax) == FALSE)
})


Precip_costa <- purrr::map_dbl(wc_costa_out, function(x){
  tmin_t1 <- grepl(pattern = '1', x = x$flag_Precip)
  tmin_t2 <- grepl(pattern = '2', x = x$flag_Precip)
  tmin_t3 <- grepl(pattern = '3', x = x$flag_Precip)
  tmin_t4 <- grepl(pattern = '4', x = x$flag_Precip)
  tmin_t5 <- grepl(pattern = '5', x = x$flag_Precip)
  
  sum(rowSums(cbind(tmin_t1, tmin_t2, tmin_t3, tmin_t4, tmin_t5)) >= 2)
})

Precip_durre <- purrr::map_dbl(wc_durre_out, function(x){
  sum(is.na(x$flag_Precip) == FALSE)
})

total_outlier_df <- data.frame(id = rep(weather_info$id,3),
                               variable = rep(c('Tmin', 'Tmax', 'Precip'), each = 33),
                               costa = c(Tmin_costa, Tmax_costa, Precip_costa),
                               durre = c(Tmin_durre, Tmax_durre, Precip_durre))

#in Tmin and Tmax costa removed more data than durre
#but in Precip there are few cases of extreme removal in precipitation

total_outlier_df <- reshape2::melt(total_outlier_df, id.vars = c('id', 'variable'), 
                                   variable.name = 'QC')

outlier_summarized <- total_outlier_df %>% 
  group_by(id, variable, QC) %>% 
  summarise(perc_flagged = (sum(value) / 11688) * 100) %>% 
  ungroup() %>% 
  group_by(variable, QC) %>% 
  summarise(min = min(perc_flagged),
            mean = mean(perc_flagged),
            median = median(perc_flagged),
            max = max(perc_flagged))
#in most cases durre was less invasive than costa



#summary on which test led to removal



test_df <- data.frame(test = c('naught_check', 'duplicated', 'record_exceedance', 'streaks', 
                'frequent_ident_value', 'gap_check', 'clim_outlier',
                'iterative_consistency', 'spike-dip', 'lagged_temperature', 
                'spatial_regression', 'spatial_corroboration', 'mega_consistency'))


table_test <- purrr::map(wc_durre_out, function(x){
  tmin <- table(x$flag_Tmin)
  tmax <- table(x$flag_Tmax)
  precip <- table(x$flag_Precip)
  
  data.frame(test = c(names(tmin), names(tmax), names(precip)),
             n = c(tmin, tmax, precip),
             variable = c(rep('Tmin', length(tmin)),
                          rep('Tmax', length(tmax)),
                          rep('Precip', length(precip))))
  
  
  
}) %>% 
  bind_rows(.id = 'id')

#remove row names
rownames(table_test) <- 1:nrow(table_test)

#get info on total number 
table_test %>% 
  group_by(test, variable) %>% 
  summarise(n = sum(n))


table_test_costa <- purrr::map(wc_costa_out, function(x){
  tmin <- table(x$flag_Tmin)
  tmax <- table(x$flag_Tmax)
  precip <- table(x$flag_Precip)
  
  data.frame(test = c(names(tmin), names(tmax), names(precip)),
             n = c(tmin, tmax, precip),
             variable = c(rep('Tmin', length(tmin)),
                          rep('Tmax', length(tmax)),
                          rep('Precip', length(precip))))
}) %>% 
  bind_rows(.id = 'id') %>% 
  filter(!test %in% c('', '1', '2', '3', '4', '5'))


#think about routines to handle this type of information




##
Tmin_costa <- purrr::map_dbl(wc_costa_out, function(x){
  tmin_t1 <- grepl(pattern = '1', x = x$flag_Tmin)
  tmin_t2 <- grepl(pattern = '2', x = x$flag_Tmin)
  tmin_t3 <- grepl(pattern = '3', x = x$flag_Tmin)
  tmin_t4 <- grepl(pattern = '4', x = x$flag_Tmin)
  tmin_t5 <- grepl(pattern = '5', x = x$flag_Tmin)
  
  outlier <- rowSums(cbind(tmin_t1, tmin_t2, tmin_t3, tmin_t4, tmin_t5)) >= 2
  
  sum(outlier & x$QC_Tmin %in% c("R", "S"))
})

Tmin_durre <- purrr::map_dbl(wc_durre_out, function(x){
  sum(is.na(x$flag_Tmin) == FALSE & x$QC_Tmin %in% c("R", "S"))
})


Tmax_costa <- purrr::map_dbl(wc_costa_out, function(x){
  tmin_t1 <- grepl(pattern = '1', x = x$flag_Tmax)
  tmin_t2 <- grepl(pattern = '2', x = x$flag_Tmax)
  tmin_t3 <- grepl(pattern = '3', x = x$flag_Tmax)
  tmin_t4 <- grepl(pattern = '4', x = x$flag_Tmax)
  tmin_t5 <- grepl(pattern = '5', x = x$flag_Tmax)
  
  outlier <- rowSums(cbind(tmin_t1, tmin_t2, tmin_t3, tmin_t4, tmin_t5)) >= 2
  
  sum(outlier & x$QC_Tmax %in% c("R", "S"))
})

Tmax_durre <- purrr::map_dbl(wc_durre_out, function(x){
  sum(is.na(x$flag_Tmax) == FALSE & x$QC_Tmax %in% c("R", "S"))
})


Precip_costa <- purrr::map_dbl(wc_costa_out, function(x){
  tmin_t1 <- grepl(pattern = '1', x = x$flag_Precip)
  tmin_t2 <- grepl(pattern = '2', x = x$flag_Precip)
  tmin_t3 <- grepl(pattern = '3', x = x$flag_Precip)
  tmin_t4 <- grepl(pattern = '4', x = x$flag_Precip)
  tmin_t5 <- grepl(pattern = '5', x = x$flag_Precip)
  
  outlier <- rowSums(cbind(tmin_t1, tmin_t2, tmin_t3, tmin_t4, tmin_t5)) >= 2
  
  sum(outlier & x$QC_Precip %in% c("R", "S"))
})

Precip_durre <- purrr::map_dbl(wc_durre_out, function(x){
  sum(is.na(x$flag_Precip) == FALSE & x$QC_Precip %in% c("R", "S"))
})

true_total_outlier_df <- data.frame(id = rep(weather_info$id,3),
                               variable = rep(c('Tmin', 'Tmax', 'Precip'), each = 33),
                               costa = c(Tmin_costa, Tmax_costa, Precip_costa),
                               durre = c(Tmin_durre, Tmax_durre, Precip_durre))

true_total_outlier_df <- reshape2::melt(true_total_outlier_df,id.vars = c('id', 'variable'), variable.name = 'QC')

#calculate share of specificity
merge(total_outlier_df, true_total_outlier_df, by = c('id', 'variable', 'QC')) %>% 
  mutate(specificity = (value.y / value.x) * 100) %>% 
  group_by(variable, QC) %>% 
  summarise(min = min(specificity, na.rm = T),
            mean = mean(specificity, na.rm = T),
            median = median(specificity, na.rm = T),
            max = max(specificity, na.rm = T))



#gwt info on recall
#--> find number of total labels

table_total_label <- purrr::map(wc_durre_out, function(x){
  data.frame(n = c(sum(x$QC_Tmin %in% c('R', 'S') & is.na(x$Tmin_org) == FALSE),
                   sum(x$QC_Tmax %in% c('R', 'S') & is.na(x$Tmax_org) == FALSE),
                   sum(x$QC_Precip %in% c('R', 'S') & is.na(x$Precip_org) == FALSE)),
             variable = c('Tmin', 'Tmax', 'Precip'))
}) %>% 
  bind_rows(.id = 'id')

merge(table_total_label, true_total_outlier_df, by = c('id', 'variable')) %>% 
  mutate(recall = (value / n) * 100) %>% 
  group_by(variable, QC) %>% 
  summarise(min = min(recall, na.rm = T),
            mean = mean(recall, na.rm = T),
            median = median(recall, na.rm = T),
            max = max(recall, na.rm = T))

#one method is better in identifying true outlier, at the cost of having a larger share of 
#FALSE positives
#the other is better at avoiding false positives but identifies less true positives

#--> confusion matrix

confusion_matirx <- merge(table_total_label, true_total_outlier_df, by = c('id', 'variable')) %>% 
  merge.data.frame(total_outlier_df, by = c('id', 'variable', 'QC')) %>% 
  mutate(FN = (n - value.x),
         TP = value.x) %>% 
  mutate(FP = value.y - TP,
         TN = 11688 - FN)

#


