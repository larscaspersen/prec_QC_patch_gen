source('07_notes_eike.R')

########
#test evaluation on spei
########
setwd('C:/Users/Lars C/Documents/PhD/projects/projects_2022/qc_patch_gen_paper/')
#devtools::install_github("larscaspersen/weather_impute")

#load functions
library(weatherImpute)
library(chillR)

#read weather data
weather <- load_temperature_scenarios('qc_results/', prefix = 'durre')

#drop also the columns which have a S or R in QC, on top of the other removed outlier
x <- weather[[1]]

weather <- purrr::map(weather, function(x){
  x$Tmin[x$QC_Tmin %in% c('R', 'S')] <- NA
  x$Tmax[x$QC_Tmax %in% c('R', 'S')] <- NA
  x$Precip[x$QC_Precip %in% c('R', 'S')] <- NA
  
  return(x)
})


#read gsod data
weather_gsod <- load_temperature_scenarios('qc_results_gsod/', prefix = 'durre')
#numbers in column names can cause trouble, add gsod before it
names(weather_gsod) <- paste0('gsod', 1:27)


#read meta-data
weather_info <- read.csv('downloaded_data/cimis_info.csv')
names
gsod_info <- read.csv('downloaded_data/gsod_target_weather_info.csv')
gsod_info$id <- paste0('gsod', 1:27)


##only do analysis for precipitation



#prepare precipitation data, so that everything below set threshold is treated as 0
prcp_threshold <- 1

library(tidyverse)

precip <- purrr::map(weather, 'Precip') %>% 
  bind_cols()

precip_gsod <- purrr::map(weather_gsod, 'Precip') %>% 
  bind_cols()


#set precipitation below threshold to zero
for(station in weather_info$id){
  precip[(precip[,station] < prcp_threshold) & !is.na(precip[,station]) ,station] <- 0
}

for(station in gsod_info$id){
  precip_gsod[(precip_gsod[,station] < prcp_threshold) & !is.na(precip_gsod[,station]) ,station] <- 0
}


#bind gsod to tmin, tmax, precip 
precip <- cbind(precip, precip_gsod)


#make sure tmin, tmax and precip have columns called Day, Month, Year, Date
precip$Day <- weather[[1]]$Day
precip$Month <- weather[[1]]$Month
precip$Year <- weather[[1]]$Year
precip$Date <- weather[[1]]$Date

#drop not needed columnbs
weather_info <- dplyr::select(weather_info, id, Name, Latitude, Longitude, Elevation)
gsod_info <- dplyr::select(gsod_info, id, Name, Latitude, Longitude, Elevation)
#combine
weather_info <- rbind(weather_info, gsod_info)

rm(gsod_info, weather_gsod, tmin_gsod, tmax_gsod, precip_gsod)


#infromation about the missingness in the datasets
precip %>%
  select(weather_info$id) %>%
  purrr::map_dbl(~ (sum(is.na(.x)) / length(.x))*100) %>%
  summary()

#get a list of periods
period_list <- lapply(weather_info$id, function(x){
  
  #get longest period of tmin, tmax and prcp combined
  longest_coverage <- na.contiguous(ts(data.frame(precip = precip[,x])))
  return(tsp(longest_coverage)[1:2])
})


#get info abput the evaluation period length, median, min, max
period_length <- lapply(period_list, function(x){
  x[2] - x[1] + 1
})

period_length <- do.call(c, period_length)
median(period_length) / 365
min(period_length) / 365
max(period_length) / 365

#get info on missingness
target <- weather_info$id

#get info on missingness of each variable
prcp_missing <- apply(precip[, target], MARGIN = 2, function(x){
  (sum(is.na(x)) / length(x)) * 100
})
median(prcp_missing)
min(prcp_missing)
max(prcp_missing)

#visualise evaluation period

#library(ggplot2)

# p0 <- naniar::vis_miss(data.frame(Tmin = tmin$cimis_2, Tmax = tmax$cimis_2,
#                                   Prcp = precip$cimis_2),warn_large_data = F,sort_miss = T)
# 
# p1 <- naniar::vis_miss(data.frame(Tmin = tmin$cimis_2, Tmax = tmax$cimis_2,
#                             Prcp = precip$cimis_2),warn_large_data = F,sort_miss = T) +
#   geom_linerange(aes(ymin = period_list[[1]][1], ymax = period_list[[1]][2]), color = "red", size = 1)
# 
# p2 <- naniar::vis_miss(data.frame(Tmin = tmin$cimis_2, Tmax = tmax$cimis_2,
#                                   Prcp = precip$cimis_2),warn_large_data = F,sort_miss = T) +
#   geom_linerange(aes(ymin = period_list[[1]][1], ymax = period_list[[1]][2]), color = "red", size = 1) +
#   coord_cartesian(ylim = c(9000, 10000))
# 
# 
# p3 <- naniar::vis_miss(data.frame(Tmin = tmin$cimis_2, Tmax = tmax$cimis_2,
#                                   Prcp = precip$cimis_2),warn_large_data = F,sort_miss = T) +
#   geom_linerange(aes(ymin = period_list[[1]][1], ymax = period_list[[1]][2]), color = "red", size = 1) +
#   coord_cartesian(ylim = c(9200, 9600))
# 
# ggsave(p0, filename = 'figures/example_eval_period_blank.jpeg')
# ggsave(p1, filename = 'figures/example_eval_period.jpeg')
# ggsave(p2, filename = 'figures/example_eval_period_zoom1.jpeg')
# ggsave(p3, filename = 'figures/example_eval_period_zoom2.jpeg')

#check that for every evalaution period, there is no na 
test <- lapply(1:nrow(weather_info), function(x){
  sum(is.na(precip[period_list[[x]][1]:period_list[[x]][2], weather_info$id[x]]))
} )
sum(unlist(test))
#--> no evaluation period contains na for any variable

#set which patch methods to use
patch_methods <- c("patch_mean",  'patch_normal_ratio',
                   'patch_mice', 'patch_amelia', 'patch_climatol',
                   'patch_pca', 'patch_pca', 'patch_idw',
                   'wrapper_nm1', 'wrapper_nm2', 'wrapper_nm3')

method_patches_everything = c(F, F, T, T, T, T, T, T, F, F, F)

additional_args_rain <- list(list(n_donors = 5),
                             list(n_donors = 5),
                             list(rain_data = T, prcp_threshold = prcp_threshold, n.impute = 5, max.iter = 5, parallel = T),
                             list(rain_data = T, prcp_threshold = prcp_threshold, n.impute = 5, parallel = 'snow'),
                             NULL,
                             list(method = 'nipals', nPcs = 3),
                             list(method = 'ppca', nPcs = 3),
                             list(phi = 2),
                             NULL,
                             NULL,
                             NULL)

# patch_methods <- c('wrapper_nm1', 'wrapper_nm2', 'wrapper_nm3')
# 
# 
# #indicate if function either patches the whole data frame or if it patches one column after another
# method_patches_everything = c(F, F, F)
# 
# 
# #define additional arguments needed for the function call, needs to be of same length as patch methods
# 
# additional_args_rain <- list(NULL,
#                              NULL,
#                              NULL)

#at first determine the longest period per station in which tmin, tmax and prcp have data together
weather_list <- list(list(precip, additional_args_rain))
weather_vars <- c('prcp')

#missingness to evaluate
p_missing <- 0.4

#carryout patching evaluation, return the whole evaluation period, as the spei index is calculated for it


eval_data <-  weatherImpute::get_eval_one_station(weather = precip, 
                                      weather_info = weather_info,
                                      target = weather_info$id, 
                                      patch_methods = patch_methods,
                                      p_missing = p_missing, 
                                      additional_args = additional_args, 
                                      method_patches_everything = method_patches_everything,
                                      period = period_list, 
                                      return_data = 'evaluation_period', 
                                      mute = FALSE)

#save workspace
#save.image(file = "patch_california_workspace.RData")


#stopped here, restart from this point tomorrow
load('patch_california_workspace.RData')


#calculation done, do plotting

# for spei calculation i need to bring the data in differetn format:
# currently data is organised as: one df per variable, one column for each patch_method
# goal: have a list, for each station one df, each df contain tmin, tmax, prec of one station, long format indicates original, patch method etc


write.csv(eval_data, file = 'data/CA-patch_eval_prec_only.csv', row.names = F)
