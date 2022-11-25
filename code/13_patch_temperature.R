library(chillR)
library(tidyverse)

#read weather data
weather <- load_temperature_scenarios('qc_results/', prefix = 'durre')

#drop also the columns which have a S or R in QC, on top of the other removed outlier

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

gsod_info <- read.csv('downloaded_data/gsod_target_weather_info.csv')
gsod_info$id <- paste0('gsod', 1:27)


#prepare precipitation data, so that everything below set threshold is treated as 0
prcp_threshold <- 1


weather <- c(weather, weather_gsod)

weather_info <- rbind(weather_info[,c('id', 'Name', 'Latitude', 'Longitude', 'Elevation', 
                                      'Tmin_complete', 'Tmax_complete', 'Precip_complete')], 
                      gsod_info[,c('id', 'Name', 'Latitude', 'Longitude', 'Elevation', 
                                   'Tmin_complete', 'Tmax_complete', 'Precip_complete')])

rm(gsod_info, weather_gsod)



#set precipitation below threshold to zero
for(station in weather_info$id){
  weather[[station]]['Precip'][weather[[station]]['Precip'] < prcp_threshold] <- 0
}


#load aux weather data
aux_list <- load_temperature_scenarios('downloaded_data/aux_data/', prefix = 'aux_ws')
aux_info <- read.csv('downloaded_data/aux_wather_info.csv')


#add target weather data to aux list and aux_info

aux_list <- c(aux_list, weather)

aux_info <-  rbind(aux_info[,c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')],
weather_info[,c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')])

patched_list <- list()


#make sure that the column "id" is present for each data.frame
for(station in names(weather)){
  weather[[station]]$id <- weather_info$id[weather_info$id == station]
  
}



patched_list <- purrr::map(weather, function(x){
  id <- x$id[1]
  coords <- c(weather_info$Longitude[weather_info$id == id], weather_info$Latitude[weather_info$id == id])
  
  aux_info["distance"] <- round(sp::spDistsN1(as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                              c(coords[1], coords[2]), longlat = TRUE), 2)
  
  #sort by increasing distance
  aux_info <- aux_info[order(aux_info$distance),]
  
  #take first 25 stations
  aux_info_sub <- aux_info[2:26,]
  
  chillR::patch_daily_temps(weather = weather[[id]], patch_weather = aux_list[aux_info_sub$id], 
                            max_mean_bias = 3, max_stdev_bias = 3)
})


#evaluate how many gaps before and after
gaps_before <- purrr::map(weather, function(x){
  c(sum(is.na(x$Tmin)), sum(is.na(x$Tmax)), sum(is.na(x$Precip)))
}) %>% 
  bind_cols() %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(id = rownames(.))

gaps_remaining <- purrr::map(patched_list, function(x){
  c(sum(is.na(x$weather$Tmin)), sum(is.na(x$weather$Tmax)), sum(is.na(x$weather$Precip)))
}) %>% 
  bind_cols() %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(id = rownames(.))

#--> remove weather stations with more than a month of missing data
id_keep <- gaps_remaining$id[gaps_remaining$V1 <= 30 & gaps_remaining$V2 <= 30]



###
final_weather <-  purrr::map(patched_list, function(x) x$weather)

final_weather <- purrr::map(final_weather[id_keep], fix_weather)

final_weather <- purrr::map(final_weather, function(x) x$weather)

#add information on number of patched and interpolated days to weather info
final_weather_info <- weather_info[weather_info$id %in% id_keep,]

#adjust column names
colnames(gaps_before) <- c('Tmin_gap_total', 'Tmax_gap_total', 'Prec_gap_total', 'id')
colnames(gaps_remaining) <- c('Tmin_gap_patched', 'Tmax_gap_patched', 'Prec_gap_patched', 'id')

#merge info on gaps to weather_info
final_weather_info <- merge.data.frame(final_weather_info, gaps_before, by = 'id')
final_weather_info <- merge.data.frame(final_weather_info, gaps_remaining, by = 'id')


#write fixed weather data
save_temperature_scenarios(final_weather, path = 'data/weather/', prefix = 'CA_temp_rain')
write.csv(final_weather_info, file = 'data/CA_weather_info.csv', row.names = FALSE)
