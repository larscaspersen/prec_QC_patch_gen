#go with normal ratio for fixing, 

#fix temperature data with chillR routine and precipitation with normal ratio method

########
#test evaluation on spei
########

#load functions
library(weatherImpute)
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
names
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

gsod_stat <- handle_gsod("list_stations", location = c(weather_info$Longitude[1], 
                                          weather_info$Latitude[1], 
                                          weather_info$Elevation[1]),
            stations_to_choose_from = 1500)

gsod_stat <- gsod_stat[(gsod_stat$BEGIN <= 19900101 & 
             gsod_stat$END >= 20220101 &
             gsod_stat$Lat >= 30 &
             gsod_stat$Lat <= 45 &
             gsod_stat$Long <= -110 & 
             gsod_stat$Long >= -130),]

gsod_stat <- gsod_stat[,c('chillR_code', 'STATION.NAME', 'Lat', 'Long', 'Elev', 'BEGIN', 'END')]
colnames(gsod_stat) <- c('id', 'Name', 'Latitude', 'Longitude', 'Elevation', 'Start', 'End')
 
cimis_stat <- handle_cimis(action = 'list_stations')
cimis_stat <- cimis_stat[cimis_stat$Start_date <= as.Date('1990-01-01') &
                           cimis_stat$End_date >= as.Date('2022-01-01'),]

cimis_stat <- cimis_stat[,c('Stat_num', 'Name', 'Latitude', 'Longitude', 'Elevation', 'Start_date', 'End_date')]
colnames(cimis_stat) <- c('id', 'Name', 'Latitude', 'Longitude', 'Elevation', 'Start', 'End')
cimis_stat$id <- paste0('cimis_', cimis_stat$id)

#combine cimis and gsod
aux_info <- rbind(gsod_stat, cimis_stat)

rm(gsod_stat, cimis_stat)

#mark already downloaded weather stations
#I changed the id of gsod weather stations...


#--> two weather stations are not in the list. how?
#because the gsod data I downloaded was not via chillR
weather_info$old_id <- aux_info$id[match(weather_info$Name, aux_info$Name)]

#add the "missing" weather stations to the aux_info list
aux_info <- rbind(aux_info[, c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')],
      weather_info[is.na(weather_info$old_id), c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')])


#mark which ones were already downloaded
aux_info$downloaded <- ifelse(aux_info$id %in% c(weather_info$id, weather_info$old_id), yes = 'yes', no = 'no')

#mark which station comes from which source

aux_info$source <-  ifelse(str_detect(aux_info$id, pattern = 'cimis'), yes = 'cimis', no = 'gsod')


aux_list <- list()

#download auxiliary weather stations for each target station
for(station in names(weather)){
  
  #station <- names(weather)[1]
  
  #target weather station coordinates
  coords <- c(weather_info$Longitude[weather_info$id == station], weather_info$Latitude[weather_info$id == station])
  
  
  aux_info["distance"] <- round(sp::spDistsN1(as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                                       c(coords[1], coords[2]), longlat = TRUE), 2)
  
  #sort by increasing distance
  aux_info <- aux_info[order(aux_info$distance),]
  
  #take first 25 stations
  aux_info_sub <- aux_info[2:26,]
  

  #split data to be downloaded according to source
  cimsi_download <- aux_info_sub[aux_info_sub$source == 'cimis' & aux_info_sub$downloaded == 'no',]
  gsod_download <- aux_info_sub[aux_info_sub$source == 'gsod' & aux_info_sub$downloaded == 'no',]
  
  
  #download and handle gsod
  
  
  #if there are stations for gsod to download, do that
  if(nrow(gsod_download) > 0)
    #download the data
    gsod_aux <-  chillR::handle_gsod(action = 'download_weather', gsod_download$id, 
                                     time_interval = c(1990, 2021))
    
    #cleaning step
    gsod_aux <- chillR::handle_gsod(gsod_aux)
    
    #list which ones had no data and should be removed from the aux list
    empy_download <- gsod_download$id[!(gsod_download$Name %in% names(gsod_aux))]
    
    #mark in overall aux list as empty
    if(length(empy_download) > 0){
      aux_info$downloaded[aux_info$id %in% empy_download] <- 'empty'
    }
    
    #list successfull downloads
    successful_download <- gsod_download$id[gsod_download$Name %in% names(gsod_aux)]
    
    #mark in aux_info, append to aux_list
    if(length(successful_download) > 0){
      aux_info$downloaded[aux_info$id %in% successful_download] <- 'yes'
      names(gsod_aux) <- successful_download
      aux_list <- c(aux_list, gsod_aux)
    }
    
    
    #download and handle cimis
    
    if(nrow(cimsi_download) > 0){
      
      #extract the number from the id, it is only the number which is needed
     ids <- strsplit(cimsi_download$id,split='_', fixed=TRUE) %>% 
        purrr::map_chr(function(x) x[2])
     
     #in case of cimis_83 it seems that it does not contain any data for 2021, at least
     #I always get an error message, so download only data unitl 2020
     
     if(ids == "83"){
       #cimis id needs to be a chara
       cimis_aux <- chillR::handle_cimis(action = "download_weather", 
                                         location = ids,
                                         time_interval = c(1990, 2020))
       
       #remove rows which contain quality flag R and S
       cimis_aux$weather$`Minimum Air Temperature`[cimis_aux$weather$`QC for Minimum Air Temperature` %in% c("R", "S")] <- NA
       cimis_aux$weather$`Maximum Air Temperature`[cimis_aux$weather$`QC for Maximum Air Temperature` %in% c("R", "S")] <- NA
       cimis_aux$weather$`Average Air Temperature`[cimis_aux$weather$`QC for Average Air Temperature` %in% c("R", "S")] <- NA
       cimis_aux$weather$Precipitation[cimis_aux$weather$`QC for Precipitation` %in% c("R", "S")] <- NA
       
       #clean downloaded data
       cimis_aux <- chillR::handle_cimis(cimis_aux)
       
       #append one row for end of observation period, fill intermediate missing observations with blank values
       cimis_aux$weather <- make_all_day_table(rbind(cimis_aux$weather, data.frame(Year = 2021, Month = 12, Day = 31, 
                                          Tmin = NA, Tmax = NA, Tmean = NA, Prec = NA)), add.DATE = FALSE)
       
     } else {
       
       #cimis id needs to be a chara
       cimis_aux <- chillR::handle_cimis(action = "download_weather", 
                                         location = ids,
                                         time_interval = c(1990, 2021))
       
       #remove rows which contain quality flag R and S
       cimis_aux$weather$`Minimum Air Temperature`[cimis_aux$weather$`QC for Minimum Air Temperature` %in% c("R", "S")] <- NA
       cimis_aux$weather$`Maximum Air Temperature`[cimis_aux$weather$`QC for Maximum Air Temperature` %in% c("R", "S")] <- NA
       cimis_aux$weather$`Average Air Temperature`[cimis_aux$weather$`QC for Average Air Temperature` %in% c("R", "S")] <- NA
       cimis_aux$weather$Precipitation[cimis_aux$weather$`QC for Precipitation` %in% c("R", "S")] <- NA
       
       #clean downloaded data
       cimis_aux <- chillR::handle_cimis(cimis_aux)
       
     }
     
     #list which ones had no data and should be removed from the aux list
     empy_download <- cimsi_download$id[!(cimsi_download$Name %in% names(cimis_aux))]
     
     #mark in overall aux list as empty
     if(length(empy_download) > 0){
       aux_info$downloaded[aux_info$id %in% empy_download] <- 'empty'
     }
     
     #list successfull downloads
     successful_download <- cimsi_download$id[cimsi_download$Name %in% names(cimis_aux)]
     
     #mark in aux_info, append to aux_list
     if(length(successful_download) > 0){
       aux_info$downloaded[aux_info$id %in% successful_download] <- 'yes'
       names(cimis_aux) <- successful_download
       aux_list <- c(aux_list, cimis_aux)
     }
     

      
      
    }
    
}

aux_list_2 <- purrr::map(aux_list, function(x) x[[2]])
#only keeo weather stations in info file that are in the aux list
aux_info2 <- aux_info[aux_info$id %in% names(aux_list_2),]

save_temperature_scenarios(aux_list_2, 'downloaded_data/aux_data/', prefix = 'aux_ws')
write.csv(aux_info2, file = 'downloaded_data/aux_wather_info.csv', row.names = F)

