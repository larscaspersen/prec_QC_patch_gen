library(chillR)
#setwd('C:/Users/Lars C/Documents/PhD/projects/projects_2022/qc_patch_gen_paper')
devtools::install_github("larscaspersen/weatherQC")
library(weatherQC)
#download weather data: GSOD
library(GSODR)
library(tidyverse)

#get inventory
inventory <- GSODR::get_inventory()

#plot which stations in CA have at least 30 years of coverage
inventory %>% 
  filter(CTRY == 'US' & STATE == 'CA', 
         trunc(END / 10000) - trunc(BEGIN / 10000) >= 30) %>% 
  ggplot(aes(x = trunc(BEGIN / 10000))) +
  geom_histogram()

ca_inventory <- inventory %>% 
  filter(CTRY == 'US' & STATE == 'CA', 
         trunc(END / 10000) - trunc(BEGIN / 10000) >= 30)

min(trunc(ca_inventory$BEGIN / 10000))


#download gsod data
test <- get_GSOD(years = 1933:2021, station = ca_inventory$STNID)

#GSODR::reformat_GSOD(dsn = 'C:/Users/Lars C/AppData/Local/Temp/RtmpKmfQxM')




setwd('C:/Users/Lars C/Documents/PhD/projects/projects_2022/qc_patch_gen_paper')

#split by station id

for(id in unique(test$STNID)){
  
  print(id)
  #id <- "725940-99999"
  
  sub <- subset(test, test$STNID == id)
  
  #take only certain columns:
  keep_cols <- c("STNID", "NAME", "LATITUDE", "LONGITUDE", "ELEVATION",
                 "YEAR", "MONTH", "DAY", "YDAY", "TEMP","DEWP", "MAX", "MIN", 
                 "PRCP", "EA", "ES", "RH")
  
  sub <- select(sub, keep_cols)
  
  #change column names to Day Month Year
  
  colnames(sub)[colnames(sub) %in% c("YEAR", 'MONTH', 'DAY')] <- c('Year', 'Month', 'Day')
  
  sub <- chillR::add_date(sub)
  sub$Date <- as.Date(sub$Date)
  
  #check if the 31st of December 2021 is in the dataset, else add it
  if(any(sub$Year == 2021 & sub$Month == 12 & sub$Day == 31) == FALSE){
    
    
    sub <- rbind(sub,data.frame("STNID" = NA, "NAME" = NA, "LATITUDE" = NA, "LONGITUDE" = NA, 
               "ELEVATION" = NA,
               "Year" = 2021, "Month" = 12, "Day" = 31, "YDAY" = 365, 
               "TEMP" = NA,"DEWP" = NA, "MAX" = NA, "MIN" = NA, 
               "PRCP" = NA, "EA" = NA, "ES" = NA, "RH" = NA, "Date" = as.Date('2021-12-31')))

  }
  
  #if the 1st of janauary is missing, add it aswell
  if(any(sub$Year == 1933 & sub$Month == 01 & sub$Day == 01) == FALSE){
    
    
    sub <- rbind(data.frame("STNID" = NA, "NAME" = NA, "LATITUDE" = NA, "LONGITUDE" = NA, 
                                "ELEVATION" = NA,
                                "Year" = 1933, "Month" = 01, "Day" = 01, "YDAY" = 1, 
                                "TEMP" = NA,"DEWP" = NA, "MAX" = NA, "MIN" = NA, 
                                "PRCP" = NA, "EA" = NA, "ES" = NA, "RH" = NA, "Date" = as.Date('2021-12-31')),
                 sub)
    
  }
  
  sub <- make_all_day_table(sub,no_variable_check = TRUE, add.DATE = FALSE)
  sub$STNID <- sub$STNID[1]
  sub$NAME <- sub$NAME[1]
  sub$LATITUDE <- sub$LATITUDE[1]
  sub$LONGITUDE <- sub$LONGITUDE[1]
  sub$ELEVATION <- sub$ELEVATION[1]
  
  sub$STNID
  
  fname <- paste0('downloaded_data/gsod/station_', sub$STNID[1],'.csv')
  
  #save file 
  write.csv(sub, file = fname, row.names = F)
}

rm(test, ca_inventory, inventory)

#quality checks 
library(chillR)
library(tidyverse)

#read weather data
fnames <- list.files('downloaded_data/gsod/')
fnames <- paste0('downloaded_data/gsod/', fnames)
weather <- lapply(fnames, read.csv)

for( i in 1:length(weather)){
  
  if(any(weather[[i]]$Year == 1933 & weather[[i]]$Month == 1 & weather[[i]]$Day == 1) == FALSE){
    weather[[i]] <- rbind(data.frame("STNID" = NA, "NAME" = NA, "LATITUDE" = NA, "LONGITUDE" = NA, 
                            "ELEVATION" = NA,
                            "Year" = 1933, "Month" = 01, "Day" = 01, "YDAY" = 1, 
                            "TEMP" = NA,"DEWP" = NA, "MAX" = NA, "MIN" = NA, 
                            "PRCP" = NA, "EA" = NA, "ES" = NA, "RH" = NA, "Date" = as.Date('1933-01-01')),
                          weather[[i]])
    weather[[i]] <- make_all_day_table(weather[[i]],no_variable_check = TRUE, add.DATE = FALSE)
    
    weather[[i]]$STNID <- unique(weather[[i]]$STNID)[which(is.na(unique(weather[[i]]$STNID)) == FALSE)]
    weather[[i]]$NAME <- unique(weather[[i]]$NAME)[which(is.na(unique(weather[[i]]$NAME)) == FALSE)]
    weather[[i]]$LATITUDE <- unique(weather[[i]]$LATITUDE)[which(is.na(unique(weather[[i]]$LATITUDE)) == FALSE)]
    weather[[i]]$LONGITUDE <- unique(weather[[i]]$LONGITUDE)[which(is.na(unique(weather[[i]]$LONGITUDE)) == FALSE)]
    weather[[i]]$ELEVATION <- unique(weather[[i]]$ELEVATION)[which(is.na(unique(weather[[i]]$ELEVATION)) == FALSE)]
  }
}

#create weather info object
weather_info <- purrr::map(weather, function(x) x[1,c('STNID', 'NAME', 'LATITUDE', 'LONGITUDE', 'ELEVATION')]) %>% 
  bind_rows()

#adjust column names
weather <- purrr::map(weather, function(x){
  y <- x[,c('Year', 'Month', 'Day', 'MIN', 'MAX', 'TEMP', 'PRCP')]
  colnames(y) <- c('Year', 'Month', 'Day', 'Tmin', 'Tmax', 'Tmean', 'Precip')
  return(y)
})



colnames(weather_info) <- c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')

names(weather) <- weather_info$id



weather <- purrr::map(weather, function(x){
  x[x$Year >= 1990,]
})


Tmin_complete <- purrr::map_dbl(weather, function(x){
  sum(is.na(x$Tmin) == FALSE) / nrow(x)
})

Tmax_complete <- purrr::map_dbl(weather, function(x){
  sum(is.na(x$Tmax) == FALSE) / nrow(x)
  
})

Precip_complete <- purrr::map_dbl(weather, function(x){
  sum(is.na(x$Precip) == FALSE) / nrow(x)
})

weather_info <- cbind(weather_info, Tmin_complete, Tmax_complete, Precip_complete)

#only keep weather stations that had initally 90% or more covered in all variables
keep <- weather_info[weather_info$Tmin_complete >= 0.9 & 
             weather_info$Tmax_complete >= 0.9 &
             weather_info$Precip_complete >= 0.9,]

weather_keep <- weather[keep$id]

#remove old downloaded data
unlink('downloaded_data/gsod', recursive = TRUE)
#create folder again
dir.create('downloaded_data/gsod')

save_temperature_scenarios(weather_keep, path = 'downloaded_data/gsod/', prefix = 'gsod_target_')


#saveother data as auxilliary
weather_aux <- weather[!(weather_info$id %in% keep$id)]
save_temperature_scenarios(weather_aux, path = 'downloaded_data/gsod/', prefix = 'gsod_aux')

write.csv(keep, file = 'downloaded_data/gsod_target_weather_info.csv', row.names = FALSE)
write.csv(weather_info[!(weather_info$id %in% keep$id),], 
          file = 'downloaded_data/gsod_aux_weather_info.csv', row.names = FALSE)
