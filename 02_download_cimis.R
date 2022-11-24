#download cimis data
library(chillR)

#download list of stations from CIMIS website
cimis_stations <- handle_cimis("list_stations")

#earliest data most stations cover period 1990 to 2017
cimis_stations <- cimis_stations[cimis_stations$End_date > as.Date('2021-12-31') &
                                   cimis_stations$Start_date < as.Date('1990-01-01'),]


# Check for duplicated station names. Normally, they have a different chillR_code and have different data.
# This is very important in case we want to patch data from weather stations located very close.
# Add a "2" for the duplicated name
cimis_stations$Name <- as.character(cimis_stations$Name)

cimis_stations[which(duplicated(cimis_stations$Name)), "Name"] <-
  paste(cimis_stations[which(duplicated(cimis_stations$Name)), "Name"], "2")

year_start <- 1990
year_end <- 2021

CIMIS_list <- NULL

for (i in 1:length(cimis_stations$Stat_num)) {
  print(i)
  
  data <- handle_cimis("download_weather", location = as.character(cimis_stations[i, "Stat_num"]),
                       time_interval = c(year_start, year_end), station_list = cimis_stations)
  
  if (!is.list(data) || is.na(data[[1]]) || is.null(data)){
    
    cimis_stations[i, "DATA"] <- "NO_DATA"
    
    data <- make_all_day_table(data.frame(Year = c(year_start, year_end),
                                          Month = c(1, 12),
                                          Day = c(1, 31), 
                                          Tmin = as.numeric(NA), 
                                          Tmax = as.numeric(NA),
                                          Precip = as.numeric(NA)), add.DATE = F)
    
    data["Weather_Station"] <- as.character(CIMIS_WS_90s_20s[i, "Name"])
    
    data <- data[c("Weather_Station", colnames(data)[1 : 5])] } else {
      
      # If WS "i" has data, it tells to the data frame of WS that "i" effectively has data.
      # Then, select only Tmin and Tmax columns
      
      cimis_stations[i, "DATA"] <- "YES_DATA"
      
      data <- subset(data[[2]], select = c("Year", "Month", "Day", "Maximum Air Temperature", "Minimum Air Temperature", "Precipitation",
                                           "QC for Maximum Air Temperature",
                                           "QC for Minimum Air Temperature",
                                           "QC for Precipitation",
                                           "Average Air Temperature",
                                           "QC for Average Air Temperature"))
      
      #rename columns
      data <- rename(data, Tmax = "Maximum Air Temperature",
                     Tmin = "Minimum Air Temperature",
                     Tmean = "Average Air Temperature",
                     Precip = "Precipitation",
                     QC_Tmax = "QC for Maximum Air Temperature",
                     QC_Tmin = "QC for Minimum Air Temperature",
                     QC_Precip = "QC for Precipitation",
                     QC_Tmean = "QC for Average Air Temperature")
      
      data <- data.frame(Weather_Station = as.character(cimis_stations[i, "Name"]), data)}
  
  CIMIS_list <- c(CIMIS_list, list(data))
  
}

CIMIS_list <- readRDS('data/cimis_data.RData')
cimis_stations <- readRDS('data/cimis-overveiw.RData')

#adjust the id name of the stations
cimis_stations$id <- paste0('cimis_',cimis_stations$Stat_num)

#change names of list elements to id of stations
names(CIMIS_list) <- cimis_stations$id

#save weather data  
chillR::save_temperature_scenarios(CIMIS_list, path = 'data-raw/CIMIS/', prefix = 'target-station')

#save overview file
write.csv(cimis_stations, file = 'data-raw/cimis_info.csv', row.names = F)

#saveRDS(CIMIS_list, file='data/cimis_data.RData')
#saveRDS(cimis_stations, file='data/cimis-overveiw.RData')




cimis_info <-read.csv('downloaded_data/cimis_info.csv')
weather <- chillR::load_temperature_scenarios(path = 'downloaded_data/cimis/', prefix = 'target-station')

#make sure it is in the same order
weather <- weather[cimis_info$id]


Tmin_complete <- purrr::map_dbl(weather, function(x){
  sum(is.na(x$Tmin) == FALSE) / nrow(x)
})

Tmax_complete <- purrr::map_dbl(weather, function(x){
  sum(is.na(x$Tmax) == FALSE) / nrow(x)
  
})

Precip_complete <- purrr::map_dbl(weather, function(x){
  sum(is.na(x$Precip) == FALSE) / nrow(x)
})

cimis_info <- cbind(cimis_info, Tmin_complete, Tmax_complete, Precip_complete)

write.csv(cimis_info,file = 'downloaded_data/cimis_info.csv')