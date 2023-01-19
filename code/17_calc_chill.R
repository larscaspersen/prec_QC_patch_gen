setwd('../04_patching_precipitation/')

library(chillR)
library(tidyverse)


#read generated temperature
fnames <- list.files('data/historical_simulated_weather/')

weather <- lapply(paste0('data/historical_simulated_weather/', fnames), read.csv)

#split the data by year, which is now saved by column
weather <- purrr::map(weather, function(x){
  df1990 <- x[,1:7]
  df2000 <- x[,8:14]
  df2010 <- x[,15:21]
  df2020 <- x[,22:28]
  
  colnames(df1990) <- colnames(df2000) <- colnames(df2010) <- colnames(df2020) <- c('DATE', 'Year', 'Month', 'Day', 'Tmin', 'Tmax', 'Prec')
  list('1990' = df1990, '2000' = df2000, '2010' = df2010, '2020' = df2020)
})

#calculate metrics based on the historic simulated data

weather_info <- read.csv('data/CA_weather_info.csv')

dir.create('data/historic_simulated_chill')

for(i in 1:nrow(weather_info)){
  for(year in c('1990', '2000', '2010', '2020')){
    
    chill <- tempResponse_daily_list(weather[[i]][[year]], latitude = weather_info$Latitude[i])
    
    fname <- paste0('data/historic_simulated_chill/hist_sim_chill_', i, '_', weather_info$id[i],'_',year, '.csv')
    
    write.csv(chill, fname, row.names = FALSE)
    
    
    
  }
}

#read chill data and calculate the save winter chill for each station / year
test <- load_temperature_scenarios('data/historic_simulated_chill/', prefix = 'hist_sim_chill')

safe_cp <- purrr::map_dbl(test, function(x){
  quantile(x$Chill_Portions, probs = 0.1)
})


year <-  str_sub(names(test),-4,-1)
station <- str_sub(names(test),1,-6)

chill_df <- data.frame(station = station, year = year, safe_winter_chill = safe_cp)

#merge with weather info
chill_df <- merge.data.frame(chill_df, weather_info, by.x = 'station', by.y = 'id')

chill_df <- chill_df[, c('station', 'year', 'safe_winter_chill', 'Name', 'Latitude', 'Longitude', 'Elevation')]

write.csv(chill_df, file = 'data/historic_simulated_safe_winterchill.csv', row.names = F)
