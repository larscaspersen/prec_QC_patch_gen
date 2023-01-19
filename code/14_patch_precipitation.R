#patch precipitation

library(chillR)
library(tidyverse)
library(weatherImpute)

weather <- load_temperature_scenarios(path = 'data/weather/', prefix = 'CA_temp_rain')
weather_info <- read.csv('data/CA_weather_info.csv')

#cycle through the weather data, make sure that each column is called 'Prec'
#make sure that the columns are called "Prec" and not "Precip"
weather <- purrr::map(weather, function(x){
  if("Prec" %in% colnames(x)){
    x
  } else {
    x$Prec <- x$Precip
    x
  }
})




#wrapper to make 
wrap_chillR_weather <- function(target_list, weather_info, aux_list, aux_info, 
                                variable, patchmethod){
  
  target_df <- purrr::map(target_list, variable) %>% 
    bind_cols()
  
  aux_df <- purrr::map(aux_list, variable) %>% 
    bind_cols()
  
  
  
  #make sure that aux_df and target_df columns are in right order
  target_df <- target_df[weather_info$id]
  aux_df <- aux_df[aux_info$id]

  weather_df <- cbind(target_df, aux_df)
  weather_info_df <- rbind(weather_info, aux_info)
  
  #append Date, Year, Month, Day
  weather_df <- cbind(target_list[[1]][,c('Date', 'Year', 'Month', 'Day')], weather_df)
  
  
  out <- patch_flexible_several_stations(weather = weather_df, target = weather_info$id, 
                                  weather_info = weather_info_df, 
                                  method = pathch_method,
                                  method_patches_everything = F)
  
  #check if there are remaining gaps
  if(any(is.na(out))){
    n_remaining_gaps <- sum(is.na(out))
    warning(paste0('Some gaps remain. ', 
                  n_remaining_gaps,
                  ' gaps remain in all target weather stations combined'))
    
    out <- cbind(target_list[[1]][,c('Year', 'Month', 'Day')], out)
    
    warning('Because gaps remain, using patch_mice as an backup')
    
    
    #make sure that aux_names do not contain -, exchange with _
    colnames(out) <- gsub(colnames(out), pattern = '-', replacement = '_')
    weather_info_df$id <- gsub(weather_info_df$id, pattern = '-', replacement = '_')
    
    
    
    #use mice as a backup
    out <- patch_flexible_several_stations(weather = out, 
                                           target = weather_info$id, 
                                    weather_info = weather_info, 
                                    method = 'patch_mice',
                                    method_patches_everything = T)
  }
  
  #bring data.frame to original format
  
  #add column for no_Precip
  
  for(i in 1:length(target_list)){
    target_list[[i]][,variable] <- out[,names(target_list)[i]]
    target_list[[i]][,paste0('no_', variable)] <- is.na(target_df[names(target_list)[i]])
  }
  
  return(target_list)

}


pathch_method <- 'patch_normal_ratio'
aux_list <- load_temperature_scenarios('downloaded_data/aux_data/', prefix = 'aux_ws')
aux_info <- read.csv('downloaded_data/aux_wather_info.csv')

weather_info <- weather_info[, c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')]
aux_info <- aux_info[, c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')]


out <- wrap_chillR_weather(target_list = weather, 
                    weather_info = weather_info, 
                    aux_list = aux_list, 
                    aux_info = aux_info, 
                    variable = 'Prec',
                    patchmethod = 'patch_normal_ratio')


#only take columns that we need

out <- purrr::map(out, function(x){
  x[,c("Date", "doy","id", "Year", "Month", "Day", "Tmin", "Tmax", 
    "Tmean", "Prec", "no_Tmin", "no_Tmax", "no_Prec")]
  })

save_temperature_scenarios(out, path = 'data/weather/', prefix = 'CA_temp_rain')
