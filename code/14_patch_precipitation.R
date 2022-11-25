#patch precipitation

library(chillR)
library(tidyverse)

weather <- load_temperature_scenarios(path = 'data/weather/', prefix = 'CA_temp_rain')
weather_info <- read.csv('data/CA_weather_info.csv')

library(weatherImpute)

#wrapper to make 
wrap_chillR_weather <- function(target_list, weather_info, aux_list, aux_info, 
                                variable, patchmethod){
  
  pathch_method <- 'patch_normal_ratio'
  
  weather_list <- weather
  aux_list <- load_temperature_scenarios('downloaded_data/aux_data/', prefix = 'aux_ws')
  aux_info <- read.csv('downloaded_data/aux_wather_info.csv')
  
  weather_info <- weather_info[, c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')]
  aux_info <- aux_info[, c('id', 'Name', 'Latitude', 'Longitude', 'Elevation')]
  
  
  variable <- 'Prec'
  
  if(variable %in% c('Prec', 'Precip')){
    prec_df <- purrr::map(weather_list, 'Prec') %>% 
      bind_cols()
    
    precip_df <- purrr::map(weather_list, 'Precip') %>% 
      bind_cols()
    
    #merge prec and precip df
    if(nrow(prec_df) > 0 & nrow(precip_df) > 0){
      prec_df <- cbind(prec_df, precip_df)[weather_info$id]
    } else if(nrow(prec_df) > 0) {
      target_df <- prec_df
    } else if(nrow(precip_df) > 0){
      target_df <- precip_df
    }
    
    prec_df_aux <- purrr::map(aux_list, 'Prec') %>% 
      bind_cols()
    
    precip_df_aux <- purrr::map(aux_list, 'Precip') %>% 
      bind_cols()
    
    #merge prec and precip df
    if(nrow(prec_df_aux) > 0 & nrow(precip_df_aux) > 0){
      prec_df <- cbind(prec_df_aux, precip_df_aux)[weather_info$id]
    } else if(nrow(prec_df_aux) > 0) {
      aux_df <- prec_df_aux
    } else if(nrow(precip_df_aux) > 0){
      aux_df <- precip_df_aux
    }
    
    
  } else {
    target_df <- purrr::map(weather_list, variable) %>% 
      bind_cols()
    
    aux_df <- purrr::map(aux_list, variable) %>% 
      bind_cols()
    
  }
  
  #make sure that aux_df and target_df columns are in right order
  target_df <- target_df[weather_info$id]
  aux_df <- aux_df[aux_info$id]

  weather_df <- cbind(target_df, aux_df)
  weather_info_df <- rbind(weather_info, aux_info)
  
  #append Date, Year, Month, Day
  weather_df <- cbind(weather_list[[1]][,c('Date', 'Year', 'Month', 'Day')], weather_df)
  
  
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
    
    out <- cbind(weather_list[[1]][,c('Year', 'Month', 'Day')], out, aux_df)
    
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
  
  for(i in length(weather_list)){
    weather_list[[i]][,variable] <- out[,names(weather_list)[i]]
    weather_list[[i]][,paste0('no_', variable)] <- is.na(target_df[names(weather_list)[i]])
  }
  
  return(weather_list)

}

save_temperature_scenarios(weather_list, path = 'data/weather/', prefix = 'CA_temp_rain')
