#create temperature scenarios for the weather generators

#2050 and 2085

library(chillR)
library(tidyverse)
source('code/custom_chillR_functions/modified_comprehensive_precipitation_generator.R')
source('code/custom_chillR_functions/temperature_generation_rmawgen_prec.R')
source('code/custom_chillR_functions/custom_temperature_scenarios_from_records.R')

#read observed temoperature data
weather <- load_temperature_scenarios('data/weather/', 'CA_temp_rain')

#read extracted climate change data
clim_change_data <-read.csv('data/cmip6_point_extracted.csv')

#function for rolling mean
get_rolling_mean <- function(x, n = 11){
  
  x_lead <- purrr::map(1:floor(n/2), function(i) lead(x,i)) 
  x_lag <- purrr::map(1:floor(n/2), function(i) lag(x,i)) 
  
  x_lead <- do.call(cbind, x_lead)
  x_lag <- do.call(cbind, x_lag)
  
  x_adj <- cbind.data.frame(x_lag, x, x_lead)
  
  n_obs <- n - rowSums(is.na(x_adj))
  
  return(rowSums(x_adj, na.rm = TRUE) / n_obs)
}

#bring climate change data in long format, then calculate the rolling mean
test <- clim_change_data %>% 
  reshape2::melt(id.vars = c('ssp', 'model', 'variable', 'Date'), variable.name = 'id') %>% 
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date)) %>% 
  group_by(ssp, model, variable, Month, id) %>% 
  arrange(Year) %>% 
  mutate(roll_mean = get_rolling_mean(value)) %>% 
  ungroup()

#get values for the years of interest: 2050 and 2085
scenario_list <- test %>% 
  filter(Year %in% c(2050, 2085)) %>% 
  group_split(ssp, model, Year)

#some combinations have only information on precipitation, but not on temperature. lets kick them out
scenario_list_short <- purrr::map(scenario_list, function(x){
  if(all(c('tasmax', 'pr', 'tasmin') %in%  unique(x$variable))){
    return(x)
  } 
})


#remove empty entries
scenario_list_short <- scenario_list_short[lapply(scenario_list_short,length)>0]

scenario_list_final <- list()

#re-orgnaize the scenario information, so that we have a list of the same length as weather
for(id in names(weather)){
  #id <- 'cimis_2'
  scen_list_id <- purrr::map(scenario_list_short, function(x){
    pr <- as.data.frame(x[x$id == id & x$variable == 'pr',])
    tasmax <- as.data.frame(x[x$id == id & x$variable == 'tasmax',])
    tasmin <- as.data.frame(x[x$id == id & x$variable == 'tasmin',])
    
    #make sure they are ordered correctly
    pr <- pr[order(pr$Month),'roll_mean']
    tasmax <- tasmax[order(tasmax$Month), 'roll_mean']
    tasmin <- tasmin[order(tasmin$Month), 'roll_mean']
    
    return(data.frame('Tmin' = tasmin,
                      'Tmax' = tasmax,
                      'Prec' = pr))
  })
  
  ssp_year_model <-  purrr::map_chr(scenario_list_short, function(x){
    model <- unique(x$model)
    year <- unique(x$Year)
    ssp <- unique(x$ssp)
    return(paste(ssp, year, model, sep = '_'))
  })
  
  names(scen_list_id) <- ssp_year_model
  
  scenario_list_final[[id]] <- scen_list_id
}

rm(scen_list_id, scenario_df, scenario_list, scenario_list_short)

#itererate over the weather stations, 

future_weather_simulation <- list()

#check for which weather stations the code already ran
dirs <- list.dirs(path = 'data/future_simulated_weather/', full.names = F)
dirs <- gsub(' ', '', dirs)

for(id in names(weather)){
  
  if(id %in% dirs){
    next()
  }

 weather_out <- temperature_generation_rmawgen_prec(weather = weather[[id]], 
                                                                         years = c(1990, 2015),
                                                                         sim_years = c(2001, 2100),
                                                                         temperature_scenario = scenario_list_final[[id]], 
                                                                         seed = 123456789)
  fpath <- paste('data/future_simulated_weather/',id)
  dir.create(fpath)
  
  chillR::save_temperature_scenarios(weather_out, 
                                     path = fpath, 
                                     prefix = 'CA_future')
}


