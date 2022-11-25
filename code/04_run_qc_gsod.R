#read CIMIS data, run quality control on it
library(chillR)

weather_info <- read.csv('downloaded_data/gsod_target_weather_info.csv')
weather <- load_temperature_scenarios(path = 'downloaded_data/gsod/', 
                                      prefix = 'gsod_target_')

#make sure weather data is same organized as weather info
weather <- weather[weather_info$id]

library(weatherQC)
library(tidyverse)

#without spatial regression
wc_costa_out <- weather_qc_costa(weather_list = weather, weather_info = weather_info,
                                 aux_list = weather, aux_info = weather_info,
                 region = 'USA', subregion = 'California')

chillR::save_temperature_scenarios(wc_costa_out, path = 'qc_results_gsod/', prefix = 'costa')

wc_durre_out <- weather_qc_durre(weather_list = weather, weather_info = weather_info,
                                 region = 'USA', subregion = 'California')
chillR::save_temperature_scenarios(wc_durre_out, path = 'qc_results_gsod/', prefix = 'durre')
