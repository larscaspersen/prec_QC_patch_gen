#make temperature scenarios: historic

library(chillR)
weather <- load_temperature_scenarios('data/weather/', 'CA_temp_rain')

#temperature scenarios
scen_years <- c(1990, 2000, 2010, 2020)

for(i in 1:length(weather)){
  i<- 1
  
  #need function for precipitation scenario
  
  #create temperature scenarios
  temp_scenarios <- temperature_scenario_from_records(weather=weather[[i]],year=c(1990, 2000, 2010,2020))
  
  
  

}




temperature_generation()