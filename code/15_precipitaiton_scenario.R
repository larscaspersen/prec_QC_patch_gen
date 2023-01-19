#make temperature scenarios: historic

library(chillR)
weather <- load_temperature_scenarios('data/weather/', 'CA_temp_rain')
source('code/custom_chillR_functions/custom_temperature_scenarios_from_records.R')
source('code/custom_chillR_functions/temperature_generation_rmawgen_prec.R')


#temperature scenarios
scen_years <- c(1990, 2000, 2010, 2020)

hist_sim_list <- list()

for(i in 1:length(weather)){


  #create temperature scenarios
  scenarios <- custom_temperature_scenario_from_records(weather=weather[[i]],year=scen_years)
  

  
  hist_sim_list[[i]] <- temperature_generation_rmawgen_prec(weather = weather[[i]], 
                                              years = c(1990,2021), 
                                              sim_years = c(2001, 2100), 
                                              temperature_scenario = scenarios, 
                                              seed = 123456789)
    
    
}


save_temperature_scenarios(hist_sim_list, path = 'data/historical_simulated_weather/', prefix = 'hist_sim')
