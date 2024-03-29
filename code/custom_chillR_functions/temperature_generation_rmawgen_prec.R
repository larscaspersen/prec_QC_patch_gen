#create function to use rmawgen similar to in chillR
# do the same with rgenerate prec

library(chillR)
library(RGENERATEPREC)
library(RMAWGEN)
library(lubridate)
source('code/custom_chillR_functions/modified_comprehensive_precipitation_generator.R')

#read fixed weather of JUnin
# weather <- read.csv('data/WGEN/fixed_8_Junin.csv')
# 
# years <- seq(1961,2013, by =4)
# sim_years <- c(2001,2099)
# 
# generate_prec = TRUE
# seed = 99
# check_temperature_scenario_type = TRUE 
# temperature_check_args = NULL
# max_reference_year_difference = 5 
# warn_me = TRUE
# remove_NA_scenarios = TRUE
# temperature_scenario <- temperature_scenario_from_records(weather = weather, year = years)
# 
# library(RMAWGEN)
# library(RGENERATEPREC)

temperature_generation_rmawgen_prec <- function (weather, 
                                                 years, 
                                                 sim_years, 
                                                 temperature_scenario = data.frame(Tmin = rep(0,12), 
                                                                                   Tmax = rep(0, 12)), 
                                                 seed = 99, 
                                                 check_temperature_scenario_type = TRUE,
                                                 temperature_check_args = NULL, 
                                                 max_reference_year_difference = 5,
                                                 warn_me = TRUE, 
                                                 remove_NA_scenarios = TRUE)
{
  if (is.data.frame(temperature_scenario)) 
    temperature_scenario <- list(temperature_scenario)
  
  temperature_scenario_check_n_intervals <- 12
  temperature_scenario_check_check_scenario_type <- TRUE
  temperature_scenario_check_scenario_check_thresholds <- c(-5, 10)
  temperature_scenario_check_update_scenario_type <- TRUE
  temperature_scenario_check_warn_me <- TRUE
  
  if (!is.null(temperature_check_args)) {
    if ("n_intervals" %in% names(temperature_check_args)) 
      temperature_scenario_check_n_intervals <- temperature_check_args$n_intervals
    if ("check_scenario_type" %in% names(temperature_check_args)) 
      temperature_scenario_check_check_scenario_type <- temperature_check_args$check_scenario_type
    if ("scenario_check_thresholds" %in% names(temperature_check_args)) 
      temperature_scenario_check_scenario_check_thresholds <- temperature_check_args$scenario_check_thresholds
    if ("update_scenario_type" %in% names(temperature_check_args)) 
      temperature_scenario_check_update_scenario_type <- temperature_check_args$update_scenario_type
    if ("warn_me" %in% names(temperature_check_args)) 
      temperature_scenario_check_warn_me <- temperature_check_args$warn_me
  }
  
  if (is.null(temperature_scenario)) 
    stop("No temperature scenario provided")
  
  if (remove_NA_scenarios) {
    scen_ok <- rep(1, length(temperature_scenario))
    for (i in 1:length(temperature_scenario)) if (length(which(is.na(temperature_scenario[[i]]$data))) > 
                                                  0) 
      scen_ok[i] <- 0
    temperature_scenario <- temperature_scenario[which(scen_ok == 
                                                         1)]
  }
  #check each individual temperature scenario on correctness?
  for (ts in 1:length(temperature_scenario)) {
    temperature_scenario[[ts]] <- check_temperature_scenario(temperature_scenario[[ts]], 
                                                             n_intervals = temperature_scenario_check_n_intervals, 
                                                             check_scenario_type = temperature_scenario_check_check_scenario_type, 
                                                             scenario_check_thresholds = temperature_scenario_check_scenario_check_thresholds, 
                                                             update_scenario_type = temperature_scenario_check_update_scenario_type, 
                                                             warn_me = temperature_scenario_check_warn_me)
    if (!nrow(temperature_scenario[[ts]]$data) == 12) 
      stop("This function only works with monthly temperature scenarios", 
           call. = FALSE)
  }
  
  #extract weather data if weather is a list
  if ("weather" %in% names(weather) & "QC" %in% names(weather)) 
    weather <- weather$weather
  
  
  #here it gets more interesting
  #translate some arguments
  
  year_min <- years[1]
  year_max <- years[2]
  year_min_sim <- sim_years[1]
  year_max_sim <- sim_years[2]
  
  #this is the amount of repitions in the normalization algorithm
  n_GPCA_iter <- 5
  n_GPCA_iteration_residuals <- 5
  
  #this is a lag factor, in their presentation slides they showed that for trentino data a lag of 10 days worked better
  p <- 1
  p_prec <- 1
  

  PREC <- weather[, c("Month", "Day", "Year",'Prec')]
  #take only precipitation of there reference period
  colnames(PREC) <- c("month", "day", "year", "station")
  #somehow the function only works if two stations or more get supplied
  #because of spatial correlations among the stations. but this is not needed for us,
  #so we just put a copy of the stations precipitation observation to the data frame
  PREC <- cbind(PREC, station_copy = PREC$station)
  
  #threshold for occurence of precipitation (in ml)
  valmin <- 1.0

  
  TEMP_MAX <- weather[, c("Month", "Day", "Year", "Tmax")]
  colnames(TEMP_MAX) <- c("month", "day", "year", "station")
  
  TEMP_MIN <- weather[, c("Month", "Day", "Year", "Tmin")]
  colnames(TEMP_MIN) <- c("month", "day", "year", "station")
  

  #get mean min / max temperature for the whole reference period
  max_mean <- as.matrix(RMAWGEN::getMonthlyMean(TEMP_MAX, 
                                                year_min, year_max)[, 4])
  colnames(max_mean) <- "station"
  rownames(max_mean) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                          12)
  
  
  min_mean <- as.matrix(RMAWGEN::getMonthlyMean(TEMP_MIN, 
                                                year_min, year_max)[, 4])
  colnames(min_mean) <- "station"
  rownames(min_mean) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                          12)
  
  
  
  alldays <- make_all_day_table(tab = data.frame(Year = c(year_min, 
                                                          year_max), 
                                                 Month = c(1, 12), 
                                                 Day = c(1, 31)), 
                                no_variable_check = TRUE)
  alldays[, "YEARMODA"] <- alldays$Year * 10000 + alldays$Month * 
    100 + alldays$Day
  
  
  #make a data frame with tmin
  Tn <- TEMP_MIN
  Tn[, "YEARMODA"] <- Tn$year * 10000 + Tn$month * 100 + Tn$day
  allTmins <- merge(alldays, Tn, by = "YEARMODA", all.x = TRUE)
  Tx <- TEMP_MAX
  Tx[, "YEARMODA"] <- Tx$year * 10000 + Tx$month * 100 + Tx$day
  allTmaxs <- merge(alldays, Tx, by = "YEARMODA", all.x = TRUE)
  
  miss <- length(which(is.na(allTmins$station))) + length(which(is.na(allTmaxs$station)))
  if (!miss == 0) 
    stop("Weather record not complete for the calibration period - NULL returned", 
         call. = FALSE)
  
  
  simoutput <- list()
  
  
  #progressbar to track the progress, duh
  #pb <- txtProgressBar(min = 0, max = length(temperature_scenario), style = 3)
  
  #start of the loop
  
  #loop over the to be simulated temperature scenarios
  for (ts in 1:length(temperature_scenario)) {
    
    #check if it is relative or absolute
    if (temperature_scenario[[ts]]$scenario_type == "relative") 
      if (is.na(temperature_scenario[[ts]]$reference_year)) {
        if (warn_me) 
          warning("Reference year missing - can't check if relative temperature scenario is valid", 
                  call. = FALSE)
      }
    
    #check if reference year and median of the years match
    else if (!temperature_scenario[[ts]]$reference_year == 
             median(c(year_min, year_max))) 
      stop("weather data used for calibration not valid as a baseline for this scenario", 
           "the reference year of the scenario must correspond to the median year of the weather record ", 
           "(specified by c(year_min,year_max). At the moment, this is ", 
           median(c(year_min, year_max)), " but it should be ", 
           temperature_scenario[[ts]]$reference_year, 
           ", so that it works for this scenario", call. = FALSE)
    
    #take data of running mean
    temperatures <- temperature_scenario[[ts]]$data
    
    #if relative scenario, adjust the mean temerpatures accordingly
    if (temperature_scenario[[ts]]$scenario_type == "relative") {
      mean_climate_Tn_sim = min_mean + temperatures$Tmin
      mean_climate_Tx_sim = max_mean + temperatures$Tmax
      if (!is.na(temperature_scenario[[ts]]$reference_year)) 
        if (!is.numeric(temperature_scenario[[ts]]$reference_year)) {
          if (warn_me) 
            warning("Reference year not numeric", call. = FALSE)
        }
      else {
        year_diff <- abs(temperature_scenario[[ts]]$reference_year - 
                           median(c(year_min, year_max)))
        if (year_diff > 0 & year_diff <= max_reference_year_difference) 
          if (warn_me) 
            warning(year_diff, " year(s) difference between reference years of the temperature scenario", 
                    " and the dataset used for calibrating the weather generator", 
                    call. = FALSE)
        if (year_diff > max_reference_year_difference) 
          stop("Difference between reference years of the temperature scenario and the dataset used for calibrating the ", 
               "weather generator greater than ", max_reference_year_difference, 
               " years (", year_diff, " years) - this is too much!", 
               call. = FALSE)
      }
    }
    
    #this is more interesting cause I am doing mostly absolute temperature scenarios
    if (temperature_scenario[[ts]]$scenario_type == "absolute") {
      
      #get running mean of tmin and tmax as unnamed vector
      mean_climate_Tn_sim = min_mean - min_mean + temperatures$Tmin
      mean_climate_Tx_sim = min_mean - min_mean + temperatures$Tmax
      mean_climate_Prec_sim = min_mean - min_mean + temperatures$Prec
      
      #in case of precipitation the function requires at least two stations, so just supply a copy of the station and call it station copy
      mean_climate_Prec_sim <- cbind(mean_climate_Prec_sim, mean_climate_Prec_sim)
      colnames(mean_climate_Prec_sim) <- c('station', 'station_copy')
      
      if (warn_me) 
        warning(paste("Absolute temperature scenario specified - calibration weather record only used for", 
                      "simulating temperature variation, but not for the means"), 
                call. = FALSE)
    }
    
    #at first run precipitation generator
    generated_prec <- modified_ComprehensivePrecipitationGenerator(station = c('station', 'station_copy'),
                                        prec_all=PREC,
                                        year_min=year_min,
                                        year_max=year_max,
                                        year_max_sim = year_max_sim,
                                        year_min_sim = year_min_sim,
                                        mean_climate_prec_sim = mean_climate_Prec_sim,
                                        p=p_prec,
                                        n_GPCA_iteration=n_GPCA_iter,
                                        n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                        sample="monthly",
                                        no_spline=FALSE)
    
    #still needed?
    origin <- paste(temperature_scenario[[ts]]$scenario_year,1,1,sep="-")
    
    #now I selected only the temperature between the first two simulated years, because the 
    #exogen needs to have same size as prep data generated by RMAWGEN
    exogen <- normalizeGaussian(x = PREC$station[which(PREC$year >= year_min & PREC$year <= year_max)],
                                data=PREC$station,sample = "monthly")
    exogen <- data.frame(station = exogen)
    
    exogen_sim <- normalizeGaussian(x = generated_prec$prec_gen$station,
                                        data = generated_prec$prec_gen$station ,sample = "monthly")
    exogen_sim <- data.frame(station = exogen_sim)
    

    generation00 <- RMAWGEN::ComprehensiveTemperatureGenerator(station = "station", 
                                                               Tx_all = TEMP_MAX, Tn_all = TEMP_MIN, year_min = year_min, 
                                                               year_max = year_max, p = p, n_GPCA_iteration = n_GPCA_iter, 
                                                               n_GPCA_iteration_residuals = n_GPCA_iteration_residuals, 
                                                               sample = "monthly", year_min_sim = year_min_sim, 
                                                               year_max_sim = year_max_sim, mean_climate_Tn = min_mean, 
                                                               mean_climate_Tn_sim = mean_climate_Tn_sim, mean_climate_Tx = max_mean, 
                                                               mean_climate_Tx_sim = mean_climate_Tx_sim, seed = seed,
                                                               exogen = exogen, exogen_sim = exogen_sim)
    
    sim <- cbind(generation00$output$Tn_gen, generation00$output$Tx_gen, generated_prec$prec_gen$station)
    colnames(sim) <- c("Tmin", "Tmax", 'Prec')
    
    dates <- make_all_day_table(data.frame(Year = c(year_min_sim, 
                                                    year_max_sim), Month = c(1, 12), Day = c(1, 31), 
                                           nodata = c(1, 2)), no_variable_check = TRUE)
    
    #changed from -6 to -5
    dates <- dates[, -5]
    simoutput[[ts]] <- cbind(dates, sim)
    
    #setTxtProgressBar(pb, ts)
  }
  names(simoutput) <- names(temperature_scenario)
  return(gen_temps = simoutput)
}

#temperature_generation_rmawgen_prec(weather = weather, years = years, sim_years = c(2001,2099),temperature_scenario = historic_temperature_scenarios)
