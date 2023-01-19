custom_temperature_scenario_from_records <- function (weather, year, weather_start = NA, weather_end = NA, 
          scen_type = "running_mean", runn_mean = 15, prec_output = 'monthly_mean_daily_prec') 
{
  if (!is.data.frame(weather)) 
    return(warning("Error - weather object is not a data.frame"))
  if (!"Tmin" %in% colnames(weather) | !"Tmax" %in% colnames(weather) | 
      !"Day" %in% colnames(weather) | !"Month" %in% colnames(weather) | 
      !"Year" %in% colnames(weather)) 
    return(warning("Error - required columns missing ('Day','Month','Year','Tmin' or 'Tmax')"))
  if (is.na(weather_start)) 
    weather_start <- min(weather$Year)
  if (is.na(weather_end)) 
    weather_end <- max(weather$Year)
  if (weather_start == weather_end) 
    return(warning("Error - only one data year selected"))
  if (weather_start > weather_end) 
    return(warning("Error - End year before start year"))
  weather <- weather[which(weather$Year >= weather_start), 
  ]
  weather <- weather[which(weather$Year <= weather_end), ]
  if (nrow(weather) == 0) 
    return(warning("Error - no weather records in specified interval"))
  
  #add calculation of total precipitation
  past_means <- list()
  past_means[["Tmin"]] <- aggregate(weather$Tmin, by = list(weather$Year, 
                                                            weather$Month), FUN = "mean")
  past_means[["Tmax"]] <- aggregate(weather$Tmax, by = list(weather$Year, 
                                                            weather$Month), FUN = "mean")
  
  if(prec_output == 'monthly_mean_daily_prec'){
      past_means[['Prec']] <- aggregate(weather$Prec, by = list(weather$Year, 
                                                                weather$Month), FUN = "mean")
    
  } else if (prec_output == 'monthly_sum_prec'){
    
      past_means[['Prec']] <- aggregate(weather$Prec, by = list(weather$Year,
                                                              weather$Month), FUN = 'sum')
    
  }else if (prec_output == 'number_wet_days'){
    
    get_number_wet_days <- function(x, threshold = 0.2){
      return(sum(x > threshold))
    }
    
    past_means[['Prec']] <- aggregate(weather$Prec, by = list(weather$Year,
                                                              weather$Month), FUN = 'get_number_wet_days')
    
  } else {
    stop("No valid summarization option for Precipitation provided. Must be either
          'monthly_mean_daily_prec' or 'monthly_sum_prec'")
  }
  
  

  colnames(past_means[["Tmin"]]) <- c("Year", "Month", "Temp")
  colnames(past_means[["Tmax"]]) <- c("Year", "Month", "Temp")
  colnames(past_means[["Prec"]]) <- c("Year", "Month", "Temp")
  
  baseclim_baseline_duration_adjustment <- data.frame(Tmin = rep(NA,12),
                                                      Tmax = rep(NA, 12),
                                                      Prec = rep(NA, 12))
  
  rownames(baseclim_baseline_duration_adjustment) <- 1:12
  out_scenarios <- list()
  for (y in 1:length(year)) {
    for (v in c("Tmin", "Tmax", 'Prec')) {
      for (i in 1:12) {
        monthly <- past_means[[v]][which(past_means[[v]]$Month == 
                                           i), ]
        if (scen_type == "regression") {
          model <- lm(monthly$Temp ~ monthly$Year)
          baseclim_baseline_duration_adjustment[i, v] <- as.numeric(model$coefficients[1] + 
                                                                      model$coefficients[2] * year[y])
          if (is.na(baseclim_baseline_duration_adjustment[i, 
                                                          v])) 
            return(warning(paste("Error - unable to calculate regression for", 
                                 v, "in month", i)))
        }
        if (scen_type == "running_mean") {
          baseclim_baseline_duration_adjustment[i, v] <- runn_mean_pred(indep = monthly$Year, 
                                                                        dep = monthly$Temp, pred = year[y], runn_mean = runn_mean)$predicted
          if (is.na(baseclim_baseline_duration_adjustment[i, 
                                                          v])) 
            return(warning(paste("Error - cannot calculate value for", 
                                 v, "in month", i)))
        }
      }
    }
    out_scenarios[[y]] <- list(data = baseclim_baseline_duration_adjustment, 
                               scenario_year = year[y], reference_year = NA, scenario_type = "absolute", 
                               labels = "regression-based scenario")
    if (scen_type == "running_mean") 
      out_scenarios[[y]]$labels <- "running mean scenario"
  }
  names(out_scenarios) <- as.character(year)
  return(out_scenarios)
}
