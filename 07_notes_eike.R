#comments eike:
#-have a null modell which only does a coin flip for precipitation and
#average precipitation for that month
#both based on the month
#maybe use a 15 day window centered on each day
#calculate average chance for precipitation and average precipitation sum if it rains
#compare null-model against other models

#evaluation of precipitation: have several criteria:
#a) detection of precipitation event
#b) precipitation sum in case correctly identified
#c) precipitation sum in case it did not correctly identify event

#another idea Eike: use temperature data and transition
#chances to patch precipitation:
#maybe markov chains for detection of precipitation
#in case high amplitude then it probably clear sky
#too types of information: amplitude of current day and 
#if it rained the day before?

library(chillR)
library(tidyverse)
# weather <- load_temperature_scenarios(path = 'qc_results/',
#                                       prefix = 'costa')
# 
# #null model
# weather <- weather[[1]]
# #bring data in right format
# weather <- weather[,c('doy', 'Date', 'Year', 'Month', 'Day', 'Precip')]
# colnames(weather)[6] <- 'id_1'

get_prec_data <- function(weather, target, doy, prcp_threshold = 1){
  #get doys of target days
  lim_doy <- doy + c(-7,7)
  
  #adjust for lower lims in old year
  lim_doy <- ifelse(lim_doy <= 0, yes = lim_doy + 365,no = lim_doy)
  
  #adjsut for upper lims in new year
  lim_doy <- ifelse(lim_doy >= 365, yes = lim_doy - 365, no = lim_doy)
  
  
  #if the range of days falls inbetween two years, than different subsetting needed
  if(lim_doy[1] > lim_doy[2]){
    
    target_days <- weather$doy >= lim_doy[1] | weather$doy <= lim_doy[2]
    
  } else {
    
    target_days <- weather$doy >= lim_doy[1] & weather$doy <= lim_doy[2]
  }
  
  #take non zero precipitation data from the 29 window over all years, 
  #make it a empirical cumulative distribution function, then determine 
  #the percentile of the value
  
  #extract target days, drop nas
  precip <- weather[target_days, target] %>%
    stats::na.omit()
  
  #calculate chance for preciptiation
  chance_rain <- sum(precip >= prcp_threshold) / length(precip)
  #if rain, get median precipitation value
  
  #only concider values above the precipitation threshold
  precip <- precip[precip >= prcp_threshold]
  
  med_precip <- median(precip)
  
  #if there are less than 10 observations for the day of the year, then don't use the 
  #model fitting
  if(length(precip) <= 10){
    rate <- NA
    shape <- NA
  } else {
    #fit gamma model to precipitation values larger than precipiation_threshold
    m_estimates <- MASS::fitdistr(precip,"gamma")$estimate
    
    shape <- m_estimates[[1]]
    rate <- m_estimates[[2]]
  }
  
  
  return(data.frame(p = chance_rain, amount = med_precip,
                    shape = shape, rate = rate))
  
}

#get for each day of the year the chance of rain,
#take 15 day window
prepare_null_model <- function(weather, target){
  
  #incase there is no doy in weather, add it
  if('doy' %in% colnames(weather) == FALSE){
    #in case no date in weather add it too
    if('Date' %in% colnames(weather) == FALSE){
      weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                              format = '%Y-%m-%d')
    }
    weather$doy <- lubridate::yday(weather$Date)
  }
  
  #iterate over all doys, calculate the requested quantiles
  prec_list <- purrr::map(unique(weather$doy), function(x){
    #print(x)
    get_prec_data(weather = weather, target = target, doy = x)}) %>% 
    bind_rows() %>% 
    mutate(doy = 1:366)
  
  return(prec_list)
}


# precipitation occurence and median precipitation amount for the whole observation period
prec_null_model_1 <-function(prec_chance_amount, median_precipitation){
  
  occurence <- rbinom(size = 1, n=1, prob = prec_chance_amount)
  
  if(occurence == 1){
    amount <- median_precipitation
  } else{
    amount <- 0
  }
   return(amount)
  
}

# use occurence and amount for 15 day windows for each day of the year
prec_null_model_2 <-function(doy, prec_chance_amount){
  
  occurence <- rbinom(size = 1, n=1, prob = prec_chance_amount[prec_chance_amount$doy == doy, ]$p)
  
  if(occurence == 1){
    amount <- prec_chance_amount[prec_chance_amount$doy == doy,]$amount
  } else{
    amount <- 0
  }
  return(amount)
  
}

# use occurence and random amount of gamma distribution for each day of the year, using 15 day window
prec_null_model_3 <-function(doy, prec_chance_amount){
  
  occurence <- rbinom(size = 1, n=1, prob = prec_chance_amount[prec_chance_amount$doy == doy, ]$p)
  
  if(occurence == 1){
    amount <- pgamma(runif(1), 
                     shape =  prec_chance_amount[prec_chance_amount$doy == doy,]$shape,
                     rate = prec_chance_amount[prec_chance_amount$doy == doy,]$rate)
  } else{
    amount <- 0
  }
  return(amount)
  
}


wrapper_nm1 <- function(weather, target, weather_info = NULL, prcp_threshold = 1){
  
  precip <- na.omit(weather[,target])
  
  #calculate median and chance for occurence
  med_prec <- median(precip, na.rm = TRUE)
  occurence <- length(precip[precip > prcp_threshold]) / length(precip)
  
  return(ifelse(is.na(weather[,target]), 
         yes = prec_null_model_1(prec_chance_amount = occurence, 
                                 median_precipitation = med_prec), 
         no = weather[,target]))
  
}



wrapper_nm2 <- function(weather, target, weather_info = NULL, prcp_threshold = 1){
  
  #add doy if missing
  if("doy" %in% colnames(weather) == FALSE){
    weather$doy <- lubridate::yday(weather$Date)
  }
  
  #get info on occurence and median precip for each day
  #create data.frame with chances 
  prec_chance_amount <- prepare_null_model(weather, target = target)
  
  return(purrr::map2_dbl(weather$doy, weather[,target], function(i,x){
    if(is.na(x)){
      prec_null_model_2(doy = i, prec_chance_amount = prec_chance_amount)
    } else {
      x
    }
  })
  )
}


wrapper_nm3 <- function(weather, target, weather_info = NULL, prcp_threshold = 1){
  
  #add doy if missing
  if("doy" %in% colnames(weather) == FALSE){
    weather$doy <- lubridate::yday(weather$Date)
  }
  
  #get info on occurence and median precip for each day
  #create data.frame with chances 
  prec_chance_amount <- prepare_null_model(weather, target = target)
  
  #iterate over each day, check if needs to be patched
  #if so, run null model
  #in case null model returns an NA, use median prec
  out <- purrr::map2_dbl(weather$doy, weather[,target], function(i,x){
    patched <- if(is.na(x)){
      patched <- prec_null_model_3(doy = i, prec_chance_amount = prec_chance_amount)
      
      if(is.na(patched)){
        patched <- prec_chance_amount[prec_chance_amount$doy == i,]$amount
      }
      
    } else {
      patched <- x
    }
    
    return(patched)
    
  })
  
  return(out)
}

# #all wrappers work :)
# wrapper_nm1(weather = weather, target = 'id_1')
# wrapper_nm2(weather = weather, target = 'id_1')
# wrapper_nm3(weather = weather, target = 'id_1')




