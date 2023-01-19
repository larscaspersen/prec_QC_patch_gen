library(tidyverse)
library(chillR)
weather_info <- read.csv('data/CA_weather_info.csv')
#read original weather

org_weather <- load_temperature_scenarios('data/weather/', prefix = 'CA_temp_rain')
names(org_weather) <- purrr::map_chr(org_weather, function(x) x$id[1])


names(org_weather) <- weather_info$id



#read simulated weather
dirs <- list.dirs('data/future_simulated_weather/', full.names = F) %>% 
  .[-1]

weather_list <- purrr::map(dirs, function(x){
  fpath <- paste0('data/future_simulated_weather/', x, '/')
  f <- list.files(path = fpath)
  
  f_full <- paste0(fpath, f)
  
  weather_list <- purrr::map(f_full, read.csv)
  
  names(weather_list) <- f
  
  #set precipitation values lower than 0
  weather_list <- purrr::map(weather_list, function(x){
    x$Prec[x$Prec < 0] <- 0
    return(x)
  })
  
  return(weather_list)
})

#adjust names
names(weather_list) <- gsub(' ', '', dirs)


#idea: fit the SPEI with historic data and do the calculations for the simulated ones
x <- org_weather[[2]]
station <- names(org_weather)[2]
y <- weather_list[[station]][[12]]

#calculate balance for observed weather
x_monthly <- x %>% 
  group_by(Year, Month) %>%
  summarise(Tmax = mean(Tmax), 
            Tmin = mean(Tmin),
            Prec = sum(Prec)) %>% 
  group_by(Year) %>% 
  mutate(PET = as.vector(SPEI::hargreaves(Tmin = Tmin,
                                          Tmax =  Tmax, 
                                          lat = weather_info$Latitude[weather_info$id == station],
                                          Pre = Prec))) %>% 
  mutate(balance = Prec - PET)

#calculate balance for simulated weather
y_monthly <- y %>% 
  group_by(Year, Month) %>%
  summarise(Tmax = mean(Tmax), 
            Tmin = mean(Tmin),
            Prec = sum(Prec)) %>% 
  group_by(Year) %>% 
  mutate(PET = as.vector(SPEI::hargreaves(Tmin = Tmin,
                                          Tmax =  Tmax, 
                                          lat = weather_info$Latitude[weather_info$id == station],
                                          Pre = Prec))) %>% 
  mutate(balance = Prec - PET)


length(x_monthly$balance)
#at 385. stop reference period

#combine observed and simulated balance
balance <- ts(c(x_monthly$balance, y_monthly$balance), start = c(1990,1), frequency = 12)

plot(balance)
#calculate the spei for observed and simulated precipitation, but only use the observed weather for the fitting of the cumulative density function
spei_out <- SPEI::spei(data = balance, scale = 3, ref.start = c(1990,1), ref.end = c(2021,12))



plot(spei_out)
spei_out$fitted[1:12]
#--> future weather is much, much wetter... did I mix up the weather data? or is the agreement just very low?




#compare precipitation of the models with observed precipitation
extratced_cmip_weather <- read.csv('data/cmip6_point_extracted.csv')

cf <- c()
for(station in names(weather_list)){
  #we have matching years of org weather and cmip weather 2015 - 2021
  monthly_org <- org_weather[[station]] %>% 
    group_by(Year, Month) %>% 
    summarise(Tmin = mean(Tmin),
              Tmax = mean(Tmax),
              Prec = mean(Prec)) %>% 
    filter(Year %in% 2015:2021)
  
  monthly_cmip6 <- extratced_cmip_weather %>% 
    reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'station') %>% 
    reshape2::dcast(Date + model + ssp + station ~ variable, value.var = 'value') %>% 
    filter(station == station) %>% 
    mutate(Year = lubridate::year(Date),
           Month = lubridate::month(Date)) %>% 
    filter(Year %in% 2015:2021)
  
  test_merge <- merge(monthly_org, monthly_cmip6, by = c('Year', 'Month'))
  
  #calculate mean bias per month
  # monthly_bias <- test_merge %>% 
  #   group_by(Month, model) %>%
  #   summarise(correction_factor = median(Prec / pr)) 
  
  #calculate mean bias per month
  cor_factor <- test_merge %>% 
    group_by(Month, model) %>%
    summarise(correction_factor = median(Prec / pr)) %>% 
    ungroup() %>% 
    summarise(med = median(correction_factor, na.rm = TRUE)) %>% 
    pull(med)
  
  
  cf <- c(cf, cor_factor)
}

correction_factor <- median(cf)

#see if the correctionf factor leads to okay results
monthly_org <- purrr::map(org_weather, function(x){
    x %>% 
      group_by(Year, Month) %>% 
      summarise(Tmin = mean(Tmin),
                Tmax = mean(Tmax),
                Prec = mean(Prec)) %>% 
      filter(Year %in% 2015:2021)
      
  }) %>% 
  bind_rows(.id = 'station')

monthly_cmip6 <- extratced_cmip_weather %>% 
  reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'station') %>% 
  reshape2::dcast(Date + model + ssp + station ~ variable, value.var = 'value') %>% 
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date)) %>% 
  filter(Year %in% 2015:2021)

test_merge <- merge(monthly_org, monthly_cmip6, by = c('Year', 'Month', 'station'))

test_merge$pr_corrected <- test_merge$pr * correction_factor

test_merge %>% 
  reshape2::melt(id.vars = c('Month', 'Year', 'model', 'ssp', 'station', 'Date')) %>% 
  #filter(variable %in% c('Prec', 'pr_corrected', 'pr')) %>% 
  filter(variable %in% c('Prec', 'pr_corrected')) %>% 
  ggplot(aes(x = as.factor(Month), y = value, fill = variable)) + 
  geom_boxplot() +
  facet_wrap(~station)

out <- test_merge %>% 
  group_by(station) %>% 
  summarise(test_val =  ks.test(x = Prec, y = pr_corrected)$p.value)
#they are all very different... :(
#what should I do?

#how should I handle the pr? 
#ask Eike and the others during dormancy meeting






out <- ks.test(x = test_merge$Prec, y = test_merge$pr_corrected)
out$p.value

ggplot(mtcars, aes(sample = mpg, colour = factor(cyl))) +
  stat_qq() +
  stat_qq_line()



hist(test_merge$Prec - test_merge$pr_corrected)
hist(test_merge$Prec - test_merge$pr)


test_merge %>% 
  filter(correction_factor > 0) %>% 
  summarise(cf = median(correction_factor))
#--> use a correction factor of roughly 0.13

test_merge$pr_corrected <- test_merge$pr * 0.13

# test_merge$cf <- test_merge$Prec / test_merge$pr
# 
# cf_df <- test_merge %>% 
#   group_by(Month, model) %>% 
#   summarise(cor_f = mean(cf))


test_merge %>% 
  #filter(ssp == 'ssp126') %>% 
  ggplot(aes(x = Prec, y = pr)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  facet_grid(model ~ Month)

test_merge %>% 
  #filter(ssp == 'ssp126') %>% 
  ggplot(aes(x = Prec, y = pr_corrected)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  facet_grid(model ~ Month)

boxplot(monthly_bias$correction_factor ~ monthly_bias$Month)

ts(x_monthly$balance, start = c(1990,1), frequency = 12)



#calculate chill, precipitation sum, drought index?
library(chillR)

#need a modified version of tempResponse_daily_list() function


x<- weather_list[[1]][[1]]
id <- names(weather_list)[1]

x %>% 
  group_by(Year) %>% 
  summarise(prec_sum = sum(Prec))

x %>% 
  group_by(Year) %>% 
  filter(Month %in% 4:10) %>% 
  summarise(prec_sum_growing_season = sum(Prec))


#need a longer reference period...
#fitting for each simulated year won't work...
sei_out <- x %>% 
  group_by(Year, Month) %>%
  summarise(Tmax = mean(Tmax), 
            Tmin = mean(Tmin),
            Prec = sum(Prec)) %>% 
  group_by(Year) %>% 
  mutate(PET = as.vector(SPEI::hargreaves(Tmin = Tmin,
                                Tmax =  Tmax, 
                                lat = weather_info$Latitude[weather_info$id == id],
                                Pre = Prec))) %>% 
  mutate(balance = Prec - PET) %>% 
  purrr::pluck('balance') %>% 
  SPEI::spei(scale = 18)
  # group_by(Year) %>% 
  # group_split() %>% 
  # purrr::map(function(y) SPEI::spei(y$balance, scale = 3))

plot(sei_out)

SPEI::spei(y$balance, scale = 1)

  
  group_by(Year) %>% 
  purrr::pluck('balance') %>% 
  SPEI::spei(scale = 3) %>% 
  as.vector()

#how to summarize the SPEI?

#actually we have only 100 times one year observations, so how can I summarize the SPEI?
plot(sei_out)
#maybe count number of months with SPEI lower than 1 for all the years?




x_sum <-x %>% 
  group_by(Year, Month) %>%
  summarise(Tmax = mean(Tmax), 
            Tmin = mean(Tmin),
            Prec = sum(Prec))



as.vector(SPEI::hargreaves(Tmin = x_sum$Tmin,
                 Tmax =  x_sum$Tmax, 
                 lat = weather_info$Latitude[weather_info$id == id],
                 Pre = x_sum$Prec))


#function to make chunks out of vector
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

library(chillR)
#use this function for the chill calculation
tempResponse_daily_list()


weather_sum <- purrr::map(weather_list, function(x){
  purrr::map(x, function(y){
    #y <- weather_list[[1]][[1]]
    #calculate precipitation sums 
    prec_year <- y %>% 
      group_by(Year) %>% 
      summarise(prec_sum = round(sum(Prec), digits = 2))
    
    prec_season <- y %>% 
      group_by(Year) %>% 
      filter(Month %in% 4:10) %>% 
      summarise(prec_sum_growing_season = round(sum(Prec), digits = 2))
    
    prec_df <- merge(prec_year, prec_season, by = 'Year')
    
    
    sei_out <- y %>% 
      group_by(Year, Month) %>%
      summarise(Tmax = mean(Tmax), 
                Tmin = mean(Tmin),
                Prec = sum(Prec)) %>% 
      group_by(Year) %>% 
      mutate(PET = as.vector(SPEI::hargreaves(Tmin = Tmin,
                                              Tmax =  Tmax, 
                                              lat = weather_info$Latitude[weather_info$id == id],
                                              Pre = Prec))) %>% 
      mutate(balance = Prec - PET) %>% 
      purrr::pluck('balance') %>% 
      SPEI::spei(scale = 3) %>% 
      purrr::pluck('fitted') %>% 
      as.vector()
    
    prec_df$median_spei <- purrr::map_dbl(chunk2(sei_out, n = 100), function(i){
      round(median(i, na.rm = T), digits = 2)})
    
    return(prec_df)
    
  })
})






#do the same calculations for the historic simulation data
weather_hist <- load_temperature_scenarios('data/historical_simulated_weather/', prefix = 'hist_sim')
weather_hist <- purrr::map(weather_hist, function(x){
  df1990 <- x[,1:7]
  df2000 <- x[,8:14]
  df2010 <- x[,15:21]
  df2020 <- x[,22:28]
  
  colnames(df1990) <- colnames(df2000) <- colnames(df2010) <- colnames(df2020) <- c('DATE', 'Year', 'Month', 'Day', 'Tmin', 'Tmax', 'Prec')
  list('1990' = df1990, '2000' = df2000, '2010' = df2010, '2020' = df2020)
})
#weather_hist is in same order as weather
names(weather_hist) <- names(org_weather)

x <- org_weather[[1]]
station <- names(org_weather)[1]
x_hist <- weather_hist[[station]][[4]]

x_monthly <- x %>% 
  group_by(Year, Month) %>%
  summarise(Tmax = mean(Tmax), 
            Tmin = mean(Tmin),
            Prec = sum(Prec)) %>% 
  group_by(Year) %>% 
  mutate(PET = as.vector(SPEI::hargreaves(Tmin = Tmin,
                                          Tmax =  Tmax, 
                                          lat = weather_info$Latitude[weather_info$id == id],
                                          Pre = Prec))) %>% 
  mutate(balance = Prec - PET)

x_hist_monthly <- x_hist %>% 
  group_by(Year, Month) %>%
  summarise(Tmax = mean(Tmax), 
            Tmin = mean(Tmin),
            Prec = sum(Prec)) %>% 
  group_by(Year) %>% 
  mutate(PET = as.vector(SPEI::hargreaves(Tmin = Tmin,
                                          Tmax =  Tmax, 
                                          lat = weather_info$Latitude[weather_info$id == id],
                                          Pre = Prec))) %>% 
  mutate(balance = Prec - PET)


balance <- ts(c(x_monthly$balance, x_hist_monthly$balance), start = c(1990,1), frequency = 12)

plot(balance)
spei_out <- SPEI::spei(data = balance, scale = 3, ref.start = c(1990,1), ref.end = c(2021,12))

plot(spei_out)

