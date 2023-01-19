source('code/custom_chillR_functions/download_cmip6.R')
library(tidyverse)

test <- get_scenarioMIP_data(coordinates = data.frame('Longitude' = 6.99,
                                        'Latitude' = 50.6,
                                        'id' = 'CKA'),
                     start_year = 2015,
                     end_year = 2100, 
                     metric = 'pr', 
                     experiment = 'ssp585', 
                     keep_downloaded = TRUE,
                     frequency = 'mon') 



weather <- read.csv('../s08_wetterdaten_cka/daten/weather_daily_cka_2011-2022.csv')

pr_summarized <- weather %>% 
  group_by(Year, Month) %>% 
  summarize(pr = mean(Precipitation_mm)) %>% 
  filter(Year %in% 2015:2021)

pr_summarized$model <- 'measured'
pr_summarized$ssp <- 'measured'
pr_summarized$variable <- 'pr'
pr_summarized$Date <- lubridate::ym(paste(pr_summarized$Year, pr_summarized$Month))

colnames(test)[5] <- 'pr'

test$Month <- lubridate::month(test$Date)
test$Year <- lubridate::year(test$Date)



weather_combined <- rbind(test, pr_summarized)

weather_combined %>% 
  ggplot(aes(x = model, y = pr)) + 
  geom_violin(aes(fill = ssp)) + 
  theme_bw() +
  scale_fill_manual(breaks = c('measured', 'ssp585'),values = c('Firebrick', 'Steelblue'))+
  facet_wrap(~Month) +
  ylab('Mean Daily Precipitation (mm/day)\nfor period 2015 - 2021')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave('compare_precipitation_CMIP6.jpeg', height = 10, width = 20, units = 'cm',
       device = 'jpeg')
