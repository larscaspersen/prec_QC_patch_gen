library(tidyverse)
source('07_notes_eike.R')

eval <- read.csv('data/CA-patch_eval_prec_only.csv')
prcp_threshold <- 1


#load functions
library(weatherImpute)
library(chillR)

#read weather data
weather <- load_temperature_scenarios('qc_results/', prefix = 'durre')

#drop also the columns which have a S or R in QC, on top of the other removed outlier
x <- weather[[1]]

weather <- purrr::map(weather, function(x){
  x$Tmin[x$QC_Tmin %in% c('R', 'S')] <- NA
  x$Tmax[x$QC_Tmax %in% c('R', 'S')] <- NA
  x$Precip[x$QC_Precip %in% c('R', 'S')] <- NA
  
  return(x)
})


#read gsod data
weather_gsod <- load_temperature_scenarios('qc_results_gsod/', prefix = 'durre')
#numbers in column names can cause trouble, add gsod before it
names(weather_gsod) <- paste0('gsod', 1:27)


#read meta-data
weather_info <- read.csv('downloaded_data/cimis_info.csv')
names
gsod_info <- read.csv('downloaded_data/gsod_target_weather_info.csv')
gsod_info$id <- paste0('gsod', 1:27)


##only do analysis for precipitation
precip <- purrr::map(weather, 'Precip') %>% 
  bind_cols()

precip_gsod <- purrr::map(weather_gsod, 'Precip') %>% 
  bind_cols()


#set precipitation below threshold to zero
for(station in weather_info$id){
  precip[(precip[,station] < prcp_threshold) & !is.na(precip[,station]) ,station] <- 0
}

for(station in gsod_info$id){
  precip_gsod[(precip_gsod[,station] < prcp_threshold) & !is.na(precip_gsod[,station]) ,station] <- 0
}


#bind gsod to tmin, tmax, precip 
precip <- cbind(precip, precip_gsod)




#####
#plot of daily imputation
#####

#subset only imputed data
eval_daily <- eval[eval$new_na,]

precip$Day <- weather[[1]]$Day
precip$Month <- weather[[1]]$Month
precip$Year <- weather[[1]]$Year
precip$Date <- weather[[1]]$Date#bring to long format

precip$doy <- lubridate::yday(precip$Date)





eval_daily_long <- reshape2::melt(eval_daily, 
                                  id.vars = c('Date', 'Year', 'Month', 'Day', 'station','original', 'new_na'),
                                  variable.name = 'patch_method')


eval_daily_long$value[eval_daily_long$value < prcp_threshold] <- 0
eval_daily_long$original[eval_daily_long$original < prcp_threshold] <- 0

sum(is.na(eval_daily_long$value))
sum(is.na(eval_daily_long$original))

#which method had nas?
eval_daily_long$station <- as.factor(eval_daily_long$station)
summary(eval_daily_long[is.na(eval_daily_long$value),])
#--> remove method 
#--> patch mean and wrapper nm3 had NAs

#fill holes for null model 3
missinfg_nm3 <- eval_daily_long %>% 
  filter(is.na(value) == TRUE & patch_method == 'wrapper_nm3') %>% 
  mutate(doy = lubridate::yday(Date)) 

#add row number
missinfg_nm3$rownr <- 1:nrow(missinfg_nm3)


for(station in unique(missinfg_nm3$station)){
  #station <- missinfg_nm3$station[1]
  
  #create dataframe with median etc
  prec_info <- prepare_null_model(weather = precip, target = station )
  
  sub <- subset(missinfg_nm3, missinfg_nm3$station == station)
  
  for(i in 1:nrow(sub)){
    sub$value[i] <- prec_info[prec_info$doy == sub$doy[i],]$amount
  }
  
  #fill the missing values
  missinfg_nm3$value[sub$rownr] <- sub$value
  
}

#now map these values back to the original data frame
for(i in 1:nrow(missinfg_nm3)){
  
  eval_daily_long[eval_daily_long$Date == missinfg_nm3$Date[i] &
                    eval_daily_long$station == missinfg_nm3$station[i] &
                    eval_daily_long$patch_method == 'wrapper_nm3','value'] <- missinfg_nm3$value[i]
  
}

#kick out patch mean because it had quite a number of NAN after gap filling
eval_daily_long <- dplyr::filter(eval_daily_long, patch_method != 'patch_mean')


#should save that stuff......


#make confusion matrix
classificaction_df_mean <- eval_daily_long %>% 
  group_by(station, patch_method) %>%
  mutate(tp = value > prcp_threshold & original > prcp_threshold,
         tn = value <= prcp_threshold & original <= prcp_threshold,
         fp = value > prcp_threshold & original <= prcp_threshold,
         fn = value <= prcp_threshold & original > prcp_threshold) %>% 
  summarise(tp = sum(tp), 
            tn = sum(tn),
            fp = sum(fp),
            fn = sum(fn),
            n = n()) %>% 
  group_by(patch_method) %>% 
  summarise(tp = mean(tp / n),
            tn = mean(tn / n),
            fp = mean(fp / n),
            fn = mean(fn / n)) %>% 
  reshape2::melt(id.vars = 'patch_method', value.name = 'mean')

classificaction_df_sd <- eval_daily_long %>% 
  group_by(station, patch_method) %>%
  mutate(tp = value > prcp_threshold & original > prcp_threshold,
         tn = value <= prcp_threshold & original <= prcp_threshold,
         fp = value > prcp_threshold & original <= prcp_threshold,
         fn = value <= prcp_threshold & original > prcp_threshold) %>% 
  summarise(tp = sum(tp), 
            tn = sum(tn),
            fp = sum(fp),
            fn = sum(fn),
            n = n()) %>% 
  group_by(patch_method) %>% 
  summarise(tp = sd(tp / n),
            tn = sd(tn / n),
            fp = sd(fp / n),
            fn = sd(fn / n)) %>% 
  reshape2::melt(id.vars = 'patch_method', value.name = 'sd') 

classificaction_df <- merge(classificaction_df_mean, classificaction_df_sd, 
                            by = c('patch_method', 'variable'))

rm(classificaction_df_mean, classificaction_df_sd)



classificaction_df %>% 
  filter(!patch_method %in% c('wrapper_nm1', 'wrapper_nm2', 'wrapper_nm3')) %>% 
  ggplot(aes(x = variable, y = mean, fill = patch_method)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar( aes(x=variable, ymin=mean-sd, ymax=mean+sd), 
                 position = position_dodge(0.9), width = .3) +
  theme_bw()




#calculate mcc and f1 score
classificaction_df <- eval_daily_long %>% 
  filter(!patch_method %in% c('wrapper_nm1', 'wrapper_nm2', 'wrapper_nm3')) %>% 
  group_by(station, patch_method) %>%
  mutate(tp = value > prcp_threshold & original > prcp_threshold,
         tn = value <= prcp_threshold & original <= prcp_threshold,
         fp = value > prcp_threshold & original <= prcp_threshold,
         fn = value <= prcp_threshold & original > prcp_threshold) %>% 
  summarise(tp = sum(tp) / n(), 
            tn = sum(tn) / n(),
            fp = sum(fp) / n(),
            fn = sum(fn) / n()) %>% 
  group_by(station, patch_method) %>% 
  summarise(f1 = (2 * tp) / (2 * tp + fp + fn),
            mcc = ((tp*tn) - (fp*fn)) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))) %>% 
  na.omit()

classificaction_df %>% 
  reshape2::melt(id.vars = c('station', 'patch_method')) %>% 
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin()+
  facet_wrap(~variable)+
  theme_bw()

classificaction_df_mean <- classificaction_df %>% 
  group_by(patch_method) %>% 
  summarise(f1 = mean(f1),
            mcc = mean(mcc)) %>% 
  reshape2::melt(id.vars = 'patch_method', value.name = 'mean')

classificaction_df_sd <- classificaction_df %>% 
  group_by(patch_method) %>% 
  summarise(f1 = sd(f1),
            mcc = sd(mcc)) %>% 
  reshape2::melt(id.vars = 'patch_method', value.name = 'sd')


classificaction_df <- merge(classificaction_df_mean, classificaction_df_sd, 
                            by = c('patch_method', 'variable'))

rm(classificaction_df_mean, classificaction_df_sd)

classificaction_df %>% 
  ggplot(aes(x = variable, y = mean, fill = patch_method)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar( aes(x=variable, ymin=mean-sd, ymax=mean+sd), 
                 position = position_dodge(0.9), width = .3) +
  theme_bw()

#hardly any difference between f1 and mcc




###############
#















