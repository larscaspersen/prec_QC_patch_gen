library(RGENERATEPREC) #dry_spell calculation
library(tidyverse)

#load data
eval <- read.csv('data/CA-patch_eval_prec_only.csv')
prcp_threshold <- 1

eval_long <- reshape2::melt(eval, 
                            id.vars = c('Date', 'Year', 'Month', 'Day', 'station','original', 'new_na'),
                            variable.name = 'patch_method')

#make sure precipitatio below the treshold is treated as zero
eval_long$original[eval_long$original < prcp_threshold] <- 0
eval_long$value[eval_long$value < prcp_threshold] <- 0


#######
#Dryspells
#######

#add season to the eval_long
eval_long[eval_long$Month %in% c(12,1,2), 'season'] <- '1-DJF'
eval_long[eval_long$Month %in% c(3:5), 'season'] <- '2-MAM'
eval_long[eval_long$Month %in% c(6:8), 'season'] <- '3-JJA'
eval_long[eval_long$Month %in% c(9:11), 'season'] <- '4-SON'



#calculate dry spell of observed and predicted data
spell_sim <- eval_long %>%
  group_by(patch_method, station, Year, season) %>%
  summarise( dry_spell_obs = dw.spell(original),
             dry_spell_sim = dw.spell(value))

#result is dataframes within data frame
#--> extract and summarise the spells

#raw spell output
wetspell_sim <- data.frame(NULL)
dryspell_sim <- data.frame(NULL)

wetspell_obs <- data.frame(NULL)
dryspell_obs <- data.frame(NULL)

#spell output summarised by incidences of spell length
wetspell_sim_sum <- data.frame(NULL)
dryspell_sim_sum <- data.frame(NULL)

wetspell_obs_sum <- data.frame(NULL)
dryspell_obs_sum <- data.frame(NULL)

#somehow it sets the Year always to 1961 and the month always to 1
#add info manually

#probably there is some apply function doing the job, but I cant figure out how so I use a for loop instead
for(i in 1:nrow(spell_sim)){
  spell_sim[i,]$dry_spell_obs[[1]]$season <- spell_sim[i,]$season
  spell_sim[i,]$dry_spell_obs[[1]]$year <- spell_sim[i,]$Year
  
  spell_sim[i,]$dry_spell_sim[[1]]$season <- spell_sim[i,]$season
  spell_sim[i,]$dry_spell_sim[[1]]$year <- spell_sim[i,]$Year
}


#iterate over all the models
for(model in levels(as.factor(spell_sim$patch_method))){
  #iterate over the simulated years
  for(code in levels(as.factor(spell_sim$station))){
    
    #extract dataframe from list which fits model and code
    #just take output of same model and reference year
    sub_sim <- spell_sim$dry_spell_sim[which(spell_sim$patch_method == model & spell_sim$station == code)]
    sub_obs <- spell_sim$dry_spell_obs[which(spell_sim$patch_method == model & spell_sim$station == code)]
    
    #get rid of dates in the dataframes because bindrows cant handle it
    sub_sim <- lapply(sub_sim, function(x) x[,!(colnames(x) %in% c('end_date', 'start_date'))] )
    sub_obs <- lapply(sub_obs, function(x) x[,!(colnames(x) %in% c('end_date', 'start_date'))] )
    
    #bind all the list by rows
    sub_sim <- bind_rows(sub_sim)
    sub_obs <- bind_rows(sub_obs)
    
    for(state in c('dry', 'wet')){
      
      sub_sim_state <- filter(sub_sim, spell_state == state)
      sub_obs_state <- filter(sub_obs, spell_state == state)
      
      
      sub_obs_state$model <- model
      sub_sim_state$model <- model
      
      sub_obs_state$code <- code
      sub_sim_state$code <- code
      

      #condense information by counting the incidences per dryspell length
      sub_obs_sum <- count(group_by(.data = sub_obs_state, spell_length, season))
      sub_sim_sum <- count(group_by(.data = sub_sim_state, spell_length, season))
      
      #add info of reference year and model to it
      sub_obs_sum$patch_method <- model
      sub_obs_sum$station <- code
      
      sub_sim_sum$patch_method <- model
      sub_sim_sum$station <- code
      
      #store observation in final container
      if(state == 'dry'){
        dryspell_obs_sum <- rbind(dryspell_obs_sum, sub_obs_sum)
        dryspell_sim_sum <- rbind(dryspell_sim_sum, sub_sim_sum)
        
        dryspell_obs <- rbind(dryspell_obs, sub_obs_state)
        dryspell_sim <- rbind(dryspell_sim, sub_sim_state)
        
        
      } else if(state == 'wet'){
        wetspell_obs_sum <- rbind(wetspell_obs_sum, sub_obs_sum)
        wetspell_sim_sum <- rbind(wetspell_sim_sum, sub_sim_sum)
        
        wetspell_obs <- rbind(wetspell_obs, sub_obs_state)
        wetspell_sim <- rbind(wetspell_sim, sub_sim_state)
      }
      
      
    }
    
  }
}
#container were everything should be stored

#match observed and predicted dryspell lenghts for each station
#match by station and spell length


dryspell_sum <- merge.data.frame(dryspell_sim_sum, dryspell_obs_sum, 
                                 by = c('spell_length', 'station', 'season', 'patch_method'),
                                 suffixes = c('_sim', '_obs'))

ggplot(dryspell_sum, aes(x = n_obs, y = n_sim)) + 
  geom_jitter(alpha = 0.1) + 
  geom_abline(slope = 1, linetype = 'dashed')+ 
  ylab('Predicted instances of dryspell length groups')+
  xlab('Observed instances of dryspell length groups')+
  theme_bw(base_size = 18)+
  facet_grid(patch_method~season)
#--> this graph is difficult to interprete. we don't know which bubble shows which daylength
#putting color in aesthics for spell length doesn't help either
ggsave('figures/eval_precipitation/patching_qqplot-dryspell.jpeg',
       height = 18, width = 24, units = 'cm')


#make a density plot for dryspell length

#add dryspell_obs to dryspell_sim, but only one time as a further model variable
dry_sub <- dryspell_obs %>% 
  filter(model == 'patch_climatol') %>% 
  mutate(model = 'observed')

dryspell_density <- rbind(dry_sub, dryspell_sim)

dryspell_density %>%
  filter(!model %in% c('patch_mean', 'wrapper_nm1', 'wrapper_nm2', 'wrapper_nm3')) %>%
  ggplot(aes(x = spell_length, col = model)) + 
  stat_ecdf(geom = "point", size = 2) +
  stat_ecdf(data = dryspell_density[dryspell_density$model == 'observed',], 
            geom = "point", size = 2, aes(x = spell_length)) +
  #coord_trans(x="log2") +
  xlab('Dryspell length (days)') + 
  ylab('Cumulative Density') +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  theme_bw(base_size = 15) + 
  facet_grid(~season)
ggsave('figures/eval_precipitation/patching_density-dryspell.jpeg',
       height = 18, width = 24, units = 'cm')


#same for wetspell length
wet_sub <- wetspell_obs %>% 
  filter(model == 'patch_climatol') %>% 
  mutate(model = 'observed')

wetspell_density <- rbind(wet_sub, wetspell_sim)

wetspell_density %>%
  filter(!model %in% c('patch_mean', 'wrapper_nm1', 'wrapper_nm2', 'wrapper_nm3')) %>%
  ggplot(aes(x = spell_length, col = model)) + 
  stat_ecdf(geom = "point", size = 2) +
  stat_ecdf(data = wetspell_density[wetspell_density$model == 'observed',], 
            geom = "point", size = 2, aes(x = spell_length)) +
  #coord_trans(x="log2") +
  xlab('Wetspell length (days)') + 
  ylab('Cumulative Density') +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  theme_bw(base_size = 15) + 
  facet_grid(~season)
ggsave('figures/eval_precipitation/patching_density-wetspell.jpeg',
       height = 18, width = 24, units = 'cm')
#wetspell length weniger aussagekräftig

#--> scheint als würde zB amelia mehr von wechselhaftem wetter ausgehen, denn in der 
#niederschlagsmenge schlägt sich die schlechtere performance nicht nieder



#make confusion matrices














# UCIPM_WS_90 <- read.csv('data/fixed_temps/weather_stations_UCIPM.csv')
# 
# # #iterate over each stations 
# # for(i in 1:nrow(UCIPM_WS_90)){
# #   
# #   code <- UCIPM_WS_90$chillR_code[i]
# #   location <- UCIPM_WS_90$Name[i]
# #   
# #   p <-ggplot(dryspell_sum[dryspell_sum$code == code,], aes(x = count_obs, y = count_sim)) + 
# #     geom_jitter() + 
# #     geom_abline(slope = 1, linetype = 'dashed')+ 
# #     ylab('Predicted instances of dryspell length groups')+
# #     xlab('Observed instances of dryspell length groups')+
# #     ggtitle(paste(i, location, sep = ' '))+
# #     facet_grid(model~season)
# #   
# #   fname <- paste0('figures/compare_patching/evaluate_patching/dryspell/',i,'_',location,'_qqplot-dryspell-length.jpeg')
# #   ggsave(plot = p, filename = fname,
# #          height = 18, width = 24, units = 'cm')
# # }
# 
# #no clear differences detectable
# 
# 
# #calculate rpiq per season and rpiq for each station
# dryspell_eval_all <- dryspell_sum %>%
#   group_by(model, code) %>%
#   summarise(RMSE = RMSEP(predicted = count_obs, observed = count_sim),
#             RPIQ = RPIQ(predicted = count_obs, observed = count_sim))
# 
# hist(dryspell_eval_all$RPIQ)
# dryspell_eval_all$RPIQ_classes <- cut(dryspell_eval_all$RPIQ, breaks = c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5))
# 
# #make maps of RPIQ
# 
# #add info to spatial data frame
# 
# #split eval all into climatol and reddprec
# 
# dryspell_eval_climatol <- dryspell_eval_all[dryspell_eval_all$model == 'climatol',]
# dryspell_eval_reddprec<- dryspell_eval_all[dryspell_eval_all$model == 'reddprec',]
# 
# sites_climatol <- merge.data.frame(UCIPM_WS_90, dryspell_eval_climatol, by.x = 'chillR_code',  by.y = 'code')
# sites_reddprec <- merge.data.frame(UCIPM_WS_90, dryspell_eval_reddprec, by.x = 'chillR_code',  by.y = 'code')
# 
# sites_climatol <- st_as_sf(sites_climatol, coords = c("Longitude", "Latitude"), 
#                            crs = 4326, agr = "constant")
# sites_reddprec <- st_as_sf(sites_reddprec, coords = c("Longitude", "Latitude"), 
#                            crs = 4326, agr = "constant")
# 
# #problem: both have different classes and use different colours as a consequence,
# #--> set classes and color by hand
# 
# ########
# #
# #
# ####
# ###
# #
# ##
# #
# ##
# ##
# 
# ggm1 <- ggplot(data = states) +
#   geom_sf() +
#   geom_sf(data = sites_climatol, aes(color = RPIQ_classes), size = 4) +
#   annotation_scale(location = "bl")+
#   labs(x = "Longitude", y = "Latitude")+
#   coord_sf(xlim = c(-125, -114), ylim = c(32, 43), expand = FALSE)+
#   scale_color_manual(name = 'RPIQ score for \ndryspell length\nmodel: Climatol',
#                      breaks = c('(0,0.5]', '(0.5,1]', '(1,1.5]', '(1.5,2]', '(2,2.5]', '(2.5,3]', '(3,3.5]'),
#                      values = c('#ffffcc', '#c7e9b4', '#7fcdbb',  '#41b6c4', '#1d91c0', '#225ea8', '#0c2c84'))+
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
#                          style = north_arrow_fancy_orienteering)+
#   theme_bw(base_size = 18 )+
#   theme(legend.position = c(.78,.8), legend.title = element_text(size = 13), 
#         legend.text = element_text(size = 13))
# 
# ggm2 <- ggplot(data = states) +
#   geom_sf() +
#   geom_sf(data = sites_reddprec, aes(color = RPIQ_classes), size = 4) +
#   annotation_scale(location = "bl")+
#   labs(x = "Longitude", y = "Latitude")+
#   coord_sf(xlim = c(-125, -114), ylim = c(32, 43), expand = FALSE)+
#   scale_color_manual(name = 'RPIQ score for \ndryspell length\nmodel: reddPrec',
#                      breaks = c('(0,0.5]', '(0.5,1]', '(1,1.5]', '(1.5,2]', '(2,2.5]', '(2.5,3]', '(3,3.5]'),
#                      values = c('#ffffcc', '#c7e9b4', '#7fcdbb',  '#41b6c4', '#1d91c0', '#225ea8', '#0c2c84'))+
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
#                          style = north_arrow_fancy_orienteering)+
#   theme_bw(base_size = 18 )+
#   theme(legend.position = c(.78,.8), legend.title = element_text(size = 13), 
#         legend.text = element_text(size = 13))
# 
# 
# minimap = ggplot() + 
#   geom_sf(data = us_states_2163, fill = "white") + 
#   geom_sf(data = california_bb, fill = NA, color = "red", size = 1.2) +
#   theme_void()
# 
# library(cowplot)
# gg_inset_map1 = ggdraw() +
#   draw_plot(ggm1) +
#   draw_plot(minimap, x = 0.7, y = 0.45, width = 0.25, height = 0.25)
# 
# gg_inset_map2 = ggdraw() +
#   draw_plot(ggm2) +
#   draw_plot(minimap, x = 0.7, y = 0.45, width = 0.25, height = 0.25)
# 
# ggsave(gg_inset_map1, file = 'figures/final_figures/patching_map_dryday_climatol.jpeg',
#        device = 'jpeg', height = 18, width = 15, units = 'cm')
# ggsave(gg_inset_map2, file = 'figures/final_figures/patching_map_dryday_reddprec.jpeg',
#        device = 'jpeg', height = 18, width = 15, units = 'cm')
# 
# 
# 
# #evaluate RPIQ per model and season
# dryspell_eval_season <- dryspell_sum %>%
#   group_by(model, code, season) %>%
#   summarise(RMSE = RMSEP(predicted = count_obs, observed = count_sim),
#             RPIQ = RPIQ(predicted = count_obs, observed = count_sim))
# 
# 
# sites$season <- factor(sites$season, levels = c('1-DJF', '2-MAM', '3-JJA', '4-SON'))
# 
# dryspell_eval_season$RPIQ[is.infinite(dryspell_eval_season$RPIQ)] <- NA
# 
# #check for interaction of model and season
# ggline(dryspell_eval_season, x = "season", y = "RPIQ", color = "model", add = "mean_se",
#        palette = c("#00AFBB", "#E7B800"))
# #there might be interaction of model and season, as reddprec for 3-JJA higher
# 
# #two way anova with model and season as explaining variable
# res.aov <- aov(RPIQ ~ model + season, data = dryspell_eval_season)
# summary(res.aov)
# #model not important for RPIQ, only season p < 0.001
# #post-hoc test tukey.hsd
# TukeyHSD(res.aov)
# 
# #tukey hsd says that 1-DJF vs 2-MAM p < 0.001
# #                    1-DJF vs 3-JJA p > 0.05
# #                    1-DJF vs 4-SON p < 0.001
# #                    2-MAM vs 3-JJA p < 0.001
# #                    2-MAM vs 4-SON p > 0.05
# #                    3-JJA vs 4-SON p < 0.001
# 
# 
# #ttest within the groups of season
# stat.test <- dryspell_eval_season %>%
#   group_by(season) %>%
#   t_test(RPIQ ~ model) %>%
#   adjust_pvalue(method = "BH") %>%
#   add_significance()
# stat.test
# 
# #1-DJF: climatol vs reddprec ns
# #2-MAM: climatol vs reddprec *
# #3-JJA: climatol vs reddprec ns
# #4-SON: climatol vs reddprec ns
# 
# 
# #show as boxplots
# ggplot(dryspell_eval_season, aes(x = season, y = RPIQ, fill = model)) + geom_boxplot() +
#   geom_signif(y_position = c(4.5, 4.5, 4.5, 4.5), xmin = c(0.8, 1.8, 2.8, 3.8), xmax = c(1.2, 2.2, 3.2, 4.2),
#               annotation = c("NS",'NS', "NS", 'NS'), tip_length = 0) + 
#   scale_fill_manual(values=c("#56B4E9", "#CD534C99"),
#                     labels = c('Climatol', 'reddPrec'),
#                     name = 'Patching Model')+
#   ylab('RPIQ of instances per dryspell length group')+
#   xlab('Season')+
#   theme_bw(base_size = 18)
# ggsave(filename = 'figures/final_figures/patching_boxplot_dryspell.jpeg',
#        device = 'jpeg', height = 18, width = 24, units = 'cm')
# 
# 
# 
# #calculate bias in drypsell count
# dryspell_sum$bias <- dryspell_sum$count_obs - dryspell_sum$count_sim
# 
# ggplot(dryspell_sum, aes( x = as.factor(spell_length), y = bias, fill = model)) + 
#   geom_boxplot()+
#   xlab('Dryspell length (days)') + ylab('Bias (observed count - predicted count)') + 
#   scale_fill_manual(values=c("#56B4E9", "#CD534C99"),
#                     labels = c('Climatol', 'reddPrec'),
#                     name = 'Patching Model')+
#   theme_bw(base_size = 18)+
#   theme(legend.position = c(.87,.9), legend.background = element_rect(linetype="solid", 
#                                                                       colour ="black", 
#                                                                       size = .5))
# ggsave('figures/final_figures/patching_bias_dryspell.jpeg',
#        height = 18, width = 24, units = 'cm', device = 'jpeg')
# 
# #compare wetspell distribution of patched with original data