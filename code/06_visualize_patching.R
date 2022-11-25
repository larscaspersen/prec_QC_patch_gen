

eval <- read.csv('data/CA-patch_eval.csv')
prcp_threshold <- 1
#####
#plot of daily imputation
#####

#subset only imputed data
eval_daily <- eval[eval$new_na,]

#bring to long format
eval_daily_long <- reshape2::melt(eval_daily, 
                                  id.vars = c('Date', 'Year', 'Month', 'Day', 'station','original', 'new_na', 'variable'),
                                  variable.name = 'patch_method')

eval_daily_long$value[eval_daily_long$value < prcp_threshold & eval_daily_long$variable == 'prcp'] <- 0
eval_daily_long$original[eval_daily_long$original < prcp_threshold & eval_daily_long$variable == 'prcp'] <- 0

sum(is.na(eval_daily_long$value))
sum(is.na(eval_daily_long$original))

#which method had nas?
eval_daily_long$station <- as.factor(eval_daily_long$station)
summary(eval_daily_long[is.na(eval_daily_long$value),])
#--> remove method 
eval_daily_long <- dplyr::filter(eval_daily_long, patch_method != 'patch_mean')

#--> there are few cases where patch_mean failed, remove them from evaluation
eval_daily_long <- eval_daily_long[!is.na(eval_daily_long$value),]

#check cases where patch_normal_ratio created weird results
eval_daily_long <- eval_daily_long[which(!(abs(eval_daily_long$value) > 50 & eval_daily_long$variable == 'tmin')),]

library(tidyverse)

## qqplot with r2, rpiq and rmse value

#calculate metrics
r2_temp <- eval_daily_long %>%
  filter(variable %in% c('tmin', 'tmax')) %>% 
  group_by(patch_method, variable) %>%
  summarise(r2 = cor(value, original)^2,
            rmse = chillR::RMSEP(predicted = value, observed = original),
            rdp = sd(original) / chillR::RMSEP(predicted = value, observed = original),
            rpiq = RPIQ(predicted = value, observed = original))

r2_prec <- eval_daily_long %>%
  filter(variable == 'prcp') %>% 
  filter(original != 0) %>% 
  group_by(patch_method, variable) %>%
  summarise(r2 = cor(value, original)^2,
            rmse = chillR::RMSEP(predicted = value, observed = original),
            rdp = sd(original) / chillR::RMSEP(predicted = value, observed = original),
            rpiq = RPIQ(predicted = value, observed = original))

#set theme
theme_set(theme_bw(14))


#qqplot of precipitation
p4 <- eval_daily_long %>%
  filter(variable == 'prcp') %>%
  filter(original != 0) %>% 
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, col = 'red', linetype = 'dashed') + 
  geom_label(data = r2_prec[r2_prec$variable == 'prcp',], 
             aes(x = Inf, y = Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2),
                               '\nRMSE = ', round(rmse, digits = 2), 
                               '\nRPIQ = ', round(rpiq, digits = 2), sep = " ")),
             hjust = 1, vjust = 3)+
  xlab('Predicted Daily Precipitation [mm]')+
  ylab('Observed Daily Precipitation [mm]')+
  facet_wrap(~patch_method)
ggsave(plot = p4, filename = 'figures/eval_spei/CA_qq_daily_prcp.jpeg',
       height = 20, width  = 25, units = 'cm',device = 'jpeg')

#qqplot of tmin
p5 <- eval_daily_long %>%
  filter(variable == 'tmin') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, col = 'red', linetype = 'dashed') + 
  geom_label(data = r2_temp[r2_temp$variable == 'tmin',], 
             aes(x = Inf, y = Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2),
                               '\nRMSE = ', round(rmse, digits = 2),
                               "\nRPIQ = ", round(rpiq, digits = 2), sep = " ")),
             hjust = 1, vjust = 3)+
  xlab('Predicted Daily Minimum Temperature [°C]')+
  ylab('Observed Daily Minimum Temperature [°C]')+
  facet_wrap(~patch_method)
ggsave(plot = p5,filename = 'figures/eval_spei/CA_qq_daily_tmin.jpeg',
       height = 20, width  = 25, units = 'cm',device = 'jpeg')


#qqplot of tmax
p6 <- eval_daily_long %>%
  filter(variable == 'tmax') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, col = 'red', linetype = 'dashed') + 
  geom_label(data = r2_temp[r2_temp$variable == 'tmax',], 
             aes(x = Inf, y = Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2),
                               '\nRMSE = ', round(rmse, digits = 2),
                               "\nRPIQ = ", round(rpiq, digits = 2), sep = " ")),
             hjust = 1, vjust = 3)+
  xlab('Predicted Daily Maximum Temperature [°C]')+
  ylab('Observed Daily Maximum Temperature [°C]')+
  facet_wrap(~patch_method)
ggsave(plot = p6, filename = 'figures/eval_spei/CA_qq_daily_tmax.jpeg',
       height = 20, width  = 25, units = 'cm',device = 'jpeg')






### cumulative density plot

#for density plot I also need original data in long format
eval_long2 <- reshape2::melt(eval_daily, 
                             id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'new_na', 'variable'),
                             variable.name = 'patch_method')

eval_long2$value[eval_long2$value < prcp_threshold & eval_long2$variable == 'prcp'] <- 0

#colour palette with original as black
cbp1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1_without_org <- c("#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#check the differences in density plot
p7 <- eval_long2 %>%
  filter(variable == 'prcp', patch_method != 'patch_mean') %>%
  ggplot(aes(value+1, col = patch_method)) + stat_ecdf(geom = "step") +
  coord_trans(x="log2") +
  xlab('Log(Daily Precipitation [mm] + 1)') + 
  ylab('Cumulative Density')+
  scale_colour_manual(values=cbp1)
ggsave(plot = p7, filename = 'figures/eval_spei/CA_density_daily_prcp.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')


p8 <- eval_long2 %>%
  filter(variable == 'tmin', patch_method != 'patch_mean') %>%
  ggplot(aes(value+1, col = patch_method)) + stat_ecdf(geom = "step") +
  xlab('Daily Minimum Temperature [°C]') + 
  ylab('Cumulative Density')+
  scale_colour_manual(values=cbp1)
ggsave(plot = p8, filename = 'figures/eval_spei/CA_density_daily_tmin.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')


p9 <- eval_long2 %>%
  filter(variable == 'tmax', patch_method != 'patch_mean') %>%
  ggplot(aes(value+1, col = patch_method)) + stat_ecdf(geom = "step") +
  xlab('Daily Maximum Temperature [°C])') + 
  ylab('Cumulative Density')+
  scale_colour_manual(values=cbp1)
ggsave(plot = p9, filename = 'figures/eval_spei/CA_density_daily_tmax.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')




####
#evaluation metrics for precipitation
####
library(chillR)
library(weatherImpute)

#points in id cause problems, remove it
new_id <- str_split(eval_daily_long$station, pattern = '[.]')
new_id <- lapply(new_id, function(x){
  x[1]
})
eval_daily_long$station <- unlist(new_id)


get_eval_metrics <- function (eval_df, eval_fun = c("calc_MAE", "chillR::RPIQ", 
                                                    "chillR::RMSEP", "stats::cor"), calc_summary_score = T, 
                              bigger_better = c(F, T, F, T), weights = NULL) 
{
  if (calc_summary_score) 
    if (length(bigger_better) != length(eval_fun)) 
      stop("Length of evaluation functions and vector indicating if bigger score is \n         better need to be of same length, when calculating a summary score on all metrics")
  patch_method <- unique(eval_df$patch_method)
  eval_long <- eval_df
  stations_org <- unique(eval_long$station)
  eval_list <- split(eval_long, f = list(eval_long$station, 
                                         eval_long$patch_method))
  getfun <- function(x) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    }
    else {
      x
    }
  }
  eval_out <- lapply(eval_list, function(x) {
    scores <- lapply(eval_fun, function(y) {
      print(y)
      do.call(getfun(y), list(x$value, x$original))
    })
    return(append(scores, nrow(x)))
  })
  eval_metric <- data.table::rbindlist(lapply(eval_out, c))
  colnames(eval_metric) <- c(eval_fun, "n")
  id.vars <- (strsplit(names(eval_list), split = "[.]"))
  id.vars <- lapply(id.vars, function(x) as.data.frame(t(x[c(1, 
                                                             length(x))])))
  id.vars <- dplyr::bind_rows(id.vars)
  colnames(id.vars) <- c("station", "patch_method")
  id.vars <- id.vars[, c("station", "patch_method")]
  eval_metric <- cbind(id.vars, eval_metric)
  eval_metric <- stats::na.omit(eval_metric)
  if (calc_summary_score == T) {
    if (is.null(weights)) {
      weights <- rep(1, length(eval_fun))
    }
    intermed <- eval_metric[, eval_fun]
    neg_value_present <- colSums(eval_metric[, eval_fun] < 
                                   0) > 0
    if (any(neg_value_present)) {
      target_col <- eval_fun[neg_value_present]
      mins <- apply(eval_metric[target_col], MARGIN = 2, 
                    min)
      intermed[, target_col] <- eval_metric[target_col] + 
        matrix(abs(mins), ncol = length(target_col), 
               nrow = nrow(intermed), byrow = T)
    }
    max_metric <- intermed[, eval_fun] %>% dplyr::select(where(is.numeric)) %>% 
      dplyr::summarise_all(max)
    weight_df <- rbind.data.frame((weights)/max_metric)
    weight_df <- weight_df[rep(1, nrow(eval_metric)), ]
    intermed <- intermed[, eval_fun] * weight_df
    intermed[, bigger_better] <- 1 - intermed[, bigger_better]
    eval_metric$score <- rowSums(intermed)
    max_score <- max(eval_metric$score)
    eval_metric$score <- max_score - eval_metric$score
  }
  colnames(eval_metric) <- gsub(pattern = "get_", replacement = "", 
                                colnames(eval_metric))
  colnames(eval_metric) <- gsub(pattern = "calc_", replacement = "", 
                                colnames(eval_metric))
  colnames(eval_metric) <- gsub(pattern = ".*::", replacement = "", 
                                colnames(eval_metric))
  return(eval_metric)
}

library(weatherImpute)


#boxplot of different evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval_daily_long[eval_daily_long$variable == 'prcp',], 
                                 eval_fun = c('RMSEP', 'calc_MAE',  'calc_d_index', 'calc_hanssen_kuipers', 'calc_MCC'), 
                                 calc_summary_score = T, 
                                 bigger_better = c(F, F, T, T, T))

eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

#get rid of 'patch_'
eval_metrics_long$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_metrics_long$patch_method )

p10 <- eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab('Imputation Method') + 
  xlab('Score of evaluation metric') + 
  scale_fill_manual(values = cbp1_without_org)
ggsave(p10, filename = 'figures/eval_spei/CA_scores_daily_prec.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



#boxplot of different evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval_daily_long[eval_daily_long$variable == 'tmin',], 
                                 eval_fun = c('RMSEP', 'calc_MAE', 'RPIQ',  'calc_NSE', 'calc_S_index'), 
                                 calc_summary_score = T, bigger_better = c(F, F, T, T, T))

eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

#get rid of 'patch_'
eval_metrics_long$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_metrics_long$patch_method )

p10_1 <- eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab('Imputation Method') + 
  xlab('Score of evaluation metric') + 
  scale_fill_manual(values = cbp1_without_org)
ggsave(p10_1, filename = 'figures/eval_spei/CA_scores_daily_tmin.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


#boxplot of different evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval_daily_long[eval_daily_long$variable == 'tmax',], 
                                 eval_fun = c('RMSEP', 'calc_MAE', 'RPIQ',  'calc_NSE', 'calc_S_index'), 
                                 calc_summary_score = T, bigger_better = c(F, F, T, T, T))


eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

#get rid of 'patch_'
eval_metrics_long$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_metrics_long$patch_method )

p10_2 <- eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab('Imputation Method') + 
  xlab('Score of evaluation metric') + 
  scale_fill_manual(values = cbp1_without_org)
ggsave(p10_2, filename = 'figures/eval_spei/CA_scores_daily_tmax.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



#####
#calculate monthly means and spei
#####


#drop new_na column from eval because it is specific for each variable and this causes problems
eval <- dplyr::select(eval, -'new_na')

#melt patching methods
eval <- reshape2::melt(eval, id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'variable'), variable.name = 'patch_method')

#dcast the variable so that this is in seperate columns again
eval <- reshape2::dcast(eval, Date + Year + Month + Day + station + patch_method ~ variable, value.var = 'value')

#summarise to monthly means / prec sum
eval_monthly <- eval %>%
  filter(patch_method != 'patch_mean') %>%
  group_by(station, Year, Month, patch_method) %>%
  summarise(tmin = mean(tmin, na.rm = T),
            tmax = mean(tmax, na.rm = T),
            prcp = sum(prcp, na.rm = T))

#add latitude info to df
#get row position of station in meta_data
eval_monthly[, 'row'] <- apply(eval_monthly, 1, function(x){
  which(weather_info$id %in% x['station'])
} )
#access latitude from meta data by row, add to monthly summary dataframe
eval_monthly[,'latitude']<- weather_info$Latitude[eval_monthly$row]

#drop patch mean from levels
eval_monthly$patch_method <- factor(eval_monthly$patch_method)

#split data frame into lists
eval_monthly <- split(eval_monthly, f = list(eval_monthly$patch_method, eval_monthly$station))

#get monthly evapotranspiration 
evap_list <- lapply(eval_monthly, function(x){
  SPEI::hargreaves(Tmin = x$tmin, Tmax = x$tmax, lat = x$latitude[1], Pre = x$prcp)
})

#bring matrix to vector by rows (that is why I transposed it first)
climatic_balance <- mapply(function(x,y){
  
  #precipitation minus evapotranspiration (which was at first in matrix, melt in vector by rows)
  x$prcp - as.vector(t(y))
}, eval_monthly, evap_list) 


#calculate SPEI
spei_list <- lapply(climatic_balance, function(x) SPEI::spei(ts(x), scale = 3))

#glimpse at the spei
plot(spei_list[[1]])

spei_list <- lapply(spei_list, function(x){
  #take fitted data and bring it to vector format
  as.numeric(x$fitted)
})

#add info of month, year, station, patch method, then rbind everything
eval_monthly <- do.call(rbind, eval_monthly)
spei_list <- do.call(c, spei_list)

#add spei to eval monthly
eval_monthly$spei <- spei_list

#drop columns of row and latitude
eval_monthly <- select(eval_monthly, -all_of(c('row', 'latitude')))

#dcast and melt eval_monhtly to make original a seperate column
eval_monthly <- reshape2::melt(eval_monthly, id.vars = c('station', 'Year', 'Month', 'patch_method'))

eval_monthly <- reshape2::dcast(eval_monthly, station + Year + Month + variable ~ patch_method, value.var = 'value')

eval_monthly <- reshape2::melt(eval_monthly, id.vars= c('station', 'Year', 'Month', 'variable', 'original'), variable.name = 'patch_method')

library(ggplot2)

#calculate r2 and add it to the plot
r2 <- eval_monthly %>%
  filter(is.na(value) == F, is.infinite(value) == F, is.na(original) == F, is.infinite(original) == F) %>%
  group_by(patch_method, variable) %>%
  summarise(r2 = cor(value, original,use = "pairwise.complete.obs")^2,
            rmse = chillR::RMSEP(predicted = value, observed = original, na.rm = T),
            rpiq = RPIQ(value, original))


p11 <- eval_monthly %>%
  filter(variable == 'spei', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'spei',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), 
                               '\nRMSE = ', round(rmse, digits = 2),
                               '\nRPIQ = ', round(rpiq, digits = 2),
                               sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted SPEI') + 
  xlab('Observed SPEI') +
  facet_wrap(~patch_method)

p12 <- eval_monthly %>%
  filter(variable == 'prcp', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'prcp',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), 
                               '\nRMSE = ', round(rmse, digits = 2),
                               '\nRPIQ = ', round(rpiq, digits = 2),
                               sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted Monthly Precipitation Sum [mm]') + 
  xlab('Observed Monthly Precipitation Sum [mm]') +
  facet_wrap(~patch_method)

p13 <- eval_monthly %>%
  filter(variable == 'tmin', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'tmin',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), 
                               '\nRMSE = ', round(rmse, digits = 2),
                               '\nRPIQ = ', round(rpiq, digits = 2),
                               sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted Monthly Mean Minimum Temperature [°C]') + 
  xlab('Observed Monthly Mean Minimum Temperature [°C]') +
  facet_wrap(~patch_method)

p14 <- eval_monthly %>%
  filter(variable == 'tmax', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'tmax',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), 
                               '\nRMSE = ', round(rmse, digits = 2),
                               '\nRPIQ = ', round(rpiq, digits = 2),
                               sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted Monthly Mean Maximum Temperature [°C]') + 
  xlab('Observed Monthly Mean Maximum Temperature [°C]') +
  facet_wrap(~patch_method)

ggsave(p11, filename = 'figures/eval_spei/CA_qq_monthly_spei.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
ggsave(p12, filename = 'figures/eval_spei/CA_qq_monthly_prcp.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
ggsave(p13, filename = 'figures/eval_spei/CA_qq_monthly_tmin.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
ggsave(p14, filename = 'figures/eval_spei/CA_qq_monthly_tmax.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



### boxplot of observed (cconfidence interval) and predicted precipitation sums per month
eval_monthly2 <- reshape2::dcast(eval_monthly, formula = station + Year + Month + variable + original ~ patch_method, value.var = 'value')
eval_monthly2 <- reshape2::melt(eval_monthly2, id.vars = c('station', 'Year', 'Month', 'variable'), variable.name = 'patch_method')
eval_monthly2$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_monthly2$patch_method)

org_ci <- eval_monthly2 %>%
  filter(patch_method == 'original', is.na(value) == F, is.infinite(value) == F) %>%
  group_by(Month, variable) %>%
  summarise(lower = as.numeric(quantile(value, probs = 0.05, na.rm = T)),
            upper = as.numeric(quantile(value, probs = 0.95, na.rm = T)))
org_ci$patch_method <- 'original'



#plot imputed precipitation sum against precipitation sum confidence interval observed
p15 <- ggplot() + 
  geom_blank(data= eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'tmin',], aes(x = as.factor(Month) , y =  value)) + 
  geom_ribbon(data = org_ci[org_ci$variable == 'tmin',], 
              aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'tmin',], 
               aes(x = as.factor(Month), y = value, fill = patch_method))+
  scale_fill_manual(values=cbp1_without_org)
ggsave(p15, filename = 'figures/eval_spei/CA_monthly_tmin_boxplot.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

p16 <- ggplot() + 
  geom_blank(data= eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'prcp',], aes(x = as.factor(Month) , y =  value)) + 
  geom_ribbon(data = org_ci[org_ci$variable == 'prcp',], 
              aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'prcp',], 
               aes(x = as.factor(Month), y = value, fill = patch_method))+
  scale_fill_manual(values=cbp1_without_org)
ggsave(p16, filename = 'figures/eval_spei/CA_monthly_prcp_boxplot.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


p17 <- ggplot() + 
  geom_blank(data= eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'spei',], aes(x = as.factor(Month) , y =  value)) + 
  geom_ribbon(data = org_ci[org_ci$variable == 'spei',], 
              aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'spei',], 
               aes(x = as.factor(Month), y = value, fill = patch_method))+
  scale_fill_manual(values=cbp1_without_org)
ggsave(p17, filename = 'figures/eval_spei/CA_monthly_spei_boxplot.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


#the methods preserve the monthly means well....
#maybe just deleting random days is not enough? Or it just shows, that the choice of methods doesn't matter this much
#I sense danger comming