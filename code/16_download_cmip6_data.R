library(epwshiftr)
epwshiftr::get_data_node()

query <- init_cmip6_index(variable = c('tasmin', 'tasmax', 'pr'),
                          frequency = 'mon', 
                          experiment = c('ssp370', 'ssp126', 'ssp245', 'ssp585'),
                          resolution = '100 km',
                          source = NULL,
                          latest = TRUE,
                          activity = 'ScenarioMIP', years = c(2015, 2100))

query <- query[as.Date(query$datetime_start) == as.Date('2015-01-01'),]
query <- query[as.Date(query$datetime_end) == as.Date('2100-12-01'),]

#try downloading the stuff

dir.create('cmip6')

flist <- list.files('cmip6/')

active_nodes <- epwshiftr::get_data_node()
str(active_nodes)

#download the weather data
for(i in 1:nrow(query)){
  
  print(i)
  print(query$data_node[i] %in% active_nodes$data_node[active_nodes$status == 'DOWN'])
  
  #check if the node we want to use active, if not skip it
  if(query$data_node[i] %in% active_nodes$data_node[active_nodes$status == 'DOWN']){
    print('next')
    next
  }
  

  #last part of url should be the file name
  fname <- strsplit(query$file_url[i],split='/', fixed=TRUE)[[1]]
  fname <- fname[length(fname)]
  
  #check if fname is in flist
  if(!(fname %in% flist)){
    #mode wb is very important
    download.file(url = query$file_url[i], 
                  destfile =  paste0('cmip6/',fname),
                  mode = "wb")
  }
  
  
}





library(raster)
library(sf)
library(tmap)

extract_cmip_data <- function(fname, weather_info){
  
  #fname <- 'cmip6/pr_Amon_TaiESM1_ssp370_r1i1p1f1_gn_201501-210012.nc'
  #weather_info <- read.csv('data/CA_weather_info.csv')
  
  #determine the name of the variable
  #names can be pr, tasmin, and tasmax
  fragment_fname <- stringr::str_split(stringr::str_split(fname, pattern = '/')[[1]][2], pattern = '_')[[1]]
  #usual names in the file
  weather_vars <- c('tasmin', 'tasmax', 'pr')
  #findout which one is present here
  dname <- weather_vars[weather_vars %in% fragment_fname]
  
  b <- brick(fname, dname)
  
  #get time data
  time <- getZ(b)
  
  #how to get info of NA
  fillvalue <- ncdf4::ncatt_get(ncdf4::nc_open(fname), dname, "_FillValue")
  NAvalue(b) <- fillvalue$value
  
  #if longitude only defined as degree east
  if(as.vector(extent(b))[2] > 180){
    
    weather_info$Longitude[weather_info$Longitude < 0] <- weather_info$Longitude[weather_info$Longitude < 0] + 360
  } 
  
  
  extract.pts <- cbind(weather_info$Longitude,weather_info$Latitude)
  ext <- extract(b,extract.pts,method="bilinear")
  
  #transpose extracted data
  ext <- t(ext)
  
  ext_df <- as.data.frame(ext, row.names = F)
  colnames(ext_df) <- weather_info$id
  ext_df$Date <- time
  
  return(ext_df)
}

weather_info <- read.csv('data/CA_weather_info.csv')
fnames <- list.files('cmip6/')
fnames <- paste0('cmip6/', fnames)

extracted_df <- purrr::map(fnames, function(x){
  
  if(x == c('cmip6/pr_Amon_CIESM_ssp245_r1i1p1f1_gr_201501-210012.nc')){
    return(NA)
  }
  #print(x)
  extract_df <- extract_cmip_data(fname = x, weather_info)
  
  fragment_names <- stringr::str_split(stringr::str_split(x, '/')[[1]][2],'_')[[1]]
   
  extract_df$variable <- fragment_names[1]
  extract_df$model <- fragment_names[3]
  extract_df$ssp <- fragment_names[4]
  
  return(extract_df)
  
} )


extracted_df <- do.call(rbind, extracted_df)

#omit rows with NA
extracted_df <- na.omit(extracted_df)

library(tidyverse)

pr_adj <- extracted_df %>% 
  reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
  filter(variable == 'pr') %>% 
  mutate(value = round(value * 60 * 60 * 24, digits = 2))

tmp_adj <- extracted_df %>% 
  reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
  filter(variable != 'pr') %>% 
  mutate(value = round(value - 273.15, digits = 2))

extracted_df <- rbind(tmp_adj, pr_adj) %>% 
  reshape2::dcast(formula = Date + variable + model + ssp ~ id)

write.csv(extracted_df, 'data/cmip6_point_extracted.csv', row.names = F)



#how should I process the data?
#running mean around the center?





extracted_df <- read.csv('data/cmip6_point_extracted.csv')

extracted_df$Month <- lubridate::month(extracted_df$Date)


get_rolling_mean <- function(x, n = 11){
  
  x_lead <- purrr::map(1:floor(n/2), function(i) lead(x,i)) 
  x_lag <- purrr::map(1:floor(n/2), function(i) lag(x,i)) 
  
  x_lead <- do.call(cbind, x_lead)
  x_lag <- do.call(cbind, x_lag)
  
  x_adj <- cbind.data.frame(x_lag, x, x_lead)
  
  n_obs <- n - rowSums(is.na(x_adj))
  
  return(rowSums(x_adj, na.rm = TRUE) / n_obs)
}

extracted_df$year <- lubridate::year(extracted_df$Date)

test <- extracted_df %>% 
  reshape2::melt(id.vars = c('Month', 'variable', 'model', 'ssp', 'Date', 'year'), variable.name = 'id') %>% 
  arrange(Date) %>% 
  group_by(Month, variable, model, ssp, id) %>% 
  mutate(roll_mean = get_rolling_mean(value, n = 31), 
         Date = lubridate::my(paste(Month, year, sep = '-'))) %>% 
  ungroup() 


library(ggnewscale)
# Custom Key Glyph. Vertical Line if color = "blue". Horizontal Line otherwise
draw_key_cust <- function(data, params, size) {
  if (data$colour %in% c("blue", "red")) {
    draw_key_vpath(data, params, size)
  } else {
    draw_key_path(data, params, size)
  }
}


p1 <- test %>% 
  filter(Month == 1, variable == 'tasmax', id == 'cimis_12', ssp == 'ssp585') %>% 
  ggplot(aes(x=Date)) +
  annotate("rect", xmin=as.Date('2035-01-01'), xmax=as.Date('2065-12-31'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") +
  annotate("rect", xmin=as.Date('2070-01-01'), xmax=as.Date('2100-12-31'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(aes(y= roll_mean, col = 'running_mean'), key_glyph = "cust", size = 2) +
  geom_point(aes(y = value), alpha = 0.5) +
  ylab('Tmax (°C)') +
  facet_wrap(~model) +
  theme_bw(base_size =  15) +
  geom_vline(aes(xintercept = as.Date('2050-06-15'), col= 'near_future'), key_glyph = "cust") +
  geom_vline(aes(xintercept = as.Date('2085-06-15'), col= 'distant_future'),key_glyph = "cust") +
  scale_color_manual(name="",
                     labels=c(running_mean ="running mean (n=31)", near_future = "near future", distant_future = 'distant future'),
                     values=c(running_mean = "grey50", near_future = "blue", distant_future = "red"), 
                     guide = guide_legend(order = 2))
ggsave(plot = p1, filename = 'test_cimis_12_tmax.jpeg', device = 'jpeg', height = 7, width = 12)
  

p2 <- test %>% 
  filter(Month == 1, variable == 'tasmin', id == 'cimis_12', ssp == 'ssp585') %>% 
  ggplot(aes(x=Date)) +
  annotate("rect", xmin=as.Date('2035-01-01'), xmax=as.Date('2065-12-31'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") +
  annotate("rect", xmin=as.Date('2070-01-01'), xmax=as.Date('2100-12-31'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(aes(y= roll_mean, col = 'running_mean'), key_glyph = "cust", size = 2) +
  geom_point(aes(y = value), alpha = 0.5) +
  ylab('Tmin (°C)') +
  facet_wrap(~model) +
  theme_bw(base_size =  15) +
  geom_vline(aes(xintercept = as.Date('2050-06-15'), col= 'near_future'), key_glyph = "cust") +
  geom_vline(aes(xintercept = as.Date('2085-06-15'), col= 'distant_future'),key_glyph = "cust") +
  scale_color_manual(name="",
                     labels=c(running_mean ="running mean (n=31)", near_future = "near future", distant_future = 'distant future'),
                     values=c(running_mean = "grey50", near_future = "blue", distant_future = "red"), 
                     guide = guide_legend(order = 2))
ggsave(plot = p2, filename = 'test_cimis_12_tmin.jpeg', device = 'jpeg', height = 7, width = 12)


p3 <- test %>% 
  filter(Month == 1, variable == 'pr', id == 'cimis_12', ssp == 'ssp585') %>% 
  ggplot(aes(x=Date)) +
  annotate("rect", xmin=as.Date('2035-01-01'), xmax=as.Date('2065-12-31'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") +
  annotate("rect", xmin=as.Date('2070-01-01'), xmax=as.Date('2100-12-31'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(aes(y= roll_mean, col = 'running_mean'), key_glyph = "cust", size = 2) +
  geom_point(aes(y = value), alpha = 0.5) +
  ylab('Precipitation (mm / day)') +
  xlab('') +
  facet_wrap(~model) +
  theme_bw(base_size =  15) +
  geom_vline(aes(xintercept = as.Date('2050-06-15'), col= 'near_future'), key_glyph = "cust") +
  geom_vline(aes(xintercept = as.Date('2085-06-15'), col= 'distant_future'),key_glyph = "cust") +
  scale_color_manual(name="",
                     labels=c(running_mean ="running mean (n=31)", near_future = "near future", distant_future = 'distant future'),
                     values=c(running_mean = "grey50", near_future = "blue", distant_future = "red"), 
                     guide = guide_legend(order = 2))
ggsave(plot = p3, filename = 'test_cimis_12_prec.jpeg', device = 'jpeg', height = 7, width = 12)









#I need to add information on the model, the variable



fname <- "cmip6/tasmax_Amon_BCC-CSM2-MR_ssp370_r1i1p1f1_gn_201501-210012.nc"

b <- brick(fname, dname)

#get time data
time <- getZ(b)

#how to get info of NA
fillvalue <- ncatt_get(ncdf4::nc_open(fname), 'tasmax', "_FillValue")
NAvalue(b) <- fillvalue$value


#original region of interest
ROI <- c(-126,-112,32,42)
#modified to the projection of the brick
ROI[1:2] <- ROI[1:2] +360

#ectrtact the ROI
sub_b <- crop(b, extent(ROI))

#get old extend
ext_old <- extent(sub_b)
#define proper one with -180  to 180 longitude format
extent(sub_b) <- c(as.vector(ext_old) - c(360,360, 0, 0))



# read the shapefile
world_shp <- read_sf('data/CA_Counties/CA_Counties_TIGER2016.shp')
world_outline <- as(st_geometry(world_shp), Class="Spatial")



weather_info <- read.csv('data/CA_weather_info.csv')

Porig <- SpatialPointsDataFrame(weather_info[, c("Longitude", "Latitude")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),
                                data = weather_info[, which(!(colnames(weather_info) %in% c("id",
                                                                                            "Latitude", "Longitude")))])

m1 <- tm_shape(subset(sub_b,1)) + 
  tm_raster(title = 'Tmax (K)') +
  tm_shape(world_outline) +
  tm_borders(lwd = 1, col = 'black') +
  tm_shape(Porig) + 
  tm_symbols(size = 0.3, shape = 4, col = 'steelblue') +
  tm_graticules(lines = F, labels.col = "black") +
  tm_compass(position = c(0.035, 0.1), size = 1.75, text.size = 0.8) +
  tm_scale_bar(position = c(0.005, 0.02), bg.color = "transparent", breaks = c(0, 100, 200),
               text.size = 0.6,  color.dark = "grey20") +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "steelblue") +
  tm_layout(main.title = 'January 2015\nGCM: BCC-CSM2-MR, ssp370',
            main.title.position = "center",
            main.title.size = 1.4,
            main.title.color = "black",
            legend.title.size = 0.85,
            legend.text.size = 0.75,
            legend.outside = F,
            legend.position = c(0.75, 0.7),
            legend.frame = "white",
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            attr.color = "black",
            outer.bg.color = "white")
tmap_save(tm = m1,filename = 'map_gcm.jpeg', height = 15, width = 20, units = 'cm')




# plt <- plot(subset(sub_b,1))
# 
# plt <- lattice::levelplot(subset(b, 1))
# 
# 
# plt + layer(sp.lines(world_outline, col="black", lwd=1.0))
# 
# 
# 
# 
# 
# 
# 
# library(ncdf4)
# library(tidyverse)
# 
# 
# 
# #open the file
# ncin <- ncdf4::nc_open(fname)
# 
# 
# 
# 
# 
# 
# fillvalue <- ncatt_get(ncin,dname,"_FillValue")
# tmp_array <- ncvar_get(ncin,dname)
# lon <- ncvar_get(ncin,"lon")
# #lon is in degree east, but I want values > 180 to be minus
# #lon[lon > 180]  <- lon[lon > 180] - 360
# 
# 
# lat <- ncvar_get(ncin,"lat")
# dunits <- ncatt_get(ncin,dname,"units")
# 
# 
# 
# time <- ncvar_get(ncin, "time")
# tunits <- ncatt_get(ncin, "time", "units") #check units
# 
# library(chron)
# 
# # convert time -- split the time units string into fields
# tustr <- strsplit(tunits$value, " ")
# tdstr <- strsplit(unlist(tustr)[3], "-")
# tmonth <- as.integer(unlist(tdstr)[2])
# tday <- as.integer(unlist(tdstr)[3])
# tyear <- as.integer(unlist(tdstr)[1])
# 
# time <- lubridate::as_date(time, origin = as.Date(paste(tyear, tmonth, tday, sep = '-'), format = '%Y-%m-%d'))
# 
# 
# 
# 
# lonlattime <- as.matrix(expand.grid(lon,lat,time))
# 
# tmp_obs <- data.frame(cbind(lonlattime, as.vector(tmp_array)))
# 
# #filter out observation which are not in the region of interest
# 
# 
# 
# 
# 
# tmp_slice <- tmp_array[,,1]
# 
# image(lon,lat,tmp_slice)
# 
# 
# 
# 
# 
# #define the extent of the region of interest
# ROI <- raster::extent(-124,-110,32,42)
#  raster::as.data.frame(ncin), ROI)
# 
# 
# 
# 
# 
# #right away let's replace the nc FillValues with NAs
# tmp_array[tmp_array==fillvalue$value] <- NA
# 
# 
# 
# 
# # get a single slice or layer (January)
# m <- 1
# tmp_slice <- tmp_array[,,m]
# 
# library(lattice)
# library(RColorBrewer)
# 
# # quick map
# image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))
# 
# # levelplot of the slice
# grid <- expand.grid(lon=lon, lat=lat)
# #precipitation is in kg/m2/s
# #--> convert to mm per day (one kg of rain rises to 1mm at 1m2)
# #60*60*24
# summary(tmp_array*60*60*24)
# 
# cutpts <- c(0,10,20,30,40,50,60,70,80,90,200)
# levelplot(tmp_slice * 60 * 60 * 24 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#           col.regions=(rev(brewer.pal(10,"RdBu"))))


