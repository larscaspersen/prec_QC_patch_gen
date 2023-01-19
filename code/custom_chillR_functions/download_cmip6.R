#function to extract information of one 
extract_cmip_data <- function(fname, coords){
  
  #fname needs to be a character
  assertthat::assert_that(is.character(fname))
  
  #coords needs to be a data.frame with column names
  assertthat::assert_that(is.data.frame(coords))
  assertthat::assert_that(all(c('Longitude', 'Latitude') %in% colnames(coords)))
  

  #determine the name of the variable
  #names can be pr, tasmin, and tasmax
  fragment_fname <- stringr::str_split(stringr::str_split(fname, pattern = '/')[[1]][2], pattern = '_')[[1]]
  #usual names in the file
  weather_vars <- c('tasmin', 'tasmax', 'pr')
  #findout which one is present here
  dname <- weather_vars[weather_vars %in% fragment_fname]
  
  
  
  #load the file
  b <- raster::brick(fname, dname)
  
  #get time data
  time <- raster::getZ(b)
  
  #how to get info of NA
  fillvalue <- ncdf4::ncatt_get(ncdf4::nc_open(fname), dname, "_FillValue")
  #ncdf4::nc_close(fname)
  raster::NAvalue(b) <- fillvalue$value
  
  #if longitude only defined as degree east
  if(as.vector(raster::extent(b))[2] > 180){
    
    coords$Longitude[coords$Longitude < 0] <- coords$Longitude[coords$Longitude < 0] + 360
  } 
  
  
  extract.pts <- cbind(coords$Longitude,coords$Latitude)
  ext <- raster::extract(b,extract.pts,method="bilinear")
  
  #transpose extracted data
  ext <- t(ext)
  
  ext_df <- as.data.frame(ext, row.names = F)
  #colnames(ext_df) <- weather_info$id
  ext_df$Date <- time
  
  return(ext_df)
}



get_scenarioMIP_data <- function(coordinates, 
                                 start_year,
                                 end_year,
                                 metric,
                                 experiment,
                                 frequency = 'mon',
                                 resolution = '100 km',
                                 keep_downloaded = FALSE,
                                 path_download = 'temp_cmip6',
                                 ...){
  
  
  #assertthat::assert_that(all(metric %in% c('tasmin', 'tasmax', 'pr')), msg = "Only c('tasmin', 'tasmax', 'pr') are valid options for metric")
  assertthat::is.number(start_year)
  assertthat::is.number(end_year)
  assertthat::assert_that(is.data.frame(coordinates))
  assertthat::assert_that(all(c('id', 'Longitude', 'Latitude') %in% colnames(coordinates)))
  assertthat::assert_that(all(is.character(metric)))
  assertthat::assert_that(is.character(experiment))
  #assertthat::assert_that(all(is.character(scenario)))

  
  #coordinates <- data.frame(id = 'id_1', Longitude = 9.2, Latitude = 54.6 )
  
  #query the data.base
  query <- epwshiftr::init_cmip6_index(variable = metric,
                                       frequency = frequency, 
                                       experiment = experiment,
                                       resolution = resolution,
                                       source = NULL,
                                       latest = TRUE,
                                       activity = 'ScenarioMIP', 
                                       years = c(start_year, end_year),
                                       ...)
  
  #if query didn't reutrn anything stop the funciton
  if(length(query) == 0){
    error('The search returned no valid variable. Try different search options')
  }
  
  #the date subset doesnt work so well, so make sure that only this period is covered
  #somehow the end date must be 1st of december 2100 and not 31st...
  query <- query[as.Date(query$datetime_start) == as.Date(paste0(start_year,'-01-01')),]
  query <- query[as.Date(query$datetime_end) == as.Date(paste0(end_year,'-12-01')),]
  
  if(length(query) == 0){
    error('Subsetting the query for the desired start_year and end_year lead to no remaining entry. Try different start and end year')
  }
  

  ######
  #download data
  ######
  
  #create temporary directory
  if(dir.exists(path_download) == FALSE){
    dir.create(path_download) 
  }
  
  #if the directory already exists, check which files were already downloaded
  flist <- list.files(paste0(path_download,'/'))
  
  #check which nodes are available for download
  active_nodes <- epwshiftr::get_data_node()
  
  #download the weather data
  for(i in 1:nrow(query)){
    
    #check if the node we want to use active, if not skip it
    if(query$data_node[i] %in% active_nodes$data_node[active_nodes$status == 'DOWN']){
      #print('next')
      next
    }
    
    
    #last part of url should be the file name
    fname <- strsplit(query$file_url[i],split='/', fixed=TRUE)[[1]]
    fname <- fname[length(fname)]
    
    #check if fname is in flist
    if(!(fname %in% flist)){
      #mode wb is very important
      download.file(url = query$file_url[i], 
                    destfile =  paste0(path_download,'/',fname),
                    mode = "wb")
    }
    
    
  }
  
  #get names of downloaded files
  fnames <- list.files(paste0(path_download,'/'))
  fnames <- paste0(path_download,'/', fnames)
  
  #only work with files which are not empty
  fnames <- fnames[file.size(fnames) > 0L]
  
  extracted_df <- purrr::map(fnames, function(x){
    
    #print(x)
    extract_df <- extract_cmip_data(fname = x, coords = coordinates)
    
    fragment_names <- stringr::str_split(stringr::str_split(x, '/')[[1]][2],'_')[[1]]
    
    extract_df$variable <- fragment_names[1]
    extract_df$model <- fragment_names[3]
    extract_df$ssp <- fragment_names[4]
    
    return(extract_df)
    
  } )
  
  
  extracted_df <- do.call(rbind, extracted_df)
  
  #omit rows with NA
  extracted_df <- stats::na.omit(extracted_df)
  
  #library(tidyverse)
  
  if('pr' %in% metric){
    pr_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(variable == 'pr') %>% 
      mutate(value = round(value * 60 * 60 * 24, digits = 2))
  } else {
    pr_adj <- NULL
  }
  
  if('tasmin' %in% metric){
    tmin_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(variable == 'tasmin') %>% 
      mutate(value = round(value - 273.15, digits = 2))
  } else {
    tmin_adj <- NULL
  }
  
  if('tasmax' %in% metric){
    tmin_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(variable == 'tasmax') %>% 
      mutate(value = round(value - 273.15, digits = 2))
  } else {
    tmax_adj <- NULL
  }
  
  #in case the metric contains variable which are not tasmin, tasmax, pr
  if(any(!metric %in% c('tasmin', 'tasmax', 'pr'))){
    other_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(!variable %in% c('tasmax', 'tasmin', 'pr'))
  } else {
    other_adj <- NULL
  }
  
  #bind everything back together, bring back to long format
  extracted_df <- rbind(tmin_adj, tmax_adj, pr_adj, other_adj) %>% 
    reshape2::dcast(formula = Date + variable + model + ssp ~ id)
  
  
  if(keep_downloaded == FALSE){
    unlink(paste0(path_download,'/'), recursive = TRUE)
  }
  
  return(extracted_df)
}
