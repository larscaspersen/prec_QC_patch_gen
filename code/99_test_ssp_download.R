library(epwshiftr)
query <- init_cmip6_index(variable = 'tasmin', frequency = 'mon', experiment = 'ssp585')

library(ncdf4)
ncin <- nc_open('hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20490101-20491231.nc')
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

tunits
# time is measured in days since 2015-01-01

#starts at first of Jan 2049, as indicated in file name
(time[1] /365) + 2015

# get temperature
tmp_array <- ncvar_get(ncin,"hurs")
dlname <- ncatt_get(ncin,"hurs","long_name")
dunits <- ncatt_get(ncin,"hurs","units")
fillvalue <- ncatt_get(ncin,"hurs","_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

#close nc file
nc_close('hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20490101-20491231.nc')





# load some packages
library(chron)
library(lattice)
library(RColorBrewer)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))


#replace NA
# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1])))

# get a single slice or layer (January)
d <- 1
tmp_slice <- tmp_array[,,d]

# quick map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))
