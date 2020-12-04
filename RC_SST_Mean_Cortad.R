library(ncdf4)
library(raster)
library(lubridate)
library(rts)  

wd_bathy <- raster('F:/ReefCloud/gebco_2020_netcdf/GEBCO_2020.nc')
wd_eez <- shapefile('F:/ReefCloud/EEZ_3countries/EEZ_3countries.shp')

my_eez <- subset(wd_eez,ISO_Ter1 == 'PLW')
my_bathy <- mask(crop(wd_bathy, extent(my_eez)),my_eez)

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
my_varname <- 'FilledSST'


RC_SST_Mean_Cortad <- function(url, varname){
   
  sst_crop <- mask(
              crop(
              brick(url,varname),
                    extent(my_eez)),
                    my_eez,
                    overwrite=T,
                    progress = 'text',
                    na.rm = T)
  
  sst_mean_cel <- calc((sst_crop)- 273.15, fun= mean, na.rm = T)
    sst_mean_1k <- raster::resample(sst_mean_cel, my_bathy, method = 'bilinear')
      sst_mean <- trim(sst_mean_1k, values = NA)
  plot(sst_mean)
}

RC_SST_Mean_Cortad(my_url, my_varname)

