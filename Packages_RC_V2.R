library(raster)
library(ncdf4)
library(rts)
library(lubridate)

wd_bathy <- raster('F:/ReefCloud/gebco_2020_netcdf/GEBCO_2020.nc')
wd_eez <- shapefile('F:/ReefCloud/EEZ_3countries/EEZ_3countries.shp')

country = 'PLW'
my_eez <- subset(wd_eez, ISO_Ter1 == country)
my_bathy <- mask(crop(wd_bathy, extent(my_eez)),my_eez)
