library(raster)
library(ncdf4)
library(aws.s3)
library(rgdal)
install.packages('rgdal')
#library(rgeos)
update.packages()
library(sp)

install.packages('rgdal', type = "source", configure.args=c(
  '--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config',
  '--with-proj-include=/Library/Frameworks/PROJ.framework/Headers',
  '--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))

load('/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/RC_covariates.RData')
getwd()
bucketlist()
mybkt <- get_bucket(bucket = 'reefcloud-modelling-outputs')

#### First have to make sure that the global bathymetry and world EEZ files are loaded from the local machine
# into my bucket 'reefcloud-modelling-outputs'. this only needs to be done once in the beginning of the 
# analysis. then can just retrieve it from the bucket    -----------------
    # wd_bathy <- raster('/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/GEBCO_2020.nc')
    # wd_eez <- shapefile(
    #       '/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/World_EEZ_v10_20180221/eez_v10.shp')
    # s3save(wd_bathy, object = 'worldBathy',  bucket = 'mybkt')
    # s3save(wd_eez, object = 'worldEEZ',  bucket = 'mybkt')
    # if (c("wd_bathy",'wd_eez') %in% ls()) rm(c("wd_bathy",'wd_eez'))

wd_bathy <- raster('/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/GEBCO_2020.nc')

wd_eez <- shapefile(
         '/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/World_EEZ_v10_20180221/eez_v10.shp')


# s3load('worldEEZ', bucket = 'mybkt' )
# s3load('worldBathy', bucket = 'mybkt' )  


# to assign focal EEZ
my_eez <- subset(wd_eez,ISO_Ter1 == 'PLW')
my_bathy <- mask(crop(wd_bathy, extent(my_eez)),my_eez)

plot(my_bathy)
plot(my_eez, add = T)

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc'#'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
my_varname <- 'FilledSST'
  
RC_SST_Mean_Cortad <- function(url, varname){
  # if(!require(c(raster, ncdf4, lubridate, aws.s3))){
  #   install.packages(c('ncdf4','raster','lubridate','aws.s3'))
  #   library(raster); library(ncdf4); library(lubridate); library(aws.s3)}

  my_brick <- brick(my_url,'SSTA')
  my_brick
  
    # my_brick <- mask(crop(brick(url, varname = varname),
  #                       extent(my_eez)),
  #                       my_eez,
  #                       overwrite=T,
  #                       progress = 'text',
  #                       na.rm = T)
  # 
  # sst_mean_celc <- calc((my_brick)- 273.15, fun= mean, na.rm = T)
  # sst_mean_1k <- raster::resample(sst_mean_celc, my_bathy, method = 'bilinear')
  # sst_mean <- trim(sst_mean_1k, values = NA)
  # sst_mean
}

RC_SST_Mean_Cortad(my_url, my_varname)


install.packages(c('ncdf4','raster','sp','rgdal'), depen)



save.image(file = '/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/RC_covariates.RData')
