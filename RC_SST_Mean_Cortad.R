library(raster)
library(ncdf4)
library(aws.s3)
library(rgdal)

getwd()
bucketlist()
mybkt <- get_bucket(bucket = 'reefcloud-modelling-outputs')

#### First have to make sure that the global bathymetry and world EEZ files are loaded from the local machine
# into my bucket 'reefcloud-modelling-outputs'. this only needs to be done once in the beginning of the 
# analysis. then can just retrieve it from the bucket    -----------------
    # bathy <- raster('/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/GEBCO_2020.nc')
    # wd_eez <- shapefile(
    #        '/home/rstudio/R/ReefCloud_Covariates/Covariates_ReefCloud/World_EEZ_v10_20180221/eez_v10.shp')
    # s3save(bathy, object = 'worldBathy',  bucket = 'mybkt')
    # s3save(wd_eez, object = 'worldEEZ',  bucket = 'mybkt')
    # if (c("bathy",'wd_eez') %in% ls()) rm(c("bathy",'wd_eez'))

s3load('worldEEZ', bucket = 'mybkt' )
s3load('worldBathy', bucket = 'mybkt' )  







RC_SST_Mean_Cortad <- function(nc, var, ext, fxn){
  if(!require(raster)){install.packages(c('ncdf4','raster','lubridate','rts', 'aws.s3', 'aws.ec2metadata packages')); library(raster)}
  rs <- crop(brick(nc, varname=var),extent(ext), filename=var, datatype=NULL, overwrite=T)
  rs1 <- calc(rs, fun= function(x) {x - 273.15})
  r <- calc(rs1, fun= fxn, na.rm = TRUE)
  r
}


#install.packages(c('ncdf4','raster','lubridate','rts', 'aws.s3', 'aws.ec2metadata packages'))
