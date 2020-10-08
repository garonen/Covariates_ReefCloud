# My plan
# 1. Get the relevant data and subset to the required time frame
# 2. Create annual averages
# 2. Convert to rasters
# 3. crop to country's EEZ
# 4. downscale to the resolution of bathy grids i.e. 1 km
library(ncdf4)
library(raster)
library(lubridate)

#load()
my_wd <- 'F:/ReefCloud/Covariates_ReefCloud'
if(!dir.exists('./Outputs')) dir.create('./Outputs')
url_name <- 'https://data.nodc.noaa.gov/thredds/dodsC/cortad/Version6/cortadv6_WeeklySST.nc'
url_brick <- brick(url_name)
  attributes(url_brick)

##### Import bathymetry raster ----------------------------------
  bathy <- raster('F:/ReefCloud/gebco_2020_netcdf/GEBCO_2020.nc')
    plot(bathy)
  #### Subset to time (z) of interest, currently 2002-2020 -------------------------
z_time <- getZ(url_brick)
  year_id <- which(z_time > as.Date('2002-01-01'))
    rf_brick <- subset(url_brick, year_id)
# have to reintroduce time into brick as it gets chopped during subsetting
rf_brick <- setZ(rf_brick, z_time[year_id])
# Crop global rasters by a polygon feature of interest----------------------------------
my_eez <- shapefile('F:/ReefCloud/EEZ_3countries/EEZ_3countries.shp')
   rf_brick_eez <- 
     mask(crop(rf_brick,
               extent(my_eez)),
               overwrite = T,
               my_eez,
               filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/Weekly_SST_EEZ.tif'
     )
plot(rf_brick_eez[[4]])


crop2_time <- setZ(my_eez_crop2,z_time[year_id])


###################################################################################################
#########   Extract relevant months and years from the full brick #################################
###################################################################################################
my_time <- which((month(z_time)== 06 | month(z_time)== 09) & 
                   (year(z_time) == 2000 | year(z_time) == 2001))
  print(brick_06 <- subset(url_brick, my_time))
  brick_06 <- 
              mask(crop(brick_06,
               extent(my_eez)),
               overwrite = T,
               my_eez#,
               #filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/Weekly_SST_EEZ.tif'
     )
  brick_06 <- setZ(brick_06,z_time[my_time])
plot(brick_06[[2]])
     
library(foreach)
library(doParallel)
UseCores <- detectCores()-1
cl <- makeCluster(UseCores)
registerDoParallel(cl)
#
crop_brick <- stack()
crop_brick <- foreach(i = 1:nlayers(brick_06),
                     .init = crop_brick,
                     .combine = addLayer,
                     .inorder = T,
                     .packages = c('raster', 'lubridate')) %dopar% {
    print(i)
                    
    my_crop <- mask(crop(brick_06[[i]],
               extent(my_eez)),
               my_eez,
               filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/Weekly_SST_EEZ_Crop06.tif')
    
}
crop_brick <- setZ(crop_brick, z_time)                       
annual_sst_mean <- zApply(crop_brick,  by = year(getZ(crop_brick)), fun = 'mean')- 273.15
plot(annual_sst_mean[[2]])

monthly_sst_mean <- zApply(crop_brick,  by = month(getZ(crop_brick)), fun = 'mean')- 273.15

stopCluster(cl)
# system.time({
# foreach(i = unique(month(getZ(brick_06))), .packages = c('raster', 'lubridate')) %dopar% {
#           
#           print(i)
# 
#           sst_month <- subset(brick_06,which(month(getZ(brick_06)) == i))- 273.15
#   print(sst_month)
# 
#   sst_montmean <- mean(sst_month,
#                        filename = paste0(mywd, "/",'Mean_Monthly_SST_',i),
#                                    format = "GTiff", overwrite = TRUE,
#                                    datatype = 'FLT4S', progress = 'text') # converting to Celsius
# 
#   # sst_montmean_res <- resample(sst_montmean, gebco_grid_crop,
#   #                                  method = 'bilinear',
#   #                                  filename = paste0(mywd, "/",'Mean_Monthly_SST_',mon),
#   #                                  format = "GTiff", overwrite = TRUE,
#   #                                  datatype = 'FLT4S', progress = 'text')
# 
#   #sst_montmean_brick <- brick(sst_montmean, sst_montmean_brick)
# }
# })
# #endCluster()
# monthlies <- zApply(brick_06,  by = month(getZ(brick_06)), fun = 'mean')- 273.15
# stopCluster(cl)


UseCores <- detectCores()-1
cl <- makeCluster(UseCores)
registerDoParallel(cl)
crop_brick <- stack()
crop_brick <- foreach(i = 1:nlayers(url_brick),
                     .init = crop_brick,
                     .combine = addLayer,
                     .inorder = T,
                     .packages = c('raster', 'lubridate')) %dopar% {
    print(i)
                     }
    my_crop <- mask(crop(url_brick[[i]],
               extent(my_eez)),
               my_eez,
               filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/Weekly_SST_EEZ_Crop.tif')
    
}
crop_brick <- setZ(crop_brick, z_time)                       
annual_sst_mean <- zApply(crop_brick,  by = year(getZ(crop_brick)), fun = 'mean')- 273.15
plot(annual_sst_mean[[2]])

monthly_sst_mean <- zApply(crop_brick,  by = month(getZ(crop_brick)), fun = 'mean')- 273.15

stopCluster(cl)
###################################################################################################

mywd <- 'F:/ReefCloud/Covariates_ReefCloud/Outputs'  
sst_montmean_brick <- brick()
for(i in unique(month(z_time[year_id]))) { # will have to adjust here for multiple years 
  # add doTry and parallel 
  sst_month <- subset(crop2_time,
                      which(month(getZ(crop2_time)) == i)) - 273.15 # converting to Celsius
  print(sst_month) 
  sst_montmean <- mean(sst_month, na.rm = T)
  writeRaster(sst_montmean,filename = paste0(mywd, "/",'Mean_Monthly_SST_',i),
                          format = "GTiff", overwrite = TRUE,
                          datatype = 'FLT4S', progress = 'text') 
  
  # sst_montmean_res <- resample(sst_montmean, gebco_grid_crop,
  #                                  method = 'bilinear',
  #                                  filename = paste0(mywd, "/",'Mean_Monthly_SST_',i),
  #                                  format = "GTiff", overwrite = TRUE,
  #                                  datatype = 'FLT4S', progress = 'text')
  print(sst_montmean)                             
  print(sst_montmean_brick <- brick(sst_montmean,sst_montmean_brick))
}         

plot(sst_montmean_brick)
writeRaster(sst_montmean_brick, filename = )


zoo <- zApply(crop2_time, by=month(getZ(crop2_time)), fun = 'mean', name='month' )
plot(zoo)
crop2_time[[1]]

# mean1 <- raster('F:/ReefCloud/Covariates_ReefCloud/Outputs/Mean_Monthly_SST_1.tif')
# plot(mean1)
# rm(mean1)
##################################################################################################

##### calculate mean by year---------------------------------------------------------
mean_99 <- mean(brick99)- 273.15
plot(mean_99)


# 
# pnt_exrct <- extract(recent_brick, my_point_data, method = 'bilinear')
# head(pnt_exrct)

#################################################################################################
#####################################################################################################
url_ssta <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc'
ssta_brick <- brick(url_ssta, varname = 'SSTA')
attributes(ssta_brick)
plot(ssta_brick[[995]])

# cw <-'F:/ReefCloud/ct5km_ssta-mean_v3.1_2006.nc'
# cwrast <- raster(cw)
# cwnc <- ncdf4::nc_open(cw)
# 
# attributes(cwnc$var)$names[2]
# ncatt_get(cwnc, attributes(cwnc$var)$names[2])
# plot(cwrast)
summary(my_eez)
attributes(my_eez)
my_eez$data



pal <- subset(my_eez,Territory1 == 'Palau')
plot(pal, add = T)

 #### Subset to time (z) of interest, currently 2002-2020 -------------------------
ssta_time <- getZ(ssta_brick)
  year_idSSTA <- which(ssta_time > as.Date('2002-01-01'))
    ssta_brick_sub <- subset(ssta_brick, year_idSSTA)
# have to reintroduce time into brick as it gets chopped during subsetting
ssta_brick_sub <- setZ(ssta_brick_sub, ssta_time[year_idSSTA])
# Crop global rasters by a polygon feature of interest----------------------------------
ssta_crop <- brick()
for (i in 1:nlayers(ssta_brick_sub))
  {
  print(i)
   ssta_brick_pal <- 
    mask(crop(ssta_brick_sub[[i]],
          extent(pal)),
           overwrite = T,
            pal
  )
print(ssta_crop <- brick(ssta_brick_pal,ssta_crop))
}

plot(ssta_crop[[2:5]])
# have to reintroduce the time
print(ssta_crop <- setZ(ssta_crop, ssta_time[year_idSSTA]))
##### now calculate annual means of SSTA in degrees Kelvin--------------
ssta_annual <- zApply(ssta_crop, by = year(getZ(ssta_crop)), fun = 'mean', name = 'annTemp')
plot(ssta_annual)
##### and resample the brick to the resolution of the bathy raster
bathy_pal <- mask(crop(bathy, extent(pal)),pal)
plot(bathy_pal)

ssta_annual1km <- resample(ssta_annual, bathy_pal, method = 'bilinear') # can write this stack out 
# but first have to sort out Z values because they get cut out all the time
plot(ssta_annual1km)
print(ssta_annual1km <- setZ(ssta_annual1km, getZ(ssta_annual)))



#  Saving the RASTER BRICK
# writeRaster(ndvit1, filename="ndvi_2015t1.tif", bandorder='BSQ',
#             format="GTiff", overwrite=TRUE)