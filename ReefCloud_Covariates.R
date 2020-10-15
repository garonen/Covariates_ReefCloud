# My plan
# 1. Get the relevant data and subset to the required time frame
# 2. Create annual averages
# 2. Convert to rasters
# 3. crop to country's EEZ
# 4. downscale to the resolution of bathy grids i.e. 1 km
library(ncdf4)
library(raster)
library(lubridate)
library(rts)  


load('F:/ReefCloud/Covariates_ReefCloud/RC_covariates.RData')
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

# cw <-'F:/ReefCloud/ct5km_dhw-max_v3.1_2019.nc'
# cwrast <- raster(cw)
# cwnc <- ncdf4::nc_open(cw)
# 
# attributes(cwnc$var)$names[2]
# ncatt_get(cwnc, attributes(cwnc$var)$names[2])
# plot(cwrast)
# nc_close(cw)
# 
# 
# summary(my_eez)
# attributes(my_eez)
# my_eez$data



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
nc_close(url_ssta)
###################################################################################################
#################  TSA Thermal Stress Anomalies ------------------
#   (defined as SST minus Maximum of Weekly Mean Climatological SST)
###################################################################################################
url_tsa <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_TSA.nc'
tsa_brick <- brick(url_tsa, varname = 'TSA')
attributes(tsa_brick)
plot(tsa_brick[[995]])
### since the dates are the same across all datasets i can use the same dates string as before
tsa_brick_sub <- subset(tsa_brick, year_idSSTA)
  tsa_brick_sub <- setZ(tsa_brick_sub, ssta_time[year_idSSTA])
# Crop global rasters by a polygon feature of interest----------------------------------
tsa_crop <- brick()
for (i in 1:nlayers(tsa_brick_sub)){
  tsa_brick_pal <- 
    mask(crop(tsa_brick_sub[[i]],
          extent(pal)),
           overwrite = T,
            pal
  )
print(tsa_crop <- brick(tsa_brick_pal,tsa_crop))
}
plot(tsa_crop[[2:5]])
# have to reintroduce the time
print(tsa_crop <- setZ(tsa_crop, ssta_time[year_idSSTA]))
##### now calculate annual means of SSTA in degrees Kelvin--------------
tsa_annual <- zApply(tsa_crop, by = year(getZ(tsa_crop)), fun = 'mean')
plot(tsa_annual)
##### and resample the brick to the resolution of the bathy raster
tsa_annual1km <- resample(tsa_annual, bathy_pal, method = 'bilinear') # can write this stack out 
# but first have to sort out Z values because they get cut out all the time
print(tsa_annual1km <- setZ(tsa_annual1km, getZ(tsa_annual)))
####################################################################################################
######## Filled SST standard_name: sea_surface_skin_temperature ---------------
# long_name: Gap-Filled weekly SST time series
####################################################################################################
url_sst <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
sst_brick <- brick(url_sst, varname = 'FilledSST')
attributes(sst_brick)
plot(sst_brick[[995]])
### since the dates are the same across all datasets i can use the same dates string as before
sst_brick_sub <- subset(sst_brick, year_idSSTA)
  sst_brick_sub <- setZ(sst_brick_sub, ssta_time[year_idSSTA])
# Crop global rasters by a polygon feature of interest----------------------------------
sst_crop <- brick()
for (i in 1:nlayers(sst_brick_sub)){
  sst_brick_pal <- 
    mask(crop(sst_brick_sub[[i]],
          extent(pal)),
           overwrite = T,
            pal
  )
print(sst_crop <- brick(sst_brick_pal,sst_crop))
}
plot(sst_crop[[2:5]])
# have to reintroduce the time
print(sst_crop <- setZ(sst_crop, sort(ssta_time[year_idSSTA], decreasing = T)))
##### now calculate annual means of SSTA in degrees Kelvin--------------
sst_annual <- zApply(sst_crop, by = year(getZ(sst_crop)), fun = 'mean')
plot(sst_annual)
##### and resample the brick to the resolution of the bathy raster
sst_annual1km <- resample(sst_annual, bathy_pal, method = 'bilinear') # can write this stack out 
# but first have to sort out Z values because they get cut out all the time
print(sst_annual1km <- setZ(sst_annual1km, getZ(sst_annual)))

plot(sst_annual1km)
##################################################################################################
##################################################################################################
sst_pal <- 
    mask(crop(sst_brick,
          extent(pal)),
          overwrite = T,
          pal,
          progress = 'text',
         na.rm = T
  ) 
print(sst_ann_1km <- raster::resample(sst_pal, bathy_pal, method = 'bilinear'))
sst_rs <- rts(sst_ann_1km, getZ(sst_brick)) # Z-values should be the same for al Cortad variables
# To calculate SST climatology, for each pixel weekly SST aggregated to monthly composites
# then averaged across the time series to produce 12-monthly mean climatology
sst_mnt <- apply.monthly(sst_rs, FUN = mean, na.rm = T) #  monthly composites
sst_ann <- apply.yearly(sst_mnt, FUN = mean, na.rm = T) # averaging across the time series
########## Extract only summer months and calculate temperature means across years---------------
smr_time <- which(month(getZ(sst_crop)) >= 06 & month(getZ(sst_crop))<= 08) 
  print(smr_sst <- subset(sst_crop, smr_time))
  smr_sst <- setZ(smr_sst, getZ(sst_crop)[smr_time])
  plot(smr_sst[[1:5]])
  sst_smr_annl <- zApply(smr_sst, by = year(getZ(smr_sst)), fun = 'mean', 'years')
  plot(sst_smr_annl)
#####################################################################################################
####################### Sum of SST Anomalies >= 1 deg C over previous 12 weeks -----------
##################    (SSTA Degree Heating Week)
#############################################################################

ssta_dhw <- brick(url_ssta, varname = 'SSTA_DHW')  
plot(ssta_dhw[[1:4]])
attributes(ssta_dhw)  
  getZ(ssta_dhw)
  
ssta_dhw_pal <- 
    mask(crop(ssta_dhw,
          extent(pal)),
          overwrite = T,
          pal,
          progress = 'text'
  )  
  
plot(ssta_dhw_pal[[1950:1982]])

ssta_dhw_rs <- rts(ssta_dhw_pal, getZ(ssta_dhw))

ssta_dhw_ann <- apply.yearly(ssta_dhw_rs, FUN= max, na.rm = T) # keep all years in the time series
# because sometimes it can have effect since 2 years before the current year as with SSTA frequency
plot(ssta_dhw_ann[[30:38]])

print(ssta_dhwAnn_1km <- raster::resample(ssta_dhw_ann@raster, bathy_pal, method = 'ngb'))
summary(ssta_dhw_pal[[1980]])
plot(ssta_dhwAnn_1km[[30:38]])
names(ssta_dhwAnn_1km)  <- year(ssta_dhw_ann@time) # have to name the raster by year
# of dhw because same value can occur multiple times throughout the year
 writeRaster(ssta_dhwAnn_1km,
             'F:/ReefCloud/Covariates_ReefCloud/Outputs/SSTA_DHW_Max/SSTA_DHW.tif',
             overwrite = T,
             progress = 'text',
             format = 'GTiff',
             datatype = 'FLT4S',
             bylayer = T,
             suffix = year(ssta_dhw_ann@time)
           
           )
# install.packages('remote')  
# remotes::install_github("RS-eco/processNC", build_vignettes=T)
# library(processNC)  
# nc_ssta_freq <- paste0("https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc")
# 
# 
# basename(nc_ssta_freq)
# ssta_freq_pal <- subsetNC(nc_ssta_freq, 
#                           ext = pal,
#                           startdate = 2002,
#                           enddate = 2019,
#                           var = 'SSTA_Frequency')

##########  Frequency of SST Anomalies >= 1 deg C over previous 52 weeks #########################
### so in this case i can find the last date of that year and and used that raster to represent 
### ssta frequency in that year
ssta_freq <- brick(url_ssta, varname = 'SSTA_Frequency')   
ssta_freq_pal <- 
    mask(crop(ssta_freq,
          extent(pal)),
          overwrite = T,
          pal,
          progress = 'text'
  )
plot(ssta_freq_pal[[60:70]])

######  Trying to install rts package then identify last dates rasters in each year and extract
# ssta frequency for each year
class(z_time)
# ssta_freq_pal@data@names <- as.character(z_time)
# attributes(ssta_freq_pal)

summary(ssta_freq_pal[[1980]])
# first resample to the bathy raster resolution
print(ssta_freq1km <- raster::resample(ssta_freq_pal, bathy_pal, method = 'ngb'))

  ssta_freq1km[is.na(ssta_freq1km)] <- -99999
  ssta_freq_rcls <- reclassify(ssta_freq1km, cbind(NA, -99999))
  
    summary(ssta_freq_rcls[[1980]])
    plot(ssta_freq_rcls[[90:100]])
    
# convert to rts object
ssta_rs <- rts(ssta_freq_rcls, z_time)
plot(ssta_rs[[90:100]])
attributes(ssta_rs)

ssta_ends <- subset(ssta_rs,endpoints(ssta_rs, 'years')) # so this is ssta frequency for
# the previous 52 weeks in each year
plot(ssta_ends[[30:38]])

### Save individual rasters in a folder with names allocated from the dates vector
 writeRaster(ssta_ends@raster,
             'F:/ReefCloud/Covariates_ReefCloud/Outputs/SSTA_Frequency/SSTA_Freq.tif',
             overwrite = T,
             progress = 'text',
             format = 'GTiff',
             datatype = 'INT2U',
             bylayer = T,
             suffix = rownames(as.data.frame(ssta_ends@time))
           
           )
save.image(file = 'F:/ReefCloud/Covariates_ReefCloud/RC_covariates.RData')
