source('F:/ReefCloud/Covariates_ReefCloud/Packages_RC_V2.R')

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
my_varname <- 'FilledSST'


RC_SST_Mean_Cortad <- function(url, varname, hemis = 'south'){
  
#set designated wd------
out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
  my_wd <- paste0(out_dir, '/', country, '_Summer_', my_varname)
#and create outputs folder which will be rewritten with every iteration of the function----
if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = F, recursive = T )

#brick, crop and mask the global netcdfs,   
rast_crop <- mask(
            crop(
            brick(url,varname = varname),
                 extent(my_eez)),
                        my_eez,
                        overwrite=T,
                        progress = 'text',
                        na.rm = T)
# if statement to decide which months represent summer for northern/southern hemisphere----
if (hemis == 'south') {
  smr_sst <- brick(subset(rast_crop,
                          which(month(getZ(rast_crop)) >= 12 | month(getZ(rast_crop))<= 02)))

  } else if (hemis == 'north'){
    smr_sst <- brick(subset(rast_crop,
                            which(month(getZ(rast_crop)) >= 06 & month(getZ(rast_crop))<= 08)))

  } else {
    stop(cat('hemis must be \'south\' or \'north\''))
  }
#convert Kelvin to Celsius, and downscale to the bathymetry raster resolution----
sst_mean_cel <- calc((smr_sst)- 273.15, fun= mean, na.rm = T)
  sst_mean_1k <- raster::resample(sst_mean_cel, my_bathy, method = 'bilinear')
    sst_mean <- trim(sst_mean_1k, values = NA)
    
#save to the created folder----
writeRaster(sst_mean,
            filename = paste0(my_wd,'/', country, '_Mean_Summer_', my_varname),
            overwrite = T,
            na.rm =T,
            progress = 'text',
            format = 'GTiff',
            datatype = 'FLT4S')
      
  plot(sst_mean)
    print(paste0("Location of the processed rasters: ", my_wd))
}

RC_SST_Mean_Cortad(my_url, my_varname, hemis='north') # default hemisphere = south


