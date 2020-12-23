# hemis defines the hemisphere either north or south to extract
# the relevant summer months for each hemisphere.
# Default is 'south'-----
RC_Summer_SST_Mean <- function(url, varname, hemis = 'south'){
  
#set designated wd------
out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/Outputs'
  my_wd <- paste0(out_dir, '/', country, '_Summer_', my_varname$cortad_covariate)
#and create outputs folder which will be rewritten with every iteration of the function----
if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = FALSE, recursive = TRUE )

#brick, crop and mask the global netcdfs,   
rast_crop <- mask(
             crop(
             brick(url,varname = varname),
                 extent(my_eez)),
                        my_eez,
                        overwrite=TRUE,
                        progress = 'text',
                        na.rm = TRUE)
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
sst_mean_cel <- calc((smr_sst)- 273.15, fun= mean, na.rm = TRUE)
  rast_1k <- raster::resample(sst_mean_cel, my_bathy, method = 'ngb')
    sst_mean <- trim(rast_1k, values = NA)
    
#save to the created folder----
writeRaster(sst_mean,
            filename = paste0(my_wd,'/', country, '_Mean_Summer_', my_varname$cortad_covariate),
            overwrite = TRUE,
            na.rm =TRUE,
            progress = 'text',
            format = 'GTiff',
            datatype = 'FLT4S')
      
  plot(sst_mean)
    print(paste0("Location of the processed rasters: ", my_wd))
}