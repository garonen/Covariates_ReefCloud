# Can be used for either SSTA or TSA frequency
RC_FREQ_Cortad <- function(url, varname,...){ # varname can be either SSTA_DHW or TSA_DHW with the corresponding url
  
#set designated wd------
out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/Outputs'
  my_wd <- paste0(out_dir, '/', country, '_', my_varname$cortad_covariate)
#and create outputs folder which will be rewritten with every iteration of the function----
if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = FALSE, recursive = TRUE )
  
#brick of global netcdfs, crop and mask the area of interest based on the EEZ----
rast_crop <- mask(
             crop(
             brick(url,varname = varname),
                  extent(my_eez)),
                        my_eez,
                        overwrite=TRUE,
                        progress = 'text',
                        na.rm = TRUE)   

#create an rts raster stack-----
rast_rs <- rts(rast_crop, getZ(rast_crop))  

#extract date for last week of the year, those values are the anomaly frequency for the previous
  # 52 weeks in each year--------------
rast_endp <- subset(rast_rs,endpoints(rast_rs, 'years'), na.rm = TRUE,...)  
  
#downscale to the bathymetry raster resolution----
rast_1k <- raster::resample(brick(rast_endp@raster), my_bathy, method = 'ngb') 
  rast_freq <- trim(rast_1k, values = NA)

  #save to the created folder----
writeRaster(rast_freq,
            filename = paste0(my_wd,'/', country, '_', my_varname$cortad_covariate),
            overwrite = TRUE,
            na.rm =TRUE,
            progress = 'text',
            format = 'GTiff',
            datatype = 'INT2U',
            bylayer = TRUE,
            suffix = rownames(as.data.frame(rast_endp@time))
           
           )
plot(rast_freq)  
  print(paste0("Location of the processed rasters: ", my_wd))
  
}