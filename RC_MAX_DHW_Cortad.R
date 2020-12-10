source('F:/ReefCloud/Covariates_ReefCloud/Packages_RC_V2.R')

# my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc'
# my_varname <- 'SSTA_DHW'

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_TSA.nc'
my_varname <- 'TSA_DHW'

RC_MAX_DHW_Cortad <- function(url, varname){ # varname can be either SSTA_DHW or TSA_DHW with the corresponding url
  
#set designated wd------
out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
  my_wd <- paste0(out_dir, '/', country, '_Max_', my_varname)
#and create outputs folder which will be rewritten with every iteration of the function----
if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = F, recursive = T )
  
#brick of global netcdfs, crop and mask the area of interest based on the EEZ----
rast_crop <- mask(
             crop(
             brick(url,varname = varname),
                  extent(my_eez)),
                        my_eez,
                        overwrite=T,
                        progress = 'text',
                        na.rm = T)   

#calculate maximum DHW for each year in the time series
max_dhw_ann <- zApply(rast_crop,
                      by = year(getZ(rast_crop)),
                      fun = max,
                      name = as.character(year(getZ(rast_crop))))
  
#downscale to the bathymetry raster resolution----
rast_1k <- raster::resample(max_dhw_ann, my_bathy, method = 'ngb')
max_dhw <- trim(rast_1k, values = NA)

#save to the created folder----
writeRaster(max_dhw,
            filename = paste0(my_wd,'/', country, '_Max_', my_varname),
            overwrite = T,
            na.rm =T,
            progress = 'text',
            format = 'GTiff',
            datatype = 'FLT4S',
            bylayer = T,
            suffix = names(max_dhw)
           
           )
plot(max_dhw)
  print(paste0("Location of the processed rasters: ", my_wd))
  
}
RC_MAX_DHW_Cortad(my_url, my_varname)
