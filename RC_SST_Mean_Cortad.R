source('F:/ReefCloud/Covariates_ReefCloud/Packages_RC_V2.R')

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
my_varname <- 'FilledSST'


RC_SST_Mean_Cortad <- function(url, varname){
  
#set designated wd------
out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
  my_wd <- paste0(out_dir, '/', country, '_', my_varname)
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
#convert Kelvin to Celsius, and downscale to the bathymetry raster resolution----
sst_mean_cel <- calc((rast_crop)- 273.15, fun= mean, na.rm = T)
  rast_1k <- raster::resample(sst_mean_cel, my_bathy, method = 'bilinear')
    sst_mean <- trim(rast_1k, values = NA)
    
#save to the created folder----
writeRaster(sst_mean,
            filename = paste0(my_wd,'/', country, '_', 'Mean_', my_varname),
            overwrite = T,
            na.rm =T,
            progress = 'text',
            format = 'GTiff',
            datatype = 'FLT4S')
      
  plot(sst_mean)
    print(paste0("Location of the processed rasters: ", my_wd))
}

RC_SST_Mean_Cortad(my_url, my_varname)

