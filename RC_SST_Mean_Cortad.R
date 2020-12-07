source('F:/ReefCloud/Covariates_ReefCloud/Packages_RC_V2.R')

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
my_varname <- 'FilledSST'


RC_SST_Mean_Cortad <- function(url, varname){
  
  # set designated wd
  wd <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
  setwd(wd)
  my_wd <- paste0(wd, '/', my_varname)
  # and create outputs folder
  if(!dir.exists(my_wd)) dir.create(my_wd)
   
  sst_crop <- mask(
              crop(
              brick(url,varname),
                    extent(my_eez)),
                    my_eez,
                    overwrite=T,
                    progress = 'text',
                    na.rm = T)
  
  sst_mean_cel <- calc((sst_crop)- 273.15, fun= mean, na.rm = T)
    sst_mean_1k <- raster::resample(sst_mean_cel, my_bathy, method = 'bilinear')
      sst_mean <- trim(sst_mean_1k,
                       values = NA)
      
      if(!dir.exists(paste0(my_wd)))
        
        
      writeRaster(sst_mean,
                  filename = paste0(my_wd,'/', 'Mean_', my_varname),
                  overwrite = T,
                  na.rm =T,
                  progress = 'text',
                  format = 'GTiff',
                  datatype = 'FLT4S')
      
  plot(sst_mean)
  print(paste0("Location : ", my_wd))
}

RC_SST_Mean_Cortad(my_url, my_varname)

