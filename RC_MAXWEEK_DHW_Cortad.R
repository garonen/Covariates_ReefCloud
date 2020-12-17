source('F:/ReefCloud/Covariates_ReefCloud/Packages_RC_V2.R')

my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc'
my_varname <- 'SSTA_DHW'

# my_url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_TSA.nc'
# my_varname <- 'TSA_DHW'

# varname can be either SSTA_DHW or TSA_DHW with the corresponding url
# if rop25 is TRUE it will calculate the week numbers of top 25th % of DHW values.
# it is FALSE by default----
RC_MAXWEEK_DHW_Cortad <- function(url, varname, top25 = FALSE){ 
  
  #brick of global netcdfs, crop and mask the area of interest based on the EEZ----
  rast_crop <- mask(
    crop(
      brick(url,varname = varname),
      extent(my_eez)),
    my_eez,
    overwrite=T,
    progress = 'text',
    na.rm = T)   
  
  
  if (top25 == FALSE) {
    #set designated wd------
    out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
    my_wd <- paste0(out_dir, '/', country, '_MaxWeek_', my_varname)
    #and create outputs folder which will be rewritten with every iteration of the function----
    if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = F, recursive = T )
    
    
    maxbrick <- brick() # brick of week numbers when maximum DHW occurred
    
    for (i in sort(unique(year(getZ(rast_crop))), decreasing = T)) {
      ts_year <- subset(rast_crop,
                        which(year(getZ(rast_crop)) == i))
      # calculate maximum value of DHW per year
      yr_max <- calc(ts_year, fun = max, na.rm = T)
      
      # set NA's to a values otherwise which.max()doesn't work
      ts_year[is.na(ts_year)] <- -9999 
      
      # create raster of week numbers that max DHW occurred in
      max_year <- calc(ts_year, function (x) {which.max(x)})
      names(max_year)<- as.character(i)
      maxbrick <- brick(max_year, maxbrick)
    }          
    
    #downscale to the bathymetry raster resolution----
    rast_1k <- raster::resample(maxbrick, my_bathy, method = 'ngb', na.rm = T)
    max_wk <- trim(rast_1k, values = NA)
    
    #save to the created folder----
    writeRaster(max_wk,
                filename = paste0(my_wd,'/', country, '_MaxWeek_', my_varname),
                overwrite = T,
                na.rm =T,
                progress = 'text',
                format = 'GTiff',
                datatype = 'INT2U',
                bylayer = T,
                suffix = names(maxbrick)
    )
    
    plot(max_wk)
    print(paste0("Location of the processed rasters: ", my_wd))
    
  # } else if (top25 == TRUE){
  #   #set designated wd------
  #   out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
  #   my_wd <- paste0(out_dir, '/', country, '_MaxWeek25_', my_varname)
  #   #and create outputs folder which will be rewritten with every iteration of the function----
  #   if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = F, recursive = T )
  #   
  #   # #downscale to the bathymetry raster resolution----
  #   # rast_crop <- raster::resample(rast_crop, my_bathy, method = 'ngb')
  #   
  #   
  #   maxbrick <- brick() # brick of week numbers when maximum DHW occurred
  #   max_quantyr <- brick() # brick of top 25 % DHW values
  #   maxbrick25 <- brick()
  #   
  #   
  #   for (i in sort(unique(year(getZ(rast_crop))), decreasing = T)) {
  #     ts_year <- subset(rast_crop,
  #                       which(year(getZ(rast_crop)) == i))
  #     
  #     # calculate maximum value of DHW per year
  #     yr_max <- calc(ts_year, fun = max, na.rm = T)
  #     # calculate top quantile to figure out the DHW. need this to create a threshold of 
  #     #values of DHW of top 25 %
  #     yr_quant <- calc(ts_year, fun = function(x,...)
  #       as.numeric(quantile(x, probs = 0.75,...)),
  #       na.rm = T)
  #     
  #     # set NA's to a values otherwise which.max()doesn't work
  #     ts_year[is.na(ts_year)] <- -9999 
  #     
  #     # create raster of week numbers that max DHW occurred in
  #     max_year <- calc(ts_year, function (x) {which.max(x)})
  #     names(max_year)<- as.character(i)
  #     maxbrick <- brick(max_year, maxbrick)
  #     # create brick of top 25 %
  #     names(yr_quant)<- as.character(i)
  #     max_quantyr <- brick(yr_quant, max_quantyr)
  #     # subset of week numbers that top 25 % of DHW values occurred in each year
  #     m_wks <- max_year[yr_max > maxValue(yr_quant)] 
  #     m_wks_rst <- max_year
  #     m_wks_rst[] <- NA
  #     max25 <- which(getValues(max_year) == m_wks)
  #     m_wks_rst[max25] <- max_year[max25]
  #     names(m_wks_rst)<- as.character(i)
  #     maxbrick25 <- brick(m_wks_rst, maxbrick25)
  #     
  #   }
  #   
  #   # #downscale to the bathymetry raster resolution----
  #   rast_1k <- raster::resample(maxbrick25, my_bathy, method = 'ngb')
  #   #max_wk <- trim(rast_1k, values = NA)
  #   
  #   #save to the created folder----
  #   writeRaster(rast_1k,
  #               filename = paste0(my_wd,'/', country, '_MaxWeek25_', my_varname),
  #               overwrite = T,
  #               #na.rm =T,
  #               progress = 'text',
  #               format = 'GTiff',
  #               datatype = 'INT2U',
  #               bylayer = T,
  #               suffix = names(rast_1k)
  #  )
    
    # plot(rast_1k)
    # print(paste0("Location of the processed rasters: ", my_wd))
    # 
} else if (top25 == TRUE){
    #set designated wd------
    out_dir <- 'F:/ReefCloud/Covariates_ReefCloud/RC_Outputs'
    my_wd <- paste0(out_dir, '/', country, '_MaxWeek25_', my_varname)
    #and create outputs folder which will be rewritten with every iteration of the function----
    if(!dir.exists(my_wd)) dir.create(my_wd, showWarnings = F, recursive = T )
    
    
    
    #maxbrick <- brick() # brick of week numbers when maximum DHW occurred
    #max_quantyr <- brick() # brick of top 25 % DHW values
    maxbrick25 <- brick()
    
    
    for (i in sort(unique(year(getZ(rast_crop))), decreasing = T)) {
      ts_year <- subset(rast_crop,
                        which(year(getZ(rast_crop)) == i))
      
      # calculate top quantile to figure out the DHW. need this to create a threshold of 
      #values of DHW of top 25 %
      yr_quant <- calc(ts_year, fun = function(x,...)
                  as.numeric(quantile(x, probs = 0.75,...)),
                  na.rm = T)
      # create brick of top 25 %
      # names(yr_quant)<- as.character(i)
      # max_quantyr <- brick(yr_quant, max_quantyr)
      
      # calculate maximum value of DHW per year
      yr_max <- calc(ts_year, fun = max, na.rm = T)
      
      # set NA's to a values otherwise which.max()doesn't work
      ts_year[is.na(ts_year)] <- -9999 
      
      # create raster of week numbers that max DHW occurred in
       max_year <- calc(ts_year, function (x) {which.max(x)})
      # names(max_year)<- as.character(i)
      # maxbrick <- brick(max_year, maxbrick)
      # 
      
      
      
      
      
      
      # subset of week numbers that top 25 % of DHW values occurred in each year
      m_wks <- max_year[yr_max > maxValue(yr_quant)] 
      m_wks_rst <- max_year
      m_wks_rst[] <- NA
      max25 <- which(getValues(max_year) == m_wks)
      m_wks_rst[max25] <- max_year[max25]
      names(m_wks_rst)<- as.character(i)
      maxbrick25 <- brick(m_wks_rst, maxbrick25)
      
    }
    
    # #downscale to the bathymetry raster resolution----
    #rast_1k <- raster::resample(maxbrick25, my_bathy, method = 'ngb')
    #max_wk <- trim(rast_1k, values = NA)
    
    #save to the created folder----
    writeRaster(maxbrick25,
                filename = paste0(my_wd,'/', country, '_MaxWeek25_', my_varname),
                overwrite = T,
                #na.rm =T,
                progress = 'text',
                format = 'GTiff',
                datatype = 'INT2U',
                bylayer = T,
                suffix = names(maxbrick25))
    
    plot(maxbrick25)
    
  } else {
    stop(paste0('top25 must be TRUE or FALSE'))
  }
}


RC_MAXWEEK_DHW_Cortad(my_url, my_varname, top25 = T )

