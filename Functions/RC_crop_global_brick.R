RC_crop_brick <- function(url, varname,...){

#brick of global netcdfs, crop and mask the area of interest based on the EEZ----
brick_crop <- mask(
             crop(
             brick(url,varname = varname),
                   extent(my_eez)),
                          my_eez,
                          overwrite=T,
                          progress = 'text',
                          na.rm = T,...)
brick_crop
}