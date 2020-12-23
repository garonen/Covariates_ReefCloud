RC_crop_bathy <- function(my_eez){ # this will get the result of RC_select_country.R
  wd_bathy <- raster('F:/ReefCloud/Covariates_ReefCloud/Data/gebco_2020_netcdf/GEBCO_2020.nc')
  sub_bathy <- mask(crop(wd_bathy,
                         extent(my_eez)),
                         mask = my_eez)
  plot(sub_bathy)
  sub_bathy
}  


