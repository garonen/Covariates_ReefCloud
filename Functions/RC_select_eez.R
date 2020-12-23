RC_select_eez <- function(country) {
if (missing(country)) {
  stop("country not provided. Please see link to ISO country 3 letter names")
} else {
  wd_eez <- shapefile('F:/ReefCloud/Covariates_ReefCloud/Data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
    sub_eez <- subset(wd_eez, ISO_TER1 == country)
      plot(sub_eez)
      sub_eez
  }
}


