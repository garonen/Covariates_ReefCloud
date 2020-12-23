RC_lookup_url_table <- function(varname) {
if (missing(varname)) {
  stop(paste0("Please type variable name. Choose one of the options:
              'MeanSST','MeanSummerSST','MaxSSTA_DHW','SSTA_Freq','MaxTSA_DHW',
              'TSA_Freq','MaxWeek_SSTA','MaxWeek_TSA'"))
} else{
  d_f <- data.frame(url = c(
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_FilledSST.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_TSA.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_TSA.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_SSTA.nc',
    'https://www.ncei.noaa.gov/thredds-ocean/dodsC/cortad/Version6/cortadv6_TSA.nc'),
  cortad_covariate = c(
      'FilledSST',
      'FilledSST',
      'SSTA_DHW',
      'SSTA_Frequency',
      'TSA_DHW',
      'TSA_Frequency',
      'SSTA_DHW',
      'TSA_DHW'),
  target = c(
      'MeanSST',
      'MeanSummerSST',
      'MaxSSTA_DHW',
      'SSTA_Freq',
      'MaxTSA_DHW',
      'TSA_Freq',
      'MaxWeek_SSTA',
      'MaxWeek_TSA'
      ))
  d_f[d_f$target == varname, c('url','cortad_covariate')]

  }
}  
