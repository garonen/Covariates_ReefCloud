library(ncdf4)
library(raster)
nchd <- nc_open('F:/ReefCloud/cortadv6_WeeklySST.nc')
print(nchd)
attributes(nchd$var)$names[2]
nchd$var[[8]]

lon <- ncvar_get(nchd, varid = "lon")
lat <- ncvar_get(nchd, varid = "lat")
tm <- ncvar_get(nchd, 'time')
dim(tm)
summary(lat)

wkly_sst <- ncvar_get(nchd, attributes(nchd$var)$names[[8]], start = c(1,1,1960), count = c(-1,-1,3))
dim(wkly_sst)
length(wkly_sst)
wkly_sst[1:10,1:10,3]

print(wkl_slc <- wkly_sst[,,3])
dim(wkl_slc)
wkl_slc

r <- raster (t(wkl_slc),
             xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
             crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r, ylim = c(-90,90))

rb <- brick(wkly_sst,
            xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rb <- t(rb)
  flip(rb, direction ='y')

  for(i in 1:length(lon)){
  if(lon[i] < 0) lon[i] <- lon[i]+360
}
  
#plot(extent(rb))
plot(t(rb))
pnt_lon <- 149.5978
pnt_lat <- 22.6275

pnt_ser <- extract(rb, SpatialPoints(cbind(pnt_lon, pnt_lat)), method = 'simple')

pnt_df <- data.frame(lyr = seq(from =1, to =3, by =1), Temp = t(pnt_ser))

plot(extent(t(rb)))
plot(t(rb$layer.1), add = T)
plot(SpatialPoints(cbind(pnt_lat, pnt_lon)), add = T, size = 8)
# image(flip(rb, direction = 'x'))
# summary(lon)
# nchd$var$WeeklySST$storage

# dat1 <- list()
# dat1$x <- c(lon)
# dat1$y <- c(lat)
# dat1$z <- c(wkly_sst)
# 
# plot(raster(list(x=lon, y = lat, z = wkl_slc)))

length(as.vector(wkly_sst)[1:50])
tlonlat <- as.matrix(expand.grid(lon, lat, tm))


nc_close(nchd)
###################################################################################################
####################################################################################################
#########  Some options to connect to Opendap directory and extract netcdf data ###################
#############################################################################################
#################################################

# try the devtools location for the most uptodate ncdf4
devtools::install_github(‘mdsumner/ncdf4’)
# try nc_open(readunlim = T)
#try get_cortad() function
#try (grid.nc$var) to see what it called
# try subsetting on Cortad Opendap website and importing that link

#install.packages('ncdf4')
library(ncdf4)
url_grid <- 'https://data.nodc.noaa.gov/cortad/Version6/cortadv6_WeeklySST.nc'
download.file(url_grid, 'cortadv6_WeeklySST.nc', method = 'auto', quiet = F, mode = 'wb', cacheOK = T)

grid.ncdw <- nc_open('cortadv6_WeeklySST.nc',write = F, readunlim = T, verbose = F)
print(grid.ncdw)
names(grid.ncdw$var)

grid.ncdw$nvars
grid.ncdw$var$WeeklySST
grid.ncdw$dim$time$vals
# install.packages('ncdf4.helpers')
# library(ncdf4.helpers)

#time.ser <- nc.get.time.series(grid.nc, v = 'WeeklySST', time.dim.name = 'time',
                               #correct.for.gregorian.julian = F)
#time.index <- which(as.Date(ncvar_get(grid.nc, 'time'),
 #                           origin = as.Date('1981-12-31')) > '2019-12-10')



#week.sst<- ncvar_get(grid.nc, 'WeeklySST')
#rf.time.sub <- nc.get.var.subset.by.axes(grid.nc, 'WeeklySST',
                                      #   axis.indices = list(T = time.index))



# rf.time.sub[1:30,1:30,]
# 
# library(fields)

grid.lon <- ncvar_get(grid.ncdw, 'lon')
grid.lat <- ncvar_get(grid.ncdw, 'lat')
grid.time <- ncvar_get(grid.ncdw, 'time')
grid.date <- as.Date(ncvar_get(grid.ncdw, 'time'), origin = as.Date('1981-12-31'))
tail(grid.date, n = 50)
week.sst <- ncvar_get(grid.ncdw, 'WeeklySST', start = c(1,1,1960), verbose = T)#, count = c(10,10,3))
week.sst[1:10, 1:10,5]
print(week.sst)
dim(week.sst)
week.slc <- week.sst[,,5]
week.slc[34:50,12:20]

rs <- raster(t(week.slc),
             xmn=min(grid.lon), xmx=max(grid.lon), ymn=min(grid.lat), ymx=max(grid.lat),
             crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(rs)


image.plot(grid.lon, grid.lat, week.sst)
nc_close(grid.nc)
week.sst$
##########################################################################################
url_grid_fld <- 'https://data.nodc.noaa.gov/thredds/dodsC/cortad/Version6/cortadv6_FilledSST.nc'
grid_nc_fld <- nc_open(url_grid_fld, write = F, readunlim = T, verbose = F)
names(grid_nc_fld$var)
print(grid_nc_fld)
grid_nc_fld$dim$time
grid.lon <- ncvar_get(grid_nc_fld, 'lon')
grid.lat <- ncvar_get(grid_nc_fld, 'lat')
grid.time <- ncvar_get(grid_nc_fld, 'time')
dim(grid.lon)
image(grid.lon, sort(grid.lat, decreasing = F), tmp_slice)
image(grid.lon, flip(grid.lat), tmp_slice)
wk_fld_sst <- ncvar_get(grid_nc_fld,'FilledSSTmean')

wk_fld_temp <- ncvar_get(grid_nc_fld,'FilledSST',
                            start = c(1,1,1980),
                                    verbose = T, 
                             count = c(10,10,3))

tmp_slice <- wk_fld_temp[,,1]
tmp_slice[1:20,1:20,1]
dim(tmp_slice)


head(wk_fld_sst)

fname <- 'F:/ReefCloud/cortadv6_WeeklySST.nc'
all_brick <- brick(fname)
  plot(extent(all_brick))
  plot(all_brick[[50]], add = T)

print(ROI <- extent(100,155,-35,35))

recent_brick <- all_brick[[1900:1982]]# years that we are interested in
crop_brick <- crop(recent_brick, ROI)
plot(crop_brick[[25]])
attributes(crop_brick)
names(crop_brick)

##### URL to brick and bb subset
url_name <- 'https://data.nodc.noaa.gov/thredds/dodsC/cortad/Version6/cortadv6_WeeklySST.nc'
url_brick <- brick(url_name)
plot(url_brick[[25]])

recent_url_brick <- url_brick[[]]


pnt_exrct <- extract(recent_brick, my_point_data, method = 'bilinear')
head(pnt_exrct)


###################################################################################################
#########     option 2                   --------------
###################################################################################################
#Load the packages:
library("sp")
library("ncdf")
 
getOpenDapURLAsSpatialGrid = function(opendapURL,variableName,bboxInDegrees){
  print(paste("Loading opendapURL",opendapURL));
  # Open the dataset
  dataset = open.ncdf(opendapURL)
    bbox=bboxInDegrees;
  # Get lon and lat variables, which are the dimensions of depth. For this specific dataset they have the names lon and lat
  G.x=get.var.ncdf(dataset,"lon")
  G.y=get.var.ncdf(dataset,"lat")
 
  # Make a selection of indices which fall in our subsetting window
  # E.g. translate degrees to indices of arrays.
  xindicesinwindow=which(G.x>bbox[1]&G.x<bbox[3]);
  xmin=min(xindicesinwindow)
  xmax=max(xindicesinwindow)
  xcount=(xmax-xmin)+1; # needs to be at least 1
 
  yindicesinwindow=which(G.y>bbox[2]&G.y<bbox[4]);
  ymin=min(yindicesinwindow)
  ymax=max(yindicesinwindow)
  ycount=(ymax-ymin)+1;# needs to be at least 1
 
  print(paste("Indices:",xmin,ymin,xmax,ymax));# <== print bbox in indices
 
  # Get the variable depth
  G.z=get.var.ncdf(dataset, variableName,start=c(xmin,ymin), count=c(xcount,ycount));
 
  # Transpose this dataset, sometimes X and Y are swapped
  #G.z=t(G.z)
 
  # At the beginning we loaded the complete lat and lon variables
  # in order to find which indices belong in our subset window
  # In order to create a spatialdatagrid frame, we need to make the lat and lon variables
  # the same size as the requested matrix. E.g. The lat and lon (or y and x) needs to be subsetted:
  G.sx = G.x[xmin:xmax]
  G.sy = G.y[ymin:ymax]
 
  # Optionally create dims with equal cellsizes
  # This is sometimes needed because there can be small errors in the values of the x and y variables.
  makeCellsizesEqual=TRUE
  if(makeCellsizesEqual){
    # Make cellsizes equal for X dimension
    cellsizex=(G.sx[length(G.sx)]-G.sx[1])/(length(G.sx)-1)
    tempX=(((1:length(G.sx))-1))*cellsizex+G.sx[1]
    G.sx=tempX
 
    # Make cellsizes equal for Y dimension
    cellsizey=(G.sy[length(G.sy)]-G.sy[1])/(length(G.sy)-1)
    tempY=(((1:length(G.sy))-1))*cellsizey+G.sy[1]
    G.sy=tempY
  }
 
  # We have now x, y, and z complete. In order to create a SpatialGridDataFrame
  # We need to make the shape of all variables the same
  # This means that the x and y variables also need to become a matrix.
 
  # Create a matrix of X values
  G.mx=rep(G.sx,dim(G.z)[2])
 
  # Create a matrix field of Y values
  G.my=(as.vector(t(matrix(rep(G.sy,dim(G.z)[1]),nrow=dim(G.z)[2],ncol=dim(G.z)[1]))))
 
  # Make a dataframe of the X, Y and Z values
  myspatialgrid=data.frame(topo=as.vector(G.z),lon=G.mx,lat=G.my)
 
  # We have now gathered all information required to create a SpatialGridDataFrame object
 
  # Assign X and Y coordinates
  coordinates(myspatialgrid)=~lon+lat
 
  # Make a gridded dataset, previousely the object was just a bunch of points with XY coodinates
  gridded(myspatialgrid) = TRUE
  fullgrid(myspatialgrid) = TRUE
 
  # This can be converted to a SpatialGridDataFrame
  myspatialgrid = as(myspatialgrid, "SpatialGridDataFrame")
 
  # Optionally assign a projection string to this object
  attributes(myspatialgrid)$proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs  <>")
 
  myspatialgrid;
}
 
 
# Set the bounding box window we want to subset for, in degrees
# order: min-x, min-y, max-x, max-y
bboxInDegrees=c(0,50,10,55)
 
url_grid = array();
url_grid[1] = 'http://geoport.whoi.edu/thredds/dodsC/bathy/etopo1_bed_g2';
url_grid[2] = 'http://geoport.whoi.edu/thredds/dodsC/bathy/etopo2_v2c.nc';
url_grid[3] = 'http://geoport.whoi.edu/thredds/dodsC/bathy/srtm30plus_v1.nc';
url_grid[4] = 'http://geoport.whoi.edu/thredds/dodsC/bathy/srtm30plus_v6';
url_grid[5] = 'http://geoport.whoi.edu/thredds/dodsC/bathy/smith_sandwell_v9.1.nc';
url_grid[6] = 'http://geoport.whoi.edu/thredds/dodsC/bathy/smith_sandwell_v11';
 
 
# Get the data, choose i=1 till 6
i=6;
topogrid=getOpenDapURLAsSpatialGrid(url_grid[i] ,"topo",bboxInDegrees);
print(paste("mean:",mean(topogrid$topo)));
spplot(topogrid,at=c(-60:40,200),col.regions=bpy.colors,main=url_grid[i],xlab=paste("Mean: ",mean(topogrid$topo)))

#################################################################################
#####################################################################################

#calc
#drawExtent()
#e.g.
# reg.clim <- extract(tmin1.c, drawExtent())  # click twice to 
# # draw extent of the region of interest
# summary(reg.clim)

# plot(tmin1.c)
# click(tmin1.c, n = 3)  # click n times in the map to get values
###############################################################################################
url_name <- 'https://data.nodc.noaa.gov/thredds/dodsC/cortad/Version6/cortadv6_WeeklySST.nc'
#ncdf_grid <- nc_open('cortadv6_WeeklySST.nc',write = F, readunlim = T, verbose = F)
#print(ncdf_grid$dim$time)
url_brick <- brick(url_name)
plot(url_brick[[25]])
attributes(url_brick)

#### Subset to time (z) of interest--------------------------------------
z_time <- getZ(url_brick)
year_id <- which(z_time > as.Date('1999-12-31') & z_time < as.Date('2000-03-01'))

print(brick99 <- subset(url_brick, year_id))
plot(brick99[[2]])

# crop99 <- crop(brick99, ROI,
#                      filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/SST_crop99.grd',
#                      overwrite = T)
# plot(crop99[[5]])
# my_proj <- '+init=EPSG:4326 +proj=longlat +datum=WGS84 +lon_0=135 +no_defs +ellps=WGS84 +towgs84=0,0,0'
# 
# crop99_proj <- projectRaster(brick99, crs = my_proj)
# plot(crop99_proj[[5]])
# 
# my_eez_proj <- spTransform(my_eez, crs(my_proj))
# plot(my_eez_proj, add = T)


#### subset to area of interest ---------------
# Option 1, defind extent:
   # ROI <- extent(128,181,-27,13)
    # crop_roi <- crop(url_brick, ROI)
    #   plot(crop_roi[[5]])

# Option 2, draw extent on the map:
  # newext <- drawExtent() # click twice on the map to select the region of interest
  #   new_brick <- crop(url_brick, newext)
  #     plot(new_brick)

# Option 3, by a polygon feature:-----------------------------------------------------------
my_eez <- shapefile('F:/ReefCloud/EEZ_3countries/EEZ_3countries.shp')
   # my_eez_crop <- crop(my_eez, ROI)
    plot(my_eez, add = T)
      extent(my_eez)
  # crop_brick <- crop(url_brick, extent(my_eez),
  #                    filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/WeeklySST_crop.grd',
  #                    overwrite = T)
  # plot(crop_brick[[10]])
  
  
my_eez_crop2 <- mask(crop(brick99, extent(my_eez)),
                          filename = 'F:/ReefCloud/Covariates_ReefCloud/Outputs/SST_mask.tif',
                          overwrite = T,
                          my_eez)
plot(my_eez_crop2[[4]])

# have to reintroduce time into brick as it gets chopped during subsetting
crop2_time <- setZ(my_eez_crop2,z_time[year_id])
unique(year(z_time[year_id]))
#crop2_1k <- resample
# crop2_proj <- projectRaster(my_eez_crop2, crs = my_proj)
# plot(crop2_proj[[4]])
###################################################################################################
#########   Extract relevant months and years from the full brick #################################
###################################################################################################
# my_time <- which(month(z_time)== 06 &
#                    year(z_time) == 2000)
#   print(brick_06 <- subset(url_brick, my_time))
# plot(brick_06[[2]])                 
     
# library(foreach)
# library(doParallel)
# beginCluster(detectCores()-1)
# cl<- makeCluster(6)
# registerDoParallel(cl)
# 
# foreach(mon = iter(month(z_time[year_id])), .packages = c('raster', 'lubridate')) %dopar% {
#   sst_month <- subset(crop2_time,which(month(getZ(crop2_time)) == mon))- 273.15
#   print(sst_month) 
#   sst_montmean <- mean(sst_month,
#                        filename = paste0(mywd, "/",'Mean_Monthly_SST_',mon),
#                                    format = "GTiff", overwrite = TRUE,
#                                    datatype = 'FLT4S', progress = 'text') # converting to Celsius
#   
#   # sst_montmean_res <- resample(sst_montmean, gebco_grid_crop,
#   #                                  method = 'bilinear',
#   #                                  filename = paste0(mywd, "/",'Mean_Monthly_SST_',mon),
#   #                                  format = "GTiff", overwrite = TRUE,
#   #                                  datatype = 'FLT4S', progress = 'text')
#                                
#   sst_montmean_brick <- brick(sst_montmean, sst_montmean_brick)       
# }
# #endCluster()
# stopCluster(cl)

mywd <- 'F:/ReefCloud/Covariates_ReefCloud/Outputs'  
sst_montmean_brick <- brick()
for(i in unique(month(z_time[year_id]))) { # will have to adjust here for multiple years 
  # add doTry and parallel 
  sst_month <- subset(crop2_time,
                      which(month(getZ(crop2_time)) == i)) - 273.15 # converting to Celsius
  print(sst_month) 
  sst_montmean <- mean(sst_month, na.rm = T)
  writeRaster(sst_montmean,filename = paste0(mywd, "/",'Mean_Monthly_SST_',i),
                          format = "GTiff", overwrite = TRUE,
                          datatype = 'FLT4S', progress = 'text') 
  
  # sst_montmean_res <- resample(sst_montmean, gebco_grid_crop,
  #                                  method = 'bilinear',
  #                                  filename = paste0(mywd, "/",'Mean_Monthly_SST_',i),
  #                                  format = "GTiff", overwrite = TRUE,
  #                                  datatype = 'FLT4S', progress = 'text')
  print(sst_montmean)                             
  print(sst_montmean_brick <- brick(sst_montmean,sst_montmean_brick))
}         

plot(sst_montmean_brick)
writeRaster(sst_montmean_brick, filename = )


zoo <- zApply(crop2_time, by=month(getZ(crop2_time)), fun = 'mean', name='month' )
plot(zoo)
crop2_time[[1]]

# mean1 <- raster('F:/ReefCloud/Covariates_ReefCloud/Outputs/Mean_Monthly_SST_1.tif')
# plot(mean1)
# rm(mean1)
##################################################################################################

##### calculate mean by year---------------------------------------------------------
mean_99 <- mean(brick99)- 273.15
plot(mean_99)


# 
# pnt_exrct <- extract(recent_brick, my_point_data, method = 'bilinear')
# head(pnt_exrct)
