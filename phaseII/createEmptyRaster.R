# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#raster(nrows, ncols, xmn=-180, xmx=180, ymn=-90, ymx=90,
#		crs, ext, resolution, vals=NULL)
#ext = xmin, ymin, xmax, ymax  - so use one or the other
#resolution=c(200,200) this is xres,yres = the number of rows and columns = nrows, ncols, so use one or the other

library(raster)
library(rgdal)
library(sp)
library(geosphere)
library(rgeos)

nr<-distGeo(c(-124.420735,32.6),c(-94.61085,32.6))/1000
#looking for an exact number of kms
nc<-distGeo(c(-94.61085,32.59315),c(-94.61085,49))/1000

rast<-raster(xmn=-124.420735, xmx=-94.61085, ymn=32.59315, ymx=49.0, crs=CRS("+proj=longlat +datum=WGS84"),nrows=nr,ncols=nc,vals=1)
save(rast,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/1kmRast.RData")
#rastdd<-projectRaster(from=rast,crs=CRS("+proj=longlat +datum=WGS84"))

#load and filter the data by the extent (need to convert to UTMs)
map<-readOGR("V:/Data/jurisdiction/terrestrial/usfws","FWS_Regional_Boundaries")
#mapu<-spTransform(map,CRS("+proj=utm +zone=10 +north ellps=WGS84"))

plot(rast)
plot(map,add=T)

#read the nlcd raster
nlcd<-raster("V:/Data/vegetation/nlcd/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")

#load the shapefile for r 8 and 6
r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygons")
#need to reproject the polygons to the raster projection
r68n<-spTransform(r68,CRS(projection(nlcd)))
nlcd68<-mask(nlcd,r68n)

plot(nlcd68)
