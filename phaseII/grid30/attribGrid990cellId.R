# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(raster);library(data.table)

memory.limit(5000000)

#######################################
## NOT DO: create a grid990 with grid cellId - this is not really needed, because it can be obtained with "extract"
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990.grd"
rast<-raster(rpth)
vals<-c(1:4559784)
rast[]<-vals
writeRaster(rast,filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990_wCellValues.grd",overwrite=T)


#######################################
# 1) Assign the 990 grid cellId

rast<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990_wCellValues.grd")
fpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/wObjId/fullyAttributed/"
prast<-projection(rast)
fls<-list.files(fpth)
for(ff in fls){
	print(ff)
	load(paste(fpth,ff,sep=""))
	xy30r68<-data.table(xy30r68)
	xy30<-data.table(xy30r68)
	coordinates(xy30)<-c("x","y")
	proj4string(xy30) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
	xy30<-spTransform(xy30,CRS(prast))
	xy30r68[,g990cellId:=extract(rast,xy30),]
	save(xy30r68,file=paste(fpth,ff,sep=""))
}


