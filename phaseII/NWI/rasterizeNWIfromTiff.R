# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(raster)
library(sp)
library(rgdal)



#base grid
#baserast<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/nlcdR68.grd")
baserast<-raster("/home/lsalas/adalo/base990/grid990.grd")
extb<-extent(baserast)

#load the tiffs from list
#grdpth<-"C:/Users/lsalas/Dropbox/FWS_I&M/rasterized_nwi_features/"
grdpth<-"/home/lsalas/adalo/rasterized_nwi_features/"

grds<-list.files(grdpth,pattern = ".tif")
grds<-subset(grds,!grepl(".tif.",grds,fixed=T))

#Not using mode anymore
Mode<-function(x,...){
	ux<-na.omit(unique(x))
	mx<-ux[which.max(tabulate(match(x, ux)))]
	return(mx)
}

processWetlandRaster<-function(rast,baserast,val,...){
	arast<-aggregate(rast,fact=33,fun=sum,na.rm=TRUE)
	arast<-calc(arast,fun=function(x){pv<-ceiling((x/val)*100/1089)})
	arast<-calc(arast,fun=function(x){y<-ifelse(is.na(x),0,x);return(y)})
	erast<-extend(arast,baserast)
	if(!identical(dim(erast),dim(baserast))){
		erast<-crop(erast,baserast)
	}
	extent(erast)<-extent(baserast)
	return(erast)
}

riparian<-stack()
estuarine<-stack()
deepwater<-stack()
for(gg in grds[16:19]){
	rast<-raster(paste(grdpth,gg,sep=""))
	print(gg)
	if(gg=="CA_south_forested_estuarine_wetland_and_deepwater_rasterize_30_m_with_snap.tif"){
		rast<-calc(rast,fun=function(x){y<-floor(x*3/2);return(y)})
		rast<-calc(rast, fun=function(x){y<-ifelse(x==4,2,x);return(y)})
	}
	crast<-crop(rast,extb,snap="near")
	rm(list="rast");gc()
	
	#here, we need to aggregate separately the 1's from the 2's from the 3's
	cripa<-calc(crast,fun=function(x){y<-ifelse(x==1,1,NA);return(y)})
	cestu<-calc(crast,fun=function(x){y<-ifelse(x==3,3,NA);return(y)})
	cdeep<-calc(crast,fun=function(x){y<-ifelse(x==2,2,NA);return(y)})
	rm(list="crast");gc()
	
	eripa<-processWetlandRaster(rast=cripa,baserast=baserast,val=1)
	eestu<-processWetlandRaster(rast=cestu,baserast=baserast,val=3)
	edeep<-processWetlandRaster(rast=cdeep,baserast=baserast,val=2)
	rm(list=c("cripa","cestu","cdeep"));gc()
	
	riparian<-stack(riparian,eripa)
	estuarine<-stack(estuarine,eestu)
	deepwater<-stack(deepwater,edeep)
	rm(list=c("eripa","eestu","edeep"));gc()
}
nlayers(riparian)==NROW(grds)

#now collapse each stack
nwiriparian<-max(riparian,na.rm=T)
nwiestuarine<-max(estuarine,na.rm=T)
nwideep<-max(deepwater,na.rm=T)
rm(list=c("riparian","deepwater","estuarine"));gc()


#stack' em
nwiwetlands2<-stack(nwiriparian,nwiestuarine,nwideep)
names(nwiwetlands2)<-c("nwiriparian","nwiestuarine","nwideepwater")
b<-writeRaster(nwiwetlands2,filename="/home/lsalas/adalo/nwiraster.grd",overwrite=T)
#b<-writeRaster(nwiwetlands,filename="/home/lsalas/adalo/nwiraster.tif",format="GTiff",overwrite=T)

############# In case need to rename
library(raster)
library(rgdal)
library(sp)

nwi<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/NWI990/nwiraster.grd")
names(nwi)<-c("nwiriparian","nwideepwater","nwiestuarine")
z<-writeRaster(nwi,filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/NWI990/nwiraster2.grd")


