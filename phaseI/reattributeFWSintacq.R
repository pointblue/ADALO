# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#read current collate.df
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")
#load("/home/lsalas/Documents/refuge/collated.RData")

#load the layers
library(rgdal)
library(sp)
library(raster)
fwsAppr<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/geodata/IandM_cadastral_R8R2",layer="FWSApproved201507")
fwsProj<-projection(fwsAppr)
fwsInt<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/geodata/IandM_cadastral_R8R2",layer="FWSInterest201507")

#prepare intersect
sppts<-unique(collated.df[,c("Lon","Lat")])
sppts$pointId<-c(1:nrow(sppts))
collated.df<-merge(collated.df,sppts,all.x=TRUE)
coordinates(sppts)<-c("Lon","Lat")
proj4string(sppts)<-CRS(fwsProj)	
#no need to reproject - points are in teh same projection as the shapefiles

#intersect
fwsAppr.attr<-over(sppts,fwsAppr)
fwsInt.attr<-over(sppts,fwsInt)
sppts$fwsAppr2015<-fwsAppr.attr$ORGNAME
sppts$fwsApprType<-fwsAppr.attr$APPTYPE
sppts$fwsInt2015<-fwsInt.attr$INTTYPE1
sppts$fwsRSLType<-fwsInt.attr$RSL_TYPE
sppts$fwsStatus<-fwsInt.attr$STATUS
sppts$fwsORGN<-fwsInt.attr$ORGNAME
#need to filter NWR and WMA by Status=0 (interest layer) for Narrow definition (and for Encompassing too)
#need to get ORGNAME from Interest layer too, because Approved layer is not all-encompassing
sppts.df<-as.data.frame(sppts)
collated.df<-merge(collated.df,sppts.df[,c("pointId","fwsAppr2015","fwsApprType","fwsInt2015","fwsRSLType","fwsStatus","fwsORGN")],by="pointId",all.x=TRUE)
collated.df$orgName<-ifelse(!is.na(collated.df$fwsAppr2015),collated.df$fwsAppr2015,
		ifelse(!is.na(collated.df$fwsORGN),collated.df$fwsORGN,NA))
#save collated.df
save(collated.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")