# TODO: Add comment
# 
# Author: lsalas
###############################################################################

#Do this in TS3

library(raster)
library(sp)
library(rgdal)

######################################################################
#first I must convert the text names into integers
library(foreign)
pd<-read.dbf("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS/PADUS1_4Combined.dbf")
pd$ID<-1:nrow(pd)
ownNm<-data.frame(Own_Name=unique(pd$Own_Name))
ownNm$OwnKey<-1:nrow(ownNm)

mgtType<-data.frame(Mang_Type=unique(pd$Mang_Type))
mgtType$MgtKey<-1:nrow(mgtType)

locNm<-data.frame(Loc_Nm=unique(pd$Loc_Nm))
locNm$LocKey<-1:nrow(locNm)

sttNm<-data.frame(State_Nm=unique(pd$State_Nm))
sttNm$SttKey<-1:nrow(sttNm)

pd<-merge(pd,ownNm,by="Own_Name",all.x=T)
pd<-merge(pd,mgtType,by="Mang_Type",all.x=T)
pd<-merge(pd,locNm,by="Loc_Nm",all.x=T)
pd<-merge(pd,sttNm,by="State_Nm",all.x=T)
pd<-pd[order(pd$ID),]

save(ownNm,mgtType,locNm,sttNm, file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS/padusKeys.RData")
write.dbf(pd,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS/PADUS1_4Combined_wKeys.dbf")

#now replace the original dbf with this version
##############################################################################


#padus<-readOGR("V:/Data/landuse/current/PADUS1_4Shapefile",layer="PADUS1_4Combined")
padus<-readOGR("c:/users/lsalas/desktop/refuge/padus/padus",layer="PADUS1_4Combined")

grid<-raster("C:/Users/lsalas/Desktop/refuge/grid990.grd")
gridProj<-projection(grid)

tm<-Sys.time()
#padus<-spTransform(sppts,CRS(gridProj))
#gridPadus<-rasterize(padus,grid,field="OwnKey",fun=function(x,...){ux<-unique(x);b<-ux[which.max(tabulate(match(x, ux)))];return(as.integer(b))})
#gridPadus<-rasterize(padus,grid,field="OwnKey",fun=function(x,...){ux<-unique(x);if(NROW(ux)==1){b<-ux}else{b<-min(ux,na.rm=T)};return(as.integer(b))})
gridPadus<-rasterize(padus,grid,field="OwnKey",fun=min)
#writeRaster(gridPadus,filename="C:/Users/lsalas/Desktop/refuge/PADUSgrid990_OwnKey.grd",overwrite=T)
writeRaster(gridPadus,filename="/home/lsalas/refuge/PADUSgrid990_OwnKey.grd",overwrite=T)

#gridPadus<-rasterize(padusTr,grid,field="Mang_Type")
gridPadus<-rasterize(padus,grid,field="MgtKey",fun=min)
#writeRaster(gridPadus,filename="C:/Users/lsalas/Desktop/refuge/PADUSgrid990_MgmType.grd",overwrite=T)
writeRaster(gridPadus,filename="/home/lsalas/refuge/PADUSgrid990_MgmType.grd",overwrite=T)

#gridPadus<-rasterize(padusTr,grid,field="Loc_Nm")
gridPadus<-rasterize(padus,grid,field="LocKey",fun=min)
#writeRaster(gridPadus,filename="C:/Users/lsalas/Desktop/refuge/PADUSgrid990_LocNm.grd",overwrite=T)
writeRaster(gridPadus,filename="/home/lsalas/refuge/PADUSgrid990_LocNm.grd",overwrite=T)

#gridPadus<-rasterize(padus,grid,field="State_Nm")
gridPadus<-rasterize(padus,grid,field="SttKey",fun=min)
#writeRaster(gridPadus,filename="C:/Users/lsalas/Desktop/refuge/PADUSgrid990_State.grd",overwrite=T)
writeRaster(gridPadus,filename="/home/lsalas/refuge/PADUSgrid990_State.grd",overwrite=T)
Sys.Time()-tm


