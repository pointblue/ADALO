# TODO: Add comment
# 
# Author: lsalas
###############################################################################

#Do this in TS3

library(raster)
library(sp)
library(rgdal)

######################################################################
#This creates the keys for the padusCats table
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

