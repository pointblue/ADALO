# TODO: Add comment
# 
# Author: lsalas
###############################################################################

libs<-c("raster","rgdal","rgeos","sp","data.table")
lapply(libs, require, character.only = TRUE)


###Starting with the stack of NWI/NLCD/CDL
# Order of aggregation: I WILL IGNORE THIS!
#	(1) If a cell has an NWI type, it gets that value, else
#	(3) If cell overlaps with USDA CDL ag type, it gets that value, else
#	(4) Assign the NLCD value.

rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"

#read NLCD
nlcd<-stack(paste(rpth,"NLCD/nlcd68stk_masked.grd",sep=""))
#must reclass 22+23+24=NLCD_devel
#must reclass 90+95=NLCD_Wetl
#drop NLCD_0 and the aggregated ones
nlcd_devel<-nlcd[[9]]+nlcd[[10]]+nlcd[[16]];names(nlcd_devel)<-"NLCD_devel"
nlcd<-stack(nlcd,nlcd_devel)
nlcd_wetl<-nlcd[[3]]+nlcd[[12]];names(nlcd_wetl)<-"NLCD_wetl"
nlcd<-stack(nlcd,nlcd_wetl)
nlcd<-nlcd[[-16]];nlcd<-nlcd[[-12]];nlcd<-nlcd[[-10]];nlcd<-nlcd[[-9]];nlcd<-nlcd[[-3]];nlcd<-nlcd[[-1]]

#load the nwi and cdl stacks, convert to percentages
#CDL - convert to percentages
cdl<-stack(paste(rpth,"CropData/CDLstack_masked.grd",sep=""))
cdl100<-calc(cdl,fun=function(x)round(x*100/1089));names(cdl100)<-names(cdl)

#NWI - convert to percentages, except 108
nwi<-stack(paste(rpth,"NWI/NewClassification/rasters/NewNWIstack_masked.grd",sep=""))
nwi<-nwi[[-14]]#no variance after the mask
nwi100<-calc(nwi,fun=function(x)round(x*100/1089));names(nwi100)<-names(nwi)

#Climate
clim<-stack(paste(rpth,"BioClimGrids/stack/bioclim_masked.grd",sep=""))

#Regionvars
regions<-stack(paste(rpth,"RegionVars/regionstk_masked.grd",sep=""))

covarstack<-stack(nlcd,cdl100)
covarstack<-stack(covarstack,nwi100)
covarstack<-stack(covarstack,clim)
covarstack<-stack(covarstack,regions)

names(covarstack)<-gsub("key_","nwi_",names(covarstack))

writeRaster(covarstack,filename=paste(rpth,"CovarStack/covarstack_masked.grd",sep=""),overwrite=TRUE)




