# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(raster)
library(rgdal)
library(sp)
library(rgeos)

#adding the bioclimatics, not NWI for now
#get the list of bioclimatics
#load the NLCD stack
#loop through the bioclimatics and add them to the stack
#save the stack
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2"
nlcd68stk<-stack(paste(pth,"/NLCD/nlcd68stk_masked.grd",sep=""))

fls<-list.files(paste(pth,"/BioClimGrids",sep=""))
fls<-subset(fls,!fls %in% c("Old","zipped"))
vdf<-read.csv(paste(pth,"/BioClimGrids/BioClim_Vars.csv",sep=""))
vdfn<-as.character(vdf$File.Name);vdfn<-subset(vdfn,vdfn!="")
vdfn[which(!vdfn %in% fls)]

#missing "elev_std_c" so
vdfn<-subset(vdfn,vdfn!="elev_std_c")

for (fle in vdfn){
	print(fle)
	rast<-raster(paste(pth,"/BioClimGrids/",fle,sep=""))
	nlcd68stk<-stack(nlcd68stk,rast)
}

r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
covars_m<-mask(nlcd68stk,r68)
writeRaster(covars_m,filename=paste(pth,"/NLCD/covars_masked.grd",sep=""))


