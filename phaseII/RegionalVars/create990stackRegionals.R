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
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
r990<-raster(paste(rpth,"Grid990/base_grid990.tif",sep=""))

fls<-c("bcr_p_c","epa_iii_p_c")

regionstk<-r990
for (fle in fls){
	print(fle)
	rast<-raster(paste(rpth,"/RegionVars/",fle,sep=""))
	crast<-crop(rast,r990)
	regionstk<-stack(regionstk,crast)
}
regionstk<-regionstk[[-1]]
lon <- init(r990, 'x');regionstk<-stack(regionstk,lon)
lat <- init(r990, 'y');regionstk<-stack(regionstk,lat)
names(regionstk)<-c("bcr_p_c","epa_iii_p_c","lon","lat")
r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
regionstk_m<-mask(regionstk,r68)
writeRaster(regionstk_m,filename=paste(rpth,"RegionVars/regionstk_masked.grd",sep=""),overwrite=T)

