# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(raster)
library(rgdal)
library(sp)
library(rgeos)

#adding the bioclimatics, not NWI for now
# ATTENTION: we are NOT integerizing all
#	elev, aet and precipitation stay the same, just integers
#	temperature and stdev are x100 then rounded, then integerized

rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
r990<-raster(paste(rpth,"Grid990/base_grid990.tif",sep=""))

fls<-list.files(paste(rpth,"/BioClimGrids",sep=""))
fls<-subset(fls,!fls %in% c("Old","zipped","Bioclim_Vars.csv","stack"))
vdf<-read.csv(paste(rpth,"/BioClimGrids/BioClim_Vars.csv",sep=""))
vdfn<-as.character(vdf$File.Name);vdfn<-subset(vdfn,vdfn!="")
vdfn[which(!vdfn %in% fls)]

#variables to integerize
intv<-c("elev_std_c","tavg_spc","tavg_suc","tavg_fac","tavg_wic","tmax_spc","tmax_suc","tmax_fac","tmax_wi_avc","tmax_wm2c",
		"tmin_cm_prc","tmin_spc","tmin_suc","tmin_fac","tmin_wi_avc","temp_range")

bioclimstk<-r990
for (fle in vdfn){
	print(fle)
	rast<-raster(paste(rpth,"/BioClimGrids/",fle,sep=""))
	#if(fle %in% intv){ 	#convert to integers where needed
	#	rast<-calc(rast,fun=function(x)as.integer(round(x*100)))
	#	names(rast)<-fle
	#}else{
	#	rast<-calc(rast,fun=function(x)as.integer(x))
	#	names(rast)<-fle
	#}
	bioclimstk<-stack(bioclimstk,rast)
}

bioclimstk<-bioclimstk[[-1]]
r68<-readOGR(paste(rpth,"mask/Regions6and8",sep=""),"r68polygonsAEA")
bioclimstk_m<-mask(bioclimstk,r68)

writeRaster(bioclimstk_m,filename=paste(rpth,"BioClimGrids/stack/bioclim_masked.grd",sep=""),overwrite=TRUE)


