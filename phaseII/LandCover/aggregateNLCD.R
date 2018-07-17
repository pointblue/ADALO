# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#First, read the file
#then crop it so that only half of the country results
#then extract all the possible categories of cover
#then aggregate with fact=33 with fun being the percent of cells with each land cover
#	results should become a brick
#then ovrlay the brick with the polygon and collapse it to a data.frame with cell Ids
#this is then attributed with the bird data to contrsuct the model 5


library(sp)
library(raster)
library(rgdal)
library(rgeos)

#### Skip this?
nlcd<-raster("V:/Data/vegetation/nlcd/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")

#load the r68 file to get the extent in the right projection
r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
ext68<-extent(r68)

#crop by extent
nlcd68<-crop(nlcd,ext68)

#write out
writeRaster(nlcd68,filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/nlcdR68.grd")

#need to get the values to do the aggregate...
#load the r68 file to get the extent in the right projection
r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
q<-slot(r68,"polygons")
r8<-SpatialPolygons(list(q[[1]]));r6<-SpatialPolygons(list(q[[2]]))
ext8<-extent(r8);ext6<-extent(r6)

#crop by extent
lc6<-crop(nlcd68,ext6);lc8<-crop(nldc68,ext8)

#write out
writeRaster(lc6,filename="c:/users/lsalas/desktop/refuge/nlcdR6.grd")
writeRaster(lc8,filename="c:/users/lsalas/desktop/refuge/nlcdR8.grd")

#load separately
r6<-raster("c:/users/lsalas/desktop/refuge/nlcdR6.grd")
vals6<-unique(values(r6))

r8<-raster("c:/users/lsalas/desktop/refuge/nlcdR8.grd")
vals8<-unique(values(r6))

vals<-unique(c(vals6,vals8))
save(vals6,vals8,vals,file="c:/users/lsalas/desktop/refuge/nlcdvals.RData")


###############################################################
#aggregate the raster
#get the categories of cover from above vals file
nlcd68<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/nlcdR68.grd")
load(file="c:/users/lsalas/desktop/refuge/nlcdvals.RData")

getLandCovPercent<-function(x,...){
	a<-round(100*sum(x==val)/1089)
	return(a)
}

nlcd68b<-list()
for(vv in vals){
	val=vv
	trst<-aggregate(nlcd68,fact=33,fun=getLandCovPercent)
	nlcd68b<-c(nlcd68b,trst)
}
nlcd68stk<-stack(nlcd68b[[2]],nlcd68b[[3]],nlcd68b[[4]],nlcd68b[[5]],nlcd68b[[6]],nlcd68b[[7]],nlcd68b[[8]],nlcd68b[[9]],nlcd68b[[10]],nlcd68b[[11]],nlcd68b[[12]],nlcd68b[[13]],nlcd68b[[14]],nlcd68b[[15]],nlcd68b[[16]],nlcd68b[[17]],nlcd68b[[18]])
nams<-paste("NLCD_",vals,sep="")
names(nlcd68stk)<-nams

writeRaster(nlcd68stk,filename="c:/users/lsalas/desktop/refuge/nlcd68stk.grd")


#Mask this guy?
r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
nlcd68stkm<-mask(nlcd68stk,r68)
writeRaster(nlcd68stkm,filename="c:/users/lsalas/desktop/refuge/nlcd68stk_masked.grd")





