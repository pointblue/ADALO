# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## IN SOBIM

library(raster)
library(plyr)
rpth<-"/home/lsalas/adalo/"
tst<-raster(paste(rpth,"nlcdR68.grd",sep=""))
dd<-seq(1,ncell(tst),by=10000000);dd<-c(dd,4961676868+1)
for(aa in 1:(NROW(dd)-1)){
	sc<-dd[aa];ec<-(dd[aa+1]-1)
	xy30<-xyFromCell(tst,cell=sc:ec)
	filen<-paste("seg_",aa,".RData",sep="")
	save(xy30,file=paste(rpth,"grid30/",filen,sep=""))
}

crsinfo<-projection(tst)
save(crsinfo,file="/home/lsalas/adalo/grid30/projData.RData")


