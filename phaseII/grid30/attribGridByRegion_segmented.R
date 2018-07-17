# TODO: Add comment
# 
# Author: lsalas
###############################################################################

libs<-c("raster","rgdal","plyr","data.table","rgeos","sp")
lapply(libs, require, character.only = TRUE)

readpth<-"/home/lsalas/adalo/grid30/"
#readpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segmented/"
savepth<-"/home/lsalas/adalo/grid30R68/"
#savepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion"

r68<-readOGR("/home/lsalas/adalo/Region68polys","r68polygonsAEA")
#r68<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/FWSR68","r68polygonsAEA")

#load(paste(readpth,"gridProjData.RData",sep=""))  #no need: the polys are on the same projection as the grid data

segments<-list.files(readpth,pattern="seg")
for(ss in segments){
	load(paste(readpth,ss,sep=""));xy30<-as.data.frame(xy30)
	seqval<-as.numeric(substr(ss,5,regexpr(".RData",ss)-1))
	sstv<-((seqval-1)*10000000)+1
	ssev<-ifelse(seqval<497,seqval*10000000,((seqval-1)*10000000)+1676868)
	cid<-sstv:ssev
	xy30r68<-data.table(xy30)
	xy30r68<-xy30r68[,cellId:=cid,]
	coordinates(xy30)<-c("x","y")
	proj4string(xy30) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
	xy30r68[,Region:=over(xy30,r68["REGION_1"]),]
	xy30r68<-data.table(as.data.frame(xy30r68))
	xy30r68<-subset(xy30r68,!is.na(Region))
	if(nrow(xy30r68)==0){
		print(paste("Segment",seqval,"not in regions 6 or 8"))
	}else{
		save(xy30r68,file=paste(savepth,"r68_",ss,sep=""))
		rm(list=c("xy30r68","xy30"));gc()
		print(seqval)
	}
}


