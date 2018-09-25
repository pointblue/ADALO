# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#make baserast
library(raster);library(data.table)

memory.limit(5000000)

#######################################
# 0) DONE: create a grid990 with grid cellId - this is not really needed, because it can be obtained with "extract"
# Also done in /grid30/attribGrid990cellId.R
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990.grd"
rast<-raster(rpth)
vals<-c(1:4559784)
rast[]<-vals
writeRaster(rast,filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990_wCellValues.grd",overwrite=T)


#######################################
# 1) DONE: Assign the 990 grid cellId, create the intersect by aggregating numcells

rast<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990_wCellValues.grd")
fpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/wObjId/fullyAttributed/"
prast<-projection(rast)
fls<-list.files(fpth)
baselst<-list();i<-0
for(ff in fls){
	print(ff)
	load(paste(fpth,ff,sep=""))
	xy30r68<-data.table(xy30r68)
	xy30<-data.table(xy30r68)
	coordinates(xy30)<-c("x","y")
	proj4string(xy30) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
	xy30<-spTransform(xy30,CRS(prast))
	xy30r68[,g990cellId:=extract(rast,xy30),]
	basetmp<-aggregate(cellId~g990cellId+USFWSregion+pdobjid+LCCregion+USFSregion+NPSregion+USJVregion+BCRregion+StateFIPS+CountyFIPS,data=xy30r68,FUN=NROW)
	i<-i+1;baselst[[i]]<-basetmp
}
basetable<-rbindlist(baselst)
basetable$NPS<-ifelse(basetable$NPSregion=="Midwest",1,ifelse(basetable$NPSregion=="Intermountain",2,3))
basetable<-basetable[,which(names(basetable)!="NPSregion")]
basetable$Regions<-paste(basetable$USFWSregion,substr(basetable$USFS,2,2),basetable$NPS,
		ifelse(nchar(basetable$LCCregion)==1,paste("0",basetable$LCCregion,sep=""),basetable$LCCregion),
		ifelse(nchar(basetable$USJVregion)==1,paste("0",basetable$USJVregion,sep=""),basetable$USJVregion),
		ifelse(nchar(basetable$BCRregion)==1,paste("0",basetable$BCRregion,sep=""),basetable$BCRregion),sep="")
names(basetable)<-gsub("NPS","NPSregion",names(basetable))
basetable$intId<-c(1:nrow(basetable))
save(basetable,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/warehouse/basetable.RData")

#############################################
# 2) Loop through each table of raster results, merge with basetable by g990cellId, remove NA's, use [metric, g990cellId, pdobjid, Regions] to write to sql table
# experiment first with creating a file per species
savepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/warehouse/"
load(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/warehouse/basetable.RData")
spp<-c("BAIS","BLRA","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","LETE","MAGO","MOPL","NOPI","RIRA","SACR","SNPL","SPPI","TRBL","WIFL")
flpthm4<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4rasterNew/asTables/";m4fls<-list.files(flpthm4)
flpthm5<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/asTables/";m5fls<-list.files(flpthm5)
flpthhapet<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/hapet/asTables/";hapetfls<-list.files(flpthhapet)
flpthecn<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/ecn/asTables/";ecnfls<-list.files(flpthecn)

processAsTable<-function(flslst,ss,flspth,mv,basetable){
	ff<-subset(flslst,grepl(ss,flslst,ignore.case=TRUE))
	load(paste(flspth,ff,sep=""))
	names(spdf)<-gsub(ss,"metricValue",names(spdf))
	spdf$metric<-mv;spdf$species<-ss
	sptbl<-merge(basetable,spdf,by="g990cellId")
	sptbl<-sptbl[,c("intId","species","period","g990cellId","pdobjid","Regions","USFWSregion","NPSregion","LCCregion","USFSregion","USJVregion","BCRregion","StateFIPS","CountyFIPS","ncells","metric","metricValue")] 
	sptbl$cellMetric<-sptbl$ncells*sptbl$metricValue
	sptbl<-data.table(sptbl)
	return(sptbl)
}

for(ss in spp){
	splst<-list();i<-0
	if(TRUE %in% grepl(ss,m4fls)){
		sptbl<-processAsTable(flslst=m4fls,ss=ss,flspth=flpthm4,mv=4,basetable=basetable)
		sptbl<-subset(sptbl,metricValue>0)
		i<-i+1;splst[[i]]<-sptbl
	}
	if(TRUE %in% grepl(ss,m5fls)){
		sptbl<-processAsTable(flslst=m5fls,ss=ss,flspth=flpthm5,mv=5,basetable=basetable)
		i<-i+1;splst[[i]]<-sptbl
	}
	if(TRUE %in% grepl(ss,hapetfls)){
		sptbl<-processAsTable(flslst=hapetfls,ss=ss,flspth=flpthhapet,mv=6,basetable=basetable)
		i<-i+1;splst[[i]]<-sptbl
	}
	if(TRUE %in% grepl(ss,ecnfls)){
		sptbl<-processAsTable(flslst=ecnfls,ss=ss,flspth=flpthecn,mv=7,basetable=basetable)
		i<-i+1;splst[[i]]<-sptbl
	}
	speciesdf<-rbindlist(splst)
	save(speciesdf,file=paste(savepth,ss,".RData",sep=""))
	print(paste("done with",ss))
}



