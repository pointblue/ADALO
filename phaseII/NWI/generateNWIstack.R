# TODO: Add comment
# 
# Author: lsalas
###############################################################################



## SOOO what's the diff between all and small? Small has only a few 990 cells

### ATTENTION ######################################################
## This first part must run in my laptop
#load the 990 raster
#load each NWI RData, aggregate something by key and id990
#create a raster for each value of key, subset by the key, assign the cells in id990 the value of the aggregate (i.e., count or % if all)

# Order of aggregation:
#	(1) If a 30m cell overlaps with an NWI "all" cell, it gets that value, else
#	(2) If a 30 m cell overlaps with NWI "small" cell, it gets that value, else
#	(3) If cell overlaps with USDA CDL ag type, it gets that value, else
#	(4) Assign the NLCD value.

libs<-c("raster","plyr","data.table")
lapply(libs, require, character.only = TRUE)

rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
r990<-raster(paste(rpth,"Grid990/base_grid990.tif",sep=""))

fls<-list.files(path=paste(rpth,"NWI/NewClassification/rasters/",sep=""),pattern=".RData")
fls<-subset(fls,!grepl(".tif",fls))
allfls<-subset(fls,grepl("_all",fls))
smallfls<-subset(fls,grepl("small",fls))
files.df<-data.frame(all=allfls,small=smallfls,stringsAsFactors=F)
files.df$pairId<-1:nrow(files.df)

#aggregate and generate count by id990 and key, rbind across all alls and all smalls
mergeAllSmall<-function(x,files.df=files.df){
	ff<-subset(files.df,pairId==x)
	aa<-ff$all;ss<-ff$small
	load(paste(rpth,"NWI/NewClassification/rasters/",aa,sep=""))
	adf<-aggregate(as.formula("cellId~key+id990"),data=df,FUN=NROW)
	names(adf)<-c("key","id990","ncellsAll")
	load(paste(rpth,"NWI/NewClassification/rasters/",ss,sep=""))
	sdf<-aggregate(as.formula("cellId~key+id990"),data=df,FUN=NROW)
	names(sdf)<-c("key","id990","ncellsSmall")
	mdf<-merge(adf,sdf,by=c("key","id990"),all=T)
	rm(list=c("df","adf","sdf"));gc()
	mdf<-data.table(mdf)
	mdf[,cellValue:=ifelse(!is.na(ncellsAll),ncellsAll,ncellsSmall),]
	return(mdf)
}

jointdf<-ldply(.data=1:(nrow(files.df)),.fun=function(x){rdf=mergeAllSmall(x,files.df=files.df);return(rdf)})
maxdf<-aggregate(as.formula("cellValue~key+id990"),data=jointdf,FUN=max)

nwistack<-r990;names(nwistack)<-"base"
for(kk in unique(maxdf$key)){
	rdf<-subset(maxdf,key==kk)
	cid<-as.integer(rdf$id990)
	civ<-as.integer(rdf$cellValue)
	r990[]<-NA
	r990[cid]<-civ
	names(r990)<-paste("key_",kk,sep="")
	nwistack<-stack(nwistack,r990)
}
nwistack<-nwistack[[-1]]

writeRaster(nwistack, filename=paste(rpth,"NWI/NewClassification/rasters/NewNWIstack.grd",sep=""), bandorder="BIL", overwrite=TRUE)
#writeRaster(nwistack, filename=paste(rpth,"NWI/NewClassification/rasters/NewNWIstack.tif",sep=""), format="GTiff",options="INTERLEAVE=BAND", overwrite=TRUE)
save(maxdf,file=paste(rpth,"NWI/NewClassification/rasters/NewNWIstack.RData",sep=""))

r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
nwistack_m<-mask(nwistack,r68)
writeRaster(nwistack_m,filename=paste(rpth,"NWI/NewClassification/rasters/NewNWIstack_masked.grd",sep=""),overwrite=TRUE)

