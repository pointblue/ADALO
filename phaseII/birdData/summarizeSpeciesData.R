# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(raster)
library(rgdal)
library(sp)

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data/bySpecies/filteredByRange/"
shapepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/RangeMaps/"

rast<-raster(xmn=-124.4004, xmx=-94.60802, ymn=32.53513, ymx=49.00027, crs=CRS("+proj=longlat +datum=WGS84"),nrows=980,ncols=1260,vals=1)

seasons<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Seasons.csv")

getBrEvents<-function(df,seasons,spv){
	ssn<-subset(seasons,SpCode==spv)
	begm<-ssn$BrStart;endm<-ssn$BrEnd
	df<-subset(df,month %in% seq(begm,endm))
	return(df)
}

getWiEvents<-function(df,spv){
	if(spv=="CA_RIRA"){
		df<-subset(df,month %in% c(10,11,12))
	}else{
		df<-subset(df,month %in% c(11,12,1))
	}
	return(df)
}

getStats<-function(df,seasonval,seasons,spv){
	if(seasonval=="breeding"){
		df<-getBrEvents(df,seasons,spv)
	}else{
		df<-getWiEvents(df,spv)
	}
	rawpct<-sum(df$eventSuccess=="positive")/nrow(df)
	rd<-extract(rast,df[,c("lon","lat")],cellnumbers=TRUE,df=TRUE)
	df$cellId<-rd$cells
	cp<-subset(df,eventSuccess=="positive")
	cellpct<-NROW(unique(cp$cellId))/NROW(unique(df$cellId))
	tdf<-data.frame(species=spv, season=seasonval, rawPct=rawpct,cellPct=cellpct)
	return(tdf)
}

fls<-list.files(path=pth)
fls<-subset(fls,substr(fls,1,4) %in% substr(seasons$SpCode,1,4))

summ.df<-data.frame()
for(fff in fls){
	sss<-ifelse(!grepl("RIRA",fff,fixed=T),substr(fff,1,4),
			ifelse(grepl("CA_RIRA",fff,fixed=T),substr(fff,1,7),substr(fff,1,9)))
	load(paste(pth,fff,sep=""))
	plot.df$obsDate<-as.Date(as.character(plot.df$obsDate),"%Y-%m-%d")
	plot.df$month<-as.numeric(format(plot.df$obsDate,"%m"))
	
	#split into breeding and winter
	bdf<-getStats(df=plot.df,seasonval="breeding",seasons=seasons,spv=sss)
	summ.df<-rbind(summ.df,bdf)
	wdf<-getStats(df=plot.df,seasonval="winter",seasons=seasons,spv=sss)
	summ.df<-rbind(summ.df,wdf)
}

write.csv(summ.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/summary_eventsDetections.csv")

