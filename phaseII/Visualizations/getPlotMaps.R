# TODO: Add comment
# 
# Author: lsalas
###############################################################################


##This is in liu of makeMaps.R
## The goal is to download a map image, and then actively crop it to the bounding box of the data
library(ggmap)
library(rgdal)
library(sp)
library(raster)

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data/bySpecies/filteredByRange/"
mappth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/SurveyMaps/"
shapepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/RangeMaps/"

getFilterName<-function(sss){
	fln<-list.files(path=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/",sss,"_Dist",sep=""),pattern=".dbf")
	lyn<-substr(fln,1,nchar(fln)-4)
	return(lyn)
}


map<-get_map(location=c(lon=-110.75, lat=41.5),zoom=4,maptype="hybrid",source="google")

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

#need this function to retreive the layer name
getFilterName<-function(spv){
	fln<-list.files(path=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/",spv,"_Dist",sep=""),pattern=".dbf")
	lyn<-substr(fln,1,nchar(fln)-4)
	return(lyn)
}

makePlotMap<-function(df,sss,shapepth,mappth,seasonval){
	minlon<-min(df$lon,na.rm=T)-0.5;maxlon<-max(df$lon,na.rm=T)+0.5;
	minlat<-min(df$lat,na.rm=T)-0.5;maxlat<-max(df$lat,na.rm=T)+0.5;
	
	sss<-ifelse(sss=="WIFL","SW_WIFL",sss)
	fltlyr<-getFilterName(sss)
	spfilt<-readOGR(paste(shapepth,sss,"_Dist",sep=""),layer=fltlyr)
	projection(spfilt)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	spfilt<-fortify(spfilt)
	minlon<-ifelse(minlon>min(spfilt$long),min(spfilt$long),minlon)
	minlat<-ifelse(minlat>min(spfilt$lat),min(spfilt$lat),minlat)
	maxlon<-ifelse(maxlon<max(spfilt$long),max(spfilt$long),maxlon)
	maxlat<-ifelse(maxlat<max(spfilt$lat),max(spfilt$lat),maxlat)
	sxlim<-c(minlon,maxlon);sylim<-c(minlat,maxlat)
	plot<- ggmap(map, extent="normal", maprange=FALSE) + coord_map(projection="mercator", xlim=sxlim, ylim=sylim) + 
			geom_polygon(aes(x=long, y=lat), fill='grey', data=spfilt,alpha=0.5) +
			geom_point(data=df, aes(x=lon, y=lat, color=eventSuccess), size=1, alpha=0.5) + 
			scale_color_manual(values=c("red","blue")) +
			labs(x="",y="", colour="",title=paste("Sampling locations for",sss))
	
	jpeg(filename=paste(mappth,sss,"_",seasonval,"_presence.jpg",sep=""),width=1000,height=1000,quality=100)
		print(plot)
	dev.off()
	return("1")
}

fls<-list.files(path=pth)
fls<-subset(fls,substr(fls,1,4) %in% substr(seasons$SpCode,1,4))

for(fff in fls){
	sss<-ifelse(!grepl("RIRA",fff,fixed=T),substr(fff,1,4),
			ifelse(grepl("CA_RIRA",fff,fixed=T),substr(fff,1,7),substr(fff,1,9)))
	load(paste(pth,fff,sep=""))
	plot.df$obsDate<-as.Date(as.character(plot.df$obsDate),"%Y-%m-%d")
	plot.df$month<-as.numeric(format(plot.df$obsDate,"%m"))
	
	br.plot.df<-getBrEvents(df=plot.df,seasons=seasons,spv=sss)
	wi.plot.df<-getWiEvents(df=plot.df,spv=sss)
	
	bv<-makePlotMap(df=br.plot.df,sss=sss,shapepth=shapepth,mappth=mappth,seasonval="breeding")
	wv<-makePlotMap(df=wi.plot.df,sss=sss,shapepth=shapepth,mappth=mappth,seasonval="winter")
	print(paste(sss,bv,wv))
}

#