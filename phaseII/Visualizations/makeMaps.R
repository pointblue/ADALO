# TODO: Add comment
# 
# Author: lsalas
###############################################################################


######################## THIS IS STEP III  ###########################################

#####  ATTENTION: this file ended up being used to generate the species' filtered data, plots are now made with getPlotMaps.R

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/bySpecies/"
mappth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/SurveyMaps/"

library(rgdal)
library(sp)
library(raster)

#use this util I made to plot on google maps
source("c:/users/lsalas/git/sparklemotion/lsalas_rcode/LSalas/Maps/mapLocationsUtil.R")

#need this function for further spatial filtering
addSpatialFilter<-function(df,spv,filtname){
	spfilt<-readOGR(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/",spv,"_Dist",sep=""),layer=filtname)
	projection(spfilt)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	sppts<-df[,c("lon","lat")]
	coordinates(sppts)<-c("lon","lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	#sppts.spf<-spTransform(sppts,CRS(spfProj))
	spfilt.attr<-over(sppts,spfilt)
	if(TRUE %in% grepl("OBJECTID",names(spfilt.attr),fixed=T)){
		df$distFilter<-spfilt.attr$OBJECTID
	}else if(TRUE %in% grepl("PRESENCE",names(spfilt.attr),fixed=T)){
		df$distFilter<-spfilt.attr$PRESENCE
	}else{
		fld<-names(spfilt.attr)[1]
		df$distFilter<-spfilt.attr[,fld]
	}
	
	df<-subset(df,!is.na(distFilter))
	return(df)
}

#need this function to retreive the layer name
getFilterName<-function(spv){
	fln<-list.files(path=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/",spv,"_Dist",sep=""),pattern=".dbf")
	lyn<-substr(fln,1,nchar(fln)-4)
	return(lyn)
}

#and this function saves the map
makeMapFile<-function(data,zs,titl,mappth,spv,...){
	#pp<-create_plot(df=data,mt="terrain", zs = zs, latFieldName="lat", lonFieldName="lon",pz=2,titl=titl,sourcem="osm") #try decreasing sz (3 = continent level) until all points are plotted
	#jpeg(filename=paste(mappth,spv,".jpg",sep=""),width=1000,height=1000,quality=100)
	#	print(pp)
	#dev.off()
	ppe<-create_plot(df=data,map_dot_color="eventSuccess",mt="terrain", zs = zs, latFieldName="lat", lonFieldName="lon",pz=2,titl=titl,sourcem="osm") 
	jpeg(filename=paste(mappth,spv,"_presence.jpg",sep=""),width=1000,height=1000,quality=100)
		print(ppe)
	dev.off()
	return(1)
}

#
spcd=c("BRCO","BLRA","LETE","RIRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL","SNPL","MOPL",
		"MCLO","CCLO","BUOW","SPPI","BAIS","BEVI","RNPH","BOBO","GRSG","FEHA","BBSA","MAGO")


#get a map using get_map function, then crop it by the bounding box, and then capture to plot with ggmap(map)

#zsv<-c(6,5,6,6,5,5,5,5,5,6)
for(sss in spcd){
	load(paste(pth,sss,".RData",sep=""))
	spdf<-aggregate(as.formula("count~source+species+obsDate+lon+lat"),data=spdf,FUN=sum)
	spdf$eventSuccess<-ifelse(spdf$count>0,"positive","negative")
	spv<-sss
	#zs<-ifelse(spv %in% c("BRCO","RIRA","TRBL"),6,ifelse(spv %in% c("LETE","BLRA"),5,4))
	
	if(spv=="RIRA"){
		#add filter for yumaensis dist
		spvy<-"Yuma_RIRA"
		fltlyr<-getFilterName(spvy)
		plot.df<-addSpatialFilter(df=spdf,spv=spvy,filtname=fltlyr)
		#titl<-"Yuma RIRA survey event locations"
		save(plot.df,file=paste(pth,"filteredByRange/",spvy,"_filtered.RData",sep=""))
		#r<-makeMapFile(data=plot.df,zs=zs,titl=titl,mappth=mappth,spv=spvy)
		
		#add filter for California dist
		spvc<-"CA_RIRA"
		fltlyr<-getFilterName(spvc)
		plot.df<-addSpatialFilter(df=spdf,spv=spvc,filtname=fltlyr)
		#titl<-"California RIRA survey event locations"
		save(plot.df,file=paste(pth,"filteredByRange/",spvc,"_filtered.RData",sep=""))
		#r<-makeMapFile(data=plot.df,zs=zs,titl=titl,mappth=mappth,spv=spvc)
		
	}else if(spv=="WIFL"){
		#add filter for SW WIFL
		spv<-"SW_WIFL"
		fltlyr<-getFilterName(spv)
		plot.df<-addSpatialFilter(df=spdf,spv=spv,filtname=fltlyr)
		#titl<-"Southwestern WIFL survey event locations"
		save(plot.df,file=paste(pth,"filteredByRange/",sss,"_filtered.RData",sep=""))
		#r<-makeMapFile(data=plot.df,zs=zs,titl=titl,mappth=mappth,spv=spv)
		
	}else{
		fltlyr<-getFilterName(spv)
		plot.df<-addSpatialFilter(df=spdf,spv=spv,filtname=fltlyr)
		#titl<-paste(spv,"survey event locations")
		head(plot.df)
		save(plot.df,file=paste(pth,"filteredByRange/",sss,"_filtered.RData",sep=""))
		#r<-makeMapFile(data=plot.df,zs=zs,titl=titl,mappth=mappth,spv=spv)
	}
		
}
