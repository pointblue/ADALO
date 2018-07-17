# TODO: Add comment
# 
# Author: lsalas
###############################################################################


######################## THIS IS STEP III  ###########################################

#####  ATTENTION: this file ended up being used to generate the species' filtered data, plots are now made with getPlotMaps.R

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/bySpecies/"
mappth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/RangeMaps/Enhanced/"

library(rgdal)
library(sp)
library(raster)
library(data.table)

#use this util I made to plot on google maps
source("c:/users/lsalas/git/sparklemotion/lsalas_rcode/LSalas/Maps/mapLocationsUtil.R")
seasons<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/seasons.csv",stringsAsFactors=FALSE)

#need this function for further spatial filtering
addSpatialFilter<-function(df,spv,seas,mappth,layern){
	seasp<-ifelse(seas=="b","_b_","_w_")
	seaslp<-ifelse(seas=="b","breeding/","wintering/")
	
	spfilt<-readOGR(paste(mappth,seaslp,spv,seasp,"Dist",sep=""),layer=layern)
	projection(spfilt)<-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
	sppts<-df[,c("lon","lat")]
	coordinates(sppts)<-c("lon","lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	sppts.spf<-spTransform(sppts,CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
	spfilt.attr<-over(sppts.spf,spfilt)
	if(TRUE %in% grepl("OBJECTID",names(spfilt.attr),fixed=T)){
		df$distFilter<-spfilt.attr$OBJECTID
	}else if(TRUE %in% grepl("PRESENCE",names(spfilt.attr),fixed=T)){
		df$distFilter<-spfilt.attr$PRESENCE
	}else{
		fld<-names(spfilt.attr)[1]
		df$distFilter<-spfilt.attr[,fld]
	}
	
	df<-data.table(subset(df,!is.na(distFilter)))
	return(df)
}

#NO need anymore- need this function to retreive the layer name
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

getDateLims<-function(spdf,ssnd){
	bms<-ssnd$BrMoStart;bds<-ssnd$BrDyStart;bme<-ssnd$BrMoEnd;bde<-ssnd$BrDyEnd
	bmst<-ifelse(nchar(bms)==1,paste("0",bms,sep=""),as.character(bms))
	bmet<-ifelse(nchar(bme)==1,paste("0",bme,sep=""),as.character(bme))
	bdst<-ifelse(nchar(bds)==1,paste("0",bds,sep=""),as.character(bds))
	bdet<-ifelse(nchar(bde)==1,paste("0",bde,sep=""),as.character(bde))
	wms<-ssnd$WiMoStart;wds<-ssnd$WiDyStart;wme<-ssnd$WiMoEnd;wde<-ssnd$WiDyEnd
	wmst<-ifelse(nchar(wms)==1,paste("0",wms,sep=""),as.character(wms))
	wmet<-ifelse(nchar(wme)==1,paste("0",wme,sep=""),as.character(wme))
	wdst<-ifelse(nchar(wds)==1,paste("0",wds,sep=""),as.character(wds))
	wdet<-ifelse(nchar(wde)==1,paste("0",wde,sep=""),as.character(wde))
	
	#make sure not to miss observations prior to start month, but before end month.
	if(bme>bms){
		spdf[,brStart:=as.Date(paste(obsYear,bmst,bdst,sep="/"),"%Y/%m/%d")]
		spdf[,brEnd:=as.Date(paste(obsYear,bmet,bdet,sep="/"),"%Y/%m/%d")]
	}else{
		spdf[,brStart:=ifelse(obsMonth>bms,paste(obsYear,bmst,bdst,sep="-"),paste(obsYear-1,bmst,bdst,sep="-"))]
		spdf[,brStart:=as.Date(brStart,"%Y-%m-%d")]
		spdf[,brEnd:=ifelse(obsMonth>bms,paste(obsYear+1,bmet,bdet,sep="-"),paste(obsYear,bmet,bdet,sep="-"))]
		spdf[,brEnd:=as.Date(brEnd,"%Y-%m-%d")]
	}
	#same as above...
	if(wme>wms){
		spdf[,wiStart:=as.Date(paste(obsYear,wms,wds,sep="/"),"%Y/%m/%d")]
		spdf[,wiEnd:=as.Date(paste(obsYear,wme,wde,sep="/"),"%Y/%m/%d")]
	}else{
		spdf[,wiStart:=ifelse(obsMonth>wms,paste(obsYear,wms,wds,sep="-"),paste(obsYear-1,wms,wds,sep="-"))]
		spdf[,wiStart:=as.Date(wiStart,"%Y-%m-%d")]
		spdf[,wiEnd:=ifelse(obsMonth>wms,paste(obsYear+1,wme,wde,sep="-"),paste(obsYear,wme,wde,sep="-"))]
		spdf[,wiEnd:=as.Date(wiEnd,"%Y-%m-%d")]
	}
	return(spdf)
}
#
spcd=c("BAIS","BLRA","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","LETE","MAGO","MOPL","NOPI","RIRA","SACR","SNPL","SPPI","TRBL","WIFL")
		#"MCLO","BEVI","RNPH","GRSG","BBSA")

limYr<-as.integer(format(Sys.time(),"%Y"))-11
#get a map using get_map function, then crop it by the bounding box, and then capture to plot with ggmap(map)

#zsv<-c(6,5,6,6,5,5,5,5,5,6)
for(sss in spcd){
	print(sss)
	load(paste(pth,sss,".RData",sep=""))
	spdf<-data.table(spdf)
	spdf<-spdf[, .(count=sum(count,na.rm=T)), by = c("source","species","obsDate","lon","lat")]
	spdf[,eventSuccess:=ifelse(spdf$count>0,"positive","negative")]
	spdf[,obsYear:=as.integer(format(obsDate,"%Y"))]
	spdf<-spdf[obsYear>=limYr]
	spdf[,obsMonth:=as.integer(format(obsDate,"%m"))]
	
	#split by season
	spref<-ifelse(sss=="RIRA","CA_RIRA",sss)
	ssnd<-subset(seasons,SpCode==spref)
	ssdf<-getDateLims(spdf,ssnd)
	brlayer<-ssnd$brlayer;wilayer<-ssnd$wilayer
	ssn<-c("b","w")
	for(seas in ssn){
		#apply date filter
		if((seas=="b" & brlayer!="") || (seas=="w" & wilayer!="")){
			if(seas=="b"){
				sspdf<-spdf[obsDate>=brStart & obsDate<=brEnd]
				if(nrow(sspdf)>0){
					plot.df<-addSpatialFilter(df=sspdf,spv=sss,seas=seas,mappth=mappth,layern=brlayer)
				}else{
					stop(paste("Not enough breeding data for",sss))
				}
				
			}else{
				sspdf<-spdf[obsDate>=wiStart & obsDate<=wiEnd]
				if(nrow(sspdf)>0){
					plot.df<-addSpatialFilter(df=sspdf,spv=sss,seas=seas,mappth=mappth,layern=wilayer)
				}else{
					stop(paste("Not enough winter data for",sss))
				}
				
			}
			save(plot.df,file=paste(pth,"filteredByRangeNew/",sss,"_",seas,"_filtered.RData",sep=""))
		}
	}
		
}


### next is makeMetrics2.R