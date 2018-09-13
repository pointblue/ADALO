# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(rgdal)
library(sp)
library(raster)

#use this util I made to plot on google maps
source("c:/users/lsalas/workspace/PRBOGeneralUpdate/LSalas/Maps/mapLocationsUtil.R")

#need this function for further spatial filtering
addSpatialFilter<-function(df,filtname){
	spfilt<-readOGR(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/geodata/",filtname,sep=""),layer=filtname)
	spfProj<-projection(spfilt)
	sppts<-df[,c("Lon","Lat")]
	coordinates(sppts)<-c("Lon","Lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	sppts.spf<-spTransform(sppts,CRS(spfProj))
	spfilt.attr<-over(sppts.spf,spfilt)
	df$distFilter<-spfilt.attr$Id
	df<-subset(df,!is.na(distFilter))
	return(df)
}


##################################################################################################################################################################
## SKIP THIS - read below
#here load all data from all species
addMissingDay<-function(x){
	y<-x
	if(substr(x,9,10)=="00"){y<-paste(substr(x,1,8),"01",sep="")}
	return(y)
}

addSeason<-function(x){
	y<-"O"
	ss<-x[1];mm<-as.integer(x[2])
	if(ss %in% c("BRCO","CANV","NOPI")){
		y<-ifelse(mm %in% c(4:8),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss %in% c("BLRA","CLRA","TRBL")){
		y<-ifelse(mm %in% c(3:8),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss=="LETE"){
		y<-ifelse(mm %in% c(4:9),"B","O")
	}else if(ss=="SACR"){
		y<-ifelse(mm %in% c(3:9),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss=="LBCU"){
		y<-ifelse(mm %in% c(4:7),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss=="WIFL"){
		y<-ifelse(mm %in% c(5:8),"B","O")
	}else{}
	return(y)
}

makeCollated<-function(){
	collated.df<-data.frame()
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/"

	##ebird data first
	files<-list.files(path=pth,pattern="ebird_")
	for(fff in files){
		load(paste(pth,fff,sep=""))
		df<-unique(ebird.df[,c("ObsCount","SpeciesCode","LATITUDE","LONGITUDE","OBSERVATION_DATE","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")])
		names(df)<-c("obsCount","SpeciesCode","Lat","Lon","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")
		df$month<-format(df$obsDate,"%m")
		collated.df<-rbind(collated.df,df)
	}
	
	##bbs data now
	files<-list.files(path=pth, pattern="bbs_")
	for(fff in files){
		load(paste(pth,fff,sep=""))
		if(nrow(bbs.df)>0){
			df<-unique(bbs.df[,c("obsCount","SpeciesCode","DecimalLatitude","DecimalLongitude","ObservationDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")])
			df$obsDate<-strptime(as.character(df$ObservationDate),"%Y-%m-%d")
			df$month<-format(df$obsDate,"%m")
			df<-df[,c("obsCount","SpeciesCode","DecimalLatitude","DecimalLongitude","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","month","source")]
			names(df)<-c("obsCount","SpeciesCode","Lat","Lon","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","month","source")
			collated.df<-rbind(collated.df,df)
		}
	}
	
	
	##cadc data now 
	files<-list.files(path=pth, pattern="cadc_")
	for(fff in files){
		load(paste(pth,fff,sep=""))
		if(nrow(cadc.df)>0){
			cadc.df$ObsDate<-as.character(cadc.df$ObsDate)
			cadc.df$ObsDate<-apply(as.data.frame(cadc.df$ObsDate),1,FUN=addMissingDay)
			cadc.df$obsDate<-strptime(as.character(cadc.df$ObsDate),"%Y-%m-%d")
			df<-unique(cadc.df[,c("ObservationCount","SpeciesCode","DecimalLatitude","DecimalLongitude","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")])
			names(df)<-c("obsCount","SpeciesCode","Lat","Lon","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")
			df$month<-format(df$obsDate,"%m")
			collated.df<-rbind(collated.df,df)
		}
	}
	
	##rail data
	files<-list.files(path=pth, pattern="_rail_")
	for(fff in files){
		print(fff)
		load(paste(pth,fff,sep=""))
		if(nrow(rail.df)>0){
			rail.df$ObsDate<-rail.df$SamplingEventDt
			rail.df$obsDate<-strptime(as.character(rail.df$ObsDate),"%Y-%m-%d")
			if(grepl("sfb_",fff)){
				df<-unique(rail.df[,c("BirdCountMax","SpeciesCode","DecimalLatitude","DecimalLongitude","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")])
			}else{
				df<-unique(rail.df[,c("BirdCount","SpeciesCode","DecimalLatitude","DecimalLongitude","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")])
			}
			names(df)<-c("obsCount","SpeciesCode","Lat","Lon","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")
			df$month<-format(df$obsDate,"%m")
			collated.df<-rbind(collated.df,df)
		}
	}
	
	## add TRBL data
	filn<-paste(pth,"ice_trbl.RData",sep="")
	load(filn)
	trbl.df$obsDate<-strptime(as.character(rail.df$ObsDate),"%Y-%m-%d")
	trbl.df$source<-"trbl"
	df<-unique(trbl.df[,c("NumObs","SpeciesCode","Latitude","Longitude","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")])
	names(df)<-c("obsCount","SpeciesCode","Lat","Lon","obsDate","padus","caState","cdfw","fwsAppr","fwsIntStatus","fwsIntName","source")
	df$month<-format(df$obsDate,"%m")
	collated.df<-rbind(collated.df,df)
	#############################################
	
	##createOwnType: privateLand-summer vs privateLand-winter vs publicLand-summer vs publicLand-winter 
	ot<-apply(collated.df[,c("padus","caState","cdfw","fwsAppr","fwsIntStatus")])
	collated.df$OwnType<-ifelse(!collated.df$padus %in% c("Private Land","No data","Unknown","Native American Land"),"Public",
			ifelse(!is.na(collated.df$caState),"Public",
					ifelse(!is.na(collated.df$cdfw),"Public",
							ifelse(!is.na(collated.df$fwsAppr),"Public",
									ifelse(is.na(collated.df$fwsIntStatus),"Private",as.character(collated.df$fwsIntStatus))))))
	
	############
	#add season by species
	collated.df$season<-apply(collated.df[,c("SpeciesCode","month")],1,FUN="addSeason")

	collated.df$OwnTypeSeason<-paste(collated.df$OwnType,collated.df$season,sep="")
	
	save(collated.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")
	############################################################################################################################################################
	
}

####################
# start here to make plots & analyses, need dependencies above (lines 6-26)
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")
#example of outcome. We want this to be Orien's run code, so he could request the outputs he needs.
#Orien also suggested that filtering for outputs be species-based, and thus to create a metadata table describing
#the filters applied to the data (and also covariates used in incidence model)
spcd<-c("BRCO","BLRA","LETE","CLRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL")
zsv<-c(6,5,6,6,5,5,5,5,5,6)
for(sss in spcd){
	plot.df<-subset(collated.df,SpeciesCode==sss)
	if(sss=="CLRA"){
		#add filter for yumaensis dist
		plot.df<-addSpatialFilter(plot.df,filtname="Yuma_RIRA_Dist")
		titl<-"Yuma RIRA survey event locations"
	}else if(sss=="WIFL"){
		#add filter for SW WIFL
		plot.df<-addSpatialFilter(plot.df,filtname="SW_WIFL_Dist")
		titl<-"Southwestern WIFL survey event locations"
	}else{
		plot.df<-addSpatialFilter(plot.df,filtname=paste(sss,"_Dist",sep=""))
		titl<-paste(sss,"survey event locations")
	}
	zs<-ifelse(sss %in% c("BRCO","LETE","CLRA","TRBL"),6,5)
	#plot.df$eventSuccess<-ifelse(plot.df$obsCount>0,"positive","negative")
	#pp<-create_plot(df=plot.df,map_dot_color="eventSuccess",mt="terrain", zs = 6, latFieldName="Lat", lonFieldName="Lon",pz=2,titl=titl,sourcem="osm") #try decreasing sz (3 = continent level) until all points are plotted
	pp<-create_plot(df=plot.df,mt="terrain", zs = zs, latFieldName="Lat", lonFieldName="Lon",pz=2,titl=titl,sourcem="osm") #try decreasing sz (3 = continent level) until all points are plotted
	
	png(filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LocationMaps/",sss,".png",sep=""),width=1000,height=1000)
		print(pp)
	dev.off()
	
}


for(sss in spcd){
	plot.df<-subset(collated.df,SpeciesCode==sss)
	print(sss)
	pds<-summary(plot.df$padus); print(paste("PADUS",pds))
	css<-summary(plot.df$caState); print(paste("CAstate",css))
	cds<-summary(plot.df$cdfw); print(paste("CDFW",cds))
	aps<-summary(plot.df$fwsAppr); print(paste("FWSappr",aps))
	print("################")
	
}

















