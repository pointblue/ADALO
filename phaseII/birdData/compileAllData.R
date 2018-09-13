# TODO: Add comment
# 
# Author: lsalas
###############################################################################

######################## THIS IS STEP II ###########################################

library(sp)
library(rgdal)

convertToDecimalLatLon<-function(data,idfields,latfield,lonfield,cdprojstr="+proj=utm +zone=10 +north ellps=WGS84"){
	readydata<-subset(data,data[,latfield]<70 & data[,lonfield] < 0)
	nogeodata<-subset(data,is.na(data[,latfield]))
	toconvdata<-subset(data,data[,latfield]>70 & data[,lonfield] > 0 & !is.na(data[,latfield]))
	if(nrow(data)!=sum(nrow(readydata),nrow(nogeodata),nrow(toconvdata))){
		stop("Error: data don't add up!")
	}
	coorddata<-toconvdata[,c(idfields,lonfield,latfield)]
	
	library(rgdal)
	coordinates(coorddata)<-c(lonfield,latfield)
	proj4string(coorddata) <- CRS(cdprojstr)
	coorddata.dll <- spTransform(coorddata, CRS("+proj=longlat +datum=WGS84"))
	coorddata.dll<-as.data.frame(coorddata.dll)
	fldnames<-names(coorddata.dll)
	readydata<-readydata[,fldnames]
	nogeodata<-nogeodata[,fldnames]
	convdata<-rbind(readydata,nogeodata,coorddata.dll)
	if(nrow(convdata)!=nrow(data)){
		stop("Error: converted data don't add up!")
	}
	tblnames<-gsub(lonfield,"DecimalLongitude",names(convdata))
	tblnames<-gsub(latfield,"DecimalLatitude",tblnames)
	names(convdata)<-tblnames
	return(convdata)
}

#########################################################################################################################
## SKIP to line 100
#need a table with species, location, date, lon, lat, count, source.

#First let's get the ICE TRBL data
dat11<-read.csv(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/ICE_TRBL/observations_survey2011.csv",header=T)
dat12<-read.csv(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/ICE_TRBL/observations_survey2012.csv",header=T)
datice<-rbind(dat11,dat12)
datice<-datice[,c("Observation.Date","Location","Number.of.Birds")]
names(datice)<-c("obsDate","location","count")
locice<-read.csv(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/ICE_TRBL/locations_public.csv",header=T)
locice<-locice[,c("Name","Latitude","Longitude")]
names(locice)<-c("location","lat","lon")
ice.trbl<-merge(datice,locice,all.x=T)
ice<-subset(ice.trbl,!is.na(lat))
ice$species<-"TRBL"
ice$source<-"ICE"
ice<-ice[,c("lon","lat","obsDate","species","count","source")]
ice$obsDate<-as.Date(as.character(ice$obsDate),"%Y-%m-%d")
ice.mrg<-ice
save(ice.mrg,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/ice_TRBL.RData")

#now BCOR data
#Species
spp.df<-data.frame(
		commonName=c("Brandt\\'s Cormorant","Black Rail","Least Tern","Ridgway\\'s Rail","Canvasback","Sandhill Crane","Long-billed Curlew",
				"Northern Pintail","Willow Flycatcher","Tricolored Blackbird","Snowy Plover","Mountain Plover","McCown\\'s Longspur",
				"Chestnut-collared Longspur","Burrowing Owl","Sprague\\'s Pipit","Baird\\'s Sparrow","Bell\\'s Vireo","Red-necked Phalarope",
				"Bobolink","Greater Sage-Grouse","Ferruginous Hawk","Buff-breasted Sandpiper","Marbled Godwit"),
		spcd=c("BRCO","BLRA","LETE","RIRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL","SNPL","MOPL",
				"MCLO","CCLO","BUOW","SPPI","BAIS","BEVI","RNPH","BOBO","GRSG","FEHA","BBSA","MAGO"))

eff.bcor<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/bcorData/Point Blue IMBCR effort.csv")
#must convert to dd latlon
bceff<-data.frame()
for(zz in unique(eff.bcor$Zone)){
	tdf<-subset(eff.bcor,Zone==zz)
	cdprojstr<-paste("+proj=utm +zone=",zz," +north ellps=WGS84",sep="")
	effdd<-convertToDecimalLatLon(tdf,idfields=c("TransectNum","Point","DATE","YEAR"),latfield="Easting",lonfield="Northing",cdprojstr=cdprojstr)
	bceff<-rbind(bceff,effdd)
}

dat.bcor<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/bcorData/Point Blue IMBCR raw data.csv")
dat.bcor<-dat.bcor[,c("TransectNum","Point","DATE","BirdCode","CL_Count")]
names(dat.bcor)<-c("TransectNum","Point","DATE","species","count")

for(ss in unique(dat.bcor$species)){
	sdf<-subset(dat.bcor,species==ss)
	tdf<-merge(bceff,sdf, all.x=T)
	tdf<-tdf[,c("DecimalLongitude","DecimalLatitude","DATE","species","count")]
	names(tdf)<-c("lon","lat","obsDate","species","count")
	tdf$count<-ifelse(is.na(tdf$count),0,tdf$count)
	tdf$species<-ifelse(is.na(tdf$species),ss,tdf$species)
	tdf$obsDate<-as.Date(as.character(tdf$obsDate),"%m/%d/%Y")
	tdf$obsDate<-as.Date(format(tdf$obsDate,"%Y-%m-%d"))
	tdf$source<-"bcor"
	bcor.mrg<-tdf
	save(bcor.mrg,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/bcor_",ss,".RData",sep=""))
}




########################################################################################################
# Retake here - ideally in the future we'd have a table with all events, and added columns with the counts, one column per species

library(RODBC)

getLatLon<-function(df){
	udf<-unique(df$SamplingUnitId)
	suids<-paste(udf,collapse=",")
	sqltxt<-paste("select SamplingUnitId,astext(CoordinatesCollection) as wkt from samplingunit where samplingunitid in (",suids,")",sep="")
	conn<-odbcConnect("prbodb")
	geo<-sqlQuery(conn,sqltxt)
	odbcClose(conn)
	if(nrow(geo)>0){
		geo$lon<-as.numeric(substr(a,7,regexpr(" ",a,fixed=T)-1))
		geo$lat<-as.numeric(substr(a,regexpr(" ",a,fixed=T)+1,nchar(a)-1))
	}
	geo<-geo[,c("SamplingUnitId","lon","lat")]
	df<-merge(df,geo,all.x=T)
	return(df)
}

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/"
#let's create a file per species
spcd=c("BRCO","BLRA","LETE","RIRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL","SNPL","MOPL",
		"MCLO","CCLO","BUOW","SPPI","BAIS","BEVI","RNPH","BOBO","GRSG","FEHA","BBSA","MAGO")
fls<-list.files(pth)

for(ss in spcd){
	spdf<-data.frame()
	if(ss=="TRBL"){
		load(paste(pth,"ice_TRBL.RData",sep=""))
		tdf<-ice.mrg[,c("source","species","count","obsDate","lon","lat")]
		tdf$obsDate<-as.Date(as.character(tdf$obsDate))
		spdf<-rbind(spdf,tdf)
	}
	#ebird data
	flse<-subset(fls,grepl("ebird",fls))
	flse<-subset(flse,grepl(ss,flse))
	if(NROW(flse)==1){
		load(paste(pth,flse[1],sep=""))
		tdf<-ebird.mrg[,c("source","SpeciesCode","ObsCount","OBSERVATION_DATE","LONGITUDE","LATITUDE")]
		names(tdf)<-c("source","species","count","obsDate","lon","lat")
		tdf$obsDate<-as.Date(as.character(tdf$obsDate))
		spdf<-rbind(spdf,tdf)
	}
	#bbs data
	flsb<-subset(fls,grepl("bbs_",fls))
	flsb<-subset(flsb,grepl(ss,flsb))
	if(NROW(flsb)==1){
		load(paste(pth,flsb[1],sep=""))
		bbs.mrg$ObservationDate<-paste(bbs.mrg$YearCollected,
				ifelse(nchar(bbs.mrg$MonthCollected)==1,paste("0",bbs.mrg$MonthCollected,sep=""),bbs.mrg$MonthCollected),
				ifelse(nchar(bbs.mrg$DayCollected)==1,paste("0",bbs.mrg$DayCollected,sep=""),bbs.mrg$DayCollected),sep="-")
		tdf<-bbs.mrg[,c("source","SpeciesCode","obsCount","ObservationDate","DecimalLongitude","DecimalLatitude")]
		names(tdf)<-c("source","species","count","obsDate","lon","lat")
		tdf$obsDate<-as.Date(as.character(tdf$obsDate))
		spdf<-rbind(spdf,tdf)
	}
	#bcor data
	flsbc<-subset(fls,grepl("bcor_",fls))
	flsbc<-subset(flsbc,grepl(ss,flsbc))
	if(NROW(flsbc)==1){
		load(paste(pth,flsbc[1],sep=""))
		tdf<-bcor.mrg[,c("source","species","count","obsDate","lon","lat")]
		tdf$obsDate<-as.Date(as.character(tdf$obsDate))
		spdf<-rbind(spdf,tdf)
	}
	#cadc_as
	flsca<-subset(fls,grepl("cadc_as",fls))
	flsca<-subset(flsca,grepl(ss,flsca))
	if(NROW(flsca)==1){
		load(paste(pth,flsca[1],sep=""))
		tdf<-cadc.mrg[,c("source","SpeciesCode","ObservationCount","ObsDate","DecimalLongitude","DecimalLatitude")]
		names(tdf)<-c("source","species","count","obsDate","lon","lat")
		tdf$obsDate<-as.Date(as.character(tdf$obsDate))
		spdf<-rbind(spdf,tdf)
	}
	#cadc_pc
	flscp<-subset(fls,grepl("cadc_pc",fls))
	flscp<-subset(flscp,grepl(ss,flscp))
	if(NROW(flscp)==1){
		load(paste(pth,flscp[1],sep=""))
		tdf<-cadc.mrg[,c("source","SpeciesCode","ObservationCount","ObsDate","DecimalLongitude","DecimalLatitude")]
		names(tdf)<-c("source","species","count","obsDate","lon","lat")
		tdf$obsDate<-as.Date(as.character(tdf$obsDate))
		spdf<-rbind(spdf,tdf)
	}
	#rails
	flsr<-subset(fls,grepl("rail",fls))
	flsr<-subset(flsr,grepl(ss,flsr))
	if(NROW(flsr)==1){
		for(ff in flsr){
			load(paste(pth,ff,sep=""))
			if(grepl("sfb_rail",ff)){
				tdf<-cadc.mrg[,c("source","SpeciesCode","BirdCountMax","SamplingEventDt","SamplingUnitId")]
			}else{
				tdf<-cadc.mrg[,c("source","SpeciesCode","BirdCount","SamplingEventDt","SamplingUnitId")]
			}
			tdf<-getLatLon(tdf)
			names(tdf)<-c("source","species","count","obsDate","lon","lat")
			tdf$obsDate<-as.Date(as.character(tdf$obsDate))
			spdf<-rbind(spdf,tdf)
		}
	}
	save(spdf,file=paste(pth,"bySpecies/",ss,".RData",sep=""))
}


### Next step: makeMaps.R


