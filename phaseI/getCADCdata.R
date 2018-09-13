# TODO: Add 0's after merging, attribute with data source & protocol & date
# 
# Author: lsalas
###############################################################################

## This code file pre-queries the data to use for the refuge prioritization project
## Further filtering by Region 8 and other features happens in the file attribute_geospatial.R
## Plotting happens in the file plotData.R

## DEPENDENCIES
library(RODBC)
library(RPostgreSQL)

## FUNCTIONS
connNodeDb<-function(dsn, ...){
	library(RODBC)
	con<-odbcConnect(dsn=dsn, ...)
	return(con)
}

getRavianWH<-function(con1,sql){	#ravian_wh
	df<-sqlQuery(con1,query=sql,rows_at_time=1)
	return(df)
}

getFips<-function(rec,con4){
	wkt<-as.character(rec)
	res<-character()
	for(www in wkt){
		sql.intesect<-paste("SELECT statefp||countyfp from tl_2013_us_county_wgs84 as counties WHERE ST_Intersects(counties.geom, ST_GeomFromText('",wkt,"', 4326))",sep="")
		rs <- try(dbSendQuery(con4, statement = sql.intesect),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-fetch(rs, n= -1)
			res.tmp<-as.character(res.tmp)
		}else{
			res.tmp<-"ERROR"
		}
		res<-c(res,res.tmp)
	}
	if(NROW(res)==0)res<-"ERROR"
	return(res)
}

getCountyCodes<-function(con2,fipsval){
	fips<-fipsval[1]
	sqlfips<-paste("SELECT COUNTY_CODE FROM ebird.ebd_country_state_county WHERE fips5 = '",fips,"';",sep="")
	ccode<-sqlQuery(con2,query=sqlfips,rows_at_time=1)
	return(as.character(ccode[1,1]))
}

getPRBOdb<-function(con3,sql){	#ravian_wh
	df<-sqlQuery(con3,query=sql,rows_at_time=1)
	return(df)
}

processSUIDs<-function(sss,con2,con3,con4,rail="no"){
	###Get the state, Filter for NV, CA, OR (include NM, AZ for WIFL)
	###Further filterin in the attribute_geospatial.R file
	sqlpointgeo<-paste("select SamplingUnitID,CAST(AsText(PointGeometry) AS CHAR) from SamplingUnit where SamplingUnitID = ",sss,";",sep="")
	df.pointgeo<-getPRBOdb(con3=con3, sql=sqlpointgeo)
	df.pointgeo<-na.omit(df.pointgeo)
	if(NROW(df.pointgeo)>0){
		names(df.pointgeo)<-c("SamplingUnitId","WKT")
		###Do the intercept with the layer Doug made and retrieve the fips code for the county
		df.pointgeo$fips<-getFips(rec=as.character(df.pointgeo$WKT),con4=con4)
		df.pointgeo<-subset(df.pointgeo,fips!="ERROR")
		#create a connection with ebird.ebd_country_state_county and retrieve the COUNTY_CODE
		fipvals<-character()
		for(fff in as.character(df.pointgeo$fips)){
			fip<-getCountyCodes(con2=con2,fipsval=fff)
			fipvals<-c(fipvals,fip)
		}
		df.pointgeo$county_code<-fipvals
		df.pointgeo<-na.omit(df.pointgeo)
		if(rail=="no"){
			df.pointgeo<-subset(df.pointgeo,grepl("-CA-",county_code) | grepl("-NV-",county_code) | grepl("-OR-",county_code) | grepl("-AZ-",county_code) | grepl("-NM-",county_code))
		}else{
			df.pointgeo<-subset(df.pointgeo,grepl("-CA-",county_code) | grepl("-AZ-",county_code))
		}
		
	}
	return(df.pointgeo)
}

getSpeciesData<-function(pointgeo.df,con1,spp.df,dtype){
	if(NROW(pointgeo.df)>0){
		spcodes<-spp.df$spcd
		for(ccc in spcodes){
			print(ccc)
			sus<-pointgeo.df
			if(ccc!="WIFL"){
				sus<-subset(sus,grepl("-CA-",county_code) | grepl("-NV-",county_code) | grepl("-OR-",county_code),select="SamplingUnitId")
			}
			susfiltered<-paste(sus$SamplingUnitId,collapse=",")
			### Query all projects, protocols and years that may include the species of interest, get all the records and all the events, species by species
			if(dtype=="AS"){
				sqlobs<-paste("select ProjectCode, ProtocolCode, SamplingUnitId, DecimalLatitude, DecimalLongitude, CAST(ObservationDate AS CHAR) AS ObsDate, YearCollected, Plot, SpeciesCode, ObservationCount from RavianAreaSearchBase_v1 where SamplingUnitId in (",susfiltered,") and SpeciesCode = '",ccc,"';",sep="")
				sqleff<-paste("select distinct ProjectCode, ProtocolCode, SamplingUnitId, DecimalLatitude, DecimalLongitude, CAST(ObservationDate AS CHAR) AS ObsDate, YearCollected, Plot from RavianAreaSearchBase_v1 where SamplingUnitId in (",susfiltered,")",sep="")
				filen<-paste("/home/lsalas/refuge/cadc_as_",ccc,".RData",sep="")
			}else{
				sqlobs<-paste("select ProjectCode, ProtocolCode, SamplingUnitId, DecimalLatitude, DecimalLongitude, CAST(ObservationDate AS CHAR) AS ObsDate, YearCollected, Transect, Point, SpeciesCode, ObservationCount from RavianPointCountBase_v1 where SamplingUnitId in (",susfiltered,") and SpeciesCode = '",ccc,"';",sep="")
				sqleff<-paste("select distinct ProjectCode, ProtocolCode, SamplingUnitId, DecimalLatitude, DecimalLongitude, CAST(ObservationDate AS CHAR) AS ObsDate, YearCollected, Transect, Point from RavianPointCountBase_v1 where SamplingUnitId in (",susfiltered,")",sep="")
				filen<-paste("/home/lsalas/refuge/cadc_pc_",ccc,".RData",sep="")
			}
			cadc.obs<-getRavianWH(con1=con1,sql=sqlobs)
			cadc.eff<-getRavianWH(con1=con1,sql=sqleff)
			cadc.mrg<-merge(cadc.eff,cadc.obs,all.x=TRUE)
			cadc.mrg$SpeciesCode<-ccc
			cadc.mrg$source<-paste("cadc",dtype,sep="")
			
			save(cadc.obs,cadc.eff,cadc.mrg,file=filen)
		}
	}
	### Return the aggregate of records
	return("1")
}



#Species
spp.df<-data.frame(
		commonName=c("Brandt\\'s Cormorant","Black Rail","Least Tern","Clapper Rail","Canvasback","Sandhill Crane","Long-billed Curlew","Northern Pintail","Willow Flycatcher","Tricolored Blackbird"),
		spcd=c("BRCO","BLRA","LETE","CLRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL"))
## NOTE: we'll divide yumaensis from obsoletus by geography

#Rail species
railspp.df<-data.frame(commonName=c("Black Rail","Clapper Rail"),spcd=c("BLRA","CLRA"))


########################
### DATASETS ### Generate effort table first! Then add the ALL_SPECIES_REPORTED = 1 filter. Dummkopf!
# eBird
con<-connNodeDb(dsn="ebird_wh")
#get effort first:
effsql<-paste(
		"select distinct", 
		"SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED, DURATION_MINUTES, NUMBER_OBSERVERS, GROUP_IDENTIFIER, LATITUDE, LONGITUDE",
		"from ebd where",
		"(STATE_CODE in ('US-CA','US-NV','US-OR'))",
		"and (APPROVED = 1)",
		"and (PROTOCOL_TYPE in ('eBird - Exhaustive Area Count', 'eBird - Traveling Count', 'eBird - Stationary Count', 'EBIRD', 'Audubon NWR Protocol', 'EBIRD_CA','TNC California Waterbird Count'))")
ebird.eff<-sqlQuery(con,effsql,rows_at_time=1) 

effsql.clra<-paste(
		"select distinct", 
		"SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED, DURATION_MINUTES, NUMBER_OBSERVERS, GROUP_IDENTIFIER, LATITUDE, LONGITUDE",
		"from ebd where",
		"(STATE_CODE in ('US-CA','US-NV','US-OR','US-AZ'))",
		"and (APPROVED = 1)",
		"and (PROTOCOL_TYPE in ('eBird - Exhaustive Area Count', 'eBird - Traveling Count', 'eBird - Stationary Count', 'EBIRD', 'Audubon NWR Protocol', 'EBIRD_CA','TNC California Waterbird Count'))")
ebird.eff.clra<-sqlQuery(con,effsql.clra,rows_at_time=1)

effsql.wifl<-paste(
		"select distinct", 
		"SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED, DURATION_MINUTES, NUMBER_OBSERVERS, GROUP_IDENTIFIER, LATITUDE, LONGITUDE",
		"from ebd where",
		"(STATE_CODE in ('US-CA','US-NV','US-OR','US-AZ','US-NM'))",
		"and (APPROVED = 1)",
		"and (PROTOCOL_TYPE in ('eBird - Exhaustive Area Count', 'eBird - Traveling Count', 'eBird - Stationary Count', 'EBIRD', 'Audubon NWR Protocol', 'EBIRD_CA','TNC California Waterbird Count'))")
ebird.eff.wifl<-sqlQuery(con,effsql.wifl,rows_at_time=1)

#obs:
for(sss in 1:(nrow(spp.df))){
	spec<-spp.df[sss,"commonName"]
	spcd<-spp.df[sss,"spcd"]
	obssql<-paste(
			"select SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, MAX(OBSERVATION_COUNT) AS ObsCount, LATITUDE, LONGITUDE, OBSERVATION_DATE ",
			"from ebd where ",
			"(STATE_CODE in ('US-CA','US-NV','US-OR')) ",
			"and (APPROVED = 1) ",
			"and (PROTOCOL_TYPE in ('eBird - Exhaustive Area Count', 'eBird - Traveling Count', 'eBird - Stationary Count', 'EBIRD', 'Audubon NWR Protocol', 'EBIRD_CA','TNC California Waterbird Count')) ",
			"and (COMMON_NAME = '",spec,"') group by SAMPLING_EVENT_IDENTIFIER", sep="")
	if(spec=="Clapper Rail"){
		obssql<-paste(
				"select SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, MAX(OBSERVATION_COUNT) AS ObsCount, LATITUDE, LONGITUDE, OBSERVATION_DATE ",
				"from ebd where ",
				"(STATE_CODE in ('US-CA','US-NV','US-OR','US-AZ')) ",
				"and (APPROVED = 1) ",
				"and (PROTOCOL_TYPE in ('eBird - Exhaustive Area Count', 'eBird - Traveling Count', 'eBird - Stationary Count', 'EBIRD', 'Audubon NWR Protocol', 'EBIRD_CA','TNC California Waterbird Count')) ",
				"and (COMMON_NAME = '",spec,"') group by SAMPLING_EVENT_IDENTIFIER", sep="")
	}
	if(spec=="Willow Flycatcher"){
		obssql<-paste(
				"select SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, MAX(OBSERVATION_COUNT) AS ObsCount, LATITUDE, LONGITUDE, OBSERVATION_DATE ",
				"from ebd where ",
				"(STATE_CODE in ('US-CA','US-NV','US-OR','US-AZ','US-NM')) ",
				"and (APPROVED = 1) ",
				"and (PROTOCOL_TYPE in ('eBird - Exhaustive Area Count', 'eBird - Traveling Count', 'eBird - Stationary Count', 'EBIRD', 'Audubon NWR Protocol', 'EBIRD_CA','TNC California Waterbird Count')) ",
				"and (COMMON_NAME = '",spec,"') group by SAMPLING_EVENT_IDENTIFIER", sep="")
	}
	ebird.obs<-sqlQuery(con,obssql,rows_at_time=1)
	ebird.obs$SpeciesCode<-spcd
		
	#merge with effort
	if(spec=="Clapper Rail"){
		ebird.mrg<-merge(ebird.eff.clra,ebird.obs,by=c("SAMPLING_EVENT_IDENTIFIER"),all.x=TRUE)
		effort<-ebird.eff.clra
	}else if(spec=="Willow Flycatcher"){
		ebird.mrg<-merge(ebird.eff.wifl,ebird.obs,by=c("SAMPLING_EVENT_IDENTIFIER"),all.x=TRUE)
		effort<-ebird.eff.wifl
	}else{
		ebird.mrg<-merge(ebird.eff,ebird.obs,by=c("SAMPLING_EVENT_IDENTIFIER"),all.x=TRUE)
		effort<-ebird.eff
	}
	ebird.mrg$ObsCount<-ifelse(is.na(ebird.mrg$ObsCount),0,ebird.mrg$ObsCount)
	ebird.mrg$COMMON_NAME<-spec; ebird.mrg$SpeciesCode<-spcd; ebird.mrg$source<-"ebird"
	filen<-paste("/home/lsalas/refuge/ebird_",spcd,".RData",sep="")
	save(ebird.obs,effort,ebird.mrg,file=filen)
}

odbcClose(con)

#############################
# BBS
con<-connNodeDb(dsn="bbs_wh")

effsql<-paste("SELECT StratumName, SurveyAreaIdentifier, StratumNameSurveyAreaIdentifier, Longitude As DecimalLongitude, Latitude As DecimalLatitude,",
		"Year, Month, Day, ObservationDate, StartTime",
		"from bbsFlat where (Latitude is not null) and (Longitude is not null);")

bbs.eff<-sqlQuery(con,effsql,rows_at_time=1)

#obs:
for(sss in 1:(nrow(spp.df))){
	spec<-spp.df[sss,"commonName"]
	spcd<-spp.df[sss,"spcd"]
	obssql<-paste(
		"SELECT StratumName, SurveyAreaIdentifier, StratumNameSurveyAreaIdentifier, Longitude As DecimalLongitude, Latitude As DecimalLatitude, ",
		"Year, Month, Day, ObservationDate, StartTime, CommonName, SpeciesCode,  obsCount ",
		"from bbsFlat where (Latitude is not null) and (Longitude is not null) ",
		"and (CommonName = '",spec,"');",sep="")
	
	bbs.obs<-sqlQuery(con,obssql,rows_at_time=1)
	bbs.mrg<-merge(bbs.eff,bbs.obs,by=c("StratumName","SurveyAreaIdentifier","StratumNameSurveyAreaIdentifier","DecimalLongitude","DecimalLatitude","Year","Month","Day","ObservationDate","StartTime"),all.x=T)
	bbs.mrg$obsCount<-ifelse(is.na(bbs.mrg$obsCount),0,bbs.mrg$obsCount)
	bbs.mrg$CommonName<-spec
	bbs.mrg$SpeciesCode<-spcd
	bbs.mrg$source<-"bbs"
	filen<-paste("/home/lsalas/refuge/bbs_",spcd,".RData",sep="")
	save(bbs.obs,bbs.mrg,file=filen)
}

odbcClose(con)


###################################
# CADC
#First need to get sampling unit Ids in CA and NV

con1<-try(connNodeDb(dsn = "ravian_wh", rows_at_time = 1),silent=TRUE)
con2<-try(connNodeDb(dsn="ebird_wh", rows_at_time = 1),silent=TRUE)
con3<-try(connNodeDb(dsn = "prboDB", pwd="&pBtt2G.", rows_at_time = 1),silent=TRUE)
con4<-try(dbConnect(dbDriver("PostgreSQL"),host="spdb.pointblue.org",dbname="spatial2",port="5432",user="postgres"),silent=TRUE)
if(inherits(con1,"try-error"))stop("no connection 1")
if(inherits(con2,"try-error"))stop("no connection 2")
if(inherits(con3,"try-error"))stop("no connection 3")
if(inherits(con4,"try-error"))stop("no connection 4")

###Process AS data
suidsql<-"SELECT DISTINCT SamplingUnitID from RavianAreaSearchBase_v1 WHERE ProjectCode NOT IN ('CLRA','DMWADC','DUMBIO','DUMM','TestCLRA','TOOTH','DUMST');"	
df.suid<-getRavianWH(con1=con1,sql=suidsql)
suids<-as.integer(df.suid$SamplingUnitID)

pointgeo.df<-data.frame()
for(sss in suids){
	su.df<-processSUIDs(sss=sss,con2=con2,con3=con3,con4=con4,rail="no")
	pointgeo.df<-rbind(pointgeo.df,su.df)
}
x<-getSpeciesData(pointgeo.df=pointgeo.df,con1=con1,spp.df=spp.df,dtype="AS")

###Process PC data
suidsql<-"SELECT DISTINCT SamplingUnitID from RavianPointCountBase_v1 WHERE ProjectCode NOT IN ('CLRA','DMWADC','DUMBIO','DUMM','TestCLRA','TOOTH','DUMST');"	
df.suid<-getRavianWH(con1=con1,sql=suidsql)
suids<-as.integer(df.suid$SamplingUnitID)

pointgeo.df<-data.frame()
for(sss in suids){
	su.df<-processSUIDs(sss=sss,con2=con2,con3=con3,con4=con4,rail="no")
	pointgeo.df<-rbind(pointgeo.df,su.df)
}
x<-getSpeciesData(pointgeo.df=pointgeo.df,con1=con1,spp.df=spp.df,dtype="PC")

odbcClose(con1);odbcClose(con2);odbcClose(con3);dbDisconnect(con4)


############CADC rail data

con1<-try(connNodeDb(dsn = "trxDbGen", pwd="&pBtt2G.", rows_at_time = 1),silent=TRUE)
con2<-try(connNodeDb(dsn="ebird_wh", rows_at_time = 1),silent=TRUE)
con3<-try(connNodeDb(dsn = "prboDB", pwd="&pBtt2G.", rows_at_time = 1),silent=TRUE)
con4<-try(dbConnect(dbDriver("PostgreSQL"),host="spdb.pointblue.org",dbname="spatial2",port="5432",user="postgres"),silent=TRUE)
if(inherits(con1,"try-error"))stop("no connection 1")
if(inherits(con2,"try-error"))stop("no connection 2")
if(inherits(con3,"try-error"))stop("no connection 3")
if(inherits(con4,"try-error"))stop("no connection 4")

#read the project names
projsql<-"select distinct ProjectProtocol.ProjectId from ProjectProtocol where ProjectProtocol.ProtocolId in (select Protocol.ProtocolId from Protocol where Protocol.ProtocolTypeCd in ('Rail PointCount','SecretiveMarshBirdCount'));"
df.proj<-getRavianWH(con1=con3,sql=projsql)
# subset to omit some...
df.proj<-subset(df.proj,!ProjectId %in% c("CLRA","DMWADC","DUMBIO","DUMM","TestCLRA","TOOTH","DUMST"))
projstr<-paste("'",paste(df.proj$ProjectId,collapse="','"),"'",sep="")

#get the samplingunitids from those projects and their pointgeometry lat/lons using samplingunitproject table
suidsql<-paste("select SamplingUnitID from samplingunitproject where ProjectID in (",projstr,");")
df.suids<-getRavianWH(con1=con3,sql=suidsql)

#filter by the region of interest - this will filter effort by the following states: CA and AZ only
df.filtsus<-data.frame()
for(sss in unique(df.suids$SamplingUnitID)){
	fltsu<-processSUIDs(sss=sss,con2=con2,con3=con3,con4=con4,rail="yes")
	df.filtsus<-rbind(df.filtsus,fltsu)
}
susstr<-paste(df.filtsus$SamplingUnitId,collapse=",")

#get the effort data from those projects and filtered SUIDs
#get the obs data from those projects for RIRA and BLRA, and filtered SUIDs
#for SFBRAIL, use _RailCount, and for others use _Count, SamplingEventTypeCd = "MarshbirdCount"
for(ppp in df.proj$ProjectId){
	if(ppp=="SFBAYRAILS"){
		sqleff<-paste("select distinct ProjectId,SamplingUnitId,ProtocolId,SamplingEventTypeCd,SamplingEventCounter,SamplingEventDt,SamplingEventStartTm from SFBAYRAILS.SamplingEvent_RailCount where SamplingEventTypeCd='PointCount' and samplingUnitId in (",susstr,");",sep="")
		rail.eff<-getRavianWH(con1=con1,sql=sqleff)
		#get obs, merge, append to dfs...
		for(sss in railspp.df$spcd){	
			if(sss=="BLRA"){
				fbid<-"263"
			}else{
				fbid<-"464,2837"
			}
			sqlobs<-paste("select ProjectId,SamplingUnitId,ProtocolId,SamplingEventTypeCd,SamplingEventCounter,BirdCountMax from SFBAYRAILS.SamplingEventObservation_RailCount where SamplingEventTypeCd='PointCount' and samplingUnitId in (",susstr,") and FieldBirdId in (",fbid,");",sep="")
			rail.obs<-getRavianWH(con1=con1,sql=sqlobs)
			rail.obs$SpeciesCode<-sss
			rail.mrg<-merge(rail.eff,rail.obs, all.x=TRUE)
			rail.mrg$SpeciesCode<-sss
			rail.mrg$BirdCountMax<-ifelse(is.na(rail.mrg$BirdCountMax),0,rail.mrg$BirdCountMax)
			rail.mrg$source<-"rail_sfb"
			filen<-paste("/home/lsalas/refuge/sfb_rail_",sss,".RData",sep="")
			save(rail.eff,rail.obs,rail.mrg,file=filen)
		}
	}else{
		sqleff<-paste("select distinct ProjectId,SamplingUnitId,ProtocolId,SamplingEventTypeCd,SamplingEventCounter,SamplingEventDt,SamplingEventStartTm from ",ppp,".SamplingEvent_Count where SamplingEventTypeCd='MarshBirdCount' and samplingUnitId in (",susstr,");",sep="")
		rail.eff<-getRavianWH(con1=con1,sql=sqleff)
		#get obs, merge, append to dfs...
		if(nrow(rail.eff)>0){
			for(sss in railspp.df$spcd){	
				if(sss=="BLRA"){
					fbid<-"263"
				}else{
					fbid<-"464,2837"
				}
				sqlobs<-paste("select ProjectId,SamplingUnitId,ProtocolId,SamplingEventTypeCd,SamplingEventCounter,BirdCount from ",ppp,".SamplingEventObservation_Count where SamplingEventTypeCd='MarshBirdCount' and samplingUnitId in (",susstr,") and FieldBirdId in (",fbid,");",sep="")
				rail.obs<-getRavianWH(con1=con1,sql=sqlobs)
				if(nrow(rail.obs)>0){
					rail.obs$SpeciesCode<-sss
					rail.mrg<-merge(rail.eff,rail.obs,all.x=TRUE)
					rail.mrg$SpeciesCode<-sss
					rail.mrg$BirdCount<-ifelse(is.na(rail.mrg$BirdCount),0,rail.mrg$BirdCount)
				}else{
					rail.mrg<-rail.eff
					rail.mrg$BirdCount<-0
					rail.mrg$SpeciesCode<-sss
				}
				rail.mrg$source<-"rail_fws"
				filen<-paste("/home/lsalas/refuge/",ppp,"_rail_",sss,".RData",sep="")
				save(rail.eff,rail.obs,rail.mrg,file=filen)
			}
		}
	}
}

odbcClose(con1);odbcClose(con2);odbcClose(con3);dbDisconnect(con4)


###############
#		
#		NEED data from the secretive marshbird dataset for CLRA (Yuma), and BLRA



###################################
# Tricolored Blackbird
# Will put request for data
# Locations here: http://tricolor.ice.ucdavis.edu/locations/public
# Survey results here: http://tricolor.ice.ucdavis.edu/observations/survey/2012

###################################
# Internaional Crane Foundation
# Will put request for data

###################################
# GBBO
#Put request for data, no data. However, we were redirected to Chris Nicolai for waterfowl data in NV


###################################
# SFBBO
# Will put request for data

