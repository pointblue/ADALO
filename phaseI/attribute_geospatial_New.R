# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#first, see if the data are in R8
#then attribut with PADUS
#then attribute with others

library(rgdal)
library(sp)
library(raster)
library(RPostgreSQL)

####### Open postgres connection
con<-try(dbConnect(dbDriver("PostgreSQL"),host="spdb.pointblue.org",dbname="spatial2",port="5432",user="postgres"),silent=TRUE)

####### Get other geospatial attribution layers loaded
caState<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/caStateParkBdys2014a",layer="caStateParkBdys2014a")
caStateProj<-projection(caState)
cdfw<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/CDFW_Owned_and_Operated_Lands",layer="CDFW_Owned_and_Operated_Lands")
fwsAppr<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/FWSApproved",layer="FWSApproved")
fwsProj<-projection(fwsAppr)
fwsInt<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/FWSInterest",layer="FWSInterest")

################################################### PROCESSING #################################################

##ebird data first
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/NewSet/"
files<-list.files(path=pth,pattern="ebird_")

#get the effort table from one file, process and then apply to the others
## CAREFUL!!! Need to do this separately for WIFL and CLRA because these include different spatial domains, so different effort
fff<-files[1]
load(paste(pth,fff,sep=""))
eff.df<-unique(effort[,c("LATITUDE","LONGITUDE")])
eff.df$Lat<-as.character(eff.df$LATITUDE); eff.df$Lon<-as.character(eff.df$LONGITUDE)
eff.df<-subset(eff.df,!is.null(Lat) & !is.null(Lon) & !is.na(Lat) & !is.na(Lon) & Lat!="" & Lon!="")

fwsreg<-character()
for(rrr in 1:(nrow(eff.df))){
	pt<-paste("POINT(",eff.df[rrr,"Lon"]," ",eff.df[rrr,"Lat"],")",sep="")
	sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		fwsreg<-c(fwsreg,as.character(res.tmp))
	}else{
		fwsreg<-c(fwsreg,"No data")
	}
}
eff.df$fwsreg<-fwsreg

padus<-character()
for(rrr in 1:(nrow(eff.df))){
	pt<-paste("POINT(",eff.df[rrr,"Lon"]," ",eff.df[rrr,"Lat"],")",sep="")
	sqltxt<-paste("SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom);",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		padus<-c(padus,as.character(res.tmp))
	}else{
		padus<-c(padus,"No data")
	}
}
eff.df$padus<-padus

#convert the points to sp points for other attributions
sppts<-eff.df
coordinates(sppts)<-c("LONGITUDE","LATITUDE")
proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")

sppts.ca<-spTransform(sppts,CRS(caStateProj))
sppts.fws<-spTransform(sppts,CRS(fwsProj))

#attribute
caState.attr<-over(sppts.ca,caState)
eff.df$caState<-caState.attr$MgmtStatus
cdfw.attr<-over(sppts.ca,cdfw)
eff.df$cdfw<-cdfw.attr$PROP_TYPE
fwsAppr.attr<-over(sppts.fws,fwsAppr)
eff.df$fwsAppr<-fwsAppr.attr$ORGNAME
fwsInt.attr<-over(sppts.fws,fwsInt)
eff.df$fwsIntStatus<-fwsInt.attr$STATUS
eff.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")

for(fff in files){
	load(paste(pth,fff,sep=""))
	ebird.mrg$ObsCount<-ifelse(is.na(ebird.mrg$ObsCount),0,ebird.mrg$ObsCount)
	ebird.mrg$Lat<-as.character(ebird.mrg$LATITUDE); ebird.mrg$Lon<-as.character(ebird.mrg$LONGITUDE)
	ebird.df<-merge(ebird.mrg,eff.df,by=c("Lon","Lat","LONGITUDE","LATITUDE"),all.x=TRUE)
	
	if(grepl("WIFL",fff) | grepl("CLRA",fff)){
		ebird.df<-subset(ebird.df,fwsreg %in% c("2","8"))
	}else{
		ebird.df<-subset(ebird.df,fwsreg=="8")
	}
	
	save(ebird.obs,ebird.df,ebird.mrg,effort,eff.df,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,fff,sep=""))
}


##### BBS data
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/NewSet/"
files<-list.files(path=pth, pattern="bbs_")

#get attribution for effort data, from only one file:
#get the effort table from one file, process and then apply to the others
fff<-files[1]
load(paste(pth,fff,sep=""))
eff.df<-unique(bbs.mrg[,c("DecimalLatitude","DecimalLongitude")])
eff.df$Lat<-as.character(eff.df$DecimalLatitude); eff.df$Lon<-as.character(eff.df$DecimalLongitude)
eff.df<-subset(eff.df,!is.null(Lat) & !is.null(Lon) & !is.na(Lat) & !is.na(Lon) & Lat!="" & Lon!="")

fwsreg<-character()
for(rrr in 1:(nrow(eff.df))){
	pt<-paste("POINT(",eff.df[rrr,"Lon"]," ",eff.df[rrr,"Lat"],")",sep="")
	#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
	sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		fwsreg<-c(fwsreg,as.character(res.tmp))
	}else{
		fwsreg<-c(fwsreg,"No data")
	}
}
eff.df$fwsreg<-fwsreg

padus<-character()
for(rrr in 1:(nrow(eff.df))){
	pt<-paste("POINT(",eff.df[rrr,"Lon"]," ",eff.df[rrr,"Lat"],")",sep="")
	#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
	sqltxt<-paste("SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom);",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		padus<-c(padus,as.character(res.tmp))
	}else{
		padus<-c(padus,"No data")
	}
}
eff.df$padus<-padus
	
#convert the points to sp points for further attribution...
sppts<-eff.df
coordinates(sppts)<-c("DecimalLongitude","DecimalLatitude")
proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")

sppts.ca<-spTransform(sppts,CRS(caStateProj))
sppts.fws<-spTransform(sppts,CRS(fwsProj))

#attribute
caState.attr<-over(sppts.ca,caState)
eff.df$caState<-caState.attr$MgmtStatus
cdfw.attr<-over(sppts.ca,cdfw)
eff.df$cdfw<-cdfw.attr$PROP_TYPE
fwsAppr.attr<-over(sppts.fws,fwsAppr)
eff.df$fwsAppr<-fwsAppr.attr$ORGNAME
fwsInt.attr<-over(sppts.fws,fwsInt)
eff.df$fwsIntStatus<-fwsInt.attr$STATUS
eff.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")


for(fff in files){
	load(paste(pth,fff,sep=""))
	bbs.mrg<-subset(bbs.mrg,!is.null(DecimalLatitude) & !is.null(DecimalLongitude) & !is.na(DecimalLatitude) & !is.na(DecimalLongitude) & DecimalLatitude!="" & DecimalLongitude!="")
	bbs.df<-merge(bbs.mrg,eff.df,by=c("DecimalLatitude","DecimalLongitude"),all.x=TRUE)
	
	if(grepl("WIFL",fff) | grepl("CLRA",fff)){
		bbs.df<-subset(bbs.df,fwsreg %in% c("2","8"))
	}else{
		bbs.df<-subset(bbs.df,fwsreg=="8")
	}
	
	save(bbs.obs,bbs.df,bbs.mrg,file=paste(pth,fff,sep=""))	
}


############# CADC data ######################
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/NewSet/"
files<-list.files(path=pth, pattern="cadc_")

for(fff in files){
	load(paste(pth,fff,sep=""))
	cadc.mrg$Lat<-as.character(cadc.mrg$DecimalLatitude); cadc.mrg$Lon<-as.character(cadc.mrg$DecimalLongitude)
	cadc.mrg<-subset(cadc.mrg,!is.null(Lat) & !is.null(Lon) & !is.na(Lat) & !is.na(Lon) & Lat!="" & Lon!="")
	df<-unique(cadc.mrg[,c("Lat","Lon")])
	
	fwsreg<-character()
	for(rrr in 1:(nrow(df))){
		pt<-paste("POINT(",df[rrr,"Lon"]," ",df[rrr,"Lat"],")",sep="")
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
		rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-rs
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			fwsreg<-c(fwsreg,as.character(res.tmp))
		}else{
			fwsreg<-c(fwsreg,"No data")
		}
	}
	df$fwsreg<-fwsreg
	
	padus<-character()
	for(rrr in 1:(nrow(df))){
		pt<-paste("POINT(",df[rrr,"Lon"]," ",df[rrr,"Lat"],")",sep="")
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom);",sep="")
		rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-rs
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			padus<-c(padus,as.character(res.tmp))
		}else{
			padus<-c(padus,"No data")
		}
	}
	df$padus<-padus
	cadc.df<-merge(cadc.mrg,df,by=c("Lon","Lat"),all.x=TRUE)
	
	#convert the points to sp points
	sppts<-cadc.df
	coordinates(sppts)<-c("DecimalLongitude","DecimalLatitude")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	
	sppts.ca<-spTransform(sppts,CRS(caStateProj))
	sppts.fws<-spTransform(sppts,CRS(fwsProj))
	
	#attribute
	caState.attr<-over(sppts.ca,caState)
	cadc.df$caState<-caState.attr$MgmtStatus
	cdfw.attr<-over(sppts.ca,cdfw)
	cadc.df$cdfw<-cdfw.attr$PROP_TYPE
	fwsAppr.attr<-over(sppts.fws,fwsAppr)
	cadc.df$fwsAppr<-fwsAppr.attr$ORGNAME
	fwsInt.attr<-over(sppts.fws,fwsInt)
	cadc.df$fwsIntStatus<-fwsInt.attr$STATUS
	cadc.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")
	if(grepl("WIFL",fff) | grepl("CLRA",fff)){
		cadc.df<-subset(cadc.df,fwsreg %in% c("2","8"))
	}else{
		cadc.df<-subset(cadc.df,fwsreg=="8")
	}
	
	print(fff)
	save(cadc.df,cadc.eff,cadc.obs,cadc.mrg,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,fff,sep=""))
}

############# rail data ######################
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/NewSet/"
files<-list.files(path=pth, pattern="_rail_")
library(RODBC)
conwkt<-odbcConnect(dsn="prbodb")
for(fff in files){
	load(paste(pth,fff,sep=""))
	#need to get the point geometry for the SUs from SamplingUnitId
	suids<-paste(unique(rail.eff$SamplingUnitId),collapse=",")
	sqlwkt<-paste("select SamplingUnitId,CAST(AsText(CoordinatesCollection) AS CHAR) FROM SamplingUnit WHERE SamplingUnitId IN (",suids,")",sep="")
	wkt.df<-sqlQuery(conwkt,sqlwkt,rows_at_time=1);names(wkt.df)<-c("SamplingUnitId","WKT")
	wkt.df<-subset(wkt.df,!is.null(WKT))
	wkt.df$DecimalLongitude<-substr(wkt.df$WKT,regexpr(pattern="(",wkt.df$WKT,fixed=T)+1,regexpr(pattern=" ",wkt.df$WKT,fixed=T)-1)
	wkt.df$DecimalLatitude<-substr(wkt.df$WKT,regexpr(pattern=" ",wkt.df$WKT,fixed=T)+1,regexpr(pattern=")",wkt.df$WKT,fixed=T)-1)
	
	fwsreg<-character()
	for(rrr in 1:(nrow(wkt.df))){
		pt<-as.character(wkt.df[rrr,"WKT"])
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
		rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-rs
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			fwsreg<-c(fwsreg,as.character(res.tmp))
		}else{
			fwsreg<-c(fwsreg,"No data")
		}
	}
	wkt.df$fwsreg<-fwsreg
	
	padus<-character()
	for(rrr in 1:(nrow(wkt.df))){
		pt<-as.character(wkt.df[rrr,"WKT"])
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom);",sep="")
		rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-rs
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			padus<-c(padus,as.character(res.tmp))
		}else{
			padus<-c(padus,"No data")
		}
	}
	wkt.df$padus<-padus
	rail.df<-merge(rail.mrg,wkt.df,by="SamplingUnitId",all.x=TRUE)
	if(class(rail.df$DecimalLongitude)=="character" | class(rail.df$DecimalLongitude)=="factor"){
		rail.df$DecimalLongitude<-as.numeric(as.character(rail.df$DecimalLongitude))
		rail.df$DecimalLatitude<-as.numeric(as.character(rail.df$DecimalLatitude))
	}
	
	#convert the points to sp points
	sppts<-rail.df
	coordinates(sppts)<-c("DecimalLongitude","DecimalLatitude")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	
	sppts.ca<-spTransform(sppts,CRS(caStateProj))
	sppts.fws<-spTransform(sppts,CRS(fwsProj))
	
	#attribute
	caState.attr<-over(sppts.ca,caState)
	rail.df$caState<-caState.attr$MgmtStatus
	cdfw.attr<-over(sppts.ca,cdfw)
	rail.df$cdfw<-cdfw.attr$PROP_TYPE
	fwsAppr.attr<-over(sppts.fws,fwsAppr)
	rail.df$fwsAppr<-fwsAppr.attr$ORGNAME
	fwsInt.attr<-over(sppts.fws,fwsInt)
	rail.df$fwsIntStatus<-fwsInt.attr$STATUS
	rail.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")
	if(grepl("CLRA",fff)){
		rail.df<-subset(rail.df,fwsreg %in% c("2","8"))
	}else{
		rail.df<-subset(rail.df,fwsreg=="8")
	}
	
	print(fff)
	save(rail.df,rail.eff,rail.obs,rail.mrg,wkt.df,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,fff,sep=""))
}

########## TRBL data ##################
## load the ICE data
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/NewSet/"

locs<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/ICE_TRBL/locations_public.csv")
locs<-locs[,c("Name","Latitude","Longitude")]
obs11<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/ICE_TRBL/observations_survey2011.csv")
obs12<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/data/ICE_TRBL/observations_survey2012.csv")
obs<-rbind(obs11,obs12)
obs<-obs[,c("Observation.Date","Location","Number.of.Birds")]
names(obs)<-c("ObsDate","Name","NumObs")
trbl.df<-merge(obs,locs,by="Name",all.x=TRUE)
trbl.df<-subset(trbl.df,!is.na(Latitude) & !is.na(Longitude))
trbl.df$NumObs<-ifelse(is.na(trbl.df$NumObs),0,trbl.df$NumObs) 
trbl.df$SpeciesCode<-"TRBL"

fwsreg<-character()
for(rrr in 1:(nrow(trbl.df))){
	pt<-paste("POINT(",trbl.df[rrr,"Longitude"]," ",trbl.df[rrr,"Latitude"],")",sep="")
	#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
	sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		fwsreg<-c(fwsreg,as.character(res.tmp))
	}else{
		fwsreg<-c(fwsreg,"No data")
	}
}
trbl.df$fwsreg<-fwsreg

padus<-character()
for(rrr in 1:(nrow(trbl.df))){
	pt<-paste("POINT(",trbl.df[rrr,"Longitude"]," ",trbl.df[rrr,"Latitude"],")",sep="")
	#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
	sqltxt<-paste("SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom);",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		padus<-c(padus,as.character(res.tmp))
	}else{
		padus<-c(padus,"No data")
	}
}
trbl.df$padus<-padus

#convert the points to sp points
sppts<-trbl.df
coordinates(sppts)<-c("Longitude","Latitude")
proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")	#Assumption - ask Bob Meese!

sppts.ca<-spTransform(sppts,CRS(caStateProj))
sppts.fws<-spTransform(sppts,CRS(fwsProj))

#attribute
caState.attr<-over(sppts.ca,caState)
trbl.df$caState<-caState.attr$MgmtStatus
cdfw.attr<-over(sppts.ca,cdfw)
trbl.df$cdfw<-cdfw.attr$PROP_TYPE
fwsAppr.attr<-over(sppts.fws,fwsAppr)
trbl.df$fwsAppr<-fwsAppr.attr$ORGNAME
fwsInt.attr<-over(sppts.fws,fwsInt)
trbl.df$fwsIntStatus<-fwsInt.attr$STATUS
trbl.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")
trbl.df<-subset(trbl.df,fwsreg==8)

save(trbl.df,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,"ice_trbl.RData",sep=""))

dbDisconnect(con)


