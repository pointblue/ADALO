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


##ebird data first
pth<-"Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/data/ebird/"
files<-list.files(path=pth,pattern=".RData")
files<-files[grepl("_eff.",files)==FALSE]	#removing the eff file for now

####### Open postgres connection
con<-try(dbConnect(dbDriver("PostgreSQL"),host="spdb.pointblue.org",dbname="spatial2",port="5432",user="postgres"),silent=TRUE)

caState<-readOGR("Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/caStateParkBdys2014a",layer="caStateParkBdys2014a")
caStateProj<-projection(caState)
cdfw<-readOGR("Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/CDFW_Owned_and_Operated_Lands",layer="CDFW_Owned_and_Operated_Lands")
fwsAppr<-readOGR("Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/FWSApproved",layer="FWSApproved")
fwsProj<-projection(fwsAppr)
fwsInt<-readOGR("Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/deliverable/FWSInterest",layer="FWSInterest")

for(fff in files){
	load(paste(pth,fff,sep=""))
	ebird.obs$Lat<-as.character(ebird.obs$LATITUDE); ebird.obs$Lon<-as.character(ebird.obs$LONGITUDE)
	ebird.obs<-subset(ebird.obs,!is.null(Lat) & !is.null(Lon) & !is.na(Lat) & !is.na(Lon) & Lat!="" & Lon!="")
	df<-unique(ebird.obs[,c("Lat","Lon")])
	
	fwsreg<-character()
	for(rrr in 1:(nrow(df))){
		pt<-paste("POINT(",df[rrr,"Lon"]," ",df[rrr,"Lat"],")",sep="")
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
		rs <- try(dbSendQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-fetch(rs, n= -1)
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
		rs <- try(dbSendQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-fetch(rs, n= -1)
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			padus<-c(padus,as.character(res.tmp))
		}else{
			padus<-c(padus,"No data")
		}
	}
	df$padus<-padus
	ebird.df<-merge(ebird.obs,df,by=c("Lon","Lat"),all.x=TRUE)
		
	#convert the points to sp points
	sppts<-ebird.obs
	coordinates(sppts)<-c("LONGITUDE","LATITUDE")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	
	sppts.ca<-spTransform(sppts,CRS(caStateProj))
	sppts.fws<-spTransform(sppts,CRS(fwsProj))
	
	caState.attr<-over(sppts.ca,caState)
	ebird.df$caState<-caState.attr$MgmtStatus
	cdfw.attr<-over(sppts.ca,cdfw)
	ebird.df$cdfw<-cdfw.attr$PROP_TYPE
	fwsAppr.attr<-over(sppts.fws,fwsAppr)
	ebird.df$fwsAppr<-fwsAppr.attr$ORGNAME
	fwsInt.attr<-over(sppts.fws,fwsInt)
	ebird.df$fwsIntStatus<-fwsInt.attr$STATUS
	ebird.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")
	ebird.df<-subset(ebird.df,fwsreg==8)
	
	save(ebird.obs,ebird.df,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,fff,sep=""))
}


##### BBS data
pth<-"Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/data/bbs/"
files<-list.files(path=pth, pattern=".csv")

#All OGR files already loaded, see lines 24-29 above

for(fff in files){
	bbs.obs<-read.csv(paste(pth,fff,sep=""))
	bbs.obs$Lat<-as.character(bbs.obs$DecimalLatitude); bbs.obs$Lon<-as.character(bbs.obs$DecimalLongitude)
	bbs.obs<-subset(bbs.obs,!is.null(Lat) & !is.null(Lon) & !is.na(Lat) & !is.na(Lon) & Lat!="" & Lon!="")
	df<-unique(bbs.obs[,c("Lat","Lon")])
	
	fwsreg<-character()
	for(rrr in 1:(nrow(df))){
		pt<-paste("POINT(",df[rrr,"Lon"]," ",df[rrr,"Lat"],")",sep="")
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT region_8 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
		rs <- try(dbSendQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-fetch(rs, n= -1)
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			fwsreg<-c(fwsreg,as.character(res.tmp))
		}else{
			fwsreg<-c(fwsreg,"No data")
		}
	}
	df$fwsreg<-fwsreg
	
	if(NROW(df)>0){
		padus<-character()
		for(rrr in 1:(nrow(df))){
			pt<-paste("POINT(",df[rrr,"Lon"]," ",df[rrr,"Lat"],")",sep="")
			#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
			sqltxt<-paste("SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom);",sep="")
			rs <- try(dbSendQuery(con, statement = sqltxt),silent=TRUE)
			if(!inherits(rs,"try-error")){
				res.tmp<-fetch(rs, n= -1)
				if(NROW(res.tmp)==0){res.tmp<-"No data"}
				padus<-c(padus,as.character(res.tmp))
			}else{
				padus<-c(padus,"No data")
			}
		}
		df$padus<-padus
		bbs.df<-merge(bbs.obs,df,by=c("Lon","Lat"),all.x=TRUE)
				
		sppts<-bbs.df
		coordinates(sppts)<-c("DecimalLongitude","DecimalLatitude")
		proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
		
		sppts.ca<-spTransform(sppts,CRS(caStateProj))
		sppts.fws<-spTransform(sppts,CRS(fwsProj))
		caState.attr<-over(sppts.ca,caState)
		bbs.df$caState<-caState.attr$MgmtStatus
		cdfw.attr<-over(sppts.ca,cdfw)
		bbs.df$cdfw<-cdfw.attr$PROP_TYPE
		fwsAppr.attr<-over(sppts.fws,fwsAppr)
		bbs.df$fwsAppr<-fwsAppr.attr$ORGNAME
		fwsInt.attr<-over(sppts.fws,fwsInt)
		bbs.df$fwsIntStatus<-fwsInt.attr$STATUS
		bbs.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")
		bbs.df<-subset(bbs.df,fwsreg==8)	#filter for reg 8
		
		save(bbs.obs,bbs.df,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,substr(fff,1,4),".RData",sep=""))
	}else{
		bbs.df<-data.frame()
		save(bbs.obs,bbs.df,file=paste(pth,substr(fff,1,4),".RData",sep=""))
	}
	
}


### CADC data
pth<-"Q:/Petaluma/lsalas/Documents/lsalas/IandMR8/data/cadc/"
files<-list.files(path=pth, pattern=".RData")

for(fff in files){
	load(paste(pth,fff,sep=""))
	if(fff=="rail_data.RData"){
		cadc.df$DecimalLatitude<-cadc.df$Lat;cadc.df$DecimalLongitude<-cadc.df$Lon
		cadc.df$DecimalLatitude<-as.numeric(cadc.df$DecimalLatitude)
		cadc.df$DecimalLongitude<-as.numeric(cadc.df$DecimalLongitude)
	}
	cadc.df$Lat<-as.character(cadc.df$DecimalLatitude); cadc.df$Lon<-as.character(cadc.df$DecimalLongitude)
	cadc.df<-subset(cadc.df,!is.null(Lat) & !is.null(Lon) & !is.na(Lat) & !is.na(Lon) & Lat!="" & Lon!="")
	df<-unique(cadc.df[,c("Lat","Lon")])
	
	fwsreg<-character()
	for(rrr in 1:(nrow(df))){
		pt<-paste("POINT(",df[rrr,"Lon"]," ",df[rrr,"Lat"],")",sep="")
		#SELECT own_type FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('POINT(-122.23908 39.4948100009)',4326)::geometry, pad.geom);
		sqltxt<-paste("SELECT region_1 FROM fws_regions AS reg WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, reg.geom);",sep="")
		rs <- try(dbSendQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-fetch(rs, n= -1)
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
		rs <- try(dbSendQuery(con, statement = sqltxt),silent=TRUE)
		if(!inherits(rs,"try-error")){
			res.tmp<-fetch(rs, n= -1)
			if(NROW(res.tmp)==0){res.tmp<-"No data"}
			padus<-c(padus,as.character(res.tmp))
		}else{
			padus<-c(padus,"No data")
		}
	}
	df$padus<-padus
	cadc.df<-merge(cadc.df,df,by=c("Lon","Lat"),all.x=TRUE)
		
	#convert the points to sp points
	sppts<-cadc.df
	coordinates(sppts)<-c("DecimalLongitude","DecimalLatitude")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	
	sppts.ca<-spTransform(sppts,CRS(caStateProj))
	sppts.fws<-spTransform(sppts,CRS(fwsProj))
	
	caState.attr<-over(sppts.ca,caState)
	cadc.df$caState<-caState.attr$MgmtStatus
	cdfw.attr<-over(sppts.ca,cdfw)
	cadc.df$cdfw<-cdfw.attr$PROP_TYPE
	fwsAppr.attr<-over(sppts.fws,fwsAppr)
	cadc.df$fwsAppr<-fwsAppr.attr$ORGNAME
	fwsInt.attr<-over(sppts.fws,fwsInt)
	cadc.df$fwsIntStatus<-fwsInt.attr$STATUS
	cadc.df$fwsIntName<-ifelse(fwsInt.attr$STATUS==0,fwsInt.attr$ORGNAME,"NA")
	cadc.df<-subset(cadc.df,fwsreg==8)
	
	save(cadc.df,caState.attr,cdfw.attr,fwsAppr.attr,fwsInt.attr,file=paste(pth,fff,sep=""))
}

dbDisconnect(con)

