# TODO: Add comment
# 
# Author: lsalas
###############################################################################


load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")

coll<-unique(collated.df[,c("Lat","Lon")])

library(rgdal)
library(sp)
library(raster)
library(RPostgreSQL)

####### Open postgres connection
con<-try(dbConnect(dbDriver("PostgreSQL"),host="spdb.pointblue.org",dbname="spatial2",port="5432",user="postgres"),silent=TRUE)

###################################
padus_attrib<-data.frame(p_des_tp=rep(NA,times=nrow(coll)),shape_area=rep(0,times=nrow(coll)))
for(rrr in 1:(nrow(coll))){
	pt<-paste("POINT(",coll[rrr,"Lon"]," ",coll[rrr,"Lat"],")",sep="")
	sqltxt<-paste("SELECT p_des_tp, shape_area FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom) AND own_type LIKE '%Federal Land%';",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-data.frame(p_des_tp="No data",shape_area=NA)}
		padus_attrib[rrr,]<-c(as.character(res.tmp[1,1]),round(as.numeric(res.tmp[1,2])))
	}else{
		padus_attrib[rrr,]<-c("error",NA)
	}
}

coll.df<-cbind(coll,padus_attrib)

collated.df<-merge(collated.df,coll.df,by=c("Lat","Lon"),all.x=TRUE)

save(collated.df,coll.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")

###################################
ownn<-character()
for(rrr in 1:(nrow(coll))){
	pt<-paste("POINT(",coll[rrr,"Lon"]," ",coll[rrr,"Lat"],")",sep="")
	sqltxt<-paste("SELECT own_name FROM mwadc.padus_1_1_cbi_edition_wgs84 AS pad WHERE ST_Intersects(ST_GeomFromText('",pt,"',4326)::geometry, pad.geom) AND own_type LIKE '%Federal Land%';",sep="")
	rs <- try(dbGetQuery(con, statement = sqltxt),silent=TRUE)
	if(!inherits(rs,"try-error")){
		res.tmp<-rs
		if(NROW(res.tmp)==0){res.tmp<-"No data"}
		ownn<-c(ownn,as.character(res.tmp))
	}else{
		ownn<-c(ownn,"NA")
	}
}
coll.df$ownname<-ownn

collated.df<-merge(collated.df,coll.df,by=c("Lat","Lon"),all.x=TRUE)

save(collated.df,coll.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")
