# TODO: Add comment
# 
# Author: lsalas
###############################################################################

###ATTENTION!
#Before you run - start mysqld


libs<-c("RODBC","data.table")
lapply(libs, require, character.only = TRUE)
 
insertData<-function(df,sv,regionv,conn){
	q<-0
	if(regionv==6){
		df<-subset(df,Region==6)
		tblnam<-"region6"
	}else if(regionv==8){
		df<-subset(df,Region==8)
		tblnam<-"region8"
	}else{}
	if(nrow(df)>0){
		cc<-(1:ceiling(nrow(df)/100))*100
		cc[NROW(cc)]<-nrow(df)
		
		j<-1
		for(zz in cc){
			dmodel<-paste("INSERT INTO ", tblnam, " VALUES ",sep="")
			tdf<-df[j:zz,c("Region","cellId","x","y")]
			tdf$seqval<-sv
			tdf$mdl<-apply(tdf,1,FUN=function(z){
						d<-paste("(",z[1],",",z[2],",",z[3],",",z[4],",",z[5],")",sep="");
						return(d)
					})
			mdlv<-paste(paste(tdf$mdl,collapse=","),sep="")
			sqltxt<-paste(dmodel,mdlv,sep="")
			q<-sqlQuery(conn,sqltxt)
			j<-zz+1
		}
	}else{
		q<-9
	}
	
	return(q)
	
}


readpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/"
fls<-list.files(readpth, pattern="seg")

#establish the connection
conn<-odbcConnect("adalo")

#loop through the files, read contents, append to mysql table
for(ff in fls){
	load(paste(readpth,ff,sep=""))
	seqval<-as.numeric(substr(ff,9,regexpr(".RData",ff)-1))
	q<-insertData(df=xy30r68,sv=seqval,regionv=6,conn=conn)
	print(seqval)
}

odbcClose(conn)

################
#count the uploads and seqvals
library(data.table)
nr<-numeric()
readpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/databased/"
fls<-list.files(readpth, pattern="seg")
for(ff in fls){
	load(paste(readpth,ff,sep=""))
	xy30r68<-subset(xy30r68,Region==6)
	if(nrow(xy30r68)>0){
		nrv<-nrow(xy30r68);nr<-c(nr,nrv)
	}
}
NROW(fls)==NROW(nr)
sum(nr)




libs<-c("RODBC","data.table")
lapply(libs, require, character.only = TRUE)

readpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/"
fls<-list.files(readpth, pattern="seg")

#establish the connection
conn<-odbcConnect("adalo")

#loop through the files, read contents, append to mysql table
for(ff in fls){
	load(paste(readpth,ff,sep=""))
	nr6<-nrow(subset(xy30r68,Region==6))
	seqval<-as.numeric(substr(ff,9,regexpr(".RData",ff)-1))
	nsr6<-sqlQuery(conn,paste("select count(*) from region6 where Segment =",seqval))
	sc6<-as.numeric(nsr6[1,1])
	if(nr6!=sc6){
		print(paste(seqval,nr6,sc6))
	}
}
odbcClose(conn)

