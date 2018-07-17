# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("RODBC","data.table")
lapply(libs, require, character.only = TRUE)

insertData<-function(tdf,conn,lv){
	dmodel<-paste("INSERT INTO padus30m VALUES ",sep="")
	tdf$mdl<-apply(tdf,1,FUN=function(z,lv){
				d<-paste("(",z[1],",",z[2],",",lv,")",sep="");
				return(d)
			},lv=lv)
	mdlv<-paste(paste(tdf$mdl,collapse=","),sep="")
	sqltxt<-paste(dmodel,mdlv,sep="")
	q<-sqlQuery(conn,sqltxt)
	return(q)
}

memory.limit(size=100000000)

load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Padus_R68/padrast_objid.RData")

#establish the connection
conn<-odbcConnect("adalo")

for(ii in 1:NROW(dflst)){
	tdf<-dflst[[ii]]
	w<-insertData(tdf=tdf,conn=conn,lv=ii)
}
odbcClose(conn)

