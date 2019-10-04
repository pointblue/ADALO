# TODO: Reload basetable, hen reload the species tables
# 
# Author: lsalas
###############################################################################


## need to populate the basetable, padusdf, and the individual species obs tables
## must use this padus table: //prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS_R68/padus_r68_attTable.csv, extracted from arcMAP.

#need data appending function: conn, sourcetable, targettable, names of char fields, 
#	read table contents in increments of 10, append all columns

library(RODBC)

#conn is a valid and open connection
#stable is the source table with data to append
#dtable is the destination table where data will be appended 
##	IMPORTANT: the order of the stable fields must match the dtable fields
#charfields is a character vector of column name for those columns to be treated as character
addDataToTable<-function(conn, stable, dtable, charfields){
	pref<-paste("INSERT INTO ",dtable," VALUES ",sep="")
	tnams<-names(stable)
	nr<-nrow(stable);sv<-seq(0,nr,10)+1;ev<-seq(10,nr,10)
	if(max(ev)<nr){ev<-c(ev,nr)}
	if(max(sv)>nr){sv<-subset(sv,sv<nr)}
	q<-character()
	for(ii in 1:(NROW(sv))){
		ss<-sv[ii];ee<-ev[ii]
		tdf<-stable[c(ss:ee),]
		et<-nrow(tdf)
		vstat<-list()
		for(x in 1:et){
			#rdf<-tdf[x,]
			rfdat<-list(); e<-0
			for(nn in tnams){	#create the list of results for 1 record
				e<-e+1
				if (nn %in% charfields){
					cvc<-as.character(tdf[x,nn])
					if(grepl("'",cvc)){
						cvc<-gsub("'","''",cvc)
					}else if(grepl('"',cvc)){
						cvc<-gsub('"',"''",cvc)
					}else{}
					tch<-paste("'",cvc,"'",sep="")
					rfdat[[e]]<-tch
				}else{
					rfdat[[e]]<-tdf[x,nn]
				}
			}
			vstat[[x]]<-paste("(",paste(rfdat,collapse=","),")",sep="")
		}
		apndvals<-paste(vstat,collapse=",")
		sqltxt<-paste(pref,apndvals,sep="")
		qa<-sqlQuery(conn,sqltxt)
		q<-c(q,qa)
	}
	return(q)
}

#append the padusCats table first
#Schema: padusObjId mgmtType mgrName desType unitName all Integers?
pdobjects<-read.csv("/home/lsalas/adalo/warehouse/padus_r68_attTable.csv",stringsAsFactors=F)
pdobj<-pdobjects[,c("OBJECTID","Mang_Type","Mang_Name","d_Des_Tp","Unit_Nm","Category")]
names(pdobj)<-c("padusObjId","mgmtType","mgrName","desType","unitName","Category")

#create the connection 
conn<-odbcConnect("adalo")

#then pass the table to the function  #REDO - modify size of desTp based on length
tm<-Sys.time()
qq<-addDataToTable(conn=conn,stable=pdobj,dtable="padusCats",charfields=c("mgmtType","mgrName","desType","unitName","Category"))
Sys.time()-tm

#need a lookup for the padusCats
mtt<-unique(pdobjects[,c("Mang_Type","d_Mang_Typ")]);names(mtt)<-c("categoryCode","codeDescription");mtt$padusCat<-"mgmtType"
mtn<-unique(pdobjects[,c("Mang_Name","d_Mang_Typ")]);names(mtn)<-c("categoryCode","codeDescription");mtn$padusCat<-"mgrName"
padCatsLookup<-rbind(mtt,mtn);padCatsLookup<-padCatsLookup[,c("padusCat","categoryCode","codeDescription")]
tm<-Sys.time()
qq<-addDataToTable(conn=conn,stable=padCatsLookup,dtable="padusCatsLookup",charfields=c("padusCat","categoryCode","codeDescription"))
Sys.time()-tm

#load the baseIntersects table
load("/home/lsalas/adalo/warehouse/basetable.RData")
basetable<-basetable[,c("intId","pdobjid","USFWSregion","USFSregion","NPSregion","LCCregion","USJVregion","BCRregion","StateFIPS","CountyFIPS","g990cellId","ncells")]
names(basetable)<-gsub("pdobjid","padusObjId",names(basetable))
basetable$USFWSregion<-as.integer(as.character(basetable$USFWSregion))
basetable$USFSregion<-as.integer(as.character(basetable$USFSregion))
basetable$NPSregion<-as.integer(basetable$NPSregion)
tm<-Sys.time()
qq<-addDataToTable(conn=conn,stable=basetable,dtable="baseIntersects",charfields=c("StateFIPS","CountyFIPS"))
Sys.time()-tm
odbcClose(conn)


#######################################
## FIRST: Upload the tables in ...RefugePrioritization\Phase2\warehouse to /home/ubuntu/adalo/
# then...
# delete contents to update then add new tables, and report match
spcon<-odbcConnect("whadalo")
spp<-c("BAIS","BLRA","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","LETE","MAGO","MOPL","NOPI","RIRA","SACR","SNPL","SPPI","TRBL","WIFL")
pth<-"/home/ubuntu/adalo/"
tm<-Sys.time()
for(ss in spp){
	load(paste0(pth,ss,".RData",sep=""))
	speciesdf<-speciesdf[,c("intId","pdobjid","ncells","period","metric","metricValue","cellMetric")]
	names(speciesdf)<-gsub("pdobjid","padusObjId",names(speciesdf))
	speciesdf$period<-ifelse(speciesdf$period=="breeding",1,0)
	ww<-sqlQuery(spcon,paste("DELETE FROM",tolower(ss)))
	qq<-addDataToTable(conn=spcon,stable=speciesdf,dtable=tolower(ss),charfields="")
	tbl<-nrow(speciesdf)
	whs<-sqlQuery(spcon,paste("SELECT COUNT(*) FROM",tolower(ss)))
	print(paste(ss,"table:",tbl,"warehouse:",whs[1,1]))
}
Sys.time()-tm
odbcClose(spcon)

#######################################################################
## Faster to populate locally and then do a mysqldump??
mysqldump -u lsalas -p --opt --databases adalo > adalo_dump_06122019.sql
#then
mysql -u adalo -p < adalo_dump_06122019.sql


