# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("raster","rgdal","plyr","data.table","rgeos","sp","foreign")
lapply(libs, require, character.only = TRUE)

gpth<-"/home/lsalas/adalo/"

#Create a table out of the dbf in the padus
#######################################################################
## skip
#read PADUS.dbf into data.table
padus<-data.table(read.dbf(paste(gpth,"PADUS_R68/padus_r68.dbf",sep="")))
#select only the fields we want
padus<-padus[,.(OBJECTID,Mang_Type,Mang_Name,Des_Tp,Unit_Nm)]
save(padus,file="/home/lsalas/adalo/padus30/padus_ObjectId.RData")
########################################################################

#filter each by R6&8 polygons, attributing with R8 or R6
r68<-readOGR(paste(gpth,"Region68polys",sep=""),"r68polygonsAEA")
r68proj<-projection(r68)
r68fields<-"REGION_1"

#load the raster from Doug into a table
padrast<-raster(paste(gpth,"padus30/padus_30m.tif",sep=""))
rwsv<-as.numeric(nrow(padrast))
ncsv<-as.numeric(ncol(padrast))
dflst<-list()
for(ss in 0:(rwsv-1)){	#
	ss<-as.numeric(ss)
	ind<-ss+1
	cidvst<-(ss*ncsv)+1
	cidved<-(ss*ncsv)+ncsv
	tdf<-data.frame(cellId=(cidvst:cidved),pdobjid=getValues(padrast,row=ind))
	row.names(tdf)<-NULL
	tdf<-data.table(na.omit(tdf))
	dflst[[ind]]<-tdf
}
padus_cellObjId<-rbindlist(dflst)

padrast<-data.table(as.data.frame(padrast))
#add cellID
padrast<-padrast[,cellId:=row.names(padrast)]

#merge with padus on FeatureId

#write to mysql table

#Then need to get the value of the species rasters into the 30m grid raster
#convert each into a table, and add to mysql based on cellId match
