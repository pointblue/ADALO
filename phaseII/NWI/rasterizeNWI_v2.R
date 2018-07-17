# TODO: Make sure that PEM1C is understood as PEMC
# TODO: Make sure to consider also attribution to the value after the slash: PEM1C/SUB
# 
# Author: lsalas
###############################################################################


#This code file processes the esri grid files and reclassifies them into 11 categories as described in:
# https://docs.google.com/spreadsheets/d/17uPJmFCsw-0BKjX4dWosNlQFYDFGwLR5mPaMc0bf4D8/edit#gid=1643610883

#For each esri grid file:
# read, convert to raster, then to data.table, then reclass, write as tif.
#Once that's done, we patch them together and with NLCD wetlands
#Need to obtain lat/lon and then attribute with r990 celId

library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(data.table)
memory.limit(size=35000)

reclass<-data.frame(attribute=c("M1","M2","E1","E2AB","E2RF","E2RS","E2SB","E2US","E2EM","E2FO","E2SS",
								"R1RS","R1US","R2RS","R2US","R3RS","R3US","R4RS","R4US","R5RS","R5US",
								"R1RB","R1UB","R1AB","R1SB","R1EM","R2RB","R2UB","R2AB","R2SB","R2EM",
								"R3RB","R3UB","R3AB","R3SB","R3EM","R4RB","R4UB","R4AB","R4SB","R4EM",
								"R5RB","R5UB","R5AB","R5SB","R5EM","L1","L2RS","L2US","L2RB","L2UB",
								"L2AB","L2EM","LH$","L$","PRB","PUB","PAB","PUS","PML","PEM$","PEM[1-9]/",
								"PEM[1-9]*F","PEM[1-9]*G","PEM[1-9]*H","PEM[1-9]*C","PEM[1-9]*E",
								"PEM[1-9]*K","PEM[1-9]*A","PEM[1-9]*B","PEM[1-9]*D","PEM[1-9]*J",
								"PEM[1-9]*T","PEM[1-9]*V","PEM[1-9]*Q","PEM[1-9]*R","PEM[1-9]*S",
								"PFO","PSS"),
					NWIvalue=c(101,102,101,rep(102,5),103,104,104,rep(105,10),rep(106,25),107,105,105,
								rep(107,9),105,108,109,109,109,109,109,110,110,110,rep(111,4),
								112,112,113,113,114,104,104))

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/NewClassification/"
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
r990<-raster(paste(rpth,"Grid990/base_grid990.tif",sep=""))
								
reclassKeys<-function(keys,reclass){
	keys<-keys[,which(names(keys)!="Rowid_")]
	keys$ATTRIBUTE<-toupper(keys$ATTRIBUTE)
	rekeys<-data.frame()
	for(aa in reclass$attribute){
		kv<-as.integer(subset(reclass,attribute==aa)$NWIvalue)
		rgx<-paste("^",aa,sep="")
		rgx2<-paste("/",aa,sep="")
		tkv<-subset(keys,grepl(rgx,ATTRIBUTE) | grepl(rgx2,ATTRIBUTE))
		if(nrow(tkv)>0){	#if the category exists in the key...
			tkv$key<-kv
			rekeys<-rbind(rekeys,tkv)
		}
	}
	res<-merge(keys,rekeys,by=c("ATTRIBUTE","VALUE","COUNT"),all.x=T)
	res<-unique(res[,c("ATTRIBUTE","VALUE","key")])
	return(res)
}

get990cellId<-function(rv,r990){
	rv990<-extract(r990,rv,cellnumbers=TRUE, df=TRUE)
	id990<-as.integer(rv990$cells)
	return(id990)
}


states<-c("CA","CO","KS","MT","ND","NE","NV","OR","SD","UT","WY")	#

for(ss in states){
	dpth<-paste(pth,ss,sep="")
	fls<-list.files(dpth,pattern=".ovr")
	fls<-gsub(".ovr","",fls)
	print(paste(ss,"to do:",paste(fls,collapse=", ")))
	for(ff in fls){
		rpth<-paste(dpth,ff,"dblbnd.adf",sep="/")
		rdf<-readGDAL(rpth)
		rast<-raster(rdf)
		df<-data.table(as.data.frame(rast,xy=TRUE))
		df<-subset(df,!is.na(band1));names(df)<-c("x","y","VALUE")
		df[,VALUE:=as.integer(VALUE)]
		df[,cellId:=as.integer(row.names(df))]
		#read the keys
		pth1<-paste(dpth,"/",ff,"_key.csv",sep="")
		pth2<-paste(dpth,"/",ff,".csv",sep="")
		if(file.exists(pth1)){
			keys<-read.csv(pth1)
		}else if(file.exists(pth2)){
			keys<-read.csv(pth2)
		}else{
			print(paste("Did not find key file for",ss,ff))
		}
		#reclass the keys to the 11 categories, using the reclass file, looping through each attribute value
		rkv<-reclassKeys(keys=keys,reclass=reclass)
		####### This is for test for Orien
		#rkv$state<-ss
		#rkv$file<-ff
		#wrtpth<-paste(pth,"/",ff,"_keyMatch.csv",sep="")
		#outfile<-rkv[,c("state","file","ATTRIBUTE","VALUE","key")]
		#write.csv(outfile,file=wrtpth)
		####### End of test
		rkv$VALUE<-as.numeric(rkv$VALUE);rkv$key<-as.integer(rkv$key)
		df<-merge(df,rkv[,c("VALUE","key")],by="VALUE",all.x=TRUE)
		rv<-df[,c("x","y")]
		df$id990<-get990cellId(rv,r990)
		#cid<-df$cellId;cev<-df$key
		#rast[cid]<-cev
		#writeRaster(rast,filename=paste(pth,"rasters/",ff,".tif",sep=""),format="GTiff",overwrite=T)
		ff<-ifelse(ff=="nwi_ca_n_all2","ca_n_all2",ff)
		save(df,file=paste(pth,"rasters/",ff,".RData",sep=""))
		print(paste("Done with",ff))
		rm(list=c("df","rkv"))
	}
	print(paste("Done with",ss))
	nrdf<-list.files(path=paste(pth,"rasters/",sep=""),pattern=".RData");nrdf<-subset(nrdf,grepl(tolower(ss),nrdf))
	NROW(fls)==NROW(nrdf)
}


