# TODO: Add comment
# 
# Author: lsalas
###############################################################################

libs<-c("raster","data.table","plyr")
lapply(libs, require, character.only = TRUE)

memory.limit(5000000)

g990<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990.grd")

####################################
# M4New
library(plyr);library(raster);library(rgdal)
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4rasterNew/"
spp<-c("BAIS","BLRA","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","LETE","MAGO","MOPL","NOPI","RIRA","SACR","SNPL","SPPI","TRBL","WIFL")
rfls<-list.files(rpth,pattern=".tif")
for(ss in spp){
	print(ss)
	spfls<-subset(rfls,grepl(ss,rfls))
	spdf<-ldply(.data=spfls,.fun=function(x,rpth,ss,g990){
				per<-ifelse(grepl("_b.",x),"breeding","winter");
				rast<-raster(paste(rpth,x,sep=""));
				chk<-compareRaster(g990,rast)
				if(chk==TRUE){
					tdf<-as.data.frame(rast);
					names(tdf)<-ss;tdf$period<-per;
					tdf$g990cellId<-as.integer(row.names(tdf));
					#not subsetting for values > 0.01 because this is the empirical, not model projection
				}else{
					tdf<-data.frame(errormsg=paste(x,"is of the wrong extent or resolution or..."))
				}
				return(tdf)
			},rpth=rpth,ss=ss,g990=g990)
	spdf<-subset(spdf,!is.na(spdf[,ss]))
	save(spdf,file=paste(rpth,"asTables/M4_",ss,".RData",sep=""))
}

###################################
# M5New
# breeding
savepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/asTables/"
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/breeding/masked/"
spp<-c("BAIS","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","MAGO","MOPL","NOPI","SACR","SPPI","TRBL")
rfls<-list.files(rpth,pattern="mask_01")
for(ss in spp){
	print(paste(ss,"breeding"))
	spfln<-subset(rfls,grepl(ss,rfls,ignore.case=TRUE))
	rast<-raster(paste(rpth,spfln,sep=""));
	chk<-compareRaster(g990,rast)
	if(chk==TRUE){
		spdf<-as.data.frame(rast);
		names(spdf)<-ss;spdf$period<-"breeding";
		spdf$g990cellId<-as.integer(row.names(spdf));
		spdf<-subset(spdf,!is.na(spdf[,ss]) & spdf[,ss]>0.01)
		save(spdf,file=paste(savepth,"M5_",ss,".RData",sep=""))
		print(paste("Done with breeding",spfln))
	}else{
		print(paste(spfln,"(breeding) is of the wrong extent or resolution or..."))
	}
	
}

# winter
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/winter/masked/"
spp<-c("CANV","LBCU","MAGO","MOPL","NOPI","SACR","TRBL")
rfls<-list.files(rpth)
for(ss in spp){
	print(paste(ss,"winter"))
	spfln<-subset(rfls,grepl(ss,rfls,ignore.case=TRUE))
	rast<-raster(paste(rpth,spfln,sep=""));
	chk<-compareRaster(g990,rast)
	if(chk==TRUE){
		tdf<-as.data.frame(rast);
		names(tdf)<-ss;tdf$period<-"winter";
		tdf$g990cellId<-as.integer(row.names(tdf));
		tdf<-subset(tdf,!is.na(tdf[,ss]) & tdf[,ss]>0.01)
		load(paste(savepth,"M5_",ss,".RData",sep=""))
		spdf<-rbind(spdf,tdf)
		save(spdf,file=paste(savepth,"M5_",ss,".RData",sep=""))
		print(paste("Done with winter",spfln))
	}else{
		print(paste(spfln,"(winter) is of the wrong extent or resolution or..."))
	}
	
}

###################################
# HAPET
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/hapet/"
spp<-c("BAIS","BOBO","CCLO","SPPI")
rfls<-list.files(rpth,pattern=".tif");rfls<-subset(rfls,!grepl("tif.",rfls))
for(ss in spp){
	print(ss)
	spfln<-subset(rfls,grepl(ss,rfls,ignore.case=TRUE))
	rast<-raster(paste(rpth,spfln,sep=""));
	chk<-compareRaster(g990,rast)
	if(chk==TRUE){
		spdf<-as.data.frame(rast);
		names(spdf)<-ss;spdf$period<-"breeding";
		spdf$g990cellId<-as.integer(row.names(spdf));
		spdf<-subset(spdf,!is.na(spdf[,ss]))	#& spdf[,ss]>0.01 Can't filter hapet - densities are very low
		save(spdf,file=paste(rpth,"/asTables/HAPET_",ss,".RData",sep=""))
		print(paste("Done with breeding",spfln))
	}else{
		print(paste(spfln,"(breeding) is of the wrong extent or resolution or..."))
	}
	
}


#######################################
# ECN
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/ECN/"
spp<-c("BUOW","TRBL")
rfls<-list.files(rpth,pattern=".tif");rfls<-subset(rfls,!grepl("tif.",rfls))
for(ss in spp){
	print(ss)
	spfln<-subset(rfls,grepl(ss,rfls,ignore.case=TRUE))
	rast<-raster(paste(rpth,spfln,sep=""));
	chk<-compareRaster(g990,rast)
	if(chk==TRUE){
		spdf<-as.data.frame(rast);
		names(spdf)<-ss;spdf$period<-"breeding";
		spdf$g990cellId<-as.integer(row.names(spdf));
		spdf<-subset(spdf,!is.na(spdf[,ss]))	
		save(spdf,file=paste(rpth,"/asTables/ECN_",ss,".RData",sep=""))
		print(paste("Done with breeding",spfln))
	}else{
		print(paste(spfln,"(breeding) is of the wrong extent or resolution or..."))
	}
	
}



### Then merge inner join with base table and append to warehouse - this is in makeWarehouse_v1.R




