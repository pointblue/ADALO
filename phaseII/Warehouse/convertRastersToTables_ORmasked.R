# TODO: Add comment
# 
# Author: lsalas
###############################################################################


### SEE convertRastersToTables_New.R
## This is just a subset of that, for re-building the warehouses based on masked outputs Orien created


libs<-c("raster","data.table","plyr","rgdal")
lapply(libs, require, character.only = TRUE)

memory.limit(5000000)

g990<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/grid990/grid990.grd")

###################################
# M5New
# breeding
savepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/June2019/asTables/"
savemasked<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/M5_rastersMasked_NEW/June2019/"
mskpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/mask/speciesMasks/"

#need the selected results
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/June2019/breeding/"
spp<-data.frame(species=c("BAIS","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","MAGO","MOPL","NOPI","SACR","SPPI","TRBL"),
		selModel=c("wGeo_wLog","noGeo_wLog","noGeo_wLog","noGeo_wLog","noGeo_wLog","wGeo_wLog","noGeo_wLog","noGeo_wLog","noGeo_wLog","noGeo_wLog","noGeo_wLog","noGeo_wLog","wGeo_wLog"),
		stringsAsFactors=FALSE)

for(ii in 1:nrow(spp)){
	ss<-spp[ii,"species"]
	print(paste(ss,"breeding"))
	selmod<-spp[ii,"selModel"]
	rfls<-list.files(paste0(rpth,"m5rasterNew2_",selmod,"/"))
	spfln<-subset(rfls,grepl(ss,rfls,ignore.case=TRUE) & grepl(".tif",rfls) & !grepl(".xml",rfls))
	rast<-raster(paste0(rpth,"m5rasterNew2_",selmod,"/",spfln));
	chk<-compareRaster(g990,rast)
	if(chk==TRUE){
		#apply the land cover mask
		mskfln<-paste0(mskpth,ss,"_mask05.tif")
		msk<-raster(mskfln)
		spmsk<-mask(x=rast,mask=msk,maskvalue=0, updatevalue=NA)
		
		#save the masked result
		writeRaster(spmsk,filename=paste0(savemasked,"breeding/",ss,".tif"),format="GTiff",overwrite=TRUE)
		
		#convert to table
		spdf<-as.data.frame(spmsk);
		names(spdf)<-ss;spdf$period<-"breeding";
		spdf$g990cellId<-as.integer(row.names(spdf));
		spdf<-subset(spdf,!is.na(spdf[,ss])) #& spdf[,ss]>0.01
		save(spdf,file=paste(savepth,"M5_",ss,".RData",sep=""))
		print(paste("Done with breeding",spfln))
	}else{
		print(paste(spfln,"(breeding) is of the wrong extent or resolution or..."))
	}
	
}

# winter
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/finalFits_metric5/June2019/winter/"
spp<-data.frame(species=c("CANV","LBCU","MAGO","MOPL","SACR","TRBL","NOPI"),
		selModel=c("noGeo_wLog","wGeo_wLog","wGeo_wLog","wGeo_wLog","wGeo_wLog","noGeo_wLog","noGeo_wLog"),
		stringsAsFactors=FALSE)

for(ii in 1:nrow(spp)){
	ss<-spp[ii,"species"]
	print(paste(ss,"winter"))
	selmod<-spp[ii,"selModel"]
	rfls<-list.files(paste0(rpth,"m5rasterNew2_",selmod,"/"))
	spfln<-subset(rfls,grepl(ss,rfls,ignore.case=TRUE) & grepl(".tif",rfls) & !grepl(".xml",rfls))
	rast<-raster(paste0(rpth,"m5rasterNew2_",selmod,"/",spfln));
	chk<-compareRaster(g990,rast)
	if(chk==TRUE){
		#apply the land cover mask
		mskfln<-paste0(mskpth,ss,"_mask05.tif")
		msk<-raster(mskfln)
		spmsk<-mask(x=rast,mask=msk,maskvalue=0, updatevalue=NA)
		
		#save the masked result
		writeRaster(spmsk,filename=paste0(savemasked,"winter/",ss,".tif"),format="GTiff",overwrite=TRUE)
		
		#convert to table
		tdf<-as.data.frame(spmsk);
		names(tdf)<-ss;tdf$period<-"winter";
		tdf$g990cellId<-as.integer(row.names(tdf));
		tdf<-subset(tdf,!is.na(tdf[,ss]))  #& tdf[,ss]>0.01
		if(file.exists(paste(savepth,"M5_",ss,".RData",sep=""))){
			load(paste(savepth,"M5_",ss,".RData",sep=""))
			spdf<-rbind(spdf,tdf)
		}else{
			spdf<-tdf
		}
		save(spdf,file=paste(savepth,"M5_",ss,".RData",sep=""))
		print(paste("Done with winter",spfln))
	}else{
		print(paste(spfln,"(winter) is of the wrong extent or resolution or..."))
	}
	
}

