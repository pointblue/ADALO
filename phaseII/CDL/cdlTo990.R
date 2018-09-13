# TODO: Add comment
# 
# Author: lsalas
###############################################################################

libs<-c("raster","rgdal","plyr","data.table","rgeos","sp")
lapply(libs, require, character.only = TRUE)

cdl<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/CropData/cdl_2011_clip")

rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
r990<-raster(paste(rpth,"Grid990/base_grid990.tif",sep=""))

getADALOkey<-function(x){
	val<-ifelse(x==36,120,
			ifelse(x==92,121,
				ifelse(x==61,122,
					ifelse(x %in% c(45,59,60),124,
						ifelse(x==37,125,
							ifelse(x %in% c(66:68,70:72,74:77,204,210:212,217,218,220,223),126,
								ifelse(x==3,127,
									ifelse(x==69,129,
										ifelse(x %in% c(1,4:6,10,12,13,21:35,38,39,42,205,225,226,234:241,254),123,128)))))))))
}

dd<-seq(0,nrow(cdl),by=50);dd<-c(dd,60093)
cdldf<-data.frame()
for(z in 1:(NROW(dd)-1)){	#(NROW(dd)-1)
	stt<-dd[z]+1
	w<-getValues(cdl,row=stt,nrows=50)
	arv<-NROW(w)/82627
	tdf<-data.frame(cdlkey=w)
	stq<-((stt-1)*82627)+1;edq<-stq+nrow(tdf)-1
	ecv<-seq(stq,edq)
	tdf$cellId<-ecv;tdf<-data.table(tdf)
	#names(tdf)<-"cdlkey"
	tdf<-subset(tdf, !is.na(cdlkey) & cdlkey %in% c(1:6,10:14,21:39,41:61,66:77,92,204:214,216:227,229:250,254))
	if(nrow(tdf)>0){
		#get id990
		#civ<-as.integer(row.names(tdf));tdf$cellId<-civ
		civ<-tdf$cellId
		tdfxy<-as.data.frame(xyFromCell(cdl,civ))
		cdl990<-extract(r990,tdfxy,cellnumbers=TRUE)
		tdf$id990<-as.integer(cdl990[,1])
		#translate cdlkey to the ADALO cropkey
		tdf$cropkey<-getADALOkey(tdf$cdlkey)
		#aggregate by id990 and ADALOkey
		tdf$count<-1
		rdf<-aggregate(as.formula("count~id990+cropkey"),data=tdf,FUN=sum)
		cdldf<-rbind(cdldf,rdf)
	}
	print(z)
}

#aggregate again in case we have overlaps
cdldat<-aggregate(as.formula("count~id990+cropkey"),data=cdldf,FUN=sum)

#save, convert to stack and save
save(cdldat,file=paste(rpth,"cropdata/cdl990.RData",sep=""))

cdlstack<-r990;names(cdlstack)<-"base"
for(kk in unique(cdldat$cropkey)){
	rdf<-subset(cdldat,cropkey==kk)
	cid<-as.integer(rdf$id990)
	civ<-as.integer(rdf$count)
	r990[]<-NA
	r990[cid]<-civ
	names(r990)<-paste("cdl_",kk,sep="")
	cdlstack<-stack(cdlstack,r990)
}
cdlstack<-cdlstack[[-1]]
r68<-readOGR("V:/Data/jurisdiction/terrestrial/usfws/Regions6and8","r68polygonsAEA")
cdlstack_m<-mask(cdlstack,r68)

writeRaster(cdlstack_m, filename=paste(rpth,"CropData/CDLstack_masked.grd",sep=""), overwrite=TRUE)

