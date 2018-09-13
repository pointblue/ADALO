# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("raster","rgdal","rgeos","sp","fmsb","dismo","data.table","plyr","gbm")
lapply(libs, require, character.only = TRUE)

#rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
rpth<-"/home/lsalas/adalo/"

spdf<-data.frame(spcd=c("WIFL","SACR","TRBL","BOBO","BUOW","SPPI","BAIS","CANV","CCLO","FEHA","LBCU","MAGO","MOPL","NOPI"),
		lrate=c(0.001,0.09,0.1,0.1,0.1,0.01,0.005,0.05,0.05,0.05,0.09,0.05,0.05,0.09),
		maxt=c(5000,10000,10000,10000,10000,10000,5000,5000,5000,5000,10000,5000,5000,10000),
		binlog=rep(FALSE,14),
		bingeo=rep(FALSE,14))

seas<-"w"		#OJO!!!

#for winter
spdf<-subset(spdf,spcd %in% c("SACR","TRBL","BUOW","SPPI","CANV","LBCU","MAGO","MOPL","NOPI"))

#accessory data
seasons<-data.table(read.csv(paste(rpth,"Seasons.csv",sep=""),stringsAsFactors=FALSE))
grid<-raster(paste(rpth,"Grid990/grid990.grd",sep=""))
gridProj<-projection(grid)
gd<-as.data.frame(grid,xy=TRUE);gd$cellId<-as.integer(rownames(gd));gd<-gd[,c("x","y","cellId")]
names(gd)<-c("lon","lat","cellId")
base<-grid;base[]<-NA

#This comes from file stackAllCovariates.R in StackCovariates
covars<-stack(paste(rpth,"CovarStack/covarstack_masked.grd",sep=""))
covarsdf<-as.data.frame(covars);covarsdf$cellId<-row.names(covarsdf)

#functions
#this gets the data for all the species, ready to model
getSpeciesData<-function(covars,rpth,sss,seas,gd,svdatpth){
	load(file=paste(rpth,"m4dataNew/",sss,"_",seas,".RData",sep=""))
	m4data<-na.omit(m4data)
	
	#use cellId and the data.frame gd to get lon/lat
	spdata<-merge(m4data,gd,by="cellId",all.x=TRUE)
	
	covdata<-extract(covars,spdata[,c("lon","lat")],cellnumbers=TRUE,df=TRUE)
	names(covdata)<-ifelse(is.na(names(covdata)),"cellId",names(covdata))
	covdata<-covdata[,which(!names(covdata) %in% c("cellId","ID","cells"))]
	m5data<-cbind(spdata[,c("cellId","OwnerName","encounterYRate")],covdata)

	return(m5data)
}

#need this function for filtering by distribution range after predicting to the stack with m5
addSpatialFilter<-function(rast,sss,spfilt){
	projection(spfilt)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	spfilt<-spTransform(spfilt,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
	rastdf<-as.data.frame(rast,xy=T);names(rastdf)<-c("lon","lat","predicted")
	rastdf<-as.data.frame(na.omit(rastdf))
	coordinates(rastdf)<-c("lon","lat")
	proj4string(rastdf)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
	spfilt.attr<-over(rastdf,spfilt)
	if(TRUE %in% grepl("OBJECTID",names(spfilt.attr),fixed=T)){
		rastdf$distFilter<-spfilt.attr$OBJECTID
	}else if(TRUE %in% grepl("PRESENCE",names(spfilt.attr),fixed=T)){
		rastdf$distFilter<-spfilt.attr$PRESENCE
	}else{
		fld<-names(spfilt.attr)[1]
		rastdf$distFilter<-spfilt.attr[,fld]
	}
	
	rastdf<-subset(rastdf,!is.na(distFilter))
	nrast<-rast
	nrast[]<-0
	nrast<-rasterize(rastdf,nrast,field="predicted")
	rm(list=c("rast","rastdf","spfilt","spfilt.attr"));gc()
	return(nrast)
}

#need this function to retreive the distribution layer name
getFilter<-function(sss,seas,seasons,rpth){
	seaslp<-ifelse(seas=="b","breeding/","wintering/")
	
	if(sss=="RIRA"){
		dsn<-paste(rpth,"RangeMaps/Enhanced/",seaslp,"RIRA_",seas,"_Dist",sep="")
		tly<-subset(seasons,SpCode=="CA_RIRA")
	}else{
		dsn<-paste(rpth,"RangeMaps/Enhanced/",seaslp,sss,"_",seas,"_Dist",sep="")
		tly<-subset(seasons,SpCode==sss)
	}
	
	layer<-ifelse(seas=="b",tly$brlayer,tly$wilayer)
	filt<-list(dsn=dsn,layer=layer)
	return(filt)
}

#need to include only meaningful variables
reviewData<-function(df){
	w<-names(df)[4:ncol(df)]
	dellist<-character()
	for(nn in w){
		q<-df[,get(nn)]
		if(NROW(q)==sum(is.na(q))){dellist<-c(dellist,nn)}
		if(min(q,na.rm=T)==max(q,na.rm=T)){dellist<-c(dellist,nn)}
	}
	return(dellist)
}

#this fits the BRT model
fitBRToptimalModel<-function(rpth,svpth,m5data,ncov,resp,lrat,fam,maxtr){
	jpeg(filename=svpth)
	brtm<-gbm.step(data=m5data, gbm.x=4:ncov, gbm.y=resp, tree.complexity = 3,
			learning.rate = lrat, bag.fraction = 0.75, n.folds = 10, family = fam, n.trees = 50, step.size = 50, max.trees = maxtr,
			plot.main = TRUE, verbose = FALSE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
	dev.off()
	nt<-brtm$n.trees
	if(nt<(0.2*maxtr) || is.null(nt)){
		jpeg(filename=svpth)
		brtm<-gbm.step(data=m5data, gbm.x=4:nc, gbm.y=resp, tree.complexity = 3,
				learning.rate = (lrat/2), bag.fraction = 0.75, n.folds = 10, family = fam, n.trees = 50, step.size = 50, max.trees = maxtr,
				plot.main = TRUE, verbose = FALSE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
		dev.off()
	}else if(nt>(0.8*maxtr)){
		jpeg(filename=svpth)
		brtm<-gbm.step(data=m5data, gbm.x=4:nc, gbm.y=resp, tree.complexity = 3,
				learning.rate = (lrat*2), bag.fraction = 0.75, n.folds = 10, family = fam, n.trees = 50, step.size = 50, max.trees = maxtr,
				plot.main = TRUE, verbose = FALSE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
		dev.off(brtm<-NULL)
	}else{}
	return(brtm)
}

#this returns the list with GOF info for the fitted model
getGOFlist<-function(m5data,brtm){
	obs<-m5data$lgrate
	pred<-brtm$fit
	cvfit<-brtm$fold.fit
	gof<-list(obs=obs,pred=pred,cvfit=cvfit)
	roc<-gbm.roc.area(obs,pred)
	varinf<-summary(brtm,plotit=FALSE)
	mgof<-list(gof=gof,roc=roc,varinf=varinf)
	return(mgof)
}

#this makes the output raster
makeOutRaster<-function(base,cid,civ,sss,seas,seasons,rpth,svrstpth){
	m5rast<-base;m5rast[cid]<-civ
	filtpars<-getFilter(sss,seas,seasons,rpth)
	spfilt<-readOGR(filtpars$dsn,filtpars$layer)
	m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,spfilt=spfilt)
	
	#save the model, data, and predicted raster
	wr<-try(writeRaster(m5rast_filt,filename=svrstpth,format="GTiff",overwrite=T),silent=T)
	return(wr)
}

mdlLst<-list()

#makeM5Raster<-function(x,seas="b",rpth,gd){  Cannot yet vectorize this because we are tweaking models
for(nnn in 1:nrow(spdf)){
#load the breeding or winter data
	sss<-as.character(spdf[nnn,"spcd"])
	lrat<-as.numeric(spdf[nnn,"lrate"])
	maxtr<-as.integer(spdf[nnn,"maxt"])
	uselog<-spdf[nnn,"binlog"]
	usegeo<-spdf[nnn,"bingeo"]
	fam<-"gaussian"
	
	print(paste(sss,seas))
		
	#get the data to model
	m5data<-getSpeciesData(covars=covars,rpth=rpth,sss=sss,seas=seas,gd=gd,svdatpth=svdatpth)
	filtfields<-reviewData(m5data);flds<-names(m5data)[which(!names(m5data) %in% filtfields)]
	if(NROW(filtfields)>0){m5data<-m5data[,..flds]}
	nc<-ncol(m5data)
	minv<-min(subset(m5data,encounterYRate>0)$encounterYRate)
	m5data[,lgrate:=log(encounterYRate+minv),]
	
	if(uselog==TRUE){
		resp=nc+1
		sufflog<-"_wLog"
	}else{
		resp=3
		sufflog<-"_noLog"
	}
	
	if(usegeo==TRUE){
		ncov=nc
		suffgeo<-"_wGeo"
	}else{
		ncov=nc-2
		suffgeo<-"_noGeo"
	}
	if(seas=="b"){
		sesfldr<-"breeding"
	}else{
		sesfldr<-"winter"
	}
	
	#save the data
	save(m5data,file=paste(rpth,"/",sesfldr,"m5dataNew2",suffgeo,sufflog,"/",sss,"_",seas,".RData",sep=""))
	
	#fit the model
	svpth<-paste(rpth,"/",sesfldr,"/m5rasterNew2",suffgeo,sufflog,"/",sss,"_",seas,"_brtOptimPlot.jpg",sep="")
	brtm<-fitBRToptimalModel(rpth=rpth,svpth=svpth,m5data=m5data,ncov=ncov,resp=resp,lrat=lrat,fam=fam,maxtr=maxtr)
		
	if(!is.null(brtm)){
		
		#gather GOF data
		mgof<-getGOFlist(m5data=m5data,brtm=brtm)
		
		#predict on the model's train data
		nt<-brtm$n.trees	#need to do it again, JIC
		m5data$predicted_gbm<-predict(brtm,n.trees=nt)#;m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
		
		#predict to covars
		preddf<-covarsdf
		preddf$predicted<-predict(brtm,preddf,n.trees=nt)#;m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
		cid<-as.integer(preddf$cellId);civ<-exp(preddf$predicted)-minv;civ<-ifelse(civ<0,0,civ)
		
		#make predicted raster
		svrstpth<-paste(rpth,sesfldr,"/m5rasterNew2",suffgeo,sufflog,"/",sss,"_",seas,"_filtered_gbm.tif",sep="")
		wr<-makeOutRaster(base,cid,civ,sss,seas,seasons,rpth,svrstpth)
		if(inherits(wr,"try-error")){print(paste(sss,wr))}
		
		#add results to list
		res<-list(model=brtm,mgof=mgof)
		rm(list=c("m5rast","m5rast_filt","m5data","spfilt"));gc()
		
	}else{
		res<-"No model fitted"
	}
	nnm<-paste(sss,"_",seas,sep="")
	mdlLst[[nnm]]<-res
}

for(mm in names(mdlLst)){
	if(class(mdlLst[[mm]])=="character"){
		print(paste(mm,":",mdlLst[[mm]]))
	}
}

#mdlLst<-lapply(X=spcd, FUN=makeM5Raster, seas="b",rpth=rpth,gd=gd)
save(mdlLst,file=paste(rpth,"/",sesfldr,"m5dataNew2",suffgeo,sufflog,"/mdlLst_b.RData",sep=""))


##############################################################################################
#Special cases, come what may...
spdf<-data.frame(spcd=c("WIFL","SACR","TRBL","BOBO","BUOW","SPPI"),
		lrate=c(0.01,0.0005,0.1,0.1,0.1,0.1,0.005),
		maxt=c(5000,5000,10000,10000,10000,10000,5000))
for(nnn in 1:nrow(spdf)){
#load the breeding or winter data
	sss<-as.character(spdf[nnn,"spcd"])
	lrat<-as.numeric(spdf[nnn,"lrate"])
	maxtr<-as.integer(spdf[nnn,"maxt"])
	seas="b"
	print(paste(sss,seas))
	load(file=paste(rpth,"m4dataNew/",sss,"_",seas,".RData",sep=""))
	m4data<-na.omit(m4data)
	
	#use cellId and the data.frame gd to get lon/lat
	spdata<-merge(m4data,gd,by="cellId",all.x=TRUE)
	
	#extract the covariate data
	covdata<-extract(covars,spdata[,c("lon","lat")],cellnumbers=TRUE,df=TRUE)
	names(covdata)<-ifelse(is.na(names(covdata)),"cellId",names(covdata))
	covdata<-covdata[,which(!names(covdata) %in% c("cellId","ID","lon","lat","cells"))]#;names(covdata)<-gsub("cells","cellId",names(covdata))
	m5data<-cbind(spdata,covdata) #merge(spdata,covdata,by=c("lon","lat"),all.x=TRUE)
	
	filtfields<-reviewData(m5data);flds<-names(m5data)[which(!names(m5data) %in% filtfields)]
	
	if(NROW(filtfields)>0){m5data<-m5data[,..flds]}
	fam<-"gaussian";resp<-3;nc<-ncol(m5data)
	if(sss %in% c("BLRA","RIRA","LETE","SNPL")){
		fam<-"bernoulli"
		m5data$presence<-ifelse(m5data$encounterYRate>0,1,0)
		resp<-nc+1
	}
	#fit the model
	jpeg(filename=paste(rpth,"m5rasterNew2_nolog_noGeo/",sss,"_",seas,"_brtOptimPlot.jpg",sep=""))
	brtm<-gbm.step(data=m5data, gbm.x=4:nc, gbm.y=resp, tree.complexity = 3,
			learning.rate = lrat, bag.fraction = 0.75, n.folds = 10, family = fam, n.trees = 50, step.size = 50, max.trees = maxtr,
			plot.main = TRUE, verbose = FALSE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
	dev.off()
	nt<-brtm$n.trees
	
	#gather GOF data
	cvfit<-brtm$cv.fitted
	obs<-m5data$encounterYRate;pred<-brtm$fit
	roc<-gbm.roc.area(obs,pred)
	varinf<-summary(brtm,plotit=FALSE)
	mgof<-list(cvfit=cvfit,roc=roc,varinf=varinf)
	
	preddf<-covarsdf
	if(sss %in% c("BLRA","RIRA","LETE","SNPL")){
		m5data$predicted_gbm<-predict(brtm,n.trees=nt,type="response")
		#predict to covars
		preddf$predicted<-predict(brtm,preddf,n.trees=nt,type="response")#;m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
	}else{
		m5data$predicted_gbm<-predict(brtm,n.trees=nt);m5data$predicted_gbm<-ifelse(m5data$predicted_gbm<0,0,m5data$predicted_gbm)
		#predict to covars
		preddf$predicted<-predict(brtm,preddf,n.trees=nt)#;m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
		preddf$predicted<-ifelse(preddf$predicted<0,0,preddf$predicted)
	}
	cid<-as.integer(preddf$cellId);civ<-preddf$predicted
	m5rast<-base;m5rast[cid]<-civ
	filtpars<-getFilter(sss,seas,seasons,rpth)
	spfilt<-readOGR(filtpars$dsn,filtpars$layer)
	m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,spfilt=spfilt)
	
	#save the model, data, and predicted raster
	writeRaster(m5rast_filt,
			filename=paste(rpth,"m5rasterNew2_nolog_noGeo/",sss,"_",seas,"_filtered_gbm.tif",sep=""),
			format="GTiff",overwrite=T)
	save(m5data,file=paste(rpth,"m5dataNew2_nolog_noGeo/",sss,"_",seas,".RData",sep=""))
	res<-list(model=brtm,mgof=mgof)
	rm(list=c("m5rast","m5rast_filt","m5data","spfilt"));gc()
	nnm<-paste(sss,"_",seas,sep="")
	mdlLst[[nnm]]<-res
}


#mdlLst_cont<-mdlLst
save(mdlLst,file=paste(rpth,"m5dataNew2_nolog_noGeo/mdlLst_specialCases_b.RData",sep=""))

###############################################################################
## Compiling result lists
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"
load(file=paste(rpth,"m5dataNew2/mdlLst_b.RData",sep=""))
mdlLst_b<-mdlLst
load(file=paste(rpth,"m5dataNew2/mdlLst_specialCases_b.RData",sep=""))
mdlLst_special<-mdlLst
for(nn in names(mdlLst_special)){
	mdlLst_b[[nn]]<-mdlLst_special[[nn]]
}
save(mdlLst_b,file=paste(rpth,"m5dataNew2/mdlLst_b_compiled.RData",sep=""))

