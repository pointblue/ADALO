# TODO: Add comment
# 
# Author: lsalas
###############################################################################


######################## THIS IS STEP IV  ###########################################

#This file prepares a first set of indices using data and models
# 1) Load the data
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/bySpecies/"

library(rgdal)
library(sp)
library(raster)
#library(mgcv)
library(fmsb)
library(gbm)
library(data.table)

#accessory data
seasons<-data.table(read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Seasons.csv",stringsAsFactors=FALSE))
grid<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Grid990/grid990.grd")
gridProj<-projection(grid)
gd<-as.data.frame(grid,xy=TRUE);gd$cellId<-as.integer(rownames(gd));gd<-gd[,c("x","y","cellId")]
names(gd)<-c("lon","lat","cellId")

#Rasterized PADUS: see file rasterizePADUS.R - the stack is the combined 3 padus grids in rasterizePADUS.R
padus<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS/padus990.grd")

#this comes from several files: getNLCD.R, aggregateNLCD.R, rasterizeNWIfromTiff.R and addBioclimNWI_toNLCD.R
covars<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NLCD/covars_masked.grd")
stnms<-names(covars)
developed<-covars[["NLCD_22"]]+covars[["NLCD_23"]]+covars[["NLCD_24"]]
nlcdwetlnd<-covars[["NLCD_90"]]+covars[["NLCD_95"]]
covars<-stack(covars,developed)
names(covars)<-c(stnms,"developed")
covars<-dropLayer(covars,"NLCD_22");covars<-dropLayer(covars,"NLCD_23");covars<-dropLayer(covars,"NLCD_24")
covars<-dropLayer(covars,"NLCD_90");covars<-dropLayer(covars,"NLCD_95")
nwiwetlands<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/NWI990/nwiraster.grd")
wetlands<-stack(nlcdwetlnd,nwiwetlands)

#the calc function must be such that it makes the nlcd wetland value be NA if there is an NWI value
getWetlandValue<-function(x,...){
	#there must be 4 values here
	y<-ifelse(sum(x[2:4],na.rm=T)>0,0,x[1])
	return(y)
}
nlcdwtlnds<-calc(wetlands,fun=getWetlandValue);names(nlcdwtlnds)<-"nlcdwetlands"
# I checked and indeed nlcdwtlnds has more NAs after getWetlandValue than nlcdwetlnd (the original)
covars<-stack(covars,nlcdwtlnds,nwiwetlands)

covarsdf<-as.data.frame(covars);covarsdf$cellId<-as.integer(row.names(covarsdf))

seasons<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/seasons.csv",stringsAsFactors=FALSE)


#functions
applyDateLims<-function(spdf,ssnd){
	bms<-ssnd$BrMoStart;bds<-ssnd$BrDyStart;bme<-ssnd$BrMoEnd;bde<-ssnd$BrDyEnd
	bmst<-ifelse(nchar(bms)==1,paste("0",bms,sep=""),as.character(bms))
	bmet<-ifelse(nchar(bme)==1,paste("0",bme,sep=""),as.character(bme))
	bdst<-ifelse(nchar(bds)==1,paste("0",bds,sep=""),as.character(bds))
	bdet<-ifelse(nchar(bde)==1,paste("0",bde,sep=""),as.character(bde))
	wms<-ssnd$WiMoStart;wds<-ssnd$WiDyStart;wme<-ssnd$WiMoEnd;wde<-ssnd$WiDyEnd
	wmst<-ifelse(nchar(wms)==1,paste("0",wms,sep=""),as.character(wms))
	wmet<-ifelse(nchar(wme)==1,paste("0",wme,sep=""),as.character(wme))
	wdst<-ifelse(nchar(wds)==1,paste("0",wds,sep=""),as.character(wds))
	wdet<-ifelse(nchar(wde)==1,paste("0",wde,sep=""),as.character(wde))
	
	#make sure not to miss observations prior to start month, but before end month.
	if(bme>bms){
		spdf[,brStart:=as.Date(paste(obsYear,bmst,bdst,sep="/"),"%Y/%m/%d")]
		spdf[,brEnd:=as.Date(paste(obsYear,bmet,bdet,sep="/"),"%Y/%m/%d")]
	}else{
		spdf[,brStart:=ifelse(obsMonth>bms,paste(obsYear,bmst,bdst,sep="-"),paste(obsYear-1,bmst,bdst,sep="-"))]
		spdf[,brStart:=as.Date(brStart,"%Y-%m-%d")]
		spdf[,brEnd:=ifelse(obsMonth>bms,paste(obsYear+1,bmet,bdet,sep="-"),paste(obsYear,bmet,bdet,sep="-"))]
		spdf[,brEnd:=as.Date(brEnd,"%Y-%m-%d")]
	}
	#same as above...
	if(wme>wms){
		spdf[,wiStart:=as.Date(paste(obsYear,wms,wds,sep="/"),"%Y/%m/%d")]
		spdf[,wiEnd:=as.Date(paste(obsYear,wme,wde,sep="/"),"%Y/%m/%d")]
	}else{
		spdf[,wiStart:=ifelse(obsMonth>wms,paste(obsYear,wms,wds,sep="-"),paste(obsYear-1,wms,wds,sep="-"))]
		spdf[,wiStart:=as.Date(wiStart,"%Y-%m-%d")]
		spdf[,wiEnd:=ifelse(obsMonth>wms,paste(obsYear+1,wme,wde,sep="-"),paste(obsYear,wme,wde,sep="-"))]
		spdf[,wiEnd:=as.Date(wiEnd,"%Y-%m-%d")]
	}
	return(spdf)
}

removeYearEffects<-function(df){
	dfy<-df[, .(encounterRate=mean(encounterRate,na.rm=T)), by = c("year")]
	##CHECK THIS!!
	setnames(dfy,c("year","ysmean"))
	smeanval<-mean(df$encounterRate,na.rm=T)
	df<-merge(df,dfy,by=c("year"))
	df[,smean:=smeanval]
	df[,cellId:=as.integer(cellId)]
	df[,encounterYRate:=encounterRate-ysmean+smean]
	df<-df[,c("cellId","encounterRate","encounterYRate")]
	return(df)
}

shrinkit<-function(df,by){
	#See section 5.2.2 of https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4816092/
	#shrinking is:
	#syi<-Bi*yi + (1-Bi)mu
	#where mu is the empirical mean rate, or here the global mean
	#Bi is ti/(ti + gmm)
	#ti is the conjugate prior, or the mean to shrink to, the mean of the by factor
	#gmm must be calculated as follows:
	#gmm=(n*mu)/sum(maxarg[(yi^2-mu)/(ti-mu^2)])
	#so that we shrink the yi observation from a global mean to a group mean
	nams<-names(df);shrknam<-paste("original",by,sep="")
	#mu<-mean(df$encounterYRate);n<-nrow(df) This should be the mean of the cell
	mudf<-df[, .(encounterYRate=mean(encounterYRate,na.rm=T)), by = c("cellId")]
	mundf<-df[, .(encounterYRate=NROW(encounterYRate)), by = c("cellId")]
	setnames(mudf,"encounterYRate","mu");setnames(mundf,"encounterYRate","nrec")
	mudf<-merge(mudf,mundf,by="cellId")
	tidf<-df[, .(encounterYRate=mean(encounterYRate,na.rm=T)), by = by]
	setnames(tidf,"encounterYRate","ti")
	df<-merge(df,tidf,by=by)
	df<-merge(df,mudf,by="cellId")
	#shr<-Bmu + (1-B)cm, with mu being the locname mean, and cm the cell mean
	
	df[,maxarg:=((encounterYRate^2)-mu)/(ti-(mu^2))]
	df[,maxarg:=ifelse(maxarg<0,0,maxarg)]
	df[,gmm:=(nrec*df$mu)/sum(df$maxarg)]
	df[,B:=ti/(ti+gmm)]
	df[,B:=B/max(B)]
	
	df[,shrunk:=(B*ti) + ((1-B)*mu)]
	df<-df[,c(nams,"shrunk"),with=FALSE]
	setnames(df,"encounterYRate",shrknam)
	setnames(df,"shrunk","encounterYRate")
	df[,encounterYRate:=ifelse(encounterYRate<0,0,encounterYRate)]
	return(df)
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
getFilter<-function(sss,seas,seasons){
	seaslp<-ifelse(seas=="b","breeding/","wintering/")
	
	if(sss=="RIRA"){
		dsn<-paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/Enhanced/",seaslp,"RIRA_",seas,"_Dist",sep="")
		tly<-subset(seasons,SpCode=="CA_RIRA")
	}else{
		dsn<-paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/Enhanced/",seaslp,sss,"_",seas,"_Dist",sep="")
		tly<-subset(seasons,SpCode==sss)
	}
	
	layer<-ifelse(seas=="b",tly$brlayer,tly$wilayer)
	filt<-list(dsn=dsn,layer=layer)
	return(filt)
}

#this is the workhorse function for m4
createM4M5data<-function(sss,df,gridProj,grid,padus,per,covarsdf){
	coords.df<-unique(df[,c("lon","lat")])
	coords.df[,rid:=1:nrow(coords.df)]
	coords.df[,lon:=as.numeric(as.character(lon))]; coords.df[,lat:=as.numeric(as.character(lat))]
	coords.df<-coords.df[(!is.null(lon) & !is.null(lat) & !is.na(lon) & !is.na(lat) & lon!="" & lat!="")]
	sppts<-coords.df
	coordinates(sppts)<-c("lon","lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	spptstr<-spTransform(sppts,gridProj)
	cellpts<-as.data.frame(spptstr);cellpts$cellId<-as.data.frame(extract(grid,spptstr,cellnumbers=TRUE))$cells
	coords.df<-merge(coords.df,cellpts[,c("rid","cellId")],by="rid",all.x=TRUE)
	dfc<-merge(df,coords.df[,c("lon","lat","cellId")],by=c("lon","lat"),all.x=TRUE)
	dfc<-data.table(dfc)
	dfc<-dfc[!is.na(cellId)]; dfc[,cellId:=as.character(cellId)]
	cellpred<-dfc[, .(count=mean(count,na.rm=T)), by = c("cellId","obsYear")]
	setnames(cellpred,c("cellId","year","encounterRate"))
	cellpred[,encounterRate:=ifelse(encounterRate==0,0,log(encounterRate))]
	cellyp<-removeYearEffects(df=cellpred)
	#need to attribute by PADUS, then shrink by LocNm
	padusvals<-data.table(as.data.frame(extract(padus,spptstr,cellnumbers=TRUE)))
	setnames(padusvals,"cells","cellId")
	padusvals[,cellId:=as.integer(padusvals$cellId)]
	#need to deal with NAs = private
	padusvals[,LocName:=ifelse(is.na(LocName),13455,LocName)]		# 13455 = private land NOTE: there are 575 categories of private, including "Private" = 59279
	padusvals[,OwnerName:=ifelse(is.na(OwnerName),23,OwnerName)]	# 23 = private
	padusvals[,MgmtType:=ifelse(is.na(MgmtType),6,MgmtType)]		# 6 = private
	
	#padusvals MUST have one value per cellId, so...
	padusown<-padusvals[, .(OwnerName=min(OwnerName,na.rm=T)), by = c("cellId")]
	#need to deal with repeated cellIds and collapse by OwnerName, not LocName, and shrink the log value
	
	cellypp<-data.table(merge(cellyp,padusown,by="cellId",all.x=TRUE))
	#cellypp[,LocName:=ifelse(is.na(LocName),999999,LocName)]
	#cellypp[,MgmtType:=ifelse(is.na(MgmtType),99,MgmtType)]
	#cellypp[,OwnerName:=ifelse(is.na(OwnerName),99,OwnerName)]
	
	#HERE!!
	cellypps<-shrinkit(df=cellypp,by="OwnerName")
	m4data<-unique(cellypps[,c("cellId","OwnerName","encounterYRate"),with=FALSE])
	save(m4data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4dataNew/",sss,"_",per,".RData",sep=""))
	#create m5 data
	m5data<-merge(m4data,covarsdf,by="cellId",all.x=TRUE)
	save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5dataNew/",sss,"_",per,".RData",sep=""))
	#rasterize m4 data
	cellres<-merge(cellypps[,c("cellId","encounterYRate"),with=FALSE],gd,by="cellId",all.x=T)
	coordinates(cellres)<-c("lon","lat")
	proj4string(cellres)<-CRS(gridProj)
	gridres<-rasterize(x=cellres,y=grid,field="encounterYRate")
	writeRaster(gridres,filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4rasterNew/",sss,"_",per,".tif",sep=""),format="GTiff",overwrite=T)
	#clean memory
	rm(list=c("df","sppts","spptstr","cellpts","coords.df","cellpred","cellyp","padusvals","cellypp","cellypps","m4data","m5data","cellres","gridres"))
	gc()
	return(1)
}

spcd=c("BAIS","BLRA","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","LETE","MAGO","MOPL","NOPI","RIRA","SACR","SNPL","SPPI","TRBL","WIFL")
periods<-c("b","w")

for(sss in spcd){  #calculate metric 4 by cell
	#NEED to do this for BREEDING and WINTER
	for(per in periods){
		fpth<-paste(pth,"filteredByRangeNew/",sss,"_",per,"_filtered.RData",sep="")
		if(file.exists(fpth)){
			print(paste(sss,per))
			load(fpth)
			plot.df[,rid:=1:nrow(plot.df)]
			res<-createM4M5data(sss=sss,df=plot.df,gridProj=gridProj,grid=grid,padus=padus,per=per,covarsdf=covarsdf)
		}
	}	
}

rm(list="plot.df");gc()

#select covariates based on variance inflation - NOT for BRT
vif_func<-function(in_frame,thresh=10,trace=T,...){
	
	if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
	
	#get initial vif value for all comparisons of variables
	vif_init<-NULL
	var_names <- names(in_frame)
	for(val in var_names){
		regressors <- var_names[-which(var_names == val)]
		form <- paste(regressors, collapse = '+')
		form_in <- formula(paste(val, '~', form))
		vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
	}
	vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
	
	if(vif_max < thresh){
		if(trace==T){ #print output of each iteration
			prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
			cat('\n')
			cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
		}
		return(var_names)
	}
	else{
		
		in_dat<-in_frame
		
		#backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
		while(vif_max >= thresh){
			
			vif_vals<-NULL
			var_names <- names(in_dat)
			
			for(val in var_names){
				regressors <- var_names[-which(var_names == val)]
				form <- paste(regressors, collapse = '+')
				form_in <- formula(paste(val, '~', form))
				vif_add<-VIF(lm(form_in, data = in_dat, ...))
				vif_vals<-rbind(vif_vals,c(val,vif_add))
			}
			max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
			
			vif_max<-as.numeric(vif_vals[max_row,2])
			
			if(vif_max<thresh) break
			
			if(trace==T){ #print output of each iteration
				prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
				cat('\n')
				cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
				flush.console()
			}
			
			in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
			
		}
		
		return(names(in_dat))
		
	}
	
}

#NOT for BRT, only one formula used, with all vars - see below
makeFormula<-function(covs){
	covs2<-paste("I(",covs,"^2)",sep="")
	fml<-as.formula(paste("encounterYRate~",paste(covs,collapse="+"),"+",paste(covs2,collapse="+"),sep=""))
	return(fml)
}

## METRIC 5 BRT
#covars needs to have 0 where there are NAs for the wetlands variables

mdlLst<-list()
fml<-as.formula(paste("encounterYRate~",paste(names(covarsdf)[c(1:35)],collapse="+"),sep=""))
for(sss in spcd){
	#load the breeding or winter data
	seas="b"
	print(paste(sss,seas))
	load(file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5dataNew/",sss,"_",seas,".RData",sep=""))
	m5data<-na.omit(m5data)
	covcols<-names(m5data);covcols<-subset(covcols,!covcols %in% c("cellId","OwnerName"))
	m5data<-m5data[,covcols,with=FALSE]
	
	#fit the model
	mdl<-gbm(formula=fml,data=m5data,distribution="gaussian",n.trees=3000,shrinkage=0.01,interaction.depth=3,
			bag.fraction = 0.5,n.minobsinnode = 10,cv.folds = 10,keep.data=TRUE,verbose=FALSE)
	#gather GOF data
	cvfit<-mdl$cv.fitted
	obs<-m5data$encounterYRate;pred<-mdl$fit
	roc<-gbm.roc.area(obs,pred)
	varinf<-summary(mdl,plotit=FALSE)
	mgof<-list(cvfit=cvfit,roc=roc,varinf=varinf)
	
	m5data$predicted_gbm<-predict(mdl);m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
	#predict to covars
	m5rast<-predict(covars,mdl);m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
	filtpars<-getFilter(sss,seas,seasons)
	spfilt<-readOGR(filtpars$dsn,filtpars$layer)
	m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,spfilt=spfilt)
	#save the model, data (with padus info), and predicted raster
	writeRaster(m5rast_filt,
			filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5rasterNew/",sss,"_",seas,"_filtered_gbm.tif",sep=""),
			format="GTiff",overwrite=T)
	save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5dataNew/",sss,"_",seas,".RData",sep=""))
	spse<-paste(sss,seas,sep="")
	mdlLst[[spse]]<-list(model=mdl,mgof=mgof)
	rm(list=c("m5rast","m5rast_filt","m5data","spfilt"));gc()
	
	if(!sss %in% c("BAIS","BOBO","LETE","SPPI","WIFL")){
		seas="w"
		print(paste(sss,seas))
		load(file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5dataNew/",sss,"_",seas,".RData",sep=""))
		m5data<-na.omit(m5data)
		m5data<-m5data[,covcols,with=FALSE]
		
		#fit the model
		mdl<-gbm(formula=fml,data=m5data,distribution="gaussian",n.trees=3000,shrinkage=0.05,interaction.depth=3,
				bag.fraction = 0.5,n.minobsinnode = 10,cv.folds = 10,keep.data=TRUE,verbose=FALSE)
		#gather GOF data
		cvfit<-mdl$cv.fitted
		obs<-m5data$encounterYRate;pred<-mdl$fit
		roc<-gbm.roc.area(obs,pred)
		varinf<-summary(mdl,plotit=FALSE)
		mgof<-list(cvfit=cvfit,roc=roc,varinf=varinf)
		
		m5data$predicted_gbm<-predict(mdl);m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
		#predict to covars
		m5rast<-predict(covars,mdl);m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
		filtpars<-getFilter(sss,seas,seasons)
		spfilt<-readOGR(filtpars$dsn,filtpars$layer)
		m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,spfilt=spfilt)
		#save the model, data (with padus info), and predicted raster
		writeRaster(m5rast_filt,
				filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5rasterNew/",sss,"_",seas,"_filtered_gbm.tif",sep=""),
				format="GTiff",overwrite=T)
		save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5dataNew/",sss,"_",seas,".RData",sep=""))
		spse<-paste(sss,seas,sep="")
		mdlLst[[spse]]<-list(model=mdl,mgof=mgof)
		rm(list=c("m5rast","m5rast_filt","m5data","spfilt"));gc()
	}
	rm(list=c("m5rast","m5rast_filt","m5data","spfilt"));gc()
}

#mdlLst_cont<-mdlLst
save(mdlLst,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5dataNew/mdlLst_cont.RData")


