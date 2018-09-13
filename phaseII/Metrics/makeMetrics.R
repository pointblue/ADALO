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

#accessory data
seasons<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Seasons.csv")
grid<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Grid990/grid990.grd")
gridProj<-projection(grid)
gd<-as.data.frame(grid,xy=TRUE);gd$cellId<-rownames(gd);gd<-gd[,c("x","y","cellId")]
names(gd)<-c("lon","lat","cellId");gd$cellId<-as.character(gd$cellId)

#Rasterized PADUS: see file rasterizePADUS.R - the stack is the combined 3 padus grids in rasterizePADUS.R
padus<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS/padus990.grd")

#this comes from several files: getNLCD.R, aggregateNLCD.R, rasterizeNWI.R (still not doing this) and addBioclimNWI_toNLCD.R
covars<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NLCD/covars_masked.grd")
stnms<-names(covars)
developed<-covars[["NLCD_22"]]+covars[["NLCD_23"]]+covars[["NLCD_24"]]
nlcdwetlnd<-covars[["NLCD_90"]]+covars[["NLCD_95"]]
covars<-stack(covars,developed,nlcdwetlnd)
names(covars)<-c(stnms,"developed","NLCD_wetlands")
#wetlands<-ifelse(is.null(covars[["NWI_wetlands"]]),covars[["nlcdwetlands"]],covars[["NWI_wetlands"]])
covarsdf<-as.data.frame(covars);covarsdf$cellId<-row.names(covarsdf)

#functions
applySeasons<-function(sss,df,seasons){
	bsm<-subset(seasons,SpCode==sss,select="BrMoStart")$BrMoStart;bsd<-subset(seasons,SpCode==sss,select="BrDyStart")$BrDyStart
	bem<-subset(seasons,SpCode==sss,select="BrMoEnd")$BrMoEnd;bed<-subset(seasons,SpCode==sss,select="BrDyEnd")$BrDyEnd
	wsm<-subset(seasons,SpCode==sss,select="WiMoStart")$WiMoStart;wsd<-subset(seasons,SpCode==sss,select="WiDyStart")$WiDyStart
	wem<-subset(seasons,SpCode==sss,select="WiMoEnd")$WiMoEnd;wed<-subset(seasons,SpCode==sss,select="WiDyEnd")$WiDyEnd
	df$year<-as.integer(df$year)
	df$BrStDate<-as.Date(paste(df$year,"-",bsm,"-",bsd,sep=""));df$BrEdDate<-as.Date(paste(df$year,"-",bem,"-",bed,sep=""))
	df$WiStDate<-as.Date(paste(df$year,"-",wsm,"-",wsd,sep=""))
	df$WiEdDate<-ifelse(wsm>7 && wem<6,as.Date(paste(df$year+1,"-",wem,"-",wed,sep="")),as.Date(paste(df$year,"-",wem,"-",wed,sep="")))
	df$season<-ifelse(df$date>=df$BrStDate & df$date<=df$BrEdDate,"breeding",
			ifelse(df$date>=df$WiStDate & df$month<=12,"winter",
					ifelse(df$month>=1 & df$date<=df$WiEdDate,"winter","other")))
	return(df)
}

removeYearEffects<-function(df){
	dfy<-aggregate(as.formula("encounterRate~year+season"),data=df,FUN=mean,na.rm=TRUE)
	names(dfy)<-c("year","season","ysmean")
	gm<-aggregate(as.formula("encounterRate~season"),data=df,FUN=mean,na.rm=TRUE)
	names(gm)<-c("season","smean")
	df<-merge(df,dfy,by=c("season","year"))
	df<-merge(df,gm,by="season")
	df$encounterYRate<-df$encounterRate-df$ysmean+df$smean;#df$encounterYRate<-ifelse(df$encounterYRate<0,0,df$encounterYRate)
	df<-df[,c("cellId","season","encounterRate","encounterYRate")]
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
	nams<-names(df);shrknam<-paste("originalBy",by,sep="")
	#mu<-mean(df$encounterYRate);n<-nrow(df) This should be the mean of the cell
	mudf<-aggregate(as.formula("encounterYRate~cellId"),data=df,FUN=mean)
	mundf<-aggregate(as.formula("encounterYRate~cellId"),data=df,FUN=NROW)
	names(mudf)<-c("cellId","mu");names(mundf)<-c("cellId","nrec")
	mudf<-merge(mudf,mundf,by="cellId")
	tidf<-aggregate(as.formula(paste("encounterYRate~",by,sep="")),data=df,FUN=mean)
	names(tidf)<-c(by,"ti")
	df<-merge(df,tidf,by=by)
	df<-merge(df,mudf,by="cellId")
	#shr<-Bmu + (1-B)cm, with mu being the locname mean, and cm the cell mean
	
	df$maxarg<-((df$encounterYRate^2)-df$mu)/(df$ti-(df$mu^2))
	df$maxarg<-ifelse(df$maxarg<0,0,df$maxarg)
	df$gmm<-(df$nrec*df$mu)/sum(df$maxarg)
	df$B<-df$ti/(df$ti+df$gmm)
	df$B<-df$B/max(df$B)
	
	df$shrunk<-(df$B*df$ti) + ((1-df$B)*df$mu)
	df<-df[,c(nams,"shrunk")]
	names(df)<-gsub("encounterYRate",shrknam,names(df))
	names(df)<-gsub("shrunk","encounterYRate",names(df))
	df$encounterYRate<-ifelse(df$encounterYRate<0,0,df$encounterYRate)
	return(df)
}

#need this function for filtering by distribution range after predicting to the stack with m5
addSpatialFilter<-function(rast,sss,filtname){
	spv<-sss;if(sss=="WIFL"){spv<-"SW_WIFL"}
	spfilt<-readOGR(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/",spv,"_Dist",sep=""),layer=filtname)
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
getFilterName<-function(sss){
	spv<-sss;if(sss=="WIFL"){spv<-"SW_WIFL"}
	fln<-list.files(path=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/phase2/RangeMaps/",spv,"_Dist",sep=""),pattern=".dbf")
	lyn<-substr(fln,1,nchar(fln)-4)
	return(lyn)
}

#this is the workhorse function for m4
createM4M5data<-function(sss,splot.df,gridProj,grid,padus,seas,covarsdf){
	coords.df<-unique(splot.df[,c("lon","lat")])
	coords.df$rid<-1:nrow(coords.df)
	coords.df$lon<-as.numeric(as.character(coords.df$lon)); coords.df$lat<-as.numeric(as.character(coords.df$lat))
	coords.df<-subset(coords.df,!is.null(lon) & !is.null(lat) & !is.na(lon) & !is.na(lat) & lon!="" & lat!="")
	sppts<-coords.df
	coordinates(sppts)<-c("lon","lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	spptstr<-spTransform(sppts,gridProj)
	cellpts<-as.data.frame(spptstr);cellpts$cellId<-as.data.frame(extract(grid,spptstr,cellnumbers=TRUE))$cells
	coords.df<-merge(coords.df,cellpts[,c("rid","cellId")],by="rid",all.x=TRUE)
	splot.df<-merge(splot.df,coords.df[,c("lon","lat","cellId")],by=c("lon","lat"),all.x=TRUE)
	splot.df<-subset(splot.df,!is.na(cellId)); splot.df$cellId<-as.character(splot.df$cellId)
	cellpred<-aggregate(as.formula("count~cellId + year + season"),data=splot.df,FUN=mean,na.rm=TRUE)
	names(cellpred)<-c("cellId","year","season","encounterRate")
	cellyp<-removeYearEffects(df=cellpred)
	#need to attribute by PADUS, then shrink by LocNm
	padusvals<-as.data.frame(extract(padus,spptstr,cellnumbers=TRUE))
	names(padusvals)<-gsub("cells","cellId",names(padusvals))
	cellypp<-merge(cellyp,padusvals,by="cellId",all.x=TRUE)
	cellypp$LocName<-ifelse(is.na(cellypp$LocName),999999,cellypp$LocName)
	cellypp$MgmtType<-ifelse(is.na(cellypp$MgmtType),99,cellypp$MgmtType)
	cellypp$OwnerName<-ifelse(is.na(cellypp$OwnerName),99,cellypp$OwnerName)
	cellypps<-shrinkit(df=cellypp,by="LocName")
	m4data<-unique(cellypps[,c("cellId","season","LocName","MgmtType","OwnerName","encounterYRate")])
	save(m4data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4data/",sss,"_",seas,".RData",sep=""))
	#create m5 data
	m5data<-merge(m4data,covarsdf,by="cellId",all.x=TRUE)
	save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
	#rasterize m4 data
	cellres<-merge(cellypps[,c("cellId","encounterYRate")],gd,by="cellId",all.x=T)
	coordinates(cellres)<-c("lon","lat")
	proj4string(cellres)<-CRS(gridProj)
	gridres<-rasterize(x=cellres,y=grid,field="encounterYRate")
	writeRaster(gridres,filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4raster/",sss,"_",seas,".tif",sep=""),format="GTiff",overwrite=T)
	#clean memory
	rm(list=c("splot.df","sppts","spptstr","cellpts","coords.df","cellpred","cellyp","padusvals","cellypp","cellypps","m4data","m5data","cellres","gridres"))
	gc()
	return(1)
}

#spcd=c("BLRA","BRCO","BUOW","CANV","CCLO","FEHA","GRSG","LBCU",
#		"MAGO","MCLO","MOPL","NOPI","Yuma_RIRA","CA_RIRA","SACR","SNPL","SPPI","WIFL")

spcd=c("BRCO","BLRA","LETE","CA_RIRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL","SNPL","MOPL",
		"MCLO","CCLO","BUOW","SPPI","BAIS","BOBO","GRSG","FEHA","MAGO","Yuma_RIRA")

## METRIC 4
for(sss in spcd){  #calculate metric 4 by cell
	#NEED to do this for BREEDING and WINTER
	
	load(paste(pth,"filteredByRange/",sss,"_filtered.RData",sep=""))
	plot.df$date<-as.Date(as.character(plot.df$obsDate),"%Y-%m-%d")
	plot.df$year<-format(plot.df$date,"%Y")
	plot.df$month<-format(plot.df$date,"%m")
	plot.df<-subset(plot.df,year>=2006)
	plot.df<-applySeasons(sss=sss,df=plot.df,seasons=seasons)
	plot.df<-subset(plot.df,season %in% c("breeding","winter"))
	
	### BREEDING
	splot.df<-subset(plot.df,season=="breeding")
	splot.df$rid<-1:nrow(splot.df)
	print(paste(sss,"breeding"))
	res<-createM4M5data(sss=sss,splot.df=splot.df,gridProj=gridProj,grid=grid,padus=padus,seas="breeding",covarsdf=covarsdf)
	
	### WINTER
	if(!sss %in% c("BAIS","BOBO","LETE","SPPI","WIFL")){
		splot.df<-subset(plot.df,season=="winter")
		splot.df$rid<-1:nrow(splot.df)
		print(paste(sss,"winter"))
		res<-createM4M5data(sss=sss,splot.df=splot.df,gridProj=gridProj,grid=grid,padus=padus,seas="winter",covarsdf=covarsdf)
	}

}

rm(list="plot.df");gc()

#select covariates based on variance inflation
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

makeFormula<-function(covs){
	covs2<-paste("I(",covs,"^2)",sep="")
	fml<-as.formula(paste("encounterYRate~",paste(covs,collapse="+"),"+",paste(covs2,collapse="+"),sep=""))
	return(fml)
}

## METRIC 5 BRT
mdlLst<-list()
fml<-as.formula(paste("encounterYRate~",paste(names(covarsdf)[c(1,2,4:8,11,13:37)],collapse="+"),sep=""))
for(sss in spcd){
	#load the breeding or winter data
	seas="breeding"
	print(paste(sss,seas))
	load(file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
	m5data<-as.data.frame(na.omit(m5data))
	m5data<-m5data[,which(!names(m5data) %in% c("cellId","NLCD_22","NLCD_23","NLCD_23","NLCD_90","NLCD_95"))]
	
	#fit the model
	mdl<-gbm(formula=fml,data=m5data,distribution="gaussian",n.trees=3000,shrinkage=0.01,interaction.depth=3,
			bag.fraction = 0.5,n.minobsinnode = 10,cv.folds = 10,keep.data=TRUE,verbose=FALSE)
	
	m5data$predicted_gbm<-predict(mdl);m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
	#predict to covars
	m5rast<-predict(covars,mdl);m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
	filtname<-getFilterName(sss)
	m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,filtname=filtname)
	#save the model, data (with padus info), and predicted raster
	writeRaster(m5rast_filt,
			filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5raster/",sss,"_",seas,"_filtered_gbm.tif",sep=""),
			format="GTiff",overwrite=T)
	save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
	spse<-paste(sss,seas,sep="")
	mdlLst[[spse]]<-list(model=mdl,vars=vif.vars)
	rm(list=c("m5rast","m5data"));gc()
	
	if(!sss %in% c("BAIS","BOBO","LETE","SPPI","WIFL")){
		seas="winter"
		print(paste(sss,seas))
		load(file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
		m5data<-as.data.frame(na.omit(m5data))
		m5data<-m5data[,which(!names(m5data) %in% c("cellId","NLCD_22","NLCD_23","NLCD_23","NLCD_90","NLCD_95"))]
		
		#fit the model
		mdl<-gbm(formula=fml,data=m5data,distribution="gaussian",n.trees=3000,shrinkage=0.05,interaction.depth=3,
				bag.fraction = 0.5,n.minobsinnode = 10,cv.folds = 10,keep.data=TRUE,verbose=FALSE)
		
		m5data$predicted_gbm<-predict(mdl);m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
		#predict to covars
		m5rast<-predict(covars,mdl);m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
		filtname<-getFilterName(sss)
		m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,filtname=filtname)
		#save the model, data (with padus info), and predicted raster
		writeRaster(m5rast,
				filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5raster/",sss,"_",seas,"_filtered_gbm.tif",sep=""),
				format="GTiff",overwrite=T)
		save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
		spse<-paste(sss,seas,sep="")
		mdlLst[[spse]]<-list(model=mdl,vars=vif.vars)
		rm(list=c("m5rast","m5data"));gc()
	}
	rm(list=c("m5rast","m5data"));gc()
}

save(mdlLst,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/mdlLst.RData")



##########################################################################################################################
##### Metric 5 - linear models
mdlLst<-list()
for(sss in spcd){
	#load the breeding or winter data
	seas="breeding"
	print(paste(sss,seas))
	load(file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
	m5data<-as.data.frame(na.omit(m5data))
	m5data<-m5data[,which(!names(m5data) %in% c("cellId","NLCD_22","NLCD_23","NLCD_23","NLCD_90","NLCD_95"))]
	vif.vars<-vif_func(in_frame=m5data[,c(6:38)],thresh=5,trace=F)
	fml<-makeFormula(covs=vif.vars)
	#fit the model
	mdl<-lm(fml,m5data)
	nmdl<-stepAIC(mdl,k = qchisq(0.95, df=1),trace=F,direction="backward")
	m5data$predicted<-predict(nmdl);m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
	#predict to covars
	m5rast<-predict(covars,nmdl);m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
	filtname<-getFilterName(sss)
	m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,filtname=filtname)
	#save the model, data (with padus info), and predicted raster
	writeRaster(m5rast_filt,
			filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5raster/",sss,"_",seas,"_filtered.tif",sep=""),
			format="GTiff",overwrite=T)
	save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
	spse<-paste(sss,seas,sep="")
	mdlLst[[spse]]<-list(model=nmdl,vars=vif.vars)
	rm(list=c("m5rast","m5data"));gc()
	
	if(!sss %in% c("BAIS","BOBO","LETE","SPPI","WIFL")){
		seas="winter"
		print(paste(sss,seas))
		load(file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
		m5data<-as.data.frame(na.omit(m5data))
		m5data<-m5data[,which(!names(m5data) %in% c("cellId","NLCD_22","NLCD_23","NLCD_23","NLCD_90","NLCD_95"))]
		vif.vars<-vif_func(in_frame=m5data[,c(6:38)],thresh=5,trace=F)
		fml<-makeFormula(covs=vif.vars)
		#fit the model
		mdl<-lm(fml,m5data)
		nmdl<-stepAIC(mdl,k = qchisq(0.95, df=1),trace=F,direction="backward")
		m5data$predicted<-predict(nmdl);m5data$predicted<-ifelse(m5data$predicted<0,0,m5data$predicted)
		#predict to covars
		m5rast<-predict(covars,nmdl);m5rast[]<-ifelse(m5rast[]<0,0,m5rast[])
		filtname<-getFilterName(sss)
		m5rast_filt<-addSpatialFilter(rast=m5rast,sss=sss,filtname=filtname)
		#save the model, data (with padus info), and predicted raster
		writeRaster(m5rast,
				filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5raster/",sss,"_",seas,"_filtered.tif",sep=""),
				format="GTiff",overwrite=T)
		save(m5data,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/",sss,"_",seas,".RData",sep=""))
		spse<-paste(sss,seas,sep="")
		mdlLst[[spse]]<-list(model=nmdl,vars=vif.vars)
		rm(list=c("m5rast","m5data"));gc()
	}
	rm(list=c("m5rast","m5data"));gc()
}

save(mdlLst,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/mdlLst.RData")


