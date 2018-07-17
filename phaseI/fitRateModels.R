# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(rgdal)
library(sp)
library(raster)
library(MASS)

#use this util I made to plot on google maps
source("c:/users/lsalas/workspace/PRBOGeneralUpdate/LSalas/Maps/mapLocationsUtil.R")

#need this function for further spatial filtering
addSpatialFilter<-function(df,filtname){
	spfilt<-readOGR(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/geodata/",filtname,sep=""),layer=filtname)
	spfProj<-projection(spfilt)
	sppts<-df[,c("Lon","Lat")]
	coordinates(sppts)<-c("Lon","Lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	sppts.spf<-spTransform(sppts,CRS(spfProj))
	spfilt.attr<-over(sppts.spf,spfilt)
	df$distFilter<-spfilt.attr$Id
	df<-subset(df,!is.na(distFilter))
	return(df)
}

#function to add cell Id
addCellId<-function(df,sss){
	ranm<-ifelse(sss=="CLRA","Yuma_RIRA_Dist_wgs84_id",ifelse(sss=="WIFL","SW_WIFL_Dist_wgs84_id","FWS_Region8_wgs84_id"))
	cidrast<-raster(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/fromNathan/grids/",ranm,"/",ranm,".tif",sep=""))
	#cidrast<-raster(paste("/home/lsalas/Documents/refuge/grids/",ranm,"/",ranm,".tif",sep=""))
	cidProj<-projection(cidrast)
	sppts<-df[,c("Lon","Lat")]
	coordinates(sppts)<-c("Lon","Lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	sppts.cid<-spTransform(sppts,CRS(cidProj))
	df$cellId<-extract(cidrast,sppts.cid)
	return(df)
}

addArea<-function(df){
	dfa<-unique(df[,c("location","cellId")])
	dfag<-aggregate(as.formula("cellId~location"),data=dfa,FUN="NROW")
	names(dfag)<-c("location","area")
	dff<-merge(df,dfag,by="location",all.x=TRUE)
	dff$area<-ifelse(is.na(dff$area),1,dff$area)
	return(dff)
}

addAgency<-function(df){
	df$agency<-ifelse(df$OwnType=="Private","Private",
			ifelse(grepl("Private",df$padus),"Private",
					ifelse(grepl("Native",df$padus),"Private",
							ifelse(df$refuge=="Y","Refuge",
									ifelse(df$statelands=="Y","State",
											ifelse(grepl("Bureau of Land Management",df$ownname),"BLM",
													ifelse(grepl("Department of Defense",df$ownname),"DOD",
															ifelse(grepl("Forest Service",df$ownname),"USFS",
																	ifelse(grepl("National Park Service",df$ownname),"NPS","OtherPublic")))))))))
#ifelse(grepl("Fish and Wildlife Service",collated.df$ownname),"FWS","OtherPublic"))))))))))
	return(df)
}

addLocation<-function(df){
	df$location<-ifelse(df$OwnType=="Private","Private",
			ifelse(df$refuge=="Y",df$fwsAppr,
					ifelse(df$p_des_tp!="No data",df$p_des_tp,
							ifelse(grepl("Private",df$padus),"Private",
									ifelse(grepl("Native",df$padus),"Private",
											ifelse(!is.na(df$caState),as.character(df$caState),
													ifelse(!is.na(df$cdfw),as.character(df$cdfw),"Other sate/local/joint")))))))
	return(df)
}


fitRateModels<-function(collated.df,spcd,seasonVal="B",destName){
	newdat<-data.frame(agency=c("Private","Refuge","State","BLM","DOD","USFS","NPS","OtherPublic"),area=rep(1,times=8))
	res.df<-data.frame()
	cat4.df<-data.frame()
	cat5.df<-data.frame()
	frequencies.df<-data.frame()
	probsPresent.df<-data.frame()
	for(sss in spcd){
		print(sss)
		data.df<-subset(collated.df,SpeciesCode==sss)
		spfilt<-ifelse(sss=="WIFL","SW_WIFL_Dist",ifelse(sss=="CLRA","Yuma_RIRA_Dist",paste(sss,"_Dist",sep="")))
		data.df<-addSpatialFilter(data.df,spfilt)
		
		#need to add cellId
		data.df<-addCellId(data.df,sss)
		
		#need to add area of location
		data.df<-addArea(data.df)
		
		## Subset by season 
		dataSeason.df<-subset(data.df,season==seasonVal)
		
		## Aggregate obs by day
		dataAgg.df<-aggregate(as.formula("obsCount~obsDate+location+Lat+Lon+SpeciesCode+fwsAppr+fwsIntStatus+OwnType+season+p_des_tp+year+refuge+statelands+agency+cellId+area"),
				data=dataSeason.df,FUN=sum)
		dataAgg.df$cellId<-as.factor(as.character(dataAgg.df$cellId))
		
		## create the Result df
		dataRes.df<-aggregate(as.formula("obsCount~location+Lat+Lon+SpeciesCode+fwsAppr+fwsIntStatus+OwnType+season+p_des_tp+year+refuge+statelands+agency+cellId+area"),
				data=dataSeason.df,FUN=sum)
		
		## Include only those cells with counts, but then merge predicted incidence rate back to dataAgg.df
		filt<-aggregate(as.formula("obsCount~cellId"),data=dataAgg.df,FUN=sum);filt<-subset(filt,obsCount>0)
		dat<-subset(dataAgg.df,cellId %in% filt$cellId)
		
		nbm<-glm.nb(formula=as.formula("obsCount~cellId"),data=dat)
		filt$PredictedRate<-exp(predict(nbm,filt))
		
		dataRes.df<-merge(dataRes.df,filt[,c("cellId","PredictedRate")],by="cellId",all.x=TRUE)
		dataRes.df$PredictedRate<-ifelse(is.na(dataRes.df$PredictedRate),0,dataRes.df$PredictedRate)
		
		#now need a table with nevents, ndetections by agency, and area by agency, and the predicted detection rate
		nevents<-as.data.frame(table(dataRes.df$agency)); nevents<-merge(agcs,nevents,by.x="agency",by.y="Var1",all.x=TRUE)
		ndetect<-as.data.frame(table(subset(dataRes.df,present==1)$agency))#HERE!!
		evdets<-merge(nevents,ndetect,by.x="agency",by.y="Var1",all.x=TRUE);names(evdets)<-c("agency","N_events","N_detections")
		survare<-aggregate(as.formula("area ~ agency"),data=unique(dataB.df[,c("agency","area")]),FUN="sum")
		freqs<-merge(survare,evdets,by="agency",all.x=TRUE)
		detrat<-freqs$N_detections/freqs$N_events; alldr<-sum(detrat);dra<-detrat/freqs$area; alldra<-sum(dra)
		freqs$DetectionRate<-detrat/alldr
		freqs$DetRateByArea<-dra/alldra
		freqs$species<-sss
		
		frequencies.df<-rbind(frequencies.df,freqs)
		
		save(dataB.df,mdl1,mdl2,mdl3,mdl4,mdl5,coef1,coef2,coef3,coef4,coef5,freqs,nRecsOwntype.df,nRecsRefuge.df,nRecsStatelands.df,nRecsAgency.df,
				file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LogisticModels/",destName,"/",sss,"_logisticRes_",seasonVal,".RData",sep=""))
	}
	write.csv(res.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LogisticModels/",destName,"/results_",seasonVal,".csv",sep=""))
	write.csv(cat4.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LogisticModels/",destName,"/cat4res_",seasonVal,".csv",sep=""))
	write.csv(cat5.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LogisticModels/",destName,"/cat5res_",seasonVal,".csv",sep=""))
	write.csv(frequencies.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LogisticModels/",destName,"/Frequencies_",seasonVal,".csv",sep=""))
	write.csv(probsPresent.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/LogisticModels/",destName,"/ProbsPresent_",seasonVal,".csv",sep=""))
	return(1)
}

load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")

collated.df$year<-as.integer(format(collated.df$obsDate,"%Y"))
collated.df<-subset(collated.df,year>2004)
collated.df$present<-ifelse(collated.df$obsCount==0,0,1)

###########################################################################################
#Narrow Refuge definition...
collated.df$refuge<-ifelse((!is.na(collated.df$fwsAppr) & collated.df$fwsIntStatus==0),"Y","N")
collated.df$statelands<-ifelse(collated.df$padus %in% c("Local Government Land","State Land","Regional Agency Land"),"Y","N")
collated.df<-addAgency(collated.df)
collated.df<-addLocation(collated.df) 
agcs<-data.frame(agency=na.omit(unique(collated.df$agency)))

################################
#Breeding, all species
spcd<-c("BRCO","BLRA","LETE","CLRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL")
res<-fitRefugeModels(collated.df,spcd,seasonVal="B",destName="Appr0only")

