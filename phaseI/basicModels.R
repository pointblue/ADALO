# TODO: Add comment
# 
# Author: lsalas
###############################################################################

######### Before assigning incidence rates...
# We hav a table with counts (or 0s), and attribution to land use types - one of which is OwnType.
# First approach is to assume each event is a replicate and see how these may affect naive presence via a logistic model
library(rgdal)
library(sp)
library(raster)
library(MASS)
#library(lme4)

#use this util I make to plot on google maps
source("c:/users/lsalas/git/sparklemotion/lsalas_rcode/LSalas/Maps/mapLocationsUtil.R")

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
	dff$area<-ifelse(is.na(dff$area),1,dff$area) #this should not happen
	return(dff)
}

addAgency<-function(df){	#The order here is critical...
	if("agency" %in% names(df)){
		df<-df[,which(names(df) != "agency")]
	}
	df$agency<-ifelse(df$refuge=="Y","Refuge",
						ifelse(df$statelands=="Y","State",
							ifelse(grepl("Bureau of Land Management",df$ownname),"BLM",
								ifelse(grepl("Department of Defense",df$ownname),"DOD",
									ifelse(grepl("Forest Service",df$ownname),"USFS",
										ifelse(grepl("National Park Service",df$ownname),"NPS",
											ifelse(df$OwnType=="Private","Private",
												ifelse(grepl("Private",df$padus),"Private",
													ifelse(grepl("Native",df$padus),"Private","OtherPublic")))))))))
#ifelse(grepl("Fish and Wildlife Service",collated.df$ownname),"FWS","OtherPublic"))))))))))
	return(df)
}

### CAREFUL HERE!!!
ags<-c("Refuge","Private","State","BLM","DOD","USFS","NPS","OtherPublic")

addLocation<-function(df){
	if("location" %in% names(df)){
		df<-df[,which(names(df) != "location")]
	}
	df$location<-ifelse(df$refuge=="Y",df$orgName,	#if it is a refuge interest, it will always have an orgName
			ifelse(df$OwnType=="Private","Private",
				ifelse(df$p_des_tp!="No data",df$p_des_tp,
					ifelse(grepl("Private",df$padus),"Private",
						ifelse(grepl("Native",df$padus),"Private",
							ifelse(!is.na(df$caState),as.character(df$caState),
								ifelse(!is.na(df$cdfw),as.character(df$cdfw),"Other sate/local/joint")))))))
	return(df)
}

fitRefugeModels<-function(collated.df,spcd,seasonVal="B",destName="Narrow",ags){
	newdat<-data.frame(agency=c("Refuge","Private","State","BLM","DOD","USFS","NPS","OtherPublic"),area=rep(1,times=8))
	res.df<-data.frame()
	cat4.df<-data.frame()
	cat5.df<-data.frame()
	lgmodels<-list()
	frequencies.df<-data.frame()
	probsPresent.df<-data.frame()
	nbrefuge.df<-data.frame()
	nbagency.df<-data.frame()
	nbmodels<-list()
	for(sss in spcd){
		print(sss)
		data.df<-subset(collated.df,SpeciesCode==sss)
		spfilt<-ifelse(sss=="WIFL","SW_WIFL_Dist",ifelse(sss=="CLRA","Yuma_RIRA_Dist",paste(sss,"_Dist",sep="")))
		data.df<-addSpatialFilter(data.df,spfilt)
		
		#need to add cellId
		data.df<-addCellId(data.df,sss)
		data.df$cellId<-as.factor(as.character(data.df$cellId))
				
		#need to add area of location
		data.df<-addArea(data.df)
		
		lgmf1<-as.formula("present~OwnType + area") # 
		#lgmf1<-as.formula("present~OwnType + area + (1|cellId)")
		lgmf2<-as.formula("present~refuge + area")
		#lgmf2<-as.formula("present~refuge + area + (1|cellId)")
		lgmf3<-as.formula("present~statelands + area")
		#lgmf3<-as.formula("present~statelands + area + (1|cellId)")
		lgmf4<-as.formula("present~agency + area")
		#lgmf4<-as.formula("present~agency + area + (1|cellId)")
		lgmf5<-as.formula("present~agency")
		#lgmf5<-as.formula("present~agency + (1|cellId)")
		lgmf6<-as.formula("present~refuge")
		#lgmf6<-as.formula("present~refuge + (1|cellId)")
		
		## Subset by season 
		dataSeason.df<-subset(data.df,season==seasonVal)
		dataSeason.df$fwsAppr2015<-as.character(dataSeason.df$fwsAppr2015)
		dataSeason.df$fwsAppr2015<-ifelse(is.na(dataSeason.df$fwsAppr2015),"NOT_FWS",dataSeason.df$fwsAppr2015)
		
		nall<-nrow(dataSeason.df)
		nRecsOwntype.df<-data.frame(all=nall,Private=sum(dataSeason.df$OwnType=="Private"),Other=(nall-(sum(dataSeason.df$OwnType=="Private"))))
		nRecsRefuge.df<-data.frame(all=nall,Refuge=sum(dataSeason.df$refuge=="Y"),Other=(nall-(sum(dataSeason.df$refuge=="Y"))))
		nRecsStatelands.df<-data.frame(all=nall,Statelands=sum(dataSeason.df$statelands=="Y"),Other=(nall-(sum(dataSeason.df$statelands=="Y"))))
		nRecsAgency.df<-data.frame(all=nall,Private=sum(dataSeason.df$agency=="Private"),
				Refuge=sum(dataSeason.df$agency=="Refuge"),
				State=sum(dataSeason.df$agency=="State"),
				BLM=sum(dataSeason.df$agency=="BLM"),
				DOD=sum(dataSeason.df$agency=="DOD"),
				USFS=sum(dataSeason.df$agency=="USFS"),
				NPS=sum(dataSeason.df$agency=="NPS"),
				otherFWS=sum(dataSeason.df$agency=="FWS"),
				otherPublic=sum(dataSeason.df$agency=="OtherPublic"))
		#dataSeason.df$area<-scale(dataSeason.df$area)
		mdl1<-glm(formula=lgmf1,data=dataSeason.df,family="binomial");coef1<-as.data.frame(coef(summary(mdl1)));coef1$param<-row.names(coef1)
		#mdl1<-glmer(formula=lgmf1,data=dataSeason.df,family="binomial");coef1<-as.data.frame(coef(summary(mdl1)));coef1$param<-row.names(coef1)
		mdl2<-glm(formula=lgmf2,data=dataSeason.df,family="binomial");coef2<-as.data.frame(coef(summary(mdl2)));coef2$param<-row.names(coef2)
		#mdl2<-glmer(formula=lgmf2,data=dataSeason.df,family="binomial");coef2<-as.data.frame(coef(summary(mdl2)));coef2$param<-row.names(coef2)
		mdl3<-glm(formula=lgmf3,data=dataSeason.df,family="binomial");coef3<-as.data.frame(coef(summary(mdl3)));coef3$param<-row.names(coef3)
		#mdl3<-glmer(formula=lgmf3,data=dataSeason.df,family="binomial");coef3<-as.data.frame(coef(summary(mdl3)));coef3$param<-row.names(coef3)
		mdl4<-glm(formula=lgmf4,data=dataSeason.df,family="binomial");coef4<-as.data.frame(coef(summary(mdl4)));coef4$param<-row.names(coef4)
		#mdl4<-glmer(formula=lgmf4,data=dataSeason.df,family="binomial");coef4<-as.data.frame(coef(summary(mdl4)));coef4$param<-row.names(coef4)
		mdl5<-glm(formula=lgmf5,data=dataSeason.df,family="binomial");coef5<-as.data.frame(coef(summary(mdl5)));coef5$param<-row.names(coef5)
		#mdl5<-glmer(formula=lgmf5,data=dataSeason.df,family="binomial");coef5<-as.data.frame(coef(summary(mdl5)));coef5$param<-row.names(coef5)
		mdl6<-glm(formula=lgmf6,data=dataSeason.df,family="binomial");coef6<-as.data.frame(coef(summary(mdl6)));coef6$param<-row.names(coef6)
		#mdl6<-glmer(formula=lgmf6,data=dataSeason.df,family="binomial");coef6<-as.data.frame(coef(summary(mdl6)));coef6$param<-row.names(coef6)
		
		ot1<-subset(coef1,param=="OwnTypePublic");ot2<-subset(coef2,param=="refugeY");ot3<-subset(coef3,param=="statelandsY")
		tdf<-data.frame(species=rep(sss,3),
				model=c("Ownership","Refuge","State"),
				logL=c(logLik(mdl1)[1],logLik(mdl2)[1],logLik(mdl3)[1]),
				aic=c(AIC(mdl1),AIC(mdl2),AIC(mdl3)),
				Param=c("OwnTypePublic","refugeY","statelandsY"),
				Coef=c(as.numeric(ot1[1]),as.numeric(ot2[1]),as.numeric(ot3[1])),
				StErr=c(as.numeric(ot1[2]),as.numeric(ot2[2]),as.numeric(ot3[2])),
				Pval=c(as.numeric(ot1[4]),as.numeric(ot2[4]),as.numeric(ot3[4]))
		)
		res.df<-rbind(res.df,tdf)
		coef4$SpeciesCode<-sss
		coef5$SpeciesCode<-sss
		coef6$SpeciesCode<-sss
		lgmodels[[sss]]<-list(mdl1,mdl2,mdl3,mdl4,mdl5,mdl6)
		preds<-predict(mdl5,newdat);btpreds<-exp(preds)/(1+exp(preds));	names(btpreds)<-newdat$agency
		#probPresentAgency<-getPreds(coef=coef5,categ="agency")
		probPresentAgency<-round(btpreds,digits=4);probPresentAgency<-as.data.frame(t(probPresentAgency));row.names(probPresentAgency)<-NULL
		probPresent<-probPresentAgency;probPresent$species<-sss
		probsPresent.df<-rbind(probsPresent.df,probPresent)
		refdata<-data.frame(refuge=c("N","Y"))
		preds<-predict(mdl6,refdata);btpreds<-exp(preds)/(1+exp(preds));names(btpreds)<-refdata$refuge
		probPresentRefuge<-round(btpreds,digits=4);probPresentRefuge<-as.data.frame(t(probPresentRefuge));row.names(probPresentRefuge)<-NULL;probPresent$species<-sss
		#probPresentRefuge<-getPreds(coef=coef5,categ="refuge")
				
		cat4.df<-rbind(cat4.df,coef4)
		cat5.df<-rbind(cat5.df,coef5)
		
		
		#############################################################################################
		## Event rate models
		## Aggregate obs by day
		dataAgg.df<-aggregate(as.formula("obsCount~obsDate+location+SpeciesCode+fwsAppr2015+OwnType+season+p_des_tp+year+refuge+statelands+agency+cellId+area"),
				data=dataSeason.df,FUN=sum,na.rm=FALSE)
		dataAgg.df$cellId<-as.factor(as.character(dataAgg.df$cellId))
		
		## create the Result df
		dataRes.df<-aggregate(as.formula("obsCount~location+Lat+Lon+SpeciesCode+fwsAppr2015+OwnType+season+p_des_tp+year+refuge+statelands+agency+cellId+area"),
				data=dataSeason.df,FUN=sum)
		
		## Include only those cells with counts, but then merge predicted incidence rate back to dataAgg.df
		filt<-aggregate(as.formula("obsCount~cellId"),data=dataAgg.df,FUN=sum);filt<-subset(filt,obsCount>0)
		dat<-subset(dataAgg.df,cellId %in% filt$cellId)
		
		nbm<-glm.nb(formula=as.formula("obsCount~cellId"),data=dat)
		filt$PredictedRate<-exp(predict(nbm,filt))
		
		dataRes.df<-merge(dataRes.df,filt[,c("cellId","PredictedRate")],by="cellId",all.x=TRUE)
		dataRes.df$PredictedRate<-ifelse(is.na(dataRes.df$PredictedRate),0,dataRes.df$PredictedRate)
		
		#Now average rates, but geometrically, sooo...
		cf<-min(filt$PredictedRate); dataRes.df$cfPredictedRate<-dataRes.df$PredictedRate+cf
		
		nbres.refuge<-estimateRatesByCat(df=dataRes.df,categ="refuge",cf=cf,spp=sss,pp=probPresentRefuge,ags)
		nbres.agency<-estimateRatesByCat(df=dataRes.df,categ="agency",cf=cf,spp=sss,pp=probPresentAgency,ags)
		nbrefuge.df<-rbind(nbrefuge.df,nbres.refuge)
		nbagency.df<-rbind(nbagency.df,nbres.agency)
		nbmodels[[sss]]<-nbm
		
		#now need a table with nevents, ndetections by agency, and area by agency
		nevents<-as.data.frame(table(dataSeason.df$agency)); nevents<-merge(ags,nevents,by.x="agency",by.y="Var1",all.x=TRUE)
		nevents$Freq<-ifelse(is.na(nevents$Freq),0,nevents$Freq)
		ndetect<-as.data.frame(table(subset(dataSeason.df,present==1)$agency))
		evdets<-merge(nevents,ndetect,by.x="agency",by.y="Var1",all.x=TRUE);names(evdets)<-c("agency","N_events","N_detections")
		survare<-aggregate(as.formula("area ~ agency"),data=unique(dataSeason.df[,c("agency","area")]),FUN="sum")
		freqs<-merge(survare,evdets,by="agency",all.x=TRUE)
		detrat<-freqs$N_detections/freqs$N_events; alldr<-sum(detrat);dra<-detrat/freqs$area; alldra<-sum(dra)
		freqs$DetectionRate<-detrat/alldr
		freqs$DetRateByArea<-dra/alldra
		freqs$species<-sss
		
		frequencies.df<-rbind(frequencies.df,freqs)
		
		save(dataSeason.df,mdl1,mdl2,mdl3,mdl4,mdl5,coef1,coef2,coef3,coef4,coef5,freqs,nRecsOwntype.df,nRecsRefuge.df,nRecsStatelands.df,nRecsAgency.df,
				dataRes.df,nbrefuge.df,nbagency.df,nbmodels,
				file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/",sss,"_allRes_",seasonVal,".RData",sep=""))
	}
	write.csv(res.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/results_",seasonVal,".csv",sep=""))
	write.csv(cat4.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/cat4res_",seasonVal,".csv",sep=""))
	write.csv(cat5.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/cat5res_",seasonVal,".csv",sep=""))
	write.csv(frequencies.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/Frequencies_",seasonVal,".csv",sep=""))
	write.csv(probsPresent.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/ProbsPresent_",seasonVal,".csv",sep=""))
	write.csv(nbrefuge.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/nbRefuge_",seasonVal,".csv",sep=""))
	write.csv(nbagency.df,file=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/",destName,"/nbAgency_",seasonVal,".csv",sep=""))
	return(1)
}

estimateRatesByCat<-function(df,categ,cf,spp,pp,ags){
	probs<-as.data.frame(t(pp));names(probs)<-"probPresent";probs[,categ]<-row.names(probs)
	df$detected<-ifelse(df$obsCount>0,1,0)
	ttdat<-unique(df[,c("cellId",categ,"cfPredictedRate")])
	fml<-as.formula(paste("log(cfPredictedRate) ~",categ))
	tt<-aggregate(fml,data=ttdat,FUN="mean")
	names(tt)<-c(categ,"lnRate")
	ttsd<-aggregate(fml,data=ttdat,FUN="sd")
	names(ttsd)<-c(categ,"lnSDRate");tt$lnSDRate<-ttsd$lnSDRate
	tt$lnUlim<-tt$lnRate+(1.96*tt$lnSDRate);tt$lnLlim<-tt$lnRate-(1.96*tt$lnSDRate)
	tt$AvgRate<-exp(tt$lnRate)-cf;tt$AvgRate<-ifelse(tt$AvgRate<0,0,tt$AvgRate)
	tt$ulim<-exp(tt$lnUlim)-cf;tt$ulim<-ifelse(tt$ulim<0,0,tt$ulim)
	tt$llim<-exp(tt$lnLlim)-cf;tt$llim<-ifelse(tt$llim<0,0,tt$llim)
	tt<-tt[,c(categ,"AvgRate","ulim","llim")]
	nc<-as.data.frame(table(ttdat[,categ]),stringsAsFactors = FALSE);names(nc)<-c(categ,"ncells")
	npdat<-unique(df[,c("cellId",categ,"detected")])
	np<-as.data.frame(table(npdat[,c(categ,"detected")]),stringsAsFactors = FALSE);names(np)<-c(categ,"detected","cellsWdet")
	np<-subset(np,detected==1,select=c(categ,"cellsWdet"))
	#asdata<-unique(df[,c("area",categ)]) #USE ncells as area!!!!
	#ad<-round(aggregate(as.formula(paste("area ~",categ)),data=asdata,FUN=sum));names(ad)<-c(categ,"totalAreakm2")
	if(categ=="refuge" && nrow(tt)==1){
		cav<-ifelse(tt$refuge=="Y","N","Y")
		tt[2,]<-c(cav,0,0,0)
		nc[2,]<-c(cav,0)
		np[2,]<-c(cav,0)
		#ad[2,]<-c(cav,0)
	}else if(categ=="agency" && nrow(tt)<NROW(ags)){ #CAREFUL!! see ags definition above
		agv<-subset(ags,!ags %in% tt$agency)
		agtt<-data.frame(agency=agv,AvgRate=0,ulim=0,llim=0);tt<-rbind(tt,agtt)
		agnc<-data.frame(agency=agv,ncells=0);nc<-rbind(nc,agnc)
		agnp<-data.frame(agency=agv,cellsWdet=0);np<-rbind(np,agnp)
		#agad<-data.frame(agency=agv,totalAreakm2=0);ad<-rbind(ad,agad)
	}else{
		#extend here
	}
	nc$ncells<-as.numeric(as.character(nc$ncells));np$cellsWdet<-as.numeric(as.character(np$cellsWdet))#;ad$totalAreakm2<-as.numeric(as.character(ad$totalAreakm2))
	ne<-as.data.frame(table(df[,c(categ,"detected")]),stringsAsFactors = FALSE);names(ne)<-c(categ,"detected","eventsWdet");ne<-subset(ne,detected==1,select=c(categ,"eventsWdet"))
	ne2<-as.data.frame(table(df[,categ]),stringsAsFactors = FALSE);names(ne2)<-c(categ,"nEvents");ne<-merge(ne2,ne)
	ne$posEventFreq<-ifelse(ne$nEvents==0,0,ne$eventsWdet/ne$nEvents);ne$scaledPosEventFreq<-scalefreq(ne$posEventFreq)
	tt<-merge(tt,probs,by=categ)
	tt<-merge(tt,nc,by=categ)
	#tt<-merge(tt,ad,by=categ)
	tt<-merge(tt,np)
	tt<-merge(tt,ne,by=categ)
	tt$posEventArea<-ifelse(tt$ncells==0,0,tt$posEventFreq/tt$ncells)
	tt$scaledPosEventArea<-scalefreq(tt$posEventArea)
	tt$posCellFreq<-ifelse(tt$ncells==0,0,tt$cellsWdet/tt$ncells)
	tt$scaledPosCellFreq<-scalefreq(tt$posCellFreq)
	#tt$detCellArea<-ifelse(tt$totalAreakm2==0,0,tt$posCellFreq/tt$totalAreakm2)
	#tt$scaledPosCellArea<-scalefreq(tt$detCellArea)
	tt$Species<-spp
	return(tt)
}

scalefreq<-function(x){
	tot<-sum(x,na.rm=TRUE)
	sx<-ifelse(is.na(x),0,round(x/tot,digits=4))
	return(sx)
}

getPreds<-function(coef,categ){
	df<-coef[,c("param","Estimate")]
	int<-df[1,2]
	if(categ=="agency"){
		df[1,1]<-"agencyBLM"
	}else if(categ=="refuge"){
		df[1,1]<-"refugeN"
	}else{}
	agn<-character()
	est<-numeric()
	for(ii in 1:nrow(df)){
		ag<-substr(df[ii,1],7,nchar(df[ii,1]));agn<-c(agn,ag)
		ev<-df[ii,2]
		if(ii>1){
			ev<-ev+int
		}
		esv<-exp(ev)/(1+exp(ev));est<-c(est,esv)
	}
	pdf<-data.frame(par=agn,Estimate=est);names(pdf)<-c(categ,"Estimate")
	return(pdf)
}

load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")

collated.df$year<-as.integer(format(collated.df$obsDate,"%Y"))
collated.df<-subset(collated.df,year>2004)
collated.df$present<-ifelse(collated.df$obsCount==0,0,1)
## fixing OwnType..., so now each record is either Public or Private
collated.df$OwnType<-ifelse(collated.df$OwnType=="Public","Public","Private")
collated.df$statelands<-ifelse(collated.df$padus %in% c("Local Government Land","State Land","Regional Agency Land"),"Y","N")


###########################################################################################
#Narrow Refuge definition...
collated.df$refuge<-ifelse(collated.df$fwsRSLType %in% c("NWR","WMA"),ifelse(collated.df$fwsStatus==0,"Y","N"),"N") #has to be NWR or WMA, but not status 1 or 2
collated.df<-addAgency(collated.df)
collated.df<-addLocation(collated.df) 
ags<-data.frame(agency=na.omit(unique(collated.df$agency)))
################################
#Breeding, all species
spcd<-c("BRCO","BLRA","LETE","CLRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL")
res<-fitRefugeModels(collated.df,spcd,seasonVal="B",destName="Narrow",ags=ags)
#Winter, BUT must exclude those who do not winter here: WIFL and LETE
spcd<-c("BRCO","BLRA","CLRA","CANV","SACR","LBCU","NOPI","TRBL")
res<-fitRefugeModels(collated.df,spcd,seasonVal="W",destName="Narrow",ags=ags)

###########################################################################################
#Now repeat all the above for the Encompassing definition
collated.df$refuge<-ifelse(!is.na(collated.df$orgName),ifelse(collated.df$fwsStatus %in% c(1,2),"N","Y"),"N") #has to have orgName, but not status 1 or 2
collated.df<-addAgency(collated.df)
collated.df<-addLocation(collated.df) 
ags<-data.frame(agency=na.omit(unique(collated.df$agency)))
################################
#Breeding, all species
spcd<-c("BRCO","BLRA","LETE","CLRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL")
res<-fitRefugeModels(collated.df,spcd,seasonVal="B",destName="Encompassing",ags=ags)
#Winter, BUT must exclude those who do not winter here: WIFL and LETE
spcd<-c("BRCO","BLRA","CLRA","CANV","SACR","LBCU","NOPI","TRBL")
res<-fitRefugeModels(collated.df,spcd,seasonVal="W",destName="Encompassing",ags=ags)



