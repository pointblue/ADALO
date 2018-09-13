# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(rgdal)
library(sp)
library(raster)

#need this function for further spatial filtering
addSpatialFilter<-function(df,filtname){
	#spfilt<-readOGR(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/geodata/",filtname,sep=""),layer=filtname)
	spfilt<-readOGR(paste("/home/lsalas/Documents/refuge/geodata/",filtname,sep=""),layer=filtname)
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

addSeason<-function(x){
	y<-"O"
	ss<-x[1];mm<-as.integer(x[2])
	if(ss %in% c("BRCO","CANV","NOPI")){
		y<-ifelse(mm %in% c(4:8),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss %in% c("BLRA","CLRA","TRBL")){
		y<-ifelse(mm %in% c(3:8),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss=="LETE"){
		y<-ifelse(mm %in% c(4:9),"B","O")
	}else if(ss=="SACR"){
		y<-ifelse(mm %in% c(3:9),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss=="LBCU"){
		y<-ifelse(mm %in% c(4:7),"B",ifelse(mm %in% c(1,11,12),"W","O"))
	}else if(ss=="WIFL"){
		y<-ifelse(mm %in% c(5:8),"B","O")
	}else{}
	return(y)
}

addCellId<-function(df,sss){
	ranm<-ifelse(sss=="CLRA","Yuma_RIRA_Dist_wgs84_id",ifelse(sss=="WIFL","SW_WIFL_Dist_wgs84_id","FWS_Region8_wgs84_id"))
	#cidrast<-raster(paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/fromNathan/grids/",ranm,"/",ranm,".tif",sep=""))
	cidrast<-raster(paste("/home/lsalas/Documents/refuge/grids/",ranm,"/",ranm,".tif",sep=""))
	cidProj<-projection(cidrast)
	sppts<-df[,c("Lon","Lat")]
	coordinates(sppts)<-c("Lon","Lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	sppts.cid<-spTransform(sppts,CRS(cidProj))
	df$cellId<-extract(cidrast,sppts.cid)
	return(df)
}

rbindVisits<-function(agg.df,rdf){
	ncr<-ncol(rdf)-5
	nca<-ncol(agg.df)-5
	if(nca>ncr){
		for(cc in 1:(nca-ncr)){
			cnam<-paste("obsCount.",(ncr+cc),sep="")
			rdf[,cnam]<-NA
		}
	}else if(nca<ncr){
		for(cc in 1:(ncr-nca)){
			cnam<-paste("obsCount.",(nca+cc),sep="")
			agg.df[,cnam]<-NA
		}
	}else{}
	agg.df<-rbind(agg.df,rdf)
	return(agg.df)
}

#load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")
load("/home/lsalas/Documents/refuge/collated.RData")

spcd<-c("BRCO","BLRA","LETE","CLRA","CANV","SACR","LBCU","NOPI","WIFL","TRBL")
for(sss in spcd){
	plot.df<-subset(collated.df,SpeciesCode==sss)
	
	#add spatial filter
	spfilt<-ifelse(sss=="WIFL","SW_WIFL_Dist",ifelse(sss=="CLRA","Yuma_RIRA_Dist",paste(sss,"_Dist",sep="")))
	data.df<-addSpatialFilter(plot.df,spfilt)	#CAREFUL 
	
	#create fourcats and the other classifiers
	data.df$refuge<-ifelse(is.na(data.df$fwsAppr),"N","Y")
	data.df$statelands<-ifelse(data.df$padus %in% c("Local Government Land","State Land","Regional Agency Land"),"Y","N")
	data.df$fourcats<-ifelse(data.df$OwnType=="Private","Private",
			ifelse(data.df$refuge=="Y","Refuge",
					ifelse(data.df$statelands=="Y","State","OtherPublic")))
	data.df$agency<-ifelse(data.df$OwnType=="Private","Private",
			ifelse(data.df$refuge=="Y","Refuge",
					ifelse(data.df$statelands=="Y","State",
							ifelse(data.df$ownname=="Bureau of Land Management (BLM)","BLM",
									ifelse(data.df$ownname=="Department of Defense (DOD) and Department of Ene*","DOD",
											ifelse(data.df$ownname=="Forest Service (USFS)","USFS",
													ifelse(data.df$ownname=="National Park Service (NPS)","NPS",
															ifelse(data.df$ownname=="Fish and Wildlife Service (FWS)","FWS","OtherPublic"))))))))
	
	#split by season (only care for B and W, separately), and filter by year 2000 and onwards)
	data.df<-subset(data.df,season %in% c("W","B"))
	data.df<-subset(data.df,as.integer(format(obsDate,"%Y"))>=2000)
	
	#need to add cellId
	data.df<-addCellId(data.df,sss)
		
	#for each season
	seaW.df<-subset(data.df,season=="W")
	aggW.df<-data.frame()
	if(nrow(seaW.df)>50){
		cIds<-unique(seaW.df$cellId)
		for(ccc in cIds){	#vectorize this with apply
			tdf<-subset(seaW.df,cellId==ccc)
			if(nrow(tdf)>4){
				#how to aggregate OwnType? For now use a hierarchy: Public -> private, so one trumps the others
				otc<-paste(tdf$OwnType,collapse=":")
				tdf$OwnType<-ifelse(grepl("Public",otc),"Public","Private")
				#do the same for refuge, stateland and fourcats
				rcc<-paste(tdf$refuge,collapse=":")
				tdf$refuge<-ifelse(grepl("Y",rcc),"Y","N")
				stc<-paste(tdf$statelands,collapse=":")
				tdf$statelands<-ifelse(grepl("Y",stc),"Y","N")
				fcc<-paste(tdf$agency,collapse=":")
				tdf$agency<-ifelse(grepl("Refuge",fcc),"Refuge",ifelse(grepl("State",fcc),"State",ifelse(grepl("BLM",fcc),"BLM",
										ifelse(grepl("USFS",fcc),"USFS",ifelse(grepl("DOD",fcc),"DOD",
														ifelse(grepl("NPS",fcc),"NPS",ifelse(grepl("FWS",fcc),"FWS",
																		ifelse(grepl("OtherPublic",fcc),"OtherPublic","Private"))))))))
				#So the point is to make sure all records in the same cell have the same aggregated values of these covariates
				#now sort ascendingly by date and assign "visit" number
				tdf<-tdf[order(tdf$obsDate),]
				tdf$visit<-c(1:nrow(tdf))
				
				#then reshape by obsDate
				#min.df<-reshape(obs.dat,idvar=c("id_code","SUBSITE","year"),timevar="visit",direction="wide")
				rdf<-reshape(tdf[,c("obsCount","SpeciesCode","visit","OwnType","refuge","statelands","agency")],idvar=c("SpeciesCode","OwnType","refuge","statelands","agency"),timevar="visit",direction="wide")
				
				#rbind
				if(nrow(aggW.df)==0){
					aggW.df<-rbind(aggW.df,rdf)
				}else{
					aggW.df<-rbindVisits(agg.df=aggW.df,rdf=rdf)
				}
				
			}
		}
	}
	
	#now breeding season
	seaB.df<-subset(data.df,season=="B")
	aggB.df<-data.frame()
	if(nrow(seaB.df)>50){
		cIds<-unique(seaB.df$cellId)
		for(ccc in cIds){	#vectorize this with apply
			tdf<-subset(seaB.df,cellId==ccc)
			if(nrow(tdf)>4){
				otc<-paste(tdf$OwnType,collapse=":")
				tdf$OwnType<-ifelse(grepl("Public",otc),"Public","Private")
				rcc<-paste(tdf$refuge,collapse=":")
				tdf$refuge<-ifelse(grepl("Y",rcc),"Y","N")
				stc<-paste(tdf$statelands,collapse=":")
				tdf$statelands<-ifelse(grepl("Y",stc),"Y","N")
				fcc<-paste(tdf$agency,collapse=":")
				tdf$agency<-ifelse(grepl("Refuge",fcc),"Refuge",ifelse(grepl("State",fcc),"State",ifelse(grepl("BLM",fcc),"BLM",
										ifelse(grepl("USFS",fcc),"USFS",ifelse(grepl("DOD",fcc),"DOD",
														ifelse(grepl("NPS",fcc),"NPS",ifelse(grepl("FWS",fcc),"FWS",
																		ifelse(grepl("OtherPublic",fcc),"OtherPublic","Private"))))))))
				tdf<-tdf[order(tdf$obsDate),]
				tdf$visit<-c(1:nrow(tdf))
				rdf<-reshape(tdf[,c("obsCount","SpeciesCode","visit","OwnType","refuge","statelands","fourcats")],idvar=c("SpeciesCode","OwnType","refuge","statelands","fourcats"),timevar="visit",direction="wide")
				if(nrow(aggB.df)==0){
					aggB.df<-rbind(aggB.df,rdf)
				}else{
					aggB.df<-rbindVisits(agg.df=aggB.df,rdf=rdf)
				}
			}
		}
	}
	save(aggW.df,aggB.df,data.df,file=paste("/home/lsalas/Documents/refuge/Incidence/",sss,".RData",sep=""))
}
