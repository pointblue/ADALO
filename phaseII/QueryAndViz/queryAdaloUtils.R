# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file contains the functions used by the file queryAdalo.R

##################################
## Functions -- 

# This is the main function to ask the questions. It is the function that defines the kinds of questions you can ask. 
# Arguments of the function --
# by 				Either species or region (default), indicating that either different species or different regions will make the rows of the output table
# metric			An integer: 4 (empirical), 5 (ADALO sdm), 6 (HAPET sdm) or 7 (POINT BLUE sdm). Defaults to empirical (4)
# period			Either NA, 0 or 1 (all, winter or breeding, respectively). Defaults to all
# species			Mandatory; a vector of one or more species to consider for the comparisons
# padusCat			The name of the PADUS category against which to develop contrasts: mgmtType, mgrName, desType, or unitName
# catValues			The PADUS values to contrast
# geopolCat 		Names one of the geopolitical domains to query for data: USFWSregion, USFSregion, NPSregion, LCCregion, USJVregion, BCRregion, StateFIPS, or CountyFIPS
# geopolValues 		The values to filter for the geopolField (e.g., 6 or 8 for USFWSregion) - this is an integer
makeQuestion<-function(by="region",metric=4,period=NA,species,padusCat=NA,catValues=NA,geopolCat=NA,geopolValues=NA){
	conn<-odbcConnect("whadalo")
	doidf<-data.frame()
	if(!is.na(padusCat) && !is.na(catValues) && NROW(padusCat)==1){
		if(NROW(catValues)==1){
			filtvals<-paste0(padusCat," like '%",catValues,"%'")
		}else{
			filts<-character()
			for(vv in catValues){
				filts<-c(filts,paste0("(",padusCat," like '%",vv,"%')"))
			}
			filtvals<-paste(filts,collapse=" OR ")
		}
		poisql<-paste0("select ",padusCat,",padusObjId from padusCats where ",filtvals)
		doidf<-sqlQuery(conn,poisql)
		doidf<-aggManyNames(doidf,mnames=catValues)
	}
	
	
	if(!is.na(geopolCat) && !is.na(geopolValues) && NROW(geopolCat)==1){
		reAS<-FALSE
	}else{
		reAS<-TRUE
	}
	
	domdf<-data.frame()
	for(ss in species){
		if(nrow(doidf)>0){
			dfpad<-getValuesByMetric(spcd=tolower(ss),metricVal=metric,doidf=doidf,conn=conn,filtPeriod=period,reportAreaSurv=reAS)
			domdf<-rbind(domdf,dfpad)
		}
		if(!is.na(geopolCat) && !is.na(geopolValues) && NROW(geopolCat)==1){
			for(gg in geopolValues){
				dfgeo<-getValuesByMetric(spcd=tolower(ss),metricVal=metric,conn=conn,doidf=NA,filtPeriod=period,geopolField=geopolCat,geopolValue=gg)
				domdf<-rbind(domdf,dfgeo)
			}
		}
	}
	odbcClose(conn)
	
	#make constrasts
	reslst<-makeContrast(by=by,domdf=domdf,reportAreaSurv=reAS)
	return(reslst)
}

## getValuesBy Metric is the workhorse function - you can ask to get values based on a PADUS domain of interest, or based on some other domain
# conn				The connection to whadalo
# spcd 				The speciescode
# metricVal 		The metric to use: 4, 5 or 6
# doidf 			The dataframe with the padus ObjectIds of the PADUS Domain Of Interest.
# filtPeriod		Integer telling the period to consider: winter (0) or breeding (1)
# geopolField 		Names one of the geopolitical domains to query for data: USFWSregion, USFSregion, NPSregion, LCCregion, USJVregion, BCRregion, StateFIPS, or CountyFIPS
# geopolValue 		The value to filter for the geopolField (e.g., 6 or 8 for USFWSregion) - this is an integer
# reportAreaSurv	Boolean indicating if to include %AreaSurveyed in the return. Default TRUE. If the request is a comparison between a unit and a geopolitical, set to FALSE
getValuesByMetric<-function(conn,spcd,metricVal,doidf=NA,filtPeriod=NA,geopolField=NA,geopolValue=NA,reportAreaSurv=TRUE){
	if(metricVal %in% c(4,5,6,7)){
		baseintq<-"select * from baseIntersects"
		if(is.data.frame(doidf)){
			poids<-paste(doidf$padusObjId,collapse=",")
			baseintq<-paste(baseintq," where padusObjId in (",poids,")",sep="")
		}else if(!is.na(geopolField) && !is.na(geopolValue)){
			if(!grepl("where",baseintq)){
				baseintq<-paste(baseintq,"where ")
			}else{
				baseintq<-paste(baseintq,"and ")
			}
			if(is.character(geopolValue)){
				baseintq<-paste(baseintq,geopolField," in ('",geopolValue,"')",sep="")
			}else{
				baseintq<-paste(baseintq,geopolField," in (",geopolValue,")",sep="")
			}
		}else{}
		
		sptblq<-paste("select * from ",spcd," where metric = ",metricVal,sep="")
		if(is.data.frame(doidf)){
			sptblq<-paste(sptblq," and padusObjId in (",poids,")",sep="")
		}
		if(!is.na(filtPeriod)){
			sptblq<-paste(sptblq,"and period =",filtPeriod)
		}
		
		sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from (",baseintq,") as t1 left join (",sptblq,") ", 
				"as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId)",sep="")
		
		df<-sqlQuery(conn,sqlq)
		if(metricVal==4){
			df<-subset(df,!is.na(df$cellMetric))
		}else{
			df$cellMetric<-ifelse(is.na(df$cellMetric),0,df$cellMetric)
		}
		if(nrow(df)>0){
			if(is.data.frame(doidf)){
				df<-merge(df,doidf,by="padusObjId",all.x=T)
				tcells<-aggregate(ncells~unitName,df,sum,na.rm=T)
				tdf<-aggregate(cellMetric~unitName,df,mean,na.rm=T)
				names(tdf)<-gsub("cellMetric","estDensity",names(tdf))
				resdf<-merge(tdf,tcells,by="unitName")
				resdf$estAbundance<-resdf$estDensity*resdf$ncells
				resdf$estAbundance<-ceiling(resdf$estAbundance/1089)
				resdf$estDensity<-resdf$estDensity/98.01	#reporting values by hectare
				resdf$estDensity<-round(resdf$estDensity,2)
				if(metricVal==4){
					nc5<-sqlQuery(conn,baseintq)
					ncdf<-merge(nc5,doidf,by="padusObjId",all.x=T)
					ncells5<-aggregate(ncells~unitName,ncdf,sum,na.rm=T)
					names(ncells5)<-gsub("ncells","totalcells",names(ncells5))
					resdf<-merge(resdf,ncells5[,c("unitName","totalcells")],by="unitName",all.x=T)
					resdf$percAreaSurveyed<-round(resdf$ncells*100/resdf$totalcells)
				}else{
					resdf$totalcells<-resdf$ncells
					resdf$percAreaSurveyed<-100
				}
				names(resdf)<-gsub("unitName","Region",names(resdf))
				resdf<-resdf[,which(!names(resdf) %in% "totalcells")]
				if(reportAreaSurv==FALSE){
					resdf<-resdf[,which(!names(resdf) %in% "percAreaSurveyed")]
				}
			}else{
				df$count<-df$ncells*df$cellMetric
				tcells<-sum(df$ncells,na.rm=T)
				tmetric<-sum(df$count,na.rm=T)
				resdf<-data.frame(Region=paste(geopolField,geopolValue),estDensity=tmetric/tcells/1089,ncells=tcells,estAbundance=ceiling(tmetric/1089))
			}
			resdf$species<-spcd
			resdf$metric<-metricVal
		}else{resdf<-NA}	#when m4 has no data for this locale
	}else{resdf<-NA}	#bad request: metric is not 4, 5, or 6
	return(resdf)
}

## This functions consolidates several names for the same unit under a single name. These several names are returned from a "like" query 
## (example: Valentine NWR, Valentine NWR I, Valentine NWR II, Valentine NWR III, and Valentine NWR IV all exist in PADUS)
# doidf 	The padus objects dataframe to simplify
# mnames 	A character string with the names to simplify to
aggManyNames<-function(doidf,mnames){
	sdf<-data.frame()
	for(nn in mnames){
		tdf<-subset(doidf,grepl(nn,unitName))
		tdf$unitName<-nn
		sdf<-rbind(sdf,tdf)
	}
	return(sdf)
}

## This function replaces NAs with 0s in the result tables
# df		The table to process
# defvar		The columns to ignore
replaceNAs<-function(df,defvar){
	cnms<-names(df)
	cnms<-subset(cnms,!cnms %in% defvar)
	for(mm in cnms){
		df[,mm]<-ifelse(is.na(df[,mm]),0,df[,mm])
	}
	return(df)
}

## This function converts the raw data into ADALO-type contrasts between species or regions
# by 				The type of contrast. Valid values: region (same species and period - compare across regions; this is the default), 
#						or species (same region and period - compare across species)
# domdf 			The data frame containing the data for which to develop the contrasts
# reportAreaSurv	Boolean indicating if to include %AreaSurveyed in the return. Default TRUE. If the request is a comparison between a unit and a geopolitical, set to FALSE
makeContrast<-function(by="region",domdf,reportAreaSurv=TRUE){
	reslst<-list()
	if(nrow(domdf)==1){
		reslst$error<-"The domain data.frame has only 1 row - nothing to contrast"
		reslst$rawdata<-domdf
	}else if(!is.data.frame(domdf)){
		reslst$error<-"The domain data.frame has no data and is not a data.frame - error obtaining the domain data.frame"
		reslst$rawdata<-domdf
	}else{
		reslst$rawdata<-domdf
		dataSource<-ifelse(unique(domdf$metric)==4,"Empirical",ifelse(unique(domdf$metric)==5,"Model_ADALO",ifelse(unique(domdf$metric)==6,"Model_HAPET","Model_ECN")))
		if(reportAreaSurv==TRUE){
			pltsurv<-ggplot(data=domdf,aes(x=Region,y=percAreaSurveyed)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="% area surveyed")
			if(NROW(unique(domdf$species))>1){
				ns<-ifelse(NROW(unique(domdf$species))%in% c(2,4),2,3)
				pltsurv<-pltsurv + facet_wrap(~species,ncol=ns)
			}
			reslst$pltsurv<-pltsurv
			densvars<-c("Region","estDensity","percAreaSurveyed","species")
			idvarreg<-c("Region","percAreaSurveyed");idvarspp<-c("species","percAreaSurveyed")
			tblsurv<-reshape(domdf[,c("Region","percAreaSurveyed","species")],idvar=c("Region"),timevar="species",direction="wide")
			names(tblsurv)<-gsub("percAreaSurveyed.","%Surv_",names(tblsurv))
			tblsurv<-replaceNAs(df=tblsurv,defvar=c("Region"))
			reslst$tblsurv<-tblsurv
		}else{
			densvars<-c("Region","estDensity","species")
			idvarreg<-c("Region");idvarspp<-c("species")
		}
		
		if(tolower(by)=="region"){
			tbldens<-reshape(domdf[,densvars],idvar=idvarreg,timevar="species",direction="wide")
			names(tbldens)<-gsub("percAreaSurveyed","%AreaSurveyed",names(tbldens))
			names(tbldens)<-gsub("estDensity.","",names(tbldens))
			tblabund<-reshape(domdf[,c("Region","estAbundance","species")],idvar=c("Region"),timevar="species",direction="wide")
			names(tblabund)<-gsub("estAbundance.","",names(tblabund))
			tbldens$Source<-dataSource;tblabund$Source<-dataSource
			tbldens<-replaceNAs(df=tbldens,defvar=c("Region","Source"))
			tblabund<-replaceNAs(df=tblabund,defvar=c("Region","Source"))
			
			reslst$tbldens<-tbldens;reslst$tblabund<-tblabund
			
			#make plots for density and abundance, where each bar is a region+period, faceted by species
			pltdens<-ggplot(data=domdf,aes(x=Region,y=estDensity)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Density (birds/Ha)")
			pltabun<-ggplot(data=domdf,aes(x=Region,y=estAbundance)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Abundance (# birds)")
			if(NROW(unique(domdf$species))>1){
				nc<-ifelse(NROW(unique(domdf$species))%in% c(2,4),2,3)
				pltdens<-pltdens + facet_wrap(~species,ncol=nc,scales="free")
				pltabun<-pltabun + facet_wrap(~species,ncol=nc,scales="free")
			}
			reslst$pltdens<-pltdens; reslst$pltabun<-pltabun
		}else if(tolower(by)=="species"){
			tbldens<-reshape(domdf[,densvars],idvar=idvarspp,timevar="Region",direction="wide")
			names(tbldens)<-gsub("percAreaSurveyed","%AreaSurveyed",names(tbldens))
			names(tbldens)<-gsub("estDensity.","",names(tbldens))
			tblabund<-reshape(domdf[,c("Region","estAbundance","species")],idvar=c("species"),timevar="Region",direction="wide")
			names(tblabund)<-gsub("estAbundance.","",names(tblabund))
			tbldens$Source<-dataSource;tblabund$Source<-dataSource
			tbldens<-replaceNAs(df=tbldens,defvar=c("species","Source"))
			tblabund<-replaceNAs(df=tblabund,defvar=c("species","Source"))
			
			reslst$tbldens<-tbldens;reslst$tblabund<-tblabund
			
			#make plots for density and abundance, where each bar is a region+period, faceted by species
			pltdens<-ggplot(data=domdf,aes(x=species,y=estDensity)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Density (birds/Ha)")
			pltabun<-ggplot(data=domdf,aes(x=species,y=estAbundance)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Abundance (# birds)")
			if(NROW(unique(domdf$Region))>1){
				nc<-ifelse(NROW(unique(domdf$Region))%in% c(2,4),2,3)
				pltdens<-pltdens + facet_wrap(~Region,ncol=nc,scales="free")
				pltabun<-pltabun + facet_wrap(~Region,ncol=nc,scales="free")
			}
			reslst$pltdens<-pltdens; reslst$pltabun<-pltabun
		}else{
			reslst$error<-"Wrong _by_ argument"
		}
	}
	return(reslst)
}


