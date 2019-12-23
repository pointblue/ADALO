# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file contains the functions used by the file queryAdalo.R

##################################
## Functions -- 

# This is the main function to ask the questions. It is the function that defines the kinds of questions you can ask. 
# Arguments of the function --
# byComp 			Either species or area (default), indicating that either different species or different areas will make the rows of the output table
# metric			An integer: 4 (empirical), 5 (ADALO sdm), 6 (HAPET sdm) or 7 (POINT BLUE sdm). Defaults to empirical (4)
# period			Either NA, 0 or 1 (all, winter or breeding, respectively). Defaults to all
# species			Mandatory; a vector of one or more species to consider for the comparisons
# padusCat			The name of the PADUS category against which to develop contrasts: mgmtType, mgrName, desType, or unitName
# catValues			The PADUS values to contrast
# geopolCat 		Names one of the geopolitical domains to query for data: USFWSregion, USFSregion, NPSregion, LCCregion, USJVregion, BCRregion, StateFIPS, or CountyFIPS
# geopolValues 		The values to filter for the geopolField (e.g., 6 or 8 for USFWSregion) - this is an integer
# geopolRestrict	A boolean indicating that the question is restricted to the domain defined by the geopolCats
makeQuestion<-function(byComp="area",metric=4,period=NA,species,padusCat=NA,catValues=NA,geopolCat=NA,geopolValues=NA,geopolRestrict=TRUE){
	conn<-odbcConnect("whadalo")
	doidf<-data.frame()
	if(!is.na(padusCat) && !is.na(catValues) && NROW(padusCat)==1){
		if(NROW(catValues)==1){
			if(catValues=="all"){
				filtvals<-"1=1"
			}else{
				filtvals<-paste0("(",padusCat," like '%",catValues,"%')")
			}
		}else{
			filts<-character()
			for(vv in catValues){
				filts<-c(filts,paste0(padusCat," like '%",vv,"%'"))
			}
			filtvals<-paste0("(",filts,collapse=" OR ",")")
		}
		#Here filter for the encompassing domain if geopolCats are provided
		if(geopolRestrict==TRUE && !is.na(geopolCat) && !is.na(geopolValues)){
			filtvals<-paste0("(",filtvals,")")
			#padusObjId in the subset of objectIds defined by the restriction
			if(is.numeric(geopolValues)){
				geopv<-paste0("(",paste(geopolValues,collapse=","),")")
			}else{
				geopv<-paste0("('",paste(geopolValues,collapse="','"),"')")
			}
			restsql<-paste0("select distinct padusObjId from baseintersects where ",geopolCat, " in ",geopv)
			restdf<-sqlQuery(conn,restsql)
			restvals<-paste(restdf$padusObjId,collapse=",")
			filtvals<-paste0(filtvals," AND (padusObjId in (",restvals,"))")
		}
		poisql<-paste0("select ",padusCat,",padusObjId from paduscats where ",filtvals)
		doidf<-sqlQuery(conn,poisql)
		doidf<-aggManyNames(doidf,mcat=padusCat,mnames=catValues)
	}
	
	
	if(!is.na(geopolCat) && !is.na(geopolValues) && NROW(geopolCat)==1){
		reAS<-FALSE
	}else{
		reAS<-TRUE
	}
	
	domdf<-data.frame()
	for(ss in species){
		if(nrow(doidf)>0){
			dfpad<-getValuesByMetric(spcd=tolower(ss),metricVal=metric,doidf=doidf,padusCat=padusCat,conn=conn,filtPeriod=period,reportAreaSurv=reAS)
			if(!is.na(dfpad) && nrow(dfpad)>0){
				domdf<-rbind(domdf,dfpad)
			}
		}
		if(!is.na(geopolCat) && !is.na(geopolValues) && NROW(geopolCat)==1){
			if(geopolValues=="all"){
				#query all values of the geopol category and re-assign to geopolValues
				gpdict<-getDictionary(species=FALSE,padus=FALSE,jurisdiction=TRUE)$jurisdiction
				jurs<-as.character(unique(gpdict$Jurisdiction))
				jursdf<-ldply(.data=jurs,.fun=function(x,geopolCat){
							tdf=data.frame(Jurisdiction=x,use=grepl(x,geopolCat));
							return(tdf)},geopolCat=geopolCat)
				subjurs<-subset(jursdf,use==TRUE)$Jurisdiction
				geopolValues<-as.integer(subset(gpdict,Jurisdiction==subjurs)$Value)
			}
			for(gg in geopolValues){
				dfgeo<-getValuesByMetric(spcd=tolower(ss),metricVal=metric,conn=conn,doidf=NA,filtPeriod=period,geopolField=geopolCat,geopolValue=gg)
				if(!is.na(dfgeo) && nrow(dfgeo)>0){
					domdf<-rbind(domdf,dfgeo)
				}
			}
		}
	}
	odbcClose(conn)
	
	domdf$AreaSizeHA<-round(domdf$sumCells*9/100,1)
	domdf$metric<-ifelse(as.character(domdf$metric)=="4", "Empirical",
			ifelse(as.character(domdf$metric)=="5", "M_BASE",
					ifelse(as.character(domdf$metric)=="6", "M_HAPET","M_ECN")))
	
	#calculate relative abundance
	if(nrow(domdf)>1){
		tabund<-sum(domdf$wgtAbundance,na.rm=T)
		tabundta<-sum(subset(domdf,!Area %in% paste(geopolCat,geopolValues))$wgtAbundance,na.rm=T)
		tabundtg<-tabund-tabundta #ONLY if there is a single geopolCat!!!
		
		domdfout<-data.frame()
		for(ss in species){
			domspdf<-subset(domdf,species==tolower(ss))
			
			if(!is.na(padusCat) && catValues=="all" && !is.na(geopolCat) && NROW(geopolValues)==1 && geopolRestrict==TRUE && byComp=="area"){ #special case where we can calculate unprotected areas
				#calculate unprotected abundance as the difference between the domain categories and entire domain
				tbla<-subset(domspdf,!Area %in% paste(geopolCat,geopolValues))		#protected
				sumpresa<-sum(tbla$presenceHA, na.rm=TRUE)
				tblb<-subset(domspdf,Area %in% paste(geopolCat,geopolValues))		#domain
				sumpresb<-sum(tblb$presenceHA, na.rm=TRUE)
				#"Area","species","metric","sumCells","wgtSumMetric","wgtDensity","hectareDensity","wgtAbundance","presenceHA"
				tblc<-data.frame(Area="Unprotected",AreaSizeHA=tblb$AreaSizeHA-sum(tbla$AreaSizeHA,na.rm=T),
						species=unique(tbla$species),metric=unique(tbla$metric),sumCells=tblb$sumCells-sum(tbla$sumCells,na.rm=T),
						wgtSumMetric=tblb$wgtSumMetric-sum(tbla$wgtSumMetric,na.rm=T))
				tblc$wgtAbundance<-round(tblc$wgtSumMetric/1089,3)
				tblc$wgtDensity<-round(tblc$wgtSumMetric/tblc$sumCells,3)
				tblc$hectareDensity<-round(tblc$wgtDensity/98.01,6)
				tblc$presenceHA<-ifelse(metric==4,tblb$AreaSizeHA-sum(tbla$AreaSizeHA,na.rm=T),
						ifelse(TRUE %in% is.na(tblb$presenceHA),NA,
						ifelse(sumpresb>sumpresa,sumpresb-sumpresa,0)))		
				tblc<-tblc[,names(tbla)]
				tbld<-rbind(tbla,tblc)
				tabund<-sum(tbld$wgtAbundance,na.rm=T)
				tbld$relAbundance<-round(tbld$wgtAbundance*100/tabund,3)
				tblb$relAbundance<-100
				domspdf<-rbind(tbld,tblb)
				domdfout<-rbind(domdfout,domspdf)
			}else if(!is.na(padusCat) && !is.na(geopolCat) && NROW(geopolCat)==1){# both padus AND geopol present, but only one geopol cat. How to extend to several? 
				#simply assign NA to geopols and calculate the relative abundance of all remaining 
				tbla<-subset(domspdf,!Area %in% paste(geopolCat,geopolValues))
				tbla$relAbundance<-round(tbla$wgtAbundance*100/tabundta,3)
				tblb<-subset(domspdf,Area %in% paste(geopolCat,geopolValues))
				tblb$relAbundance<-round(tblb$wgtAbundance*100/tabundtg,3)
				domspdf<-rbind(tbla,tblb)
				domdfout<-rbind(domdfout,domspdf)
			}else{# either padus or geopol present
				#a simple comparison of relative abundance between selected categories
				domspdf$relAbundance<-round(domspdf$wgtAbundance*100/tabund,3)
				domdfout<-rbind(domdfout,domspdf)
			}
		}
		domdf<-domdfout
	}
	
	return(domdf)
}

#make constrasts
# domdf 			is the result data.frame from makeQuestion
# reportAreaSurv 	is a boolean indicating if the percent area surveyed should be reported. Matters only for metric 4 (empirical). Default FAlSE
# byComp 			is a string (either: area or species) indicating how to construct the comparison. It assumes one or the other has more levels, thus gives it row priority:
#	if the string is "area" each row in the table is a different padus/geopolitical level, each column is a species, and cell value is indicated with outp. This is the default.
#	if the string is "species" each row is a species, each column a different area, and cell value is as prescribed in outp
# outp 				is either dens (density) or abund (abundance index) - the content of cells in the output table
# outt 				indicates if to return a table or plot: string table or plot. Defaults to "table"
# plotSorted		indicates if the output should be sorted by the output parameter (outp) descending
# valueHighlight	a string that states which value in the plot should be a bar of the primary color (see colorsHighlight for details)
# colorsHighlight	two strings indicating the hex number (so, they begin with #) of two colors. Default is c(#ca0020, #0571b0), where the first is the primary color and is 
#					assigned to the value to highlight. See here for details (make sure to check the colorblind safe box): http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=4
makeContrast<-function(domdf,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="table",plotSorted=TRUE, valueHighlight=NA, colorsHighlight=c("#ca0020", "#0571b0")){
	if(nrow(domdf)==1){
		res<-"The results data.frame has only 1 row - nothing to contrast"
	}else if(!is.data.frame(domdf)){
		res<-"The result data.frame has no data and is not a data.frame - error obtaining the domain data.frame"
	}else{
		dataSource<-unique(domdf$metric)
		if(reportAreaSurv==TRUE && unique(domdf$metric)=="Empirical" && byComp=="area"){	#compare areas by percent area surveyed.
			if(outt=="plot"){ #percAreaSuv as plot
				if(plotSorted==TRUE){
					domdf$Area<-reorder(domdf$Area,domdf$percAreaSurveyed)
				}
				if(!is.na(valueHighlight)){
					domdf$areaColor<-ifelse(domdf$Area==valueHighlight,"primCol","secCol")
					res<-ggplot(data=domdf,aes(x=Area,y=percAreaSurveyed)) + geom_bar(stat="identity",width = 0.6, aes(fill=areaColor)) + theme_bw() +
							scale_fill_manual(values=colorsHighlight, guide=FALSE) + coord_flip() + labs(x="",y="% area surveyed")
				}else{
					res<-ggplot(data=domdf,aes(x=Area,y=percAreaSurveyed)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="% area surveyed")
					
				}
				if(NROW(unique(domdf$species))>1){
					ns<-ifelse(NROW(unique(domdf$species))%in% c(2,4),2,3)
					pltsurv<-pltsurv + facet_wrap(~species,ncol=ns)
				}
			}else{	#percAreaSuv as table
				res<-reshape(domdf[,c("Area","percAreaSurveyed","species")],idvar=c("Area"),timevar="species",direction="wide")
				names(res)<-gsub("percAreaSurveyed.","%Surv_",names(res))
				res<-replaceNAs(df=res,defvar=c("Area","AreaSizeHA"))
			}
		}else if(byComp=="species"){	#comparison by species
			if(outp=="dens"){	#density by species
				if(outt=="plot"){	#density by species as plot
					if(plotSorted==TRUE){
						domdf$species<-reorder(domdf$species,domdf$hectareDensity)
					}
					if(!is.na(valueHighlight)){
						domdf$speciesColor<-ifelse(domdf$species==valueHighlight,"primCol","secCol")
						pltdens<-ggplot(data=domdf,aes(x=species,y=hectareDensity)) + geom_bar(stat="identity",width = 0.6, aes(fill=speciesColor)) + theme_bw() +
								scale_fill_manual(values=colorsHighlight, guide=FALSE) + coord_flip() + labs(x="",y="Density (birds/Ha)")
					}else{
						pltdens<-ggplot(data=domdf,aes(x=species,y=hectareDensity)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Density (birds/Ha)")
					}
					if(NROW(unique(domdf$Area))>1){
						nc<-ifelse(NROW(unique(domdf$Area))%in% c(2,4),2,3)
						pltdens<-pltdens + facet_wrap(~Area,ncol=nc,scales="free")
					}
					res<-pltdens
				}else{	#density by species as table
					densvars<-c("Area","wgtDensity","species")
					tbldens<-reshape(domdf[,densvars],idvar="species",timevar="Area",direction="wide")
					names(tbldens)<-gsub("wgtDensity.","",names(tbldens))
					tbldens$Source<-dataSource
					tbldens<-replaceNAs(df=tbldens,defvar=c("species","Source"))
					res<-tbldens
				}
			}else{	#abundance by species
				if(outt=="plot"){	#abundance by species as a plot
					#sort species by abundance
					if(plotSorted==TRUE){
						domdf$species<-reorder(domdf$species,domdf$wgtAbundance)
					}
					if(!is.na(valueHighlight)){
						domdf$speciesColor<-ifelse(domdf$species==valueHighlight,"primCol","secCol")
						pltabun<-ggplot(data=domdf,aes(x=species,y=wgtAbundance)) + geom_bar(stat="identity",width = 0.6, aes(fill=speciesColor)) + theme_bw() +
								scale_fill_manual(values=colorsHighlight, guide=FALSE) + coord_flip() + labs(x="",y="Total Abundance Index")
					}else{
						pltabun<-ggplot(data=domdf,aes(x=species,y=wgtAbundance)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Total Abundance Index")
					}
					if(NROW(unique(domdf$Area))>1){
						nc<-ifelse(NROW(unique(domdf$Area))%in% c(2,4),2,3)
						pltabun<-pltabun + facet_wrap(~Area,ncol=nc,scales="free")
					}else{ 	#just 1 location, but still adding the strip with location name
						pltabun<-pltabun + facet_wrap(~Area)
					}
					res<-pltabun
				}else{	#abundance by species as a table
					abundvars<-c("Area","species","relAbundance")
					tblabund<-reshape(domdf[,abundvars],idvar="species",timevar="Area",direction="wide")
					names(tblabund)<-gsub("relAbundance.","",names(tblabund))
					tblabund$Source<-dataSource
					tblabund<-replaceNAs(df=tblabund,defvar=c("species","Source"))
					
					res<-tblabund
				}
			}
			
		}else if(byComp=="area"){	#comparison by area
			idvarreg<-c("Area","AreaSizeHA")
			aggvars<-c("Area","AreaSizeHA","species")
			if(outp=="dens"){
				if(outt=="plot"){
					if(plotSorted==TRUE){
						domdf$Area<-reorder(domdf$Area,domdf$hectareDensity)
					}
					if(!is.na(valueHighlight)){
						domdf$areaColor<-ifelse(domdf$Area==valueHighlight,"primCol","secCol")
						pltdens<-ggplot(data=domdf,aes(x=Area,y=hectareDensity)) + geom_bar(stat="identity",width = 0.6, aes(fill=areaColor)) + theme_bw() +
								scale_fill_manual(values=colorsHighlight, guide=FALSE) + coord_flip() + labs(x="",y="Density (birds/Ha)")
					}else{
						pltdens<-ggplot(data=domdf,aes(x=Area,y=hectareDensity)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Density (birds/Ha)")
					}
					if(NROW(unique(domdf$species))>1){
						nc<-ifelse(NROW(unique(domdf$species))%in% c(2,4),2,3)
						pltdens<-pltdens + facet_wrap(~species,ncol=nc,scales="free")
					}
					res<-pltdens
				}else{
					tbldens<-reshape(domdf[,c(aggvars,"wgtDensity")],idvar=idvarreg,timevar="species",direction="wide")
					names(tbldens)<-gsub("wgtDensity.","",names(tbldens))
					tbldens$Source<-dataSource
					tbldens<-replaceNAs(df=tbldens,defvar=c("Area","AreaSizeHA","Source"))
					res<-tbldens
				}
			}else{	#abundance
				if(outt=="plot"){
					if(plotSorted==TRUE){
						domdf$Area<-reorder(domdf$Area,domdf$wgtAbundance)
					}
					if(!is.na(valueHighlight)){
						domdf$areaColor<-ifelse(domdf$Area==valueHighlight,"primCol","secCol")
						pltabun<-ggplot(data=domdf,aes(x=Area,y=wgtAbundance)) + geom_bar(stat="identity",width = 0.6, aes(fill=areaColor)) + theme_bw() +
								scale_fill_manual(values=colorsHighlight, guide=FALSE) + coord_flip() + labs(x="",y="Total Abundance Index")
					}else{
						pltabun<-ggplot(data=domdf,aes(x=Area,y=wgtAbundance)) + geom_bar(stat="identity",width = 0.6) + coord_flip() + labs(x="",y="Total Abundance Index")
					}
					if(NROW(unique(domdf$species))>1){
						nc<-ifelse(NROW(unique(domdf$species))%in% c(2,4),2,3)
						pltabun<-pltabun + facet_wrap(~species,ncol=nc,scales="free")
					}
					res<-pltabun
				}else{
					tblabund<-reshape(domdf[,c(aggvars,"relAbundance")],idvar=idvarreg,timevar="species",direction="wide")
					names(tblabund)<-gsub("relAbundance.","",names(tblabund))
					tblabund$Source<-dataSource
					tblabund<-replaceNAs(df=tblabund,defvar=c("Area","AreaSizeHA","Source"))
					
					res<-tblabund
				}
			}
		}else{
			res<-"Wrong _byComp_ argument value"
		}
	}
	return(res)
	
}

## getValuesBy Metric is the workhorse function - you can ask to get values based on a PADUS domain of interest, or based on some other domain
# conn				The connection to whadalo
# spcd 				The speciescode
# metricVal 		The metric to use: 4, 5 or 6
# doidf 			The dataframe with the padus ObjectIds of the PADUS Domain Of Interest.
# padusCat			If providing doidf, need to name the padusCategory to use
# filtPeriod		Integer telling the period to consider: winter (0) or breeding (1)
# geopolField 		Names one of the geopolitical domains to query for data: USFWSregion, USFSregion, NPSregion, LCCregion, USJVregion, BCRregion, StateFIPS, or CountyFIPS
# geopolValue 		The value to filter for the geopolField (e.g., 6 or 8 for USFWSregion) - this is an integer
# reportAreaSurv	Boolean indicating if to include %AreaSurveyed in the return. Default TRUE. If the request is a comparison between a unit and a geopolitical, set to FALSE
getValuesByMetric<-function(conn,spcd,metricVal,doidf=NA,padusCat=NA,filtPeriod=NA,geopolField=NA,geopolValue=NA,reportAreaSurv=TRUE){
	if(metricVal %in% c(4,5,6,7)){
		baseintq<-"select * from baseintersects"
		if(is.data.frame(doidf)){
			poids<-paste(doidf$padusObjId,collapse=",")
			baseintq<-paste(baseintq," where padusObjId in (",poids,")",sep="")
		}else if(!is.na(geopolField) && !is.na(geopolValue)){
			if(!grepl("where",baseintq)){
				baseintq<-paste(baseintq,"where ")
			}else{
				baseintq<-paste(baseintq,"and ")
			}
			if(is.character(geopolValue) && tolower(geopolValue) != "all"){
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
		
		sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.metricValue, t2.cellMetric from (",baseintq,") as t1 left join (",sptblq,") ", 
				"as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId)",sep="")
		
		df<-sqlQuery(conn,sqlq)
		if(metricVal==4){
			df<-subset(df,!is.na(df$cellMetric))
			presCellsTotal<-sum(subset(df,!is.na(df$metricValue))$ncells)
			presHAtotal<-NA
		}else{
			df$cellMetric<-ifelse(is.na(df$cellMetric),0,df$cellMetric)
			presCellsTotal<-sum(subset(df,!is.na(df$metricValue))$ncells)
			presHAtotal<-presCellsTotal*0.09
		}
		if(nrow(df)>0 && presCellsTotal>0){
			repfields<-c("Area","species","metric","sumCells","wgtSumMetric","wgtDensity","hectareDensity","wgtAbundance","percAreaSurveyed","presenceHA")
			if(is.data.frame(doidf)){
				#sumCells, #wgtSumMetric, #wgtAverageMetric
				df<-merge(df,doidf,by="padusObjId",all.x=T)
				tcells<-aggregate(as.formula(paste0("ncells~",padusCat)),df,sum,na.rm=T)
				names(tcells)<-gsub("ncells","sumCells",names(tcells))
				tnzcells<-aggregate(as.formula(paste0("ncells~",padusCat)),subset(df,!is.na(metricValue) & metricValue>0),sum,na.rm=T)
				names(tnzcells)<-gsub("ncells","sumNZCells",names(tnzcells))
				resdf<-merge(tcells,tnzcells,by=padusCat)
				tdf<-aggregate(as.formula(paste0("cellMetric~",padusCat)),df,sum,na.rm=T)
				names(tdf)<-gsub("cellMetric","wgtSumMetric",names(tdf))
				resdf<-merge(resdf,tdf,by=padusCat)
				resdf$wgtAbundance<-round(resdf$wgtSumMetric/1089,3)
				resdf$wgtDensity<-round(resdf$wgtSumMetric/resdf$sumCells,3)
				resdf$hectareDensity<-round(resdf$wgtDensity/98.01,6)
				resdf$presenceHA<-round(resdf$sumNZCells*0.09)  #the number of cells with abundance x 900 m2/ 10,000 m2
				if(metricVal==4){
					nc5<-sqlQuery(conn,baseintq)
					ncdf<-merge(nc5,doidf,by="padusObjId",all.x=T)
					ncells5<-aggregate(as.formula(paste0("ncells~",padusCat)),ncdf,sum,na.rm=T)
					names(ncells5)<-gsub("ncells","totalCells",names(ncells5))
					resdf<-merge(resdf,ncells5[,c(padusCat,"totalCells")],by=padusCat,all.x=T)
					resdf$percAreaSurveyed<-round(resdf$sumCells*100/resdf$totalCells)
				}else{
					resdf$totalcells<-resdf$totalCells
					resdf$percAreaSurveyed<-100
				}
				names(resdf)<-gsub(padusCat,"Area",names(resdf))
				resdf<-resdf[,which(!names(resdf) %in% "totalCells")]
				if(reportAreaSurv==FALSE){
					resdf<-resdf[,which(!names(resdf) %in% "percAreaSurveyed")]
					repfields<-subset(repfields,repfields!="percAreaSurveyed")
				}
			}else{ #no padus just geopol
				repfields<-subset(repfields,repfields!="percAreaSurveyed")
				sumCells<-sum(df$ncells,na.rm=T)
				wgtSumMetric<-sum(df$cellMetric,na.rm=T)
				wgtAbundance<-round(wgtSumMetric/1089,3)
				wgtDensity<-round(wgtSumMetric/sumCells,3)
				hectareDensity<-round(wgtDensity/98.01,6)
				presenceHA<-ifelse(is.na(presHAtotal),NA,presHAtotal)
				resdf<-data.frame(Area=paste(geopolField,geopolValue),sumCells=sumCells, sumNZCells=NA, wgtSumMetric=wgtSumMetric, wgtAbundance=wgtAbundance, wgtDensity=wgtDensity, hectareDensity=hectareDensity, presenceHA=presenceHA)
			}
			resdf$species<-spcd
			resdf$metric<-metricVal
			resdf<-resdf[,repfields]
		}else{resdf<-NA}	#when adalo has no data for this locale
	}else{resdf<-NA}	#bad request: metric is not 4, 5, 6, or 7
	return(resdf)
}

## This functions consolidates several names for the same unit under a single name. These several names are returned from a "like" query 
## (example: Valentine NWR, Valentine NWR I, Valentine NWR II, Valentine NWR III, and Valentine NWR IV all exist in PADUS)
# doidf 	The padus objects dataframe to simplify
# mcat		The padus category that contains the names to simplify
# mnames 	A character string with the names to simplify to
aggManyNames<-function(doidf,mcat,mnames){
	sdf<-data.frame()
	if(NROW(mnames)==1 && mnames=="all"){
		sdf<-unique(doidf)
	}else{
		for(nn in mnames){
			tdf<-subset(doidf,grepl(nn,doidf[,mcat]))
			if(nrow(tdf)>0){
				tdf[,mcat]<-nn
				sdf<-rbind(sdf,tdf)
			}
		}
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

## This function lists the dictionary of padus categories, or the dictionary of jurisdictions, or the dictionary of species and metrics
# species		If true, it lists the species and metric values
# padus			If true, it lists the padus categories dictionary
# jurisdiction	If true, it lists the jurisdictions' dictionary
getDictionary<-function(species=FALSE,padus=TRUE,jurisdiction=FALSE){
	dict<-list()
	conn<-odbcConnect("whadalo")
	if(species==TRUE){
		spdf<-data.frame()
		spcd<-c("bais","blra","bobo","buow","canv","cclo","feha","lbcu","lete","mago","mopl","nopi","rira","sacr","snpl","sppi","trbl","wifl")
		for(ss in spcd){
			sqlq<-paste("select distinct period, metric from",ss)
			tmp<-sqlQuery(conn,sqlq)
			tmp$Species<-ss
			spdf<-rbind(spdf,tmp[,c("Species","period","metric")])
		}
		dict$speciesDict<-spdf
	}
	if(padus==TRUE){
		sqlq<-"select * from paduscatslookup"
		padusdf<-sqlQuery(conn,sqlq)
		dict$padus<-padusdf
	}
	if(jurisdiction==TRUE){
		fws<-data.frame(Jurisdiction=rep("FWS",2),Value=c(6,8),Description=c("FWS Region 6 (Mountain-Prairie)","FWS Region 8 (Pacific Southwest)"))
		usfs<-data.frame(Jurisdiction=rep("USFS",5),Value=c(1,2,4,5,6),Description=c("USFS Region 1 (Northern)","USFS Region 2 (Rocky Mountain)","USFS Region 4 (Intermountain)","USFS Region 5 (Pacific Southwest)","USFS Region 6 (Pacific Northwest"))
		nps<-data.frame(Jurisdiction=rep("NPS",3),Value=c(1,2,3),Description=c("NPS Region Midwest","NPS Region Intermountain","NPS Region Pacific West"))
		lcc<-data.frame(Jurisdiction=rep("LCC",9),Value=c(2,3,4,5,6,7,11,13,15),Description=c("California LCC","Desert LCC","Eastern Tallgrass Prairie and Big Rivers LCC","Great Basin LCC","Great Northern LCC",
						"Great Plans LCC","North Pacific LCC","Plains and Prairie Potholes LCC","Southern Rockies LCC"))
		usjv<-data.frame(Jurisdiction=rep("USJV",11),Value=c(3,4,5,6,8,9,10,12,16,19,21),Description=c("Pacific Coast","Prairie Pothole","Upper Mississippi River/Great Lakes Region","Northern Great Plains",
						"Playa Lakes","Central Valley Habitat","Rainwater Basin","Sonoran","Intermountain West","Canadian Intermountain","San Francisco Bay"))
		bcr<-data.frame(Jurisdiction=rep("BCR",11),Value=c(5,9,10,11,15,16,17,18,19,32,33),Description=c("NORTHERN_PACIFIC_RAINFOREST","GREAT_BASIN","NORTHERN_ROCKIES","PRAIRIE_POTHOLES",
						"SIERRA_NEVADA","SOUTHERN_ROCKIES/COLORADO_PLATEAU","BADLANDS_AND_PRAIRIES","SHORTGRASS_PRAIRIE","CENTRAL_MIXED_GRASS_PRAIRIE","COASTAL_CALIFORNIA","SONORAN_AND_MOJAVE_DESERTS"))
		jurisdf<-rbind(fws,usfs);jurisdf<-rbind(jurisdf,nps);jurisdf<-rbind(jurisdf,lcc);jurisdf<-rbind(jurisdf,usjv);jurisdf<-rbind(jurisdf,bcr)
		dict$jurisdiction<-jurisdf
	}
	odbcClose(conn)
	return(dict)
}

## This function beautifies manager name
# mn is the manager whose name is being beautified
getManagerName<-function(mn){
	mgrNam<-ifelse(mn %in% c("ARS","DOE","NOAA","NRCS","OTHF"),"Fed - Other",
		ifelse(mn %in% c("BIA"),"Fed - BIA",
			ifelse(mn %in% c("SDNR","SDC","SDOL","SFW","SLB","SPR","OTHS"),"State",
				ifelse(mn=="BLM","Fed - BLM",
					ifelse(mn=="CITY","City",
						ifelse(mn=="CNTY","County",
							ifelse(mn=="DOD","Fed - DOD",
								ifelse(mn=="FWS","Fed - FWS",
									ifelse(mn=="JNT","Joint Mgmt",
										ifelse(mn %in% c("REG","RWD"),"Regional Agency",
											ifelse(mn=="NPS","Fed - NPS",
												ifelse(mn=="PVT","Private",
													ifelse(mn=="USACE","Fed - ACE",
														ifelse(mn=="USBR","Fed - BOR",
															ifelse(mn=="USFS","Fed - FS",
																ifelse(mn=="TRIB","Tribe",
																	ifelse(mn %in% c("UNK","UNKL"),"Unknown",
																		mn)))))))))))))))))
	return(mgrNam)
}

## This function saves the image to the html folder of the server
# pt is the plot object to save into an image
# imgName is the name to give the image. Must include .jpg
# wd and hg are image dimensions
# baseurl is the web URL where to find the saved image
printToURL<-function(pt,imgName,wd=480,hg=480,baseurl="http://ec2-18-144-7-236.us-west-1.compute.amazonaws.com/adalo/"){
	jpeg(filename = paste0("/var/www/html/adalo/",imgName),
			width = wd, height = hg, quality=100)
		print(pt)
	dev.off()
	urlr<-paste0(baseurl,imgName)
	return(urlr)
}

## This function fortifies the results table by an additional padusCat, optionally filters by a value in that newpadusCat, and optionally re-calculates relative abundance
# rdf is the results data.frame
# areaCat is the original padusCat used to query the data
# addCat is the name of the padusCat to add
# filterByCat is a string of values to filter by the new category
# recalcRelAbund is a boolean to determine if relative abundance should be re-calculated after the filter
fortifyFilterRes<-function(rdf,areaCat,addCat=NA,filterByCat=NA,recalcRelAbund=TRUE){
	if(!is.na(addCat)){
		## There may be a large number of areaCat values, so let's loop by segments of 50
		nseg<-ceiling(nrow(rdf)/50)
		conn<-odbcConnect("whadalo")
		startseg<-1
		fortable<-data.frame()
		for(ss in 1:nseg){
			if(ss*50 > nrow(rdf)){
				endseg<-nrow(rdf)
			}else{
				endseg<-ss*50
			}
			#need to escape all '
			acVals<-as.character(rdf$Area[startseg:endseg])
			acVals<-gsub("'","\\\\'",acVals)
			acstr<-paste0("'",paste(acVals,collapse="','"),"'")
			sqlq<-paste0("select ",areaCat,", ",addCat," from paduscats where ",areaCat," in (",acstr,")")
			ftbl<-sqlQuery(conn,sqlq)
			names(ftbl)<-c("Area","newPCat")
			fortable<-rbind(fortable,ftbl)
			startseg<-endseg+1
		}
		odbcClose(conn)
		rdf$Area<-as.character(rdf$Area)
		fortable$Area<-as.character(fortable$Area)
		fdf<-merge(rdf,fortable,by="Area",all.x=T)
		fdf$newPCat<-ifelse(is.na(fdf$newPCat),"UNKN",as.character(fdf$newPCat))
		fdf<-unique(fdf)
		if(!is.na(filterByCat[1])){ #if there are strings to filter by...
			fdf<-subset(fdf,newPCat %in% filterByCat)
			if(recalcRelAbund){
				tabund<-sum(fdf$wgtAbundance)
				fdf$relAbundance<-fdf$wgtAbundance/tabund
			}
		}
		names(fdf)<-gsub("newPCat",addCat,names(fdf))
		return(fdf)
	}else{
		return(rdf)
	}
}

## This function makes a Pareto chart from the data
# df is the data.frame with the data to plot, presumably from a call to makeQuestion
# yvar is the dependent variable: wgtDensity, relAbundance (default), or Area (Density Index or % Total Abundance Index or % Total Area)
# barsOnly default FALSE, but if TRUE, then the barchart only 
# xlabel to provide a beautified x-label (defaults to "PAD-US Category Levels", but should be something like "PAD-US Land Manager")
# fillColor default bar fill color (set default from colorBrewer)
## customizing the plot...
# transposePlot defaults to TRUE to transpose the plot
# addYVals defaults to TRUE to add the geom_text of the value on top of each bar
# addMetricName is either the metric value, which is then added as text to the plot, or NA to omit
# highCat the category value for bar to highlightCat, or NA
# highColor the color to use for highlight (set default from colorBrewer), or NA
# paretoCol to change the color of the pareto line and points, defaults to black
makePareto<-function(df, xvar, yvar="relAbundance",barsOnly=FALSE, xLabel="PAD-US Category Levels",transposePlot=TRUE,
		fillColor="#0571b0",addYVals=TRUE,addMetric=NA,highCat=NA,highColor="#ca0020",paretoColor="black"){
	
	ylabel<-ifelse(yvar=="relAbundance","% Total Abundance Index",
			ifelse(yvar=="wgtDensity","Density Index","% Total Area"))
	
	if(barsOnly){	#no pareto, only bar plot
		if(!is.na(highCat)){	#highlighting one category
			df$barColor<-ifelse(df[,xvar]==highCat,highColor,fillColor)
			parplot<-ggplot(df, aes(x=xvar, y=yvar)) + geom_bar(fill = barColor, stat="identity") + labs(x=xlabel,y=ylabel) + theme_bw() +
					scale_fill_manual(values=colorsHighlight, guide=FALSE)
		}else{	#no highlights
			parplot<-ggplot(df, aes(x=xvar, y=yvar)) + geom_bar(fill = fillColor, stat="identity") + labs(x=xlabel,y=ylabel) + theme_bw()
		}
		
	}else{ # Pareto plot...
		if(!is.na(highCat)){	#highlighting one category
			df$barColor<-ifelse(df[,xvar]==highCat,highColor,fillColor)
			parplot<-ggplot(df, aes(x=xvar, y=yvar)) + 
					stat_pareto(point.color = paretoColor,point.size = 2, line.color = "black", bars.fill = barColor) + 
					labs(x=xlabel,y=ylabel) + theme_bw() + scale_fill_manual(values=colorsHighlight, guide=FALSE)
		}else{	#no highlights
			parplot<-ggplot(df, aes(x=xvar, y=yvar)) + 
					stat_pareto(point.color = paretoColor,point.size = 2, line.color = "black", bars.fill = fillColor) + 
					labs(x=xlabel,y=ylabel) + theme_bw()
		}
		
	}
	
	## Beautifying...
	if(transposePlot){
		parplot<-parplot + coord_flip()
	}else{
		parplot<-parplot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
	}
	
	if(addYVals){
		parplot<-parplot  + geom_text(aes(label = y, y = y + max(res[,yvar])*0.05),position = position_dodge(0.9),vjust = 0,size=2.8)
	}
	
	if(!is.na(addMetricName)){
		parplot<-parplot + annotate("text", x = 1, y = max(res[,yvar])*0.9, label = addMetricName,size=5)
	}
	
	return(parplot)
	
					
}


