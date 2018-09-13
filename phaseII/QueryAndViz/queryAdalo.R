# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(RODBC); library(ggplot2); library(plyr)

###########################################################################################
##################  Make a question for the ADALO tool  ###################################
###########################################################################################

## OVERVIEW:
## There are two types of questions to ask - by species or by region: 
# 1) BY SPECIES - We want to know how two or more species compare within a PADUS category's domain (e.g., within a refuge), 
#	(think of a table where the columns are refuges and the rows are species, each cell reporting the abundance of the species)
#	Example question: which species should be targeted for management in refuge X?
# 2) BY REGION - We want to know how one or more species compare across domains options 
#	(in the hypothetical table, the columns are one or more species and the rows are the spatial domains). 
#	The domain comparisons come in three options"
#		2a) Compare species between two or more members of a PADUS category
#			The following categories exist: mgmtType, mgrName, desType, unitName 
#			(example mgmtType=FED, mgrName=FWS, desType=National Widlife Refuge, unitName=San Pablo Bay National Wildlife Refuge)
#			Example questions: compare species X between FWS and NPS, or between two refuges, or between mgmtType=FED and all others
#		2b) Compare species between a PAUDS Unit and a geopolitical region 
#			The following geopolitical regions exist: FWSregion, NPSregion, USFSregion, LCCregion, USJVregion, BCR, State, and County.
#			Example questions: How important is SPBNWR for CANV with relation to the entire SFBJV or the entire BCR or Region 8?
#		2c) Compare species between geopolitical regions
#			Example question: Which agency (FWS or DOD) is relatively more important (i.e. manages more individuals) for TRBL?

########################################################################
## Questions --

# Q1: compare this refuge vs that refuge on importance for BAIS using metric 4
# sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from baseIntersects as t1 left join (select * from bais where metric=4) as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId)",
#		"where t1.padusObjId in",
#		"(select t3.padusObjId from padusCats as t3 where t3.unitName in",
#		"('BENTON LAKE NATIONAL WILDLIFE REFUGE','BOWDOIN NATIONAL WILDLIFE REFUGE'))")
#conn<-odbcConnect("whadalo")
#df<-sqlQuery(conn,sqlq)
#odbcClose(conn)

# Q1: Compare two regions (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(by="region",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge','Sacramento River National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$tblabund; res$tblsurv
res$pltabun; dev.new();res$pltsurv

# Q2: Compare 4 species of birds in relation to use of some regions (Modoc and Valentine refuges)
res<-makeQuestion(by="species",metric=4,period=NA,species=c("buow","lbcu","nopi","trbl"),padusCat="unitName",catValues=c('Valentine National Wildlife Refuge','Modoc National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$tblabund; res$tblsurv
res$pltabun; dev.new();res$pltsurv

# Q3: compare this refuge vs a jurisdictional domain - San Pablo Bay vs the rest in Region 8
res<-makeQuestion(by="region",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)
res$tbldens; res$pltdens

# Q4: compare jurisdictions (R6 vs R8) for a species (canv in winter, using metric 5)
res<-makeQuestion(by="region",metric=5,period=0,species="canv",padusCat=NA,catValues=NA,geopolCat="USFWSregion",geopolValues=c(6,8))
res$tbldens; res$pltdens
res$tblabund; res$pltabun





##################
##  IGNORE THIS
#faster this way
conn<-odbcConnect("whadalo")
poid<-"select unitName,padusObjId from padusCats where unitName in ('San Pablo Bay National Wildlife Refuge','Sacramento River National Wildlife Refuge')"
doidf<-sqlQuery(conn,poid)

df4<-getValuesByMetric(spcd="canv",metricVal=4,doidf=doidf,conn=conn,filtPeriod=0)
df5<-getValuesByMetric(spcd="canv",metricVal=5,doidf=doidf,conn=conn,filtPeriod=0)

cont4<-makeContrast(by="Region",domdf=df4)

resdf<-rbind(df4,df5)
resdf$Source<-ifelse(resdf$metric==4,"Empirical",ifelse(resdf$metric==5,"Model_ADALO","Model_HAPET"))

plotdf<-reshape(resdf[,c("Region","estDensity","estAbundance","percAreaSurveyed","Source")],
		idvar=c("Region","percAreaSurveyed","Source"),varying=c("estDensity", "estAbundance"),v.names="Value",
		timevar="Parameter",times=c("estDensity", "estAbundance"),direction="long")
row.names(plotdf)<-NULL

## SKIP Density!!
p<-ggplot(data=plotdf,aes(x=Region,y=Value)) + geom_bar(stat="identity",aes(fill=Source),position="dodge") + 
		facet_wrap(~Parameter,scales="free",ncol=1) + coord_flip() + labs(x="",y="")

p<-ggplot(data=resdf,aes(x=Region,y=estAbundance)) + geom_bar(stat="identity",width=0.5,fill="navy blue") + facet_wrap(~Source,scales="free_x") + 
		coord_flip() + labs(x="",y="Abundance Index") + theme_bw()

# making the table in the ADALO Qeustions document...
poid<-"select unitName,padusObjId from padusCats where unitName like ('%Valentine National Wildlife Refuge%') or unitName like('%Modoc National Wildlife Refuge%')"
doidf<-sqlQuery(conn,poid)
doidf<-aggManyNames(doidf,mnames=c("Valentine National Wildlife Refuge","Modoc National Wildlife Refuge"))
df4<-getValuesByMetric(spcd="buow",metricVal=4,doidf=doidf,conn=conn)
df5<-getValuesByMetric(spcd="buow",metricVal=7,doidf=doidf,conn=conn)
buowdf<-rbind(df4,df5)
buowdf$Species<-"Burrowing Owl"
df4<-getValuesByMetric(spcd="lbcu",metricVal=4,doidf=doidf,conn=conn)
df5<-getValuesByMetric(spcd="lbcu",metricVal=5,doidf=doidf,conn=conn)
lbcudf<-rbind(df4,df5)
lbcudf$Species<-"Long-billed Curlew"
df4<-getValuesByMetric(spcd="nopi",metricVal=4,doidf=doidf,conn=conn)
df5<-getValuesByMetric(spcd="nopi",metricVal=5,doidf=doidf,conn=conn)
nopidf<-rbind(df4,df5)
nopidf$Species<-"Northern Pintail"
df4<-getValuesByMetric(spcd="trbl",metricVal=4,doidf=doidf,conn=conn)
df5<-getValuesByMetric(spcd="trbl",metricVal=5,doidf=doidf,conn=conn)
trbldf<-rbind(df4,df5)
trbldf$Species<-"Tri-colored Blackbird"

resdf<-rbind(buowdf,lbcudf);resdf<-rbind(resdf,nopidf);resdf<-rbind(resdf,trbldf)

refset<-data.frame(Region=c(rep("Modoc National Wildlife Refuge",8),rep("Valentine National Wildlife Refuge",8)),metric=rep(c(4,4,4,4,7,5,5,5),2),
		Species=rep(c("Burrowing Owl","Long-billed Curlew","Northern Pintail","Tri-colored Blackbird"),4))
resdf<-merge(refset,resdf,by=c("Region","metric","Species"),all.x=T)
resdf<-resdf[,c("Region","metric","Species","estAbundance","percAreaSurveyed")]
resdf$Source<-ifelse(resdf$metric==4,"Empirical",ifelse(resdf$metric==5,"Model_ADALO",ifelse(resdf$metric==6,"Model_HAPET","Model_ECN")))
resemp<-subset(resdf,metric==4,select=c("Region","Species","estAbundance","percAreaSurveyed"))
names(resemp)<-gsub("estAbundance","Empirical_Index",names(resemp))
resmod<-subset(resdf,metric>4,select=c("Region","Species","estAbundance"))
names(resmod)<-gsub("estAbundance","Model_Index",names(resmod))
relImpdf<-merge(resemp,resmod,by=c("Region","Species"))
relImpdf$Empirical_Index<-ifelse(is.na(relImpdf$Empirical_Index),0,relImpdf$Empirical_Index)
relImpdf$percAreaSurveyed<-ifelse(is.na(relImpdf$percAreaSurveyed),0,relImpdf$percAreaSurveyed)
relImpdf<-relImpdf[order(relImpdf$Species),]
#finally, make model values where none found...
relImpdf<-subset(relImpdf,Empirical_Index>0)

###################################################################################
# Q2: compare this refuge vs the rest in R8 - San Pablo Bay vs the rest in Region 8
poida<-"select unitName,padusObjId from padusCats where unitName = 'San Pablo Bay National Wildlife Refuge'"
doidfa<-sqlQuery(conn,poida)
df4a<-getValuesByMetric(spcd="canv",metricVal=4,doidf=doidfa,conn=conn,filtPeriod=0)
df5a<-getValuesByMetric(spcd="canv",metricVal=5,doidf=doidfa,conn=conn,filtPeriod=0)
resdfa<-rbind(df4a,df5a)

df4b<-getValuesByMetric(spcd="canv",metricVal=4,conn=conn,doidf=NA,filtPeriod=0,geopolField="USFWSregion",geopolValue=8)
df5b<-getValuesByMetric(spcd="canv",metricVal=5,conn=conn,doidf=NA,filtPeriod=0,geopolField="USFWSregion",geopolValue=8)
resdfb<-rbind(df4b,df5b)

resdf<-rbind(resdfa,resdfb)
resdf$Source<-ifelse(resdf$metric==4,"Empirical",ifelse(resdf$metric==5,"Model_ADALO","Model_HAPET"))

p<-ggplot(data=resdf,aes(x=Region,y=relImportance)) + geom_bar(stat="identity",width=0.5,fill="navy blue") + facet_wrap(~Source,scales="free_x") + 
		coord_flip() + labs(x="",y="Density Index") + theme_bw()

odbcClose(conn)


