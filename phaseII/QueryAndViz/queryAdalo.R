# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## Build a question in terms of the numcells in a subset of intersects
## Make a connection
## Query baseIntersects right-join the species (or just the table and then loop through the species tables to aggregate these, if for multiple species)
## Get the total number of cells, and the celMetric sums by levels of. or contrasts between, landowners -> calculate the relative importance
## Plot

library(RODBC); library(ggplot2)

# Q1: compare this refuge vs that refuge on importance for BAIS using metric 4
sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from baseIntersects as t1 left join (select * from bais where metric=4) as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId)",
		"where t1.padusObjId in",
		"(select t3.padusObjId from padusCats as t3 where t3.unitName in",
		"('BENTON LAKE NATIONAL WILDLIFE REFUGE','BOWDOIN NATIONAL WILDLIFE REFUGE'))")
conn<-odbcConnect("whadalo")
df<-sqlQuery(conn,sqlq)
odbcClose(conn)

#faster this way
conn<-odbcConnect("whadalo")
poid<-"select unitName,padusObjId from padusCats where unitName in ('BENTON LAKE NATIONAL WILDLIFE REFUGE','BOWDOIN NATIONAL WILDLIFE REFUGE')"
poidf<-sqlQuery(conn,poid)
poids<-paste(poidf$padusObjId,collapse=",")

sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from baseIntersects as t1 left join (select * from bais where metric=4) as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId) ",
		"where t1.padusObjId in (",poids,")",sep="")
df<-sqlQuery(conn,sqlq)
df$cellMetric<-ifelse(is.na(df$cellMetric),0,df$cellMetric)
df<-merge(df,poidf,by="padusObjId",all.x=T)

tcells<-aggregate(ncells~unitName,df,sum)
resdf<-aggregate(cellMetric~unitName,df,sum)
resdf<-merge(resdf,tcells,by="unitName")
resdf$relImportance<-resdf$cellMetric/resdf$ncells
resdf$metric<-4


sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from baseIntersects as t1 left join (select * from bais where metric=5) as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId) ",
		"where t1.padusObjId in (",poids,")",sep="")
df<-sqlQuery(conn,sqlq)
df$cellMetric<-ifelse(is.na(df$cellMetric),0,df$cellMetric)
df<-merge(df,poidf,by="padusObjId",all.x=T)

tcells<-aggregate(ncells~unitName,df,sum)
tdf<-aggregate(cellMetric~unitName,df,sum)
tdf<-merge(tdf,tcells,by="unitName")
tdf$relImportance<-tdf$cellMetric/tdf$ncells
tdf$metric<-5
resdf<-rbind(resdf,tdf)

sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from baseIntersects as t1 left join (select * from bais where metric=6) as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId) ",
		"where t1.padusObjId in (",poids,")",sep="")
df<-sqlQuery(conn,sqlq)
df$cellMetric<-ifelse(is.na(df$cellMetric),0,df$cellMetric)
df<-merge(df,poidf,by="padusObjId",all.x=T)

tcells<-aggregate(ncells~unitName,df,sum)
tdf<-aggregate(cellMetric~unitName,df,sum)
tdf<-merge(tdf,tcells,by="unitName")
tdf$relImportance<-tdf$cellMetric/tdf$ncells
tdf$metric<-6
resdf<-rbind(resdf,tdf)
resdf$Source<-ifelse(resdf$metric==4,"Empirical",ifelse(resdf$metric==5,"ADALO","HAPET"))

p<-ggplot(data=resdf,aes(x=unitName,y=relImportance)) + geom_bar(stat="identity",width=0.5,fill="navy blue") + facet_wrap(~Source,scales="free_x") + 
		coord_flip() + labs(x="",y="Relative Importance Index") + theme_bw()

## if metric 4 uses only cells with data - USE THIS APPROACH
sqlq<-paste("select t1.intId, t1.padusObjId, t1.ncells, t2.cellMetric from baseIntersects as t1 left join (select * from bais where metric=4) as t2 on (t1.intId = t2.intId and t1.padusObjId = t2.padusObjId) ",
		"where t1.padusObjId in (",poids,")",sep="")
df<-sqlQuery(conn,sqlq)
df<-subset(df,!is.na(df$cellMetric))
df<-merge(df,poidf,by="padusObjId",all.x=T)

tcells<-aggregate(ncells~unitName,df,sum)
resdf<-aggregate(cellMetric~unitName,df,sum)
resdf<-merge(resdf,tcells,by="unitName")
resdf$relImportance<-resdf$cellMetric/resdf$ncells
resdf$metric<-4

