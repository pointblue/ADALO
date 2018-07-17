# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(ggplot2)
library(raster)

padus<-stack("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/padus/padus990.grd")

#Application 1: Prioritizing species for conservation at three scales: refuge, region or multi-region.
spcd<-c("CCLO","BAIS","BOBO","SACR","TRBL","CANV")
p4<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m4data/"
p5<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5data/"

#need to convert the factor codes in padus categories to text
#using the .dbf file associate with the shapefile in the PADUS data file (see rasterizePADUS.R)
#then add to the metric 5 raster to summarize
paduscats<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/paduscats.csv")
q<-unique(paduscats$Own_Name);owndf<-data.frame(OwnerName=c(unique(paduscats$Own_Name),99),Owner=c(as.character(q),"Private"))
q<-unique(paduscats$Mang_Type);mgmtdf<-data.frame(MgmtType=c(unique(paduscats$Mang_Type),99),Manager=c(as.character(q),"Private"))
q<-unique(paduscats$Loc_Nm);locdf<-data.frame(LocName=c(unique(paduscats$Loc_Nm),999999),Location=c(as.character(q),"Private"))
rm5b<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5raster/CANV_breeding_filtered.tif") #Use CCLO
rm5w<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/m5raster/CANV_winter_filtered.tif")
stkb<-stack(rm5b,padus);stkbdf<-as.data.frame(stkb)
stkbdf$OwnerName<-ifelse(is.na(stkbdf$OwnerName),99,stkbdf$OwnerName)
stkbdf$LocName<-ifelse(is.na(stkbdf$LocName),999999,stkbdf$LocName)
stkbdf$MgmtType<-ifelse(is.na(stkbdf$MgmtType),99,stkbdf$MgmtType)
stkw<-stack(rm5w,padus);stkwdf<-as.data.frame(stkw)
stkwdf$OwnerName<-ifelse(is.na(stkwdf$OwnerName),99,stkwdf$OwnerName)
stkwdf$LocName<-ifelse(is.na(stkwdf$LocName),999999,stkwdf$LocName)
stkwdf$MgmtType<-ifelse(is.na(stkwdf$MgmtType),99,stkwdf$MgmtType)


# Refuge Scale Question: I am a refuge. What species should I prioritize for conservation action and monitoring?
#using CCLO...
#-Compare M4 v M5 mean encounter rate for selected species for a given refuge (breeding vs. wintering)
load(paste(p4,"CANV_breeding.RData",sep=""));m4db<-merge(m4data,locdf,by="LocName",all.x=T)
m4db<-m4db[,c("Location","encounterYRate","LocName")];m4db$Season="Breeding"
load(paste(p4,"CANV_winter.RData",sep=""));m4dw<-merge(m4data,locdf,by="LocName",all.x=T)
m4dw<-m4dw[,c("Location","encounterYRate","LocName")];m4dw$Season="Winter"
m4d<-rbind(m4db,m4dw);m4d$Species<-"Canvasback";m4d$Metric<-"Metric 4"
dat4<-aggregate(as.formula("encounterYRate~Location+LocName+Season+Species+Metric"),data=m4d,FUN=mean)

#"Arrowwood Elementary", "Aurora County Waterfowl Production Area 119", "Buffalo Gap","Cheney Stadium",
#"Cimarron Mitigation Area","Crow Creek Pocket Park","Devils Island Area","J. Clark Salyer National Wildlife Refuge"
#"Medicine Lake National Wildlife Refuge 2"
selloc<-"J. Clark Salyer National Wildlife Refuge"
df4<-subset(dat4,Location==selloc)
locn<-unique(df4$LocName)

m5b<-subset(stkbdf,LocName==locn)
if(nrow(m5b)>0){
	m5b$Season<-"Breeding";names(m5b)<-gsub("CANV_breeding_filtered","encounterYRate",names(m5b))
}
m5w<-subset(stkwdf,LocName==locn)
if(nrow(m5w)>0){
	m5w$Season<-"Winter";names(m5w)<-gsub("CANV_winter_filtered","encounterYRate",names(m5w))
}
m5df<-rbind(m5b,m5w)
if(nrow(m5df)>0){
	m5df<-merge(m5df,locdf,by="LocName",all.x=T)
	m5df$Species<-"Canvasback";m5df$Metric<-"Metric 5"
	df5<-aggregate(as.formula("encounterYRate~Location+LocName+Season+Species+Metric"),data=m5df,FUN=mean)
}else{
	df5<-data.frame()
}

plot.df<-rbind(df4,df5);plot.df$sm<-paste(plot.df$Season,plot.df$Metric,sep="-")
p<-ggplot(plot.df,aes(x=Metric,y=encounterYRate)) + geom_col(position="dodge",aes(fill=sm)) + 
		labs(y="Index of Abundance",fill="Season & metric",title=paste("CANV - ",selloc,sep=""))


# Regional Scale Question: I am a regional program (e.g., Endangered Species or Refuge I&M program). What species should I prioritize for conservation action and monitoring?
#-Compare M4 mean encounter rate for selected species by land owner by region (breeding vs. wintering)
m4db<-merge(m4data,owndf,by="OwnerName",all.x=T)
m4db<-m4db[,c("Owner","encounterYRate","OwnerName")];m4db$Season="Breeding"
m4dw<-merge(m4data,owndf,by="OwnerName",all.x=T)
m4dw<-m4dw[,c("Owner","encounterYRate","OwnerName")];m4dw$Season="Winter"
m4d<-rbind(m4db,m4dw);m4d$Species<-"Canvasback";m4d$Metric<-"Metric 4"
dat4<-aggregate(as.formula("encounterYRate~Owner+OwnerName+Season+Species+Metric"),data=m4d,FUN=sum)
ownn<-unique(dat4$OwnerName)
#-Compare M5 mean encounter rate for selected species by land owner by region (breeding vs wintering)
m5b<-subset(stkbdf,OwnerName %in% ownn)
m5b$Season<-"Breeding";names(m5b)<-gsub("CANV_breeding_filtered","encounterYRate",names(m5b))
m5w<-subset(stkwdf,OwnerName %in% ownn)
m5w$Season<-"Winter";names(m5w)<-gsub("CANV_winter_filtered","encounterYRate",names(m5w))
m5df<-rbind(m5b,m5w);m5df<-merge(m5df,owndf,by="OwnerName",all.x=T)
m5df$Species<-"Canvasback";m5df$Metric<-"Metric 5"
dat5<-aggregate(as.formula("encounterYRate~Owner+OwnerName+Season+Species+Metric"),data=m5df,FUN=sum)
dat4<-subset(dat4,OwnerName %in% dat5$OwnerName)

plot.df<-rbind(dat4,dat5);plot.df$sm<-paste(plot.df$Season,plot.df$Metric,sep="-")
p<-ggplot(plot.df,aes(x=Owner,y=encounterYRate)) + geom_col(position="dodge",aes(fill=sm)) + 
		coord_flip() + facet_wrap(~Metric,ncol=2,scale="free") +
		labs(x="Landowner Category",y="Index of Total Abundance",fill="Season & metric",title="CANV - Abundance by landowner category")

# Multi-regional Scale Question: I am at the USFWS national office. What species should I prioritize for conservation action and monitoring in the west vs. the east?
m4db<-merge(m4data,mgmtdf,by="MgmtType",all.x=T)
m4db<-m4db[,c("Manager","encounterYRate","MgmtType")];m4db$Season="Breeding"
m4dw<-merge(m4data,mgmtdf,by="MgmtType",all.x=T)
m4dw<-m4dw[,c("Manager","encounterYRate","MgmtType")];m4dw$Season="Winter"
m4d<-rbind(m4db,m4dw);m4d$Species<-"Canvasback";m4d$Metric<-"Metric 4"
dat4<-aggregate(as.formula("encounterYRate~Manager+MgmtType+Season+Species+Metric"),data=m4d,FUN=sum)
mgtt<-unique(dat4$MgmtType)
#-Compare M5 mean encounter rate for selected species by land owner by region (breeding vs wintering)
m5b<-subset(stkbdf,MgmtType %in% mgtt)
m5b$Season<-"breeding";names(m5b)<-gsub("CANV_breeding_filtered","encounterYRate",names(m5b))
m5w<-subset(stkwdf,MgmtType %in% mgtt)
m5w$Season<-"winter";names(m5w)<-gsub("CANV_winter_filtered","encounterYRate",names(m5w))
m5df<-rbind(m5b,m5w);m5df<-merge(m5df,mgmtdf,by="MgmtType",all.x=T)
m5df$Species<-"Canvasback";m5df$Metric<-"Metric 5"
dat5<-aggregate(as.formula("encounterYRate~Manager+MgmtType+Season+Species+Metric"),data=m5df,FUN=sum)
dat4<-subset(dat4,MgmtType %in% dat5$MgmtType)

plot.df<-rbind(dat4,dat5);plot.df$sm<-paste(plot.df$Season,plot.df$Metric,sep="-")
p<-ggplot(plot.df,aes(x=Manager,y=encounterYRate)) + geom_col(position="dodge",aes(fill=sm)) + 
		coord_flip() + facet_wrap(~Metric,ncol=2,scale="free") +
		labs(x="Manager Category",y="Index of Total Abundance",fill="Season & metric",title="CANV - Abundance by management agency")


# Application 2: Prioritizing lands for purchase or easement at the regional scale
# Question: I am deciding where to allocate LWCF funds to acquire lands for conservation. Which lands will contribute the most to my priority species during different seasons?
#-Identify areas with the highest M4/M5 encounter rates (top 20%?) for selected species that are within approved acquisition boundaries (within acquisition boundaries but not yet acquired) by region and by season; note: PAD-US does not provide USFWS approved acquisition boundaries, it only provides us with the land we already own in fee title. We would need to get approved acquisition boundary areas from the refuge web service and subtract the lands already acquired by USFWS to get the private land that can still be acquired.

