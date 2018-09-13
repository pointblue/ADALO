# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(reshape)
library(plyr)
pthres<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Report/PlotsTables/"
scaledmetric<-function(x,metric){
	tm<-sum(abs(x[,metric]))
	if(tm==0){
		nv<-paste("scaled_",metric,sep="")
		x[,nv]<-0
	}else{
		ts<-ifelse(x[,metric]<0,-1,1)
		nv<-paste("scaled_",metric,sep="")
		x[,nv]<-abs(x[,metric])/tm;x[,nv]<-x[,nv]*ts
	}
	return(x)
}

#need a function to calculate the frequency of cells positive per refuge
#do this with aggregate!!
isCellPositive<-function(df){
	cp<-ifelse(sum(df$obsCount,na.rm=TRUE)>0,1,0)
	ret<-unique(df[,c("pointId","SpeciesCode","season","orgName")])
	ret$cellPositive<-cp
	return(ret)
}
calcCellFreq<-function(df){
	freqp<-round(sum(df$cellPositive)/nrow(df),4)
	frdf<-unique(df[,c("SpeciesCode","season","orgName")])
	frdf$freqCellPos<-freqp
	return(frdf)
}
isEventPositive<-function(df){
	cp<-ifelse(sum(df$obsCount,na.rm=TRUE)>0,1,0)
	ret<-unique(df[,c("obsDate","SpeciesCode","season","orgName")])
	ret$eventPositive<-cp
	return(ret)
}
calcEventFreq<-function(df){
	freqp<-round(sum(df$eventPositive)/nrow(df),4)
	frdf<-unique(df[,c("SpeciesCode","season","orgName")])
	frdf$freqEventPos<-freqp
	return(frdf)
}

#load the data
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/Data/NewSet/collated.RData")
collated.df$year<-as.integer(format(collated.df$obsDate,"%Y"))
collated.df<-subset(collated.df,year>2004)

##Encompassing version
dd<-subset(collated.df,!is.na(orgName))
dd$refuge<-ifelse(dd$fwsStatus %in% c(1,2),"N","Y") #has to have orgName, but not status 1 or 2
dde<-subset(dd,refuge=="Y")
cellp.df<-aggregate(as.formula("obsCount~pointId+SpeciesCode+season+orgName"),data=dde,FUN=sum)
cellp.df$present<-ifelse(cellp.df$obsCount>0,1,0)
freqcp.df<-aggregate(as.formula("present~SpeciesCode+season+orgName"),data=cellp.df,FUN=mean)
names(freqcp.df)<-c("SpeciesCode","season","orgName","freqCellPos")
scaledFreq.df<-ddply(.data=freqcp.df, .variables=c("SpeciesCode","season"), .fun=scaledmetric, metric="freqCellPos")
		
#cellp.df<-ddply(.data=dd[,c("pointId","obsCount","SpeciesCode","season","orgName")], .variables=c("pointId","SpeciesCode","season","orgName"), .fun=isCellPositive)
#freqcp.df<-ddply(.data=cellp.df, .variables=c("SpeciesCode","season","orgName"), .fun=calcCellFreq)
#scaledFreq.df<-ddply(.data=freqcp.df, .variables=c("SpeciesCode","season"), .fun=scaledmetric, metric="freqCellPos")
#then cast:
t1<-cast(data=subset(scaledFreq.df, season=="B"),SpeciesCode ~ orgName, value="scaled_freqCellPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqCellPos_approved_B.csv"))

t1<-cast(data=subset(scaledFreq.df, season=="W"),SpeciesCode ~ orgName, value="scaled_freqCellPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqCellPos_approved_W.csv"))


#need a function to tabulate the number of events positive per refuge
eventp.df<-aggregate(as.formula("obsCount~obsDate+SpeciesCode+season+orgName"),data=dde,FUN=sum)
eventp.df$present<-ifelse(eventp.df$obsCount>0,1,0)
freqep.df<-aggregate(as.formula("present~SpeciesCode+season+orgName"),data=eventp.df,FUN=mean)
names(freqep.df)<-c("SpeciesCode","season","orgName","freqEventPos")
scaledFreq.df<-ddply(.data=freqep.df, .variables=c("SpeciesCode","season"), .fun=scaledmetric, metric="freqEventPos")

#eventp.df<-ddply(.data=dd[,c("obsDate","obsCount","SpeciesCode","season","orgName")], .variables=c("obsDate","SpeciesCode","season","orgName"), .fun=isEventPositive)
#freqep.df<-ddply(.data=eventp.df, .variables=c("SpeciesCode","season","orgName"), .fun=calcEventFreq)
#scaledFreq.df<-ddply(.data=freqep.df, .variables=c("SpeciesCode","season"), .fun=scaledmetric, metric="freqEventPos")
#then cast:
t1<-cast(data=subset(scaledFreq.df, season=="B"),SpeciesCode ~ orgName, value="scaled_freqEventPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqEventPos_approved_B.csv"))

t1<-cast(data=subset(scaledFreq.df, season=="W"),SpeciesCode ~ orgName, value="scaled_freqEventPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqEventPos_approved_W.csv"))

#Narrow version - remove any orgName from dd where...
dd$refuge<-ifelse(dd$fwsRSLType %in% c("NWR","WMA"),ifelse(collated.df$fwsStatus==0,"Y","N"),"N")
ddn<-subset(dd,refuge=="Y")

#cells
cellp.df<-aggregate(as.formula("obsCount~pointId+SpeciesCode+season+orgName"),data=ddn,FUN=sum)
cellp.df$present<-ifelse(cellp.df$obsCount>0,1,0)
freqcp.df<-aggregate(as.formula("present~SpeciesCode+season+orgName"),data=cellp.df,FUN=mean)
names(freqcp.df)<-c("SpeciesCode","season","orgName","freqCellPos")
scaledFreq.df<-ddply(.data=freqcp.df, .variables=c("SpeciesCode","season"), .fun=scaledmetric, metric="freqCellPos")
t1<-cast(data=subset(scaledFreq.df, season=="B"),SpeciesCode ~ orgName, value="scaled_freqCellPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqCellPos_interest_B.csv"))

t1<-cast(data=subset(scaledFreq.df, season=="W"),SpeciesCode ~ orgName, value="scaled_freqCellPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqCellPos_interest_W.csv"))

#events
eventp.df<-aggregate(as.formula("obsCount~obsDate+SpeciesCode+season+orgName"),data=ddn,FUN=sum)
eventp.df$present<-ifelse(eventp.df$obsCount>0,1,0)
freqep.df<-aggregate(as.formula("present~SpeciesCode+season+orgName"),data=eventp.df,FUN=mean)
names(freqep.df)<-c("SpeciesCode","season","orgName","freqEventPos")
scaledFreq.df<-ddply(.data=freqep.df, .variables=c("SpeciesCode","season"), .fun=scaledmetric, metric="freqEventPos")
t1<-cast(data=subset(scaledFreq.df, season=="B"),SpeciesCode ~ orgName, value="scaled_freqEventPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqEventPos_interest_B.csv"))

t1<-cast(data=subset(scaledFreq.df, season=="W"),SpeciesCode ~ orgName, value="scaled_freqEventPos")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"Refuge_FreqEventPos_interest_W.csv"))
