# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(ggplot2)
library(plyr)
library(reshape)

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

diffmetric<-function(x,contrast){ #split by season and Species
	x1<-subset(x,scope=="narrow");x2<-subset(x,scope=="encomp")
	resdf<-x1[,c(contrast,"season","Species","scope")]
	resdf$diffState<-"approv_minus_interst"
	resdf$diffAvgRate<-x2$AvgRate-x1$AvgRate
	resdf$diffprobPresent<-x1$probPresent-x2$probPresent
	resdf$diffPosEventFreq<-x1$posEventFreq-x2$posEventFreq
	resdf$diffPosCellFreq<-x1$posCellFreq-x2$posCellFreq
	return(resdf)
}


## read the data from each of the nb files
agNb<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Narrow/nbAgency_B.csv");agNb$season<-"B";agNb$scope<-"narrow"
agEb<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Encompassing/nbAgency_B.csv");agEb$season<-"B";agEb$scope<-"encomp"
agNw<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Narrow/nbAgency_W.csv");agNw$season<-"W";agNw$scope<-"narrow"
agEw<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Encompassing/nbAgency_W.csv");agEw$season<-"W";agEw$scope<-"encomp"
agdf<-rbind(agNb,agEb,agNw,agEw);agdf$agency<-ifelse(agdf$agency=="OtherPublic","Public",as.character(agdf$agency))
agdf<-ddply(.data=agdf,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="AvgRate")
agdf<-ddply(.data=agdf,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="probPresent")
agdf$Scope<-paste("Refuge definition:",ifelse(agdf$scope=="narrow","interests","approved"))
agdf$Scope2<-ifelse(agdf$scope=="narrow","Interests","Approved")
agdf$seas<-ifelse(agdf$season=="B","Breeding","Wintering")

reNb<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Narrow/nbRefuge_B.csv");reNb$season<-"B";reNb$scope<-"narrow"
reEb<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Encompassing/nbRefuge_B.csv");reEb$season<-"B";reEb$scope<-"encomp"
reNw<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Narrow/nbRefuge_W.csv");reNw$season<-"W";reNw$scope<-"narrow"
reEw<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Encompassing/nbRefuge_W.csv");reEw$season<-"W";reEw$scope<-"encomp"
redf<-rbind(reNb,reEb,reNw,reEw)
redf<-ddply(.data=redf,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="AvgRate")
redf<-ddply(.data=redf,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="probPresent")
redf$Scope<-paste("Refuge definition:",ifelse(redf$scope=="narrow","interests","approved"))
redf$Scope2<-ifelse(redf$scope=="narrow","Interests","Approved")
redf$seas<-ifelse(redf$season=="B","Breeding","Wintering")
redf$scope_season<-paste(redf$Scope2,redf$seas,sep=" - ")
redf$sc<-ifelse(redf$scope_season=="Approved - Breeding","AB",
		ifelse(redf$scope_season=="Approved - Wintering","AW",
				ifelse(redf$scope_season=="Interests - Breeding","IB","IW")))
redf$refuge2<-factor(ifelse(redf$refuge=="Y","Refuge","Other lands"),levels=c("Refuge","Other lands"))

##plot one: x-facet=scope, y-facet = Species, x-axis = agency , y-axis=metric value [do one plot for B and one for W], metrics: scaled_AvgRate, scaled_probPresent, scaledPosEventFreq, scaledPosCellFreq
agpBar<-ggplot(data=subset(agdf,season=="B"),aes(x=agency,y=scaled_AvgRate)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Breeding Season",x="Agency",y="Scaled Avg. Encounter Rate") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_EncounterRate_Breeding.png",sep=""), width=650, height=650)
	print(agpBar)
dev.off()

agpWar<-ggplot(data=subset(agdf,season=="W"),aes(x=agency,y=scaled_AvgRate)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Over-winter",x="Agency",y="Scaled Avg. Encounter Rate") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_EncounterRate_Winter.png",sep=""), width=650, height=650)
	print(agpWar)
dev.off()

agpBpp<-ggplot(data=subset(agdf,season=="B"),aes(x=agency,y=scaled_probPresent)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Breeding Season",x="Agency",y="Scaled Probability of Presence") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_ProbPresence_Breeding.png",sep=""), width=650, height=650)
	print(agpBpp)
dev.off()

agpWpp<-ggplot(data=subset(agdf,season=="W"),aes(x=agency,y=scaled_probPresent)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Over-winter",x="Agency",y="Scaled Probability of Presence") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_ProbPresence_Winter.png",sep=""), width=650, height=650)
	print(agpWpp)
dev.off()

agpBpe<-ggplot(data=subset(agdf,season=="B"),aes(x=agency,y=scaledPosEventFreq)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Breeding Season",x="Agency",y="Scaled Frequency of Positive Events") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_PosEventFreq_Breeding.png",sep=""), width=650, height=650)
	print(agpBpe)
dev.off()

agpWpe<-ggplot(data=subset(agdf,season=="W"),aes(x=agency,y=scaledPosEventFreq)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Over-winter",x="Agency",y="Scaled Frequency of Positive Events") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_PosEventFreq_Winter.png",sep=""), width=650, height=650)
	print(agpWpe)
dev.off()

agpBpc<-ggplot(data=subset(agdf,season=="B"),aes(x=agency,y=scaledPosCellFreq)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Breeding Season",x="Agency",y="Scaled Probability of Presence in Cell") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_PosCellFreq_Breeding.png",sep=""), width=650, height=650)
	print(agpBpc)
dev.off()

agpWpc<-ggplot(data=subset(agdf,season=="W"),aes(x=agency,y=scaledPosCellFreq)) + geom_bar(stat="identity") +
		facet_grid(Species ~ Scope) + labs(title="Over-winter",x="Agency",y="Scaled Probability of Presence in Cell") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Agency_PosCellFreq_Winter.png",sep=""), width=650, height=650)
	print(agpWpc)
dev.off()

##plot two: same as above for refuge vs other
repBar<-ggplot(data=redf,aes(x=sc,y=scaled_AvgRate,fill=refuge2)) + 
		geom_bar(position="stack",stat="identity", width=0.6) + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="Definition-Season",y="Proportion of Scaled Avg. Encounter Rate",fill="Land owner") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_EncounterRate.png",sep=""), width=300, height=500)
print(repBar)
dev.off()

repBpp<-ggplot(data=redf,aes(x=sc,y=scaled_probPresent,fill=refuge2)) + 
		geom_bar(position="stack",stat="identity", width=0.6) + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="Definition-Season",y="Proportion of Scaled Probability of Presence",fill="Land owner") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_ProbPresence.png",sep=""), width=300, height=500)
print(repBpp)
dev.off()

repBpe<-ggplot(data=redf,aes(x=sc,y=scaledPosEventFreq,fill=refuge2)) + 
		geom_bar(position="stack",stat="identity", width=0.6) + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="Definition-Season",y="Proportion of Scaled Frequency of Positive Events",fill="Land owner") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_PosEventFreq.png",sep=""), width=300, height=500)
print(repBpe)
dev.off()

repBpc<-ggplot(data=redf,aes(x=sc,y=scaledPosCellFreq,fill=refuge2)) + 
		geom_bar(position="stack",stat="identity", width=0.6) + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="Definition-Season",y="Proportion of Scaled Frequency of Positive Cells",fill="Land owner") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_PosCellFreq.png",sep=""), width=300, height=500)
print(repBpc)
dev.off()

#plot per season + refuge definition, with x axis = agency, y axis = metric value, icon color = metric (red = positive cell, blue = probPresence…). 
#The graph would show a pattern across agencies, which we can “tie” with lines to further highlight that the metrics are congruent (b/c the lines would be “parallel”)
corPlt1<-ggplot(data=agdf,aes(x=scaledPosEventFreq,y=scaledPosCellFreq,shape=seas,color=Scope2)) + geom_point(size=2) + 
		labs(x="Frequency of positive events",y="Frequency of positive cells",shape="Season",color="Refuge def.")
png(filename=paste(pthres,"Metrics_correlation1.png",sep=""), width=300, height=200)
print(corPlt1)
dev.off()

corPlt2<-ggplot(data=agdf,aes(x=scaledPosCellFreq,y=scaled_probPresent,shape=seas,color=Scope2)) + geom_point(size=2) + 
		labs(x="Frequency of positive cells",y="Probability of presence",shape="Season",color="Refuge def.")
png(filename=paste(pthres,"Metrics_correlation2.png",sep=""), width=300, height=200)
print(corPlt2)
dev.off()

corPlt3<-ggplot(data=agdf,aes(x=scaled_probPresent,y=scaled_AvgRate,shape=seas,color=Scope2)) + geom_point(size=2) + 
		labs(x="Probability of presence",y="Avg. detection rate",shape="Season",color="Refuge def.")
png(filename=paste(pthres,"Metrics_correlation3.png",sep=""), width=300, height=200)
print(corPlt3)
dev.off()

corPlt4<-ggplot(data=agdf,aes(x=scaled_AvgRate,y=scaledPosEventFreq,shape=seas,color=Scope2)) + geom_point(size=2) + 
		labs(x="Avg. detection rate",y="Frequency of positive events",shape="Season",color="Refuge def.")
png(filename=paste(pthres,"Metrics_correlation4.png",sep=""), width=300, height=200)
print(corPlt4)
dev.off()



#####################################################
## OLD VERSION
repBar<-ggplot(data=subset(redf,refuge=="Y"),aes(x=refuge,y=scaled_AvgRate)) + 
		geom_bar(stat="identity", width=0.6, aes(fill=scope_season),position="dodge") + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="",y="Scaled Avg. Encounter Rate",fill="Refuge definition - season") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_EncounterRate.png",sep=""), width=400, height=600)
print(repBar)
dev.off()

repBpp<-ggplot(data=subset(redf,refuge=="Y"),aes(x=refuge,y=scaled_probPresent)) + 
		geom_bar(stat="identity", width=0.6, aes(fill=scope_season),position="dodge") + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="",y="Scaled Probability of Presence",fill="Refuge definition - season") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_ProbPresence.png",sep=""), width=400, height=600)
	print(repBpp)
dev.off()


repBpe<-ggplot(data=subset(redf,refuge=="Y"),aes(x=refuge,y=scaledPosEventFreq)) + 
		geom_bar(stat="identity", width=0.6, aes(fill=scope_season),position="dodge") + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="",y="Scaled Frequency of Positive Events",fill="Refuge definition") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_PosEventFreq.png",sep=""), width=400, height=600)
	print(repBpe)
dev.off()

repBpc<-ggplot(data=subset(redf,refuge=="Y"),aes(x=refuge,y=scaledPosCellFreq)) + 
		geom_bar(stat="identity", width=0.6, aes(fill=scope_season),position="dodge") + 
		geom_hline(yintercept=0.5,color="black",linetype="dotted",size=1) +
		facet_wrap(~Species, ncol=2) + labs(x="",y="Scaled Frequency of Positive Cells",fill="Refuge definition") +
		scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8))
png(filename=paste(pthres,"Refuge_PosCellFreq.png",sep=""), width=400, height=600)
	print(repBpc)
dev.off()




##plot three: same as above, but showing the difference if using encompassing vs narrow scopes
agdfd<-ddply(.data=agdf,.variables=c("Species","season"),.fun=diffmetric,contrast="agency")
agdfd<-ddply(.data=agdfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffAvgRate")
agdfd<-ddply(.data=agdfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffprobPresent")
agdfd<-ddply(.data=agdfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffPosEventFreq")
agdfd<-ddply(.data=agdfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffPosCellFreq")
agdfd$season2<-ifelse(agdfd$season=="W","Season: W","Season: B")

redfd<-ddply(.data=redf,.variables=c("Species","season"),.fun=diffmetric,contrast="refuge")
redfd<-ddply(.data=redfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffAvgRate")
redfd<-ddply(.data=redfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffprobPresent")
redfd<-ddply(.data=redfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffPosEventFreq")
redfd<-ddply(.data=redfd,.variables=c("Species","season","scope"),.fun=scaledmetric,metric="diffPosCellFreq")

agpBard<-ggplot(data=subset(agdfd,Species!="CLRA"),aes(x=agency,y=scaled_diffAvgRate)) + geom_bar(stat="identity",width=0.8) +
		facet_grid(Species~season2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="Agency",y="Scaled Diff. Avg. Encounter Rate") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"Agency_DiffEncounterRate.png",sep=""), width=650, height=550)
	print(agpBard)
dev.off()

agpBppd<-ggplot(data=subset(agdfd,Species!="CLRA"),aes(x=agency,y=scaled_diffprobPresent)) + geom_bar(stat="identity",width=0.8) +
		facet_grid(Species~season2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="Agency",y="Scaled Diff. Avg. Probability of Presence") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"Agency_DiffProbPresence.png",sep=""), width=650, height=550)
	print(agpBppd)
dev.off()

agpBped<-ggplot(data=subset(agdfd,Species!="CLRA"),aes(x=agency,y=scaled_diffPosEventFreq)) + geom_bar(stat="identity",width=0.8) +
		facet_grid(Species~season2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="Agency",y="Scaled Diff. Frequency of Positive Events") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"Agency_DiffPosEvents.png",sep=""), width=650, height=550)
	print(agpBped)
dev.off()

agpBpcd<-ggplot(data=subset(agdfd,Species!="CLRA"),aes(x=agency,y=scaled_diffPosCellFreq)) + geom_bar(stat="identity",width=0.8) +
		facet_grid(Species~season2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="Agency",y="Scaled Diff. Frequency of Positive Cells") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"Agency_DiffPosCells.png",sep=""), width=650, height=550)
	print(agpBpcd)
dev.off()


repBard<-ggplot(data=subset(redfd,Species!="CLRA"),aes(x=refuge,y=scaled_diffAvgRate)) + 
		geom_bar(stat="identity",aes(fill=season),position="dodge",width=0.8) +
		facet_wrap(~Species,ncol=2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="In Refuge",y="Scaled Diff. Avg. Encounter Rate") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"refuge_DiffEncounterRate.png",sep=""), width=450, height=450)
	print(repBard)
dev.off()

repBppd<-ggplot(data=subset(redfd,Species!="CLRA"),aes(x=refuge,y=scaled_diffprobPresent)) + 
		geom_bar(stat="identity",aes(fill=season),position="dodge",width=0.8) +
		facet_wrap(~Species,ncol=2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="In Refuge",y="Scaled Diff. Probability of Presence") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"refuge_DiffProbPresence.png",sep=""), width=450, height=450)
print(repBard)
dev.off()

repBped<-ggplot(data=subset(redfd,Species!="CLRA"),aes(x=refuge,y=scaled_diffPosEventFreq)) + 
		geom_bar(stat="identity",aes(fill=season),position="dodge",width=0.8) +
		facet_wrap(~Species,ncol=2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="In Refuge",y="Scaled Diff. Frequency of Positive Events") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"refuge_DiffPosEvent.png",sep=""), width=450, height=450)
print(repBard)
dev.off()

repBpcd<-ggplot(data=subset(redfd,Species!="CLRA"),aes(x=refuge,y=scaled_diffPosCellFreq)) + 
		geom_bar(stat="identity",aes(fill=season),position="dodge",width=0.8) +
		facet_wrap(~Species,ncol=2) + geom_hline(yintercept=0,color="dark gray") +
		labs(title="Differences due to refuge definition: Approved - Interests",x="In Refuge",y="Scaled Diff. Frequency of Positive Cells") +
		scale_y_continuous(breaks=c(-0.8,-0.4,0))
png(filename=paste(pthres,"refuge_DiffPosCells.png",sep=""), width=450, height=450)
print(repBard)
dev.off()



##last: report final tables...
#For each of: encounterRate, probPresence, eventFrequency and cellFrequency, create the following table:
#Rows = species, columns = Agency, cell = metric (scaled)
dd<-subset(agdf,season=="B" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_AvgRate")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"AvgRate_Approved_B.csv"))

dd<-subset(agdf,season=="W" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_AvgRate")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"AvgRate_Approved_W.csv"))

dd<-subset(agdf,season=="B" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_AvgRate")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"AvgRate_Interests_B.csv"))

dd<-subset(agdf,season=="W" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_AvgRate")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"AvgRate_Interests_W.csv"))

dd<-subset(agdf,season=="B" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_probPresent")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"probPresent_Approved_B.csv"))

dd<-subset(agdf,season=="W" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_probPresent")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"probPresent_Approved_W.csv"))

dd<-subset(agdf,season=="B" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_probPresent")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"probPresent_Interests_B.csv"))

dd<-subset(agdf,season=="W" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaled_probPresent")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"probPresent_Interests_W.csv"))

dd<-subset(agdf,season=="B" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosCellFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosCellFreq_Approved_B.csv"))

dd<-subset(agdf,season=="W" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosCellFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosCellFreq_Approved_W.csv"))

dd<-subset(agdf,season=="B" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosCellFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosCellFreq_Interests_B.csv"))

dd<-subset(agdf,season=="W" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosCellFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosCellFreq_Interests_W.csv"))

dd<-subset(agdf,season=="B" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosEventFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosEventFreq_Approved_B.csv"))

dd<-subset(agdf,season=="W" & scope=="encomp",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosEventFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosEventFreq_Approved_W.csv"))

dd<-subset(agdf,season=="B" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosEventFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosEventFreq_Interests_B.csv"))

dd<-subset(agdf,season=="W" & scope=="narrow",select=c("agency","scaled_AvgRate","scaled_probPresent","scaledPosCellFreq","scaledPosEventFreq","Species"))
t1<-cast(data=dd,Species ~ agency, value="scaledPosEventFreq")
t1d<-as.data.frame(apply(t1,2,FUN=round,4))
write.csv(t1d,file=paste(pthres,"PosEventFreq_Interests_W.csv"))


##Map plots: for each species... refuge vs. non-refuge + dist. range



