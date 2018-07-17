# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(reshape)
agNb<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Narrow/nbAgency_B.csv");agNb$season<-"B";agNb$scope<-"narrow"
agEb<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Encompassing/nbAgency_B.csv");agEb$season<-"B";agEb$scope<-"encomp"
agNw<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Narrow/nbAgency_W.csv");agNw$season<-"W";agNw$scope<-"narrow"
agEw<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AllModels/Encompassing/nbAgency_W.csv");agEw$season<-"W";agEw$scope<-"encomp"
agdf<-rbind(agNb,agEb,agNw,agEw);agdf$agency<-ifelse(agdf$agency=="OtherPublic","Public",as.character(agdf$agency))

t1<-cast(data=subset(agdf, season=="B" & scope=="narrow",select=c("Species","agency","nEvents")),Species ~ agency, value="nEvents")
t1d<-as.data.frame(t1);t1d$SpeciesTotal<-apply(t1d[,2:9],1,sum)
totals<-apply(t1d[,2:10],2,sum);t1d[11,2:10]<-totals;t1d$Species<-as.character(t1d$Species);t1d[11,1]<-"Total"
t2<-cast(data=subset(agdf, season=="B" & scope=="narrow",select=c("Species","agency","eventsWdet")),Species ~ agency, value="eventsWdet")
t2d<-as.data.frame(t2);t2d$SpeciesTotal<-apply(t2d[,2:9],1,sum)
totals<-apply(t2d[,2:10],2,sum);t2d[11,2:10]<-totals;t2d$Species<-as.character(t2d$Species);t2d[11,1]<-"Total"
eventsBn.df<-data.frame(Defn="Int",
		Season="B",
		Species=t1d$Species,
		BLM=paste(as.character(t2d$BLM),as.character(t1d$BLM),sep="|"),
		DOD=paste(as.character(t2d$DOD),as.character(t1d$DOD),sep="|"),
		NPS=paste(as.character(t2d$NPS),as.character(t1d$NPS),sep="|"),
		Private=paste(as.character(t2d$Private),as.character(t1d$Private),sep="|"),
		Public=paste(as.character(t2d$Public),as.character(t1d$Public),sep="|"),
		Refuge=paste(as.character(t2d$Refuge),as.character(t1d$Refuge),sep="|"),
		State=paste(as.character(t2d$State),as.character(t1d$State),sep="|"),
		USFS=paste(as.character(t2d$USFS),as.character(t1d$USFS),sep="|"),
		Total=paste(as.character(t2d$SpeciesTotal),as.character(t1d$SpeciesTotal),sep="|")
)
write.csv(eventsBn.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Report/PlotsTables/obsEvents_BN.csv")


t1<-cast(data=subset(agdf, season=="B" & scope=="encomp",select=c("Species","agency","nEvents")),Species ~ agency, value="nEvents")
t1d<-as.data.frame(t1);t1d$SpeciesTotal<-apply(t1d[,2:9],1,sum)
totals<-apply(t1d[,2:10],2,sum);t1d[11,2:10]<-totals;t1d$Species<-as.character(t1d$Species);t1d[11,1]<-"Total"
t2<-cast(data=subset(agdf, season=="B" & scope=="encomp",select=c("Species","agency","eventsWdet")),Species ~ agency, value="eventsWdet")
t2d<-as.data.frame(t2);t2d$SpeciesTotal<-apply(t2d[,2:9],1,sum)
totals<-apply(t2d[,2:10],2,sum);t2d[11,2:10]<-totals;t2d$Species<-as.character(t2d$Species);t2d[11,1]<-"Total"
eventsBe.df<-data.frame(Defn="Approved",
		Season="B",
		Species=t1d$Species,
		BLM=paste(as.character(t2d$BLM),as.character(t1d$BLM),sep="|"),
		DOD=paste(as.character(t2d$DOD),as.character(t1d$DOD),sep="|"),
		NPS=paste(as.character(t2d$NPS),as.character(t1d$NPS),sep="|"),
		Private=paste(as.character(t2d$Private),as.character(t1d$Private),sep="|"),
		Public=paste(as.character(t2d$Public),as.character(t1d$Public),sep="|"),
		Refuge=paste(as.character(t2d$Refuge),as.character(t1d$Refuge),sep="|"),
		State=paste(as.character(t2d$State),as.character(t1d$State),sep="|"),
		USFS=paste(as.character(t2d$USFS),as.character(t1d$USFS),sep="|"),
		Total=paste(as.character(t2d$SpeciesTotal),as.character(t1d$SpeciesTotal),sep="|")
)
write.csv(eventsBe.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Report/PlotsTables/obsEvents_BE.csv")

#winter
t1<-cast(data=subset(agdf, season=="W" & scope=="narrow",select=c("Species","agency","nEvents")),Species ~ agency, value="nEvents")
t1d<-as.data.frame(t1);t1d$SpeciesTotal<-apply(t1d[,2:9],1,sum)
totals<-apply(t1d[,2:10],2,sum);t1d[9,2:10]<-totals;t1d$Species<-as.character(t1d$Species);t1d[9,1]<-"Total"
t2<-cast(data=subset(agdf, season=="W" & scope=="narrow",select=c("Species","agency","eventsWdet")),Species ~ agency, value="eventsWdet")
t2d<-as.data.frame(t2);t2d$SpeciesTotal<-apply(t2d[,2:9],1,sum)
totals<-apply(t2d[,2:10],2,sum);t2d[9,2:10]<-totals;t2d$Species<-as.character(t2d$Species);t2d[9,1]<-"Total"
eventsWn.df<-data.frame(Defn="Interests",
		Season="W",
		Species=t1d$Species,
		BLM=paste(as.character(t2d$BLM),as.character(t1d$BLM),sep="|"),
		DOD=paste(as.character(t2d$DOD),as.character(t1d$DOD),sep="|"),
		NPS=paste(as.character(t2d$NPS),as.character(t1d$NPS),sep="|"),
		Private=paste(as.character(t2d$Private),as.character(t1d$Private),sep="|"),
		Public=paste(as.character(t2d$Public),as.character(t1d$Public),sep="|"),
		Refuge=paste(as.character(t2d$Refuge),as.character(t1d$Refuge),sep="|"),
		State=paste(as.character(t2d$State),as.character(t1d$State),sep="|"),
		USFS=paste(as.character(t2d$USFS),as.character(t1d$USFS),sep="|"),
		SpeciesTotal=paste(as.character(t2d$SpeciesTotal),as.character(t1d$SpeciesTotal),sep="|")
)
write.csv(eventsWn.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Report/PlotsTables/obsEvents_WN.csv")


t1<-cast(data=subset(agdf, season=="W" & scope=="encomp",select=c("Species","agency","nEvents")),Species ~ agency, value="nEvents")
t1d<-as.data.frame(t1);t1d$SpeciesTotal<-apply(t1d[,2:9],1,sum)
totals<-apply(t1d[,2:10],2,sum);t1d[9,2:10]<-totals;t1d$Species<-as.character(t1d$Species);t1d[9,1]<-"Total"
t2<-cast(data=subset(agdf, season=="W" & scope=="encomp",select=c("Species","agency","eventsWdet")),Species ~ agency, value="eventsWdet")
t2d<-as.data.frame(t2);t2d$SpeciesTotal<-apply(t2d[,2:9],1,sum)
totals<-apply(t2d[,2:10],2,sum);t2d[9,2:10]<-totals;t2d$Species<-as.character(t2d$Species);t2d[9,1]<-"Total"
eventsWe.df<-data.frame(Defn="Approved",
		Season="Over-winter",
		Species=t1d$Species,
		BLM=paste(as.character(t2d$BLM),as.character(t1d$BLM),sep="|"),
		DOD=paste(as.character(t2d$DOD),as.character(t1d$DOD),sep="|"),
		NPS=paste(as.character(t2d$NPS),as.character(t1d$NPS),sep="|"),
		Private=paste(as.character(t2d$Private),as.character(t1d$Private),sep="|"),
		Public=paste(as.character(t2d$Public),as.character(t1d$Public),sep="|"),
		Refuge=paste(as.character(t2d$Refuge),as.character(t1d$Refuge),sep="|"),
		State=paste(as.character(t2d$State),as.character(t1d$State),sep="|"),
		USFS=paste(as.character(t2d$USFS),as.character(t1d$USFS),sep="|"),
		SpeciesTotal=paste(as.character(t2d$SpeciesTotal),as.character(t1d$SpeciesTotal),sep="|")
)
write.csv(eventsWe.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Report/PlotsTables/obsEvents_WE.csv")


