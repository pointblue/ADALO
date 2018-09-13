# TODO: Add comment
# 
# Author: lsalas
###############################################################################

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"

#load the range data
seasons<-read.csv(file=paste(pth,"seasons.csv",sep=""))
#loop through species
fils<-list.files(paste(pth,"Data2/bySpecies/",sep=""),pattern=".RData")
for(ff in fils){
	load(paste(pth,"Data2/bySpecies/",ff,sep=""))
	spcd<-substr(ff,1,4)
	if(spcd=="RIRA"){
		spcd<-"CA_RIRA"
	}
	if(spcd %in% seasons$SpCode){
		#remove 0s
		spdf<-subset(spdf,count>0)
		#make mnth
		spdf$mnth<-as.numeric(format(spdf$obsDate,"%m"))
		#filter by season
		ts<-subset(seasons,SpCode==spcd)
		brs<-ts[,"BrRngSt"];bre<-ts[,"BrRngEnd"];wim<-ts[,"WiRng"]
		breed<-subset(spdf,mnth>=brs & mnth<=bre)
		if(spcd=="SPPI"){
			winte<-subset(spdf,mnth %in% c(12,1,2))
		}else{
			winte<-subset(spdf,mnth==wim)
		}
		
		#save as csvs
		if(nrow(breed)>0){
			write.csv(breed,file=paste(pth,"Data2/bySpecies/forRangeGen/",spcd,"_breeding.csv",sep=""))
		}
		if(nrow(winte)>0){
			write.csv(winte,file=paste(pth,"Data2/bySpecies/forRangeGen/",spcd,"_winter.csv",sep=""))
		}
	}
}

