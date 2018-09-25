# TODO: Add comment
# 
# Author: lsalas
###############################################################################


pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data2/bySpecies/FilteredByRangeNew/"


spcd=c("BAIS","BLRA","BOBO","BUOW","CANV","CCLO","FEHA","LBCU","LETE","MAGO","MOPL","NOPI","RIRA","SACR","SNPL","SPPI","TRBL","WIFL")
periods<-c("b","w")

for(sss in spcd){  #calculate metric 4 by cell
	#NEED to do this for BREEDING and WINTER
	for(per in periods){
		fpth<-paste(pth,sss,"_",per,"_filtered.RData",sep="")
		if(file.exists(fpth)){
			print(paste(sss,per))
			load(fpth)
			cdf<-plot.df[,c("source","species","lon","lat","count")]
			write.csv(cdf,file=paste(pth,"CSVs/",sss,"_",per,".csv",sep=""))
		}
	}	
}

