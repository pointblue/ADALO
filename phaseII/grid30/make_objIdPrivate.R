# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#1) Loop through each segment, convert every NA to 1 in podbjid
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/"
fls<-list.files(paste(pth,"processed/",sep=""))

for (ff in fls){
	load(paste(pth,"processed/",ff,sep=""))
	xy30r68$pdobjid<-ifelse(is.na(xy30r68$pdobjid),0,xy30r68$pdobjid)
	save(xy30r68,file=paste(pth,"wObjId/",ff,sep=""))
	print(ff)
}

