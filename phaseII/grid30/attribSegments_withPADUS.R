# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("data.table")
lapply(libs, require, character.only = TRUE)

segpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/"

#load the padus dflst file
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/PADUS_R68/padrast_objid.RData")
#loop through it, identifying the min and max cellIds
for(pp in 25909:35108){
	pad<-dflst[[pp]]
	pad$cellId<-as.numeric(pad$cellId)
	#based on modulus 10000000, determine the segment(s) that will house these cell ids:
	#  floor(min(pad$cellId)/10000000), ceiling(max(pad$cellId)/10000000)
	stsg<-floor(min(pad$cellId)/10000000)+1
	edsg<-ceiling(max(pad$cellId)/10000000)
	#check that min(pad$cellId) is in stsg
	#check that max(pad$cellId) is in edsg
	load(paste(segpth,"r68_seg_",stsg,".RData",sep=""))
	if(min(xy30r68$cellId)>min(pad$cellId)){stop("Min cell lower than starting segment")}
	load(paste(segpth,"r68_seg_",edsg,".RData",sep=""))
	if(max(xy30r68$cellId)<max(pad$cellId)){stop("Max cell higher than ending segment")}
	#loop through the segments (already with Region attrib) from the min/max above
	# load each, merge with pad, save
	sseq<-seq(stsg,edsg,1)
	for(ss in sseq){
		load(paste(segpth,"r68_seg_",ss,".RData",sep=""))
		if("pdobjid" %in% names(xy30r68)){
			xya<-subset(xy30r68,is.na(xy30r68$pdobjid)==TRUE)
			xyb<-subset(xy30r68,is.na(xy30r68$pdobjid)==FALSE)
			xya<-merge(xya[,1:4],pad,by="cellId",all.x=T)
			xy30r68<-rbind(xya,xyb)
			xy30r68<-xy30r68[order(xy30r68$cellId),]
			save(xy30r68,file=paste(segpth,"r68_seg_",ss,".RData",sep=""))
		}else{
			xy30r68<-merge(xy30r68,pad,by="cellId",all.x=T)
			save(xy30r68,file=paste(segpth,"r68_seg_",ss,".RData",sep=""))
		}
		
	}
	print(pp)
}




################ MURUK etc.
libs<-c("data.table")
lapply(libs, require, character.only = TRUE)

segpth<-"/home/lsalas/adalo/grid30R68/"

#load the padus dflst file
load("/home/lsalas/adalo/PADUS_R68/padrast_objid.RData")
#loop through it, identifying the min and max cellIds
for(pp in 35109:46004){
	pad<-dflst[[pp]]
	pad$cellId<-as.numeric(pad$cellId)
	#based on modulus 10000000, determine the segment(s) that will house these cell ids:
	#  floor(min(pad$cellId)/10000000), ceiling(max(pad$cellId)/10000000)
	stsg<-floor(min(pad$cellId)/10000000)+1
	edsg<-ceiling(max(pad$cellId)/10000000)
	#check that min(pad$cellId) is in stsg
	#check that max(pad$cellId) is in edsg
	load(paste(segpth,"r68_seg_",stsg,".RData",sep=""))
	if(min(xy30r68$cellId)>min(pad$cellId)){stop("Min cell lower than starting segment")}
	load(paste(segpth,"r68_seg_",edsg,".RData",sep=""))
	if(max(xy30r68$cellId)<max(pad$cellId)){stop("Max cell higher than ending segment")}
	#loop through the segments (already with Region attrib) from the min/max above
	# load each, merge with pad, save
	sseq<-seq(stsg,edsg,1)
	for(ss in sseq){
		load(paste(segpth,"r68_seg_",ss,".RData",sep=""))
		if("pdobjid" %in% names(xy30r68)){
			xya<-subset(xy30r68,is.na(xy30r68$pdobjid)==TRUE)
			xyb<-subset(xy30r68,is.na(xy30r68$pdobjid)==FALSE)
			xya<-merge(xya[,1:4],pad,by="cellId",all.x=T)
			xy30r68<-rbind(xya,xyb)
			xy30r68<-xy30r68[order(xy30r68$cellId),]
			save(xy30r68,file=paste(segpth,"r68_seg_",ss,".RData",sep=""))
		}else{
			xy30r68<-merge(xy30r68,pad,by="cellId",all.x=T)
			save(xy30r68,file=paste(segpth,"r68_seg_",ss,".RData",sep=""))
		}
		
	}
	print(pp)
}


