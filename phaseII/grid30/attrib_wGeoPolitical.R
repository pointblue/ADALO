# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("rgdal","sp","rgeos","plyr","raster","data.table")
lapply(libs, require, character.only = TRUE)

#load the geopols
#lcc<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/LCC","2015_LCC_Networks")
lcc<-readOGR("/home/ubuntu/Geopolitical/LCC","2015_LCC_Networks")
lcc<-spTransform(lcc,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
lcc<-lcc[lcc$Area_Num %in% c(15,13,7,2,11,5,6,3,4),]

#nps<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/NPS","National_Park_Service__Regional_Boundaries")
nps<-readOGR("/home/ubuntu/Geopolitical/NPS","National_Park_Service__Regional_Boundaries")
nps<-spTransform(nps,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
nps<-nps[as.character(nps$Region) %in% c("Midwest","Intermountain","Pacific West"),]

#usfs<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/USFS","S_USA.AdministrativeRegion")
usfs<-readOGR("/home/ubuntu/Geopolitical/USFS","S_USA.AdministrativeRegion")
usfs<-spTransform(usfs,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
usfs<-usfs[as.character(usfs$REGION) %in% c("01","02","04","05","06"),]

#usjv<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/JV","North_American_Joint_Ventures_GCS")
usjv<-readOGR("/home/ubuntu/Geopolitical/JV","North_American_Joint_Ventures_GCS")
usjv<-spTransform(usjv,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
usjv<-usjv[usjv$code %in% c(3,4,5,6,8,9,10,12,16,21),]

#county<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/County","uscounties_albers")
county<-readOGR("/home/ubuntu/Geopolitical/County","uscounties_albers")
county<-spTransform(county,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
county<-county[as.character(county$STATE_FIPS) %in% c("06","08","20","30","31","32","38","41","46","49","56"),]

#bcr<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/BCR","bcr_albers")
bcr<-readOGR("/home/ubuntu/Geopolitical/BCR","bcr_albers")
bcr<-spTransform(bcr,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
bcr<-bcr[bcr$BCR %in% c(5,15,32,33,9,10,11,17,18,19,16),]

#segpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/wObjId/"
tm<-Sys.time()
segpth<-"/home/ubuntu/30mGrid/segByFWSRegion/wObjId/"
segments<-list.files(segpth,pattern="seg_")
for(ss in segments){
	load(paste(segpth,ss,sep=""));names(xy30r68)<-gsub("Region","USFWSregion",names(xy30r68))
	xy30r68<-data.table(xy30r68)
	xy30<-data.table(xy30r68)
	coordinates(xy30)<-c("x","y")
	proj4string(xy30) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
	xy30<-spTransform(xy30,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
	xy30r68[,LCCregion:=over(xy30,lcc["Area_Num"]),]
	xy30r68[,USFSregion:=over(xy30,usfs["REGION"]),]
	xy30r68[,NPSregion:=over(xy30,nps["Region"]),]
	xy30r68[,USJVregion:=over(xy30,usjv["code"]),]
	xy30r68[,BCRregion:=over(xy30,bcr["BCR"]),]
	xy30r68[,CountyFIPS:=over(xy30,county["FIPS"]),]
	#sadly, remove cells without fips attribution, or in states outside our interest
	xy30r68<-subset(xy30r68,!is.na(CountyFIPS))
	xy30r68[,USFSregion:=as.character(USFSregion),];xy30r68[,NPSregion:=as.character(NPSregion),]
	xy30r68[,CountyFIPS:=as.character(CountyFIPS),]
	xy30r68[,StateFIPS:=substr(CountyFIPS,1,2),]
	xy30r68<-subset(xy30r68,StateFIPS %in% c("06","08","20","30","31","32","38","41","46","49","56"))
	
	#save
	save(xy30r68,file=paste(segpth,ss,sep=""))
	rm(list=c("xy30r68","xy30"));gc()
	print(ss)
	
}
Sys.time()-tm


