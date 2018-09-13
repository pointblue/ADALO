# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("rgdal","plyr","data.table","sf","dplyr")
lapply(libs, require, character.only = TRUE)

tst<-Sys.time()
#load the geopols
lcc<-st_read("/home/ubuntu/Geopolitical/LCC","2015_LCC_Networks")
lcc<-st_transform(lcc,4326)
lcc<-lcc %>% filter(Area_Num %in% c(15,13,7,2,11,5,6,3,4))

#nps<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/NPS","National_Park_Service__Regional_Boundaries")
nps<-st_read("/home/ubuntu/Geopolitical/NPS","National_Park_Service__Regional_Boundaries")
nps<-st_transform(nps,4326)
nps<-nps %>% filter(as.character(nps$Region) %in% c("Midwest","Intermountain","Pacific West"))

#usfs<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/USFS","S_USA.AdministrativeRegion")
usfs<-readOGR("/home/ubuntu/Geopolitical/USFS","S_USA.AdministrativeRegion")
usfs<-st_transform(usfs,4326)
usfs<-usfs %>% filter(as.character(usfs$REGION) %in% c("01","02","04","05","06"))

#usjv<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/JV","North_American_Joint_Ventures_GCS")
usjv<-st_read("/home/ubuntu/Geopolitical/JV","North_American_Joint_Ventures_GCS")
usjv<-st_transform(usjv,4326)
usjv<-usjv %>% filter(usjv$code %in% c(3,4,5,6,8,9,10,12,16,21))

#county<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/County","uscounties_albers")
county<-st_read("/home/ubuntu/Geopolitical/County","uscounties_albers")
county<-st_transform(county,4326)
county<-county %>% filter(as.character(county$STATE_FIPS) %in% c("06","08","20","30","31","32","38","41","46","49","56"))

#bcr<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Geopolitical/BCR","bcr_albers")
bcr<-st_read("/home/ubuntu/Geopolitical/BCR","bcr_albers")
bcr<-st_transform(bcr,4326)
bcr<-bcr %>% filter(bcr$BCR %in% c(5,15,32,33,9,10,11,17,18,19,16))
Sys.time()-tst

segpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/30mGrid/segByFWSRegion/wObjId/"
segments<-list.files(segpth,pattern="seg_")
for(ss in segments){
	load(paste(segpth,ss,sep=""));names(xy30r68)<-gsub("Region","USFWSregion",names(xy30r68))
	xy30<-data.table(xy30r68)
	xy30<-st_as_sf(xy30, coords = c("x", "y"),
			crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs", agr = "constant")
	xy30<-st_transform(xy30,4326)
	
	xy30r68[,LCCregion:=st_intersects(xy30,lcc["Area_Num"]),]  #NO! It assumes planar coords, and the result is an sgbp object that does not contain what we need
	
	
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
