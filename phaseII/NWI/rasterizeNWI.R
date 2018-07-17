# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(raster)
library(sp)
library(rgdal)
library(rgeos)

getPolys<-function(fln,dsn,wetlands,atrib.df,ic,fact=0){
	sw<-readOGR(dsn,fln)
	spol<-slot(sw,"polygons")
	stdf<-data.frame(polynum=1:NROW(spol),type=as.character(sw$WETLAND_TY),state=ss)
	stdf<-subset(stdf,type %in% c("Freshwater Forested/Shrub Wetland","Estuarine and Marine Deepwater","Estuarine and Marine Wetland"))
	stdf$Type<-ifelse(stdf$type=="Freshwater Forested/Shrub Wetland",1,
			ifelse(stdf$type=="Estuarine and Marine Deepwater",2,3))
	attrib.df<-rbind(attrib.df,stdf)
	selpol<-spol[stdf$polynum]
	wetlands<-c(wetlands,selpol)
	return(list(wetlands,attrib.df))
}
#load the polygons from all states
#Remove anything but the 3 categories of interest
#merge into a single data frame
states<-c("CA","CO","KS","MT","ND","NE","NV","OR","SD","UT","WY")
pthpoly<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/"
ic<-0
wetlands<-list();attrib.df<-data.frame()
for(ss in states){
	print(ss)
	ic<-ic+1
	dsn<-paste(pthpoly,ss,"_shapefile_wetlands",sep="")
	if(ss %in% c("CO","KS","MT","ND","NE","OR","SD","WY")){
		fln<-paste(ss,"_Wetlands_East",sep="")
		res<-getPolys(fln=fln,dsn=dsn,wetlands=wetlands,atrib.df=attrib.df,ic=ic,fact=0)
		wetlands<-res[[1]];attrib.df<-res[[2]]
		
		fln<-paste(ss,"_Wetlands_West",sep="")
		res<-getPolys(fln=fln,dsn=dsn,wetlands=wetlands,atrib.df=attrib.df,ic=ic,fact=500000)
		wetlands<-res[[1]];attrib.df<-res[[2]]
		
	}else if(ss %in% c("CA", "NV")){
		fln<-paste(ss,"_Wetlands_North",sep="")
		res<-getPolys(fln=fln,dsn=dsn,wetlands=wetlands,atrib.df=attrib.df,ic=ic,fact=0)
		wetlands<-res[[1]];attrib.df<-res[[2]]
		
		fln<-paste(ss,"_Wetlands_South",sep="")
		res<-getPolys(fln=fln,dsn=dsn,wetlands=wetlands,atrib.df=attrib.df,ic=ic,fact=300000)
		wetlands<-res[[1]];attrib.df<-res[[2]]
		
		if(ss=="CA"){
			fln<-paste(ss,"_Wetlands_Central",sep="")
			res<-getPolys(fln=fln,dsn=dsn,wetlands=wetlands,atrib.df=attrib.df,ic=ic,fact=600000)
			wetlands<-res[[1]];attrib.df<-res[[2]]
		}
	}else{	#this applies only to UT
		fln<-paste(ss,"_Wetlands",sep="")
		res<-getPolys(fln=fln,dsn=dsn,wetlands=wetlands,atrib.df=attrib.df,ic=ic,fact=0)
		wetlands<-res[[1]];attrib.df<-res[[2]]
	}
}
row.names(attrib.df)<-NULL
attrib.df$ID<-as.character(1:nrow(attrib.df))
for(rr in 1:NROW(wetlands)){
	slot(wetlands[[rr]],"ID")<-as.character(rr)
}
wetlandsp<-SpatialPolygons(wetlands)
save(wetlandsp,attrib.df,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/wetlands_polygons_nwi.RData")

#make polynum be ID in attrib.df, then do SpatialPolygonsDataFrame(wetlandsp,data=attrib.df,match.ID=T)
names(attrib.df)<-gsub("ID","ID2",names(attrib.df))
names(attrib.df)<-gsub("polynum","ID",names(attrib.df))
wetlandspdf<-SpatialPolygonsDataFrame(wetlandsp,data=attrib.df,match.ID=T)
projection(wetlandspdf)<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
save(wetlandspdf,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/wetlandspdf_nwi.RData")
wetlandspdf<-spTransform(wetlandspdf,CRS("+proj=longlat +datum=WGS84"))
writeOGR(wetlandspdf["type"], "/home/lsalas/refuge/wetlandspdf2.kml", layer="type", driver="KML")
#writeOGR(wetlandspdf["type"], "/home/lsalas/refuge/wetlandspdfshp", layer="type", driver="ESRI Shapefile")
library(maptools)
writeSpatialShape(wetlandspdf["type"],"/home/lsalas/refuge/wetlandspdfshp")

#rasterize
baserast<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/nlcdR68.grd")
nwirast<-rasterize(wetlandspdf,baserast,field="Type",fun="last")
writeRaster(nwirast,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/NWI/nwiR68.grd")

#need to change the value of the raster to the type value
#then need to do some algebra with the nlcdR68 raster so that if it is 0, then use nlcd value, else...
#finally, aggregate the resulting raster


#get a null 30m raster and assign cell value based on polygon value
#clip by R68 boundaries
#rasterize at 30m

