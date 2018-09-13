# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#This file prepares a first set of indices using data and models
# 1) Load the data
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/Data/bySpecies/"

library(rgdal)
library(sp)
library(raster)

#NO! This should be done to the cells in the R8 R6 matrix. So we have an attribute of ownership, plus land cover per cell.
# We then copy this matrix and subset it for each species' range, and attribute with the other metrics.

spcd=c("BLRA","BRCO","BUOW","CANV","CCLO","FEHA","GRSG","LBCU",
		"MAGO","MCLO","MOPL","NOPI","Yuma_RIRA","CA_RIRA","SACR","SNPL","SPPI","WIFL")

for(sss in spcd){
	load(paste(pth,"filteredByRange/",sss,"_filtered.RData",sep=""))
	coors.df<-unique(plot.df[,c("lon","lat")])
	coors.df$lon<-as.numeric(as.character(coors.df$lon)); coors.df$lat<-as.numeric(as.character(coors.df$lat))
	coors.df<-subset(coors.df,!is.null(lon) & !is.null(lat) & !is.na(lon) & !is.na(lat) & lon!="" & lat!="")
	sppts<-coors.df
	coordinates(sppts)<-c("lon","lat")
	proj4string(sppts)<-CRS("+proj=longlat +datum=WGS84")
	
	# 2) Attribute with geospatial data
	###PADUS
	padus<-readOGR("V:/Data/landuse/current/PADUS1_4Shapefile",layer="PADUS1_4Combined")
	padusProj<-projection(padus)
	sppts.padus<-spTransform(sppts,CRS(padusProj))
	padus.attr<-over(sppts.padus,padus)
	coors.df$padus<-padus.attr$MgmtStatus
	
	###Landfire
	
	# 3) Make the file collated.df: code in plotData.R 
	# (this also needs updating after changes in attribute_geospatial_New.R)
	
}


# 4) Generate species plots: plotData.R
# 5) Fit models for probPresence and incidence rate: basicModels.R
# 5) Generate report plots: reportPlots.R
# 6) Generate report tables: tables_byRefuge.R
