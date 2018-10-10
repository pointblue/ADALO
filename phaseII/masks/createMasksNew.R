# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#This code takes the land cover variables and creates masks per species
#BUT using the enhanced masks Orien requested 

libs<-c("raster","plyr","XLConnect")
lapply(libs, require, character.only = TRUE)

rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"

maskdef<-try(readWorksheetFromFile(paste(rpth,"mask/Masks_LandCover_bySpecies.xlsx",sep="/"),sheet="Land_Cover_Vars_New"))
spp<-names(maskdef)[-1]

#create the stack with the 990 cells and only for the NLCD, NWI and CDL data
#put them in the same order as the maskdef table
covars_all<-stack(paste(rpth,"CovarStack/covarstack_masked.grd",sep=""))
covars<-covars_all[[1:36]]
covars<-covars[[c(1,11,5,2,10,6,3,4,7:9,12,13,14:23,31:33,28,26,24,25,30,29,27,36,34,35)]]
baserast<-covars[[1]];baserast[]<-NA

load(file=paste0(rpth,"CovarStack/covarsdf.RData"))
identical(names(covarsdf),maskdef$LayerName)	#must be true for the dot product to work correctly
covars.mx<-as.matrix(covarsdf)
covars.mx[is.na(covars.mx)] <- 0	#need to convert all NA's to 0's to apply the dot product
#the above change will say 0 instead of NA for any covariate, which is OK

#dot.multiply the matrix by the species' vector
#loop across species
#assign the results to a base grid 

w<-l_ply(spp,.fun=function(x,covars.mx,maskdef,baserast){
			print(x);
			a<-covars.mx %*% maskdef[,x];
			ab<-ifelse(a<2,0,1); #The 1% mask
			ac<-ifelse(a<6,0,1); #The 5% mask
			mask01<-baserast;mask05<-baserast;
			mask01[]<-ab;
			mask05[]<-ac;
			writeRaster(mask01,filename=paste(rpth,"/mask/speciesMasks/",x,"_mask01.tif",sep=""),format="GTiff",overwrite=TRUE);
			writeRaster(mask05,filename=paste(rpth,"/mask/speciesMasks/",x,"_mask05.tif",sep=""),format="GTiff",overwrite=TRUE)
		},covars.mx=covars.mx,maskdef=maskdef,baserast=baserast)

