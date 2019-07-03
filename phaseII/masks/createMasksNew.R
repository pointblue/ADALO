# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#This code takes the land cover variables and creates masks per species
#BUT using the enhanced masks Orien requested 

## Load libraries
libs<-c("raster","plyr","XLConnect","data.table")
lapply(libs, require, character.only = TRUE)

## Base path
rpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/RefugePrioritization/Phase2/"

## Get the masks definitions table
#maskdef<-try(readWorksheetFromFile(paste(rpth,"mask/Masks_LandCover_bySpecies.xlsx",sep="/"),sheet="Land_Cover_Vars_New"))
maskddd<-read.csv(paste(rpth,"mask/ADALO_land_cover_masks_20190215.csv",sep="/"))
maskddd<-subset(maskddd,Action != "Discard")
maskdeft<-maskddd[,c(23,5:22)]
maskdeft$LayerName<-as.character(maskdeft$LayerName)

## Values 21-24 are multiplied into NLCD_devel
nlcdDevel<-subset(maskddd,LayerName=="NLCD_devel");nlcdDevel<-apply(nlcdDevel[5:22],2,prod)
tdf<-data.frame(LayerName="NLCD_devel",t(nlcdDevel))
maskdeft<-subset(maskdeft,LayerName!="NLCD_devel");maskdeft<-rbind(maskdeft,tdf)

## Values 90 and 95 are multiplied into NLCD_wetl
nlcdWetl<-subset(maskddd,LayerName=="NLCD_wetl");nlcdWetl<-apply(nlcdWetl[5:22],2,prod)
tdf<-data.frame(LayerName="NLCD_wetl",t(nlcdWetl))
maskdeft<-subset(maskdeft,LayerName!="NLCD_wetl");maskdeft<-rbind(maskdeft,tdf)

## This is the list of species whose mask we'll generate
spp<-names(maskdeft)[-1]

## create the stack with the 990 cells and only for the NLCD, NWI and CDL data...
## but in the same order as the maskdef table
covars_all<-stack(paste(rpth,"CovarStack/covarstack_masked.grd",sep=""))
covars<-covars_all[[1:36]]
# checking... but see also below
maskdeft$LayerName[which(!maskdeft$LayerName %in% names(covars))]
names(covars)[which(!names(covars) %in% maskdeft$LayerName)]
maskdeft<-subset(maskdeft,LayerName %in% names(covars))  #if the above are empty vectors, this is unneccesary
## Need a base raster from which to construct the masks
baserast<-covars[[1]];baserast[]<-NA
load(file=paste0(rpth,"CovarStack/covarsdf.RData"))
#the order of layers in maskdef MUST match that of covarsdf
maskdef<-ldply(names(covarsdf),.fun=function(x,maskdeft){
			tdf<-subset(maskdeft,LayerName==x);return(tdf)
		},maskdeft=maskdeft)
#now making the stack also match...
covars<-covars[[names(covarsdf)]]
### IMPORTANT CHECK
identical(names(covarsdf),maskdef$LayerName)	#must be true for the dot product to work correctly
identical(names(covars),maskdef$LayerName)
covars.mx<-as.matrix(covarsdf)	#as.data.frame if not using matrix mult
covars.ref<-covars.mx[,1]	#keep this referenceto know which cells to make NA (because they are outside of our jurisdiction  
covars.mx[is.na(covars.mx)] <- 0	#need to convert all NA's to 0's to apply the dot product


## create the masks...
w<-l_ply(spp,.fun=function(x,covars.mx,maskdef,baserast,covars.ref){
			print(x);
			
			#NOW do the matrix multiplication:
			#spdef<-covars.mx %*% maskdef[,x]
			#If the product of all NWI layers >= 100 stop. Make it 100. This is the first 13 values.
			#Else, if the NWI product + CDL layers product >= 100 stop. Make it 100 - the NWI layers + the reminder of CDL layers. Layers 14-23
			#Else, add the value from NLCD layers - layers 24-36
			
			#splitting into smaller matrices for faster computation and to meet the above requirement
			covars.nwi<-covars.mx[,1:13];maskdef.nwi<-maskdef[1:13,x]
			spdef.nwi<-covars.nwi %*% maskdef.nwi
			
			covars.cdl<-covars.mx[,14:23];maskdef.cdl<-maskdef[14:23,x]
			spdef.cdl<-covars.cdl %*% maskdef.cdl
			
			covars.nlcd<-covars.mx[,24:36];maskdef.nlcd<-maskdef[24:36,x]
			spdef.nlcd<-covars.nlcd %*% maskdef.nlcd
			
			#construct the definition above by linking the vectors in a data.table, but adding back the NAs
			spdef.dt<-data.table(data.frame(nwi=spdef.nwi[,1],cdl=spdef.cdl[,1],nlcd=spdef.nlcd[,1],naref=covars.ref))
			spdef.dt<-spdef.dt[,def:=ifelse(is.na(naref),NA,
							ifelse(nwi>=100,100,
								ifelse((nwi+cdl)>=100,100,
										ifelse((nwi+cdl+nlcd)>=100,100,(nwi+cdl+nlcd)))))]
			
			#create the 5% and 1% masks
			maskval05<-ifelse(spdef.dt$def>5,1,0);maskval01<-ifelse(spdef.dt$def>1,1,0)
			
			#need to save the df version for re-running the models with masked data
			mask05df<-data.frame(cellId=1:(NROW(maskval05)),maskval=maskval05)
			save(mask05df,file=paste0(rpth,"mask/speciesMasks/Mask05tables/",x,"_mask05.RData"))
			
			#create the raster masks
			mask05<-baserast;mask05[]<-maskval05;
			mask01<-baserast;mask01[]<-maskval01;
			writeRaster(mask01,filename=paste(rpth,"/mask/speciesMasks/",x,"_mask01.tif",sep=""),format="GTiff",overwrite=TRUE);
			writeRaster(mask05,filename=paste(rpth,"/mask/speciesMasks/",x,"_mask05.tif",sep=""),format="GTiff",overwrite=TRUE)
		},covars.mx=covars.mx,maskdef=maskdef,baserast=baserast,covars.ref=covars.ref)


