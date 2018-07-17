# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("raster","rgdal","plyr","data.table","rgeos","sp")
lapply(libs, require, character.only = TRUE)

###############################
getVectorInfo<-function(cdf,crsinfo,vect,crsvect,fields){ #buff
	gdf<-cdf
	basefields<-names(cdf)
	coordinates(gdf)<-c("x","y")
	proj4string(gdf) <- CRS(crsinfo) 
	gdfp<-spTransform(gdf,CRS=crsvect)
	gdf<-over(gdfp,vect)
	#select the fields we want only: fields
	gdfdt<-data.table(as.data.frame(gdf))
	cdfdt<-data.table(cbind(cdf,gdfdt))
	cdfcols<-c(basefields,fields)
	cdfdt<-cdfdt[,cdfcols,with=FALSE]
	return(cdfdt)
}

gpth<-"/home/lsalas/adalo/"
fls<-list.files(paste(gpth,"grid30",sep=""),pattern="seg_")
NROW(fls)==497
load(paste(gpth,"grid30/gridProjData.RData",sep=""))  #this loads gridcrsinfo for the points

#filter each by R6&8 polygons, attributing with R8 or R6
r68<-readOGR(paste(gpth,"Region68polys",sep=""),"r68polygonsAEA")
r68proj<-projection(r68)
r68fields<-"REGION_1"

#attribute with PADUS
padus<-readOGR(paste(gpth,"PADUS_R68",sep=""),"padus_r68")
padproj<-projection(padus)
padfields<-c("Mang_Type","Mang_Name","Des_Tp","Unit_Nm")

#will need this too
dd<-seq(1,4961676868,by=10000000);dd<-c(dd,4961676868+1)

for(ff in fls[2:NROW(fls)]){
	load(paste(gpth,"grid30/",ff,sep=""))	#loads xy30
	#convert to data.frame
	xy30<-data.table(xy30)
	#assing cell Ids based on segment: id, x, y
	dv<-as.integer(substr(ff,5,regexpr(".R",ff,fixed=T)-1))
	xy30[,id:=seq(dd[dv],dd[dv+1]-1)]
	regdt<-getVectorInfo(cdf=xy30,crsinfo=gridcrsinfo,vect=r68,crsvect=r68proj,fields=r68fields)
	#this is a data.table - filter for !is.na(REGION_1)
	regdt<-subset(regdt,!is.na(REGION_1))
	rm(list="xy30");gc();
	#apply PADUS
	if(nrow(regdt)>0){
		#split, over, and save regdt
		sects<-seq(1,nrow(regdt),by=10000);sects<-c(sects,nrow(regdt)+1)
		for(pp in 1:(NROW(sects)-1)){
			regp<-regdt[c(sects[pp]:sects[pp+1]-1),]
			padus30temp<-getVectorInfo(cdf=regp,crsinfo=gridcrsinfo,vect=padus,crsvect=padproj,fields=padfields)
			rm(list="regp");gc();
			#write to file
			save(padus30temp,file=paste("/home/lsalas/adalo/padus30temp/temp_",pp,"_padus30.RData",sep=""))
			rm(list="padus30temp");gc();
		}
		rm(list="regdt");gc();	#remove this before loading the temps into list
		reglst<-list()
		for(pp in 1:(NROW(sects)-1)){
			fn<-paste("/home/lsalas/adalo/padus30temp/temp_",pp,"_padus30.RData",sep="")
			if(file.exists(fn)){
				load(fn)
				reglst[[pp]]<-padus30temp
				rm(list="padus30temp");gc();
			}
		}
		padus30<-rbindlist(reglst,use.names=T)
		#write to file
		save(padus30,file=paste("/home/lsalas/adalo/padus30/padus_",ff,sep=""))
		rm(list="padus30");gc();
		for(pp in 1:(NROW(sects)-1)){
			fn<-paste("/home/lsalas/adalo/padus30temp/temp_",pp,"_padus30.RData",sep="")
			if(file.exists(fn)){file.remove(fn)}
		}
	}
}


#

