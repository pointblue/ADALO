# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(RODBC); library(ggplot2); library(plyr)

source("/home/adalouser/ADALO/phaseII/QueryAndViz/queryAdaloUtils.R")

#source("c:/users/lsalas/git/ADALO/phaseII/QueryAndViz/queryAdaloUtils.R")
###########################################################################################
##################  Make a question for the ADALO tool  ###################################
###########################################################################################

## OVERVIEW:
## There are two types of questions to ask - by species or by area: 
# 1) BY SPECIES - We want to know how two or more species compare within a PADUS category's domain (e.g., within a refuge), 
#	(think of a table where the columns are refuges and the rows are species, each cell reporting the abundance of the species)
#	Example question: which species should be targeted for management in refuge X?
# 2) BY AREA - We want to know how one or more species compare across domains options 
#	(in the hypothetical table, the columns are one or more species and the rows are the spatial domains). 
#	The domain comparisons come in three options"
#		2a) Compare species between two or more members of a PADUS category
#			The following categories exist: mgmtType, mgrName, desType, unitName 
#			(example mgmtType=FED, mgrName=FWS, desType=National Widlife Refuge, unitName=San Pablo Bay National Wildlife Refuge)
#			Example questions: compare species X between FWS and NPS, or between two refuges, or between mgmtType=FED and all others
#		2b) Compare species between a PAUDS Unit and a geopolitical region 
#			The following geopolitical regions exist: FWSregion, NPSregion, USFSregion, LCCregion, USJVregion, BCR, State, and County.
#			Example questions: How important is SPBNWR for CANV with relation to the entire SFBJV or the entire BCR or Region 8?
#		2c) Compare species between geopolitical regions
#			Example question: Which agency (FWS or DOD) is relatively more important (i.e. manages more individuals) for TRBL?

########################################################################
## Questions --

# Q1: Compare two areas (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(by="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge','Sacramento River National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$tblabund; res$tblsurv
res$pltabun; dev.new();res$pltsurv

# Q2: Compare 4 species of birds in relation to use of some areas (Modoc and Valentine refuges)
res<-makeQuestion(by="species",metric=4,period=NA,species=c("buow","lbcu","nopi","trbl"),padusCat="unitName",catValues=c('Valentine National Wildlife Refuge','Modoc National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$tblabund; res$tblsurv
res$pltabun; dev.new();res$pltsurv

# Q3: compare this refuge vs a jurisdictional domain - San Pablo Bay vs the rest in Region 8
res<-makeQuestion(by="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)
res$tbldens; res$pltdens

# Q3-alt: compare this refuge vs a jurisdictional domain - San Pablo Bay vs the rest in Region 8, but this time for the breeding season (no canv in SPBNWR dring breeding season)
reserr<-makeQuestion(by="area",metric=4,period=1,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)
reserr$tbldens; reserr$error; reserr$rawdata

# Q4: compare jurisdictions (R6 vs R8) for a species (canv in winter, using metric 5)
res<-makeQuestion(by="area",metric=5,period=0,species="canv",padusCat=NA,catValues=NA,geopolCat="USFWSregion",geopolValues=c(6,8))
res$tbldens; res$pltdens
res$tblabund; res$pltabun

# Q5: compare a bunch of manager types within FWSregion 8, and vs the entire region 8, for canv during breeding season
res<-makeQuestion(by="area",metric=4,period=1,species="canv",padusCat="mgrName",
		catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),
		geopolCat="USFWSregion",geopolValues=8)
res$tblabun
res$rawdata
# NOTE that the abundance for R8 is the same calculated in Q3-alt
subset(res$tblabun,Area=="USFWSregion 8",select="canv")
subset(reserr$rawdata,Area=="USFWSregion 8",select="estAbundance")

# Here is an example that does not enforce the domain:
# Q6: compare San Pablo Bay vs the entire R6 for canv over winter.
res<-makeQuestion(by="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6,geopolRestrict=FALSE)
res$tbldens; res$pltdens


#################################################################
#ADALO Report Questions
# Q1: How does land ownership reliance (LOR) for BAIS, CCLO and SPPI in USFWS Region 6 differ between Refuge lands vs. all other land managers (state and federal agencies, cities, private) during the breeding season?

#First, BAIS
metrics <- c(4,5,6)
periods <- c(1)
regions <- c(6)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
res <- makeQuestion(by="area",metric=metrics[i],period=periods[j],species="bais",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=regions[k])
result <- rbind(result,res$rawdata)
    }
  }
}
result


#TEST
res <- makeQuestion(by="area",metric=6,period=1,species="bais",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=6)

res$rawdata


# Q1a: How does land ownership reliance (LOR) for BAIS in USFWS Region 6 differ among refuges during the breeding season?
res<-makeQuestion(by="area",metric=5,period=1,species="cclo",padusCat="unitName",catValues=c('Benton Lake National Wildlife Refuge','Halfbreed Lake National Wildlife Refuge','Hailstone National Wildlife Refuge','Lake Mason National Wildlife Refuge','Charles M. Russell National Wildlife Refuge','Bowdoin National Wildlife Refuge','Hewitt Lake National Wildlife Refuge','Black Coulee National Wildlife Refuge','Medicine Lake National Wildlife Refuge','Lake Zahl National Wildlife Refuge','Lostwood National Wildlife Refuge','Lake Ilo National Wildlife Refuge','Des Lacs National Wildlife Refuge','Upper Souris National Wildlife Refuge','J. Clark Salyer National Wildlife Refuge','Audubon National Wildlife Refuge','Shell Lake National Wildlife Refuge','Lake Nettie National Wildlife Refuge','Florence Lake National Wildlife Refuge','Mclean National Wildlife Refuge','Wintering River National Wildlife Refuge','Lake Alice National Wildlife Refuge','Sullys Hill National Game Preserve','Arrowwood National Wildlife Refuge','Chase Lake National Wildlife Refuge','Slade National Wildlife Refuge','Long Lake National Wildlife Refuge','Kellys Slough National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$rawdata

res_sort <- res$rawdata[order(-res$rawdata$estAbundance),] 


#Next, CCLO
metrics <- c(4,5,6)
periods <- c(1)
regions <- c(6)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="region",metric=metrics[i],period=periods[j],species="cclo",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result


#Next, SPPI
metrics <- c(4,5,6)
periods <- c(1)
regions <- c(6)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="region",metric=metrics[i],period=periods[j],species="sppi",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result


# Q2: Compare manager name (e.g., USFWS, NPS, BLM, private) in importance to TRBL during the breeding and wintering seasons
metrics <- c(4,5,7)
periods <- c(1)
regions <- c(8)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="region",metric=metrics[i],period=periods[j],species="trbl",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result


# Q2a: How does land ownership reliance (LOR) for TRBL in USFWS Region 8 differ among refuges during the breeding season?
res<-makeQuestion(by="area",metric=5,period=0,species="trbl",padusCat="unitName",catValues=c('Klamath Marsh National Wildlife Refuge','Upper Klamath National Wildlife Refuge','Lower Klamath National Wildlife Refuge','Tule Lake National Wildlife Refuge','Clear Lake National Wildlife Refuge','Bear Valley National Wildlife Refuge','Modoc National Wildlife Refuge','Humboldt Bay National Wildlife Refuge','Sacramento National Wildlife Refuge','Delevan National Wildlife Refuge','Colusa National Wildlife Refuge','Sacramento River National Wildlife Refuge','Sutter National Wildlife Refuge','Stone Lakes National Wildlife Refuge','San Joaquin River National Wildlife Refuge','San Luis National Wildlife Refuge','Merced National Wildlife Refuge','Don Edwards San Francisco Bay National Wildlife Refuge','San Pablo Bay National Wildlife Refuge','Antioch Dunes National Wildlife Refuge','Ellicott Slough National Wildlife Refuge','Salinas River National Wildlife Refuge','Marin Islands National Wildlife Refuge','Kern National Wildlife Refuge','Pixley National Wildlife Refuge','Guadalupe-Nipomo Dunes National Wildlife Refuge','Bitter Creek National Wildlife Refuge','Blue Ridge National Wildlife Refuge','Hopper Mountain National Wildlife Refuge','Coachella Valley National Wildlife Refuge','Seal Beach National Wildlife Refuge','San Diego Bay National Wildlife Refuge','Tijuana Slough National Wildlife Refuge','San Diego National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$rawdata

res_sort <- res$rawdata[order(-res$rawdata$estAbundance),] 




# Q3: Compare manager name (e.g., USFWS, NPS, BLM, private) in importance to waterfowl (CANV and NOPI) during the breeding season in Region 6.
metrics <- c(5)
periods <- c(1)
regions <- c(8)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="region",metric=metrics[i],period=periods[j],species="nopi",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result


# Q3b: Compare manager name (e.g., USFWS, NPS, BLM, private) in importance to waterfowl (CANV and NOPI) during the wintering season in Region 8.
metrics <- c(4,5,6)
periods <- c(0)
regions <- c(8)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="region",metric=metrics[i],period=periods[j],species="nopi",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result


# Q3c: Compare manager name (e.g., USFWS, NPS, BLM, private) in importance to waterfowl (CANV and NOPI) during the wintering season in California.
metrics <- c(4,5,6)
periods <- c(0)
regions <- c("06")
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="area",metric=metrics[i],period=periods[j],species="nopi",padusCat="mgrName",catValues=c('FWS','BLM','USFS','NPS','BIA','TRIB','NOAA','USACE','USBR','ARS','DOD','DOE','NRCS','OTHF','JNT','SDC','SDNR','SDOL','SFW','SLB','SPR','OTHS','NGO','CITY','CNTY','REG','RWD','PVT','UNK','UNKL'),geopolCat="StateFIPS",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result



# Q4a: Compare multiple species of birds in relation to use of some regions (Charles M. Russell National Wildlife Refuge)

metrics <- c(4,5)
periods <- c(1)
regions <- c(6)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="species",metric=metrics[i],period=periods[j],species=c("bais","bobo","buow","canv","cclo","feha","lbcu","mago","mopl","nopi","sacr","sppi"),padusCat="unitName",catValues=c('Charles M. Russell National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result


# Q4b: Compare multiple species of birds in relation to use of some regions (Sacramento National Wildlife Refuge)

metrics <- c(4,5)
periods <- c(0)
regions <- c(8)
#Initialize data frame
result <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Area","estDensity","ncells","estAbundance","species","metric")
colnames(result) <- x
#result <- NA

#Populate data frame with the runs you want
for (i in 1:length(metrics)){
  for (j in 1:length(periods)){
    for (k in 1:length(regions)){
      res <- makeQuestion(by="species",metric=metrics[i],period=periods[j],species=c("buow","canv","lbcu","mago","mopl","nopi","sacr","trbl"),padusCat="unitName",catValues=c('Sacramento National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=regions[k])
      result <- rbind(result,res$rawdata)
    }
  }
}
result




# Q3: compare this refuge vs a jurisdictional domain - San Pablo Bay vs the rest in Region 8
res<-makeQuestion(by="region",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)
res$tbldens; res$pltdens

# Q4: compare jurisdictions (R6 vs R8) for a species (canv in winter, using metric 5)
res<-makeQuestion(by="region",metric=5,period=0,species="canv",padusCat=NA,catValues=NA,geopolCat="USFWSregion",geopolValues=c(6,8))
res$tbldens; res$pltdens
res$tblabund; res$pltabun






