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
## There are two types of questions to ask - by species or by region: 
# 1) BY SPECIES - We want to know how two or more species compare within a PADUS category's domain (e.g., within a refuge), 
#	(think of a table where the columns are refuges and the rows are species, each cell reporting the abundance of the species)
#	Example question: which species should be targeted for management in refuge X?
# 2) BY REGION - We want to know how one or more species compare across domains options 
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

# Q1: Compare two regions (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(by="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge','Sacramento River National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
res$tblabund; res$tblsurv
res$pltabun; dev.new();res$pltsurv

# Q2: Compare 4 species of birds in relation to use of some regions (Modoc and Valentine refuges)
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
# NOTE that the abundance for R8 is the same calculated in Q3-alt
subset(res$tblabun,Area=="USFWSregion 8",select="canv")
subset(reserr$rawdata,Area=="USFWSregion 8",select="estAbundance")

# Here is an example that does not enforce the domain:
# Q6: compare San Pablo Bay vs the entire R6 for canv over winter.
res<-makeQuestion(by="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6,geopolRestrict=FALSE)
res$tbldens; res$pltdens


