# TODO: Add comment
# 
# Author: lsalas & orichmond
###############################################################################

libs<-c("RODBC","ggplot2","plyr","dplyr","tidyr","reshape2","qcc","ggQC","gridExtra")
lapply(libs, require, character.only = TRUE)

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

#DEFINITIONS
#Area = Land ownership category

#sumCells = total number of 30-m cells within the protected area or manager

#wgtSumMetric = the weighted sum of the metric. Each intersect (each row in the warehouse table) has the padus category value, encounter rate at 990-m, and number of 30-m cells in it. Think of the wgtSumMetric as if each 30-m within the intersect is a vote on the encounter rate, and we are calculating the tally of votes across all intersects (i.e., all 990-m cells that intersect with the protected area. So, for each intersect we multiply the encounter rate x number of 30-m cells, and then add these up across all rows.

#wgtDensity = wgtSumMetric/sumCells (this is the average vote of each 30-m cell)

#hectareDensity = estimated encounter rate of birds per hectare

#wgtAbundance = wgtDensity x number of 990-m cells, but because number of 990-m cells is sumCells/1089, you can see that wgtAbundance is also wgtSumMetric/1089. Because this is a weighted sum of the average encounter rate at 990-m, it is really an Index of Abundance

#AreaSizeHa = Each protected area exists in the warehouse as the sum of the 30-m cells that are within it. So, then: sumCells x area of a 30-m cell / 10,000, or sumCells x 900/10,000

#MakeQuestion gets you the raw data; makeContrast generates a simpler table with area or species as the rows (byComp), or a plot of density or abundance

#By and byComp are the same thing, named differently in functions to avoid closure errors (something that has to do with passing arguments by reference through functions in a stack. Is that confusing? Should I change it so it’s the same across?
#Res and reserr are just two examples of questions. You could name the same, or A and B, or…

########################################################################
## Questions --

# Q1: Compare two regions (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(byComp="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge','Sacramento River National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="table")
makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="abund",outt="table")
p<-makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="plot"); print(p)
p<-makeContrast(res,byComp="area",outp="abund",outt="plot"); print(p)
#get percent area surveyed
makeContrast(res,reportAreaSurv=TRUE,byComp="area",outt="table")


# Q2: Compare 4 species of birds in relation to use of some regions (Modoc and Valentine refuges)
res<-makeQuestion(byComp="species",metric=4,period=NA,species=c("buow","lbcu","nopi","trbl"),padusCat="unitName",catValues=c('Valentine National Wildlife Refuge','Modoc National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)
makeContrast(res,reportAreaSurv=FALSE,byComp="species",outp="dens",outt="table")
makeContrast(res,reportAreaSurv=FALSE,byComp="species",outp="abund",outt="table")
p<-makeContrast(res,reportAreaSurv=FALSE,byComp="species",outp="dens",outt="plot"); print(p)
p<-makeContrast(res,reportAreaSurv=FALSE,byComp="species",outp="abund",outt="plot"); print(p)

# Q3: compare this refuge vs a jurisdictional domain - San Pablo Bay vs the rest in Region 8
res<-makeQuestion(byComp="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)
makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="table")
p<-makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="plot"); print(p)
#no sense in comparing relative abundances for this question

# Q3-alt: compare this refuge vs a jurisdictional domain - San Pablo Bay vs the rest in Region 8, but this time for the breeding season (no canv in SPBNWR dring breeding season)
reserr<-makeQuestion(byComp="area",metric=4,period=1,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)
#not reporting values for SPB because there are no data ffor CANV in breeding at SPB
makeContrast(reserr,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="table")
print(reserr)

# Q4: compare jurisdictions (R6 vs R8) for a species (canv in winter, using metric 5)
res<-makeQuestion(byComp="area",metric=5,period=0,species="canv",padusCat=NA,catValues=NA,geopolCat="USFWSregion",geopolValues=c(6,8))
makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="table")
makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="abund",outt="table")
p<-makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="dens",outt="plot"); print(p)
p<-makeContrast(res,reportAreaSurv=FALSE,byComp="area",outp="abund",outt="plot"); print(p)

# Q5: compare all manager types within FWSregion 8, and vs the entire region 8, for canv during breeding season
res<-makeQuestion(by="area",metric=4,period=1,species="canv",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)

# NOTE that the abundance for R8 is the same calculated in Q3-alt
subset(res,Area=="USFWSregion 8",select="wgtAbundance")

subset(reserr$rawdata,Area=="USFWSregion 8",select="wgtAbundance")

# Here is an example that does not enforce the domain:
# Q6: compare San Pablo Bay vs the entire R6 for canv over winter.
res<-makeQuestion(by="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6,geopolRestrict=FALSE)
#No density for R6?
res$tbldens; res$pltdens


# Q7: Compare two regions (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(byComp="area",metric=4,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res$Manager<-getManagerName(as.character(res$Area))




###ADALO REPORT QUESTIONS

# Q1: compare all manager types within FWSregion 6, and vs the entire region 6, for bais during breeding season

#EMPIRICAL
res<-makeQuestion(by="area",metric=4,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res


#Bar Chart - Empirical Density Index
res$wgtDensity <- round(res$wgtDensity, digits=5)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$wgtDensity[-length(res$Area)])

# Render Pareto Plot
p1_adi <- ggplot(df, aes(x=reorder(x,-y), y=y)) + geom_bar(fill = c("dark blue"),stat="identity") + geom_text(aes(label=y, y=y+max(res$wgtDensity)*0.05),position = position_dodge(0.9),vjust = 0,size=2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("Average Density Index") + annotate("text", x = 18, y = max(res$wgtDensity)*0.9, label = "Empirical",size=5)
p1_adi


#Pareto Chart - Empirical Abundance Index
# Setup Data
res$relAbundance <- round(res$relAbundance, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relAbundance[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("light blue")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relAbundance)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Abundance Index") + annotate("text", x = 18, y = max(res$relAbundance)*0.9, label = "Empirical",size=5)
p1


#BASE MODEL
res<-makeQuestion(by="area",metric=5,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res

#First, how is land ownership distributed in Region 6.

#Pareto Chart - Land Ownerships by Area
res$relArea <- res$AreaSizeHA/res$AreaSizeHA[31]*100
res$relArea <- round(res$relArea, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relArea[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("light green")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relArea)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Area")
p1


#Bar Chart - Base Model Density Index
res$wgtDensity <- round(res$wgtDensity, digits=2)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$wgtDensity[-length(res$Area)])

# Render Pareto Plot
p1_adi <- ggplot(df, aes(x=reorder(x,-y), y=y)) + geom_bar(fill = c("dark blue"),stat="identity") + geom_text(aes(label=y, y=y+max(res$wgtDensity)*0.05),position = position_dodge(0.9),vjust = 0,size=2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("Average Density Index") + annotate("text", x = 18, y = max(res$wgtDensity)*0.9, label = "Base Model",size=5)
p1_adi


#Pareto Chart - Base Model Abundance Index
# Setup Data
res$relAbundance <- round(res$relAbundance, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relAbundance[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("light blue")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relAbundance)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Abundance Index") + annotate("text", x = 18, y = max(res$relAbundance)*0.9, label = "Base Model",size=5)
p1


#HAPET MODEL
res<-makeQuestion(by="area",metric=6,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res


#Pareto Chart - HAPET Model Abundance Index
# Setup Data
res$relAbundance <- round(res$relAbundance, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relAbundance[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("dark green")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relAbundance)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Occurrence Index") + annotate("text", x = 18, y = max(res$relAbundance)*0.9, label = "HAPET Model",size=5)
p1


# Q2: compare all manager types within FWSregion 8, and vs the entire region 8, for trbl during breeding season

#EMPIRICAL
res<-makeQuestion(by="area",metric=4,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
res


#Bar Chart - Empirical Density Index
res$wgtDensity <- round(res$wgtDensity, digits=5)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$wgtDensity[-length(res$Area)])

# Render Pareto Plot
p1_adi <- ggplot(df, aes(x=reorder(x,-y), y=y)) + geom_bar(fill = c("dark blue"),stat="identity") + geom_text(aes(label=y, y=y+max(res$wgtDensity)*0.05),position = position_dodge(0.9),vjust = 0,size=2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("Average Density Index") + annotate("text", x = 18, y = max(res$wgtDensity)*0.9, label = "Empirical",size=5)
p1_adi


#Pareto Chart - Empirical Abundance Index
# Setup Data
res$relAbundance <- round(res$relAbundance, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relAbundance[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("light blue")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relAbundance)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Abundance Index") + annotate("text", x = 18, y = max(res$relAbundance)*0.9, label = "Empirical",size=5)
p1


#BASE MODEL
res<-makeQuestion(by="area",metric=5,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
res

#First, how is land ownership distributed in Region 8.

#Pareto Chart - Land Ownerships by Area
res$relArea <- res$AreaSizeHA/res$AreaSizeHA[length(res$Area)]*100
res$relArea <- round(res$relArea, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relArea[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("light green")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relArea)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Area")
p1


#Bar Chart - Base Model Density Index
res$wgtDensity <- round(res$wgtDensity, digits=2)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$wgtDensity[-length(res$Area)])

# Render Pareto Plot
p1_adi <- ggplot(df, aes(x=reorder(x,-y), y=y)) + geom_bar(fill = c("dark blue"),stat="identity") + geom_text(aes(label=y, y=y+max(res$wgtDensity)*0.05),position = position_dodge(0.9),vjust = 0,size=2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("Average Density Index") + annotate("text", x = 18, y = max(res$wgtDensity)*0.9, label = "Base Model",size=5)
p1_adi


#Pareto Chart - Base Model Abundance Index
# Setup Data
res$relAbundance <- round(res$relAbundance, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relAbundance[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("light blue")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relAbundance)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Abundance Index") + annotate("text", x = 18, y = max(res$relAbundance)*0.9, label = "Base Model",size=5)
p1


#POINT BLUE MODEL
res<-makeQuestion(by="area",metric=7,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
res


#Pareto Chart - Point Blue Model Occurrence Index
# Setup Data
res$relAbundance <- round(res$relAbundance, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relAbundance[-length(res$Area)])

# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black",#size.line = 1,
		bars.fill = c("dark green")) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relAbundance)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Total Occurrence Index") + annotate("text", x = 18, y = max(res$relAbundance)*0.9, label = "Point Blue Model",size=5)
p1
