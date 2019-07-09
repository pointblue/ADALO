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
## There are two types of questions to ask - here categorized as queries by species or by region: 
# 1) QUERIES BY SPECIES - We want to know how two or more species compare within a PADUS category's domain (e.g., within a refuge), 
#	(think of a table where the columns are refuges and the rows are species, each cell reporting the abundance of the species)
#	Example question: which species should be targeted for management in refuge X? A comparison among species within a refuge or 
#	collection of refuges will help answer the question
# 2) QUERIES BY AREA - We want to know how one or more species compare across spatial domains 
#	(in the hypothetical table, the columns are one or more species and the rows are the spatial domains). 
#	The domain comparisons come in three options:
#		2a) Compare species between two or more members of a PADUS category
#			The following PADUS categories exist in the warehouse: mgmtType, mgrName, desType, unitName (others can be added later)
#			(example mgmtType=FED, mgrName=FWS, desType=National Widlife Refuge, unitName=San Pablo Bay National Wildlife Refuge)
#			Example questions: how species X' density compares between FWS and NPS, or between two refuges, or between mgmtType=FED and all other management types
#		2b) Compare species between a PAUDS Unit and a geopolitical region 
#			The following geopolitical regions exist in the warehouse: FWSregion, NPSregion, USFSregion, LCCregion, USJVregion, BCR, State, and County.
#			Example questions: How important is SPBNWR for CANV with relation to the entire SFBJV or the entire BCR or Region 8?
#		2c) Compare species between geopolitical regions
#			Example question: Which agency (FWS or DOD) is relatively more important (i.e. manages more individuals) for TRBL?

## DEFINITIONS OF TABLE OF RESULTS 
# The following are the columns in the table returned after making a question to the tool with makeQuestion:

# Area				The land ownership category
# Species			The species code of the species queried
# Metric			The metric being requested: 4, 5, 6 or ...
# sumCells			The total number of 30-m cells within the protected area or manager
# wgtSumMetric		The weighted sum of the metric. 
# 			Each intersect (each row in the warehouse table) has the padus category value, encounter rate at 990-m, and number of 30-m cells in it. 
#    		So, for each intersect we multiply the encounter rate x number of 30-m cells in it, and then add these up across all rows returned.
#			It is thus the sum of values of the metric across all 30-m cells queried
# wgtDensity		This is wgtSumMetric/sumCells (the weighted average density)
# hectareDensity	The estimated encounter rate of birds per hectare
# wgtAbundance 		wgtDensity x number of 990-m cells, but because number of 990-m cells is sumCells/1089, you can see that wgtAbundance is also wgtSumMetric/1089. 
#    		Because this is a weighted sum of the average encounter rate at 990-m, it is really an Index of Abundance.
# AreaSizeHa 		The area of the PADUS category being reported in the domain queried.
#			Each protected area exists in the warehouse as the sum of the 30-m cells that are within it. So, then it is: sumCells x area of a 30-m cell / 10,000, 
#    		or sumCells x 900/10,000
# relAbundance 		The percent of the total estimated abundance (where the total is the sum of the weighted abundances across the domain queried) 
#    		A value between 0-100. So, if the domain queried is 6 different refuges, total abundance is the sum of the weighted abundances of those 6 refuges, and
#    		relAbundance for a refuge will be its percent contribution to the total.

## PARAMETERS PASSED TO makeQuestion FUNCTION
# MakeQuestion is the function that gets you the raw data
# byComp 			Either species or area (default), indicating that either different species or different areas will make the rows of the output table
# metric			An integer: 4 (empirical), 5 (ADALO sdm), 6 (HAPET sdm) or 7 (POINT BLUE sdm). Defaults to empirical (4)
# period			Either NA, 0 or 1 (all, winter or breeding, respectively). Defaults to all
# species			Mandatory; a vector of one or more species to consider for the comparisons
# padusCat			The name of the PADUS category against which to develop contrasts: mgmtType, mgrName, desType, or unitName
# catValues			The PADUS values to contrast
# geopolCat 		Names one of the geopolitical domains to query for data: USFWSregion, USFSregion, NPSregion, LCCregion, USJVregion, BCRregion, StateFIPS, or CountyFIPS
# geopolValues 		The values to filter for the geopolField (e.g., 6 or 8 for USFWSregion) - this is an integer
# geopolRestrict	A boolean indicating that the question is restricted to the domain defined by the geopolCats

## PARAMETERS PASSED TO makeContrast FUNCTION
# makeContrast is a function that generates a simpler table with area or species as the rows (byComp), or a plot of density or abundance
# domdf 			is the result data.frame from makeQuestion
# reportAreaSurv 	is a boolean indicating if the percent area surveyed should be reported. Matters only for metric 4 (empirical). Default FAlSE
# byComp 			is a string (either: area or species) indicating how to construct the comparison. It assumes one or the other has more levels, thus gives it row priority:
#	if the string is "area" each row in the table is a different padus/geopolitical level, each column is a species, and cell value is indicated with outp. This is the default.
#	if the string is "species" each row is a species, each column a different area, and cell value is as prescribed in outp
# outp 				is either dens (density) or abund (abundance index) - the content of cells in the output table
# outt 				indicates if to return a table or plot: string table or plot. Defaults to "table"


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
res<-makeQuestion(byComp="area",metric=4,period=1,species="canv",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)

# NOTE that the abundance for R8 is the same calculated in Q3-alt
subset(res,Area=="USFWSregion 8",select="wgtAbundance")

subset(reserr$rawdata,Area=="USFWSregion 8",select="wgtAbundance")

# Here is an example that does not enforce the domain:
# Q6: compare San Pablo Bay vs the entire R6 for canv over winter.
res<-makeQuestion(byComp="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6,geopolRestrict=FALSE)
#No density for R6?
res$tbldens; res$pltdens


# Q7: Compare two regions (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(byComp="area",metric=4,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res$Manager<-getManagerName(as.character(res$Area))




###ADALO REPORT QUESTIONS

# Q1: compare all manager types within FWSregion 6, and vs the entire region 6, for bais during breeding season

#EMPIRICAL
res<-makeQuestion(byComp="area",metric=4,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
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
res<-makeQuestion(byComp="area",metric=5,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
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
res<-makeQuestion(byComp="area",metric=6,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
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
res<-makeQuestion(byComp="area",metric=4,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
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
res<-makeQuestion(byComp="area",metric=5,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
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
res<-makeQuestion(byComp="area",metric=7,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
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
