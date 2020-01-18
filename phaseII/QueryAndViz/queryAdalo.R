# ***AVIAN DISTRIBUTIONS ACROSS LAND OWNERSHIPS (ADALO)***

# Date: January 13, 2020

# Authors: 
# Leo Salas, Point Blue Conservation Science, lsalas@pointblue.org
# Nadav Nur, Point Blue Conservation Science, nnur@pointblue.org
# Orien M. W. Richmond, U.S. Fish & Wildlife Service, orien_richmond@fws.gov
# 

# NOTE: to soft-wrap the R code in this window, go to: 
# Tools > Global Options > Code Editing > Soft-wrap R source files

###############################################################################

libs<-c("RODBC","ggplot2","plyr","dplyr","tidyr","reshape2","qcc","ggQC","gridExtra")
lapply(libs, require, character.only = TRUE)
source("/home/adalouser/ADALO/phaseII/QueryAndViz/queryAdaloUtils.R")

###########################################################################################
##################  Make a question for the ADALO tool  ###################################
###########################################################################################

###INTRODUCTION TO ADALO ----
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

###ADALO DEFINITIONS ----
## PARAMETERS PASSED TO makeQuestion FUNCTION
# MakeQuestion is the function that gets you the raw data in a table format.

# byComp = A string (either "area" or "species") indicating how to construct the comparison. It assumes one or the other has more levels, thus gives it row priority. If the string is "area," each row in the table is a different PAD-US/geopolitical value, each column is a species, and cell value is indicated with outp. This is the default. If the string is "species," each row is a species, each column is different PAD-US/geoplitical value, and cell value is as prescribed in outp.

#metric = The encounter rate or occurrence metric to be used in the query. 4 = empirical encounter rate; 5 = base model encounter rate; 6 = HAPET predicted occurrence; 7 = Point Blue predicted occurrence. Defaults to empirical encounter rate (4).

#period = Either NA, 0 or 1 (all, winter or breeding, respectively). Defaults to all.

#species = Mandatory; a vector of one or more species to consider for the comparisons. Possible values in the current version of ADALO include: bais, bobo, buow, canv, cclo, lbcu, mago, mopl, nopi, sacr, sppi, trbl.

#padusCat = The PAD-US attribute for the query. Options include category (e.g., fee or easement), desType (designation type; e.g., National Wildlife Refuge, National Park), mgrName (manager name; e.g., FWS, USFS, State Fish and Game, City Land, TNC), mgmtType (manager type; e.g., Federal, Tribal, State, Private), and unitName (unit name; e.g., Rocky Mountain Arsenal National Wildlife Refuge).

# catValues = The a vector of one or more specific PAD-US values for the comparison (e.g., catValues=c('San Pablo Bay National Wildlife Refuge','Sacramento River National Wildlife Refuge'))

# geopolCat 		Names one of the geopolitical domains to query for data: USFWSregion, USFSregion, NPSregion, LCCregion, USJVregion, BCRregion, StateFIPS, or CountyFIPS
# geopolValues 		The values to filter for the geopolField (e.g., 6 or 8 for USFWSregion) - this is an integer
# geopolRestrict	A boolean indicating that the question is restricted to the domain defined by the geopolCats


### OUTPUT VARIABLES FROM RESULT OF makeQuestion FUNCTION ----
# Here are variable definitions for the variables that appear in the result of a makeQuestion function.

#Area = The PAD-US attribute value (e.g., manager name(s), unit name(s), etc.) or geopolitical domain (e.g., USFWS Region 6).

#species = Bird species code(s).

#metric = The encounter rate metric (or predicted occurrence metric) that was passed to the makeQuestion function.

#sumCells = The total number of 30-m x 30-m cells for the corresponding PAD-US attribute value (Area), species, and metric.

#wgtSumMetric = The weighted sum of encounter rate (or predicted occurrence) for the corresponding PAD-US value(Area). Encounter rate (or predicted occurrence) at the 990-m scale is weighted by the number of 30-m cells belonging to different PAD-US values. Each intersect (each row in the warehouse table) has the PAD-US attribute value, encounter rate or predicted occurrence at 990-m, and number of 30-m cells. Each 30-m cell within the intersect is like a vote on the encounter rate or predicted occurence, and we are calculating the tally of votes across all intersects (i.e., all 990-m cells that intersect with the PAD-US value). So, for each intersect we multiply the encounter rate (or predicted occurrence) x the number of 30-m cells for the PAD-US value, and then add these up across all rows. It is thus the sum of encounter rate (or predicted occurrence) across all 30-m cells queried for a given PAD-US value.

#wgtDensity = wgtSumMetric/sumCells. The average encounter rate (or predicted occurrence) per 30-m cell for the corresponding PAD-US value.

#hectareDensity = The average encounter rate (or predicted occurrence) per hectare for the corresponding PAD-US value.

#wgtAbundance = wgtDensity x number of 990-m cells, but because the number of 990-m cells is sumCells/1089, you can see that wgtAbundance is also wgtSumMetric/1089. Because this is a weighted sum of the average encounter rate at 990-m, it is really an Index of Abundance.

#AreaSizeHa = The area of the PADUS category being reported in the domain queried. Each protected area exists in the warehouse as the sum of the 30-m cells that are within it. So, then: sumCells x area of a 30-m cell / 10,000, or sumCells x 900/10,000

#relAbundance = the percent of wgtAbundance for a land owner category/area with respect to the total wgtAbundance of the categories queried (not necessarily the total abundance of the species in its range). So, for example, in a table returned from makeQuestion you add up all the abundances (wgtAbundance) and that is the total that the percentages are calculated from. A value between 0-100. If the domain queried is for 6 different refuges, total abundance is the sum of the weighted abundances of those 6 refuges, and relAbundance for a refuge will be its % contribution to the total.

#presenceHA =  The area in hectares where the species is predicted (models 5-7) to be present. This is the number of 30m cells with estimated abundance > 0 x 900 m2 / 10,000 m2. 1) presenceHA will always be <= AreaSizeHA; and 2) sum of presenceHA may or may not add up to the total of the range of the species, because the domain queried may not encompass the entirety of the range of the species.

#MakeQuestion gets you the raw data; makeContrast generates a simpler table with area or species as the rows (byComp), or a plot of density or abundance

#By and byComp are the same thing, named differently in functions to avoid closure errors (something that has to do with passing arguments by reference through functions in a stack. Is that confusing? Should I change it so it?s the same across?
#Res and reserr are just two examples of questions. You could name the same, or A and B, or?


## PARAMETERS PASSED TO makeContrast FUNCTION ----
# makeContrast is a function that generates a simpler table with area or species as the rows (byComp), or a plot of density or abundance

# domdf = The resulting data.frame from makeQuestion

# reportAreaSurv = A boolean indicating if the percent area surveyed should be reported. TRUE is appropriate only for metric 4 (empirical encounter rate). Default is FALSE.

# outp = Specifies the content of cells in the output table. Either dens (density) or abund (abundance index).

# outt = Indicates whether to return a table or plot: string table or plot. Defaults to "table."


###############################################################################################

## BASIC QUERIES ----
## Here we describe four types of queries that a user can make using the ADALO tool.


# QUESTION 1: Contribution of a Single Refuge/Refuge Complex ----
#Example: I need to identify priority Resources of Concern (ROCs) for my refuge or refuge complex. One factor that I will consider is the contribution of my refuge/complex to the total abundance index of selected bird species during one or more seasons (breeding, wintering). How do bird species rank in terms of the contribution of my refuge/complex to the total abundance index?

# Example 1a: Charles M. Russell National Wildlife Refuge - Metric 4
res<-makeQuestion(byComp="species",metric=4,period=1,species=c("bais","bobo","buow","canv","cclo","lbcu","mago","mopl","nopi","sacr","sppi"),padusCat="unitName",catValues=c('Charles M. Russell National Wildlife Refuge'))

p<-makePareto(df=res, geopolVal=NA, xvar="species", yvar="avgEncounterRate", barsOnly=TRUE, dataOnly=FALSE, xlabel="Species", transposePlot=TRUE, fillColor="#0571b0", addYVals=TRUE, addNote=NA, highCat=NA, highColor="#ca0020",paretoColor="black"); print(p)


# Example 1b: Charles M. Russell National Wildlife Refuge - Metric 5 (prop of total abundance in R6)
res<-makeQuestion(byComp="species",metric=5,period=1, species=c("bais","bobo","buow","canv","cclo","lbcu","mago","mopl","nopi","sacr","sppi"), padusCat="unitName",catValues=c('Charles M. Russell National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6)

#% Domain-relative abundance index
p<-makePareto(df=res, geopolVal="USFWSregion 6", xvar="species", yvar="totalAbundanceIndex",barsOnly=TRUE, dataOnly=FALSE, xlabel="Species",transposePlot=TRUE,fillColor="#0571b0",addYVals=TRUE, addNote=NA,highCat=NA,highColor="#ca0020",paretoColor="black"); print(p)


#Old method - No comparison to Region
res<-makeQuestion(byComp="species",metric=5,period=1,species=c("bais","bobo","buow","canv","cclo","lbcu","mago","mopl","nopi","sacr","sppi"),padusCat="unitName",catValues=c('Charles M. Russell National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)

tab1 <- makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")
tab1

p<-makeContrast(res,reportAreaSurv=FALSE,by="species",outp="dens",outt="plot"); print(p)

#Old Method - Compare to total abundance for the geoplitical domain (e.g., USFWS region)
res<-makeQuestion(byComp="species",metric=5,period=1,species=c("bais","bobo","buow","canv","cclo","lbcu","mago","mopl","nopi","sacr","sppi"),padusCat="unitName",catValues=c('Charles M. Russell National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")

p<-makeContrast(res,reportAreaSurv=FALSE,byComp="species",outp="abund",outt="plot"); print(p)


# Example 2a: Sacramento National Wildlife Refuge - Metric 4

res<-makeQuestion(byComp="species",metric=4,period=0,species=c("canv","lbcu","mago","mopl","sacr","trbl"),padusCat="unitName",catValues=c('Sacramento National Wildlife Refuge'))

p<-makePareto(df=res, geopolVal=NA, xvar="species", yvar="avgEncounterRate", barsOnly=TRUE, dataOnly=FALSE, xlabel="Species", transposePlot=TRUE, fillColor="#0571b0", addYVals=TRUE, addNote=NA, highCat=NA, highColor="#ca0020",paretoColor="black"); print(p)


#Old method - Metric 4
res<-makeQuestion(byComp="species",metric=4,period=0,species=c("canv","lbcu","mago","mopl","sacr","trbl"),padusCat="unitName",catValues=c('Sacramento National Wildlife Refuge'))

tab1 <- makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")
tab1

p<-makeContrast(res,reportAreaSurv=FALSE,by="species",outp="dens",outt="plot"); print(p)


# Example 2b: Sacramento National Wildlife Refuge - Metric 5 (prop of total abundance in R8)
res<-makeQuestion(byComp="species",metric=5,period=0, species=c("canv","lbcu","mago","mopl","nopi","sacr","trbl"), padusCat="unitName",catValues=c('Sacramento National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8)

#% Domain-relative abundance index
p<-makePareto(df=res, geopolVal="USFWSregion 8", xvar="species", yvar="totalAbundanceIndex",barsOnly=TRUE, dataOnly=FALSE, xlabel="Species",transposePlot=TRUE,fillColor="#0571b0",addYVals=TRUE, addNote=NA,highCat=NA,highColor="#ca0020",paretoColor="black"); print(p)


#Old Method - No comparison to Region
res<-makeQuestion(byComp="species",metric=5,period=0,species=c("canv","lbcu","mago","mopl","nopi","sacr","trbl"),padusCat="unitName",catValues=c('Sacramento National Wildlife Refuge'),geopolCat=NA,geopolValues=NA)

makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")

p<-makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="plot"); print(p)

#Old Method - Compare to total abundance for the geoplitical domain (e.g., USFWS region)
res<-makeQuestion(byComp="species",metric=5,period=0,species=c("canv","lbcu","mago","mopl","nopi","sacr","trbl"),padusCat="unitName",catValues=c('Sacramento National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=8,geopolRestrict=T)

makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")

p<-makeContrast(res,reportAreaSurv=FALSE,byComp="species",outp="abund",outt="plot"); print(p)



#QUESTION 2: Contribution of Multiple Individual Refuges ----
# Example: I need to identify the specific refuges that can make the largest contributions toward supporting total populations of a selected bird species within a USFWS region. This will help inform the allocation of regional resources to field stations that make the biggest contributions.
# How do refuges rank in terms of their contribution to total abundance for a selected bird species within USFWS Region X?

#Example 1: Chestnut-collared longspur in Region 6 (breeding)
#New Method - Pareto Chart
res<-makeQuestion(byComp="species",metric=5,period=1, species="cclo", padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

#Add Designation Type (park, refuge, etc.)
resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="desType", filterByCat=NA, recalcRelAbund=FALSE)

#Add Manager Name (FWS, NPS, etc.)
resfort<- fortifyFilterRes(rdf=resfort, areaCat="unitName", addCat="mgrName", filterByCat=NA, recalcRelAbund=FALSE)

head(resfort)

#Filter records for NWRs and managed by FWS
resfort <- filter(resfort,desType == "National Wildlife Refuge",mgrName == "FWS")

head(resfort)

#% Domain-relative abundance index
p<-makePareto(df=head(resfort[order(-resfort$wgtSumMetric),],10), geopolVal="USFWSregion 6", xvar="Area", yvar="totalAbundanceIndex",barsOnly=TRUE, dataOnly=FALSE, xlabel="Area",transposePlot=TRUE,fillColor="#0571b0",addYVals=TRUE, addNote=NA,highCat=NA,highColor="#ca0020",paretoColor="black"); print(p)


#Old Method
res<-makeQuestion(byComp="species",metric=5,period=1,species="cclo",padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat=c("mgrName","desType"), filterByCat=NA, recalcRelAbund=FALSE)

rfa<-subset(resfort,mgrName=="FWS",desType=="National Wildlife Refuge");rfa$Area<-as.character(rfa$Area)

rfa<-rfa[order(rfa$wgtSumMetric,decreasing=TRUE),]

rfb<-subset(resfort,Area=="USFWSregion 6")

resft<-rbind(rfa[1:10,],rfb)

p<-makePareto(df=resft, geopolVal="USFWSregion 6", xvar="Area",yvar="totalAbundanceIndex",barsOnly=FALSE, dataOnly=FALSE,xlabel="Area",transposePlot=TRUE,fillColor="#0571b0",addYVals=TRUE, addNote=NA,highCat=NA,highColor="#ca0020",paretoColor="black"); print(p)

makeContrast(resfort,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(resfort[order(-resfort$wgtSumMetric),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)


#Example 2: Tricolored blackbird in Region 8 (breeding)

#New Method - Pareto Chart
res<-makeQuestion(byComp="species",metric=5,period=1, species="trbl", padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=8,geopolRestrict=T)

#Add Manager Name (FWS, NPS, etc.)
resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="mgrName", filterByCat=NA, recalcRelAbund=FALSE)

#Add Designation Type (park, refuge, etc.)
#resfort<- fortifyFilterRes(rdf=resfort, areaCat="unitName", addCat="desType", filterByCat=NA, recalcRelAbund=FALSE)

head(resfort)

#Filter records for NWRs and managed by FWS
resfort <- filter(resfort,mgrName == "FWS")

head(resfort)

#% Domain-relative abundance index
p<-makePareto(df=head(resfort[order(-resfort$wgtSumMetric),],10), geopolVal="USFWSregion 6", xvar="Area", yvar="totalAbundanceIndex",barsOnly=FALSE, dataOnly=FALSE, xlabel="Area",transposePlot=FALSE,fillColor="#0571b0",addYVals=TRUE, addNote=NA,highCat=NA,highColor="#ca0020",paretoColor="black"); print(p)


#Old Method
res<-makeQuestion(byComp="species",metric=5,period=1,species="trbl",padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=8,geopolRestrict=T)

resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="mgrName", filterByCat=c("FWS"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(resfort,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(resfort[order(-resfort$wgtSumMetric),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)

#QUESTION 3: Contribution of USFWS-managed Lands as a Whole ----
#Example: I need to identify Regional Priorities to drive regional conservation and monitoring efforts. One factor that I will consider is the contribution of USFWS-managed lands to total populations of selected bird species during one or more seasons (breeding, wintering) within a USFWS Region.
#How do bird species rank in terms of the contribution of refuge lands as a whole to total abundance for selected species within USFWS Region X?

# Example 1: Region 6 breeding season
res<-makeQuestion(byComp="species",metric=5,period=1,species=c("bais","bobo","buow","canv","cclo","lbcu","mago","mopl","nopi","sacr","sppi"),padusCat="mgrName",catValues='FWS',geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

#% Domain-relative abundance index
p<-makePareto(df=res, geopolVal="USFWSregion 6", xvar="species", yvar="totalAbundanceIndex",barsOnly=FALSE, dataOnly=FALSE, xlabel="Species",transposePlot=FALSE,fillColor="#0571b0",addYVals=TRUE, addNote=NA,highCat=NA,highColor="#ca0020",paretoColor="black"); print(p)


#Old Method
res<-makeQuestion(byComp="species",metric=5,period=1,species=c("bais","bobo","buow","canv","cclo","lbcu","mago","mopl","nopi","sacr","sppi"),padusCat="mgrName",catValues='FWS',geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

res<-subset(res, Area=="FWS")

makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")

p<-makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="plot"); print(p)


# Example 2: Region 8 wintering season
res<-makeQuestion(byComp="species",metric=4,period=0,species=c("buow","canv","lbcu","mago","mopl","nopi","sacr","trbl"),padusCat="mgrName",catValues='FWS',geopolCat="USFWSregion",geopolValues=8,geopolRestrict=T)

res<-subset(res, Area=="FWS")

makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="table")

p<-makeContrast(res,reportAreaSurv=FALSE,by="species",outp="abund",outt="plot"); print(p)


#QUESTION 4: Contribution of Multiple Land Managers ----
#Example: I need to identify the best partners to work with or influence who contribute the most toward supporting total populations of a selected bird species within a USFWS region. This will help inform outreach and partnership efforts for collaborative conservation.
#How do landowners/managers rank in terms of the contribution of their lands to total abundance for a selected bird species within USFWS Region X?

#Example 1 ----
#Compare all land manager types within FWSregion 6, and vs the entire region 6, for bais during breeding season.

#Land Ownership by Area - All Lands ----
#First, how is land ownership distributed within the region of interest?
#Run a query for metric 5, which covers all lands.
res<-makeQuestion(byComp="area",metric=5,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res

#Rename Area manager names
res$Area <- getManagerName(res$Area)
res

#Plot a Pareto chart
p2<-makePareto(df=res,includesGeopol="USFWSregion 6", yvar="relArea",barsOnly=FALSE, dataOnly=FALSE, xlabel="PAD-US Land Manager",transposePlot=FALSE, fillColor="#0571b0",addYVals=TRUE,addNote=NA,highCat="Fed - FWS",highColor="#ca0020",paretoColor="black")
p2


#Land Ownership by Area - Protected Lands ----
#How is land ownership distributed on protected lands within the region of interest?

#Pareto Chart
df <- data.frame(
		x = res$Area[1:(length(res$Area)-2)],
		y = res$totalCells[1:(length(res$Area)-2)]/sum(res$totalCells[1:(length(res$Area)-2)])*100)
df$y <- round(df$y, digits=1)
#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#009E73"
	}
}


# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black", bars.fill = df$color_scale[order(-df$y)]) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relArea)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% of Total Area") + annotate("text", x = 6, y = 70, label = "Protected Lands",size=5)
p1



#Empirical Encounter Rate ----
res<-makeQuestion(byComp="area",metric=4,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6, geopolRestrict=T)
res


#Rename Area manager names
res$Area <- getManagerName(res$Area)
res

#Summarize data by Area and Metric
res <- res %>% 
		group_by(Area,metric) %>% 
		dplyr::summarise(avgEncounterRate=weighted.mean(wgtDensity,sumCells),totalAbundanceIndex=sum(wgtSumMetric))
res

#Bar Chart
# Set up data
res$avgEncounterRate <- round(res$avgEncounterRate, digits=3)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$avgEncounterRate[-length(res$Area)])

#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#0072B2"
	}
}

#Sort data frame by descending avgEncounterRate to get the correct colors for the barplot
df <- df[order(-df$y),]

# Render Plot
p1_eer <- ggplot(df, aes(x=reorder(x,-y), y=y)) + geom_bar(fill=df$color_scale,colour="black",stat="identity") + geom_text(aes(label=y, y=y+max(res$avgEncounterRate)*0.05),position = position_dodge(0.9),vjust = 0,size=2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("Average Empirical Encounter Rate")
p1_eer


#Base Model Total Abundance Index ----
res<-makeQuestion(byComp="area",metric=5,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res

#Rename Area manager names
res$Area <- getManagerName(res$Area)

#Summarize data by Area and Metric
res <- res %>% 
		group_by(Area,metric) %>% 
		dplyr::summarise(totalCells=sum(sumCells),AreaSizeHA=sum(presenceHA),avgEncounterRate=weighted.mean(wgtDensity,sumCells),totalAbundanceIndex=sum(wgtAbundance))
res

res$totalAbundanceIndex <- round(res$totalAbundanceIndex, digits=0)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$totalAbundanceIndex[-length(res$Area)],
		z = res$totalAbundanceIndex[-length(res$Area)]/res$totalAbundanceIndex[length(res$Area)])
df$z <- round(df$z, digits=3)*100
#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#56B4E9"
	}
}

# Render Pareto Plot
p1_tai <- ggplot(df, aes(x=x, y=z)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black", bars.fill = df$color_scale[order(-df$z)]) + geom_text(aes(label = z[order(-z)] , y = z[order(-z)] + max(df$z)*0.05), position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Base Model Total Abundance Index")
p1_tai


#Example 2 ----
#Compare all land manager types within FWSregion 8, and vs the entire region 8, for trbl during wintering season.

#Land Ownership by Area - All Lands ----
#First, how is land ownership distributed within the region of interest?
#Run a query for metric 5, which covers all lands.
res<-makeQuestion(byComp="area",metric=5,period=0,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
res

#Rename Area manager names
res$Area <- getManagerName(res$Area)
res

#Summarize data by Area and Metric
res <- res %>% 
		group_by(Area,metric) %>% 
		dplyr::summarise(totalCells=sum(sumCells),AreaSizeHA=sum(presenceHA),avgEncounterRate=weighted.mean(wgtDensity,sumCells),totalAbundanceIndex=sum(wgtAbundance))
res

#Pareto Chart
res$relArea <- res$totalCells/res$totalCells[length(res$totalCells)]*100
res$relArea <- round(res$relArea, digits=1)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$relArea[-length(res$Area)])
#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#009E73"
	}
}


# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black", bars.fill = df$color_scale[order(-df$y)]) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relArea)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% of Total Area") + annotate("text", x = 6, y = 70, label = "All Lands",size=5)
p1


#Land Ownership by Area - Protected Lands ----
#How is land ownership distributed on protected lands within the region of interest?

#Pareto Chart
df <- data.frame(
		x = res$Area[1:(length(res$Area)-2)],
		y = res$totalCells[1:(length(res$Area)-2)]/sum(res$totalCells[1:(length(res$Area)-2)])*100)
df$y <- round(df$y, digits=1)
#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#009E73"
	}
}


# Render Pareto Plot
p1 <- ggplot(df, aes(x=x, y=y)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black", bars.fill = df$color_scale[order(-df$y)]) + geom_text(aes(label = y[order(-y)] , y = y[order(-y)] + max(res$relArea)*0.05),position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% of Total Area") + annotate("text", x = 6, y = 70, label = "Protected Lands",size=5)
p1



#Empirical Encounter Rate ----
res<-makeQuestion(byComp="area",metric=4,period=0,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8, geopolRestrict=T)
res


#Rename Area manager names
res$Area <- getManagerName(res$Area)
res

#Summarize data by Area and Metric
res <- res %>% 
		group_by(Area,metric) %>% 
		dplyr::summarise(avgEncounterRate=weighted.mean(wgtDensity,sumCells),totalAbundanceIndex=sum(wgtSumMetric))
res

#Bar Chart
# Set up data
res$avgEncounterRate <- round(res$avgEncounterRate, digits=3)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$avgEncounterRate[-length(res$Area)])

#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#0072B2"
	}
}

#Sort data frame by descending avgEncounterRate to get the correct colors for the barplot
df <- df[order(-df$y),]

# Render Plot
p1_eer <- ggplot(df, aes(x=reorder(x,-y), y=y)) + geom_bar(fill=df$color_scale,colour="black",stat="identity") + geom_text(aes(label=y, y=y+max(res$avgEncounterRate)*0.05),position = position_dodge(0.9),vjust = 0,size=2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("Average Empirical Encounter Rate")
p1_eer


#Base Model Total Abundance Index ----
res<-makeQuestion(byComp="area",metric=4,period=0,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=8)
res

#Rename Area manager names
res$Area <- getManagerName(res$Area)

#Summarize data by Area and Metric
res <- res %>% 
		group_by(Area,metric) %>% 
		dplyr::summarise(totalCells=sum(sumCells),AreaSizeHA=sum(presenceHA),avgEncounterRate=weighted.mean(wgtDensity,sumCells),totalAbundanceIndex=sum(wgtAbundance))
res

res$totalAbundanceIndex <- round(res$totalAbundanceIndex, digits=0)
df <- data.frame(
		x = res$Area[-length(res$Area)],
		y = res$totalAbundanceIndex[-length(res$Area)],
		z = res$totalAbundanceIndex[-length(res$Area)]/res$totalAbundanceIndex[length(res$Area)])
df$z <- round(df$z, digits=3)*100
#Set up plot colors
df$color_scale <- NA
for (i in 1:length(df$x)){
	if (df$x[i] == "Fed - FWS") {
		df$color_scale[i] <- "#D55E00"
	} else {
		df$color_scale[i] <- "#56B4E9"
	}
}

# Render Pareto Plot
p1_tai <- ggplot(df, aes(x=x, y=z)) + stat_pareto(point.color = "black",point.size = 2, line.color = "black", bars.fill = df$color_scale[order(-df$z)]) + geom_text(aes(label = z[order(-z)] , y = z[order(-z)] + max(df$z)*0.05), position = position_dodge(0.9),vjust = 0,size=2.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("PAD-US Land Manager") + ylab("% Base Model Total Abundance Index")
p1_tai


#VARIATIONS ON BASIC QUERIES ----

#QUESTION 5:	Basic Query + Varying Geopolitical Domains ----
#Example: I need to identify the best partners to work with or influence who contribute the most toward supporting total populations of a selected bird species within the Intermountain West Joint Venture. This will help inform outreach and partnership efforts for collaborative conservation at that geographic scale. Other geopolitical areas of interest could include U.S. States, Bird Conservation Regions (BCRs), Landscape Conservation Cooperatives (LCCs), etc.
#How do landowners/managers rank in terms of the contribution of their lands to total abundance for a selected bird species within the Intermountain West Joint Venture?

#Example 1: Long-billed Curlew in IWJV (breeding)
res<-makeQuestion(byComp="area",metric=5,period=1,species="lbcu",padusCat="mgrName",catValues="all",geopolCat="USJVregion",geopolValues=16,geopolRestrict=T)
res

#resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="mgrName", filterByCat=c("FWS"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(res,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(res[order(-res$relAbundance),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)


#Example 2: Tricolored blackbirds in Central Valley JV (breeding)
res<-makeQuestion(byComp="area",metric=5,period=1,species="trbl",padusCat="mgrName",catValues="all",geopolCat="USJVregion",geopolValues=9,geopolRestrict=T)

#resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="mgrName", filterByCat=c("FWS"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(res,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(res[order(-res$relAbundance),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)


#QUESTION 6:	Basic Query + Varying Species Distribution Models (SDMs) ----
#Example 1: HAPET produced an outside species distribution model for BAIS. I need to identify the specific refuges that can make the largest contributions toward supporting total populations of BAIS within a USFWS region. This will help inform the allocation of regional resources to field stations that make the biggest contributions.
#How do refuges rank in terms of their contribution to total occupancy for BAIS within USFWS Region 6?

#Example 1a: Baird's Sparrow in Region 6 (breeding) HAPET Model
res<-makeQuestion(byComp="species",metric=6,period=1,species="bais",padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="desType", filterByCat=c("National Wildlife Refuge","Waterfowl Production Area"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(resfort,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(resfort[order(-resfort$wgtSumMetric),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)

#Example 1b: Baird's Sparrow in Region 6 (breeding) ADALO Base Model
res<-makeQuestion(byComp="species",metric=5,period=1,species="bais",padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=6,geopolRestrict=T)

resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="desType", filterByCat=c("National Wildlife Refuge","Waterfowl Production Area"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(resfort,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(resfort[order(-resfort$wgtSumMetric),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)


#Example 2a: Tricolored Blackbird in Region 6 (breeding) Point Blue Model
res<-makeQuestion(byComp="species",metric=7,period=1,species="trbl",padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=8,geopolRestrict=T)

resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="desType", filterByCat=c("National Wildlife Refuge","Waterfowl Production Area"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(resfort,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(resfort[order(-resfort$wgtSumMetric),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)


#Example 2b: Tricolored Blackbird in Region 6 (breeding) ADALO Base Model
res<-makeQuestion(byComp="species",metric=5,period=1,species="trbl",padusCat="unitName",catValues="all",geopolCat="USFWSregion",geopolValues=8,geopolRestrict=T)

resfort<- fortifyFilterRes(rdf=res, areaCat="unitName", addCat="desType", filterByCat=c("National Wildlife Refuge","Waterfowl Production Area"), recalcRelAbund=TRUE) #Filter records for one land manager/owner

makeContrast(resfort,reportAreaSurv=FALSE,by="area",outp="abund",outt="table")

p<-makeContrast(head(resfort[order(-resfort$wgtSumMetric),],10),reportAreaSurv=FALSE,by="area",outp="abund",outt="plot",plotSorted=TRUE); print(p)






########################################################################
## OLD Example Questions --

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

subset(reserr,Area=="USFWSregion 8",select="wgtAbundance")

# Here is an example that does not enforce the domain:
# Q6: compare San Pablo Bay vs the entire R6 for canv over winter.
res<-makeQuestion(byComp="area",metric=4,period=0,species="canv",padusCat="unitName",catValues=c('San Pablo Bay National Wildlife Refuge'),geopolCat="USFWSregion",geopolValues=6,geopolRestrict=FALSE)
#No density for R6?
res


# Q7: Compare two regions (SFBNWR vs SacRiverNWR) in importance to a species (CANV during winter)
res<-makeQuestion(byComp="area",metric=4,period=1,species="bais",padusCat="mgrName",catValues="all",geopolCat="USFWSregion",geopolValues=6)
res$Manager<-getManagerName(as.character(res$Area))

