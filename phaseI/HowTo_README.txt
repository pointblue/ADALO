To generate collated.df
1) Run code in getCADCdata.R
2) Attribute with geospatial data: attribute_geospatial_New.R 
	(careful with paths of source data - this was updated in a previous iteration)
	(note the code in getAdditional_padusAttrib.R and reattributeFWSintacq.R - update attribute_geospatial_New.R as appropriate)
3) Make the file collated.df: code in plotData.R 
	(this also needs updating after changes in attribute_geospatial_New.R)
4) Generate species plots: plotData.R
5) Fit models for probPresence and incidence rate: basicModels.R
5) Generate report plots: reportPlots.R
6) Generate report tables: tables_byRefuge.R
