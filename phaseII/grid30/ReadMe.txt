How did we create the segmented grid? CODE PLEASE
Has xy and cellId, divided into segments of 10,000,000 cells - kept these originals WHERE
We then attributed with R68 and removed those cells outside of the target regions 
We then split the padus raster by cell, into 60,048 (?) tables, each with cellId and objectId - CODE??
	Alternative: use Doug's table, but split into segments of cellIds of 10,000,000 cells
We then attributed the segments with the padus objectId - kept these WHERE?

Next: make NA objectIds -> 0 (= private lands)

Next: attribute geopolitical
	1) Load each segment into geopoints table with the aea crs
	2) Load the geopolitical shapefile
	3) transform the points to the geopolitical projection
	4) extract the geopolitical data into a column of the segment
	5) save the segment
	6) Loop with the next geopolitical
	
Next: loop by metric, species and period
	1) Load each segment into geopoints table with the aea crs
	2) Load the metrics raster
	3) transform the points to the metric projection
	4) extract the metric, species and period data into a column of the segment  
	5) aggregate by objectId + county
	6) save aggregated segment
	
next: aggregate the aggregated into mysql table
(Make a table per species, so each table will have a metric and period column
