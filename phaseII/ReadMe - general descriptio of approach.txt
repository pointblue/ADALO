## Overall approach  ##############################

This file describes at a high level the steps followed.

1) Gather all the data by species and season
2) Generate set of locations to join with Birdlife International and generate species ranges
3) Calculate empirical density by species and season into 990m grid (metric: empirical density)
4) Prepare geospatial attributes: convert to 990m grid, collapse or combine categories of NLCD and NWI
5) Attribute the density data table (990m grid table) with the geospatial data
6) Fit and review SDMs (metric: modeled density)
7) Prepare the PADUS at 30m (ask Doug for code)
8) Convert 30m grid to segmented tables (497 tables of 10,000,000 cells each!)
9) Intersect PADUS table with 30m grid table to get objId for each cell in each segment (i.e., each 10M-cell table)
10) Intersect 990m grid with PADUS-attributed table t0 get 990mCellId for each 30m cell in each segment
11) Aggregate the segments by 990mCellId to create intersects of 990mCellId and PADUSobjectId. This intersect results
	in each row being a unique combination of 990mCellId and PADUSobjectId, and reports the number of 30m cells by objectId within the 990m cell (the intersects)

12) Build warehouse: merge m5 and padusIntersects by 990mcellId
13) Build warehouse: merge m5 and padusIntersects by 990mcellId
14) Build warehouse: merge HAPET SDM data and padusInsersects by 990mCellId
