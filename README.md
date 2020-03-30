[Diversity and Disparities](https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTDB.htm) provides census data across years for tracts adjusted to 2010 tract boundaries. I use the 2000 census sample data, 2000 census full count data, and the 2008-2012 ACS data. The data are in the following files:

* "LTDB_Std_2000_Sample.csv": 2000 Sample Data
* "LTDB_Std_2000_fullcount.csv": 2000 Full Count Data
* "LTDB_Std_2010_Sample.csv": 2008-2012 ACS Data

I use ["99mfips.txt"](https://www.census.gov/population/estimates/metro-city/99mfips.txt) to connect MSA/PMSA to counties so that I can use counties to link tracts to MSA/PMSAs.

The following files are from American Fact Finder:

* "DEC_10_SF1_G001_with_ann.csv": 2010 tract areas
* "DEC_00_SF3_P001_with_ann.csv": 2000 MSA/CMSA populations
* "DEC_00_SF3_P001_with_ann_p.csv": 2000 PMSA populations
* "DEC_00_SF3_P053_with_ann.csv": 2000 MSA/CMSA median household incomes
* "DEC_00_SF3_P053_with_ann_p.csv": 2000 PMSA median household incomes
* "DEC_00_SF3_QTP24_with_ann.csv": 2000 MSA/CMSA employment 
* "DEC_00_SF3_QTP24_with_ann_p.csv": 2000 PMSA unemployment 

The file "MSA_Data.csv" contains the WRLURI measures by MSA/PMSA for the 47 MSAs with 10 or more community observations in the Wharton Land Use Regulation Index entered from table 11 of [A New Measure of the Local Regulatory Environment for Housing Markets:
The Wharton Residential Land Use Regulatory Index](http://realestate.wharton.upenn.edu/wp-content/uploads/2017/03/558.pdf).

The full Wharton Regulation data set is "WHARTON LAND REGULATION DATA_1_24_2008.dta" and is available for download [here](http://real-faculty.wharton.upenn.edu/gyourko/land-use-survey/).

The land unavailability index file "HOUSING_SUPPLY.dta" is associated with the paper [THE GEOGRAPHIC DETERMINANTS
OF HOUSING SUPPLY](https://mitcre.mit.edu/wp-content/uploads/2014/03/The-Quarterly-Journal-of-Economics-2010-Saiz-1253-96.pdf).
