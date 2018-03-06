# Unemployment and Extremism in Slovakia

## Summary of the FUNCTION

The aim of the developed function is to test for relationship between regional unemployment and support for extremist parties using the regional unemployment and election data in Slovakia. The function first creates a general OLS regression model between the support of a chosen political party or grouping and unemployment in a given year. If it proves to be significant it tests whether the dependent variable is spatially clustered. On condition that it is, it moves on to run a geographically weighted regression model. Lastly, it compares the GWR model with alternative regression models accounting for spatial dependency between the data.

## How to use this REPO

* The manual gives the rationale for the function and explains the statistics behind 
* The CSV files used can be used in the csv folder
* The **main** code file can be found in the rcode folder and is named function.R 
* The rcode folder also contains the datamanipulation file which shows how the data were cleaned in order to be used for the function
* The shapefile folder contains the shapefiles used for plotting (Source: http://wiki.freemap.sk/HraniceAdministrativnychUzemi , accessed 27.12.2017)