# Re-examining the meningitis belt: associations between environmental factors and epidemic meningitis risk across Africa

This repository contains R and Stata code used to create a logistic regression model and risk map for meningitis outbreaks across Africa. A 2002 study by Molesworth et al investigated the spatial distribution of meningitis epidemics in Africa occurring between 1980 and 1999 to determine the geographical risk of meningitis across Africa. This analysis was carried out to present an update on the associations between environmental variables and meningitis epidemic risk for meningitis across Africa abd to understand if the geography of the meningitis belt and risk factors associated with meningitis have changed. 

# Description


# Repository Contents
1. [Enviromental Cluster Analysis](https://github.com/molly-cliff/Meningitis-belt-location/tree/main/1.%20Enviromental%20cluster%20analysis)- Processes monthly temporal data into single surface comprising of climatic variable profiles. This is largely carried out using Principle component analysis and the  K means algorithm  to divide each seasonal variable raster into distinct clusters based on data similarity. This folder also includes code to extract average population density for each ADMN2 district within an African continental GADM shapefile.
   
2. [Data Cleaning](https://github.com/molly-cliff/Meningitis-belt-location/tree/main/2.Data%20Cleaning)- Stata code to assign meningitis epidemics  to their nearest ADMN2 district within the GADM shapefile.


3. [Intersection Analysis](https://github.com/molly-cliff/Meningitis-belt-location/tree/main/3.%20Intersections) - Maps epidemics reported to older adminstrative boundaries to exisiting (2024) ADMN2 districts using a WHO Polio shapefile due to the changing nature of district boundaries.  ADMN2 level district intersections between the WHO Polio and GADM shapefiles were calculated and filtered where they overlap by 50% and then 30% and included as an outbreak district.
   
4. [Logistic Regression](https://github.com/molly-cliff/Meningitis-belt-location/tree/main/4.%20Logistic%20regression)- Logistic regression model, both with and without the Democractic Republic of Congo as part of a sensitivity analysis. Code uses Rainfall, dust, and humidity within final logistic regression model and generates a risk map for meningitis outbreaks across Africa. Pixel based population weighting analysis is carried out within [SEDAC-population-weighting.R](https://github.com/molly-cliff/Meningitis-belt-location/blob/editing-branch/4.%20Logistic%20regression/SEDAC-population-weighting.R). Figure 3-5 and Supplemnentary Figure 1 are created within the Logistic regression and sensitivity analysis code

5.[Figures/Graphs](https://github.com/molly-cliff/Meningitis-belt-location/tree/editing-branch/5.%20Figures%20and%20Graphs)- R code to create Figures 1 (seasonal cluster trends) and 2 (continental map of outbreaks)
