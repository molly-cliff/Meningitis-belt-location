library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(sf)
library(RStoolbox)
library(rbenchmark)
library(tidyr)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#load in population data raster and africa continental shapefile
str_name<-'Population_density.tif' 
imported_raster <- raster(str_name)
Population_density<-imported_raster 
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
#crop pop density raster to africa continent shapefile
Population_density <- crop(Population_density, shape)
Population_density <- mask(Population_density, shape)
#extracts average population desnity for each district, turns this into a dataframe
Pop_density<-data.frame(shape,extract(Population_density, shape, fun=mean, na.rm=T, touches=TRUE))
Pop_density$Pop_density<-Pop_density$extract.Population_density..shape..fun...mean..na.rm...T..touches...TRUE.



# merges two datasets using a shared identifier, converts the result into a spatial dataframe, selects certain columns, and then removes duplicate rows.

Pop_density <- merge(Pop_density,shape,by="GID_2")
Pop_density <- st_as_sf(cl2testraster)
Pop_density<-Pop_density[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
Pop_density<-Pop_density[!duplicated(Pop_density[ , c("GID_2")]),]

st_write(Pop_density, "Population_density.shp",append=FALSE)
