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
str_name <- 'Population_density.tif' 
imported_raster <- raster(str_name)
Population_density <- imported_raster 
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

# Crop population density raster to Africa continent shapefile
Population_density <- crop(Population_density, shape)
Population_density <- mask(Population_density, shape)

# Extract average population density for each district and turn it into a dataframe
Pop_density <- data.frame(shape, raster::extract(Population_density, shape, fun = mean, na.rm = TRUE, touches = TRUE))
Pop_density$Pop_density <- Pop_density$extract.Population_density..shape..fun...mean..na.rm...T..touches...TRUE.

# Merge datasets, select certain columns, and remove duplicate rows
Pop_density <- merge(Pop_density, shape, by = "GID_2")
Pop_density <- st_as_sf(Pop_density)
Pop_density <- Pop_density[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
Pop_density <- Pop_density[!duplicated(Pop_density[, c("GID_2")]),]

# Write to shapefile
st_write(Pop_density, "Population_density.shp", append = FALSE)
