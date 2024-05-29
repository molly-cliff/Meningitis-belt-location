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



str_name <- 'gpw_v4_population_density_rev11_2000_2pt5_min.tif' 

imported_raster <- raster(str_name)
Population_density <- imported_raster 
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

shape <- st_transform(shape, crs(Population_density))
Population_density <- crop(Population_density, extent(shape))
# Crop population density raster to Africa continent shapefile

Population_density <- mask(Population_density, shape)

# Extract average population density for each district and turn it into a dataframe
Pop_density <- data.frame(shape, raster::extract(Population_density, shape, fun = mean, na.rm = TRUE, touches = TRUE))

Pop_density$Pop_density <- Pop_density$raster..extract.Population_density..shape..fun...mean..na.rm...TRUE..

# Merge datasets, select certain columns, and remove duplicate rows
Pop_density <- merge(Pop_density, shape, by = "GID_2")
Pop_density <- st_as_sf(Pop_density)
Pop_density <- Pop_density[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.y', 'Pop_density')]
Pop_density2000 <- Pop_density[!duplicated(Pop_density[, c("GID_2")]),]
st_write(Pop_density2000, "Pop_density2000.shp", append = FALSE)

str_name <- 'gpw_v4_population_density_rev11_2005_2pt5_min.tif'

imported_raster <- raster(str_name)
Population_density <- imported_raster 
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

shape <- st_transform(shape, crs(Population_density))
Population_density <- crop(Population_density, extent(shape))
# Crop population density raster to Africa continent shapefile

Population_density <- mask(Population_density, shape)

# Extract average population density for each district and turn it into a dataframe
Pop_density <- data.frame(shape, raster::extract(Population_density, shape, fun = mean, na.rm = TRUE, touches = TRUE))

Pop_density$Pop_density <- Pop_density$raster..extract.Population_density..shape..fun...mean..na.rm...TRUE..

# Merge datasets, select certain columns, and remove duplicate rows
Pop_density <- merge(Pop_density, shape, by = "GID_2")
Pop_density <- st_as_sf(Pop_density)
Pop_density <- Pop_density[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.y', 'Pop_density')]
Pop_density2005 <- Pop_density[!duplicated(Pop_density[, c("GID_2")]),]
st_write(Pop_density2005, "Pop_density2005.shp", append = FALSE)

str_name <- 'gpw_v4_population_density_rev11_2010_2pt5_min.tif' 

imported_raster <- raster(str_name)
Population_density <- imported_raster 
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

shape <- st_transform(shape, crs(Population_density))
Population_density <- crop(Population_density, extent(shape))
# Crop population density raster to Africa continent shapefile

Population_density <- mask(Population_density, shape)

# Extract average population density for each district and turn it into a dataframe
Pop_density <- data.frame(shape, raster::extract(Population_density, shape, fun = mean, na.rm = TRUE, touches = TRUE))

Pop_density$Pop_density <- Pop_density$raster..extract.Population_density..shape..fun...mean..na.rm...TRUE..

# Merge datasets, select certain columns, and remove duplicate rows
Pop_density <- merge(Pop_density, shape, by = "GID_2")
Pop_density <- st_as_sf(Pop_density)
Pop_density <- Pop_density[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.y', 'Pop_density')]
Pop_density2010 <- Pop_density[!duplicated(Pop_density[, c("GID_2")]),]
st_write(Pop_density2010, "Pop_density2010.shp", append = FALSE)

str_name <- 'gpw_v4_population_density_rev11_2015_2pt5_min.tif' 
imported_raster <- raster(str_name)
Population_density <- imported_raster 
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

shape <- st_transform(shape, crs(Population_density))
Population_density <- crop(Population_density, extent(shape))
# Crop population density raster to Africa continent shapefile

Population_density <- mask(Population_density, shape)

# Extract average population density for each district and turn it into a dataframe
Pop_density <- data.frame(shape, raster::extract(Population_density, shape, fun = mean, na.rm = TRUE, touches = TRUE))

Pop_density$Pop_density <- Pop_density$raster..extract.Population_density..shape..fun...mean..na.rm...TRUE..

# Merge datasets, select certain columns, and remove duplicate rows
Pop_density <- merge(Pop_density, shape, by = "GID_2")
Pop_density <- st_as_sf(Pop_density)
Pop_density <- Pop_density[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.y', 'Pop_density')]
Pop_density2015 <- Pop_density[!duplicated(Pop_density[, c("GID_2")]),]
st_write(Pop_density2015, "Pop_density2015.shp", append = FALSE)
str_name <- 'gpw_v4_population_density_rev11_2020_2pt5_min.tif' 

imported_raster <- raster(str_name)
Population_density <- imported_raster 
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

shape <- st_transform(shape, crs(Population_density))
Population_density <- crop(Population_density, extent(shape))
# Crop population density raster to Africa continent shapefile

Population_density <- mask(Population_density, shape)

# Extract average population density for each district and turn it into a dataframe
Pop_density <- data.frame(shape, raster::extract(Population_density, shape, fun = mean, na.rm = TRUE, touches = TRUE))

Pop_density$Pop_density <- Pop_density$raster..extract.Population_density..shape..fun...mean..na.rm...TRUE..

# Merge datasets, select certain columns, and remove duplicate rows
Pop_density <- merge(Pop_density, shape, by = "GID_2")
Pop_density <- st_as_sf(Pop_density)
Pop_density <- Pop_density[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.y', 'Pop_density')]
Pop_density2020 <- Pop_density[!duplicated(Pop_density[, c("GID_2")]),]
st_write(Pop_density2020, "Pop_density2020.shp", append = FALSE)
