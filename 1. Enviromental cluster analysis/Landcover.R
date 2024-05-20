library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(plyr)

# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/landcover")

# Load landcover raster
landcover2 <- raster("C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")

# Plot landcover raster
plot(landcover2)

# Load shapefile
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents")
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

# Crop landcover raster to shapefile extent
landcover2 <- crop(landcover2, shape)

# Create raster with appropriate resolution
landcover3 <- aggregate(landcover2, fact = 10, fun = mean)

# Perform bilinear interpolation to resample the raster to the new resolution
r_resampled <- resample(landcover2, landcover3, method = "bilinear")

raster2_indexed <- st_simplify(shape, preserveTopology = TRUE)
landcover2 <- mask(r_resampled, raster2_indexed)

# Plot the masked raster
plot(landcover2)

# Extract raster values to shapefile and create a dataframe
cl2test <- data.frame(shape, extract(landcover2, raster2_indexed, fun = modal, na.rm = TRUE))

# Round extracted zones
cl2test$zonesrounded <- round_any(cl2test$extract.landcover2..raster2_indexed..fun...modal..na.rm...TRUE., 10)

# Define breakpoints and category names for land categories
breakpoints <- c(0, 40, 120, 130, 140, 150, 160, 180, 190, 200, 210, 220)
category_names <- c("Cropland", "Tree cover", "Shrubland", "Grassland", "Lichen and Moss",
                    "Sparse vegetation", "Flooded areas", "Urban areas", "Bare areas",
                    "Water bodies", "Permanent snow and ice")

# Create a new column 'new_land_category' with named land categories
cl2test$new_land_category <- cut(cl2test$zonesrounded, breaks = breakpoints, labels = category_names, include.lowest = TRUE)

# Merge data with shapefile and create an sf object
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)

# Select relevant columns
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'new_land_category')]

# Remove duplicated rows based on 'GID_2'
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]), ]

# Write the final shapefile
st_write(total3test, "Landcover.shp", append = FALSE)
