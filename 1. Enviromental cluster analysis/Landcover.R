# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents")

# Load required libraries
library(raster)
library(sf)
library(plyr)
library(dplyr)

# Read raster data
landcover <- raster("C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")

# Plot the raster
plot(landcover)

# Read shapefile
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

# Crop raster to the extent of the shapefile
landcover_cropped <- crop(landcover, shape)

# Aggregate raster
landcover_aggregated <- aggregate(landcover_cropped, fact = 5, fun = mean)

# Resample raster using bilinear interpolation
landcover_resampled <- resample(landcover_cropped, landcover_aggregated, method = "bilinear")

# Fix invalid geometries in shapefile
shape_valid <- st_make_valid(shape)

# Check validity of fixed geometries
validity_fixed <- st_is_valid(shape_valid)

# Simplify shapefile geometries
shape_simplified <- st_simplify(shape_valid, preserveTopology = TRUE)

# Mask raster using simplified shapefile
landcover_masked <- mask(landcover_resampled, shape_simplified)

# Extract raster values for each polygon in the shapefile
cl2test <- data.frame(shape, extract(landcover_masked, shape_simplified, fun = modal, na.rm = TRUE))

# Round extracted values
cl2test$zonesrounded <- round_any(cl2test$extract.landcover_masked..shape_simplified..fun...modal..na.rm...TRUE., 10)

# Define breakpoints and category names
breakpoints <- c(0, 40, 120, 130, 140, 150, 160, 180, 190, 200, 210, 220)
category_names <- c("Cropland", "Tree cover", "Shrubland", "Grassland", "Lichen and Moss", "Sparse vegetation", "Flooded areas", "Urban areas", "Bare areas", "Water bodies", "Permanent snow and ice")

# Create new column with named land categories
cl2test$new_land_category <- cut(cl2test$zonesrounded, breaks = breakpoints, labels = category_names, include.lowest = TRUE)

# Merge extracted data with original shapefile
cl2test_merged <- merge(cl2test, shape, by = "GID_2")
cl2test_sf <- st_as_sf(cl2test_merged)

# Select relevant columns
cl2test_final <- cl2test_sf[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'new_land_category')]

# Remove duplicated entries
total3test <- cl2test_final[!duplicated(cl2test_final[, c("GID_2")]),]

# Write final shapefile
st_write(total3test, "Landcoverbilinear.shp", append = FALSE)
