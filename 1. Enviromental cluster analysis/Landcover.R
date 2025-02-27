# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents")

library(raster)
library(sf)
library(plyr)
library(dplyr)


landcover <- raster("C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")
plot(landcover)


shape <- read_sf(dsn = ".", layer = "Shapefile_improved")
landcover_cropped <- crop(landcover, shape)
landcover_aggregated <- aggregate(landcover_cropped, fact = 5, fun = mean)
landcover_resampled <- resample(landcover_cropped, landcover_aggregated, method = "bilinear")

# Fix invalid geometries in shapefile
shape_valid <- st_make_valid(shape)
validity_fixed <- st_is_valid(shape_valid)
shape_simplified <- st_simplify(shape_valid, preserveTopology = TRUE)
landcover_masked <- mask(landcover_resampled, shape_simplified)
cl2test <- data.frame(shape, extract(landcover_masked, shape_simplified, fun = modal, na.rm = TRUE))

# Round extracted values
cl2test$zonesrounded <- round_any(cl2test$extract.landcover_masked..shape_simplified..fun...modal..na.rm...TRUE., 10)

# Define breakpoints and category names
breakpoints <- c(0, 40, 120, 130, 140, 150, 160, 180, 190, 200, 210, 220)
category_names <- c("Cropland", "Tree cover", "Shrubland", "Grassland", "Lichen and Moss", "Sparse vegetation", "Flooded areas", "Urban areas", "Bare areas", "Water bodies", "Permanent snow and ice")

cl2test$new_land_category <- cut(cl2test$zonesrounded, breaks = breakpoints, labels = category_names, include.lowest = TRUE)
cl2test_merged <- merge(cl2test, shape, by = "GID_2")
cl2test_sf <- st_as_sf(cl2test_merged)
cl2test_final <- cl2test_sf[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'new_land_category')]
total3test <- cl2test_final[!duplicated(cl2test_final[, c("GID_2")]),]

# Write final shapefile
st_write(total3test, "Landcoverbilinear.shp", append = FALSE)
