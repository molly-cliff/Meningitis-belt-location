# Load required libraries
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(stringi)
library(writexl)

# Set working directory and read WHO Polio shapefile
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/WHO POLIO SHAPEFILES/WHO POLIO SHAPEFILES/ADMN2_AFRO")
ADMN2 <- read_sf(dsn = ".", layer = "ADM2_AFRO")

# Set working directory and read GADM shapefile
setwd("C:/Users/mvc32/Documents/Climate_meningitis_belt")
shape2 <- st_read("Shapefile_improved.shp")

# Set working directory to Disease Data
setwd("C:/Users/mvc32/Documents/Climate_meningitis_belt/Disease Data")

# Select specific columns from ADMN2 shapefile
ADMN2 <- ADMN2[, c("SHAPE_Leng", "SHAPE_Area", "geometry", "ADM2_NAME", "ADM1_NAME", "ADM0_NAME")]

# Read weekly incidence data from Excel file
file_path <- "Weeklyincidencepoliomergetest.xlsx"
weeklyincidence_merge <- readxl::read_excel(file_path)

# Create a district_country variable for matching
weeklyincidence_merge <- weeklyincidence_merge[!duplicated(weeklyincidence_merge[, c("district_country")]), ]
ADMN2$district_countrykeep <- paste(ADMN2$ADM2_NAME, ADMN2$ADM0_NAME, sep = " ")
ADMN2$district_countrykeep <- stri_trans_totitle(ADMN2$district_countrykeep)
shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
ADMN2$district_country <- stri_trans_totitle(ADMN2$district_countrykeep)

# Join the WHO matched polio file to missing epidemics file
filtered_shapefile2 <- ADMN2 %>%
  right_join(weeklyincidence_merge, by = "district_country")

# Filter GADM shapefile for specific countries
countries_to_keep <- c(
  "Central African Republic",
  "Democratic Republic of the Congo",
  "Chad",
  "Burkina Faso",
  "Niger",
  "Cameroon",
  "Mali",
  "Burundi"
)
shape2 <- shape2[shape2$COUNTRY %in% countries_to_keep, ]

# Check and repair invalid geometries in shapefiles
invalid_geoms1 <- st_is_valid(filtered_shapefile2)
invalid_geoms2 <- st_is_valid(shape2)

if (sum(!invalid_geoms1) > 0) {
  cat("Invalid geometries in shapefile1:\n")
  print(which(!invalid_geoms1))
  filtered_shapefile2 <- st_make_valid(filtered_shapefile2)
}

if (sum(!invalid_geoms2) > 0) {
  cat("Invalid geometries in shapefile2:\n")
  print(which(!invalid_geoms2))
  shape2 <- st_make_valid(shape2)
}

# Simplify geometries for faster processing
filtered_shapefile2 <- st_simplify(filtered_shapefile2, dTolerance = 0.001)
shape2 <- st_simplify(shape2, dTolerance = 0.001)

# Print Coordinate Reference Systems (CRS) of shapefiles
print(st_crs(filtered_shapefile2))
print(st_crs(shape2))

# Transform the geometries of shape2 to match the CRS of filtered_shapefile2
target_crs <- st_crs(filtered_shapefile2)
shape2_transformed <- st_transform(shape2, crs = target_crs)

# Calculate the intersection of the two shapefiles
intersections <- st_intersection(filtered_shapefile2, shape2_transformed)
intersections$district_country_important <- intersections$district_country.1
intersections$area_intersection <- st_area(intersections)

# Add original area to the shapefiles and prepare for joining
filtered_shapefile2$area_original <- st_area(filtered_shapefile2)
shapefile1 <- as.data.frame(filtered_shapefile2)
intersections <- as.data.frame(intersections)
common_column <- "district_countryshape"
joined_shapefiles <- merge(intersections, shapefile1, by = common_column, all.x = TRUE)

# Filter intersections where the area is at least 50% or 30% of the original district
filtered_intersections50 <- joined_shapefiles[
  joined_shapefiles$area_intersection >= 0.5 * joined_shapefiles$area_original, ]
filtered_intersections50 <- filtered_intersections50[!duplicated(filtered_intersections50[, c("district_countryshape")]), ]
filtered_intersections50 <- filtered_intersections50[!duplicated(filtered_intersections50[, c("district_country_important")]), ]
filtered_intersections50_subset <- filtered_intersections50[, -which(names(filtered_intersections50) == "district_country")]
filtered_intersections50$district_country<-filtered_intersections50$district_countryimportant
joined_shapefiles_test <- anti_join(joined_shapefiles, filtered_intersections50, by = c("district_country_important"))
filtered_intersections30 <- joined_shapefiles_test[
  joined_shapefiles_test$area_intersection >= 0.3 * joined_shapefiles_test$area_original, ]
filtered_intersections30 <- filtered_intersections30[!duplicated(filtered_intersections30[, c("district_country_important")]), ]
filtered_intersections30_subset <- filtered_intersections30[, -which(names(filtered_intersections50) == "district_country")]
filtered_intersections30$district_country<-filtered_intersections30$district_countryimportant
# Write filtered data to Excel files
write_xlsx(filtered_intersections50, "filtered_intersections50_weekly.xlsx")
write_xlsx(filtered_intersections30, "filtered_intersections30_weekly.xlsx")

# Repeat process for annual data
file_path <- "annualreducedincidence.xlsx"

#file_path <- "testingannual.xlsx"
annualincidence_merge<-readxl::read_excel(file_path)


filtered_shapefile2 <- ADMN2 %>%
  right_join(annualincidence_merge, by = "district_country")
filtered_shapefile<-filtered_shapefile2[!duplicated(filtered_shapefile2[ , c("district_country")]),]
#shape2 <-st_read("Shapefile_improved.shp")
countries_list <- c(
  "Democratic Republic of the Congo"
)


# Filter the shapefile based on the list of countries
shape2 <- shape2[shape2$COUNTRY %in% countries_list, ]
shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")

# Check for invalid geometries in shapefile1
invalid_geoms1 <- st_is_valid(filtered_shapefile)

# Check for invalid geometries in shapefile2
invalid_geoms2 <- st_is_valid(shape2)
# Identify and print any invalid geometries
if (sum(invalid_geoms1) > 0) {
  cat("Invalid geometries in shapefile1:\n")
  print(which(invalid_geoms1))
}

if (sum(invalid_geoms2) > 0) {
  cat("Invalid geometries in shapefile2:\n")
  print(which(invalid_geoms2))
}

# Repair invalid geometries in shapefile1
shapefile1 <- st_make_valid(filtered_shapefile)

# Repair invalid geometries in shapefile2
shapefile2 <- st_make_valid(shape2)

shapefile1 <- st_simplify(shapefile1, dTolerance = 0.001)
shapefile2 <- st_simplify(shapefile2, dTolerance = 0.001)

print(st_crs(shapefile1))
print(st_crs(shapefile2))

# Choose the target CRS (replace "EPSG:4326" with your desired CRS)
target_crs <- st_crs(shapefile1)

# Transform the geometries of shapefile2 to match the CRS of shapefile1
shapefile2_transformed <- st_transform(shapefile2, crs = target_crs)
shapefile2_transformed$district_countryimportant <- shapefile2_transformed$district_country



# Calculate the intersection
intersections <- st_intersection(shapefile1, shapefile2_transformed)
#intersections$district_country_important <-intersections$district_country.1
# Calculate the area of each intersection
intersections$area_intersection <- st_area(intersections)

intersections$district_countryshape <- intersections$district_countrykeep
intersections<-intersections[ , c('GID_1','GID_0', 'GID_2','NAME_2','district_countryshape'
                                  ,'district_country','area_intersection','district_countryimportant')]

shapefile1$district_countryshape <- shapefile1$district_countrykeep
shapefile1$area_original <- st_area(shapefile1)
shapefile1<-shapefile1[ , c("ADM2_NAME.x","ADM1_NAME.x", "ADM0_NAME.x" ,'COUNTRY','district_country'
,'area_original',"geometry.x" )]


# Calculate the area of each original district in shapefile1


shapefile1<-as.data.frame(shapefile1)
intersections<-as.data.frame(intersections)
common_column <- "district_country"
joined_shapefiles <- merge(intersections, shapefile1, by =  common_column, all.x = TRUE)

library(units)

# Filter the intersections where the area is at least 50% of either district
filtered_intersections50 <- joined_shapefiles[
  joined_shapefiles$'area_intersection' >= 0.5 * joined_shapefiles$'area_original', ]
filtered_intersections50 <-filtered_intersections50 [!duplicated(filtered_intersections50[ , c("district_countryshape")]),]


filtered_intersections50 <-filtered_intersections50 [!duplicated(filtered_intersections50[ , c("district_countryimportant")]),]
joined_shapefiles_test <- anti_join(joined_shapefiles, filtered_intersections50, by = c("district_countryimportant"))
filtered_intersections30 <- joined_shapefiles_test[
joined_shapefiles_test$'area_intersection' >= 0.3 * joined_shapefiles_test$'area_original', ]
filtered_intersections30 <-filtered_intersections30 [!duplicated(filtered_intersections30[ , c("district_countryimportant")]),]

# Count the number of qualifying intersections
num_overlapping_districts <- nrow(filtered_intersections50)
filtered_intersections50$epidemic <- 1
filtered_intersections30$epidemic <- 1
filtered_intersections50 <- filtered_intersections50[, -which(names(filtered_intersections50) == "district_country")]
filtered_intersections50$district_country<-filtered_intersections50$district_countryimportant
filtered_intersections30 <- filtered_intersections30[, -which(names(filtered_intersections30) == "district_country")]
filtered_intersections30$district_country<-filtered_intersections30$district_countryimportant

 library(writexl)
write_xlsx(filtered_intersections50, "filtered_intersections50_annual2.xlsx")
write_xlsx(filtered_intersections30, "filtered_intersections30_annual.xlsx")
