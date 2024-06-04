library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#this is essentially reading in already created layers and joining together

library(raster)

library(sf)
library(lwgeom)

library(wesanderson)
library(hrbrthemes) # for plot themes
library(gapminder) # for data
library(ggbump) # for the bump plot

#read in shapefile of africa
shape2 <-st_read("Shapefile_improved.shp")
shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/Disease_data")
file_path <- "totalweeklyepidemic.xlsx"
weeklyincidence_merge<-readxl::read_excel(file_path)
weeklyincidence_merge<-weeklyincidence_merge[!duplicated(weeklyincidence_merge[ , c("district_country")]),]



merged_data <- merge(shape2, weeklyincidence_merge, by = "district_country", all.x = TRUE)
#setwd("C:/Users/mvc32/Documents/Climate_meningitis_belt/Disease Data")
file_path <- "totalannualepidemic2.xlsx"
annualincidence_merge<-readxl::read_excel(file_path)
annualincidence_merge<-annualincidence_merge[!duplicated(annualincidence_merge[ , c("district_country")]),]
#shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
merged_data <- merge(merged_data, annualincidence_merge, by = "district_country", all.x = TRUE)


#Convert to sf object
africa <- st_as_sf(merged_data)

#Create epidemic column
africa$epidemic <- ifelse(africa$epidemic_annual == 1 | africa$epidemic_weekly == 1, 1, 0)
africa$epidemic[is.na(africa$epidemic)] <- 0

africa$epidemic_2 <- ifelse(africa$COUNTRY.x == "Democratic Republic of the Congo" & africa$epidemic == 1, 1,
                        ifelse(africa$epidemic == 1, 2,
                               ifelse(africa$epidemic == 0, 0, NA)))

africa1<-st_as_sf(africa)

# Extract the column you want to convert to raster
column_to_raster <- africa$epidemic_2

# Create a raster template from the shapefile
raster_template <- raster(extent(africa), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(africa, raster_template, field = column_to_raster)

# Plot the rasterized column
plot(rasterized_column)


library(RColorBrewer)
magma_like_palette <- brewer.pal(3, "PuBu")

# Plot the rasterized column with a color scale
plot(rasterized_column, col = magma_like_palette)
