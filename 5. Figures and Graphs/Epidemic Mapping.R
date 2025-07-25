library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(raster)
library(lwgeom)
library(readxl)
library(RColorBrewer)
library(readxl)
library(tidyr)

setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#this is essentially reading in already created layers and joining together


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


magma_like_palette <- brewer.pal(3, "PuBu")

# Plot the rasterized column with a color scale
plot(rasterized_column, col = magma_like_palette)




setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
shape2 <- st_read("Shapefile_improved.shp")

data <- read_excel("outbreak_counts.xlsx")


plot(rasterized_column)

magma_like_palette <- brewer.pal(12, "PuBu")

# Plot the rasterized column with a color scale
plot(rasterized_column, col = magma_like_palette)
# Load required libraries

data<-data %>%
  group_by(district_country) %>%
  summarise(sum_outbreaks = sum(outbreaks_per_year, na.rm = TRUE))
shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
data_merge <- left_join(shape2, data, by = "district_country")

# Check district_country values in data but not in shape2
setdiff(data$district_country, shape2$district_country)



africa$epidemic_2 <- ifelse(africa$COUNTRY.x == "Democratic Republic of the Congo" & africa$epidemic == 1, 1,
                            ifelse(africa$epidemic == 1, 2,
                                   ifelse(africa$epidemic == 0, 0, NA)))





# Merge data frames by 'district_country' (adjust the key if needed)
data_merge <- left_join(africa, data, by = "district_country")

# Overwrite 'epidemic' with 'epidemic_flag' if epidemic_flag > 1, else keep original

data_merge <- data_merge %>%
  mutate(epidemic = ifelse(sum_outbreaks > 1, sum_outbreaks, epidemic),
         epidemic = ifelse(is.na(epidemic), 0, epidemic))

data_merge <-st_as_sf(data_merge )
column_to_raster <- data_merge$epidemic

# Create a raster template from the shapefile
raster_template <- raster(extent(data_merge ), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(data_merge, raster_template, field = column_to_raster)

# Plot the rasterized column
plot(rasterized_column,
    
     xlab = "Longitude",
     ylab = "Latitude",
     legend.args = list(text = "Number of meningitis epidemics", side = 4, line = 2.5, cex = 0.8),
     axes = TRUE,
     box = FALSE)

