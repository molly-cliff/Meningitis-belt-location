library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(RColorBrewer)

setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#Read in GADM Shapefile and Rainfall clusters
tempcat <- read_sf(dsn = ".", layer = "temperature_10_clusters")
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")



years <- 2003:2022
path <- "~/INLA project/temperatuture"

all_files <- list.files(path, full.names = TRUE)

# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.01\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_jan <- terra::mean(raster_stack)
mean_jan<-data.frame(shape,raster::extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$mean





all_files <- list.files(path, full.names = TRUE)

# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.02\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_feb <- terra::mean(raster_stack)
mean_feb<-data.frame(shape,terra::extract(mean_feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_feb$feb<-mean_feb$mean





# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.03\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_mar <- terra::mean(raster_stack)
mean_mar<-data.frame(shape,raster::extract(mean_mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_mar$mar<-mean_mar$mean









# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.04\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_apr<-terra::mean(raster_stack)
mean_apr<-data.frame(shape,raster::extract(mean_apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_apr$apr<-mean_apr$mean











# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.05\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_may <- terra::mean(raster_stack)
mean_may<-data.frame(shape,raster::extract(mean_may, shape, fun=mean, na.rm=T, touches=TRUE))
mean_may$may<-mean_may$mean




# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.06\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_june <- terra::mean(raster_stack)
mean_june<-data.frame(shape,raster::extract(mean_june, shape, fun=mean, na.rm=T, touches=TRUE))
mean_june$june<-mean_june$mean





# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.07\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_july <- terra::mean(raster_stack)
mean_july<-data.frame(shape,raster::extract(mean_july, shape, fun=mean, na.rm=T, touches=TRUE))
mean_july$july<-mean_july$mean





# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.08\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_aug <- terra::mean(raster_stack)
mean_aug<-data.frame(shape,raster::extract(mean_aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_aug$aug<-mean_aug$mean





# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.09\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack<- rast(rast_list)

mean_sept <- terra::mean(raster_stack)
mean_sept<-data.frame(shape,raster::extract(mean_sept, shape, fun=mean, na.rm=T, touches=TRUE))
mean_sept$sept<-mean_sept$mean






# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.10\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_oct <- terra::mean(raster_stack)
mean_oct<-data.frame(shape,raster::extract(mean_oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_oct$oct<-mean_oct$mean





# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.11\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_nov <- terra::mean(raster_stack)
mean_nov<-data.frame(shape,raster::extract(mean_nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_nov$nov<-mean_nov$mean



# Find January 1 files for each year
file_names <- sapply(years, function(y) {
  f <- grep(paste0("^AIRS\\.", y, "\\.12\\.01"), basename(all_files), value = TRUE)
  if (length(f) == 0) return(NA)
  file.path(path, f)
})

# remove missing
file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, terra::rast)

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_dec <- terra::mean(raster_stack)
mean_dec<-data.frame(shape,raster::extract(mean_dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_dec$dec<-mean_dec$mean



#Subset each month to not include islands, as these are not included in final GADM shapefile analysis and have large amounts of missing data
mean_jan <- subset(mean_jan, COUNTRY != "Cabo Verde")
mean_jan <- subset(mean_jan, COUNTRY != "Mauritius")
mean_jan <- subset(mean_jan, COUNTRY != "Seychelles")
mean_jan <- subset(mean_jan, COUNTRY != "São Tomé and Príncipe")
mean_jan <- subset(mean_jan, COUNTRY != "Comoros")

mean_feb <- subset(mean_feb, COUNTRY != "Cabo Verde")
mean_feb <- subset(mean_feb, COUNTRY != "Mauritius")
mean_feb <- subset(mean_feb, COUNTRY != "Seychelles")
mean_feb <- subset(mean_feb, COUNTRY != "São Tomé and Príncipe")
mean_feb <- subset(mean_feb, COUNTRY != "Comoros")

mean_mar <- subset(mean_mar, COUNTRY != "Cabo Verde")
mean_mar <- subset(mean_mar, COUNTRY != "Mauritius")
mean_mar <- subset(mean_mar, COUNTRY != "Seychelles")
mean_mar <- subset(mean_mar, COUNTRY != "São Tomé and Príncipe")
mean_mar <- subset(mean_mar, COUNTRY != "Comoros")

mean_apr <- subset(mean_apr, COUNTRY != "Cabo Verde")
mean_apr <- subset(mean_apr, COUNTRY != "Mauritius")
mean_apr <- subset(mean_apr, COUNTRY != "Seychelles")
mean_apr <- subset(mean_apr, COUNTRY != "São Tomé and Príncipe")
mean_apr <- subset(mean_apr, COUNTRY != "Comoros")

mean_may <- subset(mean_may, COUNTRY != "Cabo Verde")
mean_may <- subset(mean_may, COUNTRY != "Mauritius")
mean_may <- subset(mean_may, COUNTRY != "Seychelles")
mean_may <- subset(mean_may, COUNTRY != "São Tomé and Príncipe")
mean_may <- subset(mean_may, COUNTRY != "Comoros")

mean_june <- subset(mean_june, COUNTRY != "Cabo Verde")
mean_june <- subset(mean_june, COUNTRY != "Mauritius")
mean_june <- subset(mean_june, COUNTRY != "Seychelles")
mean_june <- subset(mean_june, COUNTRY != "São Tomé and Príncipe")
mean_june <- subset(mean_june, COUNTRY != "Comoros")

mean_july <- subset(mean_july, COUNTRY != "Cabo Verde")
mean_july <- subset(mean_july, COUNTRY != "Mauritius")
mean_july <- subset(mean_july, COUNTRY != "Seychelles")
mean_july <- subset(mean_july, COUNTRY != "São Tomé and Príncipe")
mean_july <- subset(mean_july, COUNTRY != "Comoros")

mean_aug <- subset(mean_aug, COUNTRY != "Cabo Verde")
mean_aug <- subset(mean_aug, COUNTRY != "Mauritius")
mean_aug <- subset(mean_aug, COUNTRY != "Seychelles")
mean_aug <- subset(mean_aug, COUNTRY != "São Tomé and Príncipe")
mean_aug <- subset(mean_aug, COUNTRY != "Comoros")

mean_sept <- subset(mean_sept, COUNTRY != "Cabo Verde")
mean_sept <- subset(mean_sept, COUNTRY != "Mauritius")
mean_sept <- subset(mean_sept, COUNTRY != "Seychelles")
mean_sept <- subset(mean_sept, COUNTRY != "São Tomé and Príncipe")
mean_sept <- subset(mean_sept, COUNTRY != "Comoros")

mean_oct <- subset(mean_oct, COUNTRY != "Cabo Verde")
mean_oct <- subset(mean_oct, COUNTRY != "Mauritius")
mean_oct <- subset(mean_oct, COUNTRY != "Seychelles")
mean_oct <- subset(mean_oct, COUNTRY != "São Tomé and Príncipe")
mean_oct <- subset(mean_oct, COUNTRY != "Comoros")

mean_nov <- subset(mean_nov, COUNTRY != "Cabo Verde")
mean_nov <- subset(mean_nov, COUNTRY != "Mauritius")
mean_nov <- subset(mean_nov, COUNTRY != "Seychelles")
mean_nov <- subset(mean_nov, COUNTRY != "São Tomé and Príncipe")
mean_nov <- subset(mean_nov, COUNTRY != "Comoros")

mean_dec <- subset(mean_dec, COUNTRY != "Cabo Verde")
mean_dec <- subset(mean_dec, COUNTRY != "Mauritius")
mean_dec <- subset(mean_dec, COUNTRY != "Seychelles")
mean_dec <- subset(mean_dec, COUNTRY != "São Tomé and Príncipe")
mean_dec <- subset(mean_dec, COUNTRY != "Comoros")


#Subset dataframes


mean_jan$Jan<-mean_jan$mean
mean_jan<-mean_jan[ , c('GID_2', 'NAME_2','Jan')]


mean_feb$Feb<-mean_feb$mean
mean_feb<-mean_feb[ , c('GID_2', 'NAME_2','Feb')]


mean_mar$Mar<-mean_mar$mean
mean_mar<-mean_mar[ , c('GID_2', 'NAME_2','Mar')]



mean_apr$Apr<-mean_apr$mean
mean_apr<-mean_apr[ , c('GID_2', 'NAME_2','Apr')]



mean_may$May<-mean_may$mean
mean_may<-mean_may[ , c('GID_2', 'NAME_2','May')]




mean_june$June<-mean_june$mean
mean_june<-mean_june[ , c('GID_2', 'NAME_2','June')]



mean_july$July<-mean_july$mean
mean_july<-mean_july[ , c('GID_2', 'NAME_2','July')]


mean_aug$Aug<-mean_aug$mean
mean_aug<-mean_aug[ , c('GID_2', 'NAME_2','Aug')]


mean_sept$Sept<-mean_sept$mean
mean_sept<-mean_sept[ , c('GID_2', 'NAME_2','Sept')]


mean_oct$Oct<-mean_oct$mean
mean_oct<-mean_oct[ , c('GID_2', 'NAME_2','Oct')]



mean_nov$Nov<-mean_nov$mean
mean_nov<-mean_nov[ , c('GID_2', 'NAME_2','Nov')]


mean_dec$Dec<-mean_dec$mean
mean_dec<-mean_dec[ , c('GID_2', 'NAME_2','Dec')]



#Bind monthly average and rainfall clusters into one data frame
test <- data.frame(mean_jan, mean_feb, mean_mar, mean_apr, mean_may, mean_june,
                   mean_july, mean_aug, mean_sept, mean_oct, mean_nov, mean_dec,
                   tempcat)

# Rename columns for consistency
test <- test %>%
  mutate(
    ADMN2_code = GID_2,
    ADMN2_name = NAME_2
  )


datasets <- test[, c("zonalcat","Jan","Feb","Mar", "Apr", "May", "June",
                     "July", "Aug", "Sept", "Oct", "Nov", "Dec")]
# Group by 'zonalcat' and calculate the mean for each group
df <- datasets %>%
  group_by(zonalcat) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Remove the 7th row from the dataframe
df <- df[-11, ]
library(tidyr)
# Convert the dataset from wide to long format
data_long <- gather(df, month, temp, Jan:Dec, factor_key = TRUE)
data_long


# Use a ColorBrewer palette
palette <- brewer.pal(10, "Paired")

# Plot the data using ggplot2 with a different palette and increased element sizes
compare <- data_long %>%
  ggplot(aes(x = month, y = temp, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1.5) +  # Increased line thickness
  geom_point(size = 4) +   # Increased point size
  labs(y = "Temperature (K)", x = "Month", colour = "Class", title = "Temperature classes") +
  theme(
    text = element_text(size = 16),  # Increased base text size
    axis.title = element_text(size = 18),  # Increased axis title size
    axis.text = element_text(size = 14),  # Increased axis text size
    plot.title = element_text(size = 20),  # Increased plot title size
    legend.title = element_text(size = 16),  # Increased legend title size
    legend.text = element_text(size = 14)  # Increased legend text size
  )+ scale_y_continuous(breaks = seq(290, 305, by = 1))

# Plot the graph
print(compare)



tempcat <- read_sf(dsn = ".", layer = "temperature_10_clusters")

shape2 <-st_read("Shapefile_improved.shp")
#shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
tempcat $district_country <- paste(tempcat $NAME_2_x, tempcat $COUNTRY_x, sep = " ")
#create district intersection for weighting later

plot(tempcat ['zonalcat'])
tempcat $zonal_cat_numeric <- ifelse(tempcat $zonalcat == "Class 1", as.numeric("1"),
                                        ifelse(tempcat $zonalcat == "Class 2", as.numeric("2"),
                                               ifelse(tempcat $zonalcat == "Class 3", as.numeric("3"),
                                                      ifelse(tempcat $zonalcat == "Class 4", as.numeric("4"),
                                                             ifelse(tempcat $zonalcat == "Class 5", as.numeric("5"),
                                                                    ifelse(tempcat $zonalcat == "Class 6", as.numeric("6"),
                                                                           ifelse(tempcat $zonalcat == "Class 7", as.numeric("7"),
                                                                                  ifelse(tempcat $zonalcat == "Class 8", as.numeric("8"),
                                                                                         ifelse(tempcat $zonalcat == "Class 9", as.numeric("9"), 
                                                                                                ifelse(tempcat $zonalcat == "Class 10", as.numeric("10"), NA))))))))))

column_to_raster <- tempcat $zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(tempcat ), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(tempcat , raster_template, field = column_to_raster)

# Plot the rasterized column
library(RColorBrewer)
magma_like_palette <- brewer.pal(10, "Paired")

plot(rasterized_column, col=magma_like_palette)
