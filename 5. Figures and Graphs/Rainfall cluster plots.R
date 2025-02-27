library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(wesanderson)
library(RColorBrewer)

setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#Read in GADM Shapefile and Rainfall clusters
cl2test <-read_sf(dsn = ".", layer = "rainfallbilinear")
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")


setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Rainfall")


years <- 2002:2022
file_names <- paste0("chirps-v2.0_", years, ".01.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_jan <- calc(raster_stack, mean)
mean_jan<-data.frame(shape,extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.

file_names <- paste0("chirps-v2.0_", years, ".02.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_feb <- calc(raster_stack, mean)
mean_feb<-data.frame(shape,extract(mean_feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_feb$Feb<-mean_feb$extract.mean_feb..shape..fun...mean..na.rm...T..touches...TRUE.

#Mar
file_names <- paste0("chirps-v2.0_", years, ".03.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Mar <- calc(raster_stack, mean)
mean_Mar<-data.frame(shape,extract(mean_Mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Mar$Mar<-mean_Mar$extract.mean_Mar..shape..fun...mean..na.rm...T..touches...TRUE.

#Apr
file_names <- paste0("chirps-v2.0_", years, ".04.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Apr <- calc(raster_stack, mean)
mean_Apr<-data.frame(shape,extract(mean_Apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Apr$Apr<-mean_Apr$extract.mean_Apr..shape..fun...mean..na.rm...T..touches...TRUE.


#May
file_names <- paste0("chirps-v2.0_", years, ".05.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_May <- calc(raster_stack, mean)
mean_May<-data.frame(shape,extract(mean_May, shape, fun=mean, na.rm=T, touches=TRUE))
mean_May$May<-mean_May$extract.mean_May..shape..fun...mean..na.rm...T..touches...TRUE.

#June

file_names <- paste0("chirps-v2.0_", years, ".06.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_June <- calc(raster_stack, mean)
mean_June<-data.frame(shape,extract(mean_June, shape, fun=mean, na.rm=T, touches=TRUE))
mean_June$June<-mean_June$extract.mean_June..shape..fun...mean..na.rm...T..touches...TRUE.

#July
 years <- 2002:2021
file_names <- paste0("chirps-v2.0_", years, ".07.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_July <- calc(raster_stack, mean)
mean_July<-data.frame(shape,extract(mean_July, shape, fun=mean, na.rm=T, touches=TRUE))
mean_July$July<-mean_July$extract.mean_July..shape..fun...mean..na.rm...T..touches...TRUE.

#Aug
years <- 2002:2022
file_names <- paste0("chirps-v2.0_", years, ".08.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Aug <- calc(raster_stack, mean)
mean_Aug<-data.frame(shape,extract(mean_Aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Aug$Aug<-mean_Aug$extract.mean_Aug..shape..fun...mean..na.rm...T..touches...TRUE.
#Sep
file_names <- paste0("chirps-v2.0_", years, ".09.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Sep <- calc(raster_stack, mean)
mean_Sep<-data.frame(shape,extract(mean_Sep, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Sep$Sep<-mean_Sep$extract.mean_Sep..shape..fun...mean..na.rm...T..touches...TRUE.
#Oct
file_names <- paste0("chirps-v2.0_", years, ".10.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Oct <- calc(raster_stack, mean)
mean_Oct<-data.frame(shape,extract(mean_Oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Oct$Oct<-mean_Oct$extract.mean_Oct..shape..fun...mean..na.rm...T..touches...TRUE.
#Nov
file_names <- paste0("chirps-v2.0_", years, ".11.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Nov <- calc(raster_stack, mean)
mean_Nov<-data.frame(shape,extract(mean_Nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Nov$Nov<-mean_Nov$extract.mean_Nov..shape..fun...mean..na.rm...T..touches...TRUE.
#Dec
file_names <- paste0("chirps-v2.0_", years, ".12.tif")

raster_list <- lapply(file_names, raster)
raster_stack <- stack(raster_list)

mean_Dec <- calc(raster_stack, mean)
mean_Dec<-data.frame(shape,extract(mean_Dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_Dec$Dec<-mean_Dec$extract.mean_Dec..shape..fun...mean..na.rm...T..touches...TRUE.
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

mean_Mar <- subset(mean_Mar, COUNTRY != "Cabo Verde")
mean_Mar <- subset(mean_Mar, COUNTRY != "Mauritius")
mean_Mar <- subset(mean_Mar, COUNTRY != "Seychelles")
mean_Mar <- subset(mean_Mar, COUNTRY != "São Tomé and Príncipe")
mean_Mar <- subset(mean_Mar, COUNTRY != "Comoros")

mean_Apr <- subset(mean_Apr, COUNTRY != "Cabo Verde")
mean_Apr <- subset(mean_Apr, COUNTRY != "Mauritius")
mean_Apr <- subset(mean_Apr, COUNTRY != "Seychelles")
mean_Apr <- subset(mean_Apr, COUNTRY != "São Tomé and Príncipe")
mean_Apr <- subset(mean_Apr, COUNTRY != "Comoros")

mean_May <- subset(mean_May, COUNTRY != "Cabo Verde")
mean_May <- subset(mean_May, COUNTRY != "Mauritius")
mean_May <- subset(mean_May, COUNTRY != "Seychelles")
mean_May <- subset(mean_May, COUNTRY != "São Tomé and Príncipe")
mean_May <- subset(mean_May, COUNTRY != "Comoros")

mean_June <- subset(mean_June, COUNTRY != "Cabo Verde")
mean_June <- subset(mean_June, COUNTRY != "Mauritius")
mean_June <- subset(mean_June, COUNTRY != "Seychelles")
mean_June <- subset(mean_June, COUNTRY != "São Tomé and Príncipe")
mean_June <- subset(mean_June, COUNTRY != "Comoros")

mean_July <- subset(mean_July, COUNTRY != "Cabo Verde")
mean_July <- subset(mean_July, COUNTRY != "Mauritius")
mean_July <- subset(mean_July, COUNTRY != "Seychelles")
mean_July <- subset(mean_July, COUNTRY != "São Tomé and Príncipe")
mean_July <- subset(mean_July, COUNTRY != "Comoros")

mean_Aug <- subset(mean_Aug, COUNTRY != "Cabo Verde")
mean_Aug <- subset(mean_Aug, COUNTRY != "Mauritius")
mean_Aug <- subset(mean_Aug, COUNTRY != "Seychelles")
mean_Aug <- subset(mean_Aug, COUNTRY != "São Tomé and Príncipe")
mean_Aug <- subset(mean_Aug, COUNTRY != "Comoros")

mean_Sep <- subset(mean_Sep, COUNTRY != "Cabo Verde")
mean_Sep <- subset(mean_Sep, COUNTRY != "Mauritius")
mean_Sep <- subset(mean_Sep, COUNTRY != "Seychelles")
mean_Sep <- subset(mean_Sep, COUNTRY != "São Tomé and Príncipe")
mean_Sep <- subset(mean_Sep, COUNTRY != "Comoros")

mean_Oct <- subset(mean_Oct, COUNTRY != "Cabo Verde")
mean_Oct <- subset(mean_Oct, COUNTRY != "Mauritius")
mean_Oct <- subset(mean_Oct, COUNTRY != "Seychelles")
mean_Oct <- subset(mean_Oct, COUNTRY != "São Tomé and Príncipe")
mean_Oct <- subset(mean_Oct, COUNTRY != "Comoros")

mean_Nov <- subset(mean_Nov, COUNTRY != "Cabo Verde")
mean_Nov <- subset(mean_Nov, COUNTRY != "Mauritius")
mean_Nov <- subset(mean_Nov, COUNTRY != "Seychelles")
mean_Nov <- subset(mean_Nov, COUNTRY != "São Tomé and Príncipe")
mean_Nov <- subset(mean_Nov, COUNTRY != "Comoros")

mean_Dec <- subset(mean_Dec, COUNTRY != "Cabo Verde")
mean_Dec <- subset(mean_Dec, COUNTRY != "Mauritius")
mean_Dec <- subset(mean_Dec, COUNTRY != "Seychelles")
mean_Dec <- subset(mean_Dec, COUNTRY != "São Tomé and Príncipe")
mean_Dec <- subset(mean_Dec, COUNTRY != "Comoros")


#Subset dataframes
mean_jan<-mean_jan[ , c('GID_2', 'NAME_2','extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Feb<-mean_feb[ , c('GID_2', 'NAME_2','extract.mean_Feb..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Mar<-mean_Mar[ , c('GID_2', 'NAME_2','extract.mean_Mar..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Apr<-mean_Apr[ , c('GID_2', 'NAME_2','extract.mean_Apr..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_May<-mean_May[ , c('GID_2', 'NAME_2','extract.mean_May..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_June<-mean_June[ , c('GID_2', 'NAME_2','extract.mean_June..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_July<-mean_July[ , c('GID_2', 'NAME_2','extract.mean_July..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Aug<-mean_Aug[ , c('GID_2', 'NAME_2','extract.mean_Aug..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Sep<-mean_Sep[ , c('GID_2', 'NAME_2','extract.mean_Sep..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Oct<-mean_Oct[ , c('GID_2', 'NAME_2','extract.mean_Oct..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Nov<-mean_Nov[ , c('GID_2', 'NAME_2','extract.mean_Nov..shape..fun...mean..na.rm...T..touches...TRUE.')]
mean_Dec<-mean_Dec[ , c('GID_2', 'NAME_2','extract.mean_Dec..shape..fun...mean..na.rm...T..touches...TRUE.')]
cl2test<-cl2test[ , c('GID_2', 'NAME_2_x', 'zonalcat')]


#Bind monthly average and rainfall clusters into one data frame
test<-cbind(mean_jan,mean_feb,mean_Mar, mean_Apr, mean_May, mean_June,mean_July,mean_Aug,mean_Sep,mean_Oct,mean_Nov,mean_Dec,cl2test)
#Change row names
test$Jan<-test$extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.
test$Feb<-test$extract.mean_feb..shape..fun...mean..na.rm...T..touches...TRUE.
test$Mar<-test$extract.mean_Mar..shape..fun...mean..na.rm...T..touches...TRUE.
test$Apr<-test$extract.mean_Apr..shape..fun...mean..na.rm...T..touches...TRUE.
test$May<-test$extract.mean_May..shape..fun...mean..na.rm...T..touches...TRUE.
test$June<-test$extract.mean_June..shape..fun...mean..na.rm...T..touches...TRUE.
test$July<-test$extract.mean_July..shape..fun...mean..na.rm...T..touches...TRUE.
test$Aug<-test$extract.mean_Aug..shape..fun...mean..na.rm...T..touches...TRUE.
test$Sep<-test$extract.mean_Sep..shape..fun...mean..na.rm...T..touches...TRUE.
test$Oct<-test$extract.mean_Oct..shape..fun...mean..na.rm...T..touches...TRUE.
test$Nov<-test$extract.mean_Nov..shape..fun...mean..na.rm...T..touches...TRUE.
test$Dec<-test$extract.mean_Dec..shape..fun...mean..na.rm...T..touches...TRUE.
test$Zone<-test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
test$ADMN2_code<-test$GID_2
test$ADMN2_name<-test$NAME_2
# Load necessary libraries
library(tidyr)


# Select specific columns from the 'test' dataset
datasets <- test[, c('zonalcat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')]

# Group data by 'zonalcat' and calculate the mean for each group
df <- datasets %>%
  group_by(zonalcat) %>%
  summarise_all("mean")

# Remove the 7th row from the dataframe
df <- df[-7, ]

# Convert the dataset from wide to long format
data_long <- gather(df, month, Rainfall, Jan:Dec, factor_key = TRUE)
data_long

# Use a color palette from the 'wesanderson' package
test_palette <- wes_palette("FantasticFox1", n = 6, type = "continuous")

# Plot the data using ggplot2
ggplot(data_long, aes(x = month, y = Rainfall, group = zonalcat)) +
  geom_line(aes(color = zonalcat)) +
  geom_point(aes(color = zonalcat)) +
  scale_color_manual(values = test_palette) +
  labs(title = "Rainfall classes", x = "Month", y = "Monthly Atmospheric Precipitation (mm)") +
  theme(
    plot.title = element_text(size = 14),  # Adjust size of the plot title
    axis.title = element_text(size = 14),  # Adjust size of axis titles
    axis.text = element_text(size = 10)    # Adjust size of axis text
  )

# Plot 'zonalcat' column in the 'cl2test' dataset
plot(cl2test['zonalcat'])

# Use a ColorBrewer palette
palette <- brewer.pal(6, "Paired")

# Plot the data using ggplot2 with a different palette and increased element sizes
compare <- data_long %>%
  ggplot(aes(x = month, y = Rainfall, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1.5) +  # Increased line thickness
  geom_point(size = 4) +   # Increased point size
  labs(y = "Monthly Atmospheric Precipitation (mm)", x = "Month", colour = "Class", title = "Rainfall classes") +
  theme(
    text = element_text(size = 16),  # Increased base text size
    axis.title = element_text(size = 18),  # Increased axis title size
    axis.text = element_text(size = 14),  # Increased axis text size
    plot.title = element_text(size = 20),  # Increased plot title size
    legend.title = element_text(size = 16),  # Increased legend title size
    legend.text = element_text(size = 14)  # Increased legend text size
  )

# Plot the graph
print(compare)

# Convert 'zonalcat' based on 'Class' values using ifelse and convert to numeric
cl2test$zonal_cat_numeric <- ifelse(cl2test$zonalcat == "Class 1", as.numeric("1"),
                                    ifelse(cl2test$zonalcat == "Class 2", as.numeric("2"),
                                           ifelse(cl2test$zonalcat == "Class 3", as.numeric("3"),
                                                  ifelse(cl2test$zonalcat == "Class 4", as.numeric("4"),
                                                         ifelse(cl2test$zonalcat == "Class 5", as.numeric("5"),
                                                                ifelse(cl2test$zonalcat == "Class 6", as.numeric("6"), NA))))))

# Print the modified dataframe
print(cl2test)

# Extract the column you want to convert to raster
column_to_raster <- cl2test$zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(cl2test), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(cl2test, raster_template, field = column_to_raster)

# Plot the rasterized column using a color palette
magma_like_palette <- brewer.pal(6, "Paired")

plot(rasterized_column, col = magma_like_palette)
