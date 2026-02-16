library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
Aerocat <- read_sf(dsn = ".", layer = "aerotest_10clusters9")
library(purrr)
library(R.utils)

path <-"C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/AOD"


all_files <- list.files(path, full.names = TRUE)


years <- 2002:2022

file_names <- unlist(lapply(years, function(y) {
  list.files(
    path,
    pattern = paste0("^", y, "01-.*\\.nc$"),
    full.names = TRUE
  )
}))




rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

shape<-shape2
# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_jan <- terra::mean(raster_stack)
mean_jan<-data.frame(shape,raster::extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$mean

file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "02-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})


# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_feb <- terra::mean(raster_stack)
mean_feb<-data.frame(shape,raster::extract(mean_feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_feb$Feb<-mean_feb$mean




file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "03-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})


# Stack into one multilayer raster
raster_stack <- rast(rast_list)
plot(raster_stack)

raster_stack <- raster_stack[[-16]]

raster_stack <- raster_stack[[-5]]
plot(raster_stack)
mean_mar <- terra::mean(raster_stack)
mean_mar<-data.frame(shape,raster::extract(mean_mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_mar$Mar<-mean_mar$mean


file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "04-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})


# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_apr <- terra::mean(raster_stack)
mean_apr<-data.frame(shape,raster::extract(mean_apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_apr$Apr<-mean_apr$mean




file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "05-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]


# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})


# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_may <- terra::mean(raster_stack)
mean_may<-data.frame(shape,raster::extract(mean_may, shape, fun=mean, na.rm=T, touches=TRUE))
mean_may$May<-mean_may$mean


file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "06-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})


# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_june <- terra::mean(raster_stack)
mean_june<-data.frame(shape,raster::extract(mean_june, shape, fun=mean, na.rm=T, touches=TRUE))
mean_june$June<-mean_june$mean



file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "07-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_july <- terra::mean(raster_stack)
mean_july<-data.frame(shape,raster::extract(mean_july, shape, fun=mean, na.rm=T, touches=TRUE))
mean_july$July<-mean_july$mean



file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "08-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]
# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

# Stack into one multilayer raster
raster_stack <- rast(rast_list)
raster_stack <- raster_stack[[-5]]
mean_aug <- terra::mean(raster_stack)
mean_aug<-data.frame(shape,raster::extract(mean_aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_aug$Aug<-mean_aug$mean





file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "09-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

raster_stack <- raster_stack[[-5]]
plot(raster_stack)
mean_sept <- terra::mean(raster_stack)
mean_sept<-data.frame(shape,raster::extract(mean_sept, shape, fun=mean, na.rm=T, touches=TRUE))
mean_sept$Sept<-mean_sept$mean






file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "10-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_oct <- terra::mean(raster_stack)
mean_oct<-data.frame(shape,raster::extract(mean_oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_oct$Oct<-mean_oct$mean



file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "11-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_nov <- terra::mean(raster_stack)
mean_nov<-data.frame(shape,raster::extract(mean_nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_nov$Nov<-mean_nov$mean


file_names <- sapply(years, function(y) {
  f <- grep(
    paste0("^", y, "12-.*\\.nc$"),
    basename(all_files),
    value = TRUE
  )
  if (length(f) == 0) return(NA_character_)
  file.path(path, f)
})

file_names <- file_names[!is.na(file_names)]

# Read each nc4 as a SpatRaster
rast_list <- lapply(file_names, function(f) {
  rast(f, subds = "AOD550")
})

# Stack into one multilayer raster
raster_stack <- rast(rast_list)

mean_dec <- terra::mean(raster_stack)
mean_dec<-data.frame(shape,raster::extract(mean_dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_dec$Dec<-mean_dec$mean





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



#filter to the key parts
mean_jan<-mean_jan[ , c('GID_2', 'NAME_2','Jan')]

mean_feb<-mean_feb[ , c('GID_2', 'NAME_2','Feb')]


mean_mar<-mean_mar[ , c('GID_2', 'NAME_2','Mar')]

mean_apr<-mean_apr[ , c('GID_2', 'NAME_2','Apr')]

mean_may<-mean_may[ , c('GID_2', 'NAME_2','May')]

mean_june<-mean_june[ , c('GID_2', 'NAME_2','June')]

mean_july<-mean_july[ , c('GID_2', 'NAME_2','July')]

mean_aug<-mean_aug[ , c('GID_2', 'NAME_2','Aug')]

mean_sept<-mean_sept[ , c('GID_2', 'NAME_2','Sept')]

mean_oct<-mean_oct[ , c('GID_2', 'NAME_2','Oct')]

mean_nov<-mean_nov[ , c('GID_2', 'NAME_2','Nov')]

mean_dec<-mean_dec[ , c('GID_2', 'NAME_2','Dec')]





#rows_to_replace <- Aerocat$zonalcat %in% c("Class 9")

# Replace values in those rows with new values
#Aerocat$zonalcat[rows_to_replace] <- "Class 8"

#rows_to_replace <- Aerocat$zonalcat %in% c("Class 10")

# Replace values in those rows with new values
#Aerocat$zonalcat[rows_to_replace] <- "Class 9"

cl2test<-Aerocat [ , c('GID_2', 'NAME_2_x', 'zonalcat')]

#write out into dataframe
library(dplyr)
test<-cbind(mean_jan,mean_feb,mean_mar, mean_apr, mean_may, mean_june,mean_july,mean_aug,mean_sept,mean_oct,mean_nov,mean_dec,cl2test)
datasets<-test[ , c('zonalcat', 'Jan','Feb','Mar','Apr','May', 'June', 'July', 'Aug','Sept','Oct','Nov','Dec')]
datasets[is.na(datasets)] <- 0
df <- datasets  %>% 
  group_by(zonalcat) %>%
  summarise_all("mean")
library(tidyr)
total3test_plot <- gather(df, month, class, "Jan":"Dec", factor_key=TRUE)

rows_to_replace <- total3test_plot$zonalcat %in% c("Class 9")

# Replace values in those rows with new values
total3test_plot$zonalcat[rows_to_replace] <- "Class 8"
total3test_plot
rows_to_replace <- total3test_plot$zonalcat %in% c("Class 10")

# Replace values in those rows with new values
total3test_plot$zonalcat[rows_to_replace] <- "Class 9"
total3test_plot
# Remove the 7th row from the dataframe
df <- df[-1, ]
library(tidyr)
# Convert the dataset from wide to long format
data_long <- gather(df, month, temp, Jan:Dec, factor_key = TRUE)
data_long
rows_to_replace <- data_long$zonalcat %in% c("Class 9")

# Replace values in those rows with new values
data_long$zonalcat[rows_to_replace] <- "Class 8"
data_long

rows_to_replace <- data_long$zonalcat %in% c("Class 10")

# Replace values in those rows with new values
data_long$zonalcat[rows_to_replace] <- "Class 9"
data_long
library(forcats)
palette <- brewer.pal(9, "Paired")
compare <- data_long %>%
  ggplot(aes(x = month, y = temp, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  labs(
    y = "Aerosol optical depth (AOD)",
    x = "Month",
    colour = "Class",
    title = "Aerosol optical depth classes"
  ) +
  scale_y_continuous(
    breaks = seq(0.25, 0.65, by = 0.05),
    limits = c(0.25, 0.65),        # ensures axis is drawn
    expand = c(0.02, 0.02)
  ) +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),  # space from plot
    plot.title = element_text(size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.margin = margin(10, 10, 10, 30)  # 🔑 extra left margin
  )

print(compare)



shape2 <-st_read("Shapefile_improved.shp")
#shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
Aerocat $district_country <- paste(Aerocat $NAME_2_x, Aerocat $COUNTRY_x, sep = " ")
#create district intersection for weighting later

plot(Aerocat ['zonalcat'])
Aerocat $zonal_cat_numeric <- ifelse(Aerocat $zonalcat == "Class 1", as.numeric("1"),
                                     ifelse(Aerocat $zonalcat == "Class 2", as.numeric("2"),
                                            ifelse(Aerocat $zonalcat == "Class 3", as.numeric("3"),
                                                   ifelse(Aerocat $zonalcat == "Class 4", as.numeric("4"),
                                                          ifelse(Aerocat $zonalcat == "Class 5", as.numeric("5"),
                                                                 ifelse(Aerocat $zonalcat == "Class 6", as.numeric("6"),
                                                                        ifelse(Aerocat $zonalcat == "Class 7", as.numeric("7"),
                                                                               ifelse(Aerocat $zonalcat == "Class 8", as.numeric("8"), 
                                                                                      ifelse(Aerocat $zonalcat == "Class 9", as.numeric("9"), NA)))))))))

column_to_raster <- Aerocat $zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(Aerocat ), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(Aerocat , raster_template, field = column_to_raster)

# Plot the rasterized column
library(RColorBrewer)
magma_like_palette <- brewer.pal(9, "Paired")

plot(rasterized_column, col=magma_like_palette)


