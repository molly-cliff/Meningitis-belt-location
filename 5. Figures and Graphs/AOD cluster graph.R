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
Aerocat <-read_sf(dsn = ".", layer = "Aero9clusters")
library(purrr)
library(R.utils)


shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/aero")

#folders containing the respective layers for each month are added in
april<- list.files(path = "Apr", pattern='.nc$', all.files= T, full.names= T)
may<- list.files(path = "May", pattern='.nc$', all.files= T, full.names= T)
june<- list.files(path = "June", pattern='.nc$', all.files= T, full.names= T)
july<- list.files(path = "July", pattern='.nc$', all.files= T, full.names= T)
aug<- list.files(path = "Aug", pattern='.nc$', all.files= T, full.names= T)
sep<- list.files(path = "Sep", pattern='.nc$', all.files= T, full.names= T)
oct<- list.files(path = "Oct", pattern='.nc$', all.files= T, full.names= T)
nov<- list.files(path = "Nov", pattern='.nc$', all.files= T, full.names= T)
dec<- list.files(path = "Dec", pattern='.nc$', all.files= T, full.names= T)
jan<- list.files(path = "Jan", pattern='.nc$', all.files= T, full.names= T)
feb<- list.files(path = "Feb", pattern='.nc$', all.files= T, full.names= T)
mar<- list.files(path = "Mar", pattern='.nc$', all.files= T, full.names= T)

#rastlist <- list.files(path = "/aero", pattern='.nc$', all.files= T, full.names= T)
#these are then stacked by each month
library(raster)
#allrasters <- stack(rastlist)
Apr <- stack(april)
May <- stack(may)
June<- stack(june)
July <- stack(july)
Aug <- stack(aug)
Sep <- stack(sep)
Oct <- stack(oct)
Nov <- stack(nov)
Dec <- stack(dec)
Jan <- stack(jan)
Feb<- stack(feb)
Mar <- stack(mar)

#calculated mean for each month
#new working data

detach("package:R.utils", unload=TRUE)
#crop and mask rasters for each month to the GADMN shapefile
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
Jan<- crop(Jan, shape)
Jan <- mask(Jan, shape)
#then calculate the average AOD across month of interest and turn into a dataframe
mean_jan <- calc(Jan, mean)
mean_jan<-data.frame(shape,extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE

#Do the same for each month: Feb
Feb<- crop(Feb, shape)
Feb <- mask(Feb, shape)
mean_feb <- calc(Feb, mean)
mean_feb<-data.frame(shape,extract(mean_feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_feb$Feb<-mean_feb$extract.mean_feb..shape..fun...mean..na.rm...T..touches...TRUE

#Mar
Mar<- crop(Mar, shape)
Mar <- mask(Mar, shape)
mean_mar <- calc(Mar, mean)
mean_mar<-data.frame(shape,extract(mean_mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_mar$Mar<-mean_mar$extract.mean_mar..shape..fun...mean..na.rm...T..touches...TRUE

#Apr
Apr<- crop(Apr, shape)
Apr <- mask(Apr, shape)
mean_apr <- calc(Apr, mean)
mean_apr<-data.frame(shape,extract(mean_apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_apr$Apr<-mean_apr$extract.mean_apr..shape..fun...mean..na.rm...T..touches...TRUE

#May
May<- crop(May, shape)
May <- mask(May, shape)
mean_may <- calc(May, mean)
mean_may<-data.frame(shape,extract(mean_may, shape, fun=mean, na.rm=T, touches=TRUE))
mean_may$May<-mean_may$extract.mean_may..shape..fun...mean..na.rm...T..touches...TRUE

#June
June<- crop(June, shape)
June <- mask(June, shape)
mean_june <- calc(June, mean)
mean_june<-data.frame(shape,extract(mean_june, shape, fun=mean, na.rm=T, touches=TRUE))
mean_june$June<-mean_june$extract.mean_june..shape..fun...mean..na.rm...T..touches...TRUE

#July
July<- crop(July, shape)
July <- mask(July, shape)
mean_july <- calc(July, mean)
mean_july<-data.frame(shape,extract(mean_july, shape, fun=mean, na.rm=T, touches=TRUE))
mean_july$July<-mean_july$extract.mean_july..shape..fun...mean..na.rm...T..touches...TRUE

#August
Aug<- crop(Aug, shape)
Aug <- mask(Aug, shape)
mean_aug <- calc(Aug, mean)
mean_aug<-data.frame(shape,extract(mean_aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_aug$Aug<-mean_aug$extract.mean_aug..shape..fun...mean..na.rm...T..touches...TRUE
#Fill in gaps in AOD with 0
mean_aug[is.na(mean_aug)] <- 0

#Sep
Sep<- crop(Sep, shape)
Sep <- mask(Sep, shape)
mean_sep <- calc(Sep, mean)
mean_sep<-data.frame(shape,extract(mean_sep, shape, fun=mean, na.rm=T, touches=TRUE))
mean_sep$Sep<-mean_sep$extract.mean_sep..shape..fun...mean..na.rm...T..touches...TRUE
#Fill in gaps in AOD with 0
mean_sep[is.na(mean_sep)] <- 0

#Oct
Oct<- crop(Oct, shape)
Oct <- mask(Oct, shape)
mean_oct <- calc(Oct, mean)
mean_oct<-data.frame(shape,extract(mean_oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_oct$Oct<-mean_oct$extract.mean_oct..shape..fun...mean..na.rm...T..touches...TRUE

#Nov
Nov<- crop(Nov, shape)
Nov <- mask(Nov, shape)
mean_nov <- calc(Nov, mean)
mean_nov<-data.frame(shape,extract(mean_nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_nov$Nov<-mean_nov$extract.mean_nov..shape..fun...mean..na.rm...T..touches...TRUE


#Dec
Dec<- crop(Dec, shape)
Dec <- mask(Dec, shape)
mean_dec <- calc(Dec, mean)
mean_dec<-data.frame(shape,extract(mean_dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_dec$Dec<-mean_dec$extract.mean_dec..shape..fun...mean..na.rm...T..touches...TRUE


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


mean_sep <- subset(mean_sep, COUNTRY != "Cabo Verde")
mean_sep <- subset(mean_sep, COUNTRY != "Mauritius")
mean_sep <- subset(mean_sep, COUNTRY != "Seychelles")
mean_sep <- subset(mean_sep, COUNTRY != "São Tomé and Príncipe")
mean_sep <- subset(mean_sep, COUNTRY != "Comoros")


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



#Filter mean data frame to key rows
mean_jan<-mean_jan[ , c('GID_2', 'NAME_2','Jan')]
mean_feb<-mean_feb[ , c('GID_2', 'NAME_2','Feb')]
mean_mar<-mean_mar[ , c('GID_2', 'NAME_2','Mar')]
mean_apr<-mean_apr[ , c('GID_2', 'NAME_2','Apr')]
mean_may<-mean_may[ , c('GID_2', 'NAME_2','May')]
mean_june<-mean_june[ , c('GID_2', 'NAME_2','June')]
mean_july<-mean_july[ , c('GID_2', 'NAME_2','July')]
mean_aug<-mean_aug[ , c('GID_2', 'NAME_2','Aug')]
mean_sep<-mean_sep[ , c('GID_2', 'NAME_2','Sep')]
mean_oct<-mean_oct[ , c('GID_2', 'NAME_2','Oct')]
mean_nov<-mean_nov[ , c('GID_2', 'NAME_2','Nov')]
mean_dec<-mean_dec[ , c('GID_2', 'NAME_2','Dec')]
cl2test<-Aerocat[ , c('GID_2', 'NAME_2_x', 'zonalcat')]

#Bind monthly average and AOD clusters into one data frame
library(dplyr)
test<-cbind(mean_jan,mean_feb,mean_mar, mean_apr, mean_may, mean_june,mean_july,mean_aug,mean_sep,mean_oct,mean_nov,mean_dec,cl2test)
#Filter rows accordingly
test<-test[ , c('NAME_2_x','zonalcat', 'Jan','Feb','Mar','Apr','May', 'June', 'July', 'Aug','Sep','Oct','Nov','Dec')]


# Total number of NA values in the dataset
na_counts <- sapply(test, function(x) sum(is.na(x)))
total_na_count <- sum(na_counts)
clean_data <- na.omit(test)
na_counts <- sapply(clean_data, function(x) sum(is.na(x)))
total_na_count <- sum(na_counts)

# Select specific columns from the dataset
datasets <- clean_data[, c('zonalcat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')]

# Group data by 'zonalcat' and calculate the mean for each group
df <- datasets %>%
  group_by(zonalcat) %>%
  summarise_all("mean")

# Reclassify the 'zonalcat' column
df$zonalcat <- gsub("Class 4", "Class 3", df$zonalcat)
df$zonalcat <- gsub("Class 5", "Class 4", df$zonalcat)
df$zonalcat <- gsub("Class 6", "Class 5", df$zonalcat)
df$zonalcat <- gsub("Class 7", "Class 6", df$zonalcat)
df$zonalcat <- gsub("Class 8", "Class 7", df$zonalcat)
df$zonalcat <- gsub("Class 9", "Class 8", df$zonalcat)

# Load the 'tidyr' package, loaded later due to package clashing ussyes
library(tidyr)

# Reshape the data from wide to long format
total3test_plot <- gather(df, Month, class, "Jan":"Dec", factor_key=TRUE)


# Define the ColorBrewer palette
palette <- brewer.pal(8, "Paired")

# Plotting code using ggplot2
compare <- total3test_plot %>%
  ggplot(aes(x = Month, y = class, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1) +  # Adjust the size parameter for thicker lines
  geom_point(size = 3) + # Add points with a size parameter to control their size
  labs(y = "AOD", x = "Month", colour = "Class", title = "AOD classes") +
  theme(text = element_text(size = 12))

# Plot the graph
plot(compare)

# Reclassify the 'zonalcat' column in the Aerocat dataset
Aerocat$zonalcat <- gsub("Class 4", "Class 3", Aerocat$zonalcat)
Aerocat$zonalcat <- gsub("Class 5", "Class 4", Aerocat$zonalcat)
Aerocat$zonalcat <- gsub("Class 6", "Class 5", Aerocat$zonalcat)
Aerocat$zonalcat <- gsub("Class 7", "Class 6", Aerocat$zonalcat)
Aerocat$zonalcat <- gsub("Class 8", "Class 7", Aerocat$zonalcat)
Aerocat$zonalcat <- gsub("Class 9", "Class 8", Aerocat$zonalcat)

# Plot the 'zonalcat' column
plot(Aerocat['zonalcat'])

# Create a numeric column based on the 'zonalcat' classification
Aerocat$zonal_cat_numeric <- ifelse(Aerocat$zonalcat == "Class 1", as.numeric("1"),
                                    ifelse(Aerocat$zonalcat == "Class 2", as.numeric("2"),
                                           ifelse(Aerocat$zonalcat == "Class 3", as.numeric("3"),
                                                  ifelse(Aerocat$zonalcat == "Class 4", as.numeric("4"),
                                                         ifelse(Aerocat$zonalcat == "Class 5", as.numeric("5"),
                                                                ifelse(Aerocat$zonalcat == "Class 6", as.numeric("6"),
                                                                       ifelse(Aerocat$zonalcat == "Class 7", as.numeric("7"),
                                                                              ifelse(Aerocat$zonalcat == "Class 8", as.numeric("8"),
                                                                                     ifelse(Aerocat$zonalcat == "Class 9", as.numeric("9"), NA)))))))))

# Assign the numeric column to a variable
column_to_raster <- Aerocat$zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(Aerocat), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(Aerocat, raster_template, field = column_to_raster)

# Plot the rasterized column using a color palette
library(RColorBrewer)
magma_like_palette <- brewer.pal(8, "Paired")

plot(rasterized_column, col = magma_like_palette)
magma_like_palette <- brewer.pal(8, "Paired")

plot(rasterized_column, col=magma_like_palette)
