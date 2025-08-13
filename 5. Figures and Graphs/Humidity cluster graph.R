
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
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
library(raster)
Humiditycat <- read_sf(dsn = ".", layer = "Specific-hum-10cluster")

cl2test<-Humiditycat 
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/Humidity")


surface_pressure = stack("surface_pressure.nc")
surface_pressure<-rotate(surface_pressure)
dew_temp = stack("dew_temp.nc")
dew_temp<-rotate(dew_temp)

setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")

# Constants
a1 <- 611.2
a3 <- 17.67
T0 <- 273.15
a4 <- 29.65
Rdry <- 287  # J/kg/K
Rvap <- 461.5  # J/kg/K


# Function to calculate saturation water vapor pressure (es)
es <- function(Td) {
  return(a1 * exp(a3 * (Td - T0) / (Td - a4)))
}

# Function to calculate saturation specific humidity (qsat)
qsat <- function(Td, ps) {
  es_Td <- es(Td)
  return((Rdry / Rvap) * es_Td / (ps - ((1 - (Rdry / Rvap)) * es_Td)))
}

# Example usage
Td <- dew_temp  # Dew point temperature in Kelvin
ps <- surface_pressure  # Surface pressure in Pa
specific_humidity <- qsat(Td, ps)
print(specific_humidity)

rtesthum<-specific_humidity



#seperate out the layers into the respective months
Jan <- rtesthum[[c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241)]]
Feb <- rtesthum[[c(2, 14, 26, 38, 50, 62, 74, 86, 98, 110, 122, 134, 146, 158, 170, 182, 194, 206, 218, 230, 242)]]
Mar <- rtesthum[[c(3, 15, 27, 39, 51, 63, 75, 87, 99, 111, 123, 135, 147, 159, 171, 183, 195, 207, 219, 231, 243)]]
Apr <- rtesthum[[c(4, 16, 28, 40, 52, 64, 76, 88, 100, 112, 124, 136, 148, 160, 172, 184, 196, 208, 220, 232, 244)]]
May <- rtesthum[[c(5, 17, 29, 41, 53, 65, 77, 89, 101, 113, 125, 137, 149, 161, 173, 185, 197, 209, 221, 233, 245)]]
June <- rtesthum[[c(6, 18, 30, 42, 54, 66, 78, 90, 102, 114, 126, 138, 150, 162, 174, 186, 198, 210, 222, 234, 246)]]

July <- rtesthum[[c(7, 19, 31, 43, 55, 67, 79, 91, 103, 115, 127, 139, 151, 163, 175, 187, 199, 211, 223, 235, 247)]]

Aug <- rtesthum[[c(8, 20, 32, 44, 56, 68, 80, 92, 104, 116, 128, 140, 152, 164, 176, 188, 200, 212, 224, 236, 248)]]
Sep <- rtesthum[[c(9, 21, 33, 45, 57, 69, 81, 93, 105, 117, 129, 141, 153, 165, 177, 189, 201, 213, 225, 237, 249)]]
Oct <- rtesthum[[c(10, 22, 34, 46, 58, 70, 82, 94, 106, 118, 130, 142, 154, 166, 178, 190, 202, 214, 226, 238, 250)]]
Nov <- rtesthum[[c(11, 23, 35, 47, 59, 71, 83, 95, 107, 119, 131, 143, 155, 167, 179, 191, 203, 215, 227, 239, 251)]]
Dec <- rtesthum[[c(12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252)]]
#calcuating humidity average for each month

mean_jan <- calc(Jan, mean)
mean_jan<-data.frame(shape,extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE

mean_feb <- calc(Feb, mean)
mean_feb<-data.frame(shape,extract(mean_feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_feb$Feb<-mean_feb$extract.mean_feb..shape..fun...mean..na.rm...T..touches...TRUE

mean_mar <- calc(Mar, mean)
mean_mar<-data.frame(shape,extract(mean_mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_mar$Mar<-mean_mar$extract.mean_mar..shape..fun...mean..na.rm...T..touches...TRUE

mean_apr <- calc(Apr, mean)
mean_apr<-data.frame(shape,extract(mean_apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_apr$Apr<-mean_apr$extract.mean_apr..shape..fun...mean..na.rm...T..touches...TRUE

mean_may <- calc(May, mean)
mean_may<-data.frame(shape,extract(mean_may, shape, fun=mean, na.rm=T, touches=TRUE))
mean_may$May<-mean_may$extract.mean_may..shape..fun...mean..na.rm...T..touches...TRUE

mean_june <- calc(June, mean)
mean_june<-data.frame(shape,extract(mean_june, shape, fun=mean, na.rm=T, touches=TRUE))
mean_june$June<-mean_june$extract.mean_june..shape..fun...mean..na.rm...T..touches...TRUE

mean_july <- calc(July, mean)
mean_july<-data.frame(shape,extract(mean_july, shape, fun=mean, na.rm=T, touches=TRUE))
mean_july$July<-mean_july$extract.mean_july..shape..fun...mean..na.rm...T..touches...TRUE


mean_aug <- calc(Aug, mean)
mean_aug<-data.frame(shape,extract(mean_aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_aug$Aug<-mean_aug$extract.mean_aug..shape..fun...mean..na.rm...T..touches...TRUE



mean_sep <- calc(Sep, mean)
mean_sep<-data.frame(shape,extract(mean_sep, shape, fun=mean, na.rm=T, touches=TRUE))
mean_sep$Sep<-mean_sep$extract.mean_sep..shape..fun...mean..na.rm...T..touches...TRUE



mean_oct <- calc(Oct, mean)
mean_oct<-data.frame(shape,extract(mean_oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_oct$Oct<-mean_oct$extract.mean_oct..shape..fun...mean..na.rm...T..touches...TRUE


mean_nov <- calc(Nov, mean)
mean_nov<-data.frame(shape,extract(mean_nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_nov$Nov<-mean_nov$extract.mean_nov..shape..fun...mean..na.rm...T..touches...TRUE



mean_dec <- calc(Dec, mean)
mean_dec<-data.frame(shape,extract(mean_dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_dec$Dec<-mean_dec$extract.mean_dec..shape..fun...mean..na.rm...T..touches...TRUE
#filter layers

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

cl2test<-Humiditycat[ , c('GID_2', 'NAME_2_x', 'zonalcat')]
library(tidyr)
test<-cbind(mean_jan,mean_feb,mean_mar, mean_apr, mean_may, mean_june,mean_july,mean_aug,mean_sep,mean_oct,mean_nov,mean_dec,cl2test)
datasets<-test[ , c('zonalcat', 'Jan','Feb','Mar','Apr','May', 'June', 'July', 'Aug','Sep','Oct','Nov','Dec')]
datasets[is.na(datasets)] <- 0
library(dplyr)
#write out into dataframe, alongside the classes data
df <- datasets  %>% 
  group_by(zonalcat) %>%
  summarise_all("mean") 
df <- df %>%
  filter(zonalcat != 0)




df$zonalcat <- gsub("Class", "Cluster", df$zonalcat)


data_long <- gather(df, month, Humidity, Jan:Dec, factor_key=TRUE)
data_long
# Set the correct order for zonalcat
desired_order <- paste("Cluster", 1:10)
data_long$zonalcat <- factor(data_long$zonalcat, levels = desired_order)

palette <- brewer.pal(10, "Paired")

compare <- data_long %>%
  ggplot(aes(x = month, y = Humidity, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1.5) +  # Increased line thickness
  geom_point(size = 4) +   # Increased point size
  labs(y = "Specific humidity (g/m3)", x = "Month", colour = "Cluster", title = "Specific humidity clusters") +
  theme(
    text = element_text(size = 14),  # Increased base text size
    axis.title = element_text(size = 14),  # Increased axis title size
    axis.text = element_text(size = 14),  # Increased axis text size
    plot.title = element_text(size = 16),  # Increased plot title size
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.text = element_text(size = 14)  # Increased legend text size
  )

# Plot the graph
print(compare)

setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#this is essentially reading in already created layers and joining together

shape2 <-st_read("Shapefile_improved.shp")
#shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
Humiditycat$district_country <- paste(Humiditycat$NAME_2_x, Humiditycat$COUNTRY_x, sep = " ")
#create district intersection for weighting later

plot(Humiditycat['zonalcat'])
Humiditycat$zonal_cat_numeric <- ifelse(Humiditycat$zonalcat == "Class 1", as.numeric("1"),
                                    ifelse(Humiditycat$zonalcat == "Class 2", as.numeric("2"),
                                           ifelse(Humiditycat$zonalcat == "Class 3", as.numeric("3"),
                                                  ifelse(Humiditycat$zonalcat == "Class 4", as.numeric("4"),
                                                         ifelse(Humiditycat$zonalcat == "Class 5", as.numeric("5"),
                                                                ifelse(Humiditycat$zonalcat == "Class 6", as.numeric("6"),
                                                                       ifelse(Humiditycat$zonalcat == "Class 7", as.numeric("7"),
                                                                              ifelse(Humiditycat$zonalcat == "Class 8", as.numeric("8"),
                                                                                     ifelse(Humiditycat$zonalcat == "Class 9", as.numeric("9"), 
                                                                                            ifelse(Humiditycat$zonalcat == "Class 10", as.numeric("10"), NA))))))))))

column_to_raster <- Humiditycat$zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(Humiditycat), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(Humiditycat, raster_template, field = column_to_raster)

# Plot the rasterized column
library(RColorBrewer)
magma_like_palette <- brewer.pal(10, "Paired")

plot(rasterized_column, col=magma_like_palette)
