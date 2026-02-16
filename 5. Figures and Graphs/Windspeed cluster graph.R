library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
windspeed <- read_sf(dsn = ".", layer = "windspeed5clusterstest")
library(purrr)
library(R.utils)

path <-"C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt"

library(raster)

rastlist <- list.files(
  path = "windspeed",
  pattern = "\\.nc$",
  full.names = TRUE
)

allrasters <- stack(rastlist)
allrasters <- rotate(allrasters)

layer_names <- names(allrasters)

# Extract month safely from names like X2002.01.01
months <- substr(layer_names, 7, 8)
allrastersrotate <- allrasters 
#layer_names <- names(allrastersrotate)



months <- sub("^X[0-9]{4}\\.([0-9]{2})\\..*$", "\\1", layer_names)

month_stacks <- lapply(sprintf("%02d", 1:12), function(m) {
  idx <- which(months == m)
  if (length(idx) == 0) return(NULL)
  allrastersrotate[[idx]]
})

names(month_stacks) <- month.abb


library(raster)
library(sf)

list2env(month_stacks, envir = .GlobalEnv)

mean_jan <- terra::mean(Jan)
mean_jan<-data.frame(shape,raster::extract(mean_jan, shape, fun=mean, na.rm=T, touches=TRUE))
mean_jan$Jan<-mean_jan$raster..extract.mean_jan..shape..fun...mean..na.rm...T..touches...TRUE.



mean_feb <- terra::mean(Feb)
mean_feb<-data.frame(shape,raster::extract(mean_feb, shape, fun=mean, na.rm=T, touches=TRUE))
mean_feb$Feb<-mean_feb$raster..extract.mean_feb..shape..fun...mean..na.rm...T..touches...TRUE.



mean_mar <- terra::mean(Mar)
mean_mar<-data.frame(shape,raster::extract(mean_mar, shape, fun=mean, na.rm=T, touches=TRUE))
mean_mar$Mar<-mean_mar$raster..extract.mean_mar..shape..fun...mean..na.rm...T..touches...TRUE.



mean_apr <- terra::mean(Apr)
mean_apr<-data.frame(shape,raster::extract(mean_apr, shape, fun=mean, na.rm=T, touches=TRUE))
mean_apr$Apr<-mean_apr$raster..extract.mean_apr..shape..fun...mean..na.rm...T..touches...TRUE.




mean_may <- terra::mean(May)
mean_may<-data.frame(shape,raster::extract(mean_may, shape, fun=mean, na.rm=T, touches=TRUE))
mean_may$May<-mean_may$raster..extract.mean_may..shape..fun...mean..na.rm...T..touches...TRUE.



mean_june <- terra::mean(Jun)
mean_june<-data.frame(shape,raster::extract(mean_june, shape, fun=mean, na.rm=T, touches=TRUE))
mean_june$June<-mean_june$raster..extract.mean_june..shape..fun...mean..na.rm...T..touches...TRUE.



Jul<-Jul[[-22]]

mean_july <- terra::mean(Jul)
mean_july<-data.frame(shape,raster::extract(mean_july, shape, fun=mean, na.rm=T, touches=TRUE))
mean_july$July<-mean_july$raster..extract.mean_july..shape..fun...mean..na.rm...T..touches...TRUE.



mean_aug <- terra::mean(Aug)
mean_aug<-data.frame(shape,raster::extract(mean_aug, shape, fun=mean, na.rm=T, touches=TRUE))
mean_aug$Aug<-mean_aug$raster..extract.mean_aug..shape..fun...mean..na.rm...T..touches...TRUE.




mean_sept <- terra::mean(Sep)
mean_sept<-data.frame(shape,raster::extract(mean_sept, shape, fun=mean, na.rm=T, touches=TRUE))
mean_sept$Sept<-mean_sept$raster..extract.mean_sept..shape..fun...mean..na.rm...T..touches...TRUE.




mean_oct <- terra::mean(Oct)
mean_oct<-data.frame(shape,raster::extract(mean_oct, shape, fun=mean, na.rm=T, touches=TRUE))
mean_oct$Oct<-mean_oct$raster..extract.mean_oct..shape..fun...mean..na.rm...T..touches...TRUE.





mean_nov <- terra::mean(Nov)
mean_nov<-data.frame(shape,raster::extract(mean_nov, shape, fun=mean, na.rm=T, touches=TRUE))
mean_nov$Nov<-mean_nov$raster..extract.mean_nov..shape..fun...mean..na.rm...T..touches...TRUE.




mean_dec <- terra::mean(Dec)
mean_dec<-data.frame(shape,raster::extract(mean_dec, shape, fun=mean, na.rm=T, touches=TRUE))
mean_dec$Dec<-mean_dec$raster..extract.mean_dec..shape..fun...mean..na.rm...T..touches...TRUE.


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

cl2test<-windspeed [ , c('GID_2', 'NAME_2_x', 'zonalcat')]
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



# Remove the 7th row from the dataframe
df <- df[-1, ]
library(tidyr)
# Convert the dataset from wide to long format
data_long <- gather(df, month, temp, Jan:Dec, factor_key = TRUE)
data_long


library(forcats)

compare <- data_long %>%
  ggplot(aes(x = month, y = temp, group = zonalcat, color = zonalcat)) +
  scale_color_manual(values = palette) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  labs(
    y = "Windspeed (m/s)",
    x = "Month",
    colour = "Class",
    title = "Windspeed classes"
  ) +
  scale_y_continuous(
    breaks = seq(1.9, 3, by = 0.1),
    limits = c(1.9, 3),        # ensures axis is drawn
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
    plot.margin = margin(10, 10, 10, 30)  # 
  )

print(compare)




windspeed <- read_sf(dsn = ".", layer = "windspeed5clusterstest")

shape2 <-st_read("Shapefile_improved.shp")
#shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
windspeed $district_country <- paste(windspeed$NAME_2_x, windspeed$COUNTRY_x, sep = " ")
#create district intersection for weighting later

plot(windspeed  ['zonalcat'])
windspeed  $zonal_cat_numeric <- ifelse(windspeed  $zonalcat == "Class 1", as.numeric("1"),
                                     ifelse(windspeed  $zonalcat == "Class 2", as.numeric("2"),
                                            ifelse(windspeed  $zonalcat == "Class 3", as.numeric("3"),
                                                   ifelse(windspeed  $zonalcat == "Class 4", as.numeric("4"),
                                                          ifelse(windspeed  $zonalcat == "Class 5", as.numeric("5"), NA)))))

column_to_raster <- windspeed  $zonal_cat_numeric

# Create a raster template from the shapefile
raster_template <- raster(extent(tempcat ), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(tempcat , raster_template, field = column_to_raster)

# Plot the rasterized column
library(RColorBrewer)
magma_like_palette <- brewer.pal(5, "Paired")

plot(rasterized_column, col=magma_like_palette)
