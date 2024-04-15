
library(profvis)
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/landcover")

# read in landcover reaster data
landcover2<- raster("C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")

plot(landcover2)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents")
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
#crop landcover raster to africa continent shapefile
landcover2 <- crop(landcover2, shape)
#decrease resolution so that we are able to work with it
landcover2<- aggregate(landcover2, fact=10, fun = mean)


#Disables the usage of the s2 package for spatial indexing in R's sf packageand simplifies the geometry of the shape object while preserving topology.
sf_use_s2(FALSE)
raster2_indexed <- st_simplify(shape, preserveTopology = TRUE)
landcover2 <- mask(landcover2, raster2_indexed)
plot(landcover2)

library(plyr)
#extracts the most common landcover class for each district, turns this into a dataframe and then rounds values to nearest 10
cl2test<-data.frame(shape,extract(landcover2, raster2_indexed, fun=modal, na.rm = TRUE))
cl2test$zonesrounded<-round_any(cl2test$extract.landcover2..raster2_indexed..fun...modal..na.rm...TRUE., 10)  

#creates breakpoints to simplify number of land categories and assigns names
breakpoints <- c(0,40,120,130,140,
                 150,160,180,190,200,210,220 
)
category_names <- c("Cropland", 
                    "Tree cover",
                    "Shrubland",
                    "Grassland",
                    "Lichen and Moss",
                    "Sparse vegetation",
                    "Flooded areas",
                    "Urban areas",
                    "Bare areas",
                    "Water bodies",
                    "Permanent snow and ice")
                    
                    
         

# Create a new column 'new_land_category' with named land categories
# Simplified land categories are taken from European Space Agency 2017 African Land Cover Data https://www.esa.int/ESA_Multimedia/Images/2017/10/African_land_cover
cl2test$new_land_category <- cut(cl2test$zonesrounded, breaks = breakpoints, labels = category_names, include.lowest = TRUE)

# merges two datasets using a shared identifier, converts the result into a spatial dataframe, selects certain columns, and then removes duplicate rows.
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','new_land_category')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "Landcovertest.shp",append=FALSE)
