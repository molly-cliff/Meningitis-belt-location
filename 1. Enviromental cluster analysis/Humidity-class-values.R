library(profvis)
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")

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


shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- crop(specific_humidity, shape)
allrasters <- mask(allrasters, shape)
allrasters [is.na(allrasters)] <- 0

#reduction of dimensionality of data
rpc <- rasterPCA(allrasters)
summary(rpc$model)
raster_stack <- stack(rpc$map)


test <- raster_stack[[1]]
test2 <- raster_stack[[2]]
files_stack <- stack(test,test2)
files_stack<-mask(files_stack, shape)
#extract k means for africa, to create 20 different clusters of rainfall

km<-as.matrix(files_stack)
num_na <- sum(is.na(km))
km[is.na(km)] <- mean(km, na.rm = TRUE)
num_na <- sum(is.na(km))


#####


####### testing here 
# Determine the optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow chart
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

# Add a line for the potential elbow point
abline(v = which(diff(wss) == max(diff(wss))) + 1, col = "red", lty = 2)



#set seed makes this reproducible as kmeans clustering can vary
set.seed(3)
kmeans_result <- kmeans(km, centers = 10)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,10) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)

cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 5', 
                                         ifelse(zonaltest == 2, 'Class 4', 
                                                ifelse(zonaltest == 3, 'Class 9',
                                                       ifelse(zonaltest == 4, 'Class 6', 
                                                              ifelse(zonaltest == 5, 'Class 2',
                                                                     ifelse(zonaltest == 6, 'Class 1',
                                                                            ifelse(zonaltest == 7, 'Class 10',
                                                                                   ifelse(zonaltest == 8, 'Class 3',
                                                                                          ifelse(zonaltest == 9, 'Class 7',
                                                                                                 ifelse(zonaltest == 10, 'Class 8', 0)))))))))))

cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)









st_write(total3test, "Specific-hum-10cluster.shp",append=FALSE)



#set seed makes this reproducible as kmeans clustering can vary
set.seed(3)
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)

cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 1', 
                                         ifelse(zonaltest == 2, 'Class 3', 
                                                ifelse(zonaltest == 3, 'Class 4',
                                                       ifelse(zonaltest == 4, 'Class 2', 
                                                              ifelse(zonaltest == 5, 'Class 8',
                                                                     ifelse(zonaltest == 6, 'Class 7',
                                                                            ifelse(zonaltest == 7, 'Class 5',
                                                                                   ifelse(zonaltest == 8, 'Class 9',
                                                                                          ifelse(zonaltest == 9, 'Class 6', 0))))))))))

cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)









st_write(total3test, "Specific-hum-9cluster.shp",append=FALSE)


#set seed makes this reproducible as kmeans clustering can vary
set.seed(3)
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,8) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)

cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 2', 
                                         ifelse(zonaltest == 2, 'Class 7', 
                                                ifelse(zonaltest == 3, 'Class 6',
                                                       ifelse(zonaltest == 4, 'Class 4', 
                                                              ifelse(zonaltest == 5, 'Class 8',
                                                                     ifelse(zonaltest == 6, 'Class 3',
                                                                            ifelse(zonaltest == 7, 'Class 1',
                                                                                   ifelse(zonaltest == 8, 'Class 5',0)))))))))

cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)









st_write(total3test, "Specific-hum-8cluster.shp",append=FALSE)



#set seed makes this reproducible as kmeans clustering can vary
set.seed(6)
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,11) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)

cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 6', 
                                         ifelse(zonaltest == 2, 'Class 7', 
                                                ifelse(zonaltest == 3, 'Class 1',
                                                       ifelse(zonaltest == 4, 'Class 4', 
                                                              ifelse(zonaltest == 5, 'Class 3',
                                                                     ifelse(zonaltest == 6, 'Class 10',
                                                                            ifelse(zonaltest == 7, 'Class 8',
                                                                                   ifelse(zonaltest == 8, 'Class 9',
                                                                                          ifelse(zonaltest == 9, 'Class 11',
                                                                                                 ifelse(zonaltest == 10, 'Class 2',
                                                                                                        ifelse(zonaltest == 11, 'Class 5', 0))))))))))))
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
rows_to_replace <- cl2test$zonalcat %in% c("Class 8")
cl2test$zonalcat[rows_to_replace] <- "Class 9"

# Merge with shape
cl2testraster <- merge(cl2test, shape, by = "GID_2")

# Convert to sf object
cl2testraster <- st_as_sf(cl2testraster)

# Select relevant columns
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicates based on GID_2
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]), ]

# Create a frequency table for zonalcat
table(total3test$zonalcat)

st_write(total3test, "Specific-hum-11cluster.shp",append=FALSE)













#set seed makes this reproducible as kmeans clustering can vary
set.seed(5)
kmeans_result <- kmeans(km, centers = 12)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,12) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
#plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)





#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)
#labelling zones, into the linked classes based on hc

cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 8', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 3',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 11',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 12',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 9',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 10',
                                                                                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==12 , 'Class 4',0)))))))))))))
#why are there so many NAs? data quality? how to fill in


cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


rows_with_missing <- cl2test[is.na(cl2test$zonalcat), ]


# here we should think about class distribution

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "specifichumidity12clusters.shp",append=FALSE)





