library(profvis)
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd(setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/Humidity"))

#read in vapor pressure and temperature to calculate absolute humidity
vapor_pressure = stack("cru_ts4.07.2001.2010.vap.dat.nc", "cru_ts4.07.2011.2020.vap.dat.nc", "cru_ts4.07.2021.2022.vap.dat.nc")
temperature = stack("cru_ts4.07.2001.2010.tmp.dat.nc", "cru_ts4.07.2011.2020.tmp.dat.nc", "cru_ts4.07.2021.2022.tmp.dat.nc")


setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")

Rd <- 287.05 # Specific gas constant for dry air (J/kg*K)

# Function to calculate absolute humidity
calculate_absolute_humidity <- function(vapor_pressure, temperature) {
  # Convert temperature from Celsius to Kelvin
  temp_K <- temperature + 273.15
  
  # Calculate absolute humidity
  ah <- (0.622 * vapor_pressure) / (Rd * temp_K)
  
  return(ah)
}


# Calculate absolute humidity
absolute_humidity <- calculate_absolute_humidity(vapor_pressure, temperature)
#crop and mask humidity raster data to africa ADMN2 shapefile
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- crop(absolute_humidity, shape)
allrasters <- mask(allrasters, shape)
allrasters [is.na(allrasters)] <- 0

#reduction of dimensionality of data
rpc <- rasterPCA(allrasters)
summary(rpc$model)
raster_stack <- stack(rpc$map)

#stack rasters account for 95% of data variance
test <- raster_stack[[1]]
test2 <- raster_stack[[2]]
files_stack <- stack(test,test2)
files_stack<-mask(files_stack, shape)

#extract k means for africa, we will go on to test different numbers of clusters
km<-as.matrix(files_stack)
num_na <- sum(is.na(km))
km[is.na(km)] <- mean(km, na.rm = TRUE)
num_na <- sum(is.na(km))

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
#conducts K-means clustering with 8 clusters on a dataset,employs hierarchical clustering
#visualizes the resulting clusters on a raster map and displays the dendrogram from hierarchical clustering.
set.seed(1)
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
#plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 1', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 2', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 7',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 5',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 4',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 6',0)))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


#merge shapefile and aero data to form logistic regression dataset for 8 clusters

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "Absolutehumidity8clusters.shp",append=FALSE)



#set seed makes this reproducible as kmeans clustering can vary
#conducts K-means clustering with 9 clusters on a dataset,employs hierarchical clustering
#visualizes the resulting clusters on a raster map and displays the dendrogram from hierarchical clustering.
set.seed(2)
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
#plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 5',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 7',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 4',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 9',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 2',0))))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


#merge shapefile and aero data to form logistic regression dataset for 9 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

rows_to_replace <- total3test$zonalcat %in% c("Class 1")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 2"

st_write(total3test, "Absolutehumidity9clusters.shp",append=FALSE)


#set seed makes this reproducible as kmeans clustering can vary
#conducts K-means clustering with 10 clusters on a dataset,employs hierarchical clustering
#visualizes the resulting clusters on a raster map and displays the dendrogram from hierarchical clustering.
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
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 8', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 7',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 9', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 10',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 1',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 5',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 2',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 3',0)))))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


#merge shapefile and aero data to form logistic regression dataset for 10 clusters)
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

rows_to_replace <- total3test$zonalcat %in% c("Class 4")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 5"

st_write(total3test, "Absolutehumidity10clusters.shp",append=FALSE)



#set seed makes this reproducible as kmeans clustering can vary
#conducts K-means clustering with 11 clusters on a dataset,employs hierarchical clustering
#visualizes the resulting clusters on a raster map and displays the dendrogram from hierarchical clustering.
set.seed(4)
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

library(RColorBrewer)
magma_like_palette <- brewer.pal(11, "Paired")

# Plot the rasterized column with a color scale
plot(r_cluster, col = magma_like_palette)

#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram

cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 10', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 6', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 5',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 1', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 9',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 2',0))))))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


#merge shapefile and aero data to form logistic regression dataset for 11 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
rows_to_replace <- total3test$zonalcat %in% c("Class 2")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 1"
table(total3test$zonalcat)
st_write(total3test, "Absolutehumidity11clusters.shp",append=FALSE)



#set seed makes this reproducible as kmeans clustering can vary
#conducts K-means clustering with 12 clusters on a dataset,employs hierarchical clustering
#visualizes the resulting clusters on a raster map and displays the dendrogram from hierarchical clustering.
set.seed(12)
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
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)
#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 2', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 3',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 10', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 1',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 9',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 11',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 12',
                                                                                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==12 , 'Class 5',0)))))))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


#merge shapefile and aero data to form logistic regression dataset for 12 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
rows_to_replace <- total3test$zonalcat %in% c("Class 12")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 1"

st_write(total3test, "Absolutehumidity12clusters.shp",append=FALSE)

