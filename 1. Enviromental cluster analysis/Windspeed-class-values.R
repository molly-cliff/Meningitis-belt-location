# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
library(raster)

# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")

# List and stack windspeed netCDF files
rastlist <- list.files(path = "windspeed", pattern='.nc$', full.names= TRUE)
allrasters <- stack(rastlist)
allrastersrotate <- rotate(allrasters)

# List and stack rainfall TIFF files
rastlist <- list.files(path = "Rainfall", pattern='.tif$', all.files= T, full.names= T)
rainfall <- stack(rastlist)

# Read in the coordinate reference system (CRS) from the rainfall data
africa_crs <- crs(rainfall)
allrastersrotate <- projectRaster(allrastersrotate, crs = africa_crs)

# Reduce the resolution of the windspeed raster
allrastersrotate2 <- aggregate(allrastersrotate, fact=5, fun = mean)

# Resample the windspeed raster
r_resampledrotate <- resample(allrastersrotate, allrastersrotate2, method = "bilinear")

# Read the shapefile
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

# Crop and mask the windspeed raster to the shapefile boundaries
allrastersrotate <- crop(r_resampledrotate, shape)
allrastersrotate <- mask(allrastersrotate, shape)

# Replace NA values in the raster with 0
allrastersrotate[is.na(allrastersrotate)] <- 0

# Perform PCA on the raster data for dimensionality reduction
rpc <- rasterPCA(allrastersrotate)
raster_stack <- stack(rpc$map)
summary(rpc$model)

# Extract the first two principal components
files_stack <- raster_stack[[1:2]]

# Prepare data for K-means clustering
km <- as.matrix(files_stack)
km[is.na(km)] <- mean(km, na.rm = TRUE)

# Determine optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow chart to find the optimal number of clusters
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

# Refine the cluster range and plot again
wss <- numeric(15)
for (i in 1:15) {
  kmeans_model <- kmeans(km, centers = i, nstart = 15)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the graph
plot(1:15, wss, type = "b", pch = 15, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares", main = "Windspeed K-means Cluster Testing")

# Highlight points 6-9
points(6:11, wss[6:11], col = "blue", pch = 15)

# Add a legend
legend("topright", legend = c("WSS", "Tested k-means clusters 6-11"), col = c("black", "blue"), pch = c(15, 15))



# Perform K-means clustering with 6 centers
set.seed(12)
kmeans_result <- kmeans(km, centers = 6)
cluster_labels <- kmeans_result$cluster
# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,6) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
# Assign hierarchical clustering labels
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 5', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 2', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             0 )))))))
# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)



# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
table(total3test$zonalcat)
st_write(total3test, "windspeed6classes.shp", append = FALSE)


# Perform K-means clustering with 7 centers
set.seed(120)
kmeans_result <- kmeans(km, centers = 7)
cluster_labels <- kmeans_result$cluster
# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,7) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)

# Assign hierarchical clustering labels
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 5', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 3', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 7', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 2',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 4',0))))))))

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)



# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
table(total3test$zonalcat)
st_write(total3test, "windspeed7classes.shp", append = FALSE)




# Perform K-means clustering with 8 centers
set.seed(121)
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster
# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,8) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)




# Assign hierarchical clustering labels
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 7', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 8', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 2', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 5',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',  0)))))))))

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)



# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
table(total3test$zonalcat)
st_write(total3test, "windspeed8classes.shp", append = FALSE)

# Perform K-means clustering with 9 centers
set.seed(4320)
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



# Extract most common cluster value for each district in Africa
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)


# Assign hierarchical clustering labels
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 8', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 4', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 9',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 5',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 2',
                                                                                                  
                                                                                                  
                                                                                                  
                                                                                                  0))))))))))
# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)



# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
table(total3test$zonalcat)
rows_to_replace <- total3test$zonalcat %in% c("Class 9")
total3test$zonalcat[rows_to_replace] <- "Class 8"

# Write to shapefile
st_write(total3test, "windspeed9classes.shp", append = FALSE)


## Perform K-means clustering with 10 centers

set.seed(420)
kmeans_result <- kmeans(km, centers = 10)
cluster_labels <- kmeans_result$cluster
# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,10) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



# Extract most common cluster value for each district in Africa
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)



# Assign hierarchical clustering labels
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 6', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 8', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 5',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 7',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 9',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 3',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 10',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 4',
                                                                                                         
                                                                                                         
                                                                                                         0)))))))))))


# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
table(total3test$zonalcat)
rows_to_replace <- total3test$zonalcat %in% c("Class 2")
total3test$zonalcat[rows_to_replace] <- "Class 1"

# Write to shapefile
st_write(total3test, "windspeed10classes.shp", append = FALSE)



## Perform K-means clustering with 11 centers
set.seed(420000)
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster
# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,11) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


# Extract most common cluster value for each district in Africa
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
# Assign hierarchical clustering labels
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 2',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 10', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 6',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 9',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 4',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 11',
                                                                                                                
                                                                                                                0))))))))))))
# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
table(total3test$zonalcat)
rows_to_replace <- total3test$zonalcat %in% c("Class 11")
total3test$zonalcat[rows_to_replace] <- "Class 10"

# Write to shapefile
st_write(total3test, "windspeed11classes.shp", append = FALSE)
