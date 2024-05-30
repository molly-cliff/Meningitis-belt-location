library(dplyr)

library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
library(raster)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
rastlist <- list.files(path = "windspeed", pattern='.nc$', full.names= TRUE)
allrasters <- stack(rastlist)
allrastersrotate<-rotate(allrasters)


rastlist <- list.files(path = "Rainfall", pattern='.tif$', all.files= T, full.names= T)
#read in shapes
library(raster)

rainfall <- stack(rastlist)

africa_crs <- crs(rainfall)
allrastersrotate <- projectRaster(allrastersrotate, crs = africa_crs)
# Reduce resolution of the file
allrastersrotate2 <- aggregate(allrastersrotate, fact=5, fun = mean)
library(raster)

r_resampledrotate <- resample(allrastersrotate, allrastersrotate2, method = "bilinear")

shape <-read_sf(dsn = ".", layer = "Shapefile_improved")



allrastersrotate <- crop(r_resampledrotate, shape)
allrastersrotate <- mask(allrastersrotate, shape)


allrastersrotate[is.na(allrastersrotate)] <- 0

# Dimensionality reduction using PCA
rpc <- rasterPCA(allrastersrotate)
raster_stack <- stack(rpc$map)
summary(rpc$model)

# Extracting rasters comprising 95% of the data
files_stack <- raster_stack[[1:2]]



# Extract k-means for Africa
km <- as.matrix(files_stack)
km[is.na(km)] <- mean(km, na.rm = TRUE)

# Determine optimal number of clusters using elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot elbow chart
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

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







#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 5', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 2', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             0 )))))))
#why are there so many NAs? data quality? how to fill in
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
# here we should think about class distribution




cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

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




#we are trying this with different cluster sizes, 10 clusters, through to 6
#10 clusters
#set seed makes this reproducible as kmeans clustering can vary
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



#we are trying this with different cluster sizes, 11 clusters
#set seed makes this reproducible as kmeans clustering can vary
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
