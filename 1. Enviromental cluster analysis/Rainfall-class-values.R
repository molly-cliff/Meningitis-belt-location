library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
setwd("C:/Users/mvc32/Documents/OneDrive - University of Cambridge/Documents")

#read in and stack rainfall raster data
rastlist <- list.files(path = "Rainfall", pattern='.tif$', all.files= T, full.names= T)
allrasters <- stack(rastlist)
#reduce resolution so that we can work with data
allrasters<- aggregate(allrasters, fact=2, fun = mean)

#crop and mask aerosol raster data to africa ADMN2 shapefile
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- crop(allrasters, shape)
allrasters <- mask(allrasters, shape)

#reduction of dimensionality of data
rpc <- rasterPCA(allrasters)
summary(rpc$model)
beep(sound = 3, expr = NULL)
#take the principle component analysis comprise of around 95% of variability
#layer these to then create a raster stack to later be used in
#K means analysis to  divide each environmental raster into distinct clusters 
raster_stack <- stack(rpc$map)
test <- raster_stack[[1]]
test2 <- raster_stack[[2]]
test3 <- raster_stack[[3]]
test4 <- raster_stack[[4]]
test5 <- raster_stack[[5]]
test6 <- raster_stack[[6]]
test7 <- raster_stack[[7]]
test8 <- raster_stack[[8]]
test9 <- raster_stack[[9]]
test10 <- raster_stack[[10]]
test11 <- raster_stack[[11]]
test12 <- raster_stack[[12]]
test13 <- raster_stack[[13]]
test14 <- raster_stack[[14]]
test15 <- raster_stack[[15]]
test16 <- raster_stack[[16]]
test17 <- raster_stack[[17]]
test18 <- raster_stack[[18]]
test19 <- raster_stack[[19]]
test20 <- raster_stack[[20]]
test21 <- raster_stack[[21]]
test22 <- raster_stack[[22]]
test23 <- raster_stack[[23]]
test24 <- raster_stack[[24]]
test25 <- raster_stack[[25]]
test26 <- raster_stack[[26]]


files_stack <- stack(test,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19
                     ,test20,test21,test22,test23,test24,test25,test26)
files_stack<-mask(files_stack, shape)

#extract k means for africa, we will go on to test different numbers of clusters
# here we take our raster stack and turn it into a matrix
allrasters<-files_stack                     
km<-as.matrix(allrasters)
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
#abline(v = which(diff(wss) == max(diff(wss))) + 1, col = "red", lty = 2)

#set seed makes this reproducible as kmeans clustering can vary
set.seed(1)
kmeans_result <- kmeans(km, centers = 6)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
#plot(r_cluster, col = ifelse(comparison, "green", "red"))
#allows for determination of which clusters are similar to each other for class labelling later
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,6) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(allrasters)
cl[] = cluster_labels 
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 3', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 4',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                         0 )))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
#merge shapefile and rainfall data to form logistic regression dataset for 6 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "rainfall6clusers.shp",append=FALSE)


#set seed makes this reproducible as kmeans clustering can vary
set.seed(2)
kmeans_result <- kmeans(km, centers = 7)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
#plot(r_cluster, col = ifelse(comparison, "green", "red"))
#allows for determination of which clusters are similar to each other for class labelling later
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,7) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(allrasters)
cl[] = cluster_labels 
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 3',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 7', 
                                                                              0 ))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
#merge shapefile and rainfall data to form logistic regression dataset for 7 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "rainfall7clusters.shp",append=FALSE)




#set seed makes this reproducible as kmeans clustering can vary
set.seed(3)
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
#plot(r_cluster, col = ifelse(comparison, "green", "red"))
#allows for determination of which clusters are similar to each other for class labelling later
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,8) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(allrasters)
cl[] = cluster_labels 
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 6',0 )))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
#merge shapefile and rainfall data to form logistic regression dataset for 8 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "rainfall8clusters.shp",append=FALSE)


#set seed makes this reproducible as kmeans clustering can vary
set.seed(4)
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
#plot(r_cluster, col = ifelse(comparison, "green", "red"))
#allows for determination of which clusters are similar to each other for class labelling later
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(allrasters)
cl[] = cluster_labels 
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 2',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 6',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 9',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 5', 0 ))))))))))
#see how many nas there are 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
# get rid of islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
#merge shapefile and rainfall data to form logistic regression dataset for 9 clusters
cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
st_write(total3test, "rainfall9clusters.shp",append=FALSE)



