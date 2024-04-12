library(dplyr)

library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
#setwd("C:/Users/mvc32/Documents/OneDrive - University of Cambridge/Documents")


rastlist <- list.files(path = "Rainfall", pattern='.tif$', all.files= T, full.names= T)
#read in shapes
library(raster)

allrasters <- stack(rastlist)

allrasters<- aggregate(allrasters, fact=2, fun = mean)

shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- crop(allrasters, shape)
allrasters <- mask(allrasters, shape)
####we are subsetting as this takes ages to run and is very bad for memory

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

#Kmeans section
#any value with na in is given a 0 as to use in zonal analysis
                     
                     
allrasters<-files_stack                     
                     
                     
                     
km<-as.matrix(allrasters)
num_na <- sum(is.na(km))
km[is.na(km)] <- mean(km, na.rm = TRUE)
num_na <- sum(is.na(km))

wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow chart
plot(1:20, wss, type = "b", pch = 14, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

#set seed makes this reproducible as kmeans clustering can vary
set.seed(42)
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
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 3', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 4',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
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
#st_write(total3test, "Aerotestshapefinal2.shp",append=FALSE)
#st_write(total3test, "rainfallreducedclassesworking2.shp",append=FALSE)
st_write(total3test, "rainfalltestreducedclassesagainmissinggone.shp",append=FALSE)





#set seed makes this reproducible as kmeans clustering can vary
set.seed(432)
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
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 3',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 7', 
                                                                              0 ))))))))
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
#st_write(total3test, "Aerotestshapefinal2.shp",append=FALSE)
#st_write(total3test, "rainfallreducedclassesworking2.shp",append=FALSE)
st_write(total3test, "rainfall7clusters.shp",append=FALSE)




#set seed makes this reproducible as kmeans clustering can vary
set.seed(42342)
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
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 6',0 )))))))))
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
#st_write(total3test, "Aerotestshapefinal2.shp",append=FALSE)
#st_write(total3test, "rainfallreducedclassesworking2.shp",append=FALSE)
st_write(total3test, "rainfall8clusters.shp",append=FALSE)






#set seed makes this reproducible as kmeans clustering can vary
set.seed(222)
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


#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 2',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 6',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 9',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 5', 0 ))))))))))
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
#st_write(total3test, "Aerotestshapefinal2.shp",append=FALSE)
#st_write(total3test, "rainfallreducedclassesworking2.shp",append=FALSE)
st_write(total3test, "rainfall9clusters.shp",append=FALSE)






#set seed makes this reproducible as kmeans clustering can vary
set.seed(222)
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
#plot(r_cluster, col = ifelse(comparison, "green", "red"))
#allows for determination of which clusters are similar to each other for class labelling later
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,11) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(allrasters)
cl[] = cluster_labels 
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonalcat<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.


cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]