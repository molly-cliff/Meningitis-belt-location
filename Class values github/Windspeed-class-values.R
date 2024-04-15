library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)


rastlist <- list.files(path = "windspeed", pattern='.nc$', all.files= T, full.names= T)
allrasters <- stack(rastlist)
allrasters<- aggregate(allrasters, fact=5, fun = mean)
shape <-read_sf(dsn = ".", layer = "Shapefile_improved")
files_stack <- crop(allrasters, shape)



files_stack <- mask(files_stack, shape)
rpc<-rasterPCA(files_stack)
summary(rpc$model)
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

files_stack <- stack(test,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12)

#                     
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
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 2',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 6',
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
st_write(total3test, "windspeedcluster6.shp",append=FALSE)


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
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
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
st_write(total3test, "windspeedcluster7.shp",append=FALSE)



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
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 6', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 4',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 1', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 5',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 2',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 8',
                                                                                    0 )))))))))

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



rows_to_replace <- total3test$zonalcat %in% c("Class 1")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 2"
st_write(total3test, "windspeedcluster8.shp",append=FALSE)

#we are trying this with different cluster sizes, 10 clusters, through to 6
#10 clusters
#set seed makes this reproducible as kmeans clustering can vary
set.seed(4320)
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 7',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 2', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 4',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 9',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 3',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 1',
                                                                                                  
                                                                                                         
                                                                                                         
                                                                                                         0))))))))))
#why are there so many NAs? data quality? how to fill in
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)




cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


rows_with_missing <- cl2test[is.na(cl2test$zonalcat), ]


# here we should think about class distribution

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]


rows_to_replace <- total3test$zonalcat %in% c("Class 4")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 5"

# Print the updated data frame
print(df)


# here we should think about class distribution
st_write(total3test, "windspeedfixed.shp",append=FALSE)


#we are trying this with different cluster sizes, 10 clusters, through to 6
#10 clusters
#set seed makes this reproducible as kmeans clustering can vary
set.seed(420)
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 9',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 7',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 3',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 5',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 10',
                                                                                                         
                                                                                                         
                                                                                                         0)))))))))))
#why are there so many NAs? data quality? how to fill in
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)




cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


rows_with_missing <- cl2test[is.na(cl2test$zonalcat), ]


# here we should think about class distribution

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
# here we should think about class distribution
st_write(total3test, "windspeed10clusters.shp",append=FALSE)


#set seed makes this reproducible as kmeans clustering can vary
set.seed(420)
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,11) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other
#detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 5',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 8', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 9',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 10',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 6',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 4',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 11',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 2',
                                                                                                                
                                                                                                                0))))))))))))
#why are there so many NAs? data quality? how to fill in
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)




cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")


rows_with_missing <- cl2test[is.na(cl2test$zonalcat), ]

# here we should think about class distribution

cl2testraster <- merge(cl2test,shape,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]
# here we should think about class distribution
st_write(total3test, "windspeed11clusters.shp",append=FALSE)






