#install the following packages:
install.packages("raster")
install.packages("rgdal")
install.packages("RStoolbox")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("maptools")
install.packages("sp")
install.packages("rgeos")
install.packages("ggmap")
install.packages("ggsn")
install.packages("maps")

#active the packages installed:
library(raster)
library(rgdal)  
library(RStoolbox)
library(ggplot2) 
library(randomForest)
library(maptools)
library(sp)
library(rgeos)
library(ggmap)
library(ggsn)
library(maps)

#set the working directory
setwd("C:/Users/User/Desktop/R Project")

#load both Landsat 5 and 8 scenes
LS_1984 <- brick("Walker_Lake/1984/1984_Composite/Tif/1984.tif")
LS_2018 <- brick("Walker_Lake/2018/2018_Composite/Tif/2018.tif")

#load the study area boundary shapefile
Study_area <- readOGR("Walker_Lake/Study_Area_Boundary/Boundary.shp")

#reproject the coordinate system of the shapefile
#it's required for further analysis that the shapefile has the same coordinate system as the layerstack
projection(LS_1984)
projection(LS_2018)
projection(Study_area)
Study_area <- spTransform(Study_area, CRS(proj4string(LS_1984)))

#set NA value to RASTER NA value. R will ignore this value in subsequence analysis.
NAvalue(LS_1984) <- 0
NAvalue(LS_2018) <- 0

#plot the study area on top of the Landsat image to see if the reprojection has worked using the plotRGB tool
plotRGB(LS_1984, r=1, g=4, b=7, stretch="lin")
plotRGB(LS_2018, r=5, g=6, b=4, stretch='lin')
plot(Study_area, col="green", add=TRUE)


#Resize Landsat scenes to the required study area using the crop function
StudyArea_1984 <- crop(LS_1984, Study_area)
StudyArea_2018 <- crop(LS_2018, Study_area)

#Plot the study area for the two scenes
plotRGB(StudyArea_1984,r=1, g=4, b=7, stretch="lin")
plotRGB(StudyArea_2018,r=5, g=6, b=4, stretch="lin")

#Stack both Landsat scenes
img <- stack(StudyArea_1984, StudyArea_2018)
plotRGB(img,r=1, g=4, b=7, stretch="lin")

#Load the training data that was created using QGIS
td <- shapefile("Walker_Lake/Training_Data/Training_Data.shp")
plot(td, col="yellow", add=T)

#Image classification of the two landsat scenes
beginCluster()
set.seed(7)

img1984_2018_rf = superClass(img = img, trainData = td, trainPartition = 0.7, 
                             responseCol = "Class_Name", nSamples = 500, areaWeightedSampling = TRUE,  
                             model = "rf", mode = "classification", verbose = TRUE)

set.seed(7)

img1984_2018_mlc = superClass(img = img, trainData = td, trainPartition = 0.7, 
                              responseCol = "Class_Name", nSamples = 500, areaWeightedSampling = TRUE,  
                              model = "mlc", mode = "classification", verbose = TRUE)

endCluster()

#Check levels of created land cover maps
levels(img1984_2018_rf$map)
levels(img1984_2018_mlc$map)

windows()

#Random Forest
ggR(img1984_2018_rf$map, layer = 1, geom_raster = TRUE,forceCat=TRUE)+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  scale_fill_manual(values = c("lightsalmon4", "chocolate1", "darkgreen", "burlywood3", "chartreuse", "blue"),
                    labels=img1984_2018_rf$classMapping$class,
                    name="class name:\n",
                    na.value = "NA",na.translate=FALSE
  )+
  xlab("")+
  ylab("")


north2(img1984_2018_rf$map, x = 0.1, y = 0.8, scale = 0.09, symbol = 1)
savePlot(filename = "img1984_2018_rf.jpg",type="jpg")

#Maximum likelihood classification
ggR(img1984_2018_mlc$map, layer = 1, geom_raster = TRUE,forceCat=TRUE)+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  scale_fill_manual(values = c("lightsalmon4", "chocolate1", "darkgreen", "burlywood3", "chartreuse", "blue"),
                    labels=img1984_2018_mlc$classMapping$class,
                    name="class name:\n",
                    na.value = "NA",na.translate=FALSE
  )+
  xlab("")+
  ylab("")

north2(img1984_2018_mlc$map, x = 0.1, y = 0.785, scale = 0.09, symbol = 1)
savePlot(filename="img1984_2018_mlc.jpg",type="jpg")
