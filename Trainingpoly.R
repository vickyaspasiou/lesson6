# Author: Vasiliki Aspasiou
# Data: 29/11/2013
#  This script will contain the training polygons (trainingPoly.shp)  


rm(list = ls())

library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(rasta)


# load in the training classes and Look-Up Table (LUT)
data(lulcGewata)
data(LUTGewata)

# plot lulcGewata with a meaningful legend 
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)


# draw a SpatialPolygons object in area purely represented by cropland

plot(lulcGewata, col=cols, legend=FALSE)

cropland <- drawPoly(sp=TRUE)
cropland <- gUnion(cropland, drawPoly(sp=TRUE))

#crs
projection(cropland) <- projection(lulcGewata)
projection(cropland)
plot(lulcGewata)
plot(cropland, add=TRUE)

# convert it to a SpatialPolygonsDataFrame
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
  class="cropland"), match.ID=FALSE)


# coffee

plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() 
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
coffee <- drawPoly(sp=TRUE)
projection(coffee) <- projection(lulcGewata)
# convert to SpatialPolygonsDataFrame
coffee <- SpatialPolygonsDataFrame(coffee, data=data.frame(
  class="coffee investment area"), match.ID=FALSE)

# bamboo

plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() 
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
bamboo <- drawPoly(sp=TRUE)
projection(bamboo) <- projection(lulcGewata)
# convert to SpatialPolygonsDataFrame
bamboo <- SpatialPolygonsDataFrame(bamboo, data=data.frame(
  class="bamboo plantations"), match.ID=FALSE)


# bare soil

plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() 
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
soil <- drawPoly(sp=TRUE)
soil <- gUnion(soil, drawPoly(sp=TRUE))
projection(soil) <- projection(lulcGewata)
# convert to SpatialPolygonsDataFrame
soil <- SpatialPolygonsDataFrame(soil, data=data.frame(
  class="bare soil"), match.ID=FALSE)

# forest

plot(lulcGewata, col=cols, legend=FALSE)
forest <- drawPoly(sp=TRUE)
forest <- gUnion(forest, drawPoly(sp=TRUE))
projection(forest) <- projection(lulcGewata)
# convert to SpatialPolygonsDataFrame
forest <- SpatialPolygonsDataFrame(forest, data=data.frame(
  class="forest area"), match.ID=FALSE)


# forest

plot(lulcGewata, col=cols, legend=FALSE)
wetland <- drawPoly(sp=TRUE)
projection(wetland) <- projection(lulcGewata)
# convert to SpatialPolygonsDataFrame
wetland <- SpatialPolygonsDataFrame(wetland, data=data.frame(
  class="wetland area"), match.ID=FALSE)

# merge the 6 polygons inro 1 SpatialPolygonsDataFrame object
cropland <- spChFIDs(cropland, "cropland")
forest <- spChFIDs(forest, "forest")
coffee <- spChFIDs(coffee, "coffee")
bamboo <- spChFIDs(bamboo, "bamboo")
soil <- spChFIDs(soil, "soil")
wetland <- spChFIDs(wetland, "wetland")
# now they can be bound (2 at a time) as one object using spRbind (maptools)
trainingPoly <- spRbind(cropland, forest)
trainingPoly <- spRbind(trainingPoly, coffee)
trainingPoly <- spRbind(trainingPoly, bamboo)
trainingPoly <- spRbind(trainingPoly, soil)
trainingPoly <- spRbind(trainingPoly, wetland)

# check
trainingPoly@data
plot(lulcGewata)
plot(trainingPoly, add=TRUE)

# save as .shp
writeOGR(trainingPoly, file.path("data"), "trainingPoly", driver="ESRI Shapefile")