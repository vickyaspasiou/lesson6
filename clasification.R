# Author: Vasiliki Aspasiou
# Data: 29/11/2013
#  Random Forest classification to 6 land covers,
## Landsat ETM+ bands 2,3,4,
rm(list = ls())

library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(randomForest)
library(rasta)

data(GewataB2)
data(GewataB3)
data(GewataB4)
data(vcfGewata)
data(lulcGewata)
data(LUTGewata)
trainingPoly<-readOGR("data", "trainingPoly")

# 1. a plot of the original lulcGewata raster with a meaningful legend 
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)

# 2. a plot of your training polygons
plot(trainingPoly, add=TRUE)

#NDVI 
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
dataType(ndvi) <- "INT2U"
names(ndvi) <- "NDVI"

# vcfGewata
vcfGewata[vcfGewata > 100] <- NA

#brick object to be used in RF classification
covs<-brick(GewataB2,GewataB3,GewataB4,ndvi,vcfGewata)


# Create Code field in trainingPoly
reclass <- function(x){
  which(x==levels(trainingPoly@data$class))
}
trainingPoly@data$Code <- sapply(trainingPoly@data$class, FUN=reclass)

# rasterize traingPoly based on code
classes <- rasterize(trainingPoly, ndvi, field='Code')
dataType(classes) <- "INT1U"
cols <- c("light green", "red2","brown","orange","dark green","light blue")

# mask the covs on the training poly
covmasked <- mask(covs, classes)

# add the classes layer to this new brick
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)

# extract all values into a matrix and convert to data.frame
valuetable <- getValues(trainingbrick)
valuetable <- as.data.frame(valuetable)
valuetable <- valuetable[!is.na(valuetable$class),]

# convert values in the class column to factors
valuetable$class <- factor(valuetable$class, levels = c(1:6))

# add a label column to valuetable
valuetable$label <- with(valuetable, ifelse(class==1, "bamboo",
                                            ifelse(class==2, "soi",
                                                   ifelse(class==3,"coffee",
                                                          ifelse (class==4,"cropland",
                                                                  ifelse (class==5, "forest", "wetland"))))))

valuetable <- na.omit(valuetable)

# construct a random forest model and save to .Rdata
modelRF <- randomForest(x=valuetable[,c(1:5)], y=valuetable$class,
                        importance = TRUE)
save(modelRF,file="modelRF.RData")

# predict land cover using the RF model
predLC <- predict(covs, model=modelRF, na.rm=TRUE)

# 3. the resulting thematic map with meaningful legend (as above)
plot(predLC, col=cols, legend=FALSE)
legend("bottomright", legend=c("bamboo", "soi", "coffee", "cropland","forest", "wetland"), fill=cols, bg="lightskyblue2")


# 4. a summary of the resulting randomForest model object
summary(modelRF)
class(modelRF)
str(modelRF)
names(modelRF)

# 5. the output OOB confusion matrix with accuracy per class (with correct row and column headings)
# the class with the highest accuracy is cropland and with the lowest is soil
colnames(modelRF$confusion) <- c("bamboo", "soi", "coffee", "cropland","forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("bamboo", "soi", "coffee", "cropland","forest", "wetland")
modelRF$confusion


# 6.the variable importance ranks (mean accuracy decrease and mean Gini coeficient decrease)
# the importance ranking of the input bands is the same with the 3 land cover classification
varImpPlot(modelRF)


