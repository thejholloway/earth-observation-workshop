---
title: "Workshop:Earth Observation for Official Statistics - Working with raster data"
author: "Jacinta Holloway"
date: "16 May 2018"
output: html_document
---
  
#' Start by making sure that your working directory is properly set
#' If not you can set it using setwd()

(setwd("~/Documents/R"))
#'To check your working directory use getwd
getwd()

#'Install the RStoolbox package#'
#'RStoolbox contains tools and packages for remote sensing data analysis #'
install.packages("RStoolbox")
library(RStoolbox)
library(raster)


#'PART 1 : INTRODUCTION TO R AND WORKING WITH SATELLITE IMAGES AS RASTER FILES

#' Practical taken from notes by Wageningen UR 
#' https://geoscripting-wur.github.io/IntroToRaster/

#'A raster is a matrix of pixels organised into columns and rows (a grid) where each cell contains a value that represents information. 
#' Satellite images and aerial photographs come in raster format #'

#' Generate a RasterLayer object, which we will label 'r'. A RasterLayer is single layer raster as a variable #'
r <- raster(ncol=40, nrow=20)
class(r) 

#' Simply typing the object name displays its general properties / metadata
r

#' Now we will build on our RasterLayer object, 'r'
#' Let's first put some values in the cells of the layer
r[] <- rnorm(n=ncell(r))

#' A RasterStack is equivalent to  to multi-layer RasterLayer objects (like 'r') #'
#' Create a RasterStack object with 3 layers, called 's' #'
s <- stack(x=c(r, r*2, r))

#' The exact same procedure works for creating a RasterBrick, which refers to one multi-layer file or is a multi-layer object #'
#'Create a RasterBrick object with 3 layers, called 'b' #'
b <- brick(x=c(r, r*2, r))

#' Let's look at the properties of the RasterBrick object, 'b' #'
b

#'Download required data
#' We will be downloading a raster image, called gewata, which is saved as a TIF (Tagged Image Format) file #'

download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/gewata.zip', destfile = 'gewata.zip', method = 'auto')
#' In case the download code doesn't work, use method = 'wget'
#' Unpack the archive
unzip('gewata.zip')

#' To check the file downloaded successfully, you can list all the .tif files in the current directory #'
list.files (pattern =".tif")

#' Load the file into R using the brick function #'
gewata <- brick('LE71700552001036SGS00_SR_Gewata_INT1U.tif')

#'Display the properties of the gewata RasterBrick #'
gewata
#'The metadata tells us this object is 593 by 653 pixels in size and has 6 layers


#' Plot the first layer of the RasterBrick to see the image #'
plot(gewata, 1)

#' Create a regular extent, called 'e', to use to crop the gewata RasterBrick #'
e <- drawExtent(show=TRUE)

#'On the image, click twice, for the two opposite corners of the rectangle. 

#' Crop gewata using e
gewataSub <- crop(gewata, e)

#' Now visualise the new cropped object by plotting it
plot(gewataSub, 1)

#' In the next section we will create a NDVI object with multiple layers, using Landsat images taken on different dates. Each layer will correspond to a different date #'

#' Download and unzip the file called 'tura'
download.file(url='https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/tura.zip', destfile='tura.zip', method='auto')
unzip(zipfile='tura.zip')
#' Retrieve the content of the tura sub-directory. Create an object called 'list' #' 
#' 'List' will contain the file names of all the single layers we can put into the stack to create the NDVI object #'
list <- list.files(path='tura/', full.names=TRUE)

#' Plot the first layer. This will give us an NDVI layer with the clouds masked out #'
plot(raster(list[1]))

#' Stack all the layers in an object called turaStack #'
turaStack <- stack(list)
turaStack

#' To store this object, turaStack, on your computer for later use write this file at the root of the working directory #'
#' We will save it as a .grd file, which is a standard format for raster files in R #' 
writeRaster(x=turaStack, filename='turaStack.grd', datatype='INT2S')

#' Raster arithmetic can be performed is RasterLayers are identical in terms of extent, resolution, projection etc #'

#' Calculate the NDVI based on the gewata RasterBrick object we created earlier using arithmetic #'
#' NDVI = (NIR - Red) / (NIR + Red), with NIR being band 4 and Red being band 3 of Landsat 7 images #'

ndvi <- (gewata[[4]] - gewata[[3]]) / (gewata[[4]] + gewata[[3]])
#' Plot the NDVI #'
plot(ndvi)
#' NDVI takes values between 0 and 1, so the values in the plot are as expected #'

#' Although doing arithmetic did produce the result we wanted, for larger objects and more data it is better to use functions to perform calculations #'

#' Create a function to calculate NDVI #'
#' Define the function to calculate NDVI from 
ndvCalc <- function(x) {
  ndvi <- (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
  return(ndvi)
}
ndvi2 <- calc(x=gewata, fun=ndvCalc)

#' Plot ndvi2. This will produce the same result as the plot of ndvi #' 
plot(ndvi2)

#' Alternative function
ndvOver <- function(x, y) {
  ndvi <- (y - x) / (x + y)
  return(ndvi)
}
ndvi3 <- overlay(x=gewata[[3]], y=gewata[[4]], fun=ndvOver)

#' We currently don't know where this area we are plotting is on a map. To investigate this, we can project it in Google earth by using latitute and longitude #'
#' Obtain the latitude and longitude of our NDVI layer#'
#' One single line is sufficient to project any raster to any projection
ndviLL <- projectRaster(ndvi, crs='+proj=longlat')

#' Since this function will write a file to your working directory
#' You want to make sure that it is set where you want the file to be written, so check your current working directory #'
getwd()

#' It can be changed using setwd()

#' Save the latitude and longitude of the NDVI layer to a .kml file, because this file type is a Google earth format #'
KML(x=ndviLL, filename='gewataNDVI.kml')

#' Once you have saved the file, if you navigate to the file on your computer and double click, it will open in Google Earth and show you where this area appears on a map #'

#' Using cloud masks in R #'
#' A cloud mask is an extra raster layer, that contains information about presence or absence of cloud and shaowing effects from clouds #'
#'cfmask is the cloud mask of Landsat surface reflectance product#'
#' In this example,  we will use the cfmask layer to mask remaining clouds out of a Landsat image of Tahiti, French Polynesia #'


#' Download the data and unzip the file
download.file(url='https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/tahiti.zip', destfile='tahiti.zip', method='auto')
unzip(zipfile='tahiti.zip')

#' Load the data as a RasterBrick object called 'Tahiti' and look at its metadata
tahiti <- brick('LE70530722000126_sub.grd')
tahiti

#' The metadata tells us the Tahiti RasterBrick is 1014 by 1322 pixels and has 7 layers #'

#' Display names of each individual layer
names(tahiti)

#' Visualize the data
plotRGB(tahiti, 3,4,5)

#' Visualize the cloud mask layer (layer 7)
plot(tahiti, 7)

#'1 = water, 2 = cloud shadow, 3 = snow, 4 = cloud (see https://code.google.com/p/fmask/) #'


#' We can plot the cloud layer on top of the RGB image of Tahiti. 
#'To do this, we need to assign NA values to the 'clear land pixels' so they appear transparent on the overlay plot #'
#' Extract cloud layer from the brick
cloud <- tahiti[[7]]

#' Replace 'clear land' with 'NA'
cloud[cloud == 0] <- NA

#' Plot the stack and the cloud mask on top of each other
plotRGB(tahiti, 3,4,5)
plot(cloud, add = TRUE, legend = FALSE)

#' It is easier to work on the cloud mask as a separate RasterLayer, so we will extract it #'
#' Extract cloud mask RasterLayer into an object called 'fmask'
fmask <- tahiti[[7]]
#' Remove fmask layer from the Landsat stack and call it 'tahiti 6'
tahiti6 <- dropLayer(tahiti, 7)


#' We will first do the masking using simple vector arithmetic, as if tahiti6 and fmask were simple vectors. 
#' Since we are assigning NAs, we want to discard any value of the stack which has a corresponding cloud mask pixel different from 0. This can be done in one line of code below #'
#' Perform value replacement
tahiti6[fmask != 0] <- NA

#'This approach is only suitable if the objects and values you are working with are very small. 
#' This is often not the case with satellite iamgery, so we will use a replacement function instead #'

#' Define a value replacement function. x corresponds to the RasterBrick and y to the cloud mask #'
cloud2NA <- function(x, y){
  x[y != 0] <- NA
  return(x)
}

#' Let's create a new 6 layered object, called tahiti6_2, since tahiti6 has been masked already
tahiti6_2 <- dropLayer(tahiti, 7)

#' Apply the function on the two raster objects using overlay, creating an object called tahitiCloudFree
tahitiCloudFree <- overlay(x = tahiti6_2, y = fmask, fun = cloud2NA)

#' Visualize the output
plotRGB(tahitiCloudFree, 3,4,5)

#' In this plot the clouds are now gone. If you wanted to extend on this, you could use another image from another date to create a composited image to 'fill in the gaps' #'






