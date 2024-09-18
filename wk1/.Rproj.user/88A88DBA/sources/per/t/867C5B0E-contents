library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)

#read in the shapefile
library(sf)
shape <- st_read(
  "statistical-gis-boundaries-london\\ESRI/London_Borough_Excluding_MHW.shp")

#To get a summary of the data held within the shapefile data 
summary(shape)

#To have a quick look what the shapefile looks like enter the following:
plot(shape)

#That plots everything in the shapefile (all the attributes) if you just wanted
#the geometry (outline of the shape) you could use…
library(sf)
shape %>% 
  st_geometry() %>%
  plot()

# read in the csv
library(tidyverse)
mycsv <- read_csv("fly-tipping-borough.csv", locale = locale(encoding = "GBK"), skip = 1)

# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="行标签")

#show the top 10 rows
shape%>%
  head(., n=10)

#let’s make a quick thematic map (or a qtm) using the package tmap.
library(tmap)
# set tmap to plot
tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "2012-13")


# Export data write to a .gpkg
library(sf)
shape %>%
  st_write(.,"C:/Users/86151/Desktop/GIS/wk1/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)

library(readr)
library(RSQLite)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="Rwk1.gpkg")

# list what is in it
con %>%
  dbListTables()

# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

# disconnect from it
con %>% 
  dbDisconnect()