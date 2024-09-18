#first library a few packages that we will use during the practical
#note you may need to install them first...
library(spatstat)
library(here)
library(sp)
#library(rgeos)
#library(maptools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

##First, get the London Borough Boundaries
LondonBoroughs <- st_read(here::here("statistical-gis-boundaries-london",
                                     "ESRI", "London_Borough_Excluding_MHW.shp"))


library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

#Now get the location of all Blue Plaques in the City direct from the web
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/
                       open-plaques-london-2018-04-08.geojson") %>% 
  st_transform(.,27700)

summary(BluePlaques)


#plot the blue plaques in the city
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

#Data cleaning: remove any Plaques with the same grid reference as this will cause
#problems later on in the analysis..
#remove duplicates
library(tidyverse)

library(sf)
BluePlaques <- distinct(BluePlaques)

#Spatial subsetting   just select the points inside London
BluePlaquesSub <- BluePlaques[BoroughMap,]
#check to see that they've been removed
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# add sparse=false to get the complete matrix.
intersect_indices <-st_intersects(BoroughMap, BluePlaques)
#If you have used a graphic user interface GIS before, this is the same as select 
#by location, and as using filter from dplyr is the same as select by attribute.


# 6.5.3 Spatial joining
OSM <- st_read(here::here("greater-london-latest-free.shp", 
                          "gis_osm_pois_a_free_1.shp")) %>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')

Londonborough <- st_read(here::here("statistical-gis-boundaries-london",
                                     "ESRI", "London_Borough_Excluding_MHW.shp")) %>% 
  st_transform(.,27700)

join_example <-  st_join(OSM, Londonborough)

nrow(join_example)
#We see that there are 715 rows compared to 712 from last week. This is because 
#the hotels outside London are being included as all data on the left is retained….

# read in the .csv
# and make it into spatial data
Airbnb <- read_csv("listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')

# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(Londonborough,.) %>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)

# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

#left_join() but it won’t work with spatial data…the error message says to use st_join()
#left_join<- left_join(Hotels, Airbnb,
#                      by = c("GSS_CODE" = "GSS_CODE"))

#something that looks wrong, as the first 6 rows are all for the City of London
all_accomodation <- st_join(Hotels, Airbnb)
head(all_accomodation)

# st_equals() that returns the data based on the same geometries (or polygons)….
all_accomodation <- st_join(Hotels, Airbnb, join = st_equals)
head(all_accomodation)


# 6.5.6 Study area
#extract the borough

# select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)


#Next we need to clip our Blue Plaques so that we have a subset of just those 
#that fall within the borough or interest

#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("plot")
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# we’ll set this to the extent of the Harrow boundary
#window即相当于背景范围
window <- as.owin(Harrow)
plot(window)

#spatstat has its own set of spatial objects that it works with 

#create a sp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#create a ppp object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)

BluePlaquesSub@coords[,1] #coords的第一列

#Have a look at the new ppp object
BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")



#6.6 Point pattern analysis

#6.6.1 Kernel Density Estimation
BluePlaquesSub.ppp %>%
  density(., sigma=500) %>%
  plot()
#The sigma value sets the diameter of the Kernel 

#6.6.2 Quadrat Analysis
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")

#Using the same quadratcount() function again (for the same sized grid) 
#we can save the results into a table:
#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

Qcount %>% 
  summarise_all(class)

# calculating expected probabilities based on the Poisson distribution
#where:
#x is the number of occurrences
#λ is the mean number of occurrences
#e is a constant- 2.718

sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(!Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)
  
# k is the number of blue plaques counted in a square, in the first column
QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, 
       col="Blue", 
       type="o", 
       lwd=3)

#If our p-value is > 0.05 then this indicates that we have CSR and 
#there is no pattern in our points. If it is < 0.05, this indicates that
#we do have clustering in our points.
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
#查看数据表 p.value = 0.2594 说明符合随机性

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")
#In the new plot, we can see three figures for each quadrant. The top-left 
#figure is the observed count of points; the top-right is the Poisson expected 
#number of points; the bottom value is the residual value



#6.6.4 Ripley’s K
#We can conduct a Ripley’s K test on our data very simply with the spatstat 
#package using the kest() function.
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

Kval <- as.data.frame(Kest(BluePlaquesSub.ppp, correction = "Ripley"))


#6.7 Density-based spatial clustering of applications with noise: DBSCAN
library(raster)
library(fpc)

#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)


#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)


# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

library(ggplot2)
db
db$cluster

#We can now add this cluster membership info back into our dataframe
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)

#create some convex hull polygons to wrap around the points in our clusters.
chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])

# drop it(0) from the dataframe
chulls <- chulls %>%
  filter(dbcluster >=1)

# create a ggplot2 object from our data
dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()


###add a basemap
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()

#convert the basemap to British National Grid 
#这个包弄不好？为啥
library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "osm")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")


#autoplot(basemap_bng) sometimes works
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  



