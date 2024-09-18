library(here)
library(janitor)
library(sf)
library(tidyverse)


LondonWards <- st_read(here::here("statistical-gis-boundaries-london", 
                                  "ESRI",
                                  "London_Ward.shp"))

LondonWardsMerged <- st_read(here::here("statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)


WardData <- read_csv("ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names()

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)


#have a look to check that it's 
#in the right projection
st_crs(LondonWardsMerged)


library(tmap)

#Now get the location of all Blue Plaques in the City direct from the web
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson") %>% 
  st_transform(.,27700)

tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

summary(BluePlaques)

# spatial subset 即在LondonwardMerged中的BluePlaques
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

#tmap的常见用法
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")


#7.6 Data manipulation
#st_intersects() will return a list of which points (plaques) are in each polygon
#(wards). So each row will be a polygon and the ID are the point IDs that intersect
#the polygon.
example<-st_intersects(LondonWardsMerged, BluePlaquesSub)
example1<-st_intersects(BluePlaquesSub,LondonWardsMerged)

example

check_example <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")

#Now we just take the length of each list per polygon and add this as new column…
library(sf)
points_sf_joined <- LondonWardsMerged%>%
  mutate(n = lengths(st_intersects(., BluePlaquesSub)))%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)


#How about a quick choropleth map to see how we are getting on…
points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")



#7.7 Weight matrix
library(spData)
library(sf)
library(spdep)

#First calculate the centroids of all Wards in London  centroid形心
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

summary(LWard_nb)
#Here it is telling us that the average number of neighbours is 5.88.

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)




#7.7.1 Matrix style

#create a spatial weights matrix from these weights
#B is the basic binary coding (1/0)
#W is row standardised (sums over all links to n)
#C is globally standardised (sums over all links to n)
#U is equal to C divided by the number of neighbours (sums over all links to unity)
#S is the variance-stabilizing coding scheme (sums over all links to n).

Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

Lward.lw2 <- LWard_nb %>%
  nb2mat(., style="W")

sum(Lward.lw)
sum(Lward.lw[1,])


#7.8 Autocorrelation
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

#Moran’s I test tells us whether we have clustered values (close to 1) 
#or dispersed values (close to -1
library(dplyr)

I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density


#Geary’s C tells us whether similar values or dissimilar values are clustering
C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

#Getis Ord tells us whether high or low values are clustering. 
#If G > Expected = High values clustering; if G < expected = low values clustering
G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

#7.8.4 Summary
#The Moran’s I statistic = 0.67 (remember 1 = clustered, 0 = no pattern, 
#-1 = dispersed) which shows that we have some distinctive clustering

#The Geary’s C statistic = 0.41 (remember Geary’s C falls between 0 and 2; 
#1 means no spatial autocorrelation, 
#<1 - positive spatial autocorrelation or similar values clustering,
#>1 - negative spatial autocorreation or dissimilar values clustering) 

#The General G statistic = G > expected, so high values are tending to cluster.


#7.8.5 Local Moran’s I
#use the localmoran function to generate I for each ward in the city

I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

#we can plot a map of the local Moran’s I outputs…
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

#Now create a new diverging colour brewer palette and reverse the order 
#using rev() (reverse) so higher values correspond to red
library(RColorBrewer)
MoranColours<- rev(brewer.pal(8, "RdGy"))

library(tmap)
tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")


#7.8.6 Local Getis Ord G

Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

#Add the Getis Ord  G data to the simple feature…
points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

library(RColorBrewer)

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")



#7.9 Other variables

#use head to see what other variables are in the data file
slice_head(points_sf_joined, n=2)


#Or print out the class of each column like we did in week 2, although we need to drop the geometry.
library(tidyr)
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist



I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")


#Now the Gi* statistic to look at clusters of high and low scores and explain 
#what the output map is showing and what other questions this can lead us 
#to ask next week…..

G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")

