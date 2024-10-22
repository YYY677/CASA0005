# 2.5.8
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
# this will take a few minutes
EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")

# this will take a few minutes
# geojson in local folder
#EW <- st_read(here::here("prac2_data",
#                         "Local_Authority_Districts__December_2015__Boundaries.geojson"))

# shapefile in local folder
EW <- st_read("Local_Authority_Districts_(December_2015)_Boundaries/LAD_Dec_2015_UGCB_GB.shp")

LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))

#plot it using the qtm function
qtm(LondonMap)


# 2.5.8.3 Attribute data
################################################
library(janitor)
LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)

## or
BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
###simple mapping
library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

library(OpenStreetMap)
tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


### 2.5.9 

library(tmap)
library(tmaptools)
library(tidyverse)

# this first bit of code is the same as in the practical documentation
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")


# this is an alternative option to code that includes the read_osm function
# here we use the tm_basemap function to create the basemap
# you can find out more:
?tm_basemap

# interactive map of Job seekers' Allowance Claimants
tmap_mode("view")
tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants")

Life_expectancy4 <- Life_expectancy4 %>% 
  clean_names(.)

# create life expectancy object for final plot
Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "new_code"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

# interactive map of Difference in life expectancy
# if you like, you can switch server = "OpenStreetMap" with server = "CartoDB.Positron"
# to get a different basemap for the next example
tmap_mode("view")
tm_shape(Life_expectancy4map) + 
  tm_polygons("u_kdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_basemap(server = "CartoDB.Positron") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy")

flytipping <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv")

#convert the tibble into a tidy tibble
flytipping_long <- flytipping %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )
# view the data
view(flytipping_long)

#an alternative which just pulls everything out into a single table
flytipping2 <- flytipping[,1:4]

#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )
view(flytipping_wide)

widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)




