library(sf)
library(here)
st_layers(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"))
#or
st_layers("gadm36_AUS_gpkg/gadm36_AUS.gpkg")

library(sf)
Ausoutline <- st_read(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')

#check that the coordinate reference systems of sf or sp objects 
#using the print function
print(Ausoutline)

library(sf)
st_crs(Ausoutline)$proj4string

Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

#Here we are changing from WGS84 to GDA94, which is a local CRS 
#for Australia and has the EPSG code 3112….
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)


#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

# Raster
# WorldClim data
library(sp)
library(raster)
library(terra)
jan<-terra::rast(here("wc2.1_5m_tavg", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan

# have a quick look at the data
plot(jan)

# ESRI:54009
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  terra::project(., "ESRI:54009")
plot(pr1)

# back to WGS84
pr1 <- pr1 %>%
  terra::project(., "EPSG:4326")
plot(pr1)


# look in our folder, find the files that end with .tif and 
library(fs)
dir_info("wc2.1_5m_tavg") # 比dir要详细一点，区别是

library(tidyverse)
listfiles<-dir_info("wc2.1_5m_tavg") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

#A SpatRaster is a collection of raster layers with 
#the same spatial extent and resolution.
worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp

# access the january layer
worldclimtemp[[1]]


#We can also rename our layers within the stack:
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#rename() from the dplyr package isn’t yet available for raster data 
names(worldclimtemp) <- month

#Now to get data for just January use our new layer name
worldclimtemp$Jan


#Raster location
#let’s make a dataframe of some sample sites — Australian cities/towns.
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)

#Add the city names to the rows of AUcitytemp
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")



#Part 2 descriptive statistics

Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#Or the row location:
Perthtemp <- Aucitytemp2[3,3:14]

#Make a histogram of Perth’s temperature.
hist(as.numeric(Perthtemp))


#That’s a pretty simple histogram, let’s improve the aesthetics a bit.
library(tidyverse)
#define where you want the breaks in the historgram
userbreak <- c(8,10,12,14,16,18,20,22,24,26)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

t<-Perthtemp %>%
  dplyr::select(Jan:Dec)

hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

#Check out the histogram information R generated
histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)

histinfo


#Using more data
plot(Ausoutline$geom)

#we can simplify it first 
#dTolerance controls the level of generalisation
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()


# make sure that both of our layers are in the same 
# coordinate reference system when we combine them
print(Ausoutline)
#this works nicely for rasters
crs(worldclimtemp)

#Perfect! Now let’s continue…
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.) #crop() 函数用于裁剪栅格数据

# plot the output
plot(Austemp)

#get raster data within the outline of the shape 
exactAus<-terra::mask(Austemp, Ausoutline)

#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")


#We need to make our raster into a data.frame to be compatible with ggplot2,
#using a dataframe or tibble
exactAusdf <- exactAus %>%
  as.data.frame()

library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))



#plotting multiple months of temperature data

#select columns 1-12 (all the months) and place them 
#in a new column called Month
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

#selecting two months using filter() from dplyr
twomonths <- squishdata %>%
filter(., Month=="Jan" | Month=="Jun") # | = OR

meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

#Select the colour and fill based on the variable
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


####################################################
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+ #对图形进行分面，每个月份将单独生成一个子图
  theme(plot.title = element_text(hjust = 0.5))


#How about an interactive histogram using plotly…
library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist

########################
# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE)) 

# print the top 1
head(meanofall, n=1)

# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))


