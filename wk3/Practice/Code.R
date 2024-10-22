library(terra)
library(tidyverse)
library(sf)
library(janitor)

world_cities <- read_csv('C:\\Users\\YU\\Desktop/World_Cities.csv')
australia_cities <- world_cities %>% 
  janitor::clean_names(.) %>% 
  filter(., cntry_name == 'Australia')

shape <- st_read("gadm41_AUS_shp/gadm41_AUS_0.shp")
# shape1 <- st_read("gadm41_AUS_shp/gadm41_AUS_1.shp")
# shape2 <- st_read("gadm41_AUS_shp/gadm41_AUS_2.shp")
plot(shape$geometry)

#we can simplify it first 
#dTolerance controls the level of generalisation
AusoutSIMPLE <- shape %>% 
  st_simplify(., dTolerance = 2000) %>% 
  st_geometry() %>% 
  plot()



ras <- terra::rast('wc2.1_2.5m_tmax_ACCESS-CM2_ssp370_2021-2040.tif')
# plot(ras)
plot(ras$tmax08)

site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")

AUcitypre <- terra::extract(ras, samples)

#Add the city names to the rows of AUcitytemp
AUcitypre2 <- AUcitypre %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "tmax01")


Melbournetmax <- AUcitypre2 %>% 
  filter(Site=="Melbourne") %>% 
  select(tmax01:tmax12)
#Or the row location:
Melbournepre <- AUcitypre2[2,3:14]

hist(as.numeric(Melbournetmax),
     col="red",
     main="xxx",
     xlab="monthly max temperature",
     ylab="frequency"
     )

# make sure that both of our layers are in the same 
# coordinate reference system when we combine them
print(shape)
#this works nicely for rasters
crs(ras)

#Perfect! Now let’s continue…
Austempmax <- shape %>%
  # now crop our temp data to the extent
  terra::crop(ras,.) #crop() 函数用于裁剪栅格数据

# plot(Austempmax)
plot(Austempmax[[1]])


#get raster data within the outline of the shape 
exactAus<-terra::mask(ras, shape)
plot(exactAus)

#subset using the known location of the raster
# hist(exactAus[[3]], col="red", main ="March temperature")
hist(Austempmax[[3]], col="red", main ="March temperature")

exactAusdf <- Austempmax %>%
  as.data.frame()

library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=tmax03)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(tmax03, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

#批量更改列名
names(exactAusdf) <- month


#plotting multiple months of temperature data
#select columns 1-12 (all the months) and place them in a new column called Month
squishdata <- exactAusdf %>% 
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

#selecting two months using filter() from dplyr
twomonths <- squishdata %>% 
  filter(., Month=="Jan" | Month=="Jun")

mean_two_month <- twomonths %>% 
  group_by(Month) %>% 
  summarise(Mean=mean(Temp, na.rm=TRUE))


#Select the colour and fill based on the variable
ggplot(twomonths, aes(x=Temp, colour=Month, fill=Month))+
  geom_histogram(position = "identity", alpha=0.5)+
  geom_vline(data = mean_two_month,
             aes(xintercept = Mean, color = Month),
             linetype="dashed")+
  labs(title = "ggplot2 histogram of australian jan and jun temperatures",
       x="temperature",
       y = "frequency")+
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
  labs(title = "ggplot2 faceted histogram of Australian temperatures",
       x = "Temperature",
       y = "Frequency")+
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
#set the binwidth
xbinsno <- list(star=0, end=40, size=2.5)

# plot the histogram calling all the variables we just set
ihist <- plot_ly(alpha = 0.6) %>% 
  add_histogram(x=jan$Temp,
                xbins=xbinsno, name="January") %>% 
  add_histogram(x=jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode="overlay", xaxis=x, yaxis=y)

ihist

###############################
#mean per month
meanofall <- squishdata %>% 
  group_by(Month) %>% 
  summarise(Mean=mean(Temp, na.rm=TRUE))

# standard deviation per month
sdofall <- squishdata %>% 
  group_by(Month) %>% 
  summarise(Sd=sd(Temp, na.rm=TRUE))
sdofall

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(Max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(Min = min(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotofstats <- squishdata %>% 
  group_by(Month) %>% 
  summarise(IQR=IQR(Temp, na.rm=TRUE),
            Max=max(Temp, na.rm=TRUE))

#whole year
meanwholeyear <- squishdata %>% 
  summarise(Meanyear=mean(Temp, na.rm=TRUE))







