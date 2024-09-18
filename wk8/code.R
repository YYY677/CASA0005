
library(tidyverse)
"Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
remotes::install_github('r-tmap/tmap')"
library(tmap)
library(geojsonio)
library(ggplot2)
library(plotly)
#library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(carData)
library(car)
library(fs)
library(stats)
library(janitor)

#download a zip file containing some boundaries we want to use
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile="wk8/statistical-gis-boundaries-london.zip")
 
#Get the zip file and extract it
library(dplyr)
library(stringr)
library(fs)
listfiles<-dir_info(here::here()) %>%
  dplyr::filter(str_detect(path, ".zip")) %>%
  dplyr::select(path)%>%
  pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::unzip(exdir=here::here("prac7_data"))

#Look inside the zip and read in the .shp
Londonwards<-fs::dir_info(here::here("prac7_data", 
                                     "statistical-gis-boundaries-london", 
                                     "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  dplyr::pull()%>%
  #read in the file in
  sf::st_read()

#check the data
qtm(Londonwards)


#read in some attribute data
library(readr)
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               col_names = TRUE,
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'))
#or
LondonWardProfiles <- read_csv("ward-profiles-excel-version.csv",
                               col_names = TRUE,
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = "latin1"))


#check all of the columns have been read in correctly
library(tidyr)
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

"to merge the two together using a common ID. In this case, we can use 
the ward codes to achieve the join"
#merge boundaries and data
LonWardProfiles <- Londonwards%>%
  left_join(.,
            LondonWardProfiles, 
            by = c("GSS_CODE" = "New code")) %>% 
  distinct()

#let's map our dependent variable to see if the join has worked:
tmap_mode("plot")
qtm(LonWardProfiles, 
    fill = "Average GCSE capped point scores - 2014", 
    borders = NULL,  #color of border
    fill.palette = "Blues")



#8.4.1.2 Additional Data

london_schools <- read_csv("all_schools_xy_2016.csv",
                           na = c("", "NA", "n/a"))

Datatypelist2 <- london_schools %>% 
  summarise_all(class) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "colName",
    values_to = "value"
  )

"Let’s add some schools data as well. In the st_as_sf function x is longitude,
y is latitude.
from the coordinate values stored in the x and y columns, which look like 
they are latitude and longitude values, create a new points dataset"
#将文本文件转换为点数据，x y分别是文本文件中的列
lon_schools_sf <- st_as_sf(london_schools, 
                           coords = c("x","y"), 
                           crs = 4326)

lond_sec_schools_sf <- lon_schools_sf %>%
  filter(PHASE=="Secondary")

tmap_mode("plot")
qtm(lond_sec_schools_sf)


#8.5 Analysing GCSE exam performance - testing a research hypothesis

#8.5.2 Regression Basics
"这行代码生成了一个散点图对象 q，显示了两个变量之间的关系。"
q <- qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, 
           y = `Average GCSE capped point scores - 2014`, 
           data=LonWardProfiles)
"这行代码对之前生成的散点图 q 进行了进一步的调整和增强："
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()#添加抖动效果
#‘lm’ stands for ‘linear model’, a standard function for running linear regression models.

#8.5.3 Running a Regression Model in R

#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

#now model
#the tilde ~ symbol means “is modelled by”.
model1 <- Regressiondata %>%
  lm(average_gcse_capped_point_scores_2014 ~
       unauthorised_absence_in_all_schools_percent_2013,
     data=.)

#show the summary of those outputs
#多看几遍这个吧。
summary(model1)


#8.5.3.2 broom
"We can also use glance() from broom to get a bit more summary information, 
such as  r2and the adjusted r-squared value."
glance(model1)


#8.5.8 Assumption 1 - There is a linear relationship between the dependent 
#and independent variables

"one quick way to check that a linear relationship is probable is to look 
at the frequency distributions of the variables. If they are normally distributed, 
then there is a good chance that if the two variables are in some way correlated,
this will be a linear relationship."

#let's check the distribution of these variables first
ggplot(LonWardProfiles, aes(x=`Average GCSE capped point scores - 2014`)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)
"..density.. means that the histogram is a density plot, this plots the chance 
that any value in the data is equal to that value."

ggplot(LonWardProfiles, aes(x=`Unauthorised Absence in All Schools (%) - 2013`)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)



# from 21/10 there is an error on the website with 
# median_house_price_2014 being called median_house_price<c2>2014
# this was corrected around 23/11 but can be corrected with rename..

LonWardProfiles <- LonWardProfiles %>%
  #try removing this line to see if it works...
  dplyr::rename(median_house_price_2014 =`Median House Price (£) - 2014`)%>%
  janitor::clean_names()

ggplot(LonWardProfiles, aes(x=median_house_price_2014)) + 
  geom_histogram()

#plot the raw house price variable against GCSE scores, we get the following scatter plot:
qplot(x = median_house_price_2014, 
      y = average_gcse_capped_point_scores_2014, 
      data=LonWardProfiles)
#This indicates that we do not have a linear relationship,


#power transformation:一种数学方法，用于改变数据的分布，使其更接近正态分布，
#从而更容易进行统计分析。 

ggplot(LonWardProfiles, aes(x=log(median_house_price_2014))) + 
  geom_histogram()
"This looks a little more like a normal distribution, but it is still a little skewed."

"Fortunately in R, we can use the symbox() function in the car package 
to try a range of transfomations "
symbox(~median_house_price_2014, 
       LonWardProfiles, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

"Observing the plot above, it appears that raising our house price variable 
to the power of -1 should lead to a more normal distribution:"
ggplot(LonWardProfiles, aes(x=(median_house_price_2014)^-1)) + 
  geom_histogram()

qplot(x = (median_house_price_2014)^-1, 
      y = average_gcse_capped_point_scores_2014,
      data=LonWardProfiles)

#Compare this with the logged transformation:
qplot(x = log(median_house_price_2014), 
      y = average_gcse_capped_point_scores_2014, 
      data=LonWardProfiles)


#8.5.9 Assumption 2, The residuals in your model should be normally distributed

"We can access these values using augment() from broom which will 
add model output to the original GCSE data…"
model_data <- model1 %>%
  augment(., Regressiondata)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 



#8.5.10 Assumption 3 - No Multicolinearity in the independent variables

"we could extend model 1 into a multiple regression model by adding some 
more explanatory variables that we think could affect GSCE scores. "

Regressiondata2<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014)

#the tilde ~ symbol means “is modelled by”.
model2 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014), data = Regressiondata2)

#show the summary of those outputs
tidy(model2)
glance(model2)

#and for future use, write the residuals out
model_data2 <- model2 %>%
  augment(., Regressiondata2)

# also add them to the shapelayer
LonWardProfiles <- LonWardProfiles %>%
  mutate(model2resids = residuals(model2))

"Examining the output above, it is clear that including median house price 
into our model improves the fit from an r^2 of around 42% to an r^2 of 48%. 
Median house price is also a statistically significant variable."

#但是必须保证添加的自变量与原先自变量满足无多重共线性假设，否则我们实际上是在
#重复计算这些变量的影响，并夸大了它们的解释能力。

#using the corrr() pacakge，计算互相关性，要求小于0.8
library(corrr)
Correlation <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014) %>%
  mutate(median_house_price_2014 =log(median_house_price_2014))%>%
  correlate() %>%
  # just focus on unauthorised_absence_... and house prices
  focus(-average_gcse_capped_point_scores_2014, mirror = TRUE) 

#visualise the correlation matrix
rplot(Correlation)
"在理想的情况下，我们应该寻找小于0.8的相关性。查看相关矩阵或该矩阵的相关图，
很容易看出两个自变量之间的相关性很低(约30%)。"


#8.5.10.1 Variance Inflation Factor (VIF)
"Another way that we can check for Multicolinearity is to examine the VIF for 
the model. If we have VIF values for any variable exceeding 10, then 
we may need to worry and perhaps remove that variable from the analysis"
vif(model2)

position <- c(10:74)
Correlation_all<- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(position)%>%
  correlate()

rplot(Correlation_all)



#8.5.11 Assumption 4 - Homoscedasticity

"The best way to check for homo/hetroscedasticity is to plot the residuals 
in the model against the predicted values. We are looking for a cloud of points
with no apparent patterning to them."

#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)

"There is an easier way to produce this plot using check_model() from 
the performance package, that even includes what you are looking for…"
"The default argument is check=all but we can specify what to check for…see 
the arguments section in the documentation…e.g. check = c('vif', 'qq')"
library(performance)
check_model(model2, check="all")


#8.5.12 Assumption 5 - Independence of Errors
"This assumption simply states that the residual values (errors) in your model
must not be correlated in any way. If they are, then they exhibit autocorrelation
which suggests that something might be going on in the background 
that we have not sufficiently accounted for in our model."

"如果您在没有明确的空间或时间维度的数据上运行回归模型，那么自相关的标准测试将是
Durbin-Watson测试。"
"that ranges between 0 and 4, with 2 signifiying no autocorrelation. 
> 2 suggesting negative autocorrelation and and < 2 indicating postitve"
DW <- durbinWatsonTest(model2)
tidy(DW)
#the DW statistics for our model is 1.61, perhaps nothing to worry about.

#然而，我们正在使用空间引用数据，因此我们应该检查空间自相关。
#Moran’s I checking for spatial autocorrelation
#calculate the centroids of all Wards in London
coordsW <- LonWardProfiles%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

#Now we need to generate a spatial weights matrix 
#(remember from the lecture a couple of weeks ago). 
#We'll start with a simple binary matrix of queen's case neighbours

LWard_nb <- LonWardProfiles %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

plot(LWard_knn, st_geometry(coordsW), col="blue")

#create a spatial weights matrix object from these weights

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="W")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="W")

Queen <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

Nearest_neighbour <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen
Nearest_neighbour
#we can see that the Moran’s I statistic is somewhere between 0.27 and 0.29. 
"Remembering that Moran’s I ranges from between -1 and +1 (0 indicating 
no spatial autocorrelation) we can conclude that there is some weak to 
moderate spatial autocorrelation in our residuals."



#8.6 Spatial Regression Models
#Dealing with Spatially Autocorrelated Residuals - Spatial Lag and Spatial Error models

#8.6.1.1 The Spatial Lag (lagged dependent variable) model
#Original Model
model2 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014), data = LonWardProfiles)

tidy(model2)

#8.6.1.1.1 Queen’s case lag
library(spatialreg)
slag_dv_model2_queen <- lagsarlm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                                   log(median_house_price_2014), 
                                 data = LonWardProfiles, 
                                 nb2listw(LWard_nb, style="C"), 
                                 method = "eigen")
#what do the outputs show?
tidy(slag_dv_model2_queen)

#glance() gives model stats but this need something produced from a linear model
#here we have used lagsarlm()
glance(slag_dv_model2_queen)

t<-summary(slag_dv_model2_queen)

sum(t$residuals)

library(lmtest)
lrtest(slag_dv_model2_queen, model2)

#8.6.1.1.2 Lag impacts
#We can compute these direct and indirect effects using code f
# weight list is just the code from the lagsarlm
weight_list<-nb2listw(LWard_knn, style="C")

imp <- impacts(slag_dv_model2_queen, listw=weight_list)

imp


slag_dv_model2_queen_row <- lagsarlm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                                       log(median_house_price_2014), 
                                     data = LonWardProfiles, 
                                     nb2listw(LWard_nb, style="W"), 
                                     method = "eigen")


W <- as(weight_list, "CsparseMatrix")

trMatc <- trW(W, type="mult")
trMC <- trW(W, type="MC")

imp2 <- impacts(slag_dv_model2_queen_row, tr=trMatc, R=200)

imp3 <- impacts(slag_dv_model2_queen_row, tr=trMC, R=200)

imp2

imp3

















