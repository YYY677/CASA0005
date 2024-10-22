getwd()
ls()#查看当前环境的所有变量
# rm(xx) 移除xx变量

library(tidyverse)
mycsv <- read_csv("fly-tipping-borough.csv", locale = locale(encoding = "GBK"), skip = 1) 


A <- 1
B <- 2
C <- A+B
C

#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")

#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

df <- data.frame(Data1, Data2)
plot(df, col="green")

library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head()

df %>%
  tail

#Try the subsetting your df data frame with the following commands 
#to see what is returned:
df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)

# 1
df %>% 
  dplyr::select(column1)

# 2效果同上
df$column1

# 3效果同上
df[["column1"]]

#Old skool cleaning
LondonDataOSK<- read.csv("ward-profiles-excel-version.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")

#New skool cleaning

#a .csv file (directly from the web this time
#skipping over the 'n/a' entries as you go...
LondonDataOSK <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

class(LondonDataOSK)

#We can also use the class() function (from base R) within another two functions 
#summarise_all() (from dplyr) and pivot_longer() (from tidyr) to check that our 
#data has been read in correctly
library(dplyr)
library(tidyr)
Datatypelist <- LondonDataOSK %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))

#会发现包含n/a的列类型变成了str（对比datatypelist1和2）
Datatypelist2 <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

#If you ever wish to quickly edit data, then use edit()
LondonData <- edit(LondonDataOSK)

summary(df)

LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()

#We could select just the rows we need by explicitly specifying the range of rows
LondonBoroughs<-LondonData[626:658,]
#We can also do this with the slice() function 
library(dplyr)
LondonBoroughs<-LondonData%>%
  slice(626:658)

#查询列名！有时列名和老师给的有些许差异
colnames(LondonData)

#使用 make.names() 格式化列名
names(LondonData) <- make.names(names(LondonData), unique = TRUE)

#we could filter based on a variable,
Femalelifeexp<- LondonData %>% 
  filter(`Female.life.expectancy..2009.13`>90)

#查看总结
summary(Femalelifeexp$Female.life.expectancy..2009.13)

#xx can’t be filtered like this as it’s in a character format…
# we can use the function str_detect() from the stringr package in combination 
#with filter() from dplyr.
library(stringr)
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New.code`, "^E09"))

#Check it worked:
LondonBoroughs$`Ward.name`

#That’s also the same as:
LondonBoroughs %>% 
  dplyr::select(`Ward.name`) %>%
  print()

#You will notice that you will have two rows at the top for the City of London.
# That’s fine, extract only unique rows with distinct(), again from dplyr:
LondonBoroughs<-LondonBoroughs %>%
  distinct()


# Selecting columns
#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
#the same
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))

LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese..2011.12.to.2013.14"),
                contains("Ward.name")) 




#Renaming columns
LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward.name`)%>%
  clean_names()

#Janitor removes all capitals and uses an underscore wherever there is a space
library(janitor)
LondonBoroughs <- LondonBoroughs %>%
 #here the ., means all data
 clean_names(., case="big_camel")


#More dplyr verbs
#mutate
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (FemaleLifeExpectancy2009_13 +
                                   MaleLifeExpectancy2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want, 不选择将会存储全部列
  dplyr::select(NewCode,
                WardName,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#top of data
slice_head(Life_expectancy, n=5)
#bottom of data
slice_tail(Life_expectancy,n=5)

#use the case_when() we can assign the Borough a string of “above UK average”, 
#and if below a string of “below UK average”

Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

#know the range of life expectancies for London Boroughs that are above the national average
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group


Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())


Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))



#Plotting
plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

#use plotly an open source interactive graphing library…
install.packages("plotly")

library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~x_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")
