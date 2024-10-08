library(sf)
shape2 <- st_read("Practice/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")
plot(shape2)

library(tidyverse) #包含readr，dplyr等
mycsv2 <- read_csv("Practice/statsnz-territorial-authority-2018-generalised-SHP/Paid_employee.csv",
                   locale = locale(encoding = "GBK"))
names(mycsv2)

shape2 <- shape2%>% 
  merge(.,
        mycsv2,
        by.x="TA2018_V1_",
        by.y="行标签")

shape2 %>% 
  head(., 10)

library(tmap)
tmap_mode("plot")
qtm(shape2, fill="Paid_employee")

shape2 %>% 
  st_write(.,"Practice/result.gpkg",
           "territorial-authority-2018-generalised")

library(RSQLite)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="result.gpkg")


# list what is in it
con %>%
  dbListTables()

# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv2,
               overwrite=TRUE)


# disconnect from it
con %>% 
  dbDisconnect()

