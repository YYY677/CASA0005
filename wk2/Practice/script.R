
library(sf)
library(tidyverse)

shape <- st_read("Washington_Counties_with_Natural_Shoreline___washsh_area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
plot(shape)

shape %>% 
  st_geometry() %>% 
  st_simplify(., dTolerance = 1000) %>% 
  plot()

# mycsv <- read_csv("Report_Card_Assessment_Data_2018-19_School_Year_20241003.csv", na="NULL")
mycsv <- read_csv("Report_Card_Assessment_Data_2018-19_School_Year_20241003.csv", na="NULL")

Datatypelist <- mycsv %>% 
  summarise_all(class) %>% 
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")
Datatypelist

sum_mycsv <- mycsv %>%
  # 如果有需要先将列转换为数值型
  mutate(
    `Count of students expected to test including previously passed` = as.numeric(`Count of students expected to test including previously passed`),
    CountMetStandard = as.numeric(CountMetStandard)
  ) %>%
  group_by(County) %>%
  summarise(
    total_expected_count_to_test = sum(`Count of students expected to test including previously passed`, na.rm = TRUE),
    total_passed_count = sum(CountMetStandard, na.rm = TRUE)
  )

shape_merge <- shape %>% 
  merge(.,
        sum_mycsv,
        by.x="COUNTYLABE",
        by.y="County")

#avg_meeting_rate = mean(meeting_rate)
shape_merge <- shape_merge %>% 
  mutate(meeting_rate = total_passed_count/total_expected_count_to_test,
         meeting_rate_compare = case_when(meeting_rate > mean(meeting_rate) ~ "above",
         meeting_rate == mean(meeting_rate) ~ "equal",
         TRUE ~ "below") 
         )

mean(shape_merge$meeting_rate)

#渲染
library(tmap)
# 绘制地图并根据 meeting_rate 进行颜色填充
tm_shape(shape_merge) +
  tm_polygons(
    "meeting_rate",
    title = "Meeting Rate",
    palette = "YlOrRd", # 黄色到红色渐变调色板
    breaks = seq(0.29, 0.66, length.out = 6) # 定义渐变范围为0.29到0.66，分为5个层级
    ) +
  tm_layout(title = "Meeting Rate by Region")


















