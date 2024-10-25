library(tidyverse)
library(palmerpenguins)
library(ggplot2)

penguins %>%
  ggplot(aes(x = bill_depth_mm))+
  geom_histogram



