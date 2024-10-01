# https://statisticsglobe.com/draw-stacked-bars-within-grouped-barplot-r

# load the package
library(tidyverse)
library("ggplot2")
#Read Real Data
data2<-read.table(
  "K2113517_MARC_out1.csv",
  sep = ",",
  header = TRUE  ,
  comment.char = "" ,     # Don't allow use of "#" as comment in input
  quote = "\""  ,
  fileEncoding = "UTF-8-BOM"
)
head(data2)
data2<- data2 %>% pivot_longer(cols = -c(Person,DeviceType), names_to = "Chemicals", values_to = "value")
head(data2)
ggplot(data2,                         # Draw barplot with grouping & stacking
       aes(x = DeviceType,
           y = value,
           fill = Chemicals)) +
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Person)

