library(fmsb)
library(dplyr)
library(ggplot2)


PA_ch <- read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_ch_df.csv")
PA_cl <- read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_cl_df.csv")
PA_nh <- read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_nh_df.csv")
PA_nl <- read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_nl_df.csv")

PA_ch <- PA_ch %>% summarise_all(sum)
PA_cl <- PA_cl %>% summarise_all(sum)
PA_nh <- PA_nh %>% summarise_all(sum)
PA_nl <- PA_nl %>% summarise_all(sum)

all <- rbind(PA_ch, PA_cl, PA_nh, PA_nl)[, -1]
all <- (all/all$review_count)[, -6]

df <- rbind(
  max = c(1, 1, 1, 1, 1, 1),
  min = c(0, 0, 0, 0, 0, 0),
  all
)

radarchart(
  df,
  axistype = 1,
  pcol=rainbow(4),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = c("0%", "25%", "50%", "75", "100%"),
  cglwd = 0.8,
  vlcex = 0.8,
  title = "Comparison of the four types of Italian restaurant"
)

legend(
  "topright",
  legend = c("Center and High Price", "Center and Low Price", "Off-Center and High Price", "Off-Center and Low Price"),
  fill = rainbow(4),
  bty = "n",
  cex = 0.8
)
