# Install and load necessary libraries
# install.packages("fmsb")
library(fmsb)
library(dplyr)
library(ggplot2)

# Read data
PA_ch <- "https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_ch_df.csv"

PA_ch <- read.csv(PA_ch)
PA_ch_presentage <- PA_ch %>%
  filter(stars != 3.0) %>%
  mutate(star_category = case_when(
    stars %in% c(1.0, 2.0) ~ "low",
    stars %in% c(4.0, 5.0) ~ "high"
  )) %>%
  group_by(star_category) %>%
  summarise_all(sum) %>%
  mutate_each(funs(./review_count), -star_category) %>%
  select(-stars, -review_count)

df <- rbind(
  max = c(1, 1, 1, 1, 1, 1),
  min = c(0, 0, 0, 0, 0, 0),
  PA_ch_presentage[,-1]
)

# Create the radar map without the legend argument
radarchart(
  df,
  axistype = 1,
  pcol = c("blue", "red"),
  pfcol = c(rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.3)),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  cglwd = 0.8,
  vlcex = 0.8,
  title = "Radar map of high and low stars"
)

# Add the legend function with the legend argument
legend(
  "topright",
  legend = c("high", "low"),
  fill = c("blue", "red"),
  bty = "n",
  cex = 0.8
)