# Determining Signal-to-noise ratio of titi monkey duets.


# Step 1: Install and load packages:
install = FALSE

if(install) {
  install.packages("warbleR")
  install.packages("tuneR")
  install.packages("tidyverse")
}

library(warbleR)
library(tuneR)
library(tidyverse)


# Step 2: Creating a data frame with files containing duets and all start and end times of the calls.
data <- read_csv("calculating_SNR_2022-08-18.csv", col_names = TRUE)
data <- data %>% select(-c(min.avg_H, min.avg_S, min.avg_T, max.avg_S, titi_over_thresh, titi_over_thresh_and_howler_under_thresh))
