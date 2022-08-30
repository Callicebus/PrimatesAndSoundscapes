library(tidyverse)
library(dplyr)

setwd("C:/Users/Silvy/Documents/R/Repos/PrimatesAndSoundscapes")
getwd()

file_paths <- read_csv("calculating_SNR_2022-08-22.csv", col_names = TRUE)

filepaths <- file_paths[,2]
rm(list=c("file_paths"))

for (i in filepaths$survey_file_name) {
file.copy(from = i,
          to = "C:/Users/Silvy/Documents/R/Repos/PrimatesAndSoundscapes/SNR/transect_data")
}
