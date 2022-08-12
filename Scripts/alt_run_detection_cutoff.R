library(tidyverse)
library(data.table)
library(dplyr)

# Read in file
data <- read_csv("results_second_analysis_2022-08-11.csv", col_names = TRUE)

# Update template names
data$updated_template_names <- paste(data$template, "_", data$starttime)

data$template[data$updated_template_names == "F2b _ 000"] <- "F2b1"
data$template[data$updated_template_names == "F2b _ 087"] <- "F2b2"
data$template[data$updated_template_names == "F4b _ 000"] <- "F4b1"
data$template[data$updated_template_names == "F4b _ 030"] <- "F4b2"
data$template[data$updated_template_names == "F8b _ 025"] <- "F8b1"
data$template[data$updated_template_names == "F8b _ 036"] <- "F8b2"

data <- data %>% select(-c(updated_template_names))

# Remove templates that scored low in test of 2022-07-16 (see log).
data <- subset(data, data$template != "F4b1" & data$template != "F8b1"
               & data$template != "F8b2")

# Create detection averages
output <- data %>%
  group_by(audiofile, species) %>%
  summarize(max.avg = mean(max.score),
            min.avg = mean(min.score))

output <- output %>% pivot_wider(., names_from=c("species"),
                                 values_from = c("min.avg", "max.avg"))

threshhold <- 4

# Here we create 3 threshold scenarios:
# a. Detection if max score titi >= 4
# b. Detection if max score titi >=  4 & max score howler < 4
# c. Detection if max score titi >=  4 & max score titi > max score howler
output <- output %>% mutate(titi_over_thresh =
                              if_else(max.avg_T >= threshhold, TRUE, FALSE),
                            titi_over_thresh_and_howler_under_thresh =
                              if_else(max.avg_T >= threshhold & max.avg_H < threshhold, TRUE, FALSE),
                            titi_over_thresh_and_over_howler =
                              if_else(max.avg_T >= threshhold & max.avg_T > max.avg_H, TRUE, FALSE)
                          )

# Join'output' with data that shows manual detections + distance scores.
manual_data <- read_csv("Primates_In_All_Recordings_duplicates_removed.csv", col_names = TRUE)

manual_data <- manual_data %>%
  select(audiofile, PrimateSpecies, CallDistance)

joined_data = merge(x=output,y=manual_data,by="audiofile")


# How may detections did each threshold scenario find?
sum(joined_data$titi_over_thresh) # Gives us 50 possible detections.
sum(joined_data$titi_over_thresh_and_howler_under_thresh) # Gives us 18 possible detections.
sum(joined_data$titi_over_thresh_and_over_howler) # Gives us 25 possible detections.

# For easy comparison, we'll convert the three columns we just discussed from TRUE/FALSE into 1/0 values.
joined_data$titi_over_thresh <- as.integer(joined_data$titi_over_thresh)
joined_data$titi_over_thresh_and_howler_under_thresh <- as.integer(joined_data$titi_over_thresh_and_howler_under_thresh)
joined_data$titi_over_thresh_and_over_howler <- as.integer(joined_data$titi_over_thresh_and_over_howler)

# Next, we also rename the PrimateSpecies column values so that 'None' = 0, 'Titi' = 1, 'Howler' = 2 and 'Titi/Howler' = 3.
joined_data <- joined_data %>%
  mutate(PrimateSpecies = recode(PrimateSpecies, 'None' = '0', 'Titi' = '1', 'Howler' =  '2', 'Titi/Howler' = '3' ))

write_csv(joined_data, "detectionscores-2022-08-11.csv")
