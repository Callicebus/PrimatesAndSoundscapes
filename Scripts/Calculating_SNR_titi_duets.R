library(warbleR)
library(tidyverse)

# Create a list of all the recordings in the directory
wavs <- list.files(pattern="wav$")

# We will use this list to downsample the wav files so the following analyses go a bit faster
lapply(sub, function(x) writeWave(downsample(readWave(x), samp.rate = 11000),
                                   filename = x)) # THIS OVERWRITES THE ORIGINAL FILE!!

# If needed, test this code first on a subset of your data:
# Select a subset of the recordings
sub <- wavs[c(0,2)]

# Run auto_detec() on subset of recordings to automatocally locate calls of interest.
results <- auto_detec(flist = sub, bp = c(0.2, 1.5), mindur = 10, maxdur = 35,
                      wl = 512, img = TRUE, output  = "list")

full_spectrograms(results) #Visualize your results


# Determining location/lengths of the noise part of the measurement:
# A margin that's too large causes other signals to be included in the noise measurement, so test/play around with values
# (I have used 5 seconds of noise + first 5 seconds of duet for measurements when using Raven)
snr_spectrograms(X = results$selection.table, flim = c(2, 11), snrmar = 0.2, mar = 0.7, it = "tiff")

# Now calculate SNR with the following:
Phae.snr <- sig2noise(X = results$selection.table[seq(1, nrow(results$selection.table), 2), ], mar = 0.04)




































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


# Step 2: Creating a data frame with files containing duets and all start and end times (in seconds) of the calls.
#data <- data %>% select(-c(min.avg_H, min.avg_S,
#                           min.avg_T, max.avg_S,
#                           titi_over_thresh,
#                           titi_over_thresh_and_howler_under_thresh)) # Removing unnecessary columns.

data_sheet <- read_csv("Project3_DataSheet_2022-08-20.csv", col_names = TRUE)
template_data <- read_csv("calculating_SNR_2022-08-22.csv", col_names = TRUE)

merged_data = merge(x=data_sheet,y=template_data,by="audiofile")

# Reorder columns and remove some obsolete columns.
merged_data_reordered <- merged_data[, c(15, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 19, 20, 21, 22, 23, 24)]


# --------


# Trying out warbleR

