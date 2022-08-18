# script to parse all .txt files in a "data" folder of output files
# and return a .csv file with the desired elements

library(tidyverse)
library(ggplot2)
library(data.table)

filenames <- list.files(
  "Output_ManualData", pattern = ".txt", full.names = TRUE
  # I made this line read from the Output_ManualData folder located within the
  # repo to avoid a hard coded path name! This is much better form :)
)

# initialize results
results <- tibble(
  original_output_file=character(),
  survey_file_name=character(),
  folder=character(),
  location=character(),
  recording.date=character(),
  audiofile=character(),
  template=character(),
  min.score=numeric(),
  max.score=numeric(),
  n.scores=numeric())

for (f in filenames){
  # initialize r
  r <- tibble(
    original_output_file=character(),
    survey_file_name=character(),
    folder=character(),
    location=character(),
    recording.date=character(),
    audiofile=character(),
    template=character(),
    min.score=numeric(),
    max.score=numeric(),
    n.scores=numeric())
  d <- read_lines(f, skip_empty_rows = TRUE)

  # initialize vectors
  survey_file_name <- character()
  folder <- character()
  location <- character()
  recording.date <- character()
  audiofile <- character()
  template <- character()
  min.score <- numeric()
  max.score <- numeric()
  n.scores <- numeric()
  fileindex <- 0 # this is new... now we create a tibble, r, for each set of results. the start of a new set of results are detected by the str_detect function in line 54 and the end is detected by the str_detect function in line 73

  for (i in 1:length(d)){ # loop through file line by line
    if (str_detect(d[i], "Based") == TRUE){
      fileindex <- fileindex + 1
      fname <- str_remove(d[i], "Based on the survey file:  ") # removes start of line
      fname <- str_remove(fname, " $") # removes terminal space
      locationSubstrings <- str_split(fname, "/")
      folder <- locationSubstrings[[1]][3]
      location <- locationSubstrings[[1]][4]
      recording.date <- locationSubstrings[[1]][5]
      audiofile <- locationSubstrings[[1]][6]
      index <- 0
    }

    l <- str_split(d[i], "[ ]+")

    if (str_detect(l[[1]][1], "F") == TRUE){
      index <- index + 1
      template[index] <- l[[1]][1]
      min.score[index] <- as.numeric(l[[1]][2])
      max.score[index] <- as.numeric(l[[1]][3])
      n.scores[index] <- as.numeric(l[[1]][4])
    }
    if (str_detect(l[[1]][2], "------------") == TRUE & fileindex > 0){
      # a tibble for each set of results
      r <- tibble(
        # based on recylcing of vectors, f, fname, folder, location, recording.date, and audiofile are repeated for each template
        original_output_file=f,
        survey_file_name=fname,
        folder=folder,
        location=location,
        recording.date=recording.date,
        audiofile=audiofile,
        # the template and stats are already vectors
        template=template,
        min.score=min.score,
        max.score=max.score,
        n.scores=n.scores
      )
      results <- bind_rows(results, r) # bundle all results together
    }
  }
}

# cleanup memory and work space
rm(list=c(
  "d",
  "l",
  "f",
  "r",
  "fileindex",
  "fname",
  "index",
  "survey_file_name",
  "folder",
  "location",
  "locationSubstrings",
  "recording.date",
  "audiofile",
  "template",
  "min.score",
  "max.score",
  "n.scores",
  "i"
))

# Separate data that are currently merged in a single column
results <- separate(data = results, col = "template",
                    into = c("template", "starttime", "ampcutoff", "species"),
                    sep = ",")

# Reorder columns and remove some obsolete columns.
results <- results[, c(2, 13, 10, 11, 12, 3, 4, 5, 6, 7, 8, 9)]

# Merge tables
Primates_In_All_Recordings <- read_csv("Primates_In_All_Recordings_duplicates_removed.csv", col_names = TRUE)

Merged_tables = merge(x=results,y=Primates_In_All_Recordings,by="audiofile")

fwrite(Merged_tables, "results_second_analysis_2022-08-18.csv") #The usual write_csv() only wrote the first 600 lines rather than all 14000+!

# Cleanup memory and work space
rm(list=c("results", "filenames"))


