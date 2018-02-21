# Sun Feb 18 11:19:34 2018 ------------------------------
# Script for hiding acoustic data filenames

# What I want to do: Rename all the wav files in a directory with a UNIQUE, randomized vector of letters
# Also, write a table that keeps track of the original filenames so that I can eventually rename them/reassign them to their locations
# Steps:
# 1) Create a vector of all the letters in the alphabet ( apparently this exists in R as letters() or LETTERS() )
# 2) Create a list of all the files within the directory that I want to rename
# 3) Generate a unique, random string of letters for each file and add those as a column to the above data frame
# 4) SAVE THE ABOVE AS A CSV so I can eventually reassign the files their original filenames
# 5) Actually rename each file in the directory

rm(list = ls()) #removes all objects from workspace

library(tidyverse)

# 1) Create a vector of the filenames I need to rename
listens <- list.files(path = "/Users/mary/Desktop/Research/data/listens", pattern = ".wav")
# 2) Create a vector of randomly sampled numbers with 6 digits
newname <- sample(100000:999999, length(listens), replace = FALSE)
# 3) Slap 'em together so that each file is assigned a unique random number ID and write it out as a .csv 
file_id <- as.data.frame(cbind(listens, newname))
write_csv(file_id, path = "data/file_id.csv")

# TODO: 
# 4) Actually rename the files (SCARY)... is there any way to do this and still make sure they got named correctly?
# And I'd like to be able to keep the date-time information so that Raven can read the timestamp
# Aaaaand I'd like to keep the .wav file extension
# I guess I should have duplicated a small number of wav files and then compared the first bits of audio to confirm that the code renamed correctly
# OH and can I do this without having to setwd()

# Maybe I can do this by adding an extension after the date-time, then erasing the prefix of the filename. To be continued...
setwd("/Users/mary/Desktop/Research/data/listens")
file.rename(as.vector(file_id$listens), as.vector(file_id$newname))          
