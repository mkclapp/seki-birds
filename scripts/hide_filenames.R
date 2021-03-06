# Tue Mar  6 20:52:36 2018 ------------------------------
# Script for hiding acoustic data filenames

rm(list = ls()) #removes all objects from workspace
library(tidyverse)

# Vince's suggestion: use tempfile() to create temporary copies of each file 

# 1) Create a vector of the filenames I need to rename
listens <- list.files(path = "/Volumes/NSNSD_SEKI_/listens_20180309", pattern = ".wav", full.names=TRUE)
#2) Write a function that copies wav files, gives them random names, and saves them to a specified temporary directory
create_copy <- function(incoming_file, tmpdir) { # what is 'tmpdir' doing inside those parentheses?
  new_file <- tempfile(tmpdir = tmpdir, fileext = '.wav') # sets up a temporary placeholder for the temporary files in the specified tempdir
  ok <- file.copy(incoming_file, new_file) # file.copy() copies the incoming file and renames it 
  if (!ok) {
    stop("boink! creating file copy failed!")
  }
  return(new_file)
}

# The above is just a function. We haven't actually done anything to the filenames yet

# Then write out a tibble that includes both the original filename and its corresponding masked name
audio_tempdir <- "/Users/mary/Desktop/listens_20180309"
# map() is a function like lapply that takes in an object (file) and then applies a function (create_copy) to it.
# mutate creates a new column called 'new_file' 
# is this not performing the function twice?
blah <- tibble(file = listens) %>% 
  mutate(new_file = map_chr(file, create_copy, tmpdir=audio_tempdir)) #where did we ever define what file is? 
# then write it to a .csv
write_csv(blah, path = "/Users/mary/Desktop/listens_20180309/filenames_batch_20180306.csv")

#try with 1 file in the directory
#create_copy(listens[1], "/Users/mary/Desktop/Research/data/listens/temp")



# RENAME FILES ------------------------------------------------------------
### adapted from NPS NSNSD code 'Eclipse_ClipReNames.R'
library(tidyverse)

inDir =    "/Users/mary/Desktop/listens_20180309/completed_listens/"
copy2dir = "/Users/mary/Desktop/listens_20180309/renamed_listens/"
setwd(inDir)
wavFiles <- dir(path = inDir, pattern=".wav$")

# READ in previously created spreadsheet clip names generated by 
# <Eclipse_FileNames.R> and then random assignment of survey and clip id
clipNames = read.csv('../filenames_batch_20180306_namesonly.csv', sep =",")

for (ff in 1:length(wavFiles)  ) { #for testing: ff = 20
  
  #find the matching column
  mtch = clipNames[clipNames$hidden_name == wavFiles[ff],]
  
#  if (is.na(mtch$ClipID) == FALSE ) {
    
    #get new name
    newName = paste(mtch$name,sep="")
    
    #copy file to new directory
    filestocopy = wavFiles[ff]
    file.copy(from=filestocopy, to=copy2dir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
    
    #rename file in new directory
    setwd(copy2dir)
    file.rename ( wavFiles[ff], newName )
    
    #reset to the in directory...
    setwd(inDir)
    
  }

# now do the same thing for the selection tables NOT WORKING
# TO DO: try actually renaming the files themselves instead of just the vectors
library(tidyverse)
rm(list = ls())
inDir =    "/Users/mary/Desktop/listens_20180309/selection_tables/"
copy2dir = "/Users/mary/Desktop/listens_20180309/renamed_selection_tables/"
setwd(inDir)
selTables <- dir(path = inDir, pattern=".txt") # missing 2 tables

# READ in previously created spreadsheet clip names generated by 
# <Eclipse_FileNames.R> and then random assignment of survey and clip id
clipNames = read.csv('../filenames_batch_20180306_namesonly.csv', sep =",")

# need to temporarily simplify selection table filenames so they match exactly with clipNames
clipNames$hidden_name <- str_sub(clipNames$hidden_name, end = -5)
clipNames$name <- str_sub(clipNames$name, end=-5)
selTables <- str_sub(selTables, end=-24)

for (ff in 1:length(selTables)  ) { #for testing: ff = 1
  
  #find the matching column
  #mtch = clipNames[clipNames$hidden_name == [selTables[ff],] # need to find within, not ==
  mtch = clipNames[clipNames$hidden_name == selTables[ff],] 
  #  if (is.na(mtch$ClipID) == FALSE ) {
  
  #get new name
  newName = paste0(mtch$name,"_selections.txt",sep="")
  
  #copy file to new directory
  filestocopy = paste0(selTables[ff],".Table.1.selections.txt") # have to add this appendage so it can find the actual file
  file.copy(from=filestocopy, to=copy2dir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  
  #rename file in new directory
  setwd(copy2dir)
  file.rename (paste(selTables[ff], ".Table.1.selections.txt", sep = ""), newName ) 
  #reset to the in directory...
  setwd(inDir)
  
}

# Sun Feb 18 11:19:34 2018 ------------------------------

# OLD TRY: DO NOT USE
# # 2) Create a vector of randomly sampled numbers with 6 digits
# newname <- sample(100000:999999, length(listens), replace = FALSE)
# # 3) Slap 'em together so that each file is assigned a unique random number ID and write it out as a .csv 
# file_id <- as.data.frame(cbind(listens, newname))
# write_csv(file_id, path = "data/file_id.csv")
# 
# # TODO: 
# # 4) Actually rename the files (SCARY)... is there any way to do this and still make sure they got named correctly?
# # And I'd like to be able to keep the date-time information so that Raven can read the timestamp
# # Aaaaand I'd like to keep the .wav file extension
# # I guess I should have duplicated a small number of wav files and then compared the first bits of audio to confirm that the code renamed correctly
# # OH and can I do this without having to setwd()
# 
# # Maybe I can do this by adding an extension after the date-time, then erasing the prefix of the filename. To be continued...
# setwd("/Users/mary/Desktop/Research/data/listens")
# file.rename(as.vector(file_id$listens), as.vector(file_id$newname))          
