# Random Forest with Acoustic Indices

rm(list=ls())

library(rfUtilities)
library(randomForest)
library(caret)
library(tidyverse)
library(vegan)
library(chron)
library(lubridate)


# on 100 listens ----------------------------------------------------------
# read in AI data
AI <- read_csv("data/listens_20180309_AI.csv")
glimpse(AI)
# create a column in the AI matrix that matches the wav filenames
# just some wonky data manipulation here...
AI <- AI %>% unite(filematch, Site, date, time, sep = "_", remove = FALSE)

# read in cleaned up dataframe from first 100 listens (early july)
d <- read_csv("data/audio_listen_data_clean_20181125.csv")
colnames(d)
glimpse(d)

d <- d %>% unite(filematch, lake, date, time, sep = "_", remove = FALSE)

d$filematch <- gsub("-|:", "", d$filematch)
d$filematch <- paste(d$filematch, "000.wav", sep="_")
d$filematch <- as.character(d$filematch)




# DATAFRAME SETUP ---------------------------------------------------------

# STEP 1: compile matrix with AI values and response variable(s) for all annotated data

# entire dataset to pull from
file_names <- list.files(path = "data/indices/2018_10_02", full.names = TRUE, pattern = "*.csv")

d <- do.call("rbind", (lapply(file_names,read.csv)))

# decide what columns to include
colnames(d)
d <- subset(d, select = c(Site, Date, Yr, Mo, Day, Hr, Min, Sec,
                          BKdB_low, BKdBA_low, BKdB_bird, BKdBA_bird, avgAMP, L10AMP, 
                          Hf, Ht, EI, Rough, ADI_step, Eveness_step, AR, ACIout, ACIoutI, version))

# clean up dates and times
d <- d %>%
  unite(Time, Hr, Min, Sec, sep = ":", remove = FALSE) %>%
  unite(Timestamp, Date, Time, sep = " ", remove = FALSE)

d$Timestamp <- parse_date_time(d$Timestamp, "ymd HMS")

d <- d %>% 
  separate(Timestamp, c("day", "time"), sep="\\ ", remove = FALSE) 
d <- d %>% 
  select(-Date) %>% select(-Time)

d$day<- ymd(d$day)
d$time <- chron(times=d$time)

# create separate columns for basin and fish treatment
d <- d %>% separate(Site, c("basin", "fish"), sep=-1, remove=FALSE)
d$fish <- factor(ifelse(gsub('[^12]', '', d$fish) == "2", "fish", "fishless"))

# STEP 2: create an index for all annotated-listen files and subset d using it

# read in filenames of wavs that have been annotated
july <- list.files(path = "../../../../Desktop/listens_20180309/renamed_listens", pattern = "*.wav")
mayjun <- list.files(path = "../../../../Desktop/listens_20180905", pattern = "*.wav")
index <- c(mayjun, july)
# create a column in the AI matrix that matches the wav filenames
# just some wonky data manipulation here...
d <- d %>% unite(filematch, Site, day, time, sep = "_", remove = FALSE)

#now remove all ":" and "-" from this column
d$filematch <- gsub("-|:", "", d$filematch)
d$filematch <- paste(d$filematch, "000.wav", sep="_")
d$filematch <- as.character(d$filematch)

#base-R way of subsetting d
d[with(d, filematch %in% index),] # didn't work

d %>% starts_with()

AI <- d %>% filter(filematch )