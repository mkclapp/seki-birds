

# load libraries ----------------------------------------------------------

rm(list = ls()) # clear the environment
library(tidyverse)
library(lubridate)
library(chron)

# load selection tables ---------------------------------------------------

# make an object that indexes all the selection tables
file_names <- list.files("data/june_tables/", full.names = T) # needs to be full.names = T in order for the read.delim below to work
length(file_names) # 92 because some of the files were too full of weather noise
# use the file_names index to read in and glue together all selection tables into one large dataframe
listens <- do.call("rbind", lapply(file_names, read.delim))

# read column names of the selection tables
colnames(listens)

# remove "_000.wav" from Begin.File 
listens$Begin.File <- substr(listens$Begin.File, 1, 23)

# select only the relevant columns
listens <- listens %>% select(Begin.File, Selection, Begin.Time..s., Max.Power..dB., ID, NOTES)

# add columns for lake, treatment, and basin
listens <- listens %>% separate(Begin.File, c("lake", "date", "time"), sep="_", remove=FALSE)
listens <- listens %>% separate(lake, c("basin", "fish"), sep=-1, remove=FALSE)

listens$fish <- factor(ifelse(gsub('[^12]', '', listens$lake) == "2", "fish", "fishless"))


# format dates and times
listens <- listens %>% unite(timestamp, date, time, sep = "_", remove = FALSE)

listens$timestamp <- parse_date_time(listens$timestamp, "ymd HMS")

class(listens$timestamp)

listens <- listens %>% 
  separate(timestamp, c("date", "time"), sep="\\ ", remove = FALSE) 

listens$time <- chron(times=listens$time)
colnames(listens)
glimpse(listens)

unique(listens$ID) # returns all the different entries for ID
unique(listens$NOTES) # lol


# load point count data ---------------------------------------------------

d <- read_csv("data/pointcount_20180816.csv") # working as of 12/6/2018
glimpse(d)
## exploratory plots of species richness and abundance ---------------------

## restrict distance; exclude double-counts (optional) and unidentified species
d300 <- d %>% filter(distance < 300 & species != "XXXX" & species !="XXSP" & species != "XXHU")
d150 <- d %>% filter(distance < 150 & species != "XXXX" & species !="XXSP" & species != "XXHU")
d100 <- d %>% filter(distance < 100 & doublecount == "" & species != "XXXX" & species !="XXSP" & species != "XXHU")
d50 <- d %>% filter(distance < 50 & species != "XXXX" & species !="XXSP" & species != "XXHU")

## the following summarise species richness and abundance PER POINT at different distances
pt300 <- d300 %>% group_by(basin, fish, location, date, point, time, jday) %>% 
  summarize(nspecies=n_distinct(species), total=n())
# 
# pt150 <- d150 %>% group_by(basin, fish, date, point, time, jday) %>% 
#   summarize(nspecies=n_distinct(species), total=n())
# 
# pt100 <- d100 %>% group_by(basin, fish, date, point, time, jday) %>% 
#   summarize(nspecies=n_distinct(species), total=n())
# 
# pt50 <- d50 %>% group_by(basin, fish, date, point, time, jday) %>% 
#   summarize(nspecies=n_distinct(species), total=n())


