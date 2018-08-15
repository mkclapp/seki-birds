library(tidyverse)
library(lubridate)
library(chron)
library(vegan)

# TODO: identifier column does not match perfectly; they're sometimes ~1 min off

# read in acoustic index data ---------------------------------------------
indices <- read_csv("data/listens_20180309_AI.csv")
indices$Timestamp <- parse_date_time(indices$Timestamp, "ymd HMS")
class(indices$Timestamp)
indices$Timestamp <- round_date(indices$Timestamp, "10 minutes")

# make a column with which to merge this dataset with the other
indices$identifier <- paste(indices$Site, indices$Timestamp, sep = "_")

# read in audio-listen data -----------------------------------------------
audio <- read_csv("data/audio_listen_data_clean_20180814.csv")

# make a mergeable column here too
audio$timestamp <- round_date(audio$timestamp, "10 minutes")
audio$identifier <- paste(audio$lake, audio$timestamp, sep = "_")

# Explore Background Noise ------------------------------------------------

background_info <- audio %>% filter(ID == "background") %>%
  group_by(lake, date) %>%
  summarize(num_bk_measures = n(),
            mean_power = mean(Max.Power..dB.),
            median_power = median(Max.Power..dB.),
            min_power = min(Max.Power..dB.),
            max_power = max(Max.Power..dB.),
            sd_power = sd(Max.Power..dB.)) 

# Summarise audio listen data ---------------------------------------------

# sampling effort by lake
rec_summary <- audio %>% group_by(lake) %>%
  summarise(days_sampled = n_distinct(date),
            n_samples = n_distinct(timestamp),
            minutes_sampled = n_distinct(timestamp)*10, 
            sec_sampled = minutes_sampled*60)

# create another dataframe that fixes typos, removes background entries
audio$ID <- toupper(audio$ID)
audio$ID <- str_replace(string = audio$ID, pattern = "RPWR", replacement = "ROWR")
audio$ID <- str_replace(string = audio$ID, pattern = "INSECTS", replacement = "INSECT")

biophony <- filter(audio, ID != "ECHO", !grepl("BAND", ID), !grepl("BACK", ID), !is.na(ID))
unique(biophony$ID)

biophony <- biophony %>% select(-Begin.File, -Begin.Time..s.)

soundcount <- biophony %>% group_by(identifier, basin, fish, lake, ID) %>%
  summarise(n_sounds = n())
head(soundcount)

sumsum <- soundcount %>% spread(key = ID, value = n_sounds, fill = 0) %>%
  gather(key = ID, value = soundcount, ... = AMPI:YRWA) %>%
  group_by(identifier, basin, fish, lake) %>%
  summarise(shannon = diversity(soundcount, index="shannon"))

frank <- merge(sumsum, indices, by = "identifier") #OMG it worked 

ggplot(frank) + 
  geom_point(aes(x = Rough, y = shannon, color = lake))

