# script to explore listening data
# written by M. Clapp on 7/23/2018
# last edited 7/24/2018


# Load libraries ----------------------------------------------------------

rm(list = ls()) # clear the environment
library(tidyverse)
library(lubridate)
library(chron)
library(vegan)
library(plotrix)
library(lme4)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Supplemental Info -------------------------------------------------------

lakeinfo <- read_csv("data/lakeinfo.csv", col_names = TRUE)
lakeinfo <- lakeinfo %>% separate(lake, c("basin", "fish"), sep=-1, remove=FALSE) 
lakeinfo$fish <- factor(ifelse(gsub('[^12]', '', lakeinfo$lake) == "2", "fish", "fishless"))

spec_names <- read_csv("data/species_names.csv") 
rec_summary <- read_csv("data/100listens_recsummary.csv")

# Creating data frame (DONE) ----------------------------------------------


# Step 1: read in selection tables from 100 Raven listens. Raven tables are .txt, so use read.delim()
file_names <- list.files("data/renamed_selection_tables", full.names = T, pattern = "*.txt") # needs to be full.names = T in order for the read.delim below to work
length(file_names) # this shows us that a selection table is missing. TODO: find, or make new one, sadface

# they're in, but i need a way to add the identifying info to each row. 
# another option is to write a for loop that reads in each file, adds a column called "name_ID" that adds the filename to it, then rbinds it to the previous table read in
# we need the matching spreadsheet again
clipNames <- read_csv("data/filenames_batch_20180306_namesonly.csv")
clipNames$name[!clipNames$name %in% unique(file_names)] # shows us the identity of the file that is missing in file_names

audio <- do.call("rbind", lapply(file_names, read.delim))

#miss <- clipNames$hidden_name[!clipNames$hidden_name %in% unique(d$Begin.File)] # returns eclipsed filename of any missing files in the dataframe

audio <- merge(audio, clipNames, by.x = "Begin.File", by.y = "hidden_name") # simple merge of files. adding all=T as an argument would include non-matches

# Step 2: select columns relevant to initial exploration of species data
audio <- audio %>% select(Begin.File, Selection, Begin.Time..s., Delta.Time..s., Max.Power..dB., ID, NOTES, name)

# add columns for lake, treatment, and basin
audio <- audio %>% separate(name, c("lake", "date", "time", "null"), sep="_", remove=FALSE)
audio <- audio %>% separate(lake, c("basin", "fish"), sep=-1, remove=FALSE)

audio$fish <- factor(ifelse(gsub('[^12]', '', audio$lake) == "2", "fish", "fishless"))
# remove meaningless column that resulted from splitting the filename info
audio$null <- NULL

# format dates and times
audio <- audio %>% unite(timestamp, date, time, sep = "_", remove = FALSE)

audio$timestamp <- parse_date_time(audio$timestamp, "ymd HMS")

class(audio$timestamp)

audio <- audio %>% 
  separate(timestamp, c("date", "time"), sep="\\ ", remove = FALSE) 

audio$time <- chron(times=audio$time)
colnames(audio)
head(audio)

unique(audio$ID) # returns all the different entries for ID
unique(audio$NOTES) # lol

write_csv(audio, path = "data/audio_listen_data_clean_20180814.csv",col_names = TRUE)


# Tidying ----------------------------------------------------------------- 
# read in audio and temperature data 
audio <- read_csv("data/audio_listen_data_clean_20180814.csv")
temps <- read_csv("data/temps/airtemp/2015/2015temps.csv")

# create an "identifier" column with which to bind together the two dataframes--
# this adds the air temperature data from the SongMeter to the audio data frame
audio$timestamp <- round_date(audio$timestamp, "10 minutes") 
audio$identifier <- paste(audio$lake, audio$timestamp, sep = "_")

temps$timestamp <- parse_date_time(temps$timestamp, orders = "mdy HM")
temps$timestamp <- ymd_hms(temps$timestamp)
temps$timestamp <- round_date(temps$timestamp, "10 minutes")
temps$identifier <- paste(temps$site, temps$timestamp, sep = "_")

# because the temp data are taken every 5 minutes, I am taking the mean temp of each 10-min interval
temps2 <- temps %>% select(identifier, timestamp, temp) %>%
  group_by(identifier) %>%
  summarise(mean_temp = mean(temp))

# new dataframe with temperature information
audio2 <- merge(audio, temps2, by = "identifier", all.y = FALSE)
n_distinct(audio2$identifier) # 99 files-- that is correct

# remove these dataframes now
rm(temps, temps2, audio)

# birds filters data to birds with  100% positive ID only and calls > 40 dB, and no fledglings
birds <- filter(audio2, !grepl("[*]",NOTES), !grepl("fledg", NOTES),
             # Max.Power..dB. > 40.0,
             ID == "AMPI" | ID =="GCRF" | ID == "WCSP" | ID == "DEJU" | ID == "ROWR" | ID == "MOCH" | ID == "CAFI" | ID == "CLNU" | ID == "SPSA" | ID == "AMRO" | ID == "MOBL" | ID =="FOSP" | ID =="HETH" | ID =="YRWA" | ID =="NOFL" | ID =="DUFL" | ID =="OCWA" | ID =="BRBL" | ID =="WAVI")
                     
# 2018-08-13: I'm going to keep in quiet calls for now, because I lose 200+ observations without them and I am 100% sure of their identities despite them being quiet
# 2018-08-15: Note that (see below) this filter removes an entire 10-min file, likely because it did not contain any identifiable bird calls. 
n_distinct(birds$identifier)

# add elevation and area data
birds <- merge(birds, lakeinfo[,c(1,4,5)], by = "lake", all.y = FALSE)
write_csv(birds, "data/listendata_allbirds.csv")

# Mayfly data (optional) --------------------------------------------------

mayfly <- read_csv("data/insects/mayfly_counts.csv")

mayfly_smry <- mayfly %>% group_by(round, lake, fish) %>%
  summarise(dailysum = sum(mayfly)/daysin, mean = mean(mayfly))

ggplot(data = mayfly_smry) +
  geom_point(aes(x = round, y = sum, color = fish))

stickyeffort <- mayfly %>% select(lake, round, date_in, date_out, daysin)
glimpse(stickyeffort)


# Metadata on sampling effort (DONE) -----------------------------------------
# sampling effort by lake  
rec_summary <- birds %>% group_by(lake) %>%
  summarise(days_sampled = n_distinct(date),
            n_samples = n_distinct(timestamp),
            minutes_sampled = n_distinct(timestamp)*10, 
            sec_sampled = minutes_sampled*60)
write_csv(rec_summary, path = "data/100listens_recsummary.csv")

# Explore Background Noise ------------------------------------------------

background_info <- audio %>% filter(ID == "background") %>%
  group_by(lake, date) %>%
  summarize(num_bk_measures = n(),
            mean_power = mean(Max.Power..dB.),
            median_power = median(Max.Power..dB.),
            min_power = min(Max.Power..dB.),
            max_power = max(Max.Power..dB.),
            sd_power = sd(Max.Power..dB.)) 

ggplot(data = background_info) +
  geom_boxplot(aes(x = lake, y = median_power))

# basically... this sucks; it implies that the noise floors of the mics are very different, which means that detectability may be significantly different between them
# TO DO: contact Kurt to characterize background noise more robustly by site



# Call activity -----------------------------------------------------------
# for all recordings at once (no stats)
# first need to add recording info in order to scale by # seconds sampled
birds <- merge(birds, rec_summary, by = "lake")

# call_activity is the TOTAL number of seconds of ALL recordings the bird was vocalizing per LAKE, across all recordings 
# CAUTION: this is not rarefied to sample size!
birdcalls <- birds %>% group_by(basin, fish, lake, sec_sampled, ID) %>%
  summarise(call_activity = sum(Delta.Time..s.), 
            n_calls = n()) 

# activetime is the % of time in the TOTAL seconds sampled PER LAKE that the given species was audible  
# i.e., not per 10-min call, but per lake
birdcalls$activetime <- (birdcalls$call_activity/birdcalls$sec_sampled)*100
birdcalls <- birdcalls %>% select(-call_activity)

# need to spread and gather to create entries for all speciesxlocation combos
# IMPORTANT NOTE: spread and gather DO NOT WORK if there are grouping variables nested within the one you want to summarise by.
birdcalls <- spread(data = birdcalls, key = ID, value = n_calls, fill = 0)
birdcalls <- gather(data = birdcalls, key = ID, value = n_calls, ... = AMPI:YRWA)

ggplot(data = birdcalls) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6, aes(x=ID, y=n_calls, fill=fish)) +
  labs(title = "# calls during surveys")

ggplot(data=birdcalls) +
  geom_boxplot(aes(x=ID, y=n_calls, fill=fish)) # doesn't look that good... gonna try means

## breakdown by basin.. hurts eyes x_x but potentially useful code for the future
# ggplot(data = birdcalls) +
#   geom_bar(stat="identity", position = position_dodge(), width = 0.6, 
#            aes(x=basin, y=activetime, group=fish, fill=ID))


# foo summarises mean and s.e. of calltime per 10-min sample per species
# reminder that 'birds' is all bird vocalizations with 100% positive ID, all decibel levels, no fledglings
# when i compare with acoustic indices, i will include all sounds
foo <- birds %>% select(basin, fish, lake, date, timestamp, ID, Delta.Time..s.) 

# foo$calltime is the number of seconds PER 10-MIN RECORDING that each species is calling
foo <- foo %>%
  group_by(basin, fish, lake, timestamp, ID) %>%
  summarise(calltime = sum(Delta.Time..s.))

# now create columns for every combo of ID x location
foo <- spread(data = foo, key = ID, value = calltime, fill = 0)
foo <- gather(data = foo, key = ID, value = calltime, ... = AMPI:YRWA)    

# foostats calculates the mean # of seconds per 10-min recording that each species calling, regardless of basin
foostats <- foo %>% group_by(fish, ID) %>%
  summarise(mean = mean(calltime),
            sd=sd(calltime),
            sem=std.error(calltime))

foostats <- merge(foostats, spec_names, by.x = "ID", by.y = "species")

ggplot(foostats, aes(x=spec_name, y=mean, fill=fish)) +
  geom_bar(stat="identity", position = position_dodge(), width=0.6) +
  geom_errorbar(position=position_dodge(),
                aes(ymin = mean-sem, ymax = mean+sem, width = 0.6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Call activity by species, July 2015", 
              x="species", 
              y="mean # of seconds bird is calling") +
  scale_fill_discrete(name="lake type")


# Number of calls per recording -------------------------------------------

foo3 <- birds %>% select(basin, fish, lake, date, timestamp, ID) 

# foo$calltime is the number of seconds PER 10-MIN RECORDING that each species is calling
foo3 <- foo3 %>%
  group_by(basin, fish, lake, timestamp, ID) %>%
  summarise(ncalls = n())

# now create columns for every combo of ID x location
foo3 <- spread(data = foo3, key = ID, value = ncalls, fill = 0)


foo3 <- gather(data = foo3, key = ID, value = ncalls, ... = AMPI:WIWA)    


#transform log(0)s to 0

# foostats calculates the mean # of seconds per 10-min recording that each species calling, regardless of basin
foostats3 <- foo3 %>% group_by(fish, ID) %>%
  summarise(mean = mean(ncalls),
            sd=sd(ncalls),
            sem=std.error(ncalls))

foostats3 <- merge(foostats3, spec_names, by.x = "ID", by.y = "species")

f <- ggplot(foostats3) +
  geom_bar(aes(x=spec_name, y=mean, fill=fish),
           stat="identity", position = position_dodge(), width=0.7) +
  scale_fill_manual(values = c(cbPalette[2], cbPalette[6])) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title=NULL, 
       x=NULL, 
       y="mean number of calls (+/- s.e.) per 10-minute audio recording") +
  guides(fill="none")

f +  geom_errorbar(data = foostats3,
                   aes(x=spec_name, y=mean, ymin = mean-sem, ymax = mean+sem, 
                       fill=fish, width = 0.7), position=position_dodge())

# Species-specific graphs of active time ----------------------------------

GCRF <- audio %>% filter(ID == "GCRF", 
                     !grepl("[*]",NOTES), 
                     Max.Power..dB. > 40.0) %>%
  group_by(lake, basin, fish, timestamp) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

GCRF <- merge(GCRF, rec_summary, by.x = "lake", by.y = "lake")
  
GCRF <- GCRF %>% mutate(std_activity = call_activity/sec_sampled)

ggplot(GCRF) +
  geom_boxplot(aes(x=fish, y=std_activity, color=fish)) +
 # facet_wrap(~basin) +
  labs(x = "fish condition", y = "% recording calling", title = "Calling Activity of GCRF")


AMPI <- audio %>% filter(ID == "AMPI", 
                     !grepl("[*]",NOTES), 
                     Max.Power..dB. > 40.0) %>%
  group_by(lake, basin, fish, timestamp) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

AMPI <- merge(AMPI, rec_summary, by.x = "lake", by.y = "lake")

AMPI <- AMPI %>% mutate(std_activity = call_activity/sec_sampled)

ggplot(AMPI) +
  geom_boxplot(aes(x=fish, y=std_activity, color=fish)) +
#  facet_wrap(~basin) +
  labs(x = "fish condition", y = "% recording calling", title = "Calling Activity of AMPI")


WCSP <- audio %>% filter(ID == "WCSP", 
                     !grepl("[*]",NOTES), 
                     Max.Power..dB. > 40.0) %>%
  group_by(lake, basin, fish, timestamp) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

WCSP <- merge(WCSP, rec_summary, by.x = "lake", by.y = "lake")

WCSP <- WCSP %>% mutate(std_activity = call_activity/sec_sampled)

ggplot(WCSP) +
  geom_boxplot(aes(x=fish, y=std_activity, color=fish)) +
#  facet_wrap(~basin) +
  labs(x = "fish condition", y = "% recording calling", title = "Calling Activity of WCSP")

ROWR <- audio %>% filter(ID == "ROWR", 
                     !grepl("[*]",NOTES), 
                     Max.Power..dB. > 40.0) %>%
  group_by(lake, basin, fish, timestamp) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

ROWR <- merge(ROWR, rec_summary, by.x = "lake", by.y = "lake")

ROWR <- ROWR %>% mutate(std_activity = call_activity/sec_sampled)

ggplot(ROWR) +
  geom_boxplot(aes(x=fish, y=std_activity, color=fish)) +
#  facet_wrap(~basin) +
  labs(x = "fish condition", y = "% recording calling", title = "Calling Activity of ROWR")

PIKA <- audio %>% filter(ID == "PIKA", 
                     !grepl("[*]",NOTES), 
                     Max.Power..dB. > 40.0) %>%
  group_by(lake, basin, fish) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

PIKA <- merge(PIKA, rec_summary, by.x = "lake", by.y = "lake")

PIKA <- PIKA %>% mutate(std_activity = call_activity/sec_sampled)

ggplot(PIKA) +
  geom_point(aes(x=fish, y=std_activity, color=fish)) +
  facet_wrap(~basin) +
  labs(x = "fish condition", y = "% recording calling", title = "Calling Activity of PIKA")

DEJU <- audio %>% filter(ID == "DEJU", 
                     !grepl("[*]",NOTES), 
                     Max.Power..dB. > 40.0) %>%
  group_by(lake, basin, fish) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

DEJU <- merge(DEJU, rec_summary, by.x = "lake", by.y = "lake")

DEJU <- DEJU %>% mutate(std_activity = call_activity/sec_sampled)

ggplot(DEJU) +
  geom_point(aes(x=fish, y=std_activity, color=fish)) +
  facet_wrap(~basin) +
  labs(x = "fish condition", y = "% recording calling", title = "Calling Activity of DEJU")



# iNEXT -------------------------------------------------------------------
library(iNEXT)

# prepping data for rarefaction

divmatrix <- birds %>% group_by(lake, ID, name) %>%
  summarise(time_calling = sum(Delta.Time..s.)) # seconds the species is audible within a 10-minute window 

# make by lake
divmatrix_lake <- divmatrix %>% group_by(lake, ID) %>%
  summarise(n_obs = n())

divmatrix_lake <- spread(data = divmatrix_lake, 
                         key = ID, 
                         value = n_obs,
                         fill = 0)

divmatrix_lake$n_surveys <- rec_summary$n_samples
divmatrix_lake <- as.data.frame(divmatrix_lake)
rownames(divmatrix_lake) <- divmatrix_lake[,1]
divmatrix_lake <- divmatrix_lake %>% select(-lake)
# now reorder for iNEXT analysis

divmatrix_lake <- divmatrix_lake[c(18,2:17)]

# below not working
iNEXT(divmatrix_lake, q=0, datatype="incidence_freq", size=m)

str(ant)

# MODEL STUFF -------------------------------------------------------------

# prepping for GLM modeling species richness

dat <- birds %>% select(identifier, basin, fish, lake, timestamp, mean_temp, elev_m, ID) 

dat <- dat %>% group_by(basin, fish, lake, timestamp, elev_m, mean_temp) %>% 
  summarise(n_spec = n_distinct(ID))

glimpse(dat)

# date effects-- look minimal. to be expected because of the short time span.
ggplot(dat, aes(x=timestamp, y=n_spec)) +
  geom_point() +
  geom_smooth(method = "lm")

# elevation effects-- look important
ggplot(dat, aes(x=elev_m, y=n_spec)) +
  geom_point() +
  geom_smooth(method = "lm")

# looking at activity over time by species

dat2 <- birds %>% group_by(basin, fish, lake, timestamp, ID) %>%
  summarise(calltime = sum(Delta.Time..s.))

ggplot(dat2) +
  geom_point(aes(x=timestamp, y=calltime, color = lake)) +
  facet_wrap(~ID) +
  geom_smooth(aes(x=timestamp, y=calltime, color = lake), method = "lm")
# looks like calling activity didn't change much by time-- that's good

#then use glmer to model sprich 

library(lme4)
# elev needs to be scaled because the numbers are so big:
dat$elev_m.s <- scale(dat$elev_m)
m1 <- glmer(n_spec ~ fish + elev_m.s + (1|basin/lake),  family = poisson, data = dat, glmerControl(calc.derivs = F))
plot(fitted(m1), resid(m1)) 
qqnorm(resid(m1))
qqline(resid(m1))
summary(m1)

dat.sum <- birds %>% group_by(basin, fish, lake, elev_m) %>%
  summarise(tot_spec = n_distinct(ID))

dat.sum <- merge(dat.sum, rec_summary[,c(1,3)], by = "lake")

ggplot(data=dat.sum, aes(x=elev_m, y=tot_spec, color=fish)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data=dat.sum, aes(x=n_samples, y=tot_spec, fill=fish)) +
  geom_point() +
  geom_smooth(method = "lm")

m3 <- glmer(tot_spec ~ fish * n_samples + (1|basin/lake),  family = poisson, data = dat.sum, glmerControl(calc.derivs = F))
plot(fitted(m3), resid(m3)) 
qqnorm(resid(m3))
qqline(resid(m3))
summary(m3)

write_csv(dat.sum, "data/total_sprich_listendata.csv")

m2 <- glmer(tot.rich ~ fish + elev_m + (1|basin),  family = poisson, data = dat.sum)
plot(fitted(m2), resid(m2)) 
qqnorm(resid(m2))
qqline(resid(m2))
summary(m2)

# prepping data for rarefaction

divmatrix <- birds %>% group_by(lake, ID, name) %>%
  summarise(time_calling = sum(Delta.Time..s.)) # seconds the species is audible within a 10-minute window 

# make by lake: n_obs is the number of surveys the species was detected
divmatrix_lake <- divmatrix %>% group_by(lake, ID) %>%
  summarise(n_obs = n())

divmatrix_lake <- spread(data = divmatrix_lake, 
                    key = ID, 
                    value = n_obs,
                    fill = 0)

divmatrix_lake$n_surveys <- rec_summary$n_samples
divmatrix_lake <- as.data.frame(divmatrix_lake)
rownames(divmatrix_lake) <- divmatrix_lake[,1]
divmatrix_lake <- divmatrix_lake %>% select(-lake)
divmatrix_lake <- divmatrix_lake %>% select(-n_surveys)

divmatrix <- as.data.frame(divmatrix) # assigns the name column as rownames
rownames(divmatrix) <- divmatrix[,1]
divmatrix <- divmatrix %>% select(-name) # removes the name column



# make a presence/absence matrix 
pa_matrix <- divmatrix
pa_matrix[pa_matrix > 0] <- 1