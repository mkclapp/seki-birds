# script to explore listening data
# written by M. Clapp on 7/23/2018
# last edited 7/24/2018

rm(list = ls()) # clear the environment
library(tidyverse)
library(lubridate)
library(chron)
library(vegan)
library(plotrix)

# Tidying ----------------------------------------------------------------- 

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

# birds filters data to birds with  100% positive ID only and calls > 40 dB, and no fledglings

birds <- filter(audio, !grepl("[*]",NOTES), 
             # Max.Power..dB. > 40.0,
             ID == "AMPI" | ID =="GCRF" | ID == "WCSP" | ID == "DEJU" | ID == "ROWR" | ID == "MOCH" | ID == "CAFI" | ID == "CLNU" | ID == "SPSA" | ID == "AMRO" | ID == "MOBL" | ID =="FOSP" | ID =="HETH" | ID =="YRWA" | ID =="NOFL" | ID =="DUFL" | ID =="OCWA" | ID =="BRBL" | ID =="WAVI",
                    !grepl("fledg", NOTES))
# 2018-08-13: I'm going to keep in quiet calls for now, because I lose 200+ observations without them and I am 100% sure of their identities despite them being quiet

# birds <- filter(audio, !grepl("[*]",NOTES), 
                # ID == "AMPI" | ID =="GCRF" | ID == "WCSP" | ID == "DEJU" | ID == "ROWR" | ID == "MOCH" | ID == "CAFI" | ID == "CLNU" | ID == "SPSA" | ID == "AMRO" | ID == "MOBL" | ID =="FOSP" | ID =="HETH" | ID =="XXHU" | ID =="YRWA" | ID =="NOFL" | ID =="DUFL" | ID =="OCWA" | ID =="BRBL" | ID =="WAVI") 

# metadata on this sample of recordings

# sampling effort by lake
rec_summary <- audio %>% group_by(lake) %>%
  summarise(days_sampled = n_distinct(date),
            n_samples = n_distinct(timestamp),
            minutes_sampled = n_distinct(timestamp)*10, 
            sec_sampled = minutes_sampled*60)

# Explore Background Noise ------------------------------------------------

background_info <- audio %>% filter(ID == "background") %>%
  group_by(lake, date) %>%
  summarize(num_bk_measures = n(),
            mean_power = mean(Max.Power..dB.),
            median_power = median(Max.Power..dB.),
            min_power = min(Max.Power..dB.),
            max_power = max(Max.Power..dB.),
            sd_power = sd(Max.Power..dB.)) # 

ggplot(data = background_info) +
  geom_boxplot(aes(x = lake, y = median_power))

# basically... this sucks; it implies that the noise floors of the mics are very different, which means that detectability may be significantly different between them
# TO DO: contact Kurt to characterize background noise more robustly by site

# Active time per species -------------------------------------------------
# for all recordings at once (no stats)
# first need to add recording info in order to scale by # seconds sampled
birds <- merge(birds, rec_summary, by = "lake")

# call_activity is the TOTAL number of seconds of ALL recordings the bird was vocalizing per LAKE, across all recordings 
birdcalls <- birds %>% group_by(basin, fish, lake, sec_sampled, ID) %>%
  summarise(call_activity = sum(Delta.Time..s.)) 

# activetime is the % of time in the TOTAL seconds sampled PER LAKE that the given species was audible  
# i.e., not per 10-min call, but per lake
birdcalls$activetime <- (birdcalls$call_activity/birdcalls$sec_sampled)*100
birdcalls <- birdcalls %>% select(-call_activity)

# need to spread and gather to create entries for all speciesxlocation combos
# IMPORTANT NOTE: spread and gather DO NOT WORK if there are grouping variables nested within the one you want to summarise by.
birdcalls <- spread(data = birdcalls, key = ID, value = activetime, fill = 0)
birdcalls <- gather(data = birdcalls, key = ID, value = activetime, ... = AMPI:NOFL)

ggplot(data = birdcalls) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6, aes(x=ID, y=activetime, fill=fish)) +
  labs(title = "% time audible during surveys")

ggplot(data=birdcalls) +
  geom_boxplot(aes(x=ID, y=activetime, fill=fish)) # doesn't look that good... gonna try means

# breakdown by basin.. hurts eyes x_x
ggplot(data = birdcalls) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.6, 
           aes(x=basin, y=activetime, group=fish, fill=ID))

# foo summarises mean and s.e. per 10-min sample per species
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

# trying by lake now
foostats2 <- foo %>% group_by(basin, fish, lake, ID) %>%
  summarise(mean = mean(calltime),
            sd=sd(calltime),
            sem=std.error(calltime))

foostats2 <- merge(foostats, spec_names, by.x = "ID", by.y = "species")

ggplot(foostats2, aes(x=ID, y=mean, fill=fish)) +
  geom_bar(stat="identity", position = position_dodge(), width=0.6) +
  geom_errorbar(position=position_dodge(),
                aes(ymin = mean-sem, ymax = mean+sem, width = 0.6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Call activity by species, July 2015", 
       x="species", 
       y="mean # of seconds bird is calling") +
  scale_fill_discrete(name="lake type") +
  facet_wrap(~basin, ncol=2)

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

# VEGAN STUFF -------------------------------------------------------------

# preparing data for rarefaction

divmatrix <- birds %>% group_by(name, ID) %>%
  summarise(num_calls = n())

call_div <- birds %>% group_by(lake, ID) %>%
  summarise(num_calls = n())

call_div_matrix <- spread(data=call_div,
                          key=ID,
                          value=num_calls,
                          fill=0)

cdm_vegan <- as.data.frame(call_div_matrix)
row.names(cdm_vegan) <- call_div_matrix$lake
cdm_vegan <- cdm_vegan %>% select(-lake)
  
# rar <- spread(data = divmatrix, key = ID, value = num_calls, fill = 0)
# 
# rar_vegan <- as.data.frame(rar, row.names = rar[,1])
# rar_vegan <- rar_vegan %>% select(-name)

sample <- min(rowSums(cdm_vegan))

rarefy(cdm_vegan, sample)

rarecurve(cdm_vegan, sample, xlab = "Sample Size", ylab = "Species", label = TRUE)

divmatrix <- birds %>% group_by(ID, name) %>%
  summarise(time_calling = sum(Delta.Time..s.)) # seconds the species is audible within a 10-minute window 

divmatrix <- spread(data = divmatrix, 
                    key = ID, 
                    value = time_calling,
                    fill = 0)

divmatrix <- as.data.frame(divmatrix, row.names = divmatrix[,1]) # assigns the name column as rownames
divmatrix <- divmatrix %>% select(-name) # removes the name column

# make a presence/absence matrix 
pa_matrix <- divmatrix
pa_matrix[pa_matrix > 0] <- 1

# Shannon diversity of the divmatrix (NOT WORKING)
shannon_pa <- diversity(pa_matrix)
shannon_call <- diversity(divmatrix) 

divmatrix$shannon <- shannon_call
pa_matrix$shannon <- shannon_pa

plot(shannon_pa ~ shannon_call)

# rarefaction curves
# TO DO: figure out how to create rarefaction curves as in Francis 2009

Srar <- rarefy(pa_matrix, min(rowSums(divmatrix))) # not working
specaccum(pa_matrix)




# Srar did not seem to work even with rounded numbers

#TO DO: replace NAs and #s with 0s and 1s for a presence/absence matrix

# number of species per date (# of samples per date vary!)
# first calculate summary statistics:


# Species Richness --------------------------------------------------------

listen_rich <- birds %>% group_by(basin, fish, lake, timestamp) %>% 
  summarize(nspecies=n_distinct(ID), total=n())

sprich <- birds %>% group_by(lake, basin) %>%
  summarise(sp_rich = n_distinct(ID))

sprich_ddd <- dd %>% group_by(lake, basin) %>%
  summarise(sp_rich = n_distinct(ID))

# summary stats for avg. species richness by date-- needs rarefaction to be legit I think
sprich_by_date <- birds %>% group_by(lake, basin, fish, date) %>%
  summarise(sp_rich = n_distinct(ID))

# species richness for every sample
sprich_by_sample <- birds %>% group_by(lake, basin, fish, timestamp, time) %>%
  summarise(sp_rich = n_distinct(ID))

sprich_summary <- sprich_by_date %>% # this summary works
  summarise(N = n_distinct(date),
            mean_sprich = mean(sp_rich), 
            median_sprich = median(sp_rich), 
            sd_sprich = sd(sp_rich),
            se = sd_sprich / sqrt(N))

ggplot(data = e300) +
  geom_boxplot(aes(x=fish, y=nspecies, color=fish)) +
  labs(x="treatment", y="species richness", title="Species Richness within 300m, 2014-2016") +
  facet_wrap(~basin) +
  theme_light()

ggplot(listen_rich) +
  geom_boxplot(aes(x=fish, y=nspecies, color=fish)) +
  labs(x=NULL, y="species richness", title="Species Richness in 10-minute recordings, July 2015") +
  facet_wrap(~basin) +
  theme_light()

# sprich_summary <- sprich_by_sample %>% # this summary NOT WORKING WTFFFFFFF
#   summarise(N = n_distinct(lake),
#             mean_sprich = mean(sp_rich), 
#             median_sprich = median(sp_rich), 
#             sd_sprich = sd(sp_rich),
#             se = sd_sprich / sqrt(N))

library(Rmisc)
summary <- summarySE(sprich_by_sample, measurevar = "sp_rich", groupvars = c("basin", "lake", "fish"))
sum2 <- summarySE(sprich_by_date, measurevar = "sp_rich", groupvars = c("basin", "lake", "fish"))
# plot mean species richness 
ggplot(summary, aes(x=basin, y=sp_rich, color = fish)) +
  geom_point() +
  geom_errorbar(aes(ymin = sp_rich - se, ymax = sp_rich + se), width = 0.2) + 
  labs(x = "basin", y = "average species richness", title = "Species Richness by 10-min sample")
ggplot(sum2, aes(x=basin, y=sp_rich, color = fish)) +
  geom_point() +
  geom_errorbar(aes(ymin = sp_rich - se, ymax = sp_rich + se), width = 0.2) + 
  labs(x = "basin", y = "average species richness", title = "Daily Species Richness")

#then use glmer to model sprich

library(lme4)
m1 <- glmer(sp_rich ~ fish  + (1|basin), family = poisson, data = sprich_by_sample)
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))
summary(m1)

# could also do by time stamp to use all samples
# and add time as a random effect in the model

# list of all possible birds present in selection tables
# ID == "AMPI" | "GCRF" | "WCSP" | "DEJU" | "ROWR" | "MOCH" | "CAFI" | "CLNU" | "SPSA" | "AMRO" | "MOBL" | ID =="FOSP" | ID =="HETH" | ID =="XXHU" | ID =="YRWA" | ID =="NOFL" | ID =="DUFL" | ID =="MALL" | ID =="OCWA" | ID =="BRBL" | ID =="WAVI")

richness <- ddd %>% group_by(Lake) %>%
  summarise(sp_rich = n_distinct(ID))

by_fish <- ddd %>% group_by(fish) %>%
  summarise(sp_rich = n_distinct(ID))

# MARINA 7/23/2018
# library(Rmisc)
# richness.sum <- ddply(dd, .(lake, fish, basin), summarize, richness = n_distinct(ID))
# #richness.sum <- summarySE(richness.sum, measurevar = "richness", groupvars = "fish")
# 
# ggplot(sprich_by_date, aes(x=fish, y=spp_count)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = spp_count - se, ymax = spp_count + se), width = 0.2)
# 
# ggplot(richness.sum, aes(x=fish, y=richness, group = 1)) +
#   geom_point() +
#   facet_wrap(~basin) +
#   geom_line()
# 
# hist(richness.sum$richness)
# library(lme4)
# m1 <- glmer.nb(richness ~ fish + (1|basin), data = richness.sum)
# plot(fitted(m1), resid(m1))
# qqnorm(resid(m1))
# qqline(resid(m1))
# summary(m1)