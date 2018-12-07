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
library(viridis)

cbPalette <- viridis(10)
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
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
clipNames$name <- substr(clipNames$name, 1, 23)

clipNames$name[!clipNames$name %in% file_names] # shows us the identity of the file that is missing in file_names
# the above doesn't work for me anymore

audio <- do.call("rbind", lapply(file_names, read.delim))

#miss <- clipNames$hidden_name[!clipNames$hidden_name %in% unique(d$Begin.File)] # returns eclipsed filename of any missing files in the dataframe

audio <- merge(audio, clipNames, by.x = "Begin.File", by.y = "hidden_name") # simple merge of files. adding all=T as an argument would include non-matches

# Step 2: select columns relevant to initial exploration of species data
audio <- audio %>% select(Begin.File, Selection, Begin.Time..s., Delta.Time..s., Max.Power..dB., ID, NOTES, name)

# add columns for lake, treatment, and basin
audio <- audio %>% separate(name, c("lake", "date", "time"), sep="_", remove=FALSE)
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

write_csv(audio, path = "data/audio_listen_data_clean_20181125.csv",col_names = TRUE)




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
# TODO: why is the above not working? "Error in select(., identifier, timestamp, temp) : unused arguments (identifier, timestamp, temp)""
# new dataframe with temperature information
# moving on without temps then
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
birds <- merge(birds, lakeinfo[,c(1,4,5)], by = "lake")
write_csv(birds, "data/listendata_allbirds.csv")

# Mayfly data (optional) --------------------------------------------------

mayfly <- read_csv("data/insects/mayfly_counts.csv")

mayfly_smry <- mayfly %>% group_by(round, basin, lake, fish) %>%
  summarise(dailysum = sum(mayfly/daysin), 
            mean = mean(mayfly),
            sd = sd(mayfly))

ggplot(data = mayfly_smry) +
  geom_col(aes(x = lake, y = mean, fill = fish)) +
  geom_error

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
  geom_boxplot(aes(x = lake, y = mean_power))
 
 background_avg_power <- audio %>% filter(ID == "background") %>%
   group_by(lake, date) %>%
   summarize(num_bk_measures = n(),
             mean_power = mean(Max.Power..dB.),
             median_power = median(Max.Power..dB.),
             min_power = min(Max.Power..dB.),
             max_power = max(Max.Power..dB.),
             sd_power = sd(Max.Power..dB.)) 

# basically... this sucks; it implies that the noise floors of the mics are very different, which means that detectability may be significantly different between them
# TO DO: contact Kurt to characterize background noise more robustly by site



# Call activity -----------------------------------------------------------
# for all recordings at once (no stats)
# first need to add recording info in order to scale by # seconds sampled
birds <- merge(birds, rec_summary[,c(1,3,5)], by = "lake")
#birds <- merge(birds, lakeinfo, by = "lake")

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

# add names of species not present in recordings (for figure comparison w pt ct)
# which entries in foo have a species name that does not match any species in spec_stats_2015?
foo3$ID[!foo3$ID %in% unique(spec_stats_2015$species)] 
# and conversely, which entries in spec_stats_2015 have a species name that does not match any species in foo3?
spec_stats_2015$species[!spec_stats_2015$species %in% unique(foo3$ID)]

# now create columns for every combo of ID x location
foo3 <- spread(data = foo3, key = ID, value = ncalls, fill = 0)

# so we need to add CHSP, BRBL, and WIWA to foo3, and WAVI to spec_stats_2015
foo3$BRBL <- rep(0,nrow(foo3))
foo3$CHSP <- rep(0,nrow(foo3))
foo3$WIWA <- rep(0,nrow(foo3))

# then regather the data with all the species columns
# run colnames to see which is the last column
colnames(foo3) # it's WIWA
foo3 <- gather(data = foo3, key = ID, value = ncalls, ... = AMPI:YRWA)    

# check to see that it gathered the new columns
unique(foo3$ID) # yay!

# log calltime?

# foostats calculates the mean # of seconds per 10-min recording that each species calling, regardless of basin
foostats3 <- foo3 %>% group_by(fish, ID) %>%
  summarise(mean = mean(ncalls),
            sd=sd(ncalls),
            sem=std.error(ncalls))

foostats3 <- merge(foostats3, spec_names, by.x = "ID", by.y = "species")

f <- ggplot(foostats3) +
  geom_bar(width=0.8, aes(x=spec_name, y=mean, fill=fish),
           stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c(cbPalette[2], cbPalette[6])) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = NULL, 
       x = NULL,
       y = NULL) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none", 
        plot.title = element_text(family = "Helvetica", size=30),
        axis.title = element_text(family = "Helvetica", size=18), 
        axis.text.x = element_text(family = "Helvetica", size=12, angle=50, hjust=1, vjust=1),
        axis.text.y = element_text(family = "Helvetica", size=18, angle=0))
  
f + geom_errorbar(data = foostats3, width=0.8,
                   aes(x=spec_name, y=mean, ymin = mean-sem, ymax = mean+sem, 
                       fill=fish), position=position_dodge())
ggsave(filename = "listens_by_spp_2.png", device = "png", path = "poster_plots/", width = 7, height = 3, units = "in")



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





# Rarefaction/Specaccum in vegan --------------------------------------------

library(vegan)

# prepping data for rarefaction

divmatrix <- birds %>% group_by(lake, ID, name) %>%
  #summarise(time_calling = sum(Delta.Time..s.)) # seconds the species is audible within a 10-minute window 
  summarise(n_obs = n())

# make by lake
# divmatrix_lake <- divmatrix %>% group_by(lake, ID) %>%
#   summarise(n_obs = n())

divmatrix <- spread(data = divmatrix, 
                         key = ID, 
                         value = n_obs,
                         fill = 0)

# divmatrix_lake$n_surveys <- rec_summary$n_samples
divmatrix <- as.data.frame(divmatrix)
rownames(divmatrix) <- divmatrix[,2]
divmatrix <- divmatrix[,3:19]

# anddddd presence-absence matrix
pa_matrix <- divmatrix
pa_matrix[pa_matrix > 0] <- 1

# vegdist(divmatrix, method = "raup", binary = T) dunno what this is

# we also need an "environmental" data matrix with info about elevation, lake area, etc.

divmatrix.env <- birds %>% group_by(name, basin, fish, lake, date, time, elev_m, area_msq) %>%
  summarise(n_obs = n()) # don't really need this but need to summarise something in order to make the table
divmatrix.env <- as.data.frame(divmatrix.env)
rownames(divmatrix.env) <- divmatrix.env[,1]
divmatrix.env <- divmatrix.env %>% select(-name)

# species accumulation
# this is going to generate a curve that summarises ALL 10-minute recordings
# it'll show us the number of samples we need in order to sample the theoretical complete community
# later i'll do it by lake

sp1 <- specaccum(pa_matrix, "random")

summary(sp1)
plot(sp1, ci.type="poly", lwd=2, ci.lty=0, ci.col="lightgray") 
 
# plotting sp1-4, it seems like at least for specaccum, abundance data are not used

# by lake
# janky but will do for now. 
# TODO: re-do the way I make divmatrix to reduce redundancy here

divtot.audio <- cbind(divmatrix, divmatrix.env) 
X <- split(divtot.audio, divtot.audio$lake)
names(X) <- unique(divtot.audio$lake) 
list2env(X, envir = .GlobalEnv) # OMG i'm so cool with these two lines of code

# SPECIES ACCUMULATION CURVES FOR ALL SITES, LISTEN DATA
# TODO: there must be a way to run a for loop to do this 
curve_amphit1 = specaccum(AMPHIT1[, 1:17], method = "random")
curve_amphit2 = specaccum(AMPHIT2[, 1:17], method = "random")
curve_barret1 = specaccum(BARRET1[, 1:17], method = "random")
curve_barret2 = specaccum(BARRET2[, 1:17], method = "random")
curve_center1 = specaccum(CENTER1[, 1:17], method = "random")
curve_center2 = specaccum(CENTER2[, 1:17], method = "random")
curve_upkern1 = specaccum(UPKERN1[, 1:17], method = "random")
curve_upkern2 = specaccum(UPKERN2[, 1:17], method = "random")
curve_wright1 = specaccum(WRIGHT1[, 1:17], method = "random")
curve_wright2 = specaccum(WRIGHT2[, 1:17], method = "random")

#plot curve_all first
plot(sp1, ci.type="poly", lwd=2, ci.lty=0, ci.col="lightgray", xlab = "", ylab="")
#then plot the rest
plot(curve_amphit1, add = TRUE, col = cbPalette[1], lwd=2, ci.lty=0) 
plot(curve_amphit2, add = TRUE, col = cbPalette[2], lwd=2, ci.lty=0)
plot(curve_barret1, add = TRUE, col = cbPalette[3], lwd=2, ci.lty=0)
plot(curve_barret2, add = TRUE, col = cbPalette[4], lwd=2, ci.lty=0)
plot(curve_center1, add = TRUE, col = cbPalette[5], lwd=2, ci.lty=0)
plot(curve_center2, add = TRUE, col = cbPalette[6], lwd=2, ci.lty=0)
plot(curve_upkern1, add = TRUE, col = cbPalette[7], lwd=2, ci.lty=0)
plot(curve_upkern2, add = TRUE, col = cbPalette[8], lwd=2, ci.lty=0) 
plot(curve_wright1, add = TRUE, col = cbPalette[9], lwd=2, ci.lty=0)
plot(curve_wright2, add = TRUE, col = cbPalette[10], lwd=2, ci.lty=0)
legend("bottomright", pool$lake, fill=cbPalette[1:10], bty="n")
# etc very cool thanks I think I need to do rarefaction if I want to figure out the "howmany samples needed" question
# specaccum of the entire community suggests probably somewhere aroud 40 samples 

# now make an insert with just the lake-level specaccum estimates
plot(curve_amphit1, add = TRUE, col = cbPalette[1]) 
plot(curve_amphit2, add = TRUE, col = cbPalette[2])
plot(curve_barret1, add = TRUE, col = cbPalette[3])
plot(curve_barret2, add = TRUE, col = cbPalette[4])
plot(curve_center1, col = cbPalette[5], xlab = "", ylab="")
plot(curve_center2, add = TRUE, col = cbPalette[6])
plot(curve_upkern1, add = TRUE, col = cbPalette[7])
plot(curve_upkern2, add = TRUE, col = cbPalette[8]) # start here because it has the most sites
plot(curve_wright1, add = TRUE, col = cbPalette[9])
plot(curve_wright2, add = TRUE, col = cbPalette[10])
abline(v=7, add=TRUE)
legend("bottomright", pool$lake, fill=cbPalette[1:10], bty="n")
specpool(divmatrix,pool = divmatrix.env$lake, smallsample = TRUE)
pool = with(divmatrix.env, specpool(divmatrix, lake))

fish <- rep(c("fishless", "fish"), 5)
pool$fish <- fish
pool$lake = rownames(pool)

ggplot(d=pool)+
  geom_point(aes(x=Species, y=chao, color=lake)) +
  coord_cartesian(xlim=c(0,16), ylim = c(0,16)) +
  geom_abline(intercept = 0, slope = 1) 

ggplot(data=pool, aes(x=lake, y=chao, fill=lake)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(position=position_dodge(), 
                aes(ymin=chao-chao.se, ymax=chao+chao.se)) +
  scale_fill_viridis_d()

# now just fish/fishless
# note that this reassigns X to new things!
divtot.audio <- cbind(divmatrix, divmatrix.env) 
X <- split(divtot.audio, divtot.audio$fish)
#names(X) <- "" 
list2env(X, envir = .GlobalEnv)

curve_fish = specaccum(fish[, 1:17], method = "random")
curve_fishless = specaccum(fishless[, 1:17], method = "random")

plot(sp1, ci.type="poly", lwd=2, ci.lty=0, ci.col="lightgray", xlab = "# of samples", ylab="cumulative species richness")
plot(curve_fishless, add = TRUE, col = cbPalette[6])
plot(curve_fish, add = TRUE, col = cbPalette[2]) 
legend("bottomright", legend=c("fishless", "fish-containing", "all lakes"), fill=cbPalette[c(2,6,1)], bty="n")

# are lake types different according to audio data?
# try a PERMANOVA
p1 = adonis(pa_matrix ~ fish*basin + mean_temp + elev_m, data = divmatrix.env, method = "raup")
p1

p2 = adonis(specComm2 ~fish*basin + date, data = spec2)

#Yes, basins are different and fish lakes are different
#but how?

n1 = metaMDS(divmatrix, trymax=100)
n1

# RAREFACTION CURVES FOR ALL SITES, LISTEN DATA
# NOT WORKING ARGH
# first, all the data
S <- specnumber(divmatrix) # observed number of species
(raremax <- min(rowSums(divmatrix))) # this calculates the minimum number of 
Srare <- rarefy(pa_matrix, 1) # rarefied number of species 
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(pa_matrix, step = 1, sample = raremax, col = divmatrix.env$fish, cex = 0.6)

skjgnsdf <- birds %>% group_by(identifier) %>%
  summarise(sel=n(),
            spp=n_distinct(ID))


# MODEL STUFF -------------------------------------------------------------

# prepping for GLM modeling species richness

dat <- birds %>% select(identifier, basin, fish, lake, timestamp, elev_m, ID) 

richlake <- birds %>% group_by(basin, fish, lake, elev_m) %>% 
  summarise(n_spec = n_distinct(ID))

dat.sum <- dat %>% group_by(basin, fish) %>%
  summarise(mean_rich = mean(n_spec),
            median_count = median(n_spec),
            sd_count = sd(n_spec),
            se_count = std.error(n_spec))

ggplot(d=dat)+
  geom_boxplot(aes(x=fish, y=n_spec, fill=fish), width=0.4) +
  #facet_wrap(~basin, nrow = 1) +
  scale_fill_manual(values = c(cbPalette[2], cbPalette[6])) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = NULL, 
       x = NULL,
       y = NULL) + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        plot.title = element_text(family = "Helvetica", size=30),
        axis.title = element_text(family = "Helvetica", size=18), 
        axis.text.x = element_text(family = "Helvetica", size=18),
        axis.text.y = element_text(family = "Helvetica", size=18, angle=0))
  

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
m1 <- glmer(n_spec ~ fish + elev_m.s + (1|basin/lake), family = poisson, data = dat, glmerControl(calc.derivs = F))
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

ggplot(data=dat.sum, aes(x=n_samples, y=tot_spec)) +
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
