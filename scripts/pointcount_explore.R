# Script to investigate avian point count data
# TODO----------------------------------------
# for posters/presentations, save vectors of element_text for large things
# i.e. poster_txt <- element_text(face="bold", color="white", size="24")
rm(list = ls())
library(lme4)
library(tidyverse)
library(lubridate)
library(chron)
library(vegan)

# Supplemental tables
lakeinfo <- read_csv("data/lakeinfo.csv", col_names = TRUE)
spec_names <- read_csv("data/species_names.csv") 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Tidying Raw Data (only needs to be done once) ---------------------------

## load data and clean
d <- read.csv(file = "data/PointCountData_Master copy.csv")
colnames(d) <- tolower(colnames(d))
d$date <- mdy(d$date)
d$fish <- factor(ifelse(d$fish == 1, "fishless", "fish"))

## rename basin names to look pretty later
d$basin <- tolower(d$basin)
d$basin <- sub("eastla", "East Lake", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("center", "Center", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("amphit", "Amphitheater", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("wright", "Wright Lakes", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("upkern", "Upper Kern", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("barret", "Barrett Lakes", d$basin, ignore.case =FALSE, fixed=FALSE)

## fix case issues in species and location columns
d$species <- toupper(d$species)
d$location <- toupper(d$location)

## set basin as a factor
d$basin <-as.factor(d$basin)

## change all entries with > to 500m and FO to NA 
d$distance <- as.character(d$distance)
d$distance[regexpr(">", d$distance) != -1] <- "500"
d$distance[regexpr("FO", d$distance) != -1] <- "NA"
d$distance <- as.integer(d$distance) 

d$jday <- yday(d$date)
# add species names to data matrix
d <- merge(d, spec_names, by = "species")

# write out csv for full data matrix 
write_csv(d, path = "data/pointcount_20180816.csv")

# AOU code-species name table (DONE) --------------------------------------

# this is compiled from the list of species from point count data-- missing WAVI and others from audio data
spec_AOU <- sort(unique(d$species))
spec_AOU
spec_names <- c("American Pipit", "American Robin", "Brewer's Blackbird", "Cassin's Finch", "Chipping Sparrow",
                "Clark's Nutcracker", "Dark-eyed Junco", "Dusky Flycatcher", "Fox Sparrow", "Gray-crowned Rosy Finch",
                "Hermit Thrush", "Mountain Bluebird", "Mountain Chickadee", "Northern Flicker", "Red Crossbill", "Rock Wren", 
                "Rufous Hummingbird", "Spotted Sandpiper", "White-crowned Sparrow", "Wilson's Warbler", "Unidentified Hummingbird", 
                "Unidentified Sparrow", "Unidentified Bird", "Yellow-Rumped Warbler")
spec_names <- as.data.frame(cbind(spec_names, spec_AOU))
colnames(spec_names) <- c("spec_name", "species")
# !! WAVI is not listed in the species list. fix this.
WAVI <- c("Warbling Vireo", "WAVI")
spec_names <- rbind(spec_names, WAVI)

write_csv(spec_names, path = "data/species_names.csv")


### START HERE ### (but not yet-- csv written to file not working nicely)
# TODO: fix this
# read in cleaned up point count csv (written to file 2018-08-16) ---------
#d <- read_csv("data/pointcount_20180816.csv") # for some reason this is not working nicely SO FUCK IT
## exploratory plots of species richness and abundance ---------------------

## restrict distance; exclude double-counts and unidentified species
d300 <- d %>% filter(distance < 300 & species != "XXXX" & species !="XXSP" & species != "XXHU")
d150 <- d %>% filter(distance < 150 & doublecount == "" & species != "XXXX" & species !="XXSP" & species != "XXHU")
d100 <- d %>% filter(distance < 100 & doublecount == "" & species != "XXXX" & species !="XXSP" & species != "XXHU")
d50 <- d %>% filter(distance < 50 & doublecount == "" & species != "XXXX" & species !="XXSP" & species != "XXHU")

## e summarises species richness and abundance PER POINT
pt300 <- d300 %>% group_by(basin, fish, location, date, point, time, jday) %>% 
  summarize(nspecies=n_distinct(species), total=n())

pt150 <- d150 %>% group_by(basin, fish, date, point, time, jday) %>% 
  summarize(nspecies=n_distinct(species), total=n())

pt100 <- d100 %>% group_by(basin, fish, date, point, time, jday) %>% 
  summarize(nspecies=n_distinct(species), total=n())

pt50 <- d50 %>% group_by(basin, fish, date, point, time, jday) %>% 
  summarize(nspecies=n_distinct(species), total=n())

### relationship of abundance and sp.rich. with time of day
## species richness and time of morning
ggplot()

# abundance and time of morning
ggplot(pt300,aes(x=time, y=total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~basin)

### relationship with date of year
## first make julian day column

ggplot(pt300,aes(x=jday, y=total)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~basin)

plot(nspecies~fish, data=e50)
plot(total~fish, data=e50)

# plot number of detections by distance
# looks like the majority of detections are made within 150 m:
ggplot(d300, aes(x=distance)) +
  geom_dotplot(binwidth = 10, aes(fill=species, alpha=0.7))


# boxplots by basin -------------------------------------------------------

## species richness
## boxplots represent medians, 25% and 75% quantiles.
pt300 %>%
  filter(year(date)=="2015") %>%
ggplot() +
  geom_boxplot(aes(x=fish, y=nspecies, fill=fish)) +
  facet_wrap(~basin, nrow = 1) +
  scale_fill_manual(values = c(cbPalette[2], cbPalette[6])) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = NULL, 
       x = NULL,
       y = NULL) + 
  theme_bw() +
  theme(strip.text.x = element_blank(), strip.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="bottom", 
        legend.text = element_text(family = "Helvetica", size = 18),
        plot.title = element_text(family = "Helvetica", size=30),
        axis.title = element_text(family = "Helvetica", size=18), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(family = "Helvetica", size=18, angle=0, vjust = 0.75)) +
  guides(fill=guide_legend(title=NULL))

# trying to coerce the boxplots to means and SE

pt300 %>% group_by(basin, fish, location) %>%
  summarise(n_surveys=n(),
            mean_sprich=mean(nspecies),
            sd_count = sd(nspecies),
            se_count = std.error(nspecies))
            
## abundance 

ggplot(data = pt300) +
  geom_boxplot(aes(x=fish, y=total, color=fish)) +
  labs(x="treatment", y="number of birds detected", title="Bird Abundance within 300m, 2014-2016") +
  facet_wrap(~basin) +
  theme_light()

ggplot(data = e150) +
  geom_boxplot(aes(x=fish, y=total, color=fish)) +
  labs(x="treatment", y="number of birds detected", title="Bird Abundance within 150m, 2014-2016") +
  facet_wrap(~basin) +
  theme_light()

ggplot(data = e50) +
  geom_boxplot(aes(x=fish, y=total, color=fish)) +
  labs(x="treatment", y="number of birds detected", title="Bird Abundance within 50m, 2014-2016") +
  facet_wrap(~basin) +
  theme_light()

# plots by species --------------------------------------------------------

## spec summarizes by entire point count, not just the songmeter point
spec <- d300 %>% filter(year(date)=="2015") %>%
  group_by(basin, fish, location, date, point, species) %>% 
  summarize(count=n())

## spread and gather give an entry for each species in the species pool, even if count = 0
spec <- spread(data = spec, key=species, value=count, fill=0)

spec$WAVI <- rep(0,nrow(spec))
colnames(spec)
spec <- gather(data = spec, key = species, ... = AMPI:YRWA) 
unique(spec$species)
library(plotrix) # for calculating standard error TO DO! dbl check this is calculating correctly??

## now calculate mean, median, and se for each species per location (averaging over survey date)
## not working:
# spec_stats_lake <- spec %>% group_by(basin, fish, location, species) %>%
#   summarise(num_surveys = n(),
#             mean_count = mean(value),
#             median_count = median(value),
#             sd_count = sd(value),
#             se_count = std.error(value))

# summary stats by treatment
spec_stats_2015 <- spec %>% filter(year(date) == "2015") %>% group_by(fish, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

# add nice-looking species names to data-frame... had to do this here because spread and gather don't play nice with the spec_names column
spec_stats_2015 <- merge(spec_stats_2015, spec_names, by = "species")

ggplot(spec_stats_2015, aes(x=spec_name, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) +
  geom_errorbar(position=position_dodge(),
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = " mean count of individuals per point (+/- s.e.)", 
       title = "Mean count of individuals per point, year 2015")

# for the poster
doo <- ggplot(spec_stats_2015, aes(x=spec_name, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8) +
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
        axis.text.x = element_text(family = "Helvetica", size=16, angle=90, hjust=1, vjust=0.5),
        axis.text.y = element_text(family = "Helvetica", size=18, angle=0))
doo + geom_errorbar(position=position_dodge(), width=0.8,
                    aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.8))
ggsave(filename = "ptct_by_spp.png", device = "png", path = "poster_plots/", width = 7, height = 5, units = "in")


# all years
spec_stats_fish <- spec %>% group_by(fish, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

spec_stats_fish <- merge(spec_stats_fish, spec_names, by = "species")

ggplot(spec_stats_fish, aes(x=spec_name, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) +
  geom_errorbar(position=position_dodge(),
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "mean count of individuals per point", title = "Mean count of individuals per point, all years")

# spec_sm summarizes for the songmeter point ONLY
spec_sm <- d300 %>% 
  filter(point=="1", year(date)=="2015") %>%
  group_by(basin, fish, location, date, species) %>% 
  summarize(count=n())

n <- spec %>% group_by(basin, fish, lake, elev_m) %>% 
  summarise(n_spec = n_distinct(ID))

ggplot(d=birds)+
  geom_boxplot(aes(x=fish, y=n_spec, fill=fish)) +
  facet_wrap(~basin, nrow = 1) +
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
        axis.text.x = element_blank(), 
        axis.text.y = element_text(family = "Helvetica", size=18, angle=0))



# the following spread/gather gives counts for each species for each date for each lake type
# NOT individual lakes!
spec_sm <- spread(data = spec_sm, key = species, value = count, fill = 0)
spec_sm <- gather(data = spec_sm, key = species, ... = AMPI:YRWA) 
#spec_sm <- as.factor(spec_sm$species)
# now add species names for graphing purposes

spec_sm <- merge(spec_sm, spec_names, by = "species")
spec_sm <- merge(spec_sm, lakeinfo, by.x = "location", by.y = "lake", all.y = FALSE)
# ggplot(spec_sm, aes(x=species, y=value, fill=fish)) + 
#   geom_boxplot() +
#   labs(title = "number of birds per species within 300m of points",
#        x = "species (AOU code)", 
#        y = "number of birds counted") +
#   theme_light()

spec_sm_stats <- spec_sm %>% group_by(fish, spec_name) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

ggplot(spec_sm_stats, aes(x=spec_name, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) + 
  geom_errorbar(position=position_dodge(), 
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "mean count of individuals per point", title = "Mean count of individuals at SongMeter point, year 2015")

# aaaaaand finally, point counts in early july only
spec_stats_july <- spec %>% filter(year(date) == 2015 & month(date) == 7) %>% 
  group_by(fish, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

# and also including lake data
spec_stats_july2 <- spec %>% filter(year(date) == 2015 & month(date) == 7) %>% 
  group_by(basin, fish, location, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

ggplot(spec_stats_july, aes(x=species, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) + 
  geom_errorbar(position=position_dodge(), 
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "mean count of individuals per point", title = "Mean count of individuals at point counts in July 2015") 

# GLMS --------------------------------------------------------------------

ptct <- d300 %>% 
  group_by(basin, fish, location, date, jday, time, point, elev_m, sm_temp) %>%
  summarise(n_spec = n_distinct(spec_name), 
            abundance = n())

#The first model I'll build will focus on species richness only. **MODELS CURRENTLY NOT RUNNING**
# elev needs to be scaled because the numbers are so big:
ptct$elev_m.s <- scale(ptct$elev_m)
ptct_m1 <- glmer(n_spec ~ fish + elev_m.s + (1|basin/lake), family = poisson, data = ptct, glmerControl(calc.derivs = F))


# vegan -------------------------------------------------------------------
# use spec and filter to year = 2015, no BARRETX
spec <- spec %>% filter(location !="BARRETX")

library(reshape2)

#start with the point count data
#First I want to  make it wide instead of long
spec2 = dcast(spec, basin + fish + location + point + date ~ species, value.var = "value", fun.aggregate = sum)
#remove the row names
specComm = spec2[,6:21]

specComm2 = specComm/rowSums(specComm) # relative abundance COOOOOOOOLLLLLLL

spec2 <- merge(spec2, lakeinfo, by.x = "location", by.y = "lake", all.y = FALSE)
spec2 <- merge(spec2, rec_summary, by.x = "location", by.y = "lake", all.y = FALSE)

# SPECIES ACCUMULATION
s3 <- specaccum(specComm, random=TRUE)

# compare with curve generated by listens_explore
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue") 
plot(s3, add=TRUE, ci.type = "poly", col = "darkgreen", lwd=2, ci.lty = 0, ci.col = "lightgreen")
# wow pretty similar... pts cts slightly higher slope but still very much overlapping error

# go by site
Y <- split(spec2, spec2$location)
names(Y) <- unique(spec2$location) # this overwrites the matrices from the listen data!  
list2env(Y, envir = .GlobalEnv) # OMG i'm so cool with these two lines of code

# SPECIES ACCUMULATION CURVES FOR ALL SITES, LISTEN DATA
# TODO: there must be a way to run a for loop to do this 
curve_amphit1 = specaccum(AMPHIT1[, 6:29], method = "random")
curve_amphit2 = specaccum(AMPHIT2[, 6:29], method = "random")
curve_barret1 = specaccum(BARRET1[, 6:29], method = "random")
curve_barret2 = specaccum(BARRET2[, 6:29], method = "random")
curve_center1 = specaccum(CENTER1[, 6:29], method = "random")
curve_center2 = specaccum(CENTER2[, 6:29], method = "random")
curve_upkern1 = specaccum(UPKERN1[, 6:29], method = "random")
curve_upkern2 = specaccum(UPKERN2[, 6:29], method = "random")
curve_wright1 = specaccum(WRIGHT1[, 6:29], method = "random")
curve_wright2 = specaccum(WRIGHT2[, 6:29], method = "random")

#plot curve_all first
plot(s3)
#then plot the rest
plot(curve_amphit1, add = TRUE, col = cbPalette[1]) 
plot(curve_amphit2, add = TRUE, col = cbPalette[2])
plot(curve_barret1, add = TRUE, col = cbPalette[3])
plot(curve_barret2, add = TRUE, col = cbPalette[4])
plot(curve_center1, col = cbPalette[5])
plot(curve_center2, add = TRUE, col = cbPalette[6])
plot(curve_upkern1, add = TRUE, col = cbPalette[7])
plot(curve_upkern1, add = TRUE, col = cbPalette[8])
plot(curve_wright1, add = TRUE, col = cbPalette[9])
plot(curve_wright2, add = TRUE, col = cbPalette[10])


# community composition
# try a PERMANOVA
spec2$elev_m.s <- scale(spec2$elev_m)

p1 = adonis(specComm~fish + basin + date + elev_m.s, data = spec2)
p1

#Yes, basins are different and fish lakes are different
#but how?

n1 = metaMDS(specComm, trymax = 999, distance = "raup")
n1

source("scripts/plotNMDS.R")
PlotNMDS(n1, data = spec2, group = "fish")
# nada. why did the NMDS show significant effects tho?! 


# summaries separated by basin --------------------------------------------

spec_center <- d300 %>% 
  filter(point=="1" & basin=="Center") %>%
  group_by(basin, fish, date, species) %>% 
  summarize(count=n())

spec_center <- spread(data = spec_center, key = species, value = count, fill = 0)
spec_center <- gather(data = spec_center, key = species, ... = AMPI:YRWA) 

spec_center_stats <- spec_center %>% group_by(fish, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

ggplot(spec_center_stats, aes(x=species, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) + 
  geom_errorbar(position=position_dodge(), 
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  labs(title="Mean counts of birds within 300m of SongMeter point, Center Basin, 2014-2016", 
       x="species", 
       y="mean count of individuals +/- s.e.m.")

spec_amphit <- d300 %>% 
  filter(point=="1" & basin=="Amphitheater") %>%
  group_by(basin, fish, date, species) %>% 
  summarize(count=n())

spec_amphit <- spread(data = spec_amphit, key = species, value = count, fill = 0)
spec_amphit <- gather(data = spec_amphit, key = species, ... = AMPI:WCSP) 

spec_amphit_stats <- spec_amphit %>% group_by(fish, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

ggplot(spec_amphit_stats, aes(x=species, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) + 
  geom_errorbar(position=position_dodge(), 
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  labs(title="Mean counts of birds within 300m of SongMeter point, Amphitheater Basin, 2014-2016", 
       x="species", 
       y="mean count of individuals +/- s.e.m.")

spec_upkern <- d300 %>% 
  filter(point=="1" & basin=="Upper Kern") %>%
  group_by(basin, fish, date, species) %>% 
  summarize(count=n())

spec_upkern <- spread(data = spec_upkern, key = species, value = count, fill = 0)
spec_upkern <- gather(data = spec_upkern, key = species, ... = AMPI:WCSP) 

spec_upkern_stats <- spec_upkern %>% group_by(fish, species) %>%
  summarise(num_surveys = n(),
            mean_count = mean(value),
            median_count = median(value),
            sd_count = sd(value),
            se_count = std.error(value))

ggplot(spec_upkern_stats, aes(x=species, y=mean_count, fill=fish)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.6) + 
  geom_errorbar(position=position_dodge(), 
                aes(ymin = mean_count-se_count, ymax = mean_count+se_count, width = 0.6)) +
  labs(title="Mean counts of birds within 300m of SongMeter point, Upper Kern, 2014-2016", 
       x="species", 
       y="mean count of individuals +/- s.e.m.")


# OLD CODE ----------------------------------------------------------------

# trying to present different summary stats for easier legibility


# TO DO: dotplot of sp rich with species ID (like Madi made)

ggplot()

## EDA just 2015 data
d2015 <- d[year(d$date) == 2015, ]

# grouping all-years data by year, date, and summarizing # of species and total counts
abd <- d %>% group_by(basin, fish, year=year(date), month=month(date)) %>% 
  summarize(nspecies=n_distinct(species), total=n())

# 
abdd <- d2015 %>% filter(date > "2015-06-07" & date < "2015-07-06" & distance < 151 & doublecount == "") %>% 
  group_by(basin, fish) %>% 
  summarize(nspecies=n_distinct(species), total=n())

plot(total~fish, data=abdd)
plot(nspecies~fish, data=abdd)

# summarize by species, too
# spec2015 filters down to one point count per site
spec2015 <- d300 %>% filter(date > "2015-06-07" & date < "2015-07-06") %>% 
  group_by(basin, fish, species) %>% 
  summarize(nspecies=n_distinct(species), total=n())



# basic bar graph of bird counts by species by basin ## THIS IS WRONG-- DOESN'T GIVE TRUE COUNT VALUES ##
b <- ggplot(spec2015, aes(x=species, y=total, fill=fish)) + 
  geom_bar(stat="identity", position=position_dodge(), width=0.8) +
  labs(title = "number of birds per species at fish-containing and fishless lakes",
       x = "species (AOU code)", 
       y = "number of birds counted") +
  theme(legend.title = element_blank(),
        plot.title = element_text(face="bold", size=16),
        axis.title = element_text(face="bold", size=16),
        axis.text.x = element_text(size=14, angle=45,hjust=1,vjust=0.5),
        axis.text.y = element_text(size=14, angle=0))
b

c <- ggplot(spec2015, aes(x=species, y=total, fill=fish)) + 
  geom_bar(stat="identity", position=position_dodge(), width=0.8) +
  labs(title = "number of birds per species at fish-containing and fishless lakes",
       x = "species (AOU code)", 
       y = "number of birds counted") +
  facet_wrap(~basin)
c
# summary stats for abdd
poop <- abdd %>%
  group_by(fish) %>%
  summarise_each(funs(mean, sd(., na.rm=TRUE)), -basin)

# abundance 
b <- ggplot(poop, aes(x=fish, y=total_mean, fill=fish)) +
  geom_bar(stat="identity", width = 0.6, position=position_dodge()) +
  geom_errorbar(aes(ymin=total_mean-total_sd, 
                    ymax=total_mean+total_sd), 
                    width=.2, 
                    position=position_dodge(0.9))
b + labs(x = "species (AOU code)", y = "number of birds counted") +
  theme(legend.position="none",
            plot.title = element_text(face="bold", size=16),
            axis.title = element_text(face="bold", size=16),
            axis.text.x = element_text(size=14, angle=0),
            axis.text.y = element_text(size=14, angle=0))

# sprich
s <- ggplot(poop, aes(x=fish, y=nspecies_mean, fill=fish)) +
  geom_bar(stat="identity", width = 0.6, position=position_dodge()) +
  geom_errorbar(aes(ymin=nspecies_mean-nspecies_sd, 
                    ymax=nspecies_mean+nspecies_sd), 
                width=.2, 
                position=position_dodge(0.9))
s + labs(title = "bird species richness at fish-containing and fishless lakes",
         x = "lake type", 
         y = "number of species counted") +
  theme(legend.position="none",
        plot.title = element_text(face="bold", size=16),
        axis.title = element_text(face="bold", size=16),
        axis.text.x = element_text(size=14, angle=0),
        axis.text.y = element_text(size=14, angle=0))

# using boxplots
# for abundance (number of birds)
abun <- ggplot(abdd) + geom_boxplot(aes(x=fish, y=total, fill=fish)) +
  labs(x = "lake type", 
       y = "# of birds counted") + 
  theme(legend.position="none", 
        plot.title = element_text(face="bold", size=16),
        axis.title = element_text(face="bold", size=16), 
        axis.text.x = element_text(face="bold", size=14, angle=0), 
        axis.text.y = element_text(size=14, angle=0)) +
  
  expand_limits(y=0)
  
abun

#for species richness
sprich <- ggplot(abdd) + geom_boxplot(aes(x=fish, y=nspecies, fill=fish)) +
  labs(title = "species richness at fish-containing and fishless lakes", 
       x = "lake type", 
       y = "# of species observed") + 
  theme(legend.position="none", 
        plot.title = element_text(face="bold", size=16),
        axis.title = element_text(face="bold", size=16), 
        axis.text.x = element_text(size=14, angle=0), 
        axis.text.y = element_text(size=14, angle=0)) +
  expand_limits(y=0)

sprich
