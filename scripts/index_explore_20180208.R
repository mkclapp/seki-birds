# Author: Mary Clapp
# Script to explore and display Acoustic Index Data 
# Edited 2/8/2018 from previous version to run on acoustic indices generated 2/5/2018

rm(list=ls(all=TRUE)) 

# LOAD libraries and tools ------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(chron)
library(MuMIn)

# colorblind-friendly palette: grey, orange, sky blue, green, yellow, navy, red, pink
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# COLLECT and CLEAN UP 10-minute ACI data from all sites  ------------------------------

# create master data matrix from .csv files 
#file_names <- list.files("../data/NVSPL/2015/CENTER", full.names = TRUE) #add extension definition?
#file_names <- list.files("../data/indices/2015/ten", full.names = TRUE) 
#file_names <- list.files("../data/indices/2014/ten", full.names = TRUE) 
file_names <- list.files("../data/indices/2018_02_05/", full.names = TRUE, pattern = "*.csv")

d <- do.call("rbind",lapply(file_names,read.csv))
d <- subset(d, select=c(Site, Date, Yr, Mo, Day, Hr, Min, Sec, ACIout, BKdB_low, BKdB_bird, avgAMP,
                        L10AMP, Hf, Ht, EI, Rough, ADI_step, Eveness_step, AR))

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

# add a new column with a binary windy/stormy variable

d$wind <- d$BKdB_low > 60
# d$hrs <- as.factor(hours(d$time)) #for looking at ACI by hour... I don't think I need this

# all season, between hours of 5:10 and 8:10
morning <- subset(d, format(Timestamp, '%H:%M') >= "05:10" & 
                    format(Timestamp, '%H:%M') <= "09:10")

# june 1 - july 21, morning hours
# breeding <- subset(d, format(Timestamp, '%H:%M') >= "05:10" &
#                  format(Timestamp, '%H:%M') <= "08:10" &
#                  format(day, "%m-%d") >= "06-01" &
#                  format(day, "%m-%d") <= "07-21")

# wrightweek <- subset(wright, 
#                      format(day, "%m-%d") >= "07-01" &
#                      format(day, "%m-%d") <= "07-05")

# june 1 - july 15, morning hours, days with background noise >60dB removed
# dquiet <- subset(d, wind == FALSE) 
# all season, morning hours, days with background noise >60dB removed
# quietam <- subset(morning, wind == FALSE)
# subset matrix by site
center2015 <- morning %>%
  filter(basin == "CENTER", Yr == 2015)
center2014 <- morning %>%
  filter(basin == "CENTER", Yr == 2014)
center2016 <- morning %>%
  filter(basin == "CENTER", Yr == 2016)
amphit2015 <- morning %>%
  filter( basin == "AMPHIT", Yr == 2015)
amphit2014 <- morning %>%
  filter(basin == "AMPHIT", Yr == 2014)
eastla2014 <- morning %>%
  filter(basin == "EASTLA", Yr == 2014)
barret2015 <- morning %>%
  filter(basin == "BARRET")
upkern2015 <- morning %>%
  filter(basin == "UPKERN")
wright2015 <- morning %>%
  filter(basin == "WRIGHT")


# summary stats tables for each site (ACI)
c16 <- center2016 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

a15 <- amphit2015 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

a14 <- amphit2014 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

c14 <- center2014 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

e14 <- eastla2014 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

u15 <- upkern2015 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

w15 <- wright2015 %>% 
  group_by(Mo, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

all_basin <- morning %>% 
  group_by(basin, fish, Mo) %>%
  summarize(N = length(ACIout),
             mean_ACI = mean(ACIout), 
             median_ACI = median(ACIout), 
             sd_ACI = sd(ACIout),
             se = sd_ACI / sqrt(N))

all <- morning %>% 
  group_by(fish, Mo, Yr) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

# WRITE OUT .csv files for each basin (optional)  -------------------------------
# Write out dataframe to .csv if you want to manually add wind annotations from NVSPL spectrograms:
# write.csv(center, file = "centerdata.csv")
# centersub <- read.csv("centerdata.csv")

# Not sure why I did this
# centersub <- centersub[centersub$wind == 2,]
# centersub$day <- mdy(centersub$day)


# RANDOMLY SAMPLE 10-min intervals (optional) ------------

c <- center2015[sample(nrow(center2015), 50), ]
w <- wright2015[sample(nrow(wright2015), 50), ]
listen <- morning[sample(nrow(morning), 80), ]


# PLOT Acoustic Indices! ------------------------------------------

# MEAN ACI by MONTH by BASIN +/- standard error:
ggplot(data = w15, aes(x = Mo, y = mean_ACI, group=fish, color=fish)) +
  geom_line() +
  #facet_wrap(~Yr, ncol=1)
  geom_errorbar(aes(ymin=mean_ACI-se, ymax=mean_ACI+se), width=.2)
# standard error isn't actually appropriate here because my data are non-independent 
# need to do some bayesian shite to address this
  

# SEASONAL PLOTS of ACI by BASIN:
# TODO: grey out windy days, make scales relative to format for ppt slides,
# TODO: scale x by actual date range, not relative path to center df
centerAR <- ggplot() +
  geom_boxplot(data = center2014, aes(x=day, y=Rough, group=interaction(day, fish), fill=fish)) + 
  #facet_wrap(~basin, ncol=1) + 
  theme_bw() +
  labs(title = "Acoustic Roughness", subtitle = "CENTER Basin, Morning Hours 2014", x = "Date", y = "Acoustic Roughness") +
  # the below code is for making PPT graphs:
  # theme(axis.title = element_text(size = 26), 
  #       axis.text = element_text(size = 26), 
  #       plot.title = element_text(size = rel(2), face = "bold"),
  #       legend.title = element_text(size = 26, face = "bold"),
  #       legend.text = element_text(size = 26),
  #       legend.position = "none",
  #       strip.text.x = element_text(size = 24)) 
  ylim(0, 40) 
  # + scale_x_date(limits=c(min(center$day), max(center$day))) #removes some outliers for easier reading
centerAR

# ACI
centerACI <- ggplot() +
  geom_boxplot(data = center2015, aes(x=day, y=ACIout, group=interaction(day, fish), fill=fish)) + 
  theme_bw() +
  labs(title = "Acoustic Complexity Index (ACI)", 
       subtitle = "CENTER Basin, Morning Hours 2015", 
       x = "Date", y = "Acoustic Complexity Index (ACI)") +
  ylim(0,1.5)
centerACI

amphitACI <- ggplot() +
  geom_boxplot(data = amphit2015, aes(x=day, y=ACIout, group=interaction(day, fish), fill=fish)) + 
  theme_bw() +
  labs(title = "Acoustic Complexity Index (ACI)", 
       subtitle = "AMPHIT Basin, Morning Hours 2015", 
       x = "Date", y = "Acoustic Complexity Index (ACI)") +
  ylim(0,1.5)
amphitACI

amphitAR <- ggplot() +
  geom_boxplot(data = amphit, aes(x=day, y=Rough, group=interaction(day, fish), fill=fish)) + 
  theme_bw() +
  labs(title = "Acoustic Complexity Index (ACI)", 
       subtitle = "AMPHIT Basin, Morning Hours 2015", 
       x = "Date", y = "Acoustic Roughness (AR)") +
  ylim(0,1.5)
amphitAR

amphitplot <- ggplot(data = amphit, aes(x=day, y=Rough, group=interaction(day, fish))) +
  geom_boxplot(aes(fill=fish)) + 
  facet_wrap(~basin, ncol=1) + 
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  ylim(0, 1.5) + scale_x_date(limits=c(min(center$day), max(center$day)))
amphitplot

eastlaplot <- ggplot(data = eastla, aes(x=day, y=ACIout, group=interaction(day, fish))) +
  geom_boxplot(aes(fill=fish)) +
  facet_wrap(~basin, ncol=1) +
  theme_bw() +
  labs(x = NULL, y = "Acoustic Complexity Index (ACI)") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  ylim(0, 1.5) + scale_x_date(limits=c(min(center$day), max(center$day)))
eastlaplot

barretplot <- ggplot(data = barret, aes(x=day, y=ACIout, group=interaction(day, fish))) +
  geom_boxplot(aes(fill=fish)) + 
  facet_wrap(~basin, ncol=1) + 
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.title = element_text(size = 26), 
        axis.text = element_text(size = 26), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 26),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  ylim(0, 1.5) + scale_x_date(limits=c(min(center$day), max(center$day)))
barretplot

upkernplot <- ggplot(data = upkern, aes(x=day, y=ACIout, group=interaction(day, fish))) +
  geom_boxplot(aes(fill=fish)) + 
  facet_wrap(~basin, ncol=1) + 
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.title = element_text(size = 26), 
        axis.text = element_text(size = 26), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 26),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  ylim(0, 1.5) + scale_x_date(limits=c(min(center$day), max(center$day)))
upkernplot

wrightplot <- ggplot(data = wright, aes(x=day, y=ACIout, group=interaction(day, fish))) +
  geom_boxplot(aes(fill=fish)) + 
  facet_wrap(~basin, ncol=1) + 
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.title = element_text(size = 26), 
        axis.text = element_text(size = 26), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 26),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  ylim(0, 1.5) + scale_x_date(limits=c(min(center$day), max(center$day)))
wrightplot

# look for outliers in ACI
center_outliers <- center2015 %>%
  filter(ACIout > (mean(ACIout) + sd(ACIout)))

amphit_outliers <- amphit %>%
  filter(ACIout > 0.5)

# relationship between ACI and AR

index_compare <- ggplot() +
  geom_point(data = center, aes(x=ACIout, y=Rough), alpha=0.1)


# EXPLORE background noise ----------------------------------------

# background noise
centernoise <- ggplot() +
  geom_boxplot(data = center, aes(x=day, y=BKdB_low, group=interaction(day, fish), fill=fish)) + 
  theme_bw() +
  labs(title = "Background Noise (dB)", 
       subtitle = "CENTER Basin, Morning Hours 2016", 
       x = "Date", y = "Background Noise Level (dB)") 
centernoise

# overall SPL 
centerloudness <- ggplot() + 
  geom_boxplot(data = center, aes(x=day, y=BKdB_bird, group=interaction(day, fish), fill=fish)) + 
  theme_bw() +
  labs(title = "Average Amplitude, bird frequencies (dB)", 
       subtitle = "CENTER Basin, Morning Hours 2016", 
       x = "Date", y = "(dB)") 
centerloudness

# relationship between background noise or SPL and acoustic indices
ggplot(data = center, aes(x=BKdB_low, y=ACIout)) +
  geom_point()

ggplot(data = center, aes(x=BKdB_low, y=Rough)) +
  geom_point()



#1 day only
wrightplot <- ggplot(data = wright, aes(x=Timestamp, y=ACIout, group=interaction(day, fish))) +
  geom_line(aes(fill=fish)) + 
  facet_wrap(~basin, ncol=1) + 
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.title = element_text(size = 26), 
        axis.text = element_text(size = 26), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 26),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  ylim(0, 1.5) + scale_x_date(limits=c(min(center$day), max(center$day)))
wrightplot

# ACI for one month at a time
# ggplot(morning[month(morning$day) == 06,], aes(x=day, y=ACIout, group=interaction(day, fish))) +
#   geom_boxplot(aes(fill=fish)) + 
#   facet_wrap(~basin, ncol=1)

# TODO: change ggplot scales to free y scale

# sound pressure level all sites together: this is a clusterf*ck of a graph
ggplot(quietam, aes(x=day, y=BKdB_bird, group=interaction(day, fish))) +
  geom_boxplot(aes(fill=fish)) + 
  facet_wrap(~basin, ncol=1)


# DIEL plots of ACI -------------------------------------------------------
# diel aci, all sites together
ggplot(d) + 
  geom_boxplot(aes(x=as.factor(hours(time)), y=ACIout))

ggplot(d) +
  geom_boxplot(aes(x=as.factor(hours(time)), y=ACIout)) +
  theme_bw() +
  labs(x = "hour of the day", y = "Acoustic Complexity Index (ACI)") +
  theme(axis.title = element_text(size = 26), 
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 26),
        legend.position = "none",
        strip.text.x = element_text(size = 24)) +
  facet_wrap(~fish, ncol=1)

# diel aci, separated by basin
ggplot(d) + 
  geom_boxplot(aes(x=as.factor(hours(time)), y=ACIout)) + facet_wrap(~basin)

# diel aci with windiest days removed, separated by basin
ggplot(dquiet) + 
  geom_boxplot(aes(x=as.factor(hours(time)), y=ACIout)) + facet_wrap(~basin)

###---------------------------------------------------------------------------###
### EXPLORATION of RELATIONSHIP between ACI and BACKGROUND NOISE
###---------------------------------------------------------------------------###

bah <- smoothScatter(breeding$BKdB_low, breeding$ACIout,
              xlab= "Mean Background Noise (dB)", ylab="Acoustic Complexity Index (ACI)",
              cex.lab = 1.5)
              
lines(lowess(breeding$BKdB_low, breeding$ACIout), lwd=3) #lowess  = non-parametric smoothing (like least squares)


# ggplot(d, aes(x=BKdB_low, y=ACIout)) +
#   geom_point(alpha=0.1) 

 ggplot(morning, aes(x=BKdB_bird, y=ACIout)) +
   geom_point(alpha=0.1)

# density plots of ACI

ggplot(breeding, aes(x=ACIout, fill=fish)) +
  geom_density(alpha = 0.3) +
  scale_x_log10() +
  facet_wrap (~basin) +
  theme_bw() +
  labs(title = NULL, x = "Acoustic Complexity Index (ACI)", y = "density") +
  theme(axis.title = element_text(size = 26), 
        axis.text = element_text(size = 26), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 26),
        legend.position = "none",
        strip.text.x = element_text(size = 24, face = "bold"))

ggplot(morning, aes(x=ACIout, fill=fish)) +
  geom_density(alpha = 0.3) +
  scale_x_log10() +
  facet_wrap (~basin)

# TODO: how to coerce timestamps with non-zero seconds into the nearest h:m bin?

table(dd$hrs)
ggplot(dd) + geom_boxplot(aes(x=hrs, y=ACIout))

#windiest days removed
ggplot(dquiet) + geom_boxplot(aes(x=hrs, y=ACIout)) + facet_wrap(~fish)

###---------------------------------------------------------------------------###

## STEP ???: MODELING. 

# preliminary models that don't mean anything: 
m1 <- lm(ACIout ~ fish + (1 | Site) + (1 | basin) + day + BKdB_low, data = morning) 

m2 <- glm(ACIout ~ fish + Site + basin + BKdB_low, data = morning) 

# TODO: run linear model by site; include interaction site*basin (or site)

m3 <- lm(ACIout ~ fish + day + Site*fish, data = center)

summary(m1)

dredge(m3)
