# Script to investigate avian point count data
# TODO----------------------------------------
# Update file paths for new repo! 

library(lme4)
library(ggplot2)
library(lubridate)
library(wesanderson)
library(dplyr)

# load data and clean
d <- read.csv(file = "../data/PointCountData_Master.csv")
colnames(d) <- tolower(colnames(d))
d$date <- mdy(d$date)
d$fish <- factor(ifelse(d$fish == 1, "fishless", "fish"))

# rename basin names to look pretty later
d$basin <- tolower(d$basin)
d$basin <- sub("eastla", "East Lake", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("center", "Center", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("amphit", "Amphitheater", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("wright", "Wright Lakes", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("upkern", "Upper Kern", d$basin, ignore.case =FALSE, fixed=FALSE)
d$basin <- sub("barret", "Barrett Lakes", d$basin, ignore.case =FALSE, fixed=FALSE)

d$basin <-as.factor(d$basin)

# Change all entries with > to 500m and FO to NA (vince's code)
d$distance <- as.character(d$distance)
d$distance[regexpr(">", d$distance) != -1] <- "500"
d$distance[regexpr("F0", d$distance) != -1] <- "NA"
d$distance <- as.integer(d$distance)

# exploratory plots
# 
e <- d %>% group_by(basin, fish, date, point, time) %>% filter(distance < 150 & doublecount == "") %>% summarize(nspecies=n_distinct(species), total=n())
e

# check relationship with time of day
plot(total~time, data=e)


lm <- lm(total~time, data=e)
summary(lm)

abline(lm)
plot(lm)

# relationship with date of year
# first make julian day column
e$jday <- yday(e$date)

plot(total~jday, data=e)
lm2 <- lm(total~jday, data=e)
plot(lm2)

summary(lm2)
plot(nspecies~time, data=e)

plot(total~fish, data=e)

## EDA just 2015 data
d2015 <- d[year(d$date) == 2015, ]

# grouping all-years data by year, date, and summarizing # of species and total counts
abd <- d %>% group_by(basin, fish, year=year(date), month=month(date)) %>% summarize(nspecies=n_distinct(species), total=n())

# 
abdd <- d2015 %>% filter(date > "2015-06-07" & date < "2015-07-06" & distance < 151 & doublecount == "") %>% group_by(basin, fish) %>% summarize(nspecies=n_distinct(species), total=n())

plot(total~fish, data=abdd)
plot(nspecies~fish, data=abdd)

# summarize by species, too
spec2015 <- d2015 %>% filter(date > "2015-06-07" & date < "2015-07-06" & distance < 151 & doublecount == "") %>% group_by(basin, fish, species) %>% summarize(nspecies=n_distinct(species), total=n())

# basic bar graph of bird counts by species by basin ## THIS IS WRONG-- DOESN'T GIVE TRUE COUNT VALUES ##
b <- ggplot(spec2015, aes(x=species, y=total, fill=fish)) + 
  geom_bar(stat="identity", position=position_dodge(), width=0.8) +
  labs(title = "number of birds per species at fish-containing and fishless lakes",
       x = "species (AOU code)", 
       y = "number of birds counted") +
  theme(legend.title = element_blank(),
        plot.title = element_text(face="bold", size=16),
        axis.title = element_text(face="bold", size=16),
        axis.text.x = element_text(size=14, angle=0),
        axis.text.y = element_text(size=14, angle=0))
b
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

# this is the baseR code to accomplish the above
# unsurprisingly, t-tests say there's no significant difference
plot(nspecies ~ fish, data=abdd)
t.test(nspecies ~ fish, data = abdd)

plot(total~fish, data=abdd)
t.test(total~fish, data=abdd)
