# Code to display acoustic sampling effort 
# Created by M. Clapp May 2016
# Last edit:
# Fri Feb  9 14:19:39 2018 ------------------------------

# TODO --------------------------------------------------
# Update filepaths to play with repo!
# Can I make the dotted lines finer so that you can tell when the sampling breaks are?
# Can I make the legend for "scheme" more readable?
# Can I remove sites that have no values (e.g., remove Wright and UpKern from 2014?)
# Can I label the y-axis with only basin, not fish condition?
# Can I turn the mm/dd date on the x-axis into a word (e.g., May 13)?

# LOAD libraries and data-------------------------------------------
library(ggplot2)
library(lubridate)
library(dplyr)

a <- read.csv("DATA/ACOUSTIC/acoustic_survey_effort.csv")


# DATA tidying ------------------------------------------------------------
a$date_in <- mdy(a$date_in)
a$date_out <- mdy(a$date_out)

a$basin.fish <- paste(a$basin,a$fish)

a$md_in <- format(a$date_in, "%m/%d")
a$md_out <- format(a$date_out, "%m/%d")

# julian day column (for plotting later)
a$jday_in <- yday(a$date_in)
a$jday_out <- yday(a$date_out)

# filter by year

a2014 <- a %>% filter(year == 2014)
a2015 <- a %>% filter(year == 2015)


# PLOTS summarizing acoustic data collection ------------------------------

p <- ggplot(a) + geom_linerange(aes(x=basin.fish, ymin=(jday_in), ymax=(jday_out), color=fish, linetype=scheme), size=1.8) + 
      coord_flip() + facet_wrap(~year, nrow=2, ncol=1) +
      theme(axis.text.x = element_text(size=12, angle=45), 
            axis.text.y = element_text(size=14), 
            axis.title.y = element_blank())
p + scale_y_continuous(breaks=seq(100, 300, 10))
                         
#without site names:
p <- ggplot(a) + geom_linerange(aes(x=basin.fish, ymin=(jday_in), ymax=(jday_out), color=fish, linetype=scheme), size=2) + 
  coord_flip() + facet_wrap(~year, nrow=2, ncol=1) +
  theme(axis.text.x = element_text(size=12, angle=45), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank())
p + scale_y_continuous(breaks=seq(100, 300, 10)) + 
  theme(legend.position="none")

#by year (not edited to include Julian day)

p2014 <- ggplot(a2014) + geom_linerange(mapping=aes(x=basin.fish, ymin=(date_in), ymax=(date_out), color=fish), linetype=2, size=1) + 
  coord_flip() + facet_wrap(~year, nrow=2, ncol=1) +
  theme(axis.text.x = element_text(size=12, angle=45), 
        axis.text.y = element_text(size=12, angle=0), 
        axis.ticks = element_blank()) +
  scale_y_date(date_breaks ="2 weeks", date_labels = "%b %d")
p2014

p2015 <- ggplot(a2015) + geom_linerange(aes(x=basin.fish, ymin=(date_in), ymax=(date_out), color=fish, linetype=scheme), size=1) + 
  coord_flip() + facet_wrap(~year, nrow=2, ncol=1) +
  ylab("Date") + xlab("Site") + 
  theme(axis.text.x = element_text(size=12, angle=45), 
        axis.text.y = element_text(size=12, angle=0), 
        axis.ticks = element_blank()) +
  scale_y_date(date_breaks ="2 weeks", date_labels ="%b %d")
p2015
