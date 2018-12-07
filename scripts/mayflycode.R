library(lme4)
library(ggplot2)
library(lubridate)
library(wesanderson)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Data Organization ------------------------------------------------

mayfly<-read.csv("../data/insects/mayfly.csv")
# change categorical variables to factors
mayfly$round <- factor(mayfly$round)

# clean the dates
# just be cautious: this assumes(!) that minutes are always
# written as 07 and not 7 (with leading zeros)
mayfly$time_in <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", mayfly$time_in)
mayfly$time_out <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", mayfly$time_out)

mayfly$date_in <- parse_date_time(paste(mayfly$date_in, mayfly$time_in), "mdy HM")
mayfly$date_out <- parse_date_time(paste(mayfly$date_out, mayfly$time_out), "mdy HM")

# check to see that lubridate formatted the dates correctly
print(mayfly$date_out)

#add treatment variable:
mayfly$fish <- mayfly$lake

# original
# mayfly$fish <- gsub('[^12]', '', mayfly$lake)
# Vince's — here I use named labels, as this is safer usually 
# (it's easy for 1 and 2 to accidentally become integers and funny stuff to happen)
mayfly$fish <- factor(ifelse(gsub('[^12]', '', mayfly$lake) == "2", "fish-containing", "fishless"))
# remove the 1/2 on the lake 
mayfly$basin <- factor(gsub("[12]", "\\1", mayfly$lake))

# combined lake/fish col
mayfly$lake_fish <- paste(mayfly$basin, mayfly$fish, sep=" ")
head(mayfly$lake_fish)

#change categorical variables into factors
mayfly$fish <- factor(mayfly$fish)
mayfly$lake <- factor(mayfly$lake)
mayfly$lake_fish <- factor(mayfly$lake_fish)


# Plots -------------------------------------------------------------------


# Now we want to see if we could recreate Mary's plot in her notebook
# the date_in+hours(12) below is to create gaps in the bar graphs
# ...they are not representative of the time between sticky trap switches

p <- ggplot(mayfly) + geom_linerange(aes(x=lake_fish, 
                                         ymin=(date_in+hours(5)), 
                                         ymax=(date_out-hours(5)), 
                                         color=round), size=3)
p + coord_flip() +
  theme(plot.title = element_text(face="bold", size=16), 
        axis.title = element_blank(), 
        axis.text.x = element_text(size=14, angle=0), 
        axis.text.y = element_text(size=14, angle=0))

###
### THE GOODS ###
###

library(dplyr)

m <- mayfly %>%
  group_by(fish, lake) %>% summarise(total=sum(mayfly), avg=(mean(mayfly)))

#Boxplot of total mayflies by lake type!!! YAY!
 pretty <- ggplot(m) + 
  geom_boxplot(width = 0.5, aes(x=fish, y=total, fill=fish)) +
  scale_fill_manual(values = c(cbPalette[2], cbPalette[6])) +
  labs(title = NULL, 
       x = NULL,
       y = NULL) + 
  theme_minimal() +
  theme(legend.position="none", 
        plot.title = element_text(family = "Helvetica", face="bold", size=0),
        axis.title = element_text(family = "Helvetica",  size=25), 
        axis.text.x = element_text(family = "Helvetica", face="bold", size=25, angle=0), 
        axis.text.y = element_text(family = "Helvetica", face="bold", size=25, angle=0))

t.test(total ~ fish, data = m)
t.test(mayfly ~ fish, data = mayfly)

###
###
###

## Add in mayfly images


tmp <- data.frame(date_in=mdy_hm("7/4/2015 12:00"), lake_fish="CENTER fishless")
p + geom_point(aes(y=date_in, x=lake_fish), data=tmp)

#mayflies over season plot, by lake
p <-ggplot(mayfly) + geom_linerange(aes(x=lake_fish, ymin=(date_in+hours(12)), 
                                         ymax=(date_out-hours(12)), color=fish, 
                                         size=mayfly))
p + coord_flip() + 
  theme_bw() +
  scale_color_manual(values = c(cbPalette[2], cbPalette[6]),
                     name = "lake type") +
  labs(x = NULL, y = "Site") + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = rel(2), face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))

# I want to add #s of mayflies for each round to each lake's bars
# and insert a mayfly gif on the bars corresponding with days I observed them
# 

####

### EXPLORATORY DATA ANALYSIS ###

# boxplot of mayfly counts by round of collection
ggplot(mayfly) + geom_boxplot(aes(x=fish, y=mayfly)) + facet_wrap(~round)

# density is for continuous data, not count data... but store away for other projects
# ggplot(mayfly) + geom_density(aes(x=mayfly)) + facet_wrap(~round)

#histogram of mayfly counts by round of collection... obvi very 0-inflated
ggplot(mayfly) + geom_histogram(aes(x=mayfly)) + facet_wrap(~round)
qqnorm(mayfly$mayfly)

#histogram of mayfly counts by basin and fish treatment, colorblocked by round 
ggplot(mayfly) + geom_histogram(aes(x=mayfly, fill=round)) + facet_grid(basin~fish) 

#same as above but lumping by treatment
ggplot(mayfly) + geom_histogram(aes(x=mayfly, fill=round)) + facet_wrap(~fish)




### TRYING TO BUILD A MODEL ###

m1 <-glm(mayfly ~ fish + round + basin, family = poisson, data = mayfly)
summary(m1)
drop1(m1, test="Chi")

plot(m1)

m2 <- glm.nb(mayfly ~ fish + round + basin, link=log, data=mayfly)
summary(m2)
plot(m2)

anova(m1, m2)
