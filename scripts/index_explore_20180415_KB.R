# Author: Mary Clapp & Kasey Brockelsby
# Script to explore and display Acoustic Index Data 
# Edited 4/15/2018 from previous version to run on acoustic indices generated 2/5/2018

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
file_names <- list.files(path = "data/indices/2018_02_05/", full.names = TRUE, pattern = "*.csv")

d <- do.call("rbind", (lapply(file_names,read.csv)))
d <- subset(d, select = c(Site, Date, Yr, Mo, Day, Hr, Min, Sec, ACIout, BKdB_low, BKdB_bird, avgAMP,
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


# SUBSET DATA -------------------------------------------------------------

# Before we get down into all the time slice stuff, subsetting monthly means at each site.
monthly_means_2015 <- d %>%
  filter(Yr == 2015) %>%
  group_by(Site,Mo,fish) %>%
  summarize(mean_monthly_ACI = mean(ACIout),
            sd_monthly_ACI = sd(ACIout))

ggplot(monthly_means_2015, aes(x=Mo, y=mean_monthly_ACI, colour = Site)) +
  geom_point(alpha = 0.8) +
  labs(title = "Monthly variation in ACI")

A6 <- d %>%
  filter(Yr == 2015, Mo == 6, basin == "AMPHIT")

A6_clean <- A6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[1] + (monthly_means_2015$sd_monthly_ACI[1]*2)))

A6_outliers <- A6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[1] + (monthly_means_2015$sd_monthly_ACI[1]*2)))

A6F_clean <- A6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[4] + (monthly_means_2015$sd_monthly_ACI[4]*2)))

A6F_outliers <- A6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[4] + (monthly_means_2015$sd_monthly_ACI[4]*2)))

A7 <- d %>%
  filter(Yr == 2015, Mo == 7, basin == "AMPHIT")

A7_clean <- A7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[2] + (monthly_means_2015$sd_monthly_ACI[2]*2)))

A7_outliers <- A7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[2] + (monthly_means_2015$sd_monthly_ACI[2]*2)))

A7F_clean <- A7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[5] + (monthly_means_2015$sd_monthly_ACI[5]*2)))

A7F_outliers <- A7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[5] + (monthly_means_2015$sd_monthly_ACI[5]*2)))

A8 <- d %>%
  filter(Yr == 2015, Mo == 8, basin == "AMPHIT")

A8_clean <- A8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[3] + (monthly_means_2015$sd_monthly_ACI[3]*2)))

A8_outliers <- A8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[3] + (monthly_means_2015$sd_monthly_ACI[3]*2)))

A8F_clean <- A8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[6] + (monthly_means_2015$sd_monthly_ACI[6]*2)))

A8F_outliers <- A8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[6] + (monthly_means_2015$sd_monthly_ACI[6]*2)))

A9 <- d %>%
  filter(Yr == 2015, Mo == 9, basin == "AMPHIT")

A9F_clean <- A9 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[7] + (monthly_means_2015$sd_monthly_ACI[7]*2)))

A9F_outliers <- A9 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[7] + (monthly_means_2015$sd_monthly_ACI[7]*2)))

B6 <- d %>%
  filter(Yr == 2015, Mo == 6, basin == "BARRET")

B6_clean <- B6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[8] + (monthly_means_2015$sd_monthly_ACI[8]*2)))

B6_outliers <- B6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[8] + (monthly_means_2015$sd_monthly_ACI[8]*2)))

B6X_clean <- B6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[13] + (monthly_means_2015$sd_monthly_ACI[13]*2)))

B6X_outliers <- B6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[13] + (monthly_means_2015$sd_monthly_ACI[13]*2)))

B7 <- d %>%
  filter(Yr == 2015, Mo == 7, basin == "BARRET")

B7_clean <- B7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[9] + (monthly_means_2015$sd_monthly_ACI[9]*2)))

B7_outliers <- B7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[9] + (monthly_means_2015$sd_monthly_ACI[9]*2)))

B7F_clean <- B7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[12] + (monthly_means_2015$sd_monthly_ACI[12]*2)))

B7F_outliers <- B7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[12] + (monthly_means_2015$sd_monthly_ACI[12]*2)))

B8 <- d %>%
  filter(Yr == 2015, Mo == 8, basin == "BARRET")

B8_clean <- B8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[10] + (monthly_means_2015$sd_monthly_ACI[10]*2)))

B8_outliers <- B8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[10] + (monthly_means_2015$sd_monthly_ACI[10]*2)))

B9 <- d %>%
  filter(Yr == 2015, Mo == 9, basin == "BARRET")

B9_clean <- B9 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[11] + (monthly_means_2015$sd_monthly_ACI[11]*2)))

B9_outliers <- B9 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[11] + (monthly_means_2015$sd_monthly_ACI[11]*2)))

C5 <- d %>%
  filter(Yr == 2015, Mo == 5, basin == "CENTER")

C5_clean <- C5 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[14] + (monthly_means_2015$sd_monthly_ACI[14]*2)))

C5_outliers <- C5 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[14] + (monthly_means_2015$sd_monthly_ACI[14]*2)))

C5F_clean <- C5 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[19] + (monthly_means_2015$sd_monthly_ACI[19]*2)))

C5F_outliers <- C5 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[19] + (monthly_means_2015$sd_monthly_ACI[19]*2)))

C6 <- d %>%
  filter(Yr == 2015, Mo == 6, basin == "CENTER")

C6_clean <- C6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[15] + (monthly_means_2015$sd_monthly_ACI[15]*2)))

C6_outliers <- C6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[15] + (monthly_means_2015$sd_monthly_ACI[15]*2)))

C6F_clean <- C6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[20] + (monthly_means_2015$sd_monthly_ACI[20]*2)))

C6F_outliers <- C6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[20] + (monthly_means_2015$sd_monthly_ACI[20]*2)))


C7 <- d %>%
  filter(Yr == 2015, Mo == 7, basin == "CENTER")

C7_clean <- C7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[16] + (monthly_means_2015$sd_monthly_ACI[16]*2)))

C7_outliers <- C7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[16] + (monthly_means_2015$sd_monthly_ACI[16]*2)))

C7F_clean <- C7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[21] + (monthly_means_2015$sd_monthly_ACI[21]*2)))

C7F_outliers <- C7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[21] + (monthly_means_2015$sd_monthly_ACI[21]*2)))

C8 <- d %>%
  filter(Yr == 2015, Mo == 8, basin == "CENTER")

C8_clean <- C8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[17] + (monthly_means_2015$sd_monthly_ACI[17]*2)))

C8_outliers <- C8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[17] + (monthly_means_2015$sd_monthly_ACI[17]*2)))

C8F_clean <- C8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[22] + (monthly_means_2015$sd_monthly_ACI[22]*2)))

C8F_outliers <- C8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[22] + (monthly_means_2015$sd_monthly_ACI[22]*2)))

C9 <- d %>%
  filter(Yr == 2015, Mo == 9, basin == "CENTER")

C9_clean <- C9 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[18] + (monthly_means_2015$sd_monthly_ACI[18]*2)))

C9_outliers <- C9 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[18] + (monthly_means_2015$sd_monthly_ACI[18]*2)))

C9F_clean <- C9 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[23] + (monthly_means_2015$sd_monthly_ACI[23]*2)))

C9F_outliers <- C9 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[23] + (monthly_means_2015$sd_monthly_ACI[23]*2)))

U6 <- d %>%
  filter(Yr == 2015, Mo == 6, basin == "UPKERN")

U6_clean <- U6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[24] + (monthly_means_2015$sd_monthly_ACI[24]*2)))

U6_outliers <- U6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[24] + (monthly_means_2015$sd_monthly_ACI[24]*2)))

U6F_clean <- U6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[28] + (monthly_means_2015$sd_monthly_ACI[28]*2)))

U6F_outliers <- U6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[28] + (monthly_means_2015$sd_monthly_ACI[28]*2)))

U7 <- d %>%
  filter(Yr == 2015, Mo == 7, basin == "UPKERN")

U7_clean <- U7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[25] + (monthly_means_2015$sd_monthly_ACI[25]*2)))

U7_outliers <- U7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[25] + (monthly_means_2015$sd_monthly_ACI[25]*2)))

U7F_clean <- U7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[29] + (monthly_means_2015$sd_monthly_ACI[29]*2)))

U7F_outliers <- U7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[29] + (monthly_means_2015$sd_monthly_ACI[29]*2)))

U8 <- d %>%
  filter(Yr == 2015, Mo == 8, basin == "UPKERN")

U8_clean <- U8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[26] + (monthly_means_2015$sd_monthly_ACI[26]*2)))

U8_outliers <- U8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[26] + (monthly_means_2015$sd_monthly_ACI[26]*2)))

U8F_clean <- U8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[30] + (monthly_means_2015$sd_monthly_ACI[30]*2)))

U8F_outliers <- U8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[30] + (monthly_means_2015$sd_monthly_ACI[30]*2)))

U9 <- d %>%
  filter(Yr == 2015, Mo == 9, basin == "UPKERN")

U9_clean <- U9 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[27] + (monthly_means_2015$sd_monthly_ACI[27]*2)))

U9_outliers <- U9 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[27] + (monthly_means_2015$sd_monthly_ACI[27]*2)))

U9F_clean <- U9 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[31] + (monthly_means_2015$sd_monthly_ACI[31]*2)))

U9F_outliers <- U9 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[31] + (monthly_means_2015$sd_monthly_ACI[31]*2)))

W6 <- d %>%
  filter(Yr == 2015, Mo == 6, basin == "WRIGHT")

W6_clean <- W6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[32] + (monthly_means_2015$sd_monthly_ACI[32]*2)))

W6_outliers <- W6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[32] + (monthly_means_2015$sd_monthly_ACI[32]*2)))

W6F_clean <- W6 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[35] + (monthly_means_2015$sd_monthly_ACI[35]*2)))

W6F_outliers <- W6 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[35] + (monthly_means_2015$sd_monthly_ACI[35]*2)))

W7 <- d %>%
  filter(Yr == 2015, Mo == 7, basin == "WRIGHT")

W7_clean <- W7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[33] + (monthly_means_2015$sd_monthly_ACI[33]*2)))

W7_outliers <- W7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[33] + (monthly_means_2015$sd_monthly_ACI[33]*2)))

W7F_clean <- W7 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[36] + (monthly_means_2015$sd_monthly_ACI[36]*2)))

W7F_outliers <- W7 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[36] + (monthly_means_2015$sd_monthly_ACI[36]*2)))

W8 <- d %>%
  filter(Yr == 2015, Mo == 8, basin == "WRIGHT")

W8_clean <- W8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[34] + (monthly_means_2015$sd_monthly_ACI[34]*2)))

W8_outliers <- W8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[34] + (monthly_means_2015$sd_monthly_ACI[34]*2)))

W8F_clean <- W8 %>%
  filter(ACIout < (monthly_means_2015$mean_monthly_ACI[37] + (monthly_means_2015$sd_monthly_ACI[37]*2)))

W8F_outliers <- W8 %>%
  filter(ACIout > (monthly_means_2015$mean_monthly_ACI[37] + (monthly_means_2015$sd_monthly_ACI[37]*2)))

# Concatenating these disparate things together.

outliers_2015 <- rbind(A6_outliers,A6F_outliers,A7_outliers,A7F_outliers,A8_outliers,A8F_outliers,A9F_outliers,B6_outliers,B7_outliers,B7F_outliers,B8_outliers,B9_outliers,C5_outliers,C5F_outliers,C6_outliers,C6F_outliers,C7_outliers,C7F_outliers,C8_outliers,C8F_outliers,C9_outliers,C9F_outliers,U6_outliers,U6F_outliers,U7_outliers,U7F_outliers,U8_outliers,U8F_outliers,U9_outliers,U9F_outliers,W6_outliers,W6F_outliers,W7_outliers,W7F_outliers,W8_outliers,W8F_outliers)
clean_2015 <- rbind(A6_clean,A6F_clean,A7_clean,A7F_clean,A8_clean,A8F_clean,A9F_clean,B6_clean,B7_clean,B7F_clean,B8_clean,B9_clean,C5_clean,C5F_clean,C6_clean,C6F_clean,C7_clean,C7F_clean,C8_clean,C8F_clean,C9_clean,U6_clean,U6F_clean,U7_clean,U7F_clean,U8_clean,U8F_clean,U9_clean,U9F_clean,W6_clean,W6F_clean,W7_clean,W7F_clean,W8_clean,W8F_clean)

# END of monthly/basin subsets.

# Monthly means are above. Hourly means are below.

hourly_means_2015 <- d %>%
  filter(Yr == 2015) %>%
  group_by(Site,Hr,fish) %>%
  summarize(mean_hourly_ACI = mean(ACIout),
            sd_hourly_ACI = sd(ACIout))

hourly_subset_2015 <- d %>%
  filter(Yr == 2015) %>%
  group_by(Site,Hr,fish)

d_h_outliers_2015 <- hourly_subset_2015 %>%
  filter(ACIout > (hourly_means_2015$mean_hourly_ACI + hourly_means_2015$sd_hourly_ACI*2))

d_h_extreme_2015 <- hourly_subset_2015 %>%
  filter(ACIout > (hourly_means_2015$mean_hourly_ACI + hourly_means_2015$sd_hourly_ACI*3))

d_h_clean_2015 <- hourly_subset_2015 %>%
  filter(ACIout < (hourly_means_2015$mean_hourly_ACI + hourly_means_2015$sd_hourly_ACI*2))

ggplot(d_h_clean_2015, aes(x=Hr, y=ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se=FALSE) +
  labs(title = "2015 Hourly ACI without outliers")

ggplot(d_h_outliers_2015, aes(x=Hr, y=ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  labs(title = "2015 Hourly ACI outliers")

ggplot(d_h_extreme_2015, aes(x=Hr, y=ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  labs(title = "2015 Hourly ACI extreme outliers")


# # # A really messy, roundabout way to do something that is probably much simpler than I think.
# I'm commenting it out for now, because it is not at all clean and may have other problems.
# d_clean <- d
# monthly_means_2015$max_range <- (monthly_means_2015$mean_monthly_ACI + (2 * monthly_means_2015$sd_monthly_ACI))
# for (i in (1:37)) {
#   lake <- monthly_means_2015$Site[i]
#   mon <- monthly_means_2015$Mo[i]
#   for (j in (1:118637)) {
#     if (d_clean$Mo[j] == mon & d_clean$Site[j] == lake & (d_clean$ACIout[j] < monthly_means_2015$max_range[i])) {
#       d_clean$ACIout[j] <- "OUTLIER"
#     }
#   }
# }
# d_new <- d_clean %>%
#   filter(ACIout != "OUTLIER")
# d_new$ACIout <- as.numeric(d_new$ACIout)
# # This chunk creates the proper numeric ACI values, without outliers. But it graphs  weird. Of course.
# 
# # This, then, picks out ONLY the outliers. They should be complements.
# 
# monthly_means_2015$max_range <- (monthly_means_2015$mean_monthly_ACI + (2 * monthly_means_2015$sd_monthly_ACI))
# for (i in (1:37)) {
#   lake <- monthly_means_2015$Site[i]
#   mon <- monthly_means_2015$Mo[i]
#   for (j in (1:118637)) {
#     if (d_clean$Mo[j] == mon & d_clean$Site[j] == lake & (d_clean$ACIout[j] > monthly_means_2015$max_range[i])) {
#       d_clean$ACIout[j] <- "MAIN"
#     }
#   }
# }
# d_outliers <- d_clean %>%
#   filter(ACIout != "MAIN")
# d_outliers$ACIout <- as.numeric(d_outliers$ACIout)
# 
# ggplot(d_outliers, aes(x=Mo, y=(ACIout), colour = fish)) +
#   geom_point(alpha=0.5) +
#   labs(title = "Monthly outliers")
# 
# ggplot(d_new, aes(x=Mo, y=(ACIout), colour=fish)) +
#   geom_point(alpha=0.2) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(title = "Monthly Means without outliers")
# 
# ggplot(d_new, aes(x=Hr, y=ACIout, colour=fish)) +
#   geom_point(alpha=0.2) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(title = "Hourly Means without outliers")
# 
# ggplot(monthly_means_2015, aes(x=Mo, y=mean_monthly_ACI, colour=fish)) +
#   geom_point(alpha=0.2) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(title = "Monthly Means 2015")
# 
# hourly_means_2015 <- d %>%
#   filter(Yr == 2015) %>%
#   group_by(Site,Hr,fish) %>%
#   summarize(mean_hourly_ACI = mean(ACIout),
#             sd_hourly_ACI = sd(ACIout))
# 
# ggplot(hourly_means_2015, aes(x=Hr, y=mean_hourly_ACI, colour=fish)) +
#   geom_point(alpha=0.2) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(title = "Hourly Means 2015")
# 
# hourly_main <- d %>%
#   filter(ACIout < mean_hourly_ACI + (2*sd_hourly_ACI))

# # # What an awful thing to do. Maybe move this. Mark it all out. Who knows?

# # # Another failed attempt, for posterity.

# d_outliers_2015 <- monthly_subset_2015 %>%
#   filter(ACIout > (monthly_means_2015$mean_monthly_ACI + monthly_means_2015$sd_monthly_ACI*2))
# 
# d_extreme_2015 <- monthly_subset_2015 %>%
#   filter(ACIout > (monthly_means_2015$mean_monthly_ACI + monthly_means_2015$sd_monthly_ACI*3))
# 
# d_clean_2015 <- monthly_subset_2015 %>%
#   filter(ACIout < (monthly_means_2015$mean_monthly_ACI + monthly_means_2015$sd_monthly_ACI*2))
# 
# ggplot(d_clean_2015, aes(x=Mo, y=ACIout, colour = fish)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth(method = "loess", se=FALSE) +
#   labs(title = "2015 ACI without outliers")
# 
# ggplot(d_outliers_2015, aes(x=Mo, y=ACIout, colour = fish)) +
#   geom_point(alpha = 0.2) +
#   labs(title = "2015 ACI outliers")
# 
# ggplot(d_extreme_2015, aes(x=Mo, y=ACIout, colour = fish)) +
#   geom_point(alpha = 0.2) +
#   labs(title = "2015 ACI extreme outliers") +
#   facet_wrap(~basin)

# # # And now, into the other morning stuff.

# all season, between hours of 5:10 and 9:10
morning <- subset(d, format(Timestamp, '%H:%M') >= "05:10" & 
                    format(Timestamp, '%H:%M') <= "09:10")

# ...and between 16:00 and 20:00
evening <- subset(d, format(Timestamp, "%H:%M") >= "16:00" &
                    format(Timestamp, "%H:%M") <= "20:00")

# Just sampling particular days at random to get a feel for the patterns.
day_slice <- d %>%
  filter(wind == FALSE, Mo == 6, Day == 3, Yr == 2015)

ggplot(day_slice, aes(x=Timestamp, y=ACIout, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "ACI Over Day")
# And in doing so, found something worrying. 5-9 may not be best sampling
# of data - many days have peak ACI around midday. incorporate full
# dataset? May have its own problems.

# Removing data from days where wind would disrupt it.
morning_windless <- morning %>%
  filter(wind == FALSE)

# Plotting ACI, Roughness, ADI, and Evenness across each year.
# Because Roughness had EXTREME outliers, filtered for only core set.
# Considering filtering outliers for other data?
# TRY DAILY MEANS - may be more indicative than hourly sampling.

morning_windless_2015 <- morning_windless %>%
  filter(Yr == 2015)

ggplot(morning_windless_2015, aes(x=day, y=ACIout, colour=fish)) +
geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "ACI Over Time 2015")

Rough_main_2015 <- morning_windless_2015 %>%
  filter(Rough < (mean(Rough) + 3*sd(Rough)))

ggplot(Rough_main_2015, aes(x=day, y=Rough, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Roughness Without Outliers 2015")

ggplot(morning_windless_2015, aes(x=day, y=ADI_step, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Acoustic Diversity 2015")

ggplot(morning_windless_2015, aes(x=day, y=Eveness_step, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Acoustic Evenness 2015")

morning_windless_2016 <- morning_windless %>%
  filter(Yr == 2016)

ggplot(morning_windless_2016, aes(x=day, y=ACIout, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "ACI Over Time 2016")

Rough_main_2016 <- morning_windless_2016 %>%
  filter(Rough < (mean(Rough) + 3*sd(Rough)))

ggplot(Rough_main_2016, aes(x=day, y=Rough, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Roughness Without Outliers 2016")

ggplot(morning_windless_2016, aes(x=day, y=ADI_step, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Acoustic Diversity 2016")

ggplot(morning_windless_2016, aes(x=day, y=Eveness_step, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Acoustic Evenness 2016")

morning_windless_2014 <- morning_windless %>%
  filter(Yr == 2014)

ggplot(morning_windless_2014, aes(x=day, y=ACIout, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "ACI Over Time 2014")

Rough_main_2014 <- morning_windless_2014 %>%
  filter(Rough < (mean(Rough) + 3*sd(Rough)))

ggplot(Rough_main_2014, aes(x=day, y=Rough, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Roughness Without Outliers 2014")

ggplot(morning_windless_2014, aes(x=day, y=ADI_step, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Acoustic Diversity 2014")

ggplot(morning_windless_2014, aes(x=day, y=Eveness_step, colour=fish)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Acoustic Evenness 2014")
  
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

# subset data by BASIN
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

# Experimenting with monthly data/dawn chorus, keeping it near top.

# This is where things get sticky. Overall, ACI is highest around afternoon; check on this?

# Try other groupings. In fact, be leery of group_by in general, as it generates data oddly.

hourly_ACI_exp <- d %>%
  filter(wind == FALSE) %>%
  group_by(Hr, Site, basin, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout),
            median_ACI = median(ACIout),
            sd_ACI = sd(ACIout))

hourly_ACI_Center <- hourly_ACI_exp %>%
  filter(basin == "CENTER")

hourly_ACI_East <- hourly_ACI_exp %>%
  filter(basin == "EASTLA")

ggplot(hourly_ACI_Center, aes(x = Hr, y = mean_ACI, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Center Hourly ACI")

# Straightforward - Examining correlation between background noise and ACI.

ggplot(d, aes(x = BKdB_low, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Background Noise v. ACI")

# Subset based on month; dawn chorus may only be during June.
# RESULT - Dawn chorus is only seen during May, our least sampled month.

# Check other indices, particularly Roughness and AR.
# RESULT - Roughness is near identical to ACI; Acoustic Richness is wildly different.

# Compare to Background pressure levels - are they highest in afternoon as well?
# RESULT - Low Background pressure follows the same observed pattern as ACI.

# Color by Basin/Year/etc.; look for trends there.
# RESULT - Coloring by basin is interesting, but yields largely the same patterns.
# Coloring by year?

# Basic hourly means, as defined above.

ggplot(hourly_ACI_exp, aes(x = Hr, y = mean_ACI, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Overall Hourly ACI")

# Filtered by month, to look for correlations there.
# Additionally, there appears to be little to say fish-wise (EXCEPT MAY), so colored by basin.

hACI_May <- hourly_ACI_exp %>%
  filter(Mo == 5)

hACI_June <- hourly_ACI_exp %>%
  filter(Mo == 6)

hACI_July <- hourly_ACI_exp %>%
  filter(Mo == 7)

hACI_August <- hourly_ACI_exp %>%
  filter(Mo == 8)

ggplot(hACI_May, aes(x = Hr, y = mean_ACI, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "May Hourly ACI")

ggplot(hACI_June, aes(x = Hr, y = mean_ACI, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "June Hourly ACI")

ggplot(hACI_July, aes(x = Hr, y = mean_ACI, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "July Hourly ACI")

ggplot(hACI_August, aes(x = Hr, y = mean_ACI, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "August Hourly ACI")

ggplot(hourly_ACI_exp, aes(x = Hr, y = mean_ACI, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Hourly ACI by Month")

ggplot(hourly_ACI_exp, aes(x = Hr, y = mean_ACI, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Yr) +
  labs(title = "Hourly ACI by Year")


# Conclusions? Pattern exists across June, July, and August, but is less prevalent in May.
# May is also our least sampled month. Future analyses may want to follow the other general
# trend, simply because it better represents the data.
# Additionally, sometimes fish data is useful. Sort by fish and by basin. Or even by Site?

# Checking out other indices; do they track with ACI? WHich is the best measure?
hourly_AR <- d %>%
  filter(wind == FALSE) %>%
  group_by(Mo, Hr, Site, basin, fish) %>%
  summarize(N = length(AR),
            mean_AR = mean(AR))

ggplot(hourly_AR, aes(x = Hr, y = mean_AR, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Overall Hourly AR")

ggplot(hourly_AR, aes(x = Hr, y = mean_AR, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Hourly AR by Month")


hourly_Rough <- d %>%
  filter(wind == FALSE) %>%
  group_by(Mo, Hr, Site, basin, fish) %>%
  summarize(N = length(Rough),
            mean_Rough = mean(Rough))

ggplot(hourly_Rough, aes(x = Hr, y = mean_Rough, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Overall Hourly Roughness")

ggplot(hourly_Rough, aes(x = Hr, y = mean_Rough, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Hourly Roughness by Month")

# Conclusions? Roughness is nearly identical to ACI in these comparisons.
# By constrast, Acoustic richness shows some WILDLY different results.
# I'm not sure precisely how to interpret them, bthey're kind of all over the map.

# Checking out background pressures; do they follow the overall trend?

hourly_BKdB_low <- d %>%
  filter(wind == FALSE) %>%
  group_by(Mo, Hr, Site, basin, fish) %>%
  summarize(N = length(BKdB_low),
            mean_BKdB_low = mean(BKdB_low))

ggplot(hourly_BKdB_low, aes(x = Hr, y = mean_BKdB_low, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Overall Background Levels (Low)")

ggplot(hourly_BKdB_low, aes(x = Hr, y = mean_BKdB_low, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Background Levels (Low) by Month")

ggplot(hourly_BKdB_low, aes(x = Hr, y = mean_BKdB_low, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Background Levels (Low) by Month")

hourly_BKdB_bird <- d %>%
  filter(wind == FALSE) %>%
  group_by(Mo, Hr, Site, basin, fish) %>%
  summarize(N = length(BKdB_bird),
            mean_BKdB_bird = mean(BKdB_bird))

ggplot(hourly_BKdB_bird, aes(x = Hr, y = mean_BKdB_bird, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Overall Background Levels (Bird)")

ggplot(hourly_BKdB_bird, aes(x = Hr, y = mean_BKdB_bird, colour = basin)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Background Levels (Bird) by Month")

ggplot(hourly_BKdB_bird, aes(x = Hr, y = mean_BKdB_bird, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~Mo) +
  labs(title = "Background Levels (Bird) by Month")

# Conclusions? Low background pressure does follow the same pattern as seen with mean ACI.
# Bird background pressure reamins more or less constant throughout the day.
# And again, ~fish and ~basin yield similar results.



# All the morning only analyses.

hourly_ACI_means_2015 <- morning %>%
  filter(Yr == 2015) %>%
  group_by(Hr, Site, basin, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

# ACI over time; highest activity in different month.
ggplot(center2014, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(c15, aes(x = day, y = mean_ACI, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(center2016, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(amphit2014, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(amphit2015, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(barret2015, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(eastla2014, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(upkern2015, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")

ggplot(wright2015, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess")


# SUMMARY TABLES ----------------------------------------------------------

# daily (morning and all-day) ACI average stats tables
daily_ACI_means_2015 <- morning %>%
  filter(Yr == 2015) %>%
  group_by(day, Site, basin, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

daily_Rough_means_2015 <- morning %>%
  filter(Yr == 2015) %>%
  group_by(day, Site, basin, fish) %>%
  summarize(N = length(Rough),
            mean_rough = mean(Rough), 
            median_rough = median(Rough), 
            sd_rough = sd(Rough),
            se = sd_rough / sqrt(N))

# summary stats tables for each site (ACI)

c16 <- center2016 %>% 
  group_by(day, fish) %>%
  summarize(N = length(ACIout),
            mean_ACI = mean(ACIout), 
            median_ACI = median(ACIout), 
            sd_ACI = sd(ACIout),
            se = sd_ACI / sqrt(N))

c15 <- center2015 %>% 
  group_by(day, fish) %>%
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
  group_by(day, fish) %>%
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

# PLOTS -------------------------------------------------------------------

ggplot(center2015, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "lm")

ggplot(center2016, aes(x = day, y = ACIout, colour = fish)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = TRUE, method = "loess") +
  labs(x = "day", y = "Acoustic Complexity Index", title = "Daily Average with loess smooth")


# graphical representation of mean ACI over time 

ggplot(data = ungroup(daily_ACI_means_2015), aes(x = day, y = mean_ACI, color = fish)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~basin) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # graphical representation of mean Roughness over time 
ggplot(data = ungroup(daily_Rough_means_2015), aes(x = day, y = mean_rough, color = fish)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~basin) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "time of year", 
       y = "mean morning Roughness", 
       title = "Acoustic Roughness, all sites, 5:10-9:10am")


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
