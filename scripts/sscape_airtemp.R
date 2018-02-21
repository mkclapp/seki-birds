# Created: January 2016 // Updated: October 30 2016
# Author: Mary Clapp
# Script to concatenate and generate temperature graphs of SongMeter locations 

rm(list=ls(all=TRUE)) 

library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(lubridate)

### CONCATENATE DATA - no need to repeat since complete file is already generated ###
### adapted from psychwire.wordpress.com ###
getwd()

workingDir <- "../data/temps/airtemp/2014"
filelist <- list.files(workingDir)
dataset <- do.call("rbind",lapply(filelist,
                                  FUN=function(files){read.table(files,
                                                                 header=FALSE, 
                                                                 col.names = c("date","time","temp","volt","na","na","dunno","dunno","dunno","dunno","dunno","dunno"),
                                                                 fill=TRUE,
                                                                 sep="\t")}))

# add a column for site name

dataset$site <- 

# give each site its own data frame
a1 <- dataset

# merge data tables
temps <- rbind(a1,a2,b1,b2,c1,c2,u1,u2,w1,w2)
temps <- temps[c(13,1:3)] # reorder columns
temps <- temps[,1:4] # get rid of unwanted columns

# save CSV of merged temperature data
write.csv(temps, file = "2014temps.csv")

### PLOT DATA ###

# Read and format CSV file #
temps <- read.csv('/Users/maryclapp/Desktop/2015_SM_Temp/2015temps.csv') #open .csv
temps <- temps[,2:6] #get rid of dummy column

temps$date <- as.Date(temps$date, "%Y-%B-%d") #reformat date characters to date format
temps <- within(temps, { timestamp=format(as.POSIXct(paste(date, time)), "%Y-%m-%d %H:%M:%S") }) #combine date and time into single timestamp

temps <- as.data.frame(temps)
write.csv(temps, file = "2015temps.csv")

# Find daily min and max temps for each date and site

# use aggregate to create new data frame with the maxima
agg <- aggregate(temp ~ date, temps, max)
# then simply merge with the original
df.max <- merge(df.agg, df.orig)
df.max

library(ggplot2)
ggplot(data = temps, aes(x=timestamp, y=temp))
+ geom_line(aes(color=site))

### SURVEY EFFORT GRAPHIC ###


mydir <- dir("data/temps/airtemp/TEMPDATA_2014")

file_names <- dir("data/temps/airtemp/TEMPDATA_2014") #where you have your files
d <- do.call(rbind,lapply(file_names,read.csv))
d$Date <- ymd(d$Date)