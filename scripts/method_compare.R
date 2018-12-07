library(tidyverse)
library(lubridate)
library(chron)
library(vegan)

# TODO: identifier column does not match perfectly; they're sometimes ~1 min off

# read in acoustic index data ---------------------------------------------
indices <- read_csv("data/listens_20180309_AI.csv")
indices$Timestamp <- parse_date_time(indices$Timestamp, "ymd HMS")
class(indices$Timestamp)
indices$Timestamp <- round_date(indices$Timestamp, "10 minutes")

# make a column with which to merge this dataset with the other
indices$identifier <- paste(indices$Site, indices$Timestamp, sep = "_")

# read in audio-listen data -----------------------------------------------
audio <- read_csv("data/audio_listen_data_clean_20180814.csv")

# make a mergeable column here too
audio$timestamp <- round_date(audio$timestamp, "10 minutes")
audio$identifier <- paste(audio$lake, audio$timestamp, sep = "_")

# Explore Background Noise ------------------------------------------------

background_info <- audio %>% filter(ID == "background") %>%
  group_by(lake, date) %>%
  summarize(num_bk_measures = n(),
            mean_power = mean(Max.Power..dB.),
            median_power = median(Max.Power..dB.),
            min_power = min(Max.Power..dB.),
            max_power = max(Max.Power..dB.),
            sd_power = sd(Max.Power..dB.)) 

# Summarise audio listen data ---------------------------------------------

# sampling effort by lake
rec_summary <- audio %>% group_by(lake) %>%
  summarise(days_sampled = n_distinct(date),
            n_samples = n_distinct(timestamp),
            minutes_sampled = n_distinct(timestamp)*10, 
            sec_sampled = minutes_sampled*60)

# create another dataframe that fixes typos, removes background entries
audio$ID <- toupper(audio$ID)
audio$ID <- str_replace(string = audio$ID, pattern = "RPWR", replacement = "ROWR")
audio$ID <- str_replace(string = audio$ID, pattern = "INSECTS", replacement = "INSECT")

# all biophonic sounds
biophony <- filter(audio, ID != "ECHO", !grepl("BAND", ID), !grepl("BACK", ID), !is.na(ID))
unique(biophony$ID)

biophony <- biophony %>% select(-Begin.File, -Begin.Time..s.)

soundcount <- biophony %>% group_by(identifier, basin, fish, lake, ID) %>%
  summarise(n_sounds = n())
head(soundcount)

sumsum <- soundcount %>% spread(key = ID, value = n_sounds, fill = 0) 

sumsum <- sumsum %>%
  gather(key = ID, value = soundcount, ... = AMPI:YRWA) %>%
  group_by(identifier, basin, fish, lake) %>%
  summarise(shannon = diversity(soundcount, index="shannon"),
            simpson = diversity(soundcount, index="simpson"),
            nsounds = sum(soundcount))

# frank = matrix of response vars and predictor indices
frank <- merge(sumsum, indices, by = "identifier") #OMG it worked 


# Comparisons with single indices -----------------------------------------

# (ACIout = Acoustic Complexity, AR = Acoustic Richness, Rough = Roughness)
ggplot(frank) + 
  geom_point(aes(x = ADI_step, y = simpson, color = lake))


# Random Forest -----------------------------------------------------------
# adapted from R Buxton's RandomForestCode 11/21/2018
library(rfUtilities)
library(randomForest)
library(caret)

# frank is the dataframe we need

DATA <- frank[complete.cases(frank),] # remove incomplete cases

Response <- "shannon"  #For example, "SpeciesRichness"
  
#Put a dataframe with only index covariates in here 
Covariates <- DATAcomplete[,which(colnames(DATAcomplete)=="ACIout"):which(colnames(DATAcomplete)=="AR")]
  
RFM1<-randomForest(as.formula(paste(Response, "~", 
                                                 paste(colnames(Covariates), collapse = " + "),sep = "")), data=DATA, fam="gaussian",
                                importance=TRUE, ntree=500)
RFM1

# remove collinear variables

cl<-multi.collinear(DATA[,which(colnames(DATA)=="ACIout"):which(colnames(DATA)=="AR")], p=0.05)
DATAcomplete <- DATA[,-which(colnames(DATA) %in% cl)]

# go up and change Response input to DATAcomplete

#### RANDOM FOREST MODEL SELECTION 
# NOT WORKING
# RFMODSEL<-rf.modelSel(DATA[,which(colnames(DATA)=="ACI"):which(colnames(DATA)=="AR")], 
#                       DATA[[Response]],  imp.scale = "mir", final.model = TRUE)
# 
# 
# ##Get R squared and Mean squared error for each RF type
# Output<-data.frame(MSE=mean((predict(RandomForestMod)-DATA[[Response]])^2), 
#                    Rsq=mean(RandomForestMod$rsq), Model="RandomForest", ResponseType=Response)
# 
# Output_RFSelection<-data.frame(MSE=RFMODSEL$rf.final$mse-DATA[[Response]])^2), 
# Rsq=RFMODSEL$rf.final$rsq, Model="RandomForest_ModelSelection", ResponseType=Response)
# 
# RFM2


  
# GLMs --------------------------------------------------------------------



library(lme4)

colnames(frank)
colnames(frank)[3] <- "fish"
maudio <- lm(shannon ~ BKdB_bird + BKdB_low + ACIout + Hf + Ht + Rough + ADI_step + AR, data = frank)
plot(fitted(maudio), resid(maudio)) 
qqnorm(resid(maudio))
qqline(resid(maudio))
summary(maudio)


maudio2 <- lm(shannon ~ BKdB_bird + ACIout + Hf + ADI_step, data = frankbirds)
plot(fitted(maudio2), resid(maudio2)) 
qqnorm(resid(maudio2))
qqline(resid(maudio2))
summary(maudio2)


ggplot(frank, aes(x = ADI_step, y = nsounds)) + 
  geom_point() +
  abline() +
  labs(title="Acoustic Diversity Index and Shannon diversity in 100 audio samples",
       x = "Acoustic Diversity (ADI_step)", y = "Shannon Diversity") +
  theme_minimal()

ggplot(frankbirds, aes(x = BKdB_bird, y = shannon)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Acoustic Diversity Index and background decibel level in 100 audio samples",
       x = "Background decibel level in mid-range frequency band (1413-11220 Hz)", y = "Shannon Diversity") +
  theme_minimal()


# old code [references external data frame] -------------------------------

# birds only
unique(birds$ID)

birdcount <- birds %>% group_by(identifier, basin, fish, lake, ID) %>%
  summarise(n_sounds = n())
head(birdcount)
colnames(birdcount)
sumbirds <- soundcount %>% spread(key = ID, value = n_sounds, fill = 0) %>%
  gather(key = ID, value = soundcount, ... = AMPI:YRWA) %>%
  group_by(identifier, basin, fish, lake) %>%
  summarise(shannon = diversity(soundcount, index="shannon"),
            simpson = diversity(soundcount, index="simpson"))

frankbirds <- merge(sumbirds, indices, by = "identifier") #OMG it worked 

# plot against acoustic indices
ggplot(frankbirds, aes(x = AR, y = shannon)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Acoustic Richness and Shannon diversity in 100 audio samples",
       x = "Acoustic Richness (AR)", y = "Shannon Diversity")+
  theme_minimal()

ggplot(frankbirds, aes(x = ACIout, y = shannon)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Acoustic Complexity Index and Shannon diversity in 100 audio samples",
       x = "Acoustic Complexity Index (ACI)", y = "Shannon Diversity")+
  theme_minimal()

ggplot(frankbirds, aes(x = Rough, y = shannon)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Acoustic Roughness and Shannon diversity in 100 audio samples",
       x = "Acoustic Roughness", y = "Shannon Diversity") +
  theme_minimal()

ggplot(frankbirds, aes(x = ADI_step, y = shannon)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title=NULL,
       x = "Acoustic Diversity Index (ADI)", y = "observed Shannon Diversity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none", 
        plot.title = element_text(family = "Helvetica", size=30),
        axis.title = element_text(family = "Helvetica", size=18), 
        axis.text.x = element_text(family = "Helvetica", size=16, angle=90, hjust=1, vjust=0.5),
        axis.text.y = element_text(family = "Helvetica", size=18, angle=0))

ggsave(filename = "ADIxShannon.png", device = "png", path = "poster_plots/", width = 5, height = 5, units = "in")


