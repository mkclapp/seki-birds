library(ggplot2)
library(soundecology)
library(seewave)

#get min and max values for frequency for each playback

wavdir <- "/Users/mary/Desktop/Research/data/moch_playback"
list.files(wavdir)
wavs <- list.files(wavdir, pattern=".wav", recursive=T, full.names=T)
readWave()


list.files("/Users/mary/Desktop/Research/data/moch_playback")
c1 <- read.csv("/Users/mary/Desktop/Research/data/moch_playback/CENTER1_20140816_203919.Table.1.selections.csv")
c2 <- read.csv("/Users/mary/Desktop/Research/data/moch_playback/CENTER2_20140817_080105.Table.1.selections.csv")

ggplot(c1, aes(x=Dist_to_SM, y=Max.Power..dB., color=Playback.)) +
  geom_point() +
  geom_text(aes(label=Playback.), hjust=0, vjust=0) +
  theme_bw() +
  labs(x = "Distance to Microphone (meters)", 
       y = "Max Power (dB)") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = c(0.9,0.7))

ggplot(c2, aes(x=Dist_to_Mic, y=Max.Power..dB., color=Playback., label=Playback.)) +
  geom_point() +
  geom_text(aes(label=Playback.),hjust=0, vjust=0) +
  theme_bw() +
  labs(x = "Distance to Microphone (meters)", 
       y = "Max Power (dB)") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = c(0.9,0.7))
