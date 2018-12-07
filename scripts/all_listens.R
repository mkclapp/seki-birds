# Load libraries ----------------------------------------------------------

rm(list = ls()) # clear the environment
library(tidyverse)
library(lubridate)
library(chron)
library(vegan)
library(plotrix)
library(lme4)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Read Selection Tables [DONE 12/4/2018] ----------------------------------

# IMPORTANT NOTE 11/18/2018:
# "alltables_draft" is a folder that contains copies of both the early-season listens (which contain only species lists)
# AND the early july listens (for which every sound is annotated). The file renaming does not work on this folder. 
# I need to read in separate folders and then rbind them after renaming them.

# Step 1: make list of filenames. First the early season listens...
file_names <- list.files("data/june_tables/", full.names = T) # needs to be full.names = T in order for the read.delim below to work
length(file_names) # 92 because some of the files were too full of weather noise
listens <- do.call("rbind", lapply(file_names, read.delim))

colnames(listens)

# remove "_000.wav" from Begin.File 
listens$Begin.File <- substr(listens$Begin.File, 1, 23)

listens <- listens %>% select(Begin.File, Selection, Begin.Time..s., Max.Power..dB., ID, NOTES)

# add columns for lake, treatment, and basin
listens <- listens %>% separate(Begin.File, c("lake", "date", "time"), sep="_", remove=FALSE)
listens <- listens %>% separate(lake, c("basin", "fish"), sep=-1, remove=FALSE)

listens$fish <- factor(ifelse(gsub('[^12]', '', listens$lake) == "2", "fish", "fishless"))


# format dates and times
listens <- listens %>% unite(timestamp, date, time, sep = "_", remove = FALSE)

listens$timestamp <- parse_date_time(listens$timestamp, "ymd HMS")

class(listens$timestamp)

listens <- listens %>% 
  separate(timestamp, c("date", "time"), sep="\\ ", remove = FALSE) 

listens$time <- chron(times=listens$time)
colnames(listens)
glimpse(listens)

unique(listens$ID) # returns all the different entries for ID
unique(listens$NOTES) # lol

# then the July listens...

file_names <- list.files("data/renamed_selection_tables", full.names = T, pattern = "*.txt") # needs to be full.names = T in order for the read.delim below to work
length(file_names) # 100 files

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
audio <- audio %>% select(Begin.File, Selection, Begin.Time..s., Max.Power..dB., ID, NOTES, name)

# add columns for lake, treatment, and basin
audio <- audio %>% separate(name, c("lake", "date", "time"), sep="_", remove=FALSE)
audio <- audio %>% separate(lake, c("basin", "fish"), sep=-1, remove=FALSE)

audio$fish <- factor(ifelse(gsub('[^12]', '', audio$lake) == "2", "fish", "fishless"))
# remove meaningless column that resulted from splitting the filename info

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

audio$Begin.File <- audio$name
audio$name <- NULL
colnames(audio)
colnames(listens)

# have to reorder the columns in audio to match the order in listens:
audio <- audio %>% select(Begin.File, lake, basin, fish, timestamp, date, time, Selection, Begin.Time..s., Max.Power..dB., ID, NOTES)

# rbind the dataframes
all <- rbind(audio, listens)
write_csv(x = all, path = "data/all_listens_all_20181204.csv", col_names = TRUE)
rm(listens, audio, clipNames)

# Read in data tables -----------------------------------------------------

all <- read_csv(file = "data/all_listens_all_20181204.csv") # most recently written file using the above section of code
lakeinfo <- read_csv("data/lakeinfo.csv", col_names = TRUE)
lakeinfo <- lakeinfo %>% separate(lake, c("basin", "fish"), sep=-1, remove=FALSE) 
lakeinfo$fish <- factor(ifelse(gsub('[^12]', '', lakeinfo$lake) == "2", "fish", "fishless"))

spec_names <- read_csv("data/species_names.csv") 
#rec_summary <- read_csv("data/100listens_recsummary.csv")


# Filter data -------------------------------------------------------------

# Quantify unknowns

july <- all %>% filter(month(date)==7)
june <- all %>% filter(month(date)==5 | (month(date)==6))
unk <- n_distinct(which(startsWith(june$ID, "UNK")==TRUE)) # unknowns 
unc <- n_distinct(which(startsWith(june$NOTES, "*")==TRUE)) # uncertain IDs
(unk+unc)/nrow(june)*100 # fraction of all entries for which species ID is uncertain

unk/nrow(june) *100

unique(all$ID) # return list of all IDs 
birds <- all %>% filter(!grepl("[*]",NOTES), !grepl("fledg", NOTES),
                #Max.Power..dB. > 40.0, # lose ~2000 observations if I restrict to 40dB
                ID == "AMPI" | ID =="GCRF" | ID == "WCSP" | ID == "DEJU" | ID == "ROWR" | ID == "MOCH" | 
                  ID == "CAFI" | ID == "CLNU" | ID == "SPSA" | ID == "AMRO" | ID == "MOBL" | ID =="FOSP" | 
                  ID =="HETH" | ID =="YRWA" | ID =="NOFL" | ID =="DUFL" | ID =="OCWA" | ID =="BRBL" | 
                  ID =="WAVI" | ID == "TOSO" | ID=="RECR" | ID=="WIWA")
print(c("filtering birds from all removes", nrow(all)-nrow(birds), "entries"))

# VEGAN SHIT --------------------------------------------------------------

# we need to simplify the data into a site-species matrix and an environmental matrix for the covariates

# first flatten each dataset by the number of observations for each species in each site-visit
divmatrix <- birds %>% group_by(lake, ID, Begin.File) %>%
  #summarise(time_calling = sum(Delta.Time..s.)) # seconds the species is audible within a 10-minute window 
  summarise(n_obs = n())

# make by lake
# divmatrix_lake <- divmatrix %>% group_by(lake, ID) %>%
#   summarise(n_obs = n())

# spread() transforms the matrix so that every species has its own column and sites become rows
divmatrix <- spread(data = divmatrix, 
                    key = ID, 
                    value = n_obs,
                    fill = 0)

# set the unique file identifier as the rowname in the data matrix so individual entries can be correlated with the environmental matrix
# then remove the lake and filename info 
divmatrix <- as.data.frame(divmatrix)
rownames(divmatrix) <- divmatrix[,2]
divmatrix <- divmatrix[,3:22]

# Make a presence/absence matrix
# NOTE: THIS IS CRITICAL for the combined listen data because only ~half of it has vocalizations counted and half of it is just a species list
pa_matrix <- divmatrix
pa_matrix[pa_matrix > 0] <- 1 # converts anything greater than 0 to a 1

# make environmental matrix
env <- merge(birds, lakeinfo[,c("lake","elev_m","area_msq")], by = "lake")

env <- env %>% group_by(Begin.File, basin, fish, lake, date, time, elev_m, area_msq) %>%
  summarise(sprich = n_distinct(ID)) # don't really need this but need to summarise something in order to make the table
env <- as.data.frame(env)
rownames(env) <- env[,1] # assign rownames using BeginFile
env <- env %>% select(-Begin.File)
head(env)

# the divmatrix and the env matrix both have 188 columns, one for each file that contains birds

# Rarefaction -------------------------------------------------------------

# SPECIES ACCUMULATION
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

divtot.audio <- cbind(pa_matrix, env) 
X <- split(divtot.audio, divtot.audio$lake)
names(X) <- unique(divtot.audio$lake) 
list2env(X, envir = .GlobalEnv) # OMG i'm so cool with these two lines of code

# SPECIES ACCUMULATION CURVES FOR ALL SITES, LISTEN DATA
# TODO: there must be a way to run a for loop to do this 
curve_amphit1 = specaccum(AMPHIT1[, 1:20], method = "random")
curve_amphit2 = specaccum(AMPHIT2[, 1:20], method = "random")
curve_barret1 = specaccum(BARRET1[, 1:20], method = "random")
curve_barret2 = specaccum(BARRET2[, 1:20], method = "random")
curve_center1 = specaccum(CENTER1[, 1:20], method = "random")
curve_center2 = specaccum(CENTER2[, 1:20], method = "random")
curve_upkern1 = specaccum(UPKERN1[, 1:20], method = "random")
curve_upkern2 = specaccum(UPKERN2[, 1:20], method = "random")
curve_wright1 = specaccum(WRIGHT1[, 1:20], method = "random")
curve_wright2 = specaccum(WRIGHT2[, 1:20], method = "random")


#plot curve_all first
plot(sp1, ci.type="poly", lwd=2, ci.lty=0, ci.col="lightgray", xlab = "", ylab="")
#then plot the rest
plot(curve_amphit1, add = TRUE, col = cbPalette[6], lwd=2, ci.type = "bar", ci.col = cbPalette[6]) 
plot(curve_amphit2, add = TRUE, col = cbPalette[2], lwd=2, ci.type = "bar", ci.col = cbPalette[2])
plot(curve_barret1, add = TRUE, col = cbPalette[6], lwd=2, ci.type = "bar", ci.col = cbPalette[6])
plot(curve_barret2, add = TRUE, col = cbPalette[2], lwd=2, ci.type = "bar", ci.col = cbPalette[2])
plot(curve_center1, add = TRUE, col = cbPalette[6], lwd=2, ci.type = "bar", ci.col = cbPalette[6])
plot(curve_center2, add = TRUE, col = cbPalette[2], lwd=2, ci.type = "bar", ci.col = cbPalette[2])
plot(curve_upkern1, add = TRUE, col = cbPalette[6], lwd=2, ci.type = "bar", ci.col = cbPalette[6])
plot(curve_upkern2, add = TRUE, col = cbPalette[2], lwd=2, ci.type = "bar", ci.col = cbPalette[2])
plot(curve_wright1, add = TRUE, col = cbPalette[6], lwd=2, ci.type = "bar", ci.col = cbPalette[6])
plot(curve_wright2, add = TRUE, col = cbPalette[2], lwd=2, ci.type = "bar", ci.col = cbPalette[2])
#legend("bottomright", env$lake, fill=cbPalette[1:10], bty="n")

# Soooooo still not great for everything other than CENTER and maybe AMPHIT1. god fucking dammit.
# but they are starting to differentiate. And it looks like sprich is VERY different between them
# So Beta-div-wise, I should probably use ... whichever one is for that


# Beta Diversity ----------------------------------------------------------

library(MASS)

#calculate dissimilarity indices using vegan 

sor <- vegdist(pa_matrix) # Sorenson
mean(sor)

jac <- vegdist(pa_matrix, method = "jaccard") # Jaccard
mean(jac)


# NMDS --------------------------------------------------------------------


n1 = metaMDS(pa_matrix, trymax = 100, distance = "jaccard")
n1
# "no convergent solutions" using default (bray) or jaccard



# plotting anyway
MDS1 = n1$points[,1]
MDS2 = n1$points[,2]
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Location = env$basin)

head(NMDS)

# PLOT results of NDMS
ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Location)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot")


# dunnoooooo --------------------------------------------------------------

# TO DO: google error message for isoMDS, look up graphical pkg DKarp recommended for visualizing NMDS
# haven't figured out below yet
vare.pca <- rda(pa_matrix)
plot(vare.pca)
biplot(vare.pca, scaling = -1)

vare.pca2 <- rda(pa_matrix, scale = TRUE)
plot(vare.pca2)
biplot(vare.pca2, scaling = -1)


# PERMANOVA ---------------------------------------------------------------

env$elev_m.s <- scale(env$elev_m)

adonis(formula = sor ~ fish*basin, env, perm = 200)


# betapart ----------------------------------------------------------------
library(betapart)
betapart_sor <- beta.multi(pa_matrix, index.family="sorensen")

# why does this only return three values? "multi-site dissimilarity values"
# so do I calculate the same for each lake and then compare to the global dissimilarity value?

data(ceram.s)
head(ceram.s)
ceram.beta <- beta.multi(ceram.s, index.family = "sor")
