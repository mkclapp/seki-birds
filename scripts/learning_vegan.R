# learning vegan
#8/1/2018
library(tidyverse)
library(vegan)
data(BCI)
data(BCI.env)
glimpse(BCI)
head(BCI)

# species accumulation
sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

# rarefaction
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI))) # this calculates the minimum number of 
Srare <- rarefy(BCI, raremax) # rarefied number of species 
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = BCI.env$Habitat, cex = 0.6)
class(BCI.env)
# now try with my bird data (audio)
# need to get into correct format (colnames = species, rownames = surveys) 

div <- birds %>% group_by(lake, ID, name) %>%
  summarise(n_obs = n()) # seconds the species is audible within a 10-minute window 
# incid is an incidence matrix that has no "count" data

divmatrix <- spread(data = div, 
       key = ID, 
       value = n_obs,
       fill = 0)
divmatrix <- as.data.frame(divmatrix)
rownames(divmatrix) <- divmatrix[,1]
divmatrix <- divmatrix %>% select(-lake, -name)

incid <- divmatrix
incid[incid > 0] <- 1

sp1 <- specaccum(incid)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

# rarefaction
S2 <- specnumber(divmatrix) # observed number of species
(raremax2 <- min(rowSums(divmatrix))) # this calculates the minimum number of 
Srare2 <- rarefy(divmatrix, raremax2) # rarefied number of species 
plot(S2, Srare2, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)

# to color lines by anything, they need to be factors.
dat$lake <- as.factor(dat$lake)
dat$basin <- as.factor(dat$basin)
dat$fish <- as.factor(dat$fish)
rarecurve(divmatrix, step = 1, sample = raremax2, col=dat$basin, cex = 0.6) # YIKES


# now by aggregating surveys per lake and input = number of surveys each species was detected??

# make by lake
divmatrix_lake <- div %>% group_by(lake, ID) %>%
  summarise(n_obs = n())

divmatrix_lake <- spread(data = divmatrix_lake, 
                         key = ID, 
                         value = n_obs,
                         fill = 0)

divmatrix_lake <- as.data.frame(divmatrix_lake)
rownames(divmatrix_lake) <- divmatrix_lake[,1]
divmatrix_lake <- divmatrix_lake %>% select(-lake)

S <- specnumber(divmatrix_lake) # observed number of species
(raremax <- min(rec_summary$n_samples)) # this calculates the minimum number of 
Srare <- rarefy(divmatrix_lake, raremax) # rarefied number of species 
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)

# to color lines by anything, they need to be factors.
lakeinfo$lake <- as.factor(lakeinfo$lake)
lakeinfo$basin <- as.factor(lakeinfo$basin)
lakeinfo$fish <- as.factor(lakeinfo$fish)
rarecurve(divmatrix_lake, step = 1, sample = raremax, col=lakeinfo$fish, cex = 0.6)
# hmm i don't know if the above is really correct


# species accumulation
sp2 <- specaccum(divmatrix_lake, "random")
sp2
summary(sp2)
plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

# then need to create a covariate matrix with colnames = covariates, rownames = surveys