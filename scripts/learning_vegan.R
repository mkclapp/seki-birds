# learning vegan
#8/1/2018
library(tidyverse)
library(vegan)
data(BCI)
glimpse(BCI)
head(BCI)

rarefy(BCI)

Srar <- rarefy(BCI, min(rowSums(BCI)))

class(BCI)



data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)
