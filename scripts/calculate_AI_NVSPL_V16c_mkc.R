#CALCULATE ACI, ADI for a directory of NVSPL files

# V2- starts ACI calculation on the top of an hour
# V6- runs for multiple sites
# V7- fixed to wrk with SEKI site names
# V8- also caluclates an a-weighted background noise level
# V9- under construction- addind more indicies
# CHECK BACKGROUND VALUES- remove lowest band from calculation
# V11- added acoustic richness
# V12- added acoustic activity for the noise band, addition details for cluster output
# V13- updated the cluster analysis based on the comparison
# V14- writes out a file per site... helps if there are errors!!
# v15- fixed L10, L90
# V16- changed cluster length calculation to include lenghts of 1 sec
# 16b- option to not do day cluster because too big a file!!
# 16c- directories set up and NO caluclations per day
# 16c_seki_mkc- using fmax = 10000 and changed fbinmax to 10 (2/5/2018)

rm(list=ls())
#_________________________________________________________________________________________________
## ANALYSIS SET-UP
#_________________________________________________________________________________________________
#LOAD LIBRARIES
library(tuneR)
library(seewave)
library(ineq)
library(fftw)
library(soundecology)
library(vegan)
library(RAtmosphere)
#library(date)
library(e1071)  
library(moments)
library(RSNNS)
library(sparcl)
library(dbscan)
library(cluster)
library(factoextra)
library(NbClust)

#SET UP OPTIONS
sunrise   = 0 # option to calculate ACI around sunrise... NEED LAT AND LON FOR A SITE
plt       = 0 # option to plot NVSPL files
fminimum  = "H1600" #lower limit-- HXXXX represents the center frequency of the octave band...
#fmaximum  = "H8000" #upper limit-- eg. H1600 = 1413-1778 Hz
fmaximum  = "H10000" #upper limit-- eg. H10000 = 8913-11220 Hz
BKfminimum = "H31p5" #lower limit of background noise !!!!!!!NOTE: if NVSPL calculated with PAMGuide, no data below H25! 
BKfmaximum = "H1250" #upper limit of background noise 
DayIndex   = "NotDay" #Do you want to calculate ACI for the entire day? #"NotDay" = No, "Day" = Yes 
Timestep   = 10 #In minutes. If you don't want to calculate ACI for the entire day, set how how many minutes to calculate ACI over? 
fbinMax = 10 #sets the frequency bins to looks across for cluster analysis- relates to frequency bins in biological band

#FLAGS
savFile   = 1 #saves 2 files- daily values and timestep values
pltdsate  = 0 #some initial plotting ideas- specific to SEKI dataset

#FUNCTIONS
normdBA <- function (x) { (x-(-10)) / (80 - (-10)) } 

#___________________________________________________________________________________________________
## GET FILES
#___________________________________________________________________________________________________
workingDir = "C:\\Users\\Patricelli Lab\\MKCLAPP\\Soundscapes\\analysis\\nvspl"
#workingDir =  "E:\\NPS_DATA\\EVER\\NVSPL_notAdjusted" 
#"E:\\NPS_DATA\\SEKI_clapp\\NVSPL\\NVSPL"     #"E:\\NPS_DATA\\SEKI_fire\\2NVSPL" 
#"E:\\NPS_DATA\\SEKI_fire\\SEKI_test\\SEKI10" #"E:\\NPS_DATA\\SEKI_fire\\2NVSPL" 
#"E:\\SEKI-fire\\NVSPL" 
#"E:\\NPS_DATA\\SEKI_fire\\NVSPL_v3"
#"F:\\CENTER1_2014\\NVSPL_36dB" #choose.dir() 
nvsplFiles <- list.files(workingDir, pattern="^NVSPL", recursive=T, full.names=T)
mySites <- sort(unique(gsub("NVSPL_(.+)_\\d{4}_\\d{2}_\\d{2}_\\d{2}.txt","\\1",basename(nvsplFiles))))

#___________________________________________________________________________________________________
## RUN BIG LOOP- sites (ss) and days (dys) calculate acoustic indices, background values
#___________________________________________________________________________________________________

for (ss in 1:length(mySites)  ) # 1:length(mySites) START loop through sites
{
  OUTPUT_sites = NULL
  OUTPUT_sitesD = NULL
  
  siteFiles <- nvsplFiles[grepl(paste("NVSPL_",mySites[ss],"_",sep=""), basename(nvsplFiles) )]
  # cat('Site ', mySites[ss], ": ", ss,' of ',length(mySites), ' (no. files = ' , length(siteFiles), ")", '\n', sep="") 
  #siteFiles <- nvsplFiles[grepl(mySites[ss],basename(nvsplFiles))]
  siteDays <- sort(unique(gsub("NVSPL_.+_(\\d{4}_\\d{2}_\\d{2})_\\d{2}.txt","\\1",basename(siteFiles))))
  
  #GET site lat/long
  if (sunrise == 1) {
    myFile <- choose.files() #Parks.csv
    siteloc <- read.csv(myFile, header = TRUE)
  }
  
  #FORMAT matrices and data
  uDayIndex = (unique(siteDays))
  acis = NULL
  acisD = NULL
  
  for ( dd in 1:length(uDayIndex)) # 1:length(uDayIndex) START loop through unique site days 
  {
    # dd = 14 #for testing
    #t1=proc.time()	#start timer
    cat('Calculating Acoustic Index for site: ', mySites[ss], " (", ss,' of ',length(mySites),') for file ', dd,' of ',
        length(uDayIndex), " [", uDayIndex[dd], "]", '\n',sep="")
    dy = siteDays[dd]
    siteDayFiles <- siteFiles[grepl(dy,basename(siteFiles))]
    
    dayMatrix <- NULL
    
    #GET DATA IN THE RIGHT FORMAT
    for(sf in siteDayFiles) {
      myTempMatrix <- read.csv(sf)
      timeData <- matrix(as.numeric(
        unlist(
          strsplit(
            gsub(":"," ",
                 substr(gsub('-',' ',as.character(myTempMatrix[,2])),0,19)
            ),' ')
        )
      ),ncol=6, byrow=T
      )
      
      myTempMatrix <- cbind(timeData, myTempMatrix[,3:45] )
      colnames(myTempMatrix)[1:6] <- c("Yr","Mo", "Day", "Hr", "Min", "Sec")
      dayMatrix <- rbind(dayMatrix, myTempMatrix)
      
      # review spectrogram of the 1 hr file if desired
      if (plt == 1)
      {
        myTempMatrixPLT <- cbind(timeData, myTempMatrix[,3:45] )
        site <- gsub('.txt', '', as.character(basename(sf)) )
        colnames(myTempMatrixPLT)[1:6] <- c("Yr","Mo", "Day", "Hr", "Min", "Sec")
        dayMatrix2 <-  myTempMatrixPLT
        
        plotSecs <- timeData[,5] * 60 + timeData[,6]
        plotFreqs <- grep("H\\d",colnames(dayMatrix2),value=T)
        image(plotSecs,1:length(plotFreqs),
              as.matrix(dayMatrix2[,plotFreqs]),
              col=colorRampPalette(c("blue","orange","white"))(100),
              xlab="Time (s)",ylab = "Freq Idx",
              zlim=c(-8,85))
        #locator() #allows you to pick points on the image
      }
    }
    rm(myTempMatrix)
    
    # truncate by time of day (for calculating around sunrise)
    if (sunrise == 1) {
      ind <- match(mySites[ss],siteloc$ID,nomatch = NA)
      Lat <- siteloc$Latitude[ind]
      Lon <- siteloc$Longitude[ind]
      jDay <- as.numeric(format(as.POSIXct(dy, format="%Y_%m_%d"),"%j"))
      estsun = suncalc(jDay,Lat,Lon)
      s1 = unlist(strsplit(as.character(estsun[1]), "[.]") )
      shr = as.numeric(s1[1])
      dayMatrix <- cbind(dayMatrix[,"Hr"] + (60 * dayMatrix[,"Min"] + dayMatrix[,"Sec"])/3600, dayMatrix)
      colnames(dayMatrix)[1] <- "decHr"
      
      dayMatrix <- dayMatrix[dayMatrix[,"decHr"] > (estsun$sunrise - 1) & dayMatrix[,"decHr"] < (estsun$sunrise + 2),-1]
    }
    
    # check to see if full minutes are included, remove any non-full minutes
    test = do.call(paste, c(dayMatrix[c("Hr", "Min")], sep = "_"))
    unHrMin = unique(test)
    dayMatrix = cbind(dayMatrix,test)
    
    # remove rows with NAs...unique( dayMatrix$dbA )
    #dayMatrix = dayMatrix[na.omit(dayMatrix$dbA),]
    dayMatrix = dayMatrix[!is.na(dayMatrix$dbA),]
    
    #A-weight 33 octave bands in dayMatrix
    FREQ = colnames(dayMatrix[7:(33+6)])
    aweight <- ( c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
                   -13.4, -10.9, -8.6, -6.6, -4.2, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
                   1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3) )
    dayMatrix33 =  dayMatrix[    which( colnames(dayMatrix)==FREQ[1] ):
                                 which( colnames(dayMatrix)==FREQ[33]) ]
    dayMatrix33A = t( t(dayMatrix33) + aweight)
    dayMatrix33A = cbind(dayMatrix[,1:6],dayMatrix33A)
    
    # subset so sampling starts at the top of a time stamp
    Output  <-NULL
    Output1 <- NULL
    BKADI   <- NULL
    
    #........................................................................................
    ## RUN acoustic indicies on each "Timestep"
    #........................................................................................
    
    for(i in 0:23) # possible hours
    {
      for(j in 1:(60/Timestep)) # Timestep for each hour 
      {
        
        matchMins <- seq(Timestep * (j - 1), by = 1, length.out = Timestep)
        
        # unweighted matrix
        workingMatrix <- dayMatrix[ dayMatrix$Hr == i & dayMatrix$Min %in% matchMins , ] 
        unique( dayMatrix$Hr )
        unique( dayMatrix$Min )
       
        
        # A-weighted matrix
        workingMatrixA <- dayMatrix33A[
          dayMatrix$Hr == i & dayMatrix33A$Min %in% matchMins,] 
        
        dayMatrixlim =  NULL
        BKdayMatrixlimA = NULL
        BKdayMatrixlim =  NULL
        dayMatrixlimA =  NULL
        
        if( dim(workingMatrix)[1] >= (Timestep * 60)- 60)
        {
          #TRUNCATE matrix in frequency bands of interest
          dayMatrixlim =   workingMatrix[which( colnames(workingMatrix)==fminimum ):which( colnames(workingMatrix)==fmaximum) ]
          BKdayMatrixlim = workingMatrix[which( colnames(workingMatrix)== BKfminimum ):which( colnames(workingMatrix)== BKfmaximum) ]
          dayMatrixlimA =  workingMatrixA[which( colnames(workingMatrixA)== fminimum ):which( colnames(workingMatrixA)== fmaximum) ]
          BKdayMatrixlimA= workingMatrixA[which( colnames(workingMatrixA)== BKfminimum ):which( colnames(workingMatrixA)== BKfmaximum) ]
          #Number of frequency bands
          nFQ = as.numeric( dim(dayMatrixlim) [2] )
          nFQbk =  as.numeric( dim(BKdayMatrixlim) [2] )
          #Number of seconds
          fileDur = as.numeric( dim(dayMatrixlim) [1] )
          
          #........................................................................................
          # CALCULATE per-frequency metrics- need in other indices (NOT SAVED!)
          #........................................................................................
          #(1) simple average backtound noise estimate for each 1/3 octave band
          BKBirddB <- 10*log10( colMeans( 10^(dayMatrixlim/10) ) )
          ## NOTE: how to report these values: 
          # will not report- this is what you used for background noise estimate in the frequency band of the ADI
          # how you calculated these values:
          # In each timestep (10 min) for each 1/3 octave frequency band of interest (fminimum -fmaxium), 
          # 1) converted to pressures, 2) calcuted the mean pressure for each frequency band, and 3) converted back to dB
          
          # (2) Background noise from #2 in Towsey, modified here as a level for each frequency band, not normalized!
          BK_Towsey <- NULL
          for (ff in 1:length(dayMatrixlim) ) { #loop through each frequency band 
            #(1) compute a histogram of lowest dB values, with a length equal to 1/8th of total values
            tmp <- sort ( dayMatrixlim[,ff]) [ 1:( dim(dayMatrixlim)[1] /8) ]
            #(2) smooth the histogram with moving average- did not do!!!
            #!!!!! check this: filter(tmp,rep(1,5),method = "convolution")
            #(3) find bin with the maximum count
            tmp1 <- c(table(tmp)) #count for each dB bin
            tmpval <- as.numeric( (tmp1)) #counts for each bin as, numeric
            tmpnam <- as.numeric(names(tmp1)) #dB value for each bin, numeric
            tmpc <- as.matrix( cbind(tmpnam,tmpval))
            colmax <- as.numeric(which.max(tmpval))
            #(4) accumulating counts in histogram bins below (3) until 68% of counts below (3) is reached
            cutoff = sum( tmpval[ 1:colmax-1] ) * .68 #value to stop the summation at
            # got an error when the first bin had the max # of values..... so just  use the min value
            if (cutoff == 0) {stploc = 0}
            cntbk <- 0
            for (h in 1:length(tmpval[1:colmax])-1 )
            {
              if (cntbk > cutoff) {
                break }
              loc = colmax - h #find the location to start the summation
              cntbk = cntbk + tmpval[loc]
              stploc = tmpnam[loc-1]
            }
            # (5) final calc: (3) + N(4)
            if (cutoff == 0) {BK_Towsey[ff] = tmpnam[colmax] } else { BK_Towsey[ff] = tmpnam[colmax] + stploc*0.1 }
          }
          
          
          BK_Towsey_anth<- NULL
          for (ff in 1:length(BKdayMatrixlim) ) { #loop through each frequency band 
            #(1) compute a histogram of lowest dB values, with a length equal to 1/8th of total values
            tmp <- sort ( BKdayMatrixlim[,ff]) [ 1:( dim(BKdayMatrixlim)[1] /8) ]
            #(2) smooth the histogram with moving average- did not do!!!
            #!!!!! check this: filter(tmp,rep(1,5),method = "convolution")
            #(3) find bin with the maximum count
            tmp1 <- c(table(tmp)) #count for each dB bin
            tmpval <- as.numeric( (tmp1)) #counts for each bin as, numeric
            tmpnam <- as.numeric(names(tmp1)) #dB value for each bin, numeric
            tmpc <- as.matrix( cbind(tmpnam,tmpval))
            colmax <- as.numeric(which.max(tmpval))
            #(4) accumulating counts in histogram bins below (3) until 68% of counts below (3) is reached
            cutoff = sum( tmpval[ 1:colmax-1] ) * .68 #value to stop the summation at
            # got an error when the first bin had the max # of values..... so just  use the min value
            if (cutoff == 0) {stploc = 0}
            cntbk <- 0
            for (h in 1:length(tmpval[1:colmax])-1 )
            {
              if (cntbk > cutoff) {
                break }
              loc = colmax - h #find the location to start the summation
              cntbk = cntbk + tmpval[loc]
              stploc = tmpnam[loc-1]
            }
            # (5) final calc: (3) + N(4)
            if (cutoff == 0) {BK_Towsey_anth[ff] = tmpnam[colmax] } else { BK_Towsey_anth[ff] = tmpnam[colmax] + stploc*0.1 }
          }
          
          
          # (3) Exceedence levels by frequency band (not normalized!)
          exceed = NULL
          for (iy in 1:dim(dayMatrixlim)[2]) { 
            tmp <- quantile(dayMatrixlim[,iy], c(0.05, 0.1, 0.5, 0.9, 0.95), na.rm = TRUE) 
            exceed = cbind(exceed,tmp)      }
          colnames(exceed) <- colnames(dayMatrixlim)
          L10 = (exceed[4,]) #L10 = 90th percentile
          L90 = (exceed[2,]) #L90 = 10th percentile
          difexceed = L10 - L90
          
          #........................................................................................
          
          #........................................................................................
          #START ACOUSTIC INDEX CALCULATIONS
          #........................................................................................
          ## (1) CALCULATE ACI
          t <- array(0, dim(dayMatrixlim) - c(1,0))
          for (frq in 1:(dim(t)[2])) # loop through frequency bands
          {
            t[,frq] <- abs( diff(dayMatrixlim[,frq]) )/ sum(dayMatrixlim[,frq])
            
          }
          ACIout = sum(t)# ACI results for each time chunck on a given day
          rm(t)
          #........................................................................................
          
          #........................................................................................
          ## (2,3,4,5) CALCULATE background noise SPL for timestep, gives 1 Timestep background dB value
          #A-weight the BK values
          BKdBA_low = 10*log10( sum( 10^(BKdayMatrixlimA/10))/ (Timestep*60)) 
          BKdBA_bird = 10*log10( sum( 10^(dayMatrixlimA/10))/ (Timestep*60) )
          #unweight values
          BKdB_low  = 10*log10( sum( 10^(BKdayMatrixlim/10))/ (Timestep*60)) 
          BKdB_bird = 10*log10( sum( 10^(dayMatrixlim/10))/ (Timestep*60) )
          ## NOTE: how to REPORT these values: 
          # xx dB re:1 uPa (BKfminimum Hz- BKfminimum Hz)
          # how you calculated these values:
          # In each timestep (10 min) over the frequency band of interest, 
          # 1) converted to pressures, 2) calcuted the mean pressure, and 3) converted back to dB
          #........................................................................................
          
          #........................................................................................
          ## (6,7) Average signal amplitude- #1 in Towsey et al., 2013
          #modified as the mean dBA value for the timestep, normalized by the min/max (set at: -10 to 80 dB)
          avgAMP = normdBA(mean (workingMatrix$dbA) )
          L10AMP = normdBA(quantile (workingMatrix$dbA, .1) ) #10th percentile = L90
          #........................................................................................
          
          #........................................................................................
          ## (8,9,10) Spectal Entrophy (#10 in Towsey), Temporal entropy (#7 in Towsey), Entropy Index
          Pres = colMeans(10^(dayMatrixlim/10))  #Leq for each Fq band over time period (mean of the pressures)
          Pres2 = Pres/sum(Pres)
          Hf = -sum( (Pres2 * log(Pres2))) /log(nFQ)
          Leqt = ( rowMeans( (10^(dayMatrixlim/10)) ) ) #Leq for each second over entire band (could also used dBA)
          Leqt2 = Leqt/sum(Leqt)
          Ht = -sum( (Leqt2 * log(Leqt2)) / log(fileDur) ) 
          EI = Ht * Hf
          #........................................................................................
          
          #........................................................................................
          ## (11,12,13,14) Biophony, Anthrophony, normalised difference in soundscape, ratio of A/B
          BioPh = sum( colMeans(10^(dayMatrixlim/10)) )
          AthPh = sum( colMeans(10^(BKdayMatrixlim/10)) )
          SoundScapeI = (BioPh -AthPh ) / (BioPh + AthPh )
          Bio_anth = (BioPh /AthPh ) 
          #........................................................................................
          
          #........................................................................................
          ## (15,16,17) acoustic activity, count of acoustic events, duration of acoustic events
          AA = NULL
          AAc = NULL
          s2n <- NULL
          for (f in 1:length(dayMatrixlim) ) {
            AA[f] =  length( dayMatrixlim[,f][ dayMatrixlim[,f] > BK_Towsey[f]+3 ] )/ length (dayMatrixlim[,f])
            AAc[f] = length( dayMatrixlim[,f][ dayMatrixlim[,f] > BK_Towsey[f]+3 ] )
            s2n[f] = max((dayMatrixlim[,f])) - BK_Towsey[f]
          }
          AA= sum(AA)
          AAc = sum(AAc)
          AAdur = NULL
          for (f in 1:length(dayMatrixlim) ) {
            #logical matrix...
            temp <- dayMatrixlim[,f] > BK_Towsey[f]+3 
            #table(temp)["TRUE"] ; table(temp)["FALSE"]
            temp2 <- temp*1 # convert to 0/1
            #find consecutive 1 values...
            rl <- rle(temp2)
            len = rl$lengths
            v =  rl$value
            if (length(v) == 1 )
            {
              if   (v == 0) {AAdur[f] = 0 }
              else {AAdur[f] = len }
              next
            }
            cumsum = NULL
            cntAA = 0
            for ( qq in  seq(from = 2, to = length(v), by =2) ) 
            {
              cntAA = cntAA +1
              cumsum[cntAA] = len[qq]
            }
            AAdur[f] = mean(cumsum)
          }
          AAdur = median(AAdur)
          
          #........................................................................................
          ## START HERE!!!!! (15,16,17) acoustic activity, count of acoustic events, duration of acoustic events for NOISE
          AAanth = NULL
          AAcanth = NULL
          s2nanth <- NULL
          AAduranth = NULL
          
          for (f in 1:length(BKdayMatrixlim) ) {
            AAanth[f] =  length( BKdayMatrixlim[,f][ BKdayMatrixlim[,f] > BK_Towsey_anth[f]+3 ] )/ length (BKdayMatrixlim[,f])
            AAcanth[f] = length( BKdayMatrixlim[,f][ BKdayMatrixlim[,f] > BK_Towsey_anth[f]+3 ] )
            s2nanth[f] = max((BKdayMatrixlim[,f])) - BK_Towsey_anth[f]
          }
          
          AAanth= sum(AAanth)
          AAcanth = sum(AAcanth)
          
          for (f in 1:length(BKdayMatrixlim) ) {
            #logical matrix...
            temp <- BKdayMatrixlim[,f] > BK_Towsey_anth[f]+3 
            #table(temp)["TRUE"] ; table(temp)["FALSE"]
            temp2 <- temp*1 # convert to 0/1
            #find consecutive 1 values...
            rl <- rle(temp2)
            len = rl$lengths
            v =  rl$value
            if (length(v) == 1 )
            {
              if   (v == 0) {AAduranth[f] = 0 }
              else {AAduranth[f] = len }
              next
            }
            cumsum = NULL
            cntAA = 0
            for ( qq in  seq(from = 2, to = length(v), by =2) ) 
            {
              cntAA = cntAA +1
              cumsum[cntAA] = len[qq]
            }
            AAduranth[f] = mean(cumsum)
          }
          
          AAduranth = median(AAduranth)
          
          #........................................................................................
          
          #........................................................................................
          ## (18) Roughness
          Rough = NULL
          for (f in 1:length(dayMatrixlim) ){
            x = dayMatrixlim[,f]
            x <- x/max(x)
            deriv2 <- diff(x, 1, 2)
            Rough[f] <- sum(deriv2^2, na.rm = TRUE)
          }  
          Rough = median(Rough)
          #........................................................................................
          
          #........................................................................................
          ## (19,20) Acoustic diversity, acoustic eveness
          Score = NULL
          for (f in 1:length(dayMatrixlim) ) 
          {
            Score[f] = length( dayMatrixlim[,f][dayMatrixlim[,f] > BK_Towsey[f]+3] )/ length (dayMatrixlim[,f])
          }
          ADI_step = diversity(Score, index = "shannon")
          Eveness_step = Gini(Score)
          #........................................................................................
          
          #........................................................................................
          ## (21,22,23,24,25,26) frequency with max amplitude, total count of max for each freq band (normalized by total bands),
          # kurtosis, skewness, entrophy of spectral max, entrophy of spectral variance)
          # peak frequency
          peakf = NULL
          for (j in 1:dim(dayMatrixlim)[1] ) 
          {   peakf[j] = (which.max( dayMatrixlim[j,] ) )  }
          pk2 = matrix(0,1,dim(dayMatrixlim)[2])
          for (uu in 1:nFQ)  { pk2[uu] = sum(peakf == uu)  }
          colnames(pk2) = colnames(dayMatrixlim)
          pk2nor = pk2/fileDur
          pk = as.numeric ( gsub("H","",colnames(dayMatrixlim[which.max(pk2)]) ) )
          
          # kurtosis- is the shape of peak frequencies normally distributed (kurtosis) ?
          pkd = kurtosis( as.vector(pk2nor) )
          
          # skewness- What is the skewness is this distribution? (symmetrical = 0, right skew = +)
          pks = skewness(as.vector(pk2nor))
          
          # entropy of Spectral Maxima
          pk2[pk2 == 0] <- 1e-07
          pk_prob = pk2/(sum(pk2)) # normalize
          Hm = -sum( (pk_prob * log2(pk_prob) ) )/log2(nFQ)
          
          # entropy of spectral variance- pressure and dB
          Press = (10^(dayMatrixlim/10)) #conver back to pressure
          Pv = NULL
          for (v in 1:dim(Press)[2] ) {Pv[v] = var(Press[,v]) }
          Pv2 = Pv/sum(Pv)
          HvPres = -sum( (Pv2 * log2(Pv2)) ) / log2(nFQ)
          
          Pv = NULL
          for (v in 1:dim(dayMatrixlim)[2] ) {Pv[v] = var(dayMatrixlim[,v]) }
          Pv2 = Pv/sum(Pv)
          HvSPL = -sum( (Pv2 * log2(Pv2)) ) / log2(nFQ)
          #........................................................................................
          
          #........................................................................................
          ## (27) Normalize exceedence levels for dBA
          Exceed_norm    = normdBA ( quantile(workingMatrix$dbA, c(0.05, 0.1, 0.5, 0.9, 0.95),na.rm = TRUE) )
          L10n = (Exceed_norm[4]) #L10 = 90th percentile
          L90n = (Exceed_norm[2]) #L90 = 10th percentile
          dif_L10L90 = L10n - L90n
          #........................................................................................
          
          #........................................................................................
          ## (28) Acoustic Richness- calculate temoral entropy and median broadband SPL for each minute and ranks results
          Mamp = quantile( 10*log10( ( rowMeans( (10^(dayMatrixlim/10)) ) ) ), 0.5)
          
          #........................................................................................
          ## (29) Spectral diversity (Sd)- determine optimal number of clusters for the timestep and how confident/explained
          #!!!!!UNDER CONSTRUCTION!!!!!!!!!!!
          # http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
          # http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
          
          x = scale(dayMatrixlim)  # To standarize the variables
          
          res = NbClust(x, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "silhouette")
          # use all algothrums to find optimal cluster... takes way to long!
          #res = NbClust(x, distance = "euclidean", min.nc=2, max.nc=(dim(x)[1])/60, method = "ward.D", index = "all")
          # use "gap" method (Tibshirani et al. 2001) Smallest n_{c} such that criticalValue >= 0
          #res = NbClust(x, distance = "euclidean", min.nc=2, max.nc=(dim(x)[1])/60, method = "ward.D", index = "gap")
          # Euclidean distance : Usual square distance between the two vectors (2 norm).  d(x,y)= (sum_{j=1}^{d} (x_j - y_j)^2)^(1/2)
          # Ward method minimizes the total within-cluster variance. At each step the pair of clusters with minimum cluster distance are merged. To implement this method, at each step find the pair of clusters that leads to minimum increase in total within-cluster variance after merging. Two different algorithms are found in the literature for Ward clustering. The one used by option "ward.D" (equivalent to the only Ward option "ward" in R versions <= 3.0.3) does not implement Ward's (1963) clustering criterion, whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2013). With the latter, the dissimilarities are squared before cluster updating.
          # fviz_nbclust(x, pam, method = "gap") # to visualize the results
          
          #res$All.index
          #res$All.CriticalValues
          x2 =  as.data.frame(t(res$Best.nc))
          x4 = (table(x2$Number_clusters))
          NumCL =  as.numeric(names(which.max(x4))) # "R"
          
          #........................................................................................
          # (30) Spectral persistence
          #........................................................................................
          BP = as.numeric(res$Best.partition)
          BPrle <- rle(BP)# need to find how many seconds each cluster "persisted" for
          # average duration of the clusters which persist for longer than one frame, so don't count 1s!
          SP2= mean(BPrle$lengths[BPrle$lengths>1])
          
          
          # what is the peak frequency, average broadband SPL, and average duration for each cluster
          cluster = res$Best.partition
          tempCL  = cbind(dayMatrixlim,cluster) # adds cluster number to matrix
          tempCL$max  = apply(tempCL[,1:fbinMax], 1, which.max) # add max SPL column # to the matrix
          tempCL$pkFq = round(as.numeric ( gsub("H","", colnames(tempCL[tempCL$max] ) ) )) # converts column number to frequency band
          tempCL$spl =  10*log10 (rowMeans( (10^(tempCL[,1:fbinMax]/10)) ) )
          cls = unique(res$Best.partition) #unique clusters in this data set
          CLdets = NULL
          for (clls in 1:4) {
            tmpCL = tempCL[tempCL$cluster == cls[clls],]
            tmpCLdur = (BPrle$lengths[BPrle$values == cls[clls] ] )
            
            Cldur = mean(tmpCLdur,rm.na = T) # mean duration of each cluster
            #Cldur = mean(tmpCLdur[tmpCLdur>1]) # mean duration of each cluster
            tmpCLpk = table(tmpCL$pkFq)
            CLpk = as.numeric( rownames( as.data.frame(which.max(tmpCLpk))[1] ) ) # peak frequency
            if (is.na( tmpCL[1,1])  == T ){ CLpk = NA}
            CLspl = median(tmpCL$spl)
            
            CLdets = cbind(CLdets, cbind(Cldur, CLpk, CLspl) )
            rm(tmpCL,tmpCLpk, tmpCLdur,Cldur,CLpk, CLspl)
          }
          colnames(CLdets) = c("CL1dur", "CL1pk", "CL1Leq","CL2dur", "CL2pk", "CL2Leq","CL3dur", "CL3pk", "CL3Leq","CL4dur", "CL4pk", "CL4Leq")
          rm(tempCL,cls)
          
          #........................................................................................
          
          #END  ACOUSTIC INDEX CALCULATIONS
          #........................................................................................
          #APPEND to a datasheet with first minute of chunk...
          Output = rbind(Output, cbind(mySites[ss], siteDays[dd], 
                                       workingMatrix[1,1:6], Timestep, dim(workingMatrix)[1],
                                       ACIout, BKdB_low, BKdBA_low, BKdB_bird, BKdBA_bird,
                                       avgAMP, L10AMP, Hf, Ht, EI, BioPh, AthPh, SoundScapeI, Bio_anth,
                                       AA, AAc, AAdur, AAanth, AAcanth, AAduranth, 
                                       Rough, ADI_step, Eveness_step,
                                       pk, pkd, pks, Hm, HvPres, HvSPL, dif_L10L90, Mamp, NumCL, SP2,CLdets ))
          
          BKADI = rbind(BKADI,BKBirddB) # rbind(BKADI,cbind(workingMatrix[1,1:6],BKBirddB))
          
          #colnames (site, Day, YY, MM, DD, HH, MM, SS, timestep, #Sec, 
          #ACI, BKlow_dB, BKlow_dBA, BKbird_dB, BKbird_dBA, 
          #avg_nom, L10_norm, SpectralEntropy, TemporalEntropy, EntropyIndex, Biophony, Anthrophony, SoundScapeIndex, RatioSoundScapeIndex,
          # AcousticActivity, AcousticEvents, DurAcousticEvents, Roughness, AcousticDiversity, Eveness)
          #........................................................................................
          #some clean up
          #rm(ACIout, BKdB_low,BKdBA_low, BKdB_bird, BKdBA_bird,BKBirddB)
          
        } # else {cat(i," hr ", j, " min NO DATA",'\n',sep="") } 
        
      } # END of minute loop
    }  # END of hour loop
    
    
    ## append all TIMESTEP data for each day together
    Output$AR = (rank(Output$Mamp) * rank(Output$Ht))/(length(Output)^2)
    acis = rbind(acis, Output)
    
    #........................................................................................
    ## RUN acoustic indicies on each DAY
    #........................................................................................
    
    dayMatrixlim1 =  NULL
    BKdayMatrixlim1 = NULL
    BKdayMatrixlimA1 = NULL
    dayMatrixlimA1 = NULL
    
    #TRUNCATE matrix in frequency bands of interest
    
    # !!!!! UNCOMMENT STOP HERE FOR DAY CALCULATIONS !!!!!!!
    
    # dayMatrixlim1 =   dayMatrix[which( colnames(dayMatrix) ==fminimum ): which( colnames(dayMatrix) ==fmaximum) ]
    # BKdayMatrixlim1 = dayMatrix[which( colnames(dayMatrix) == BKfminimum ): which( colnames(dayMatrix) == BKfmaximum) ]
    # BKdayMatrixlimA1 =dayMatrix33A[which( colnames(dayMatrix33A) == BKfminimum ): which( colnames(dayMatrix33A) == BKfmaximum)]
    # dayMatrixlimA1 =  dayMatrix33A[which( colnames(dayMatrix33A) == fminimum ): which( colnames(dayMatrix33A) == fmaximum) ]
    # #Number of frequency bands
    # nFQ = as.numeric( dim(dayMatrixlim1) [2] )
    # nFQbk = as.numeric( dim(BKdayMatrixlim1) [2] )
    # #Number of seconds
    # fileDurDay = as.numeric( dim(dayMatrixlim1) [1] )
    # 
    # #........................................................................................
    # # CALCULATE per-frequency metrics- need in other indices (NOT SAVED!)
    # #........................................................................................
    # #(1) simple average backtound noise estimate for each 1/3 octave band
    # BKBirddBD <- 10*log10( colMeans( 10^(dayMatrixlim1/10) ) )
    # 
    # # (2) Background noise from #2 in Towsey, modified here as a level for each frequency band, not normalized!
    # BK_TowseyD <- NULL
    # for (ff in 1:length(dayMatrixlim1) ) { #loop through each frequency band 
    #   #(1) compute a histogram of lowest dB values, with a length equal to 1/8th of total values
    #   tmp <- sort ( dayMatrixlim1[,ff]) [ 1:( dim(dayMatrixlim1)[1] /8) ]
    #   #(2) smooth the histogram with moving average- did not do!!!
    #   #!!!!! check this: filter(tmp,rep(1,5),method = "convolution")
    #   #(3) find bin with the maximum count
    #   tmp1 <- c(table(tmp)) #count for each dB bin
    #   tmpval <- as.numeric( (tmp1)) #counts for each bin as, numeric
    #   tmpnam <- as.numeric(names(tmp1)) #dB value for each bin, numeric
    #   tmpc <- as.matrix( cbind(tmpnam,tmpval))
    #   colmax <- as.numeric(which.max(tmpval))
    #   #(4) accumulating counts in histogram bins below (3) until 68% of counts below (3) is reached
    #   cutoff = sum( tmpval[ 1:colmax-1] ) * .68 #value to stop the summation at
    #   cntbk  = 0
    #   for (h in 1:length(tmpval[1:colmax])-1 )
    #   {
    #     if (cntbk > cutoff) {
    #       break }
    #     loc = colmax - h #find the location to start the summation
    #     cntbk = cntbk + tmpval[loc]
    #     stploc = tmpnam[loc-1]
    #   }
    #   # (5) final calc: (3) + N(4)
    #   # got an error when the first bin had the max # of values..... so just  use the min value
    #   if (cutoff == 0) {BK_TowseyD[ff] = tmpnam[colmax] } else { BK_TowseyD[ff] = tmpnam[colmax] + stploc*0.1 }
    #   rm(tmp1,tmpval,tmpnam,tmpc,colmax,cutoff)
    # }
    # 
    # 
    # BK_TowseyDN <- NULL
    # for (ff in 1:length(BKdayMatrixlim1) ) { #loop through each frequency band 
    #   #(1) compute a histogram of lowest dB values, with a length equal to 1/8th of total values
    #   tmp <- sort ( BKdayMatrixlim1[,ff]) [ 1:( dim(BKdayMatrixlim1)[1] /8) ]
    #   #(2) smooth the histogram with moving average- did not do!!!
    #   #!!!!! check this: filter(tmp,rep(1,5),method = "convolution")
    #   #(3) find bin with the maximum count
    #   tmp1 <- c(table(tmp)) #count for each dB bin
    #   tmpval <- as.numeric( (tmp1)) #counts for each bin as, numeric
    #   tmpnam <- as.numeric(names(tmp1)) #dB value for each bin, numeric
    #   tmpc <- as.matrix( cbind(tmpnam,tmpval))
    #   colmax <- as.numeric(which.max(tmpval))
    #   #(4) accumulating counts in histogram bins below (3) until 68% of counts below (3) is reached
    #   cutoff = sum( tmpval[ 1:colmax-1] ) * .68 #value to stop the summation at
    #   cntbk  = 0
    #   for (h in 1:length(tmpval[1:colmax])-1 ){        
    #     if (cntbk > cutoff) {
    #       break }
    #     loc = colmax - h #find the location to start the summation
    #     cntbk = cntbk + tmpval[loc]
    #     stploc = tmpnam[loc-1]       }
    #   # (5) final calc: (3) + N(4)
    #   # got an error when the first bin had the max # of values..... so just  use the min value
    #   if (cutoff == 0) {BK_TowseyDN[ff] = tmpnam[colmax] } else { BK_TowseyDN[ff] = tmpnam[colmax] + stploc*0.1 }
    #   rm(tmp1,tmpval,tmpnam,tmpc,colmax,cutoff)
    # }
    # 
    # #........................................................................................
    # # CALCUALTE ACOUSTIC INDICES
    # #........................................................................................
    # ## (1) CALCULATE ACI
    # t1 <- array(0, dim(dayMatrixlim1) - c(1,0))
    # for (frq2 in 1:(dim(t1)[2])) # loop through frequency bands
    # {    t1[,frq2] <- abs( diff(dayMatrixlim1[,frq2]) )/ sum(dayMatrixlim1[,frq2])    }
    # ACIDay <- sum(t1)
    # rm(t1)
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (2,3) run Acoustic Diversity Index and evenness for each whole day
    # # need to do for each FQ band!
    # if (length(BKADI) != 0 ) {
    #   L90 = NULL
    #   for (bb in 1:dim(BKADI)[2]) {
    #     bkn = ecdf(BKADI[,bb])
    #     bknL90 = quantile(bkn, .1) # L90 for each frequency band of interest- over what time period
    #     bknL50 = quantile(bkn, .5)
    #     bknL10 = quantile(bkn, .9)
    #     #plot(bkn)
    #     #abline(h=.1)
    #     L90[bb] = bknL90
    #   }
    # }
    # 
    # Score = NULL
    # for (f in 1:length(dayMatrixlim1) )
    # {
    #   # dataIN = dayMatrixlim1[,f]
    #   # hist(dataIN)
    #   Score[f] = ( length( dayMatrixlim1[,f] [dayMatrixlim1[,f] > L90[f]+3] ) )/ length (dayMatrixlim1[,f])
    # }
    # ADIDay = diversity(Score, index = "shannon") #acoustic diversity
    # AEIDay = Gini(Score) #acoustic evenness
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (4,5,6,7) Calculate background noise over whole day- over low frequency
    # BK1 <- 10^(BKdayMatrixlim1/10) # convert matrix to pressure (un-log)
    # # calculate total daily sound energy- multiple methods!
    # #BKdBDaily_low <- 10*log10( sum(BK1)/dim(BK1)[1] )   #average pressures, then convert
    # BKmedian_low =  10*log10( median(rowSums(BK1)) )    #median value of pressures, then convert
    # BKmean_low   =  10*log10( mean(rowSums(BK1))   )    #mean of pressures, then convert
    # 
    # #aweight
    # BK1A <- 10^(BKdayMatrixlimA1/10) # convert matrix to pressure (un-log)
    # # calculate total daily sound energy- multiple methods!
    # BKmedian_lowA <-  10*log10( median(rowSums(BK1A)) )    #median value of pressures, then convert
    # BKmean_lowA <-    10*log10( mean(rowSums(BK1A))   )    #mean of pressures, then convert
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (8,9) Average signal amplitude- #1 in Towsey et al., 2013
    # #modified as the mean dBA value for the timestep, normalized by the min/max (set at: -10 to 80 dB)
    # avgAMPD = normdBA(mean (dayMatrix$dbA) )
    # L10AMPD = normdBA(quantile (dayMatrix$dbA, .1) )
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (10,11,12) Spectal Entrophy (#10 in Towsey), Temporal entropy (#7 in Towsey), Entropy Index
    # Pres = colMeans(10^(dayMatrixlim1/10))  #Leq for each Fq band over time period (mean of the pressures)
    # Pres2 = Pres/sum(Pres)
    # HfDay = -sum( (Pres2 * log(Pres2))) /log(nFQ)
    # 
    # Leqt = ( rowMeans( (10^(dayMatrixlim1/10)) ) ) #Leq for each second over entire band (could also used dBA)
    # Leqt2 = Leqt/sum(Leqt)
    # HtDay = -sum( (Leqt2 * log(Leqt2)) / log(fileDurDay) ) 
    # 
    # EIDay = HtDay * HfDay
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (13,14,15,16) Biophony, Anthrophony, normalised difference in soundscape, ratio of A/B
    # BioPhDay = sum( colMeans(10^(dayMatrixlim1/10)) )
    # AthPhDay = sum( colMeans(10^(BKdayMatrixlim1/10)) )
    # SoundScapeIDay = (BioPhDay - AthPhDay ) / (BioPhDay + AthPhDay )
    # Bio_anthDay = (BioPhDay /AthPhDay ) 
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (15,16,17) acoustic activity, count of acoustic events, duration of acoustic events for biological band
    # AAday = NULL
    # AAcday = NULL
    # s2nday <- NULL
    # AAdurDay = NULL
    # for (f in 1:length(dayMatrixlim1) ) {
    #   AAday[f] =  length( dayMatrixlim1[,f][ dayMatrixlim1[,f] > BK_TowseyD[f]+3 ] )/ length (dayMatrixlim1[,f])
    #   AAcday[f] = length( dayMatrixlim1[,f][ dayMatrixlim1[,f] > BK_TowseyD[f]+3 ] )
    #   s2nday[f] = max((dayMatrixlim1[,f])) - BK_TowseyD[f]
    # }
    # AAday= sum(AAday)
    # AAcday = sum(AAcday)
    # 
    # for (f in 1:length(dayMatrixlim1) ) {
    #   #logical matrix...
    #   temp <- dayMatrixlim1[,f] > BK_TowseyD[f]+3 
    #   #table(temp)["TRUE"] ; table(temp)["FALSE"]
    #   temp2 <- temp*1 # convert to 0/1
    #   #find consecutive 1 values...
    #   rl <- rle(temp2)
    #   len = rl$lengths
    #   v =  rl$value
    #   if (length(v) == 1 )
    #   {
    #     if   (v == 0) {AAdurDay[f] = 0 }
    #     else {AAdurDay[f] = len }
    #     next
    #   }
    #   cumsum = NULL
    #   cntAA = 0
    #   for ( qq in  seq(from = 2, to = length(v), by =2) ) 
    #   {
    #     cntAA = cntAA +1
    #     cumsum[cntAA] = len[qq]
    #   }
    #   AAdurDay[f] = mean(cumsum)
    # }
    # AAdurDay = median(AAdurDay)
    # 
    # #(15,16,17) acoustic activity, count of acoustic events, duration of acoustic events for Noise band
    # AAdayN = NULL
    # AAcdayN = NULL
    # s2ndayN <- NULL
    # AAdurDayN = NULL
    # 
    # for (f in 1:length(BKdayMatrixlim1) ) {
    #   AAdayN[f] =  length( BKdayMatrixlim1[,f][ BKdayMatrixlim1[,f] > BK_TowseyDN[f]+3 ] )/ length (BKdayMatrixlim1[,f])
    #   AAcdayN[f] = length( BKdayMatrixlim1[,f][ BKdayMatrixlim1[,f] > BK_TowseyDN[f]+3 ] )
    #   s2ndayN[f] = max((BKdayMatrixlim1[,f])) - BK_TowseyDN[f]
    # }
    # AAdayN= sum(AAdayN)
    # AAcdayN = sum(AAcdayN)
    # 
    # for (f in 1:length(BKdayMatrixlim1) ) {
    #   #logical matrix...
    #   temp <- BKdayMatrixlim1[,f] > BK_TowseyDN[f]+3 
    #   #table(temp)["TRUE"] ; table(temp)["FALSE"]
    #   temp2 <- temp*1 # convert to 0/1
    #   #find consecutive 1 values...
    #   rl <- rle(temp2)
    #   len = rl$lengths
    #   v =  rl$value
    #   if (length(v) == 1 )
    #   {
    #     if   (v == 0) {AAdurDayN[f] = 0 }
    #     else {AAdurDayN[f] = len }
    #     next
    #   }
    #   cumsum = NULL
    #   cntAA = 0
    #   for ( qq in  seq(from = 2, to = length(v), by =2) ) 
    #   {
    #     cntAA = cntAA +1
    #     cumsum[cntAA] = len[qq]
    #   }
    #   AAdurDayN[f] = mean(cumsum)
    #   rm(temp,temp2,rl,len,v)
    # }
    # AAdurDayN = median(AAdurDayN)
    # 
    # #check
    # c(AAday, AAcday, AAdurDay, AAdayN, AAcdayN, AAdurDayN)
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (18) Roughness
    # RoughDay = NULL
    # for (f in 1:length(dayMatrixlim1) ){
    #   x = dayMatrixlim1[,f]
    #   x <- x/max(x)
    #   deriv2 <- diff(x, 1, 2)
    #   RoughDay[f] <- sum(deriv2^2, na.rm = TRUE)
    # }  
    # RoughDay = median(RoughDay)
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (19,20) Acoustic diversity, acoustic eveness
    # ScoreDay = NULL
    # for (f in 1:length(dayMatrixlim1) ) 
    # {
    #   ScoreDay[f] = length( dayMatrixlim1[,f][dayMatrixlim1[,f] > BK_TowseyD[f]+3] )/ length (dayMatrixlim1[,f])
    # }
    # ADI_v2 = diversity(ScoreDay, index = "shannon")
    # Eveness_v2 = Gini(ScoreDay)
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (21,22,23,24,25,26) frequency with max amplitude, total count of max for each freq band (normalized by total bands),
    # # kurtosis, skewness, entrophy of spectral max, entrophy of spectral variance)
    # # peak frequency
    # peakfDay = NULL
    # for (j in 1:dim(dayMatrixlim1)[1] ) 
    # {   peakfDay[j] = (which.max( dayMatrixlim1[j,] ) )  }
    # pk2day = matrix(0,1,dim(dayMatrixlim1)[2])
    # for (uu in 1:nFQ)  { pk2day[uu] = sum(peakfDay == uu)  }
    # colnames(pk2day) = colnames(dayMatrixlim1)
    # pk2norDay = pk2day/fileDurDay
    # pkday = as.numeric ( gsub("H","",colnames(dayMatrixlim1[which.max(pk2day)]) ) )
    # 
    # # kurtosis- is the shape of peak frequencies normally distributed (kurtosis) ?
    # kurDay = kurtosis( as.vector(pk2norDay) )
    # 
    # # skewness- What is the skewness is this distribution? (symmetrical = 0, right skew = +)
    # skewDay = skewness(as.vector(pk2norDay))
    # 
    # # entropy of Spectral Maxima
    # pk2day[pk2day == 0] <- 1e-07
    # pk_probDay = pk2day/(sum(pk2day)) # normalize
    # HmDay = -sum( (pk_probDay * log2(pk_probDay) ) )/log2(nFQ)
    # 
    # # entropy of spectral variance- pressure and dB
    # Press = (10^(dayMatrixlim1/10)) #conver back to pressure
    # Pv = NULL
    # for (v in 1:dim(Press)[2] ) {Pv[v] = var(Press[,v]) }
    # Pv2 = Pv/sum(Pv)
    # HvPresDay = -sum( (Pv2 * log2(Pv2)) ) / log2(nFQ)
    # 
    # Pv = NULL
    # for (v in 1:dim(dayMatrixlim1)[2] ) {Pv[v] = var(dayMatrixlim1[,v]) }
    # Pv2 = Pv/sum(Pv)
    # HvSPLDay = -sum( (Pv2 * log2(Pv2)) ) / log2(nFQ)
    # #........................................................................................
    # 
    # #........................................................................................
    # ## (27) Normalize exceedence levels for dBA
    # Exceed_normD    = normdBA ( quantile(dayMatrix$dbA, c(0.05, 0.1, 0.5, 0.9, 0.95),na.rm = TRUE) )
    # dif_L10L90Day = Exceed_normD[4]-Exceed_normD[2]
    # #........................................................................................
    # 
    # #........................................................................................
    # ## ACOUSTIC RICHNESS   
    # Mamp1 = quantile( 10*log10( ( rowMeans( (10^(dayMatrixlim1/10)) ) ) ), 0.5)
    # 
    # #........................................................................................
    # ## (29) Spectral diversity (Sd)- determine optimal number of clusters for the timestep and how confident/explained
    # #!!!!!UNDER CONSTRUCTION!!!!!!!!!!!
    # # http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
    # # http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
    # 
    # x1 = scale(dayMatrixlim1)  # To standarize the variables (dim(x1)[1])/(60*Timestep)
    # 
    # # OPTION 1: use all algothrums to find optimal cluster... takes way to long!
    # #res = NbClust(x, distance = "euclidean", min.nc=2, max.nc=(dim(x)[1])/60, method = "ward.D", index = "all")
    # # OPTION 2: use "gap" method (Tibshirani et al. 2001) Smallest n_{c} such that criticalValue >= 0
    # #ptm <- proc.time()
    # res1 = NbClust(x1, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "silhouette")
    # 
    # #proc.time() - ptm
    # # DISTANCE: Euclidean distance : Usual square distance between the two vectors (2 norm).  d(x,y)= (sum_{j=1}^{d} (x_j - y_j)^2)^(1/2)
    # # METHOD: Kmeans : This method is said to be a reallocation method. Here is the general principle
    # #1) Select as many points as the number of desired clusters to create initial centers.
    # #2) Each observation is then associated with the nearest center to create temporary clusters.
    # #3) The gravity centers of each temporary cluster is calculated and these become the new clusters centers.
    # #4) Each observation is reallocated to the cluster which has the closest center.
    # #5) This procedure is iterated until convergence.
    # # INDEX: "silhouette" or "all" or "alllong"	 Maximum value of the index     (Rousseeuw 1987)
    # 
    # # fviz_nbclust(x1, kmeans, method = "silhouette") # to visualize the results
    # # res1$All.index #the gap statistic values of the partitions obtained with number of clusters varying from min.nc to max.nc ($All.index)
    # # res1$All.CriticalValues
    # 
    # x21 =  as.data.frame(t(res1$Best.nc))
    # x41 = (table(x21$Number_clusters))
    # NumCL1 =  as.numeric(names(which.max(x41))) # "R"
    # 
    # #........................................................................................
    # # (30) Spectral persistence
    # #........................................................................................
    # BP1 = as.numeric(res1$Best.partition)
    # BPrle1 <- rle(BP1)# need to find how many seconds each cluster "persisted" for
    # # average duration of the clusters which persist for longer than one frame, so don't count 1s!
    # SP21 = mean(BPrle1$lengths[BPrle1$lengths>1])
    # #SP2u= quantile(BPrle$lengths[BPrle$lengths>1], c(.25,.5, .75))
    # #max(BPrle$lengths[BPrle$lengths>1])
    # 
    # # what is the peak frequency, average broadband SPL, and average duration for each cluster
    # cluster1 = res1$Best.partition
    # tempCL  = cbind(dayMatrixlim1,cluster1) # adds cluster number to matrix
    # tempCL$max  = apply(tempCL[,1:9], 1, which.max) # add max SPL column # to the matrix
    # tempCL$pkFq = round(as.numeric ( gsub("H","", colnames(tempCL[tempCL$max] ) ) )) # converts column number to frequency band
    # tempCL$spl =  10*log10 (rowMeans( (10^(tempCL[,1:9]/10)) ) )
    # cls = unique(res1$Best.partition) #unique clusters in this data set
    # 
    # CLdets1 = NULL
    # for (clls in 1:4) {
    #   tmpCL = tempCL[tempCL$cluster1 == cls[clls],]
    #   tmpCLdur = (BPrle$lengths[BPrle$values == cls[clls] ] )
    #   Cldur = mean(tmpCLdur,rm.na=TRUE) # mean duration of each cluster
    #   #Cldur = mean(tmpCLdur[tmpCLdur>1]) # mean duration of each cluster
    #   tmpCLpk = table(tmpCL$pkFq)
    #   CLpk = as.numeric( rownames( as.data.frame(which.max(tmpCLpk))[1] ) ) # peak frequency
    #   if (is.na( tmpCL[1,1])  == T ){ CLpk = NA}
    #   CLspl = median(tmpCL$spl)
    #   
    #   CLdets1 = cbind(CLdets1, cbind(Cldur, CLpk, CLspl) )
    #   rm(tmpCL,tmpCLpk, tmpCLdur,Cldur,CLpk, CLspl)
    # }
    # colnames(CLdets1) = c("CL1dur", "CL1pk", "CL1Leq","CL2dur", "CL2pk", "CL2Leq","CL3dur", "CL3pk", "CL3Leq","CL4dur", "CL4pk", "CL4Leq")
    # rm(cluster1,tempCL,cls)
    # 
    # 
    # #........................................................................................
    # # ()format for adding more daily indicies
    # #see full list in above section
    # #........................................................................................
    # 
    # #........................................................................................
    # ## COMBINE ALL DAILY METRICS TOGETHER FOR A GIVEN DAY... ACI, ADI, etc
    # # Output1 <- rbind(Output1, cbind(mySites[ss], siteDays[dd], "day", 
    # #                                 ACIDay, ADI, AEI, BKmean_low, BKmedian_low, BKmedian_lowA, BKmean_lowA) )
    # Output1 <- rbind(Output1, cbind(mySites[ss], siteDays[dd], "day", 
    #                                 ACIDay, ADIDay, AEIDay, BKmean_low, BKmedian_low, BKmedian_lowA, BKmean_lowA,
    #                                 avgAMPD, L10AMPD, HfDay, HtDay, EIDay, BioPhDay, AthPhDay, SoundScapeIDay, Bio_anthDay,
    #                                 AAday, AAcday, AAdurDay, AAdayN, AAcdayN, AAdurDayN, RoughDay, ADI_v2, Eveness_v2,
    #                                 pkday, kurDay, skewDay, HmDay, HvPresDay, HvSPLDay, dif_L10L90Day,Mamp1,NumCL1,SP21,CLdets1) )
    # 
    # acisD = rbind(acisD, Output1)
    # UNCOMMENT STOP HERE FOR DAY CALCULATIONS!
    
    #rm( ACIDay, ADI, AEI, BKmean_low, BKmedian_low, BKmedian_lowA, BKmean_lowA)
    #........................................................................................
    
  } # END of loop for DAYS
  
  acisD = as.data.frame(acisD)
  acisD$AR = (rank(acisD$Mamp1)* rank(acisD$HtDay))/(length(acisD)^2)
  
  OUTPUT_sites =  rbind(OUTPUT_sites, acis)
  OUTPUT_sitesD = rbind(OUTPUT_sitesD, acisD)
  
  ## WRITE OUT DATA- for each site!
  colnames(OUTPUT_sites)[2]<-"Date"
  colnames(OUTPUT_sites)[1]<-"Site"
  colnames(OUTPUT_sites)[10]<-"SampleLength_sec"
  
  #OUTPUT_sitesD = as.matrix(OUTPUT_sitesD)
  #colnames(OUTPUT_sitesD)[1]<-"Site"
  #colnames(OUTPUT_sitesD)[2]<-"Date"
  #colnames(OUTPUT_sitesD)[3]<-"Timestep" #do we really need this column?
  
  dstmp = Sys.Date()
  if (savFile == 1) {
    # TIMESTEP DATA
    outFileName <- paste0(workingDir, "\\", mySites[ss], "_",Timestep, "mins", "_NVSPL_AcousticIndex_Created", dstmp, ".csv")
    write.csv(OUTPUT_sites, file=outFileName, na ="NaN", quote=F, row.names=F)
    
    # DAILY DATA
    #outFileNameD <- paste0(workingDir, "\\\\", mySites[ss], "_Daily_NVSPL_AcousticIndex_Created", dstmp, ".csv")
    #write.csv(OUTPUT_sitesD, file=outFileNameD, na ="NaN", quote=F, row.names=F)
  }
  
} # END of loop for SITES


#___________________________________________________________________________________________________
## INITIAL PLOTTING
#___________________________________________________________________________________________________
if (pltdsate == 1) {
  
  
  OUTPUT_sitesD[,5:10] = (as.numeric(as.character(OUTPUT_sitesD[,5:10])))
  summary(OUTPUT_sites[,11:21])
  library(psych)
  corPlot(OUTPUT_sites[,11:21])  
  pairs.panels(OUTPUT_sites[,11:21])   #draw a matrix of scatter plots for the first 5 variables
  multi.hist(OUTPUT_sites[,11:21])
  pairs(OUTPUT_sites[,11:21])
  
}

if (pltdsate == 1) {

  # comparison of effects on diel patterns
  par(mfrow=c(2,3))
  
  data10 =  OUTPUT_sites[ ( OUTPUT_sites$Site == 10 | OUTPUT_sites$Site == 20 ), ]
  unique(data10$Site)
  boxplot( data10$ACIout ~ data10$Hr , 
           main ="No Rough, Yes prescribe (10,20)", xlab="Hour",
           ylab ='Acoustic Complexity Index',
           ylim=c(0, 1) ) 
  
  data10 = OUTPUT_sites[ (OUTPUT_sites$Site == 40 | OUTPUT_sites$Site == 50 ), ]
  unique(data10$Site)
  boxplot( data10$ACIout ~ data10$Hr , 
           main ="No Rough, No prescribe (2nd Generation) (40,50)", xlab="Hour",
           ylab ='Acoustic Complexity Index', ylim=c(0, 1.5) ) 
  
  data10 = OUTPUT_sites[ (OUTPUT_sites$Site == 30 ), ]
  unique(data10$Site)
  boxplot( data10$ACIout ~ data10$Hr , 
           main ="No Rough, No prescribe (Old Growth) (30)", xlab="Hour",
           ylab ='Acoustic Complexity Index', ylim=c(0, 1.5) ) 
  
  data10 =  OUTPUT_sites[ (OUTPUT_sites$Site == 80 | OUTPUT_sites$Site == 90 ), ]
  unique(data10$Site)
  data10 =  data10[ (data10$Hr >= 4 & data10$Hr <= 20 ), ]
  boxplot( data10$ACIout ~ data10$Hr , 
           main ="Yes Rough, Yes prescribe (80,90)", xlab="Hour",
           ylab ='Acoustic Complexity Index', ylim=c(0, 1.5) )
  
  data10 =  OUTPUT_sites[ (OUTPUT_sites$Site == 60 | OUTPUT_sites$Site == 70 ), ]
  unique(data10$Site)
  boxplot( data10$ACIout ~ data10$Hr , 
           main ="Yes Rough, No prescribe (60,70)", xlab="Hour",
           ylab ='Acoustic Complexity Index',ylim=c(0, 1.5) ) 
  

  
  # comparison of effects on seasonal patterns- ACI
  par(mfrow=c(2,3))
  dataD =  OUTPUT_sitesD[ ( OUTPUT_sitesD[,1] == 10 | OUTPUT_sitesD[,1] == 20 ), ]
  unique(dataD[,1])
  boxplot( as.numeric(dataD[,4]) ~ dataD[,2], 
           main ="No Rough, Yes prescribe (10,20)",
           ylab ='Acoustic Complexity Index',
           ylim=c(0, 1) ) 
  
  dataD =  OUTPUT_sitesD[ (OUTPUT_sitesD[,1] == 40  | OUTPUT_sitesD[,1] == 50  ), ]
  unique(dataD[,1])
  boxplot( as.numeric(dataD[,4]) ~ dataD[,2], 
           main ="No Rough, No prescribe (2nd Generation) (40,50)",
           ylab ='Acoustic Complexity Index',
           ylim=c(0, 1) ) 
  
  dataD =  OUTPUT_sitesD[ ( OUTPUT_sitesD[,1] == 30 ), ]
  unique(dataD[,1])
  boxplot( as.numeric(dataD[,4]) ~ dataD[,2], 
           main ="No Rough, No prescribe (Old Growth) (30)",
           ylab ='Acoustic Complexity Index',
           ylim=c(0, 1) ) 
  
  dataD =  OUTPUT_sitesD[ ( OUTPUT_sitesD[,1] == 80 | OUTPUT_sitesD[,1] == 90 ), ]
  unique(dataD[,1])
  dataD =  rbind( dataD[1:114,],dataD[123:nrow(dataD),])
  boxplot( as.numeric(dataD[,4]) ~ dataD[,2], 
           main ="Yes Rough, Yes prescribe (80,90)",
           ylab ='Acoustic Complexity Index',
           ylim=c(0, 1) ) 
  
  dataD =  OUTPUT_sitesD[ ( OUTPUT_sitesD[,1] == 60 | OUTPUT_sitesD[,1] == 70 ), ]
  unique(dataD[,1])
  boxplot( as.numeric(dataD[,4]) ~ dataD[,2], 
           main ="Yes Rough, No prescribe (60,70)",
           ylab ='Acoustic Complexity Index',
           ylim=c(0, 1) ) 
  #comparison of background noise levels
  boxplot( as.numeric(OUTPUT_sites$BKdBA_low) ~ OUTPUT_sites$Site, 
           #main ="Yes Rough, No prescribe (60,70)",
           ylab ='Background Noise Level (dBA 31.5-1250 Hz',
           ylim=c(0, 1) ) 
  
}
