#CALCULATE ACI for a directory of wav files....NOT WORKING

# uses PAMGUIDE to calculate a spectrogram at a specifcied resolution 
# and then calculates ACI over a specific frequency for a given day

#V4- includes a selection of timeperiod to calculate ACI over 
#*adapted from calculate_ACI_NVSPL_subsetbyminV2

#V5- updates to remove write out, read back in of PAMGuide file and subsample by minutes

rm(list=ls())
#___________________________________________________________________________________________________
#LOAD LIBRARIES
library(tuneR)
library(seewave)
library(ineq)
library(fftw)
library(soundecology)

#___________________________________________________________________________________________________
# ANALYSIS SET-UP (CHECK THESE SETTINGS BEFORE RUNNING!!!)
#___________________________________________________________________________________________________
# ANALYSIS PARAMS.....
Fs = 48000 #sampling frequency
winlen = 512 #window length
timeRes = winlen/Fs # Time resolution (sec)
frewRes = Fs/winlen #Frequency Resolution (Hz)

fminimum<-1.413 #lower limit in kHz
fmaximum<-11.220 #upper limit in kHz
flim = c(fminimum,fmaximum)
DayIndex="NotDay" #Do you want to calculate ACI for the entire day? #"NotDay" = No, "Day" = Yes 
Timestep=10 #set how how many minutes to calculate ACI over? #PICK NUMBER FROM 1:60
filext = "%Y%m%d_%H%M%S.wav"

# DIRECTORY OF FILES....
WAVDirsDIR <- '//Users//maryclapp//Desktop//Research//data//ptct//TrimmedFiles_2014' #choose.dir()
#WAVFilesDIR <- 'I:\\NPS_DATA\\SEKI_fire\\Sampling_V3' #choose.dir()
WAVDirs <- list.dirs(WAVDirsDIR)
#WAVDirs <- WAVDirs[grep("AUDIO", WAVDirs)]

# CALIBRATION PARAMS
# SM2 = mh=-36, G=xx, vADC=1.414
# SM3 = mh=-36, G=24+XX, vADC=1
mhset = -36
Gset = 36
vADCset = 1
enviset = "Air"

#___________________________________________________________________________________________________
# CREATE A LOOP THROUGH ALL DIRECTORIES
#___________________________________________________________________________________________________
aciDayALL = NULL

for (ff in 1:length(WAVDirs))
{
  #for testing
  # ff = 1
  # site = 10
  # get site name
  WAVFiles = list.files(WAVDirs[1], pattern = "*.wav") 
  dys = unique(gsub(".+_(\\d{8})_(.+).wav","\\1",WAVFiles) )
  
  ## sets the file names
  s1 = unlist (strsplit( WAVFiles[1], '_') ) [1] 
  site = unlist (strsplit( s1, '_') )[1] 
  filename = paste(site, filext, sep="_")

  
  # run PAMGUIDE
  setwd('//Users//maryclapp//Desktop//Research//code//PAMGuideSEKI_NVSPL_Clapp') 
  source('Meta.R')
  
  myvar <- Meta(chunksize = 500, N = winlen, timestring = filename,
                r=0, outwrite=0, plottype = "None", 
                calib=1, envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
  
  #myvar is a matrix of the data: row 1 = Frequency, column 1 = timestamp
  dimc <- dim(myvar)  	
  t <- myvar[2:dimc[1],1]
  t <- as.POSIXct(t,origin="1970-01-01")
  tString <- as.character(t)
  a <- myvar[2:dimc[1],2:dimc[2]]
  f <- myvar[1,2:dimc[2]]
  tMatrix = matrix(unlist(strsplit(tString,'-|:| ')),ncol=6, byrow=T)
  rm(myvar)
  
  #add column with unique day & unique hour minute
  testDay = paste(tMatrix[,1], tMatrix[,2], tMatrix[,3], sep = "")
  testHRMin = paste(tMatrix[,4], tMatrix[,5], sep = "_")
  tMatrix = cbind(tMatrix, testDay, testHRMin)
  colnames(tMatrix) = c("Year", "Mth","Day","Hr","Min","Sec","Date",'HrMin')
  
  unDay = unique(tMatrix[,7])
  
  # find unique days to run analysis on
  uDayIndex = unDay
  acis = NULL
  
  # loop through each day and calculate ACI
  for ( dd in 1:length(uDayIndex)) 
  {
    #1- get data for a given day
    tempData = a[tMatrix[,7]== uDayIndex[dd],]
    tempTime = tMatrix[tMatrix[,7]== uDayIndex[dd],]
    
    cat('Processing day ', dd, " of ", length(uDayIndex),'\n')
    
    # 2- check to see if full minutes are included, remove any non-full minutes!
    # unHrMin = unique(tempTime[,8])
    #loop through each HourMinute for a given Day, remove any data for non-full minutes
    # subsettedMatrix = NULL 
    # for(i in 1:length(unHrMin) )
    # {
    #   test = dim(tempTime[tempTime[,8] == unHrMin[i],])
    # 
    #   if ( test[1] >= (60/timeRes)  == FALSE ) 
    #   {
    #     #cat('Not a full minute, skip to next', '\n',sep="")
    #   }else {
    #     #cat('Full minute', '\n',sep="")
    #     subsettedMatrix = rbind(subsettedMatrix,subsettedMatrix2)
    #   }
    # }     rm(test)
    # 
    
    #3- truncate by frequency
    flim2 <- round( (flim*1000*winlen)/Fs )
    a2 <- tempData[, flim2[1]:flim2[2]]
    #time is still in the rows!!    #a2=t(a2)
    #data = cbind(a2,tempTime)
    
    Output<-NULL
    # 4- insert option to calculate over specified timestep
    
    if(DayIndex=="NotDay"){ # not a full day
      
      #how many Timesteps are possible? for a given day?
      #NOTE: assumes full minute of data
      uMin = unique(tempTime[,8]) 

      for(ii in seq(1, length(uMin), by =Timestep ) ){
        #what miutes do you want to grab..
        min2grab = uMin[ii:(ii + Timestep-1)]
        #select data for only those minutes
        cc = (a2[tempTime[,8] %in% min2grab,])
        ct = (tempTime[tempTime[,8] %in% min2grab,])
        
        numMin = ceiling( (nrow(cc)*timeRes)/60)
        if ( (numMin == Timestep) == TRUE ){ #only process data with full data for a given timestep...
          
          #run ACI on each subsetted time period
          t = array(0, dim(cc) - c(1,0)) #one less time step...
          for (j in 1:dim(t)[2] ) # loop through frequency bands
          { 
            t[,j] <- abs( diff(cc[,j]) ) / sum(cc[,j]) 
          }
          
          ACIout = sum(t)# ACI results for each time chunck on a given day
          Output = rbind(Output, cbind(site, ct[1,7], ct[1,8], Timestep, ACIout) ) 
          #append to a data with first minute of chunk... each day will have many lines for each chunk
        }
        
        #append all data for each day together....
        
      } 
      
      acis = rbind(acis, Output)
    }else{  
      
      #4- run ACI code on whole day 
      t <- array(0, dim(a2)-c(0,1))  
      for (i in 1:(dim(t)[1])) # loop through frequencies
      {
        t[i,] <- abs( diff(a2[i,]) )/ sum(a2[i,])
      }
      acis[dd] <- sum(t)# write our the results for a given day
    } #end of else statement
    
  } #end loop over days
  
  #aciDay = cbind(site, uDayIndex,acis)
  aciDayALL =  rbind(aciDayALL, acis)
  
} #end loop for all sites in a directory

#outFileName <- paste(workingDir, "\\", mySites,"_", siteDays[1],"_AcousticIndex.csv", sep="")
#write.csv(aciDayALL, file=outFileName, na="NaN", quote=F, row.names=F)