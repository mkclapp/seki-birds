## USES PAMGUIDE TO CALIBRATE AND CONVERT AUDIO FILES INTO NVSPL FORMAT
#--------------------------------------------------------------------------------

## NOTES: 
#V1- loops through all audio files in a directory
#V2- does one daily audio file at a time (because .csv file was getting too big!)
#V3- if multiple files for one day... will run on unique days in a directory and COMBINE
#V4- some minor update to the code based on Rachel Buxton work
#V5- change meta file (line 43), so it includes the list of dys to avoid copying the data (takes too LONG!)
#V6- checked the aweight calculation- worried it is not adding values correctly! FIXED and added version to the output
#V8- allow for a gain curve, not just one value (does have V7 modifications- and feature NOT WORKING!)

# linked to a specific PAMGUIDE directory as some have been changed

## WORKS FOR:
#WAVDirsDIR = "I:\\NPS_DATA\\SEKI_MClapp\\Data_2014_Setting1\\AMPHIT1_2014\\AUDIO" #WORKED
#WAVDirsDIR = "I:\\NPS_DATA\\SEKI_MClapp\\Data_2014_Setting1\\" #WORKED
#WAVDirsDIR = "I:\\NPS_DATA\\SEKI_MClapp\\Data_copies" #WORKED

## BEFORE RUNNING:
# LINE 30: set working directory (where audio files are)
# LINE 33/34: set file format 
# LINE 38: set directory with PAMGUIDE code
# LINES 42-47: set the calibration settings 
# check the timestring

rm(list=ls(all=TRUE)) 

##--------------------------------------------------------------------------------
## SET DIRECTORY WHERE ALL AUDIO FILES TO PROCESS ARE...

#SEKI-fire dataset
#WAVDirsDIR = "D:\\SEKI-fire\\EXAMPLE_June15"  #E:\\NPS_DATA\\SEKI_fire\\AUDIO_V3\\NEW" #choose.dir()
#WAVDirs <- list.dirs(WAVDirsDIR)
#WAVDirs <- WAVDirs[grep("Data", WAVDirs)]
#filext = "_1__%Y%m%d_%H%M%S.wav"
#filpat = ".+__1__\\d{8}_\\d{6}.wav"

# LEWI dataset
WAVDirsDIR = "E:\\NPS_DATA\\Phenology\\LEWI\\AUDIO\\LEWILEON"  #choose.dir()
WAVDirs <- list.dirs(WAVDirsDIR)
filext = "0_%Y%m%d_%H%M%S_000.wav"
filpat = ".wav"

#NPSA dataset
#WAVDirsDIR = "F:\\NRS10\\WAV"  #choose.dir()
#WAVDirs <- list.dirs(WAVDirsDIR)
#filext = "%Y%m%d_%H%M%S.wav"
#filpat = ".wav"


# DIRECTORY WITH PAMGUIDE
PAMdir = "E:\\CODE\\Rwork\\PAMGuideSEKI_NVSPL_24Oct2016"
#PAMdir = "C:\\Users\\mmckenna\\Documents\\Rwork\\PAMGuideSEKI_NVSPL_24Oct2016"

##--------------------------------------------------------------------------------
## SET CALIBRATION PARAMS
mhset = -35
Gset = 25.9
vADCset = 1
enviset = "Air"
envir = 2 #1= water, 2 = air measurements
test  = 0  #set to 1 if you want to test a file

# SM2 = mh=-36, G=xx, vADC=1.414
# SM3 = mh=-36, G=24+XX, vADC=1
# NRS = mh=-192.3 G=0, vADC=??
# olympus 901,record level 15: mh = -35, G = 25.9, 

##--------------------------------------------------------------------------------
## PART 1: TEST process one audio file to test all the settings are correct
##--------------------------------------------------------------------------------
## if you want to- check one file in each directory you plan to process

if (test == 1) { # test out a file....
  
  # get a test file....
  WAVFiles = list.files(WAVDirs[1], pattern = filpat) 
  ##find unique days....
  dys = unique(gsub(".+_(\\d{8})_(.+).wav","\\1",WAVFiles) )
  
  ## sets the file names
  s1 = unlist (strsplit( WAVFiles[1], '_') ) [1] 
  site = unlist (strsplit( s1, '_') )[1] 
  filename = paste(site, filext, sep="_")
  
  #CHANGE THE DRIVE DEPENDING ON WHERE YOU PLUG IN THE HARD DRIVE
  setwd(PAMdir) 
  source('PAMGuide.R')
  #ignore the following error: tuneR >= 1.0 has changed its Wave class definition
  
  #run the calibration.....
  #when you run this piece of code a window will pop up...
  #navigate to the folder you are working on and select the first file
  PAMGuide(chunksize = 700, atype = 'TOL', timestring = filename,
           r=0, outwrite=1, plottype = "None", 
           calib=1, envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
  
  # Evaluate the file created.....
  testFile <- list.files(WAVDirs[1], pattern = '.csv', recursive=T, full.names=T)
  basename(testFile)
 
  # combine data.....
  conk <- as.matrix( read.csv(testFile[1], colClasses="numeric",header=FALSE) )
  dimc <- dim(conk)  	
  
  # dB range.....
  a <- conk[2:dimc[1],2:dimc[2]] 
  hist(a,main = "Check to see if calibration is accurate", xlab="SPL dB")
  min(a)
  max(a)
  
  # time stamp- start and continious.....
  t <- conk[2:dimc[1],1]
  t <- as.POSIXct(t,origin="1970-01-01")
  cat('Check start time...')
  as.character(t[1])
  plot(conk[2:dimc[1],1],main = 'Is time continous?')
  
  # delete files, if okay, re-run if not.....
  file.remove(testFile)
 
  
  rm(conk,a,dimc,t, testFile, PAMGuide, s1,site, filename)
}

##--------------------------------------------------------------------------------
## PART 2: BATCH process a directory of audio files and convert them to an .csv file in PAMGuide format
##--------------------------------------------------------------------------------

for (ff in 1:length(WAVDirs)) 
## LOOP through each directory of WAV files (ff)
{
  # ff = 1 #for testing (do NOT run for loop lines)
  
  #cat('##################################################')
  #cat('Processing directory ', ff, " of ", length(WAVDirs),'\n')
  #cat('##################################################')
  
  t1=proc.time()
  
  ## LOOP through wav files of the same day...
  # (because PAMGUIDE putput is too process a full site directory;
  # and want to do a full day at a time to make NVSPLs)
  
  ## find unique days....
  WAVfiles = list.files(WAVDirs[ff], pattern = filpat ) 
  dys = unique(gsub(".+_(\\d{8})_(.+).wav","\\1",WAVfiles) )
  #dys = dys[74:90] #if code breaks partway through, use this to start loop on next file
  
  ## sets the file names
  s1 = unlist (strsplit( WAVfiles[1], '_') ) [1] 
  site = unlist (strsplit( s1, '_') )[1] 
  filename = paste(site, filext, sep="_")
  
  ## make NVSPL OUTPUT directory
  NVSPLdir <- paste(WAVDirs[ff],"NVSPL",sep="\\")
  dir.create(NVSPLdir,showWarnings=F,recursive=T)
  
  ## LOOP through the unique days- calibrate then convert to NVSPL format
  
  for(d in (dys) )
  {
    # d=dys[1] # for testing
    cat('##################################################')
    cat('Processing DIRECTORY ', ff, " of ", length(WAVDirs), " for DAY", d, ' of ', length(dys), '\n' )
    cat('##################################################')
    
    ## (1) GET a list files for each day-------------------------------------------
    udaylist = grep(d, WAVfiles, value=T)
    filenms = paste(WAVDirs[ff], "\\", udaylist, sep="")
    
    ## create a new directory
    #new.folder = paste(WAVDirs[ff], "temp",sep="\\")
    #dir.create(new.folder,showWarnings=F,recursive=T)
    
    ## copy the files to the new folder
    #file.copy(filenms, new.folder)
    #tempWAV = list.files(new.folder)
  
    ## (2) RUN PAMGUIDE-------------------------------------------------------------
    setwd(PAMdir)
    source('Meta.R')
    
    Meta(chunksize = 500, type = 'TOL', timestring = filename,
         r=0, outwrite=1, plottype = "None", calib=1, 
         envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
    
    ## (3) READ IN file created by PAMGUIDE------------------------------------------
    PAMfiles <- list.files(WAVDirs[ff], pattern = "Conk.*.csv", 
                         recursive=T, full.names=T)
    PAMfiles2 = list.files(WAVDirs[ff], pattern = "*.csv", 
                           recursive=T, full.names=T)
    #PAMdir = list.dirs(WAVDirs[ff], pattern = "Meta") 
                       
    # read in 1st PAM file
    conk <- as.matrix( read.csv(PAMfiles[1], colClasses="numeric",header=FALSE) )
    
    # dim(conk)[1]/ (60*60)
    # remove the folder with daily WAV files
    unlink(PAMfiles2)
    
    ## (4) EXTRACT PARAMS--------------------------------------------------------------
    aid <- conk[1,1]  
    tstampid <- substr(aid,1,1)		#extract time stamp identifier
    enviid <- substr(aid,2,2)			#extract in-air/underwater identifier
    calibid <- substr(aid,3,3)		#extract calibrated/uncalibrated identifier
    atypeid <- substr(aid,4,4)
    
    # assign PAMGuide variables envi, calib, atype from metadata
    if (tstampid == 1){tstamp = 1} else {tstamp = ""}
    if (enviid == 1){
      envi = 'Air'  ; pref <- 20			
    } else {envi = 'Wat' ; pref <- 1}
    if (calibid == 1){calib = 1
    } else {calib = 0}
    if (atypeid == 1){atype = 'PSD'
    } else if (atypeid == 2) {atype = 'PowerSpec'
    } else if (atypeid == 3) {atype = 'TOLf'
    } else if (atypeid == 4) {atype = 'Broadband'
    } else if (atypeid == 5) {atype = 'Waveform'}
    
    # extract DATA SPL DATA and TIMESTAMP.....
    dimc <- dim(conk)  	
    t <- conk[2:dimc[1],1]
    t <- as.POSIXct(t,origin="1970-01-01")
    tString <- as.character(t)
    a <- conk[2:dimc[1],2:dimc[2]] 
    f <- conk[1,2:dimc[2]]
    # hist(a)   max(a)   min(a)
    rm(conk)
    
    ## (5) FORMAT myOutput as NVSPL.....
    # (note: PAMguide starts at 25 Hz, so lower bands (12.5, 15.8, and 20 are always NaNs)
    NVSPLhead = c("SiteID","STime", "H12p5", "H15p8", "H20", "H25", "H31p5","H40","H50","H63","H80","H100","H125","H160","H200","H250","H315","H400","H500",
                  "H630","H800","H1000","H1250","H1600","H2000","H2500","H3150","H4000","H5000","H6300","H8000","H10000","H12500","H16000","H20000",
                  "dbA","dbC","dbF","Voltage","WindSpeed","WindDir","TempIns","TempOut","Humidity",
                  "INVID","INSID","GChar1","GChar2","GChar3", "AdjustmentsApplied","CalibrationAdjustment","GPSTimeAdjustment","GainAdjustment","Status")
    
    # check to see of more 1/3 OCB than 33... if so truncate data
    if(dim(a)[2] > 30) a <- a[,1:30]
    
    # check to see if less than 33 octave
    endA = ((33-4)-dim(a)[2])+1
    
    # calculate a dBA
    aweight <- c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
                 -13.4, -10.9, -8.6, -6.6, -4.2, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
                 1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3)
    # only use a-weights for the available data
    #aA <- a + aweight[4:(33-endA)]
    #a[1,] + aweight[4:(33-endA)]
    aA = t( t(a) + aweight[4:(33-endA)] )
    
    
    # convert to pressure
    press <- rowMeans(10^(aA/10))
    dBA = 10*log10(press) #hist(dBA)
    
    # rescales the values to the AMT scale, using a normalization formula
    if (envir == 1)   
    { a2 = ((a -  (-8)) / (87 - (-8))) * a 
    #hist(a2) a2 = a - 62 # accounts for offset of water/ air
    a=a2
    rm(a2)
    }
    # hist(a) 
    
    ## determine how many blank columns in NVSPL, assumes you add the first 5 columns
    nBlankCols <- length(NVSPLhead) - (dim(a)[2] + 5)
    
    ## find unique day hours
    unqHrs <- substr(tString,1,13)
    
    ## create matrix with all the data combined and add headers
    tempOutput <- cbind(site, tString, 0, 0, 0, round(a, 1), 
                        matrix(rep(0,dim(a)[1] * nBlankCols), 
                               nrow=dim(a)[1], ncol=nBlankCols))
    tempOutput[,36] = dBA
    tempOutput[,54] = "PAMGUIDE_V6"
    colnames(tempOutput) <- NVSPLhead
    
    ## separate tempOutput by unique day hours
    tempOutput <- cbind(unqHrs, tempOutput) #add a column to sort by
    unqHrData <- split(tempOutput, tempOutput[,1]) #find where to split the data
    
    ## write out data to separate files
    for(hr in 1:length(unqHrData))
    {
      dataToWrite <- matrix(unqHrData[[hr]],ncol=dim(tempOutput)[2])[,-1]
      colnames(dataToWrite) <- NVSPLhead
      outFileName <- paste(NVSPLdir,"\\", "NVSPL_", site, "_", 
                           gsub(" ","_",gsub("-","_",names(unqHrData[hr]))), 
                           ".txt", sep="")
      write.csv(dataToWrite, file=outFileName, na="", quote=F, row.names=F)
    }
    
    # remove the folder with daily WAV files
    # unlink(new.folder, recursive=TRUE)
    
  } ## END OF DAY LOOP (d)
  
  cat('done in',(proc.time()-t1)[3],'s.\n')
} ## END OF DIRECTORY LOOP (ff)
