# Scripts used to run PAMGuide and convert to NVSPL format
# generates .csv files of 1 s, 1/3 OTB calibrated SPL from audio files

rm(list=ls(all=TRUE)) 

#NOTES: 
# all plotting options are turned off because of errors with the code.
#___________________________________________________________________________________________________
# INPUTS
# timestring
# calibration params: mh, G, vADC
#___________________________________________________________________________________________________
#OUTPUTS
# CSV file with SPLs
#(...can be read into PAMGuide2NVSPLv2.R)
#___________________________________________________________________________________________________
#WORKED FOR:
#10__0__20160506_045603.wav
#___________________________________________________________________________________________________
#CALIBRATE
# SM3 = mh=-36, G=24+XX, vADC=1
#___________________________________________________________________________________________________

#___________________________________________________________________________________________________
#PART 1: TEST process one audio file to test all the settings are correct
#___________________________________________________________________________________________________
# SET DIRECTORY WHERE ALL AUDIO FILES TO PROCESS ARE...
workingDir <-"G:\\NPS_DATA\\SEKI_fire\\20160512_10\\Data"
site = "10"

setwd('E:\\Mwork\\mee\\PAMGuide') 
source('PAMGuide.R')

#calibrated
PAMGuide(chunksize = 500, atype = 'TOL', timestring = "10__0__%Y%m%d_%H%M%S.wav",
         r=0, outwrite=1, plottype = "None", calib=1, envi="Air", ctype="TS", Mh=-36, G=24, vADC=1)
# Evaluate the file created.........
testFile <- list.files(workingDir, pattern = '.csv', recursive=T, full.names=T)
basename(testFile)
# combine data
conk <- as.matrix( read.csv(testFile[1], colClasses="numeric",header=FALSE) )
dimc <- dim(conk)  	
#dB range
a <- conk[2:dimc[1],2:dimc[2]] 
hist(a,main = "Check to see if calibration is accurate", xlab="SPL dB")
min(a)
max(a)
#time stamp- start and continious
t <- conk[2:dimc[1],1]
t <- as.POSIXct(t,origin="1970-01-01")
cat('Check start time...')
as.character(t[1])
plot(conk[2:dimc[1],1],main = 'Is time continous?')
#delete files, if okay, re-run if not
file.remove(testFile)
rm(conk,a,dimc,t, testFile, PAMGuide)

#___________________________________________________________________________________________________
# PART 2: BATCH process a directory of audio files and convert them to an .csv file in PAMGuide format
#___________________________________________________________________________________________________
# ????? automate this so it runs through the data
# ????? what if sampling is not continous in the audio file= like 10 seconds every...
rm(list=ls(all=TRUE)) 

setwd('E:\\Mwork\\mee\\PAMGuide') 
source('Meta.R')

#copy details from above!
workingDir = 'G:\\NPS_DATA\\SEKI_fire\\20160512_10\\Data\\dawnchannel'
site = "10"

Meta(chunksize = 500, atype = 'TOL', timestring = "10__0__%Y%m%d_%H%M%S.wav",
     r=0, outwrite=1, plottype = "None", calib=1, envi="Air", ctype="TS", Mh=-36, G=0, vADC=1)

#___________________________________________________________________________________________________
# PART 3: CONVERT OUTPUT of PART 2 TO NVSPL
#___________________________________________________________________________________________________

NVSPLdir <- paste(workingDir,"\\NVSPL",sep="")
dir.create(NVSPLdir,showWarnings=F,recursive=T)
envir = 2 # 1= water, 2 = air measurements
#___________________________________________________________________________________________________
#READ in file(s) generated in PAMGuide
# rm(list=ls(all=TRUE))  
# workingDir = 'C:\\Projects'
PAMFiles <- list.files(workingDir, pattern = 'Conk.*.csv', recursive=T, full.names=T)
ifile <- basename(PAMFiles)
cat('Reading selected files...')
tglo <- proc.time()  						  #start file read timer
# Read in the first file...
ifile
conk <- as.matrix( read.csv(PAMFiles[1], colClasses="numeric",header=FALSE) )

# If more than one file... read in the rest of the files
if (length(ifile) > 1) 
  {
  
  for (i in 2:length(ifile)-1 ) #do not read the last file- because it is the conc
    {
    temp <- as.matrix(read.csv (PAMFiles[i], skip = 1, colClasses="numeric", header=FALSE))
    conk <- rbind(conk,temp) 
    }
}

cat('file read in ',(proc.time()-tglo)[3],' s\n',sep="")

#___________________________________________________________________________________________________
#GET METADATA CODES FOR EACH FILE
aid <- conk[1,1]  
tstampid <- substr(aid,1,1)		#extract time stamp identifier
enviid <- substr(aid,2,2)			#extract in-air/underwater identifier
calibid <- substr(aid,3,3)		#extract calibrated/uncalibrated identifier
atypeid <- substr(aid,4,4)

#assign PAMGuide variables envi, calib, atype from metadata
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

#___________________________________________________________________________________________________
#EXTRACT DATA SPL DATA and TIMESTAMP
dimc <- dim(conk)  	
t <- conk[2:dimc[1],1]
t <- as.POSIXct(t,origin="1970-01-01")
tString <- as.character(t)
a <- conk[2:dimc[1],2:dimc[2]] 
f <- conk[1,2:dimc[2]]

# hist(a)
max(a)
min(a)
median(a)
hist(a)

rm(conk)
#___________________________________________________________________________________________________
#FORMAT myOutput as NVSPL
#(note: PAMguide starts at 25 Hz, so lower bands (12.5, 15.8, and 20 are always NaNs)

NVSPLhead = c("SiteID","STime", "H12p5", "H15p8", "H20", "H25", "H31p5","H40","H50","H63","H80","H100","H125","H160","H200","H250","H315","H400","H500",
              "H630","H800","H1000","H1250","H1600","H2000","H2500","H3150","H4000","H5000","H6300","H8000","H10000","H12500","H16000","H20000",
              "dbA","dbC","dbF","Voltage","WindSpeed","WindDir","TempIns","TempOut","Humidity",
              "INVID","INSID","GChar1","GChar2","GChar3", "AdjustmentsApplied","CalibrationAdjustment","GPSTimeAdjustment","GainAdjustment","Status")

#check to see of more 1/3 OCB than 33... if so truncate data
if(dim(a)[2] > 30) a <- a[,1:30]

#calculate a dBA
aweight <- c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
             -13.4, -10.9, -8.6, -6.6, -4.2, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
             1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3)
#only use a-weights for the available data
aA <- a + aweight[4:((dim(a)[2])+3)]
# convert to pressure
press <- rowMeans(10^(aA/10))
dBA = 10*log10(press) 
hist(dBA)


#if underwater data, put offset 
if (envi == "Wat")   
{ 
  #a2 = ((a -  (-8)) / (87 - (-8))) * a # rescales the values to the AMT scale, using a normalization formula
  hist(a2)
  library(scales)
  a3 = rescale(a, to = c(-8, 87), from = range(a, na.rm = TRUE, finite = TRUE))
  hist(a3)
  #a2 = a - 62 # accounts for offset of water/ air
  a=a3
  rm(a2)
}

#determine how many blank columns in NVSPL, assumes you add the first 5 columns
nBlankCols <- length(NVSPLhead) - (dim(a)[2] + 5)

#find unique day hours
unqHrs <- substr(tString,1,13)

#create matrix with all the data combined and add headers
tempOutput <- cbind(site, tString, 0, 0, 0, round(a, 1), 
                    matrix(rep(0,dim(a)[1] * nBlankCols), 
                    nrow=dim(a)[1], ncol=nBlankCols))
tempOutput[,36] = dBA
colnames(tempOutput) <- NVSPLhead

#separate tempOutput by unique day hours
tempOutput <- cbind(unqHrs, tempOutput) #add a column to sort by
unqHrData <- split(tempOutput, tempOutput[,1]) #find where to split the data

#write out data to separate files
for(hr in 1:length(unqHrData))
{
  dataToWrite <- matrix(unqHrData[[hr]],ncol=dim(tempOutput)[2])[,-1]
  colnames(dataToWrite) <- NVSPLhead
  outFileName <- paste(NVSPLdir,"\\", "NVSPL_", site, "_", gsub(" ","_",gsub("-","_",names(unqHrData[hr]))), ".txt", sep="")
  write.csv(dataToWrite, file=outFileName, na="", quote=F, row.names=F)
}

# dim(dataToWrite)



