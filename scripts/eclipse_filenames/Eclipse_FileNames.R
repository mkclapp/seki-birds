rm(list=ls())

# READ in directory of files
inDir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\"
setwd(inDir)
wavFiles <- dir(pattern=".wav$")
orgFiles = file.info(wavFiles) #to save for records
site =  "EclipseClips"

outFil = matrix(data = NA, nrow = length(wavFiles), ncol= 8)
for (ff in 1: length(wavFiles)) {
  # ff = 1
  Finfo = file.info(wavFiles[ff])
  Finfo$Orgname = wavFiles[ff]
  
  outFil[ff,] = as.matrix(Finfo)
  colnames(outFil) = colnames(Finfo)
  
  rm(FileName, FileName1,tmp,tmp1, dateStamp, strtTime, cTime, Finfo )
}

dstmp = Sys.Date()
write.table(outFil, paste(inDir, site, "_FileNames_", dstmp, ".txt", sep=""), sep="\t")
