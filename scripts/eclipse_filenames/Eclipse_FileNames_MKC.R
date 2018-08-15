#NOT WORKING FOR ME

rm(list=ls())

# READ in directory of files
inDir = "/Users/mary/Desktop/Research/data/listens/10_min_samples/listens_center_2015"
setwd(inDir)
wavFiles <- dir(pattern=".wav$")
origFiles = file.info(wavFiles) #to save for records
site =  "EclipseClips"

outFile = matrix(data = NA, nrow = length(wavFiles), ncol= 8)
for (ff in 1: length(wavFiles)) {
  #ff = 1
  Finfo = file.info(wavFiles[ff])
  Finfo$Origname = wavFiles[ff]
  
  outFile[ff,] = as.matrix(Finfo)
  colnames(outFile) = colnames(Finfo)
  
  rm(FileName, FileName1, tmp, tmp1, dateStamp, strtTime, cTime, Finfo)
}

dstmp = Sys.Date()
write.table(outFile, paste(inDir, site, "_FileNames_", dstmp, ".txt", sep=""), sep="\t")
