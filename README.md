# seki-birds
## Interpreting acoustic data for bird activity and diversity in Sequoia-Kings Canyon National Park (SEKI)

### project description:    

in progress...

### data:  

**/indices/2018_02_05:** csv files of acoustic indices calculated for all lakes 2014-16. 1 file per lake.  
**/insects:** data files containing mayfly count data from 2015 
**/ptct:** csv of point count data from 2014-2016  

### scripts:  

**acoustic_effort.R:** visualizes sampling effort of acoustic monitors  
**calculate_ACI_wavfile_SEKIClapp_V5.R:** calculates Acoustic Complexity Index from wav files, adapted from NPS NSND  
**calculate_AI_NVSPL_V16c_mkc.R:** adapted from NPS-NSNSD, calculates many acoustic indices from NVSPL files-- out put in /data/indices/2018_02_05  
**hide_filenames.R:** renames files to hide identifying information in the filename-- DO NOT USE YET  
**index_explore_20180208.R:** exploration of acoustic index data in data/indices/2018_02_05  
**mayflycode.R:** explores mayfly count data from sticky traps  
**moch_pb.R:** reads in a selection table created in Raven of the Mountain Chickadee playback conducted in 2014 at Center Basin  
**pointcount_explore.R:** reads in point count data and explores patterns  
**sscape_airtemp.R:** plots air temperature taken from SongMeters  
