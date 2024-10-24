#
# R code to load HISTORICAL DRS INFORMATION and COMBINE into one big file
#
# This only gets run ONCE each time there is new data to hadd to the historica dataset
#
# HERE IS CORRECT WAY TO DO IT
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
}
#
suppressMessages(library("tidyverse"))
options(dplyr.summarise.inform = F)
#
# FIRST I'm choosing to load WIDE data.  Cause that is what I can find at the moment.
#
# But I'll convert it to LONG and add a flag about WHICH dataset it was
#
# Data is here:   DATA\R_analysis\BEST_JULY_2018_DRS_MORE\ALL_DRS_CHEMICALS
#
# LOAD each CSV File One at a time, append to other file
# Point to Subdirectory where i put all the files I could find
path <- "./ALL_DRS_CHEMICALS/"

# Build list of files to read
fileList <- list.files(path, pattern = "*.csv", full.names = T)

# Read File
# Looping through all in filelist to build list of ALL DRS CHEMICALS FOUND ON WHICH WRISTBANDS and RESULT
fileAll <- NULL # Setup up place to put all files we read in
for (fileName in fileList) {
  # fileName<-fileList[1]
  file <- read.csv(fileName)
  file$CASRN <- NULL # Get rid of extra unneeded column with CASRN  if that col is there
  fileLong <- pivot_longer(file, cols = -Chemical, names_to = "SampleNumber", values_to = "Result") %>%
    mutate(Source_File = basename(fileName)) %>% # Move from WIDE to LONG and add column with filename we got data from
    filter(Result > 0)
  fileAll <- bind_rows(fileAll, fileLong)
}
write_csv(fileAll, paste0(path, "output/", "full_list_of_all_DRS_results.csv"))

rm(file, fileAll, fileLong, fileList, fileName, path)
