#
# R code to load HISTORICAL DRS INFORMATION and COMBINE into one big file

## NOTE if I've just run base_code to create masterParam delete everything else:
# Clean up environment a little
rm(list=ls()[!ls() %in% c("masterParam")])
#rm(list=ls()[!ls() %in% c("testResults.bigWithClass")])

#
# This only gets run ONCE each time there is new data to hadd to the historica dataset
#
# HERE IS CORRECT WAY TO DO IT
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
}
library(here)
setwd(here::here())

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
path <- here::here("ALL_DRS_CHEMICALS_November2024","input")

# Build list of files to read
fileList <- list.files(path, pattern = "*.csv", full.names = T)

# Read File
# Looping through all in filelist to build list of ALL DRS CHEMICALS FOUND ON WHICH WRISTBANDS and RESULT
fileAll <- NULL # Setup up place to put all files we read in

# for (fileName in fileList) {
#    fileName<-fileList[1]
#   file <- read.csv(fileName)
#   file$CASRN <- NULL # Get rid of extra unneeded column with CASRN  if that col is there
#   fileLong <- pivot_longer(file,  cols = -c(Chemical, CASRN), names_to = "SampleNumber", values_to = "Result") %>%
#     mutate(Source_File = basename(fileName)) %>% # Move from WIDE to LONG and add column with filename we got data from
#     filter(Result > 0)
#   # Create a lookup table to map SampleNumber to a consistent random SubjectID
#   unique_samples <- unique(fileLong$SampleNumber)
#   subject_id_mapping <- data.frame(
#     SampleNumber = unique_samples,
#     SubjectID = sapply(unique_samples, function(x) paste0(sample(letters, 8, replace = TRUE), collapse = ""))
#   )
#
#
#   fileAll <- bind_rows(fileAll, fileLong)
# }


# Loop through each file and process
# Combine all files into a single long-format data frame without modifications
fileLong <- bind_rows(lapply(fileList, function(fileName) {
  file <- read.csv(fileName, check.names = FALSE)  #checknames stops converstion of column names
  pivot_longer(
    file,
    cols = -c(Chemical, CASRN),  # Exclude both Chemical and CASRN columns from being pivoted
    names_to = "SampleNumber",
    values_to = "Result"
  ) %>%
    mutate(Source_File = gsub('.csv$', '', gsub('^ResultsOutputFile_', '', basename(fileName)))) # Remove 'ResultsOutputFile_' prefix if it exists
}))

# Pattern to NOT change the names for is:
doNotChangeNamePattern <- '-WB$'

# Create a lookup table to map SampleNumber to a consistent random SubjectID (after creating the full data frame)
unique_samples <- unique(fileLong$SampleNumber)
subject_id_mapping <- data.frame(
  SampleNumber = unique_samples,
  #SubjectID = paste0("Subject", seq_along(unique_samples))
  SubjectID = ifelse(grepl(doNotChangeNamePattern, unique_samples)
                     , unique_samples,
                     paste0("Subject", seq_along(unique_samples)))

)

# Join the lookup table to fileLong to assign consistent SubjectIDs and make final modifications
fileAll <- fileLong %>%
  left_join(subject_id_mapping, by = "SampleNumber") %>%
  left_join(masterParam %>% select(ParameterName, ParameterID), by = c("Chemical" = "ParameterName")) %>%
  mutate(
    Result = signif(Result * (1 + runif(n(), min = -0.05, max = 0.05)), 2)  # Randomly adjust Result by -5% to +5% and trim to 2 signif
  ) %>%
  rename(ParameterName=Chemical)%>%
  mutate(SampleNumber=SubjectID)%>%
  rename(PureSampleName=SubjectID) %>%
  mutate(ResultOriginal=Result) %>%
  mutate(Units="ng/g") %>%
  filter(Result > 0) %>%
  select(ParameterID,PureSampleName,SampleNumber,ParameterName,Result,ResultOriginal,Units)


write_csv(fileAll, here::here("ALL_DRS_CHEMICALS_November2024", "output", "full_list_of_all_DRS_resultsNOV8.csv"))


rm( fileAll, fileLong, fileList, path,subject_id_mapping,doNotChangeNamePattern,unique_samples)
