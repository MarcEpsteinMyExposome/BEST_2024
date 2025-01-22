#
# R code to load HISTORICAL DRS INFORMATION and COMBINE into one big file


#
# Set all key variables.  Use the existance (or not) of "subject" to decide if they need to be loaded
#if (!exists("subject")) {   # STOP USING SUBJECT to see if MyExp_set_key_variables.R is loaded
if (!exists("MyExp_set_key_variables_R_file_is_loaded"))
  ##  NEED TO HARD CODE  R Directory Location
{
  source(here::here("R", "MyExp_set_key_variables.R"))
}
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
if (!exists("masterParam")) {
  source(r_code)
}




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
#suppressMessages(library("tidyverse"))
#options(dplyr.summarise.inform = F)
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

############# AT THIS STAGE we have FILELONG and it is perfect... i don't need to change to do the ANALYSIS OF THE DATA...
##############################    but... if i want to CHANGE ID's and mess around to output this list and uuse then down below I tweak things and join thins

write_out_all_DRS_found_with_note_if_found_in_first_processed_SBIR_71<-FALSE

if (write_out_all_DRS_found_with_note_if_found_in_first_processed_SBIR_71) {
  # First lets only get non-zero rows
  fileLongNonZero <- fileLong %>%
    filter(Result > 0)


  # Read all 88 compounds found in first 71 wristbands for NIEHS
  file71name <- here::here("ALL_DRS_CHEMICALS_November2024",
                           "CAS_Numbers_From_First_71.csv")

  file71 <- read.csv(file71name)

  #str(file71)
  # str(file71)
  # 'data.frame':	88 obs. of  1 variable:
  #   $ CASRN: chr  "2772-45-4" "96-76-4" "120-12-7" "119-61-9" .


  summary_data_all_compounds_ever_MyExposome_orig   <- fileLongNonZero %>%
    group_by(CASRN) %>%
    summarise(Count = n(),
              Percentage = (n() / n_distinct(fileLongNonZero$SampleNumber)) * 100) %>%
    arrange(desc(Count))

  # Summarize the data
  # Summarize the data and include the Chemical name
  # Summarize the data and include the Chemical name and percentage based on unique SampleNumbers
  summary_data_all_compounds_ever_MyExposome <- fileLongNonZero %>%
    group_by(CASRN, Chemical) %>%
    summarise(
      Count = n(),
      UniqueSampleCount = n_distinct(SampleNumber),
      Percentage = round((
        n_distinct(SampleNumber) / n_distinct(fileLongNonZero$SampleNumber)
      ) * 100, 1)
    ) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(InFile71 = ifelse(CASRN %in% file71$CAS, TRUE, FALSE))  # Ensure CASRN column in file71 is used correctly



  fileSummaryName <- here::here(
    "ALL_DRS_CHEMICALS_November2024",
    "All_DRS_results_flagged_if_in_71_first_WBs_TEST.csv"
  )
  write.csv(summary_data_all_compounds_ever_MyExposome,
            fileSummaryName)


}

#########################################  NOW i'm going to obfuscate things and mess around.. and write... this is the stuff BELOW output to file to USE LaTER

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
