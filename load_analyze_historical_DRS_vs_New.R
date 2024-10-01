#
# R code to load and maybe analyze HISTORICAL DRS INFORMATION
#

#
### NOTE:  I'm using the ALREADY PROCESSED OUTPUT FILES and NOT using the original INPUT files.  That is interesting... could do it the other way and then GENERATE this output file
#         Which i would then read-in or use to do compare.
#         that would have advantages of bringing along classifications and ParameterID and uniqueness and etc etc etc
#

#### I DID NOT BRING OVER CLassification info... so that info isn't here to analyze
####  COULD add the max and min for each compound in a chart comparing max/min in historical with max/min in current... in graph or chart
#### ???
### ask steven

### NOTICE:  If you do this analysis... ON AN EXISTING DATDASET that is merged in... it doesn't work as well SO EXCLUDE the dataset you are loading
#### by re-running the LOAD component after RENAMING the EXTENTION on the dataset  BUT NOTICE i now do this automatically IF the name is the same...

## OTHER IDEAS --This vs Master:  CHECK for LARGEST VALUE EVER

# HERE IS CORRECT WAY TO DO IT
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
}
suppressMessages(library("tidyverse"))
options(dplyr.summarise.inform = F)
#
# Point to Subdirectory where i put all the files I could find
#    and also location I put output from this R script
path <- "./ALL_DRS_CHEMICALS/"


#fileNamedatasetToAnalyze <- "results_Raw_and_Modified_full_and_long_UCDAVIS.csv"
#fileNamedatasetMatchingToAnalyzeData <- "ResultsOutputFile_UCDavis35.csv"
#colTypesOfDatasetToAnalyze <- "cnnccc"
#nameOfSourceFileSource <- "UCDAVIS"

#AT FIRST i did this only to compare UCDavis
# NOw I'm trying 4/2/2023 to do this for Univision
##### NOTE NOTE :   I did NOT rebuild the BIG database despite moving the result file into the correct directory
#
fileNamedatasetToAnalyze <- "results_Raw_and_Modified_full_and_long_Univision.csv"
fileNamedatasetMatchingToAnalyzeData <- "ResultsOutputFile_MASV_Univision.csv"
colTypesOfDatasetToAnalyze <- "cnnccc"
nameOfSourceFileSource <- "UNIVISION"


## SO THIS IS THE BIG MASTER DATABASE
MasterDRSdataset <- read_csv(paste0(path, "output/", "full_list_of_all_DRS_results.csv"), col_types = "ccnc")


## Remove from the MasterDataset any data that came from the dataset we're about to analyze
## somehow delete rows where source file NOT equal...
MasterDRSdataset <- MasterDRSdataset %>%
  filter(!Source_File == fileNamedatasetMatchingToAnalyzeData)


# Read In the Data on the EXISTING BEING ANALYZED dataset
#    this assumes KNIT just run and created result in directory...does NOT WORK FOR DARTMOUTH cause that is put elsewhere
### IN THE EVENTUAL CODE can just the thing I wrote out BEFORE i wrote it out so... that will be different eventually.
#  (reading in UCDAVIS hard coded to test things)
#
DataToAnalyze <- read_csv(paste0(path, "input/", fileNamedatasetToAnalyze), col_types = colTypesOfDatasetToAnalyze) %>% ### CHANGE THIS DEPENDING ON WHAT DATA IS THE ONE BEING COMPARED TO THE BIG ONE
  select(ParameterName, SampleNumber, Result) %>%
  mutate(Source_File = nameOfSourceFileSource) %>%
  rename(Chemical = ParameterName)

rm(colTypesOfDatasetToAnalyze, fileNamedatasetMatchingToAnalyzeData, fileNamedatasetToAnalyze)

# resultTbl <- DataToAnalyze
# resultTbl <- MasterDRSdataset
#
# calculate the minimum, maximum and mean of a set of chemicalsR
#
minMaxMeanChemicals <- function(resultTbl) {
  resultTbl %>%
    group_by(SampleNumber) %>%
    summarise(ChemicalsPerSample = n()) %>%
    summarise(
      min = round(min(ChemicalsPerSample), 2),
      mean = round(mean(ChemicalsPerSample), 2),
      max = round(max(ChemicalsPerSample), 2)
    )
}


# Compare the mean/min/max of the NEW data to the Master Dataset of DRS results
tableComparingMeanMinMax_Count_of_Chems <- bind_rows(minMaxMeanChemicals(DataToAnalyze), minMaxMeanChemicals(MasterDRSdataset)) %>%
  add_column("Which Dataset?" = c("NewData", "MasterDataset"))


# Find items that are in the FIRST dataset argument that are NOT in the 2nd dataset...
#    Basically what is NEW in the new dataset that we've never seen before
# CAUTION:
#     This uses the NAME of the compound as the UNIQUE key... it is possible NAMES changed or something.... ???
#       NEED TO CHECK with real examples and from name find KIM unique ID # and then check for THAT uniqueness
#

chemicalsInFirstSetNotSecond <- function(resultTbl1, resultTbl2) {
  setdiff(
    resultTbl1$Chemical,
    resultTbl2$Chemical
  )
}

#  USE Function to compare NEW data with OLD data
NewCHemsFound <- tibble(chemicalsInFirstSetNotSecond(DataToAnalyze, MasterDRSdataset))


#
# Figure out MaX values of chemical in New data COMPARED to max value of that chemical previously
#
# resultTbl <- DataToAnalyze

# Create funciton just to return MAX of each chemical
getMaxValuesPerChemical <- function(resultTbl) {
  resultTbl %>%
    group_by(Chemical) %>%
    summarise(max = max(Result))
}


# Find the MAX of each chemical in new data and see if bigger than MAX of that chemical in MASTER dataset
#
#  DOING silly trick to rename each column appropriately
#     would be better to pass in name for column but I couldn't get variables to de-reference properly and this rename works
#
newMaxNumberInNewData2.5Bigger<- (getMaxValuesPerChemical(DataToAnalyze) %>%
  rename(newDataMax = max)) %>%
  left_join(getMaxValuesPerChemical(MasterDRSdataset) %>%
    rename(masterDataMax = max)) %>%
  filter(newDataMax > 2.5 * masterDataMax)
#### NOTE:  Need to do the 2.5 times versions.....
###                    so FILTER by
###   SAMPLE for UCDavis
# Chemical                                        newDataMax masterDataMax
# <chr>                                                <dbl>         <dbl>
#   1 Citral B                                              1130           297
# 2 Promecarb artifact [5-isopropyl-3-methylphenol]        777           205

### NOW let's write out the interesting tables

write_csv(NewCHemsFound, paste0(path, "./output/", nameOfSourceFileSource,"NewChemicalsFound.csv"))
write_csv(newMaxNumberInNewData2.5Bigger, paste0(path, "./output/", nameOfSourceFileSource,"newMaxNumberInNewData2.5Bigger.csv"))

