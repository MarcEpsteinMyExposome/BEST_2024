
###
###
###  THERE are two things I need to do:
###   1) create a RESULTS FILE that has all the needed columns
###   2) Create a MasterParam table that has all the needed chemicals, each listed ONCE... THAT STEP i did MANUALLY NOT HERE.



if (!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE)};suppressMessages(library("tidyverse"))

WideTable <-
  read.table(
    "data/NIH SBIR-all data combined CSV.csv",  #
    sep = ","    ,
    header = TRUE    ,
    colClasses = "character",  # Import all as character
    comment.char = "",       # Don't allow use of "#" as comment in input
    quote = "\""  ,
    fileEncoding="UTF-8-BOM"  # THIS gets rid of the weird characters near the beginning of the first line if file is weirdly formatted
  )
##NOTE we are ASSUMING that all tested compounds have VALUES
#
longTable<-gather(WideTable,
       Sample_Name,
       Result,
       colnames(WideTable[3:ncol(WideTable)])     ,
       factor_key = FALSE)

#
#write.csv(longTable, file = "longTable.csv", row.names=FALSE)

#

longTableHasValues<-longTable %>% filter(Result!="")  ## Delete any rows with no results found.
#
#write.csv(longTableHasValues, file = "longTableHasValues.csv", row.names=FALSE)
#
#DO some tests

#length(unique(longTable$Name)) #80 now, was 78
#length(unique(longTable$CASRN)) #80 now, was 79
# SO WERE are more unique CASRN than unique NAMES
#OLD Problem is that Ionone	127-41-3  and Ionone	79-77-6 are same name diff CASRN but FIXED in latest release from Steven
##
### THERE is no clean way generically to go from CAS NUM back to Paramter ID but it doesn't seem to be a problem for this data so...just going to do it.
###
### Fixed up the file:  MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_SBIR_Tweak.csv
### and this file will be used ONE TIME to fix up the RESULTS file... and THEN be used again to READ IN the results file.
#
SBIR_Parameter_List<- "./data/MASV_sbir_parameter_list_based_on_DRS_plus_Missing.csv"

addParameterID_basedOnCASNUM <-
  read.table(
    SBIR_Parameter_List,  #note_this_WAS_manually created add_Param.csv but i renamed... MAYBE should keep this separate as a fix-up for data without ParameterID and use other for MasterParam List in R code?
    sep = ","    ,
    header = TRUE    ,
    colClasses = "character",  # Import all as character
    comment.char = "",       # Don't allow use of "#" as comment in input
    quote = "\""  ,
    fileEncoding="UTF-8-BOM"  # THIS gets rid of the weird characters near the beginning of the first line if file is weirdly formatted
  ) %>% rename(CASRN=CASNumber)
# Add fields from Parameter Name lookup file
longTableHasValuesWithParameterName<-longTableHasValues %>% left_join(addParameterID_basedOnCASNUM,by="CASRN")
# Make sure PureSampleName is there in the data
if(!     ("PureSampleName" %in% colnames(longTableHasValuesWithParameterName)   )){
  longTableHasValuesWithParameterName$PureSampleName <- longTableHasValuesWithParameterName$Sample_Name
}
# Make sure SampleNumber is there in the data
if(!     ("SampleNumber" %in% colnames(longTableHasValuesWithParameterName)   )){
  longTableHasValuesWithParameterName$SampleNumber <- longTableHasValuesWithParameterName$Sample_Name
}
# Make sure Flag is there in the data
if(!     ("Flag" %in% colnames(longTableHasValuesWithParameterName)   )){
  longTableHasValuesWithParameterName$Flag <- ""
}

### BUT there are some that don't get parameter IDS
badRows_NoParamterMatch<-longTableHasValuesWithParameterName %>% filter(is.na(ParameterID))
unique_No_match <-badRows_NoParamterMatch %>% select(CASRN,Name) %>% unique()
### STOPPING HTERE i have a file "add_ParamID.csv" which has some duplicate CAS but lets me look up ParameterID by CAS so now I can use that same thing in reverse....
#### so I'M NOT GOING TO USE  MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_SBIR_Tweak.csv   BUT USE ADD PARAMETER ID FILE INSTEAD
#
## ALSO:  Now that I fixed up the RESULTS FILE with paramterID i need to write that you:

write.csv(longTableHasValuesWithParameterName, file = "data/SBIR_longTableHasValuesWithParameterName.csv", row.names=FALSE)   # THIS BECOMES THE NEW MASTERPARAM TABLE FOR THIS SBIR DATA
rm(addParameterID_basedOnCASNUM,badRows_NoParamterMatch,longTable,longTableHasValues,unique_No_match,WideTable)
rm(longTableHasValuesWithParameterName)
rm(SBIR_Parameter_List)



