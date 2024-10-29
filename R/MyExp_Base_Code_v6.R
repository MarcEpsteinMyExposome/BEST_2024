#
#   Basic IDEA is that only need to change contents of
#        MyExp_set_key_variables.R     The definition of key variables
#     OTHER R FILES loaded and used include
#         MyExp_data.load.functions_1527_v6.R      LOAD all the data files and set them up for use (clean/prep data)
#         MyExp_data.load.functions_1527_v6.R       group print all users a separate PDF or HTML one at a time
#         MyExp_support_functions_1527_v6.R       Separate helper functions to build MESSAGES regarding summary information analyzing data

#        MyExposome_1527_v6.RMD is now used for EVERY tet and that is kinda wrong cause it SAYS 1527 but it really is GENERIC for all QUANTitative tests
#         NOTE:  THere are LOTS of different nested RMD files now... that go off that main file... one set for GROUP vs INDIVIDUAL reports, one for CLASSIFICATION of chemicals (or NOT) etc...
#           HERE are SOME of the example RMDs that we're using to load alternate text
#             CompoundClassification_text.Rmd
#             GroupChemicalClassification_text.Rmd
#             GroupClassificationAnyWristband_text.Rmd
#             GroupClassificationEveryWristband_text.Rmd
#             GroupClassificationAtLeastOne_text.Rmd
#             GroupChemicalListWithClassification_text.Rmd
#             GroupChemicalListWithOutClassification_text.Rmd
#
#            IndividualResultsAnalysis_text.Rmd
#            IndividualChemicalListWithClassification_text.Rmd
#            IndividualChemicalListWithOutClassification_text.Rmd
#            IndividualClassificationShowingChemicalsAndAverages_text.Rmd
#
#           testPrePostProcessing.Rmd ----> SHOW pre/post (before intervention/after intervention) charts and data if the data has that marked (IS MANUAL PROCESS TO SETUP DATA RIGHT NOW)
#
#
#   and THEN print-all-subjects and the "MyExp_Base_Code_v?.R code  should work fine
#
# DATA INPUTS:
#   resultsTableName    CSV test results from a specific run of a group of wristbands by ParameterID
#         NOTE: if results INCLUDES some fields that are also in masterParameterTable we IGNORE and use
#         use masterParameterTable as the master!
#   masterParamTableName:  CSV mapping ParameterIDs to actual compound names and CAS numbers
#                           This table ONLY SHOULD LIST the ParameterIDs of chemicals WE TEST FOR IN THIS TEST
#   ### THESE ARE EXAMPLE table names... these are input files
#   classificationTableName CSV classifying each chemical into a "category" by ParameterID
#   #
#   riskCalifProp65TableName Lookup Calif Prop 65 Risk factors by ParameterID
#   epaIrisTableName Lookup EPA IRIS risk factros by ParameterID
#   IARCRiskTableName Lookup IARC Risk Factors by Paramter identical
#   ###decode table explaining what the various IARC risk factors mean
#
#
# DATA OUTPUTS:
# DaTA_FRAMES:
#   class_L
#   class_L_maxC
#   classification
#   eapIris
#   masterParam
#   results_W
#   riskCalifProp65
#   SubClassScores
#   testResults.big
# VALUES:
#   refDoc
#   rmd_code
#   subject
#   support.functions.filename

# NOTE NOTE NOTE:   NOTHING IN THE BASE CODE is tied to specific SUBJECT
#
# NOTE THINGS TO WORK ON:
#
# CRITICAL PROBLEMS:
#   LATER need to address the "J" flag differently and call it out more explicitly
#   ALSO:  Should set error trap for FLAG not = U or J or BLANK cause that is only valid #'s

##################


# Function to check, install, and load packages
load_package <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      suppressMessages(library(pkg, character.only = TRUE))
    } else {
      suppressMessages(library(pkg, character.only = TRUE))
    }
  }
}

# List of required packages
# PLOTLY is used to create interactive plots,   Added to help w/ VIOLIN charts
# Added HTMLTOOLS because it allows better generation of html files and separate items in report (first used in violin)
required_packages <- c(
  "png", "ggplot2", "ggtext", "plotly", "htmltools", "rlang",
  "RColorBrewer", "reshape2", "pander", "scales", "tidyverse", "scriptName", "knitr", "rmarkdown", "grid",
  "styler",   # Added styler to allow manual styling of code using the "addins" menu item
  "readxl",
  "bsplus",    # Added BSPLUS to allow popup bottons
  "patchwork",  # added to do side-by-side graphi layout
  "here",  # USE the HERE package to locate the LOGO and provide an absolute path instead of a relative path cause relative path messes up print-all-subjects
  "base64enc"
  #"reactable"  # added as test to do display of table data you can sort filter download     ### DELETED THIS CAUSE IT DIDN"T WORK
)

other_packages <- c("sqldf", "RSQLite", "gplots") # DO NOT LOAD THESE unless needed,  move then to required packages

# Load all required packages
load_package(required_packages)


rm(load_package,other_packages,required_packages)

# Additional setup
options(warn = -1) # Turn off all warning messages
### TIDY with dplyr (i think) in update to 3.0.0 or something added weird warning messages about how group / ungroup / summarize works
#       and actually... i didn't understand before that after a GROUP on multiple things "summarize" and maybe other functions
#           auto-ungroups on the LAST of the grouped things but LEAVES everything grouped other than that "last" (last in list) item
###
### SO the line below STOPS the new error messages BUT to understand group / ungroup /summarize see this: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform = FALSE)

# Additional code for setting up environment, key variables, etc.
if (!exists("subject")) {
  source(here("R","MyExp_set_key_variables.R"))
}

if (!exists("customer_Output")) {
  source(here("R","customer_Output.R"))
}

if (!exists("load.masterParam")) {
  source(here("R",data.load.R.filename))
}


# Re-enable warnings
options(warn = 0) # Turn warning messages back on





###################
#cat("111 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

# Read in data files
masterParam <-
  load.masterParam(masterParamTableName)
#rm(load.masterParam,masterParamTableName)  # CAN NOT RM this here... cause use it again loading other param tables

#cat("222 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)
###################  READ IN CLASSIFICATION and convert from wide to LONG

classification <-
  load.classification(here(classificationTableName))
rm(load.classification,classificationTableName)

cat("333 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)
class_TEMP_hold<-classification   ## DELETE AFTER TESTING

# Create the LONG version of "classification" (which is WIDE)
class_L <- classification %>%
  tidyr::gather(classification,
    value,
    colnames(classification[2:ncol(classification)]),
    factor_key = TRUE
  ) %>% # convert wide to long
  filter(value == 1) %>% # ONLY choose the HITS
  select(ParameterID, classification) %>% # Drop the VALUE column
  mutate(ParameterID = as.character(ParameterID)) %>% # Make the ParameterID column character instead of num
  semi_join(masterParam, by = "ParameterID") # ONLY select the rows from classification that are in masterParam

rm(classification)

### NOW call that function to update class_L with all the values from the other tests

class_L <- updateWithClassSpecificMasterParam(PAH_text_string, here(pahMasterParameterTable), class_L)
class_L <- updateWithClassSpecificMasterParam(VOC_2024_text_string, here(VOC_2024_MasterParamTableName), class_L)  # Revise VOC list for 3rd time... get rid of VOPAH
class_L <- updateWithClassSpecificMasterParam(pest_text_string, here(pestMasterParameterTable), class_L)
class_L <- updateWithClassSpecificMasterParam(flameRetardant_text_string, here(flameMasterParamTableName), class_L)
class_L <- updateWithClassSpecificMasterParam(PHTH_text_string, here(PHTHmasterParameterTable), class_L)   #  Add the Phthalate list to the table... this is new test from 2024

#cat("444 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

### ADD ANY NEW CLaSSIFICATIONS and TESTS HERE:  (like fragrance or new VOC or whatever)...... i just added Phthalates as above...

rm(load.masterParam) # NOTE This is weirdly USED INSIDE FUNCTION ABOVE so ...
### HERE:  REMOVE ALL REMAINING MASTERPARAMETER TABLESS
rm(pahMasterParameterTable,VOC_2024_MasterParamTableName,pestMasterParameterTable,
   flameMasterParamTableName,PHTHmasterParameterTable,SBIR_p1_MasterParamTableName,
   vocMasterParamTableName,vopahMasterParamTableName,drsMasterParamTableName) # rem() all masterParammeter Tables other than MAIN version

rm(updateWithClassSpecificMasterParam)


### FIX UP to add back Celestolide as Personal Care and Consumer Product
### ALSO ADD 301765 1,3-dimethylnaphthalene AS A PAH  (it is in the VOC screen)
class_L <- class_L %>%
  add_row(ParameterID = "301467", classification = personalCare_text_string) %>%
  add_row(ParameterID = "301467", classification = consumerProduct_text_string) %>%
  add_row(ParameterID = "301765", classification = PAH_text_string) %>%
  distinct()

### FIX UP to remove Celestolide  as PAH
##    NO IDEA why this marked as  but need to remove
#  301467	Celestolide
##  THIS may or may not be needed but still keep it after 3/6/2019 testing
## BUt looks like it was FIXED in new 3/7/2019 data file
class_L <- class_L %>%
  filter(!(ParameterID == 301467 & classification == PAH_text_string))

### FIX UP to remove Cashmeran and Phantolide markings as PAH
##    NO IDEA why these are marked as PAH but need to remove them and they keep showing up...
##  THIS is still needed after 3/6/2019 testing
### BUT it looks like this was FIXED in new 3/7/2019 file for 301449 but not for 301478
class_L <- class_L %>%
  filter(!(ParameterID == 301449 & classification == PAH_text_string)) %>%
  filter(!(ParameterID == 301478 & classification == PAH_text_string))

### FIX UP to remove Fluoranthene  as pharmacological
##    NO IDEA why this marked as pharmacological but need to remove them
#  481  # Fluoranthene
class_L <- class_L %>%
  filter(!(ParameterID == 481 & classification == pharmacological__text_string))

### FIX UP to remove Tributyl phosphate  as VOC (found 1/29/2020 with steven on UCSF)
#  300639  # Tributyl phosphate
class_L <- class_L %>%
  filter(!(ParameterID == 300639 & classification == VOC_text_string))



#  I reduced from many do 11 or 12 classifications to 6 so I need to just change class_L to be those new classifications
class_L <- unique(convert_to_new_reduced_classifications(class_L,
                                                         here(class_conversion_table_name))
                  )
rm(convert_to_new_reduced_classifications)

### NOW i am NOT USING any of the "PAH_text_string" , Flame_Text_string, etc etc cause we've changed classifications and that is what these were so let's delete ALL OF THEM
## WHOOOPS... this was needed by GROUP PROCESSING for some reason
# rm(list = c("consumerProduct_text_string", "dioxinsAndFurans_text_string",
#             "flameRetardant_text_string", "industrial_text_string", "PAH_text_string",
#             "PCB_text_string", "personalCare_text_string", "pest_text_string",
#             "pharmacological__text_string", "PHTH_text_string", "VOC_2024_text_string",
#             "VOC_text_string", "VOPAH_text_string"))


##### TESTING having a source and mitigation strategy table to read in
#chemSourceMitigationOLD <- load.chemSourceMitigation(chemSourceMitigationInfoTableName)
#chemSourceMitigationNEW <- load.chemSourceMitigation2(chemSourceMitigationInfoTableName2)  # HERE I'm loading an XLSX file with improved information
### GO TO Google doc and find tab and download the whole thing make sure right SHEET NAME
chemSourceMitigation <- load.chemSourceMitigation2(here(chemSourceMitigationInfoTableName2),chemSourceSheetName2)  # HERE I'm loading an XLSX file with improved information but info ONLY for 88 compounds
rm(load.chemSourceMitigation2,chemSourceSheetName2)

#cat("542 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)




### COMMENT TO MARC:   Control-Shift-Home will select everything above this line

# Originally I just read in and processed the testResults table at one time.  To experiment with double checking count between masterParameterTable and testResults table I split reading table into two parts
#     SO now I just read in RAW table here... and then in a few lines pass in that exact raw table and process it
testResultsRawTable <- load.testResults_justReadTable(here(resultsTableName))
rm(load.testResults_justReadTable)

#cat("666 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

# IN THEORY the number of compounds in the PaRAMETER FILE (what we are testing FOR) should equal the # of compounds we got resuls for
#     BECAUSE OSU LAB always includes all results, even zeros, for every compound
#       EXCEPT for the DRS method in which case they only report results that they HAVE VALUES FOR
#
# SO:  Let's count how many unique compounds are in parameter file... compare to unique compounds in testResults, and FLAG IT to STOP if not a match
#   BUT sometimes we want to actually allow that difference SO for that reason I'm putting a message into the console ASKING IF we should stop or if we should allow it to continue.
#
# NOTE that I defined (in base_code) a function PAUSE
#
#
#### ALSO there is one special case where for VOC testing for WISCONSIN i want to ignore one specific chemical in all runs
#### SO i will put a HACK  here to drop it:
####
if (WisconsinFixup == TRUE & RMD_type == "VOPAH") {
  testResultsRawTable <- testResultsRawTable %>%
    filter(!ParameterID %in% "300639") # STEVEN told Marc to drop this one compound if VOPAH testing for WISCONSIN
  masterParam <- masterParam %>%
    filter(!ParameterID %in% "300639") # STEVEN told Marc to drop this one compound if VOPAH testing for WISCONSIN
}

### Another HACK... we need to DROP SOME COMPOUNDS that were NOT FOUND in the masterParameterFile so we don't hit error
## NOTE that PAH, VOPAH, and PEST all have NEW COMPOUNDS listed in the testresults for Wisconsin that were NOT FOUND in masterparameterfile
## SO FOR A HACK I'm going to FORCE DROP all the rows that have mismatches from WISCONIN before i test if there ARE mismatches!!!
## this hack drops anything from testResults that is NOT found in masterParameterTable
### FOR anyone besides WI this prob no needed cause no need to match old stuff
if (WisconsinFixup == TRUE && (RMD_type == "VOPAH" || RMD_type == "PAH" || RMD_type == "PEST")) {
  testResultsRawTable <- testResultsRawTable %>%
    filter(ParameterID %in% masterParam$ParameterID) # STEVEN and marc agreed to FORCE for WISCONIN for NOW to match masterparam table so ddrop lots from testResultsRawTable

  ##### THE CODE BELOW drops the item that we USED TO BE ABLE TO TEST FOR but now is not reported
  ##### THat other new value is DROPPED automatically..
  ### SOOOOO:  This is the right thing to do going forward... editing this out of MasterParam BUT for old data it means customers are missing one datapoint
  if (RMD_type == "PEST" && WisconsinFixup == FALSE) {
    masterParam <- masterParam %>%
      filter(!ParameterID %in% "300243") # We used to test PEST for 300243	Ethoprophos
  }

  ##### THE CODE BELOW drops the item that we USED TO BE ABLE TO TEST FOR but now is not reported because it is merged with another new value
  ##### THat other new value is DROPPED automatically..
  ### SOOOOO:  This is the right thing to do going forward... editing this out of MasterParam BUT for old data it means customers are missing one datapoint
  if (RMD_type == "PAH" && WisconsinFixup == FALSE) {
    masterParam <- masterParam %>%
      filter(!ParameterID %in% "300409") # We used to test PAH for 300409	naphtho[2,3-j]fluoranthene
  }
}



# setdiff(testResultsRawTable$ParameterID,masterParam$ParameterID )
# setdiff(masterParam$ParameterID, testResultsRawTable$ParameterID )

# setdiff(testResultsRawTable$ParameterName,masterParam$ParameterName )
# setdiff(masterParam$ParameterName, testResultsRawTable$ParameterName )



chemicalsTestedFromMasterParam <- nrow(masterParam)
uniqueChemicalsInTestResults <- length(unique(testResultsRawTable[, "ParameterName"]))
mismatchPrompt <- paste("chemicalsTestedFromMasterParam = ", chemicalsTestedFromMasterParam, " ", "uniqueChemicalsInTestResults = ", uniqueChemicalsInTestResults, " ")

if (!(WisconsinFixup == TRUE && RMD_type == "PAH") && !(WisconsinFixup == TRUE && RMD_type == "PEST")) {
  if (chemicalsTestedFromMasterParam != uniqueChemicalsInTestResults) {
    if (!allowDifferentParameterCounts) { # A flag is set in set_key_variables to allow us to ALLOW different parameter counts between masterParameter and testResults so checking that flag
      stop("Stopped because MISMATCH COUNT --  ", mismatchPrompt, "  Current file name is = ", "MyExp_Base_Code_???")
    }
  }
} else { ### COULD put an ELSE here and make sure it is WITHIN ONE  (subtract, absolute value, <=1) which will catch SOME ERRORS IN FUTURE if WISCONSIN weirdness continues
  ### NOTE THAT I COULD JUST TURN OFF THIS WHOLE CHECKING ALL THE TIME FOR WISCONSIN.... but... TRYING to preserve as much error checking as possible
  if (abs(chemicalsTestedFromMasterParam - uniqueChemicalsInTestResults) > 0) {
    if (!allowDifferentParameterCounts) { # A flag is set in set_key_variables to allow us to ALLOW different parameter counts between masterParameter and testResults so checking that flag
      stop("Stopped because MISMATCH COUNT greater than 1--  ", mismatchPrompt, "  Current file name is = ", "MyExp_Base_Code_???")
    }
  }
}
rm(mismatchPrompt)
# rm(chemicalsTestedFromMasterParam,uniqueChemicalsInTestResults)
# setdiff(testResultsRawTable$ParameterID, masterParam$ParameterID)
# setdiff(masterParam$ParameterID,testResultsRawTable$ParameterID )

testResults <- load.testResults(testResultsRawTable, masterParam)


#cat("777 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

rm(testResultsRawTable,allowDifferentParameterCounts,load.testResults)

### AT THIS POINT, testResults for DRS might have some ZERO values but always numeric results... the ZEROs are cause DRS has some weird values in it that get converted

# IF we're doing any FIXUP (time / weight normalization of wristband results, call FIXUP routine)
# NOTE that this fixup also ENHANCES the testResult dataframe with time_worn and weight and ResultOriginal which are NEEDED to do calculation
if (FixupForAnyone) {
  testResults <- fixUpTestResults(testResults,here(FixupFile))
} else if (doAIRplusNioshOSHAreporting) {
  stop("MyExp DEBUG: NOT doing ANY fixup so air calc won't work and maybe other problems---RESEARCH if hit this error")
}


# now that I've done FIXUP I want to delete all the fixup global variables just to  make things cleaner
#all_objects <- ls()
#fixup_objects <- grep("Fixup", all_objects, value = TRUE,ignore.case=TRUE)
#dput(fixup_objects)
rm(list=c("Big_Mas15_List_Fixup", "BostonFixup", "BuffaloFixup", "CHICAGOFixUp",
          "COLORADOFixUp", "FixupFile",                            #  "DartmouthFixup",   I DELETED DARMOUTH FIXUP cause that has special handling rules so keep it around
          "FixupForAnyone", "fixUpTestResults", "GEORGETOWNFixUp", "LorealFixup",
          "LouisvilleFixup", "SBIR_P1_May2022Fixup", "SBIR_P2_Part1_71_FixUp",
          "UC_DAVISFixup", "UCONNFixUp", "UCSF2020Fixup", "UCSFplusRandom10Fixup",
          "UFL_FloridaFixup", "ULILLEFRANCEFixup", "UMTFixup", "UniVisionFixup",
          "UNMFixup", "WisconsinFixup"))



# Here we're converting some REAL customer data into a DEMO dataset picking only a certain # of WBs
if (makeIntoDemoData) {
  testResults <- makeIntoDemoDataResults(testResults, howManyDemoResults) # Select only howManyDemoResults of unique SampleNumbers
  rm(howManyDemoResults)

    # Now change SampleNumbers to other Strings
  testResults$SampleNumber <- testResults$SampleNumber %>% str_replace_all(c("1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"))
  subject <- testResults[1, "SampleNumber"] # CHoose a random person (first row) as our new Subject
  #
  #
  # KEY POINT is that when making it into demo data there might be some compounds which WERE found in the bigger set, NOT found in the demo set,
  #     but MIGHT have zero values listed for them??? is that true?
}
rm(makeIntoDemoDataResults)

#### NOW we have testResults which, for every test EXCEPT DRS it has the # of rows = to # of ParameterIDs in masterParameter times # of Wristbands (SampleNumber)
### but for DRS only it has # of combinations of ParameterID and SampleNumber that were positive  NOPE.. i see some zeros... let's stry again
#### but only for DRS sometimes we have a weird combination since some non-numeric values get set to ZERO we have SOME zero result rows and that is weird but happens
#### so.... for DRS we have an indeterminate # of rows.
### so we need to eliminate any zero Results from the data... and add them back later?
##

### THIS just eliminates all ZERO result ROWS for now.... LaTER we will add them back
testResults <- testResults %>%
  filter(Result > 0)

if (testing_PRE_POST) { # THIS IS MARC JUST PLaYING AROUND with PREPOST data to see how we might chart it
  # Create a subset that is only the rows that have BOTH either pre or POST and FORCE DATA manually to have both pre and post examples
  # LATER add a check to be sure the MATCh is on PartName as well

  ### Make sure Name and PrePost not empty

  testResultsPrePost <- testResults %>%
    filter(PrePost != "") %>%
    filter(PartName != "")

  # what PartName do we want to keep?
  #### find out WHICH PartName has a both a PRE and a POST wristband
  testPartNamesWithPairs <- testResultsPrePost %>%
    select(PartName, PrePost) %>%
    unique() %>%
    group_by(PartName) %>%
    filter(n() == 2) %>% ### FIND only those rows where we have TWO PrePost values
    ungroup() %>%
    select(PartName) %>% ### Generate list of names of wristbands that come in pairs
    unique()

  ### Make sure Name and PrePost not empty and also make sure they come in PAIRS
  ### NOW only keep rows that have BOTH a PRE and a POST...
  testResultsPrePost <- testResultsPrePost %>%
    filter(PartName %in% testPartNamesWithPairs$PartName)

  rm(testPartNamesWithPairs)
}

### NOW I should enhance the testResult data with the AIR CALCULATION date when/if we have it.
# CAUTION I'm allowing NAs to be introduced by COERCION in this code and... that is OK but... is a question if I want to handle differently
if (doAIRplusNioshOSHAreporting) {
  testResults <- addAirCalculationInformation(testResults,airConcentrationTable) # This should read-in another lookup table, enhance testResults with new columns
  ### NOW i'm going to add info on NIOSH and OSHA limits to testResults
  if (FALSE) { # THIS BELOW is manual code executed by hand to write out the air concentration values
    testR_to_print <- testResults %>%
      select(ParameterID, ParameterName, CASNumber, Result, ResultOriginal, Days_worn, size_factor, volume_factor, week_factor, BoilingPoint, Test_Or_Opera, Mixed_log_Ksa_BP, log_Ke_BP, Rs_in_L_per_Day, Ca_for_Customer)
    write.csv(testR_to_print, file = "testReslts_AirConcen_Info3.csv", row.names = FALSE)
    rm(testR_to_print)
  }
  testResults <- addAirNioshOsha(testResults,airNioshOshaTable) # This should read-in another lookup table, enhance testResults with new columns
  testResults$MaxNioshOsha <- pmax(testResults$NIOSH, testResults$OSHA, na.rm = TRUE)
}
rm(addAirCalculationInformation,airConcentrationTable,airNioshOshaTable,addAirNioshOsha)

#cat("888 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

# IF we only want to take a SUBSET of the testResults we can do that here by picking only one specific batch number
if (subsetBasedOnBatchNumber) {
  testResults <- onlyPickSomeBatchesFromBiggerData(testResults, batchNumbers)
}
rm(subsetBasedOnBatchNumber,onlyPickSomeBatchesFromBiggerData)

# Save test results with Y Flag set in a separate place... test if Flag CONTAINS a Y
# FIrst try to was to see if EXACTLY was Y which is same-same but still not tech correct
# testResultsWith_Y_Flag <- testResults[testResults[, "Flag"] == "Y", ]
# SO now I'm testing if CONTAINS a "Y" and saving those

## NOTE that Maybe I don't use testResultsWith_Y_Flag anywhere since I also kept FLAG field in testResults for now
## also note the GREP below is probably not needed anymore since I always set FLAG to be exactly "Y" now
# Stop using "testResultsWith_Y_Flag" AND instead use the line: testResults.big[testResults.big[, "Flag"] == "Y", ] cause that cleaner
# testResultsWith_Y_Flag <- testResults[grep("Y", testResults[, "Flag"]), ]

# IF I WANT TO TEST if there were any Y flags in the data, i can use this: nrow(testResultsWith_Y_Flag) >0
# foundAny_Y_values_meaning_non_Quant <- nrow(testResultsWith_Y_Flag) >0


#cat("911 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)


riskCalifProp65 <-
  load.riskCalifProp65(here(riskCalifProp65TableName), masterParam)
rm(load.riskCalifProp65,riskCalifProp65TableName)

#cat("912 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)


epaIris <- load.epaIris(here(epaIrisTableName))
rm(load.epaIris,epaIrisTableName)

#cat("913 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

IARCRisk <- load.IARCRisk(
  here(IARCRiskTableName),
  here(riskIARCdecodeTableName)
  )

rm(load.IARCRisk,IARCRiskTableName,riskIARCdecodeTableName)

#cat("922 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)


# Delete the things I'm sure I won't need
rm(
  list = c(
    "masterParamTableName",
    "data.load.R.filename",
    "r_code"
  )
)


#cat("999 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)


#### NOW I have loaded all the results and classifications.
# testResults + classifications gives me the basic groups and is sorta the master results
# IARCRisk, epaIris, riskCalifProp65 are the RISK databases
# masterParam is maps ParameterID to ParameterName and CASNumber for ALL compounds not just those in testResults





## To WRITE OUT classification file as a CSV which I needed once:
# write.csv(classification, file = "classification.csv", row.names=FALSE)
# write.csv(class_L_maxC, file = "class_L_maxC.csv", row.names=FALSE)

# Makeing a classificaion lookup table for Steven
# AllChemClass<- masterParam %>%
# left_join(class_L,by="ParameterID") %>%
# select(-ParameterID) %>%
# mutate(Result=1) %>%
# spread(key=classification,value=Result,fill=0) %>%
# arrange(-`Polycyclic Aromatic Hydrocarbon (PAH)`)
#
# write.csv(AllChemClass, file = "results_output\\AllChemClass.csv", row.names=FALSE)


## To WRITE OUT masterParam file as a CSV which I needed once:
# write.csv2(masterParam[,2:3], file = "results_output\\masterParam1528.csv", row.names=FALSE)


# Get Scores for each subject (without using sqldf)
# contains how may of each classification each subject got
SubClassScores <- testResults %>%
  filter(Result > 0) %>% # choose only hits
  left_join(class_L, by = "ParameterID", relationship = "many-to-many") %>% # add "classification" value
  select(classification, SampleNumber) %>% # pick columns needed
  count(classification, SampleNumber) %>% # how many of each classification for each wristband
  arrange(SampleNumber) %>%
  dplyr::rename(aggScore = n)


results_W <- testResults %>%
  select(SampleNumber, ParameterName, Result) %>%
  spread(SampleNumber, Result, fill = 0)

rownames(results_W) <- results_W$ParameterName # set rownames
results_W$ParameterName <- NULL # DROP column

##### NOTENOTE NOTE NOTE_- The error catch below is GREAT but the DATA doesn't always MATCH
# cause sometimes testResults will have ALL (zero and nonzero) andsometmes JUST goo results
# if (nrow(masterParam) != nrow(results_W)) {
#     stop ("MyExp DEBUG:  Wrong # of rows in masterParam or in results_W: STOP --> not valid", "Current file name is = ",current_filename())
# }

#cat("999AAA in BASE CODE...\n", file = "debug_log.txt", append = TRUE)



#   THIS AVOIDS ERRORS on runs done before setting flag: Did I write output CSVs yet?
if (!exists("result_file_output_done")) {
  result_file_output_done <- FALSE
}

# create output which I will write to a FILE as CSV for use in EXCEL
#  This all the needed files to analyze the data...
# if (FALSE) {
if (!result_file_output_done) {
  result_file_output_done <- TRUE
  customer_Output(testResults, class_L, masterParam, DataFile_and_OutputFile_Prepend, DartmouthFixup)
  rm(customer_Output)
} # end Customer printing section

#cat("A 111 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)


# FIND any wristbands with ZERO values
# testResults%>%
#   group_by(SampleNumber) %>%
#   summarise(x=sum(Result)) %>%
#   filter(x==0) %>%
#   select(SampleNumber)


##########################################
### NOW add more columns to testResults that will be useful for graphing
#

### BUT I want to make sure that testResults has a ZERO RESULT entry for all tested chemicals
#   that had any values found in this run of data.  This will make the heatmap chart work correctly
#   SO I'm going to use the "complete" command from TidyR with "nesting"

# NOW i'll hold the original testResults just to keep it around temp in case for debugging
##  NOTE i later clean up testResults completely and use testResults.big everywhere
# testResults.hold <- testResults

## NOW i notice that testResults has columns that duplicate-data in the sense that they always are the same with respect to each other
##      so they should be lookup tables  NOT duplicated in testResults.  I should look them up!
#

#### NOTE that maybe I'll need to later delete the zero-result-values from testResult ???


### Lets get a small lookkup table to convert SampleNumber to PureSampleName
sampleLookup <- testResults %>%
  select(SampleNumber, PureSampleName) %>%
  filter(PureSampleName != "NA") %>%
  unique()

# add back any ZERO values for results that should be here
testResults <- testResults %>%
  # select(ParameterID,SampleNumber,Result) %>%  # We're not going to drop columns  Do that LATER as NEEDED
  # IMPORTANT NOTE:  I needed to set all the NUMERIC values to 0 with FILL so that later things didn't break
  complete(ParameterID, SampleNumber, fill = list(Result = 0, ResultOriginal = 0, Days_worn = 0, size_factor = 0, week_factor = 0))


### WE have BROKEN (for some old historical reason) the values of ParameterName and CASNumber by manipulating testResults
###  I will fix that now just by setting them using MasterParam
testResults$ParameterName <- NULL
testResults$CASNumber <- NULL
testResults <- testResults %>% left_join(masterParam, by = "ParameterID")


### WE have BROKEN (for some old historical reason) the values of PureSampleName
###  I will fix that now just by setting them using sampleLookup
testResults$PureSampleName <- NULL
testResults <- testResults %>% left_join(sampleLookup, by = "SampleNumber")


# NEXT we add new columnst to testResults for later use
# BUT we MUST REMEMBER that we have move to lookup tables ParameterID(ParameterName, CASNumber) and SampleNumber( PureSampleName )
# testResults.big has # of rows = total-wristbands-tested multiplied by # of Chemicals found on any wristband.
#       In other words, if NOBODY had that chemical, it is not in the data anywhere... BUT... if ANYONE had it then everyone has a value for it (like ZERO)
testResults.big <- testResults %>%
  group_by(ParameterID) %>%
  mutate(norm_Result = Result / max(Result)) %>% # calc normalized result as % of max found
  filter(!sum(Result) == 0) %>% # and do not deal with any CHEMS  that had no hits
  mutate(quartile = if (length(unique(quantile(Result, probs = 0:4 / 4))) < 2) {
    rep(4, length(Result))
  } else {
    cut(
      Result,
      breaks = unique(quantile(Result, probs = 0:4 / 4)),
      include.lowest = TRUE,
      labels = FALSE
    )
  }) %>%
  mutate(param_max = max(quartile)) %>% # Find maximum value of each chems quartile
  mutate(is_this_max = Result == max(Result)) %>% # Flag if max for each chemical
  # (duplicates quartile=1 but so what)
  ungroup() %>%
  mutate(Result = round(Result, 2)) %>% # Just round result in case some long #'s
  mutate(quartile = quartile + (4 - param_max)) %>% # SHIFT any ranges that top at less then 4 upwards
  mutate(quartile = ifelse(Result == 0, 0, quartile)) %>% # Set initally zero rows to quartile=0
  select(-param_max) # Drop the col I added to calculate quartile shift

#### THIS LINE BELOW FAILS when NA's are in the data for some fields SO i'm going to try to FIX
# testResults.big <- testResults.big %>%
#  replace(is.na(.), "not_found")
##
## THIS NEW VERSION replaces NA with ZERO if numeric, and "not found" if it is character.  Hopefully this doesn't break anything else
testResults.big <- testResults.big %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate_if(is.character, ~ replace_na(., "not_found"))

# Add actual chemical name (ParameterName) to testResults.big
# testResults.big2 <- testResults.big %>%
# left_join(masterParam,by="ParameterID")


# # ADDING CLASSIFICATION to testResults.big BUT NOTE that this creates duplicate rows cause some parameterID have multiple class SO
# #   ONLY use this when necessary to lookup CLASS
# testResults.bigWithClass  <- testResults.big %>%
#   left_join(class_L, by = "ParameterID",relationship = "many-to-many") %>%
#   select(ParameterID,SampleNumber,Result, classification)

# TRYING BELOW TO JUST ELIMINATE TESTRESULTS variable completely... will it work?
rm(testResults)


#cat("A 222 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

############################################## Calculate some general statistics useful in charts

# How many wristbands were tested is the number of unique SampleNumbers in testResults
howManyWristbandsTested <- length(unique(testResults.big$SampleNumber))
# howManyWristbandsTested <- length(unique(testResults$SampleNumber))

# How man WB had at-least-one of the classifications?
SubClassAtleastOne <- testResults.big %>%
  mutate(Result = case_when(
    Flag == "Y" ~ 100, # INCLUDE wristbands with Y value in FLAG
    TRUE ~ Result
  )) %>% # IF we have a Y flag on an item set the ZERO value to 100
  filter(Result > 0) %>% # choose only hits
  inner_join(class_L, by = "ParameterID", relationship = "many-to-many") %>% # add "classification" value and add many-to-many catch
  select(classification, SampleNumber) %>% # pick columns needed
  unique() %>%
  group_by(classification) %>%
  summarize(WristbandsWithThisClassificaion = dplyr::n()) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>% # Eliminate classification as a FACTOR so it sorts correctly
  dplyr::arrange(classification) %>%
  mutate(percentageWBwithClassification = percent(round(WristbandsWithThisClassificaion / howManyWristbandsTested, 2))) %>%
  dplyr::rename(
    "Classification" = classification,
    "Wristbands with This Classification" = WristbandsWithThisClassificaion,
    "Percentage with this Classification" = percentageWBwithClassification
  )


# ??See total # of reported chemicals found on ANY wristband in this set
howManyUniqueChemFoundAcrossAllWristbands <- length(unique(testResults.big$ParameterID))

# New version 3/7/2019
minMaxTR <- testResults.big %>%
  mutate(Result = case_when(
    Flag == "Y" ~ 100, # a Y flag means it WAS detected but the VALUE could not be determined
    TRUE ~ Result
  )) %>% # IF we have a Y flag on an item set the ZERO value to 100
  select(SampleNumber, ParameterID, Result) %>%
  group_by(SampleNumber) %>%
  summarise(Count = sum(Result > 0)) %>%
  arrange(desc(Count))

if (makeIntoDemoData) { #   Use this to set SUBJECT to min max middle as way of testing the various messages... if i want...
  #subject <- minMaxTR$SampleNumber[2] ## HaRD CODE TO one less than MAX SUBJECT for DEMO DATA
  # subject <- minMaxTR$SampleNumber[round(nrow(minMaxTR)/2,0)]   ## Hard code to average # of compounds
  # subject <- minMaxTR$SampleNumber[nrow(minMaxTR)-1]  ## ## Hard code to one less than MIN # of compounds
  #subject <- "AA90GCC"  ### Hardcoded to make into the one person with highest of a compound in agricultural and pharm products...Bis(2-ethylhexyl)phthalate   AA90GCC   856000   Agricultural & Pharmaceutical Chemicals
  subject <- minMaxTR$SampleNumber[round(nrow(minMaxTR)*.2,0)]   ## Hard code to 80% of # of compounds
}
rm(makeIntoDemoData)


#cat("A 333 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

maxChemFoundOnAnyOneWristband <- max(minMaxTR$Count)

# What is Minimum chemicals found on ANY wristband in this set
minChemFoundOnAnyOneWristband <- min(minMaxTR$Count)

##### NOTENOTE NOTE NOTE_- The error catch below is GREAT but the DATA doesn't always MATCH
# cause sometimes testResults will have ALL (zero and nonzero) andsometmes JUST goo results
# if (minChemFoundOnAnyOneWristband <= 0) {
#     stop ("MyExp DEBUG:  FOUND NO CHEMICALS on a WRISTBAND... is that right See BASE_CODE for STOP statement???", "Current file name is = ",current_filename())
# }

stdDevChemFoundOnAnyOneWristband <- signif(sd(minMaxTR$Count), 3)

# Remove minMaxTR because it SHOULDN'T be needed any more
rm(minMaxTR)

## Calculate statistics of min/max/etc of occurences of chemicans across wristbands
#   Use only >0 values
statSummary <- testResults.big[testResults.big$Result > 0, ] %>%
  select(ParameterName, Result) %>%
  group_by(ParameterName) %>%
  summarise(
    MinResult = signif(min(Result), 3),
    MaxResult = signif(max(Result), 3),
    MeanResult = signif(mean(Result), 3),
    MedianResult = signif(median(Result, 3)),
    Count = signif(dplyr::n(), 3),
    Standard_Deviation = toString(signif(sd(Result), 3))
  ) %>%
  replace(is.na(.), "Undefined") %>%
  dplyr::rename(Chemical_Name = ParameterName)

statSummary[statSummary$Standard_Deviation == "NA", ]$Standard_Deviation <- "Undefined"
statSummary_with_ParameterID <- statSummary %>% left_join(masterParam, by = c("Chemical_Name" = "ParameterName"))
if (nrow(statSummary) != nrow(statSummary_with_ParameterID)) {
  stop("Stopped because left_join on statSummary changed row count  Current file name is = ", "MyExp_Base_Code_???")
}
statSummary_with_ParameterID$CASNumber <- NULL
# statSummarywithParameterID could NOW be used to play against the classification table (probably use class_L) to group/sum organize stat summary summhow BUT probably should do this to the table we originally READ IN
# So probably need to WRITE OUT the 682 table with the correct information BECAUSE even then... how ... hmmm..
### ACTUALLY we already have a table uf U oricess
###### we have "2.9 Chemical Classification on every Wristband" which ... can give us min/max/AVERAGE # of chemicals in commerce / PAH /
##### and then we can calculate average/min/max of # each type  (not the average NG but the average # of compounds)
###
### CAn also generically add a chage showing "Total NG of PAH, total NG of Pesticide, TOtal NG of... etc... for each compound class"
### AND THEN
### could add a comparison of that chart to other chart with same data for BIG dataset.

# READ IN the file I OUTPUT of (682 but may change) statistical data
CombinedTestDataStatSummary <- read.csv(here("data/CombinedTestData/StatSummary682.csv")) %>%
  replace(is.na(.), "Undefined")
# Add ParameterID to table
### NOTE that all BATCH # < 100 are dartmoutnh... and then we have 200 and 300 and 400 and then are CHICAGO, COLORADO, and GEORGETOWN probalby in taht order.


#cat("A 444 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

# write.csv(statSummary, file = "data/CombinedTestData/StatSummary682.csv", row.names=FALSE)


# NOW try to combine statSummary with CombinedTestDataStatSummary
bigStatSummary <- CombinedTestDataStatSummary %>% left_join(statSummary, by = "Chemical_Name")
# bigStatSummary$Full_ST_Dev<- as.character(bigStatSummary$Full_ST_Dev)
bigStatSummary[is.na(bigStatSummary)] <- ""


if (CombinedTestData) {
  bigStatSummaryJustTheseChemicals <- statSummary %>% left_join(CombinedTestDataStatSummary, by = "Chemical_Name")
  # bigStatSummaryJustTheseChemicals$Full_ST_Dev        <- as.character(bigStatSummaryJustTheseChemicals$Full_ST_Dev       )
  bigStatSummaryJustTheseChemicals[is.na(bigStatSummaryJustTheseChemicals)] <- ""
}

#cat("933 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

## Create table summarizing average # of chemicals of each classification found in wristbands
csSummary <- as.data.frame(
  left_join(testResults.big, class_L, by = "ParameterID", relationship = "many-to-many") %>%
    mutate(Result = case_when(
      Flag == "Y" ~ 100, # COUNT as having value when Y is FLAG
      TRUE ~ Result
    )) %>% # IF we have a Y flag on an item set the ZERO value to 100
    select(classification, SampleNumber, Result) %>%
    group_by(classification, SampleNumber) %>%
    summarise(countResult = sum(Result > 0), sumResult = sum(Result)) %>%
    group_by(classification) %>%
    summarise(
      countMeanResult =
        signif(mean(countResult), 2),
      meanSumResult =
        signif(mean(sumResult), 2)
    )
)

#
## Read in Table that is the big combined dataset to compare with the smaller dataset
#
CombinedTestDataClass_StatSummary <- read.csv(here("data/CombinedTestData/csSummary682.csv")) %>%
  rename(combinedTestDataCountMeanResult = countMeanResult, combinedTestDataMeanSumResult = meanSumResult)

csSummary <- CombinedTestDataClass_StatSummary %>%
  left_join(csSummary, by = "classification") %>%
  replace(is.na(.), 0)

#cat("944 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)
###

# rm(csSummary,csSummary1,csSummary2   )
# write.csv(csSummary, file = "data/CombinedTestData/csSummary682.csv", row.names=FALSE)
# CombinedTestDataStatSummaryClassifications<-read.csv("data/CombinedTestData/csSummary682.csv")%>%
#   replace(is.na(.), "Undefined")
#

##############################################

###
# Little section to help analyze some data... all this should be commented out... 4/14/2020
### FIRST let's fill in the FSES_NAME column for Stevens spreadsheet
# First let's eliminate duplicate CASNumbers
# holdmP<- masterParam %>% group_by(CASNumber) %>% top_n(1,ParameterID)
# write.csv(holdmP, file = "masterParamLookup_v2.csv", row.names=FALSE)


# hold <- testResults %>% group_by(CASNumber)  %>% top_n(1,Result) %>%  filter(Result> 0) %>% arrange(CASNumber)
# hold <- hold %>% select(SampleNumber, ParameterID, CASNumber, Days_worn,Wristband_Size, Result )
# write.csv(hold, file = "testReslts_maxResult_Dartmouth_all459_v2.csv", row.names=FALSE)

