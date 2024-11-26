#
# Set the key variables needed to run the MyExposome Data Analysis
# Some variables change between different results from same test
# Some variables change between different tests (flame vs drs vs pah)
# Some variables may change as tests are upgraded to new versions of same test
#
# Variables are:
#   subject :  Which of wristbands to focus on in the report
#   resultsTable: What is the name of the file which has the results
#   master paramater table:  What is the name of the file which has the list of all tested-for chemicals
#
# ALSO:
#   classificationTableName:  Table that classifies all the (currently 1528) chems into chem categories
#   riskCalifProp65TableName: Which of (currently 1528) are flagged by california
#   epaIrisTableName : Which are flagged by EPA
#   IARCRiskTableName : Which are flagged by    IARC
#
#   Also:
#       Use this to set NAMES of R and RMD files to load so don't need to go change every file
#
# CAUTION:  NEED TO SET ON THING WE CAN'T SET FROM HERE...  DO IT MANUALLY:
#
#     IN the RMD file... need to change, at the TOP, the WORD DOC used for format things IF IT CHANGES
#
########## #####################################
#

#
#
#  MyExposome_1527_v6.Rmd                   The main producer of the HTML or DOCX or PDF (run interactively OR called once-per-subject by Print_All_Subjects)
#    IndividualResultsAnalysis_text.Rmd       The text and code at the NEAR of the HTML for a specific user (don't use this if doing only group summary)
#    SpecificSubjectAnalysisIntro_text.Rmd    The small bit of TEXT at the TOP of the HTML which summarizes results for individual users
#    GroupAnalysisIntro_text.Rmd              The small bit of TEXT at the TOP of the HTML which summarizes results for GROUP SUMMARY output
#    loreal_venn_text.Rmd                     Some specific VENN Diagrams for L'Oreal
#    CompoundClassification_text.Rmd      small bits of text to put "compound calssification info into document
#
#  MyExp_set_key_variables.R                This file setups variables.  Called by MyExposome_1527_v6.Rmd
#  MyExp_Base_Code_v6.R                     MAIN CODE to produce all the required data tables. (should not have any subject-specific stuff in it)
#  MyExp_support_functions_1527_v6.R        Place to put all support functions (for now it just does "mesg.v" setup)
#  MyExp_data.load.functions_1527_v6.R      Called from Base Code to load all the data tables from CSVs on disk
#  MyExp_Print_All_Subjects_1527_v6.R       Big loop which calls MyExposome_1527_v6.Rmd for each of the subjects listed in the data

#  MyExposomeFormat_1527_v6.docx
#

# MAYBE NEED to set a JAVA_HOME for some things to load/compile
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre7")

# Set working directory to be at root of project
library(here)
setwd(here::here())

# SETUP key environment stuff:
#      Setup data directory
if (!file.exists(here::here("data"))) {
  dir.create(here::here("data"))
}

#########  The following functions tell the code about the directory structure.  Where are the different kinds of files.
# Set up name of R CODE files
setRdirectory <- function(fileName) {
  here::here("R", fileName)
}
# Set up location of AIR concentration files
setAIRdirectory <- function(fileName) {
  here::here("AirConcentration", fileName)
}

# Set up location of Fixup  files
setFIXUPdirectory <- function(fileName) {
  here::here("data", "data_FixupSampleKey", fileName)
}

# Set up location of CUSTOMER DATA  files
setCUSTOMERdirectory <- function(fileName) {
  here::here("data", "data_Customers", fileName)
}

# Set up location of CUSTOMER DATA  files
setMASTERPARAM_CLASS_RISKSdirectory <- function(fileName) {
  here::here("data", "data_MasterParams_Class_Risks", fileName)
}


# Set up location of COMBINED ALL DRS  DATA  files
setCOMBINED_DRSdirectory <- function(fileName) {
  here::here("data", "CombinedTestData", fileName)
}

################ FOR SPECIFIC CUSTOMERS SET FLAGS HERE and include/not-include RMD files

# Adjustment STANDARD SETTINGS are below... we then REVERSE these settings in the customer-specific-section if we want to
#
wristbands_weight_adjusted <- FALSE
wristbands_time_adjusted_one_day <- FALSE # ADd text messages about TIME-ADJUSTING VALUES to ONE-DAY
wristbands_time_adjusted_one_week <- FALSE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK

wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
wristbands_time_and_weight_adjusted <- FALSE
wristbands_week_and_weight_adjusted <- FALSE

wristbands_time_adjusted_one_week_not_weight <- FALSE
wristbands_time_adjusted_not_weight <- FALSE


# CUSTOMER SPECIFIC FIX UP (really just means for-which-customer-is-this)
#  NOW we have "FixupForAnyone" to say if there is ANY fixup... and then specifc values set
FixupForAnyone <- any(
  DartmouthFixup <- FALSE, # is to to force weird division and fix up
  WisconsinFixup <- FALSE,
  LorealFixup <- FALSE,
  BuffaloFixup <- FALSE,
  UCSFplusRandom10Fixup <- FALSE,
  UCSF2020Fixup <- FALSE,
  UMTFixup <- FALSE, # NEVER IMPLEMENTED THIS>.. BE CAREFUL...
  UNMFixup <- FALSE, # adding University New Mexico 1/14/2022  NEVER IMPLEMENTED THIS BE CAREFUL (MUST BE IMPLEMENTED NOW I THINK???)  HUH?  I think this is OK now?
  # NOTE that UNM has an old version from 2022 and a new version with 20 wb from 2023    Using same FLAG but different fixup and resultstable settings
  COLORADOFixUp <- FALSE,
  ULILLEFRANCEFixup <- FALSE,
  UCONNFixUp <- FALSE,
  CHICAGOFixUp <- FALSE,
  GEORGETOWNFixUp <- FALSE,
  SBIR_P1_May2022Fixup <- FALSE,
  UC_DAVISFixup <- FALSE,
  UniVisionFixup <- FALSE,
  CombinedTestData <- FALSE,
  BostonFixup <- FALSE,
  LouisvilleFixup <- FALSE,
  UFL_FloridaFixup <- FALSE,
  DartmouthFixup <- FALSE, # is to to force weird division and fix up
  CombinedDRS_November6_2024 <- FALSE,
  SBIR_P2_Part1_71_FixUp <- TRUE # ALWAYS MAKE LAST ONE TRUE, REST FALSE
)

### USE the flag RMD_type to indicate if FLAME or PAH or DRS_MAS15 etc...
#     NOTE:  Try to set to "nothing" unless sure so you remember to set it!
#
#### NOTE:  I moved setting which TEST we're doing up here so can use value earlier...
#
### USE the flag RMD_type to indicate if FLAME or PAH or DRS_MAS15 etc...
#     NOTE:  Try to set to "nothing" unless sure so you remember to set it!
# OLD OLD OLD RMD_type <- 'VOC'  # This WAS original VOC Thermal Desorption Test now discontinued DO NOT MIX UP WITH VOPAH
# RMD_type <- 'PAH'
# OLD OLD OLD  RMD_type <- 'VOPAH' ## although called VOPAH it is actually solvent VOC only with SPE clean.  It WAS VOC+PAH for a while but not successfully
# RMD_type <- 'VOC_2024' ## New VOC list of 18 compounds, so this is 3rd VOC list (VOC, then VOPAH, now VOC_2024)
# RMD_type <- 'PEST'
# RMD_type <- 'FLAME'
RMD_type <- "DRS"
# RMD_type<-'SBIR_P1_DRS_plus'
# RMD_type <- 'PHTH'



show_loreal_venn_text <- FALSE # Include L'Oreal-specific RMD files and text
Big_Mas15_List_Fixup <- FALSE # IDea is to do the UCSF individual wristband we will keep a running set of ALL data
Miami_Firefighters_2017 <- FALSE

# rm(Miami_Firefighters_2017,Big_Mas15_List_Fixup,show_loreal_venn_text)


# SET THINGS TO BLANK that are used globally... probably bad idea
testDoneFor <- ""
DataFile_and_OutputFile_Prepend <- ""

### SET expected units to default of ng/WB and then CHANGE if needed for some specific test
ExpectedUnits <- "ng/WB" ### PROBABLY do NOT change this value... change it ELSEWHERE for some specific test

### IF we want to allow different parameter counts between masterParameterFile and testResultsFile set this to TRUE
### We may ahve a problem with WISCONSIN VOC of 32 vs 33 compounds... this SHOULD catch it and then i'll fix somewhere/somehow
allowDifferentParameterCounts <- FALSE


###
# FLAG to tell us if we should do TEST of PRE-POST comparison (before and after)
testing_PRE_POST <- FALSE # RIGHT NOW ONLY WISCONSIN has pre-post info so set this in WISCONSIN to TRUE sometimes


### HERE we should be putting all the customer-specific stuff.  IF there are multiple analysis per one customer then we if (?) else (?) it here
## CAUTION:  Now putting resultsTableName and FixupFile and "subject" all in custoemr specific area along with all other info about the DATA or CUSTOMER
###       the TEST ONLY SPECIFIC info goes with the test
if (CombinedDRS_November6_2024) {
  resultsTableName <- "full_list_of_all_DRS_resultsNov2024withParameterID.csv" #  NOW DOING SBIR Phase 2 first group of 71
  FixupFile <- NULL
  subject <- "40-WB" # MARC Random one from Combined Data Set
  # subject <- "25-WB"  # Judy LaKind
  # subject <- "26-WB"  # Eric Epstein

  wristbands_time_adjusted_one_day <- FALSE # ADd text messages about TIME-ADJUSTING VALUES to ONE DAY
  wristbands_time_adjusted <- FALSE
  wristbands_time_and_weight_adjusted <- FALSE
  wristbands_week_and_weight_adjusted <- FALSE
  # wristbands_day_and_weight_adjusted <- FALSE # I just added this.  Not sure it is ever going to be used but mirrors the one for week and weight so we'll see...
  testDoneFor <- "a MyExposome Wristband Study" ### COMMENT THIS OUT TO REMOVE FROM REPORT
  # testDoneFor <- ""
  ExpectedUnits <- "ng/g"
} else if (SBIR_P2_Part1_71_FixUp) {
  resultsTableName <- "F24-22_MyExpoP.O.#259_CoA-WBdata.csv" #  NOW DOING SBIR Phase 2 first group of 71
  FixupFile <- "SBIR_NIH_Part1_71_SampleKey.csv"
  # subject <- "A241133" # Random one from SBIR P2 Group 1 of 71
  # subject <- "A241137" # Random another  from SBIR P2 Group 1 of 71
  # subject <- "A241139" # Random another  from SBIR P2 Group 1 of 71
  # subject <- "A241264" # Random another  from SBIR P2 Group 1 of 71          105-WB	A241264
  # subject <- "A241149"  # this is 40-WB which is MARC
  subject <- "A241198" # this is 33-WB which is someone with a chompound beinning with 4



  wristbands_time_adjusted_one_day <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE DAY
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- FALSE
  # wristbands_day_and_weight_adjusted <- TRUE # I just added this.  Not sure it is ever going to be used but mirrors the one for week and weight so we'll see...
  # testDoneFor <- "a MyExposome Wristband Study"     ### COMMENT THIS OUT TO REMOVE FROM REPORT
  testDoneFor <- ""
  ExpectedUnits <- "ng/WB"
} else if (UniVisionFixup) {
  FixupFile <- "Sample Key_Univision_lookup_table.csv"
  testDoneFor <- "TelevisaUnivision "
  if (RMD_type == "PEST") {
    wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
    wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
    wristbands_time_and_weight_adjusted <- FALSE
    wristbands_week_and_weight_adjusted <- FALSE
    wristbands_time_adjusted_one_week_not_weight <- TRUE
    ExpectedUnits <- "ng/g"
  } else if (RMD_type == "DRS") {
    wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
    wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
    wristbands_time_and_weight_adjusted <- TRUE
    wristbands_week_and_weight_adjusted <- TRUE
    ExpectedUnits <- "ng/WB"
  }
} else if (UC_DAVISFixup) {
  FixupFile <- "Sample Key_UCDavis_lookup_table.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UC Davis"
  ExpectedUnits <- "ng/WB"
} else if (CombinedTestData) {
  FixupFile <- setCOMBINED_DRSdirectory("Fixup_Dart_george_Chic_col.csv") # NIH SBIR-ID data_Enhanced_Fixup
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "MyExposome Wristband Project"
  ExpectedUnits <- "ng/WB" # IS THIS RIGHT... VERY CONFUSING>>> NEEC TO REVIEW WITH STEVEN WHAT THE UNITS ARE and what the FIXUP FILE is...
} else if (SBIR_P1_May2022Fixup) {
  FixupFile <- "NIH SBIR-ID data_Enhanced_Fixup.csv" # NIH SBIR-ID data_Enhanced_Fixup
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "MyExposome Wristband Project"
  ExpectedUnits <- "ng/WB" # IS THIS RIGHT... VERY CONFUSING>>> NEEC TO REVIEW WITH STEVEN WHAT THE UNITS ARE and what the FIXUP FILE is...
} else if (DartmouthFixup) {
  #  THIS IS FOR FIRST 48:  "First48_to_normalize_data.csv",    ### NEXT is for next 25
  FixupFile <- "Dartmouth/Dartmouth All IDs_2024_04_25.csv" # Batch # 15, 16, 17...21 22 23 24 25 26
  resultsTableName <- "Dartmouth/Dartmouth 1_2_etc_13_etc_20_21_22_23_24_25_26_for_1166_FixPO246units.csv" # Probably FINAL Dartmouth
  subject <- "A200961" # THIS is random one in DARTMOUTH BATCh 13

  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "Dartmouth"
  DataFile_and_OutputFile_Prepend <- "Dartmouth" # ALWAYS USE THE SLASH at end, NO:  GET RID OF SLASH as moving to here::here() system
  ExpectedUnits <- "ng/WB"
} else if (UCSF2020Fixup) {
  FixupFile <- "Fixup_UCSF_1_plus_27_plus_1_Sample login info - 052419_fixed_BY_MARC_ADD_FSES_REPAIR_PARTICIPANT_NAME.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UCSF Program on Reproductive Health and the Environment (PRHE)"
} else if (UMTFixup) { #  (MO = UMT)        THIS IS MONTANA !!!!!
  # Trying to do NO adjustment on MONTANA
  FixupFile <- "Sample Key_MO.csv" #  (MO = UMT)
  wristbands_time_adjusted_one_week <- FALSE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- FALSE
  wristbands_week_and_weight_adjusted <- FALSE
  testDoneFor <- "University of Montana, School of Public and Community Health Sciences "
  #
} else if (UNMFixup) { #  (MO = UMT)       THIS IS NEW MEXICO
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  ### HUH?  We ARE going to do adjust for UNM at least for the 2nd batch for SURE
  # FixupFile <- "Sample Key_NM_lookup_table.csv" # THIS IS FIRST BATCH
  FixupFile <- "Sample Key_UNM_2nd_batch_lookup_table.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of New Mexico "
} else if (COLORADOFixUp) { #        COLORADO
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  # FixupFile <- "Sample Key_UnvColorado_FixUp.csv" #  THIS IS FOR FIRST BATCH april 2022
  # FixupFile <- "Sample Key_UnvColorado_2ndVERSION_lookupTableJUST_NEW_DATA.csv" #  THIS IS FOR JUST 2nd batch Nov 2023
  FixupFile <- "Sample Key_UnvColorado_2ndVERSION_lookupTable.csv" #  THIS IS FOR First AND 2nd BATCH (maybe batch specific NOT NEEDED?)
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  # ExpectedUnits <- "ng/WB"
  # ExpectedUnits <- "pg/uL"  ####  NOTE WEIRD HACK had to change from ng/WB to pg/uL to make it work... but they are SAME THING!!!!!
  #### changed BACK to ng/WB after I fixed the data
  ExpectedUnits <- "ng/WB"
  testDoneFor <- "Colorado School of Public Health "
} else if (ULILLEFRANCEFixup) { #        COLORADO
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  FixupFile <- "Sample Key_UNIV_LILLE_FRANCE_lookup_table.csv.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  # ExpectedUnits <- "ng/WB"
  # ExpectedUnits <- "pg/uL"  ####  NOTE WEIRD HACK had to change from ng/WB to pg/uL to make it work... but they are SAME THING!!!!!
  #### changed BACK to ng/WB after I fixed the data
  ExpectedUnits <- "ng/WB"
  testDoneFor <- "Université de Lille "
} else if (LouisvilleFixup) { #        Louisville-JUST 4 I THINK

  FixupFile <- "Sample Key - Louisville Tehran_lookup.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  ExpectedUnits <- "ng/WB"
  testDoneFor <- "Louisville"
} else if (UCONNFixUp) { #        University of Connecticut
  FixupFile <- "Sample Key_UCONNHealth_Fixup.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  # ExpectedUnits <- "ng/WB"
  # ExpectedUnits <- "pg/uL"  ####  NOTE WEIRD HACK had to change from ng/WB to pg/uL to make it work... but they are SAME THING!!!!!  Fixed from PETE so switching BACK
  ExpectedUnits <- "ng/WB"
  testDoneFor <- "University of Connecticut"
} else if (CHICAGOFixUp) { #  CHICAGO
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  FixupFile <- "Sample Key_UnvChicago_FixUp.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of Illinois Chicago, School of Public Health "
} else if (GEORGETOWNFixUp) { #  GEORGETOWN
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  FixupFile <- "Sample Key_Georgetown_PHTH.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "Georgetown Lombardi Comprehensive Cancer Center "
  if (RMD_type == "PHTH") {
    ExpectedUnits <- "ng/g"
    wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
    wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
    wristbands_time_and_weight_adjusted <- FALSE
    wristbands_week_and_weight_adjusted <- FALSE
    wristbands_time_adjusted_one_week_not_weight <- TRUE
    wristbands_time_adjusted_not_weight <- TRUE
  }
} else if (WisconsinFixup) {
  # FixupFile <-"Wisconsin ID and sample info_to_normalize.csv"
  # FixupFile <-"Fixup_Sample Key_WI_Client3_LookupTable.csv"
  # FixupFile <-"Sample Key_WI_Client3_LookupTable.csv"
  # FixupFile <- "Sample Key_WI_Client3a_LookupTable.csv"  #Tweaked to add one more First/Second pair
  FixupFile <- "Sample Key_WI_Client3_April_2024.csv" # Tweaked to be JUST AND ONLY the April 2024 Data Delivery
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- FALSE
  wristbands_week_and_weight_adjusted <- FALSE
  wristbands_time_adjusted_one_week_not_weight <- TRUE
  wristbands_time_adjusted_not_weight <- TRUE

  # ExpectedUnits <- "ng/WB" ## This WAS ng/WB for prevoius PO 206 etc... BUT... for PO 211 it seems to have become ng/g instead  THEREFORE do NOT need to do weight fix-up, just time fix-up?
  # Expected Units USED TO BE for PO206 ng/WB but we redid ALL the wisconsin info clean & fresh to drop thermal desorption and move to solvent extracdt + SPE clean so.. now all ng/g
  ExpectedUnits <- "ng/g"

  testing_PRE_POST <- FALSE # RIGHT NOW ONLY WISCONSIN has pre-post info so set this in WISCONSIN to TRUE    but then i need to do something special setup table SO N EVER MIND THIS

  testDoneFor <- "Medical College Wisconsin"
} else if (LorealFixup) {
  FixupFile <- "loreal_fixup_info2.csv"
  testDoneFor <- "L'Oreal"
  ExpectedUnits <- "ng/WB"
} else if (BuffaloFixup) {
  FixupFile <- "FixupFile_UBuffalo_10_030719.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of Buffalo"
  ExpectedUnits <- "ng/WB"
} else if (BostonFixup) {
  FixupFile <- "Sample Login Key - Boston.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "Boston University School of Public Health"
  ExpectedUnits <- "ng/WB"
} else if (UFL_FloridaFixup) {
  FixupFile <- "Sample Key UFL Florida August 2024.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of Florida, College of Medicine"
  ExpectedUnits <- "ng/WB"
} else if (UCSFplusRandom10Fixup) {
  FixupFile <- "FixupFile_UCSF_1_Plus_Random10.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UCSF (one wristband with 10 random additional for comparison purposes)"
  ExpectedUnits <- "ng/WB"
} else if (Big_Mas15_List_Fixup) {
  FixupFile <- "Big_Fixup_File.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UCSF (one wristband with MANY additional for comparison purposes)"
  ExpectedUnits <- "ng/WB"
} else if (Miami_Firefighters_2017) {
  wristbands_time_adjusted_one_week <- FALSE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- FALSE
  wristbands_week_and_weight_adjusted <- FALSE
  wristbands_weight_adjusted <- TRUE
  testDoneFor <- "Miami Firefighters Project"
  ExpectedUnits <- "ng/WB"
}

### NOTE:  All FIXUP FILE results are in the directory ./data/data_FixupSampleKey directory fix the location from generic file name to actual location
###     SO all fixup files should be just set to the specific file name, then converted to directory location HERE:
if (!is.null(FixupFile)) {
  FixupFile <- setFIXUPdirectory(FixupFile)
}

rm(Miami_Firefighters_2017, show_loreal_venn_text)

# IF you are wanting to ALSO output a big stat-summary comparing this data to the 682 unit dataset I created in a separate subdirectory
# OutputBigComparisonStatTable <-TRUE
OutputBigComparisonStatTable <- FALSE

# IF you want to make a DEMO DATA SET froma real dataset
# set all parameters above as for REAL dataset but then set these two flags below:
#

makeIntoDemoData <- FALSE
if (makeIntoDemoData) {
  testDoneFor <- "MyExposome participants"
  howManyDemoResults <- 10 #  I use 682 to generate the big CombinedTestData set of data.   I used 350 for Silent Spring
}

#
#  If we want to pick only one batch of many in testResults data, mark that here.
#   NOTE this is mostly for DARTMOUTH right now just in case we want to give them subsets based on batch numbers someday
#
subsetBasedOnBatchNumber <- FALSE # DO we want to only pick a subset of the testResults based on BATCh NUMBER
subsetBasedOnBatchNumberPickRandomSubject <- FALSE # Not implemented Yet
batchNumbers <- c("")
# For WISCONSIN we have "1 - PO 206", "2 - PO 221", "3 - PO 225", "4 - PO 231"   (AND LATER WE add in april 2024 251 BUT not sure why this subset info is here)
if (subsetBasedOnBatchNumber) {
  # batchNumbers<-c("15","16","17") #this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults (Maybe this is a dartmouth line?)
  # batchNumbers<-c("16") #this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults (Maybe this is a dartmouth line?)

  batchNumbers <- c("13") # this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults (Maybe this is a dartmouth line?)
  subsetBasedOnBatchNumberPickRandomSubject <- TRUE # NOT USING THIS YET

  # batchNumbers<-c("1 - PO 206") #this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults
  # batchNumbers<-c("2 - PO 221") #this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults
  # batchNumbers<-c("3 - PO 225") #this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults
  # batchNumbers<-c("4 - PO 231") #this is a STRING not a number and is the value of the Lab_Submission_Batch number in testResults
}

######   SET THIS TO make ONLY into stand-alone report of all data, NOT report on specific individual
# NOTE: Normally next 2 (HideIndividualization,DoSpecificSubjectAnalysis are set to different things) < HUH?  Set to SAME things, righit????>
#

whatKindReportToDo <- "SpecificSubject" # Either "Group" or "SpecificSubject" or "GroupAndSubject"
# ONLY DO ONE OF THESE THREE THINGS
if (whatKindReportToDo == "Group") { ## JUST do GROUp Analysis
  DoSpecificSubjectAnalysis <- FALSE # idea is to do specific-subject-analysis output with or without GROUP analysis
  DoGroupAnalysis <- TRUE # The idea is to be able to turn on or off doing the GROUP analysis with or without INDIVIDUAL analysis
  HideIndividualization <- !DoSpecificSubjectAnalysis # idea is to hide, on the charts, the things that colorize charts for a specific individual
} else if (whatKindReportToDo == "SpecificSubject") { ## JUST do SPECIFIC SUBJECT Analysis
  DoSpecificSubjectAnalysis <- TRUE # idea is to do specific-subject-analysis output with or without GROUP analysis
  DoGroupAnalysis <- FALSE # The idea is to be able to turn on or off doing the GROUP analysis with or without INDIVIDUAL analysis
  HideIndividualization <- !DoSpecificSubjectAnalysis # idea is to hide, on the charts, the things that colorize charts for a specific individual
} else if (whatKindReportToDo == "GroupAndSubject") { # do SPECIFIC SUBJECT Analysis AND GROUP ANALYSIS
  DoSpecificSubjectAnalysis <- TRUE # idea is to do specific-subject-analysis output with or without GROUP analysis
  DoGroupAnalysis <- TRUE # The idea is to be able to turn on or off doing the GROUP analysis with or without INDIVIDUAL analysis
  HideIndividualization <- !DoSpecificSubjectAnalysis # idea is to hide, on the charts, the things that colorize charts for a specific individual
}


### NOW OVERRIDE above 2 settings IF you are calling this from PRINT_ALL_SUBJECTS
# In_Print_All_Subjects <- TRUE # USE this when setting key variables below to ALWAYS set  customiization-per-user to be ON

if (exists("In_Print_All_Subjects")) {
  if (In_Print_All_Subjects == TRUE) {
    HideIndividualization <- FALSE # idea is to hide, on the charts, the things that colorize charts for a specific individual
    DoSpecificSubjectAnalysis <- TRUE # idea is to do BOTH group analysis and specific-subject-analysis output << THIS CHANGED, i think print-all-subjects doesn't always do group>>
  }
}
# DoGroupAnalysisOnly <- !DoSpecificSubjectAnalysis # If NOT doing specific subject analysis then need new INTRO


# FOR many reports we do NOT yet have classification info so for those we should HIDE this for now
#     that is EVERY REPORT except for DRS / MAS15 at the moment
#
# HideClassificationInformation <- FALSE #  Set to TRUE for every report EXCEPT DRS
HideClassificationInformation <- TRUE #    Set to TRUE for every report EXCEPT DRS


# This flag blocks/adds 3 sections to the GROUP part of report regarding findings in looking up chemicals in databases
#  EVENTUALLY will set this to TRUE all the time but.... starting as FALSE cause haven't tested it yet.
#   IT WORKS.... leave as TRUE
DoGroupDatabaseLookupReporting <- TRUE # NOW setting this to tRue all the time I think But we sometimes do NOT do group reporting but this is tested only in the GROUP section so is fine

#
# Flag to determine if we IGNORE any wristband that had NO RESULTS AT ALL (should NORMALLY BE SET TO FALSE)
#     Set to TRUE only if CUSTOMER has indicated that "BLANK WB should be dropped" (happened in case of Miami Firefighters)
#
DropAllZeroSampleNumbers <- FALSE
# DropAllZeroSampleNumbers<-TRUE  ##  THIS IS FOR FIREFIGHTER ONLY

#
# IF IN a specific data set to be read in, there are one or more specific samples to IGNORE COMPLETELY then use this flag to ignore them.
DropSpecificWristbands <- c("") # THIS REALLY SHOULD BE IN A SPECIFIC SECTION and not a global variable like this probably
# DropSpecificWristbands <- c("A170241")  #  THIS IS FOR FIREFIGHTER ONLY
#

# If in a specific run we want to IGNORE certain COMPOUNDS that may be show up in the data for some users
#   We need to exclude these chemicals from the MasterParameter list PLUS from the RESULTS DATASET
# DropSpecificChemicals <- c("300387","302096","302097","300386","302095","69","302101","300685","302100","146","208","210","300388","482","595") # I THINK this is for Miami?
DropSpecificChemicals <- c("")
if (WisconsinFixup & RMD_type == "VOPAH") { # STEVEN figured out that these two chemicals, for the VOPAH method, for Wisconsin, are suspect
  DropSpecificChemicals <- c("906", "880")
}

PAH_text_string <- "Polycyclic Aromatic Hydrocarbons (PAHs)"
VOC_text_string <- "Volatile Organic Compounds (VOCs)"
VOC_2024_text_string <- "Volatile Organic Compounds (VOCs)" # Revised 3rd time VOC test
# VOPAH_text_string = "VOC and SVOC"  #  CAREFUL wit this caus is really combo of VOC and PAH, not a new unique classification really
VOPAH_text_string <- "Volatile Organic Compounds (VOCs)" #  CAREFUL this USED TO BE VOPAH but now really is back to being just VOC via new method?
# OPAH_text_string = "Oxygenated PAH"  # DROPPED REPORTING this after 6/19/2019
flameRetardant_text_string <- "Flame Retardant"
PCB_text_string <- "Polychlorinated Biphenyl"
pharmacological__text_string <- "Pharmacological"
personalCare_text_string <- "Personal Care"
industrial_text_string <- "Chemicals in Commerce"
pest_text_string <- "Pesticides"
consumerProduct_text_string <- "Consumer Products"
dioxinsAndFurans_text_string <- "Dioxins and Furans"
PHTH_text_string <- "Phthalates" ############## NOTE NOTE NOTE... i'm not using PHTH properly somewhere I think...

#  HERE we set the parameter Table Names for all the available tests
# drsMasterParamTableName <- "MASV_parameters_7-18-17_fix1.csv"
# drsMasterParamTableName <- "drs_parameter_list_3-8-19.csv"
# drsMasterParamTableName <- "MASV_drs_parameter_list_3-8-19_Date_Fixed.csv"  # UPdated this on 3/10/2019  Hopefully it works.  Also fixed wrong-date-format

SBIR_p1_MasterParamTableName <- "MASV_sbir_parameter_list_based_on_DRS_plus_Missing.csv" # added ParameterID 1,000,000 for one extra. NOTE casnum NOT unique

# drsMasterParamTableName <- "MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed.csv" # UPdated this on 3/12/2019  Hopefully it works.  Also fixed wrong-date-format
# drsMasterParamTableName <- "MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_more_dates_fixed.csv" # UPdated this on 9/22/2022
drsMasterParamTableName <- "MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_more_dates_fixed_weirdSpace_add_Musk.csv" # TWEAKED the master Paramter to have a different COMPOUND NAME for two chemicals.  Need to match what comes in result if possible

flameMasterParamTableName <- "MasterParameterTable_FlameR.csv"
# pestMasterParameterTable <- "MasterParameterTable_Pesticide_2-18-19.csv" # NEW UPDATED VERSION is the SAME except for unused columns
# pestMasterParameterTable <- "MasterParameterTable_Pesticide_03_20_2023_Hand_Fixup.csv" # Used Michael Barton "all parameters" file to fix this up by doing a diff
pestMasterParameterTable <- "MasterParameterTable_Pesticide_03_20_2023_Hand_Fixup.csv" # Used Michael Barton "all parameters" file to fix this up by doing a diff

## OLD Parameter Table continaing VOC and PAH
# vopahMasterParamTableName <-"vopah_parameters_6-29-2020.csv" #####  New June 2020 version see email from Michael Barton (has about 94 compounds)
## Newer Paramater Table w/ Just VOC
vopahMasterParamTableName <- "vopah_voc_parameters_8-24-2020.csv" #####  New August 2020 version see email from Michael Barton (has 32  compounds)
# vopahMasterParamTableName_SBIR <- "vopah_voc_parameters_8-24-2020_SBIR.csv" #####  New August 2020 version see email from Michael Barton (has 32  compounds)
#         MARC checked January 2021 and this list of 32 compounds looks right???
#           NOTE that SOME of the data received has one additional compound ParameterID 300639  but we will delete/ignore that somewhere/somehow

VOC_2024_MasterParamTableName <- "MasterParamenter_Using_April2024_VOC_fixedList21.csv" #####  New April 2024 version (I kept the same name after fixing the 3 missing and changing 18 to 21 VOCs)
# Marc created NEW LIST of master parameter teable

### For Georgetown we did our FIRST Phthalate test
# PHTHmasterParameterTable <- "MasterParameterTable_PHTH.csv"        ##  OLD had wrong ParamaterID
# PHTHmasterParameterTable <- "MasterParameterTable_PHTH.csv"        ##  OLD had wrong ParamaterID
PHTHmasterParameterTable <- "MasterParameterTable_PHTH_fix_ParamaterID.csv" # New from Michael Barton with better parameter IDs

if (RMD_type == "SBIR_P1_DRS_plus") { ## IF WE are doing the SBIR Phase 1 data I have updated the VOPAH dataset to include the classification of compounds that are VOC from Steven input
  vopahMasterParamTableName <- vopahMasterParamTableName_SBIR
}

# pahMasterParameterTable <- "MasterParameterTable_PAH_2-15-19.csv" #####  New February 2019 version see email from Michael Barton
pahMasterParameterTable <- "MasterParameterTable_PAH_April2024.csv" #####  New April 2024 Derived from DATA in Wisconsin resultstable cause all other sources bad

# This is OLD VOC method via thermal desorption
vocMasterParamTableName <- "MyExposome_VOC_MasterParameter_List_3_6_2019.csv" #####  New March 2019 version see email from Michael Barton

## THIS is to allow us to read in a file that helps collapse all the classifications down further.
## there are TWO ways classifications are collapse... first is inline code when read in...
##       SECOND is later we collapse using a file we read-in that tells us how the resulting classifications should collapse.
##        eventually this should be standardized
class_conversion_table_name <- setMASTERPARAM_CLASS_RISKSdirectory("ReivsedClassificationMapping2.csv")
class_explain_table_name <- setMASTERPARAM_CLASS_RISKSdirectory("RevisedClassificationTextDescription2.csv")


#### Created a lookup table with GEMINI / CHatGPT / BARD trying to list chemical sources of exposure, health impacts, mitigation strategies etc...
### THIS TABLE is baswed on ALL 270 COMPOUNDS
# chemSourceMitigationInfoTableName <- "All270_Chems_Marc_Try2.csv"
### tRING DIFFERENT LOOKUP TABLE WITHJUST gemini  on 10/18/2024 USING XLSX intead of cSV with more info some hand-crafted
#### THIS TABLE IS BASED ON JUST the 88 compounds in the group of 71 WBs
chemSourceMitigationInfoTableName2 <- setMASTERPARAM_CLASS_RISKSdirectory("Test_figure_out_chemical_exposure_Copilot_ChatGPT_10_31_2024.xlsx") # Steven/Kelsie updated the sources and mitigation information and TG fixes
chemSourceSheetName2 <- "STEVEN_New71_ChatGPT-Try2"

#  SET name of RMD file
rmd_code <- "MyExposome_1527_v6.Rmd" # Set up name of make markdown file for all tests


### THIS SOURCE CODE IS JUST CODE< not a function> so it is essentially just more INLINE code
###  I just moved it into a separate file so that the logic is separate in terms of which file it is in but
###    it is not set up like a separate function... just more code
source(setRdirectory("testParameterSetup.R"))

#### The resultsTableName and the MasterParameterTableName are now just the string of the file name, need to make them the PATH
####
if (!is.null(resultsTableName)) {
  resultsTableName <- setCUSTOMERdirectory(resultsTableName)
}
if (!is.null(masterParamTableName)) {
  masterParamTableName <- setMASTERPARAM_CLASS_RISKSdirectory(masterParamTableName)
}


#### THESE PARAMETERS are CONSISTENT across to each TEST TYPE and EACH specific RESULT within that test
#                                       BUT may vary as tests and lists are upgraded with new info
#
# classificationTableName<-"EDF_Phase1_DRS_Classifications.csv"
# classificationTableName<-"MASV15_classifications.csv"

# OLD was 6/26, newer (i think but don't remember, is 7/18...testing that)
# classificationTableName<-"MASV15_Classifications_6-26-17.csv" ### "MASV_classiifications_7-18-17.csv"
# classificationTableName<-"MASV_classifications_7-18-17.csv"
# classificationTableName<-"MASV_classifications_10-8-18_fixed_Celestolide.csv"   #HACKED ONE LINE
# classificationTableName<-"MASV_classifications_10-8-18_fixed_Celestolide_2_15_2019_fixed_one_PAH.csv"   #HACKED ONE LINE
# classificationTableName<-"MASV_classifications_10-8-18_fixed_Celestolide_2_15_2019_fixed_one_PAH_then_fix_Cypermethrin.csv"   #HACKED ONE LINE
### NOTE THAT this classification table is really ONLY for MAS method.  Marc hacking in code to add other classifications.
classificationTableName <- "masv_classifications_3-7-19.csv" # Totally new version.  We now do all OUR fixes in CODE and some are already in this file
classificationTableName_SBIR <- "masv_classifications_3-7-19_MOD_FOR_SBIR.csv" # Totally new version.  We now do all OUR fixes in CODE and some are already in this file

# FOR SBIR Phase 1 we need to enhance the classification for a few new compounds
if (RMD_type == "SBIR_P1_DRS_plus") {
  classificationTableName <- classificationTableName_SBIR
}
rm(classificationTableName_SBIR)

### NOTE that classificationTableName used to have a prepended path and such... now above i just set it without prepending anything and HERE is set it to be correct path
if (!is.null(classificationTableName)) {
  classificationTableName <- setMASTERPARAM_CLASS_RISKSdirectory(classificationTableName)
}

# riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk.csv"   # I edited to change NAMING
# riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk_Marc_Tweak.csv"

# here because there is only one link, we will set it DIRECTLY using the HERE syntax
riskCalifProp65TableName <- setMASTERPARAM_CLASS_RISKSdirectory("MASV15_ca_prop65_risk_Marc_Tweak2.csv")
epaIrisTableName <- setMASTERPARAM_CLASS_RISKSdirectory("MASV15_epa_iris_risk.csv")
IARCRiskTableName <- setMASTERPARAM_CLASS_RISKSdirectory("MASV15_who_iarc_risk.csv")
riskIARCdecodeTableName <- setMASTERPARAM_CLASS_RISKSdirectory("RiskIARCdecode.csv")

# Air concentration Table Name and NIOSH Air concentration
# cm3VolumeSiliconeOfOneGram<-1.12  # VOLUMNE of one gram of SILICONE is 1.12cm3 (actually that is more exactly 1.1191 but who cares)
### NO NO NO
# HUH?  I think that is wrong.  I think cm3VolumeSiliconeOfoneGram is actually .893
#  OLD OLD VERSION cm3VolumeSiliconeOfOneGram<-.893  # I'm USING what Marc thinks it is not what my notes say steven told me it is  CONFIRMED w/ STEVEN my # is correct
cm3VolumeSiliconeOfOneGram <- .934 # Updated on 9/12/2022 after review w/ Steven!!

doAIRplusNioshOSHAreporting <- FALSE # Set this to TRUE to add new section to report regarding OSHA/NIOSH limits and also AIR CONCENTRATION INFO
###
### Turnign on AirPlusNiso not working properly I think.....
###

# airConcentrationTable <- "./data/AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs.csv"
# airConcentrationTable <- "./data/AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs_UPDATED.csv"

airConcentrationTable <- setAIRdirectory("AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs_UPDATED_v2_Sept19_2022.csv") # added a few values
airNioshOshaTable <- setAIRdirectory("AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs_UPDATED_NIOSH_OSHA.csv")

r_code <- setRdirectory("MyExp_Base_Code_v6.R")
data.load.R.filename <- setRdirectory("MyExp_data.load.functions_1527_v6.R") # Name of R file to load data
support.functions.filename <- setRdirectory("MyExp_support_functions_1527_v6.R") # Name of R file to SUPPORT the rMarkdown functions

# NAME OF FILE to write OUTPUT to in specialized format
# The Reference Document defines an MSWORD "template" file to use for styling headers/footers/etc
# IF YOU CHANGE THIS also change it in ALL THE RMD file headers
refDoc <- "MyExposomeFormat_1527_v6.docx"
