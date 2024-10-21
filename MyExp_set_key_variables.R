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


#### FOR HTML output if you want to have some things in TABS in the output, set this
## THIS is BUGGY and doesn't interact will with TOC so not going to use TABS for now...
tabsetting <- ""
# tabsetting <-"{.tabset }"


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







# CUSTOMER SPECIFIC FIX UP
#       Note that "DartmouthFixup" could be named "DartmouthCustomWork" or maybe..!!!
#         or we should have ONE variable called "CustomerCustomization" and it should be Dartmouth OR L'Oreal OR...
#       BUT FOR NOW we have "FixupForAnyone" to say if there is ANY fixup... and then specifc values set
FixupForAnyone <- any(
  DartmouthFixup <-    FALSE, # is to to force weird division and fix up
  WisconsinFixup <-    FALSE,
  LorealFixup <-       FALSE,
  BuffaloFixup <-      FALSE,
  UCSFplusRandom10Fixup <- FALSE,
  UCSF2020Fixup <-     FALSE,
  UMTFixup <-          FALSE, # NEVER IMPLEMENTED THIS>.. BE CAREFUL...
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
  SBIR_P2_Part1_71_FixUp <- TRUE   # ALWAYS MAKE LAST ONE TRUE, REST FALSE
)

### USE the flag RMD_type to indicate if FLAME or PAH or DRS_MAS15 etc...
#     NOTE:  Try to set to "nothing" unless sure so you remember to set it!
#
#### NOTE:  I moved setting which TEST we're doing up here so can use value earlier...
#
### USE the flag RMD_type to indicate if FLAME or PAH or DRS_MAS15 etc...
#     NOTE:  Try to set to "nothing" unless sure so you remember to set it!
#
#   ONLY ONE OF THE FOLLOWING RMD_type can be selected at one time
#
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

#
testDoneFor <- ""
DataFile_and_OutputFile_Prepend <- ""

### SET expected units to default of ng/WB and then CHANGE if needed for some specific test
ExpectedUnits <- "ng/WB" ### PROBABLY do NOT change this value... change it ELSEWHERE for some specific test

### IF we want to allow different parameter counts between masterParameterFile and testResultsFile set this to TRUE
### We may ahve a problem with WISCONSIN VOC of 32 vs 33 compounds... this SHOULD catch it and then i'll fix somewhere/somehow
allowDifferentParameterCounts <- FALSE



###
# FLAG to tell us if we should do TEST of PRE-POST comparison (before and after)
testing_PRE_POST <- FALSE # RIGHT NOW ONLY WISCONSIN has pre-post info so set this in WISCONSIN to TRUE
if (SBIR_P2_Part1_71_FixUp) {
  FixupFile <- "./data/SBIR_NIH_Part1_71_SampleKey.csv"
  wristbands_time_adjusted_one_day <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE DAY
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- FALSE
  wristbands_day_and_weight_adjusted <- TRUE   # I just added this.  Not sure it is ever going to be used but mirrors the one for week and weight so we'll see...
  testDoneFor <- "a MyExposome Wristband Study"
  ExpectedUnits <- "ng/WB"
} else if (UniVisionFixup) {
  FixupFile <- "./data/Sample Key_Univision_lookup_table.csv"
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
  FixupFile <- "./data/Sample Key_UCDavis_lookup_table.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UC Davis"
  ExpectedUnits <- "ng/WB"
} else if (CombinedTestData) {
  FixupFile <- "./data/CombinedTestData/Fixup_Dart_george_Chic_col.csv" # NIH SBIR-ID data_Enhanced_Fixup
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "MyExposome Wristband Project"
  ExpectedUnits <- "ng/WB" # IS THIS RIGHT... VERY CONFUSING>>> NEEC TO REVIEW WITH STEVEN WHAT THE UNITS ARE and what the FIXUP FILE is...
} else if (SBIR_P1_May2022Fixup) {
  FixupFile <- "./data/NIH SBIR-ID data_Enhanced_Fixup.csv" # NIH SBIR-ID data_Enhanced_Fixup
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "MyExposome Wristband Project"
  ExpectedUnits <- "ng/WB" # IS THIS RIGHT... VERY CONFUSING>>> NEEC TO REVIEW WITH STEVEN WHAT THE UNITS ARE and what the FIXUP FILE is...
} else if (DartmouthFixup) {
  #  THIS IS FOR FIRST 48:  "./data/First48_to_normalize_data.csv",    ### NEXT is for next 25
  #  THIS IS 48 + 25:   "./data/Dartmouth_First48_plus_Next25_to_Normalize.csv",
  # #next is for all 111 = 1st, 2nd, and 3rd batch
  # FixupFile <-  "./data/Dartmouth/Dartmouth_size and time_all_111_to_normalize.csv"
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_Marc_3_22_2019.csv"
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_Marc_4_1_2019.csv"
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_Marc_5_20_2019.csv"
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_7_1_2019.csv" # This is maybe for first 147 + 20 more = 167?
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_Marc_2019_09_16.csv" # This is maybe for first 167 PLUS whatever in batch 6 (how many?)
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_01_28.csv" # THIS ONE HAD TWO BAD ROWS....
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_01_30.csv" # UPDATE with each batch of new data
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_04_01.csv" # UPDATE with each batch of new data
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_07_12.csv" # UPDATE with each batch of new data
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_07_12_FIXED.csv" # ?? STEVEN FORGOT TO UPDATE Master Table for Batch 10 <marc thinks>
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_08_07.csv" # Batch # 12
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2020_12_08.csv" # Batch # 13
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2021_03_29.csv" # Batch # 14
  # FixupFile <-  "./data/Dartmouth/Dartmouth All IDs_2022_08_24.csv" # Batch # 15, 16, 17  H# Had a BUG in the DATES for batches 16 and 17
  # FixupFile <- "./data/Dartmouth/Dartmouth All IDs_2022_08_27.csv" # Batch # 15, 16, 17
  # FixupFile <- "./data/Dartmouth/Dartmouth All IDs_2023_05_04.csv" # Batch # 15, 16, 17...21
  FixupFile <- "./data/Dartmouth/Dartmouth All IDs_2024_04_25.csv" # Batch # 15, 16, 17...21 22 23 24 25 26

  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "Dartmouth"
  DataFile_and_OutputFile_Prepend <- "Dartmouth/" # ALWAYS USE THE SLASH at end, no
  ExpectedUnits <- "ng/WB"
} else if (UCSF2020Fixup) {
  FixupFile <- "./data/Fixup_UCSF_1_plus_27_plus_1_Sample login info - 052419_fixed_BY_MARC_ADD_FSES_REPAIR_PARTICIPANT_NAME.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UCSF Program on Reproductive Health and the Environment (PRHE)"
} else if (UMTFixup) { #  (MO = UMT)        THIS IS MONTANA !!!!!
  # Trying to do NO adjustment on MONTANA
  FixupFile <- "./data/Sample Key_MO.csv" #  (MO = UMT)
  wristbands_time_adjusted_one_week <- FALSE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- FALSE
  wristbands_week_and_weight_adjusted <- FALSE
  testDoneFor <- "University of Montana, School of Public and Community Health Sciences "
  #
  #
} else if (UNMFixup) { #  (MO = UMT)       THIS IS NEW MEXICO
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  ### HUH?  We ARE going to do adjust for UNM at least for the 2nd batch for SURE
  # FixupFile <- "./data/Sample Key_NM_lookup_table.csv" # THIS IS FIRST BATCH
  FixupFile <- "./data/Sample Key_UNM_2nd_batch_lookup_table.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of New Mexico "
} else if (COLORADOFixUp) { #        COLORADO
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  # FixupFile <- "./data/Sample Key_UnvColorado_FixUp.csv" #  THIS IS FOR FIRST BATCH april 2022
  # FixupFile <- "./data/Sample Key_UnvColorado_2ndVERSION_lookupTableJUST_NEW_DATA.csv" #  THIS IS FOR JUST 2nd batch Nov 2023
  FixupFile <- "./data/Sample Key_UnvColorado_2ndVERSION_lookupTable.csv" #  THIS IS FOR First AND 2nd BATCH (maybe batch specific NOT NEEDED?)
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
  FixupFile <- "./data/Sample Key_UNIV_LILLE_FRANCE_lookup_table.csv.csv" #
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

  FixupFile <- "./data/Sample Key - Louisville Tehran_lookup.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  ExpectedUnits <- "ng/WB"
  testDoneFor <- "Louisville"
} else if (UCONNFixUp) { #        University of Connecticut
  FixupFile <- "./data/Sample Key_UCONNHealth_Fixup.csv" #
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
  FixupFile <- "./data/Sample Key_UnvChicago_FixUp.csv" #
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of Illinois Chicago, School of Public Health "
} else if (GEORGETOWNFixUp) { #  GEORGETOWN
  # Trying to do NO adjustment on U of New Mexico  (NOT MONTANA do not mix them up)
  FixupFile <- "./data/Sample Key_Georgetown_PHTH.csv" #
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
  # FixupFile <-"./data/Wisconsin ID and sample info_to_normalize.csv"
  # FixupFile <-"./data/Fixup_Sample Key_WI_Client3_LookupTable.csv"
  # FixupFile <-"./data/Sample Key_WI_Client3_LookupTable.csv"
  # FixupFile <- "./data/Sample Key_WI_Client3a_LookupTable.csv"  #Tweaked to add one more First/Second pair
  FixupFile <- "./data/Sample Key_WI_Client3_April_2024.csv" # Tweaked to be JUST AND ONLY the April 2024 Data Delivery
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
  FixupFile <- "./data/loreal_fixup_info2.csv"
  testDoneFor <- "L'Oreal"
  ExpectedUnits <- "ng/WB"
} else if (BuffaloFixup) {
  FixupFile <- "./data/FixupFile_UBuffalo_10_030719.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of Buffalo"
  ExpectedUnits <- "ng/WB"
} else if (BostonFixup) {
  FixupFile <- "./data/Sample Login Key - Boston.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "Boston University School of Public Health"
  ExpectedUnits <- "ng/WB"
} else if (UFL_FloridaFixup) {
  FixupFile <- "./data/Sample Key UFL Florida August 2024.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "University of Florida, College of Medicine"
  ExpectedUnits <- "ng/WB"
} else if (UCSFplusRandom10Fixup) {
  FixupFile <- "./data/FixupFile_UCSF_1_Plus_Random10.csv"
  wristbands_time_adjusted_one_week <- TRUE # ADd text messages about TIME-ADJUSTING VALUES to ONE-WEEK
  wristbands_time_adjusted <- wristbands_time_adjusted_one_day || wristbands_time_adjusted_one_week
  wristbands_time_and_weight_adjusted <- TRUE
  wristbands_week_and_weight_adjusted <- TRUE
  testDoneFor <- "UCSF (one wristband with 10 random additional for comparison purposes)"
  ExpectedUnits <- "ng/WB"
} else if (Big_Mas15_List_Fixup) {
  FixupFile <- "./data/Big_Fixup_File.csv"
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


# IF you are wanting to ALSO output a big stat-summary comparing this data to the 682 unit dataset I created in a separate subdirectory
# OutputBigComparisonStatTable <-TRUE
OutputBigComparisonStatTable <- FALSE



# IF you want to make a DEMO DATA SET froma real dataset
# set all parameters above as for REAL dataset but then set these two flags below:
#

makeIntoDemoData <- FALSE
if (makeIntoDemoData) {
  testDoneFor <- "MyExposome participants"
  howManyDemoResults <- 3 #  I use 682 to generate the big CombinedTestData set of data.   I used 350 for Silent Spring
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
#### THIS FIRST SET is to DO the individualization


## JUST do GROUp Analysis
# DoSpecificSubjectAnalysis<-FALSE  #idea is to do specific-subject-analysis output with or without GROUP analysis
# DoGroupAnalysis <- TRUE  # The idea is to be able to turn on or off doing the GROUP analysis with or without INDIVIDUAL analysis
# HideIndividualization<-!DoSpecificSubjectAnalysis # idea is to hide, on the charts, the things that colorize charts for a specific individual


## JUST do SPECIFIC SUBJECT Analysis
DoSpecificSubjectAnalysis <- TRUE # idea is to do specific-subject-analysis output with or without GROUP analysis
DoGroupAnalysis <- FALSE # The idea is to be able to turn on or off doing the GROUP analysis with or without INDIVIDUAL analysis
HideIndividualization <- !DoSpecificSubjectAnalysis # idea is to hide, on the charts, the things that colorize charts for a specific individual


## do SPECIFIC SUBJECT Analysis AND GROUP ANALYSIS
#DoSpecificSubjectAnalysis <- TRUE # idea is to do specific-subject-analysis output with or without GROUP analysis
#DoGroupAnalysis <- TRUE # The idea is to be able to turn on or off doing the GROUP analysis with or without INDIVIDUAL analysis
#HideIndividualization <- !DoSpecificSubjectAnalysis # idea is to hide, on the charts, the things that colorize charts for a specific individual



### NOW OVERRIDE above 2 settings IF you are calling this from PRINT_ALL_SUBJECTS
# In_Print_All_Subjects <- TRUE # USE this when setting key variables below to ALWAYS set  customiization-per-user to be ON

if (exists("In_Print_All_Subjects")) {
  if (In_Print_All_Subjects == TRUE) {
    HideIndividualization <- FALSE # idea is to hide, on the charts, the things that colorize charts for a specific individual
    DoSpecificSubjectAnalysis <- TRUE # idea is to do BOTH group analysis and specific-subject-analysis output
  }
}
DoGroupAnalysisOnly <- !DoSpecificSubjectAnalysis # If NOT doing specific subject analysis then need new INTRO


# FOR many reports we do NOT yet have classification info so for those we should HIDE this for now
#     that is EVERY REPORT except for DRS / MAS15 at the moment
#
# HideClassificationInformation <- FALSE #  Set to TRUE for every report EXCEPT DRS
HideClassificationInformation <- TRUE #    Set to TRUE for every report EXCEPT DRS


# This flag blocks/adds 3 sections to the GROUP part of report regarding findings in looking up chemicals in databases
#  EVENTUALLY will set this to TRUE all the time but.... starting as FALSE cause haven't tested it yet.
#   IT WORKS.... leave as TRUE
DoGroupDatabaseLookupReporting <- TRUE # NOW setting this to tRue all the time I think

#
# Flag to determine if we IGNORE any wristband that had NO RESULTS AT ALL (should NORMALLY BE SET TO FALSE)
#     Set to TRUE only if CUSTOMER has indicated that "BLANK WB should be dropped" (happened in case of Miami Firefighters)
#
DropAllZeroSampleNumbers <- FALSE
# DropAllZeroSampleNumbers<-TRUE  ##  THIS IS FOR FIREFIGHTER ONLY



#
# IF IN a specific data set to be read in, there are one or more specific samples to IGNORE COMPLETELY then use this flag to ignore them.
DropSpecificWristbands <- c("")
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
PHTH_text_string <- "Phthalates"

#  HERE we set the parameter Table Names for all the available tests
# drsMasterParamTableName <- "./data/MASV_parameters_7-18-17_fix1.csv"
# drsMasterParamTableName <- "./data/drs_parameter_list_3-8-19.csv"
# drsMasterParamTableName <- "./data/MASV_drs_parameter_list_3-8-19_Date_Fixed.csv"  # UPdated this on 3/10/2019  Hopefully it works.  Also fixed wrong-date-format

SBIR_p1_MasterParamTableName <- "./data/MASV_sbir_parameter_list_based_on_DRS_plus_Missing.csv" # added ParameterID 1,000,000 for one extra. NOTE casnum NOT unique

# drsMasterParamTableName <- "./data/MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed.csv" # UPdated this on 3/12/2019  Hopefully it works.  Also fixed wrong-date-format
#drsMasterParamTableName <- "./data/MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_more_dates_fixed.csv" # UPdated this on 9/22/2022
drsMasterParamTableName <- "./data/MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_more_dates_fixed_weirdSpace_add_Musk.csv" # TWEAKED the master Paramter to have a different COMPOUND NAME for two chemicals.  Need to match what comes in result if possible

flameMasterParamTableName <- "./data/MasterParameterTable_FlameR.csv"
# pestMasterParameterTable <- "./data/MasterParameterTable_Pesticide_2-18-19.csv" # NEW UPDATED VERSION is the SAME except for unused columns
# pestMasterParameterTable <- "./data/MasterParameterTable_Pesticide_03_20_2023_Hand_Fixup.csv" # Used Michael Barton "all parameters" file to fix this up by doing a diff
pestMasterParameterTable <- "./data/MasterParameterTable_Pesticide_03_20_2023_Hand_Fixup.csv" # Used Michael Barton "all parameters" file to fix this up by doing a diff

## OLD Parameter Table continaing VOC and PAH
# vopahMasterParamTableName <-"./data/vopah_parameters_6-29-2020.csv" #####  New June 2020 version see email from Michael Barton (has about 94 compounds)
## Newer Paramater Table w/ Just VOC
vopahMasterParamTableName <- "./data/vopah_voc_parameters_8-24-2020.csv" #####  New August 2020 version see email from Michael Barton (has 32  compounds)
# vopahMasterParamTableName_SBIR <- "./data/vopah_voc_parameters_8-24-2020_SBIR.csv" #####  New August 2020 version see email from Michael Barton (has 32  compounds)
#         MARC checked January 2021 and this list of 32 compounds looks right???
#           NOTE that SOME of the data received has one additional compound ParameterID 300639  but we will delete/ignore that somewhere/somehow

VOC_2024_MasterParamTableName <- "./data/MasterParamenter_Using_April2024_VOC_fixedList21.csv" #####  New April 2024 version (I kept the same name after fixing the 3 missing and changing 18 to 21 VOCs)
# Marc created NEW LIST of master parameter teable

### For Georgetown we did our FIRST Phthalate test
# PHTHmasterParameterTable <- "./data/MasterParameterTable_PHTH.csv"        ##  OLD had wrong ParamaterID
# PHTHmasterParameterTable <- "./data/MasterParameterTable_PHTH.csv"        ##  OLD had wrong ParamaterID
PHTHmasterParameterTable <- "./data/MasterParameterTable_PHTH_fix_ParamaterID.csv" # New from Michael Barton with better parameter IDs


class_conversion_table_name <- "data/ReivsedClassificationMapping2.csv"
class_explain_table_name <- "data/RevisedClassificationTextDescription2.csv"




if (RMD_type == "SBIR_P1_DRS_plus") { ## IF WE are doing the SBIR data I have updated the VOPAH dataset to include the classification of compounds that are VOC from Steven input
  vopahMasterParamTableName <- vopahMasterParamTableName_SBIR
}


# pahMasterParameterTable <- "./data/MasterParameterTable_PAH_2-15-19.csv" #####  New February 2019 version see email from Michael Barton
pahMasterParameterTable <- "./data/MasterParameterTable_PAH_April2024.csv" #####  New April 2024 Derived from DATA in Wisconsin resultstable cause all other sources bad



# This is OLD VOC method via thermal desorption
vocMasterParamTableName <- "./data/MyExposome_VOC_MasterParameter_List_3_6_2019.csv" #####  New March 2019 version see email from Michael Barton


#### Created a lookup table with GEMINI / CHatGPT / BARD trying to list chemical sources of exposure, health impacts, mitigation strategies etc...
### THIS TABLE is baswed on ALL 270 COMPOUNDS
#chemSourceMitigationInfoTableName <- "./data/All270_Chems_Marc_Try2.csv"


### tRING DIFFERENT LOOKUP TABLE WITHJUST gemini  on 10/18/2024 USING XLSX intead of cSV with more info some hand-crafted
#### THIS TABLE IS BASED ON JUST the 88 compounds in the group of 71 WBs
chemSourceMitigationInfoTableName2 <- "data/Test_figure_out_chemical_exposure_Copilot_ChatGPT_2024_10_18_v3.xlsx"   # added a V2 to see if that fixes
chemSourceSheetName2<-"STEVEN_New71_ChatGPT-Try2"


#  SET name of RMD file
rmd_code <- "MyExposome_1527_v6.Rmd" # Set up name of DRS/1528 R Markdown File
#                                     (maybe someday make RMD unique to each type of test but using this is base generic)


#### THESE PARAMETERS are UNIQUE to each TEST TYPE and EACH specific RESULT within that test
#
if (RMD_type == "PHTH") { #
  rmd_code <- "MyExposome_1527_v6.Rmd"
  URL_of_Chemicals_Tested <- "Will ADD Phtalate URL to Website.  In mean time, see data report" # USED in printing report   THIS IS WRONG WRONG WRONG
  testName <- "Phthalates Quantitative test "
  testExplanation <- "This project provides a focused screen to identify Phthalates."
  HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???
  allowDifferentParameterCounts <- FALSE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters
  masterParamTableName <- PHTHmasterParameterTable

  # resultsTableName <- "./data/F24-21_MyExpoP.O.#258_PHTH_CoA_MULITPLY_ug_by_1000.csv" # PLACEHOLDER for getting Phatlates to work.  Firs data had ug instead of ng so i converted by hand
  # resultsTableName <- "./data/F24-21_MyExpoP.O.#258_PHTH_CoA_MULITPLY_ug_by_1000_fix_ParameterID.csv" #
  resultsTableName <- "./data/F24-21_MyExpo_P.O.#258_PHTH_CoA II_convert_ug_to_ng.csv" # Firs data had ug instead of ng so i converted by hand.  2nd had wrong ParamterID.

  subject <- "A240945" # Randome one from Georgetown  BUT THIS IS new georegetown one from PHTH batch... the numbers changed!
  ExpectedUnits <- "ng/g" ### NOT SURE why have to change this HERE but makes sense it is TEST SPECIFIC and not CUSTOMER SPECIFIC
} else if (RMD_type == "SBIR_P1_DRS_plus") { #
  rmd_code <- "MyExposome_1527_v6 - SBIR.Rmd" # Set up name of DRS/1528 R Markdown File
  URL_of_Chemicals_Tested <- "SBIR NEEDS TO BE UPDATED -- I DELETED THIS SECTION FROM SBIR REPORT" # USED in printing report   THIS IS WRONG WRONG WRONG
  testName <- "Chemical Analysis of Personal Environmental Exposures "
  testExplanation <- "This project detects chemicals from different groups with a focus on Pesticides, Chemicals in Commerce, PCBs (a type of industrial chemical), Flame Retardants and other chemicals of interest."
  HideClassificationInformation <- FALSE # Set to TRUE for every report EXCEPT DRS and maybe ???
  allowDifferentParameterCounts <- TRUE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters
  masterParamTableName <- SBIR_p1_MasterParamTableName
  resultsTableName <- "./data/SBIR_longTableHasValuesWithParameterName.csv" # SBIR Phase 1

  # subject<-"SK4000WB"   # this is --> Steven
  # subject<-"SK4002WB"   # this is --> Marc
  subject <- "SK4003WB" # this is --> Tamara
  # subject<-"SK4004WB"   # this is --> Kevin (or tracey?)
  # subject<-"SK4005WB"   # this is --> George O'Connell
  # subject<-"SK4006WB"   # this is --> Owen Epstein
  # subject<-"SK4007WB"   # this is --> Wendy Hillwalker
  # subject<-"SK4008WB"   # this is --> Marilyn Walker
  # subject<-"SK4009WB"   # this is --> Becs Epstein
  # subject<-"SK4010WB"   # this is --> Ana Sanchez Bachman
  # subject<-"SK4011WB"   # this is --> Ava Goldman
  # subject<-"SK4014WB"   # this is --> Jennifer Egner DARTMOUTH
  # subject<-"SK4016WB"   # this is --> Aimee Johnson DARTMOUTH
  # subject<-"SK4017WB"   # this is --> Cassie Huang EDF
  # subject<-"SK4018WB"   # this is --> Christina (“CJ”) Sivulka EDF
  # subject<-"SK4019WB"   # this is --> Joanna Slaney EDF
  # subject<-"SK4020WB"   # this is --> Mel Biada business contact
  # subject<-"SK4021WB"   # this is --> Peter Bessen business contact
  # subject<-"SK4022WB"   # this is --> Jacquelyn Hagermoser
  # subject<-"SK4023WB"   # this is --> Alix
  # subject<-"SK4024WB"   # this is --> Ben Pl.
  # subject<-"SK4025WB"   # this is --> Lyndsay  (this has 33 compounds)
  # subject<-"SK4026WB"   # this is --> Kathleen
  # subject<-"SK4027WB"   # this is --> Jennifer F.B.
  # subject<-"SK4028WB"   # this is --> Sarah
  # subject<-"SK4029WB"   # this is --> Sydney Evans EWG
  # subject<-"SK4030WB"   # this is --> Tasha Stoiber EWG
  # subject<-"SK4031WB"   # this is --> Tucker
  # subject<-"SK4033WB"   # this is --> Matt Perkins
  # subject<-"SK4034WB"   # this is --> Miles Naughton
} else if (RMD_type == "VOC") { # THIS IS OLD OLD OLD VOC information... is original (2018/19) VOC list
  URL_of_Chemicals_Tested <- "https://www.myexposome.com/voc" # USED in printing report
  testName <- "VOC Quantitative Analysis"
  testExplanation <- "This project focused on finding Volitile Organic Compounds (VOCs)."
  masterParamTableName <- vocMasterParamTableName
  resultsTableName <- "./data/Wisconsin_MyExposome_PO_206_VOC_CoA.csv"
  subject <- "A180359" # PAH this is one of WISCONSIN
  rmd_code <- "MyExposome_1527_v6.Rmd" # Set up name of DRS/1528 R Markdown File
  #                                     (maybe someday make RMD unique to each type of test but using this is base generic)
} else if (RMD_type == "PAH") { # THESE 4 LINES are for PAH TEST (which includes Miami Firefighters)
  URL_of_Chemicals_Tested <- "https://www.myexposome.com/pah" # USED in printing report
  # testingTypeText<- "Polycyclic Aromatic Hydrocarbons (PAH's)"
  testName <- "PAH Quantitative Analysis"
  testExplanation <- "This project focused on finding Polycyclic Aromatic Hydrocarbons (PAH's)."
  if (Miami_Firefighters_2017) { # OVERRIDE setting of PAH for OLD Miami Firefighter Data
    masterParamTableName <- "./data/MasterParameterTable_PAH_OLD_Firefighters" ##### OLD VERSION only use to recreate firefighter info
  } else {
    masterParamTableName <- pahMasterParameterTable
  }
  # OLD: resultsTableName<-"./data/PahTestData4.csv"  # THIS IS FOR PAH
  # OLD:resultsTableName<-"./data/PahTestData6.csv"  # THIS IS FOR PAH  .. I SHORTER with less data
  # OLD:resultsTableName<-"./data/PahTestData7.csv"  # THIS IS FOR PAH has a J and a U in it for "B"
  # OLD:resultsTableName<-"./data/PahTestData8.csv"  # THIS IS FOR PAH to see if we can do MOST AMOUNT by FAR of anyone
  # OLD:resultsTableName<-"./data/F17-03-Results.csv"  # THIS IS FOR PAH --REAL DATA FROM MIKE (no J values?)
  # OLD:resultsTableName<-"./data/F17-03-Results_HACK_FIX_TO_HAVE_FLAG.csv"  # THIS IS FOR PAH --REAL DATA FROM MIKE --HACKED BY MARC to combine result+FLAG
  # OLD:resultsTableName<-"./data/F17-03-ResultsV2.marcfix.csv"  # THIS IS FOR PAH --REAL miami DATA FROM MIKE --HACKED BY MARC to combine result+FLAG
  # OLD:resultsTableName<- "./data/F17-03-ResultsV2.csv"  #this is PAH, real Miami, WITHOUT the fix to combine result+Flag and without fix of elimn zero data
  # OLD:resultsTableName<-"./data/F17-03-ResultsV2.marcfix_DeleteZEROs.csv" ### THIS IS FOR PAH --REAL miami DATA FROM MIKE --HACKED BY MARC to combine result+FLAG PLUS delete ZERO COLUMN from INPUT DATA
  # OLD:x`resultsTableName<-"./data/F17-03_Fire_Fighters_data.csv"
  # resultsTableName<-"./data/F17-03_June_July_Fire_Fighters_Data_25_52.csv"
  # resultsTableName<-"./data/Wisconsin_PO206_PAH_OSU_MyE_Report.csv"
  # resultsTableName<-"./data/F19-34 MyExposome PO#221 PAHs-MLB.csv" #Wisconin PAH testing 2nd batch
  # resultsTableName<-"./data/F20-08 MyExposome PO225 PAH.csv" #Wisconin PAH testing 3rD batch
  # resultsTableName<-"./data/Revised PO 221 and PO 225 Wisconsin for VOC Update/F19-34 PO#221 PAH.csv" #REVISED 8/27/2020 Wisconin PAH testing 2nd batch   (this is 221 same as old 221)
  # resultsTableName <- "./data/F21-10 MyExposome P.O.#231_PAH.csv" # PAH testing 4th batch
  resultsTableName <- "./data/F24-07 MyExposome PO 251 CoA report_CSV_PAH.csv" # PAH testing April 2024 Wisconsin

  # subject<-"A180237B"             #PAH fake  # THIS IS FOR PAH
  # subject<-"A170251"             #PAH REAL   # THIS IS FOR PAH and is the BLANK WRISTBAND
  # subject<-"A170277"          ###   #PAH REAL   # THIS IS FOR PAH and is LOADED WITH EVERY CHEMICAL
  # subject<-"A170263"             #PAH REAL   # THIS IS FOR PAH.  Good representiatvie sample from 1st 25
  # subject<-"A170213"             #PAH REAL   # THIS IS FOR PAH.  Good representiatvie sample from 2nd 52
  # subject<-"A170241"             #PAH REAL   # Miami Data BAD READ ON 15 CHEMICALS, compromised wristband...
  # subject<-"A170229"             #PAH REAL   # THIS IS FOR PAH   BEST representative sample from 1st 25
  # rmd_code = "MyExposome_PAH_v2.Rmd"  ###   #Set up name of PAH R Markdown File (unique to each type of test)
  # subject<-"A180359"   #PAH this is one of WISCONSIN
  # subject<-"A191134"  #PAH this is one of WISCONSIN from 2nd batch
  # subject<-"A200555"  #PAH this is one of WISCONSIN from 3rd batch
  subject <- "A240027" # this is one of WISCONSIN from 5th (april 2024 batch)
} else if (RMD_type == "VOPAH") { # THIS has undergone weird changes.. NOW IT IS ONLY the VOC only method w/ Solvent and SPE clean
  URL_of_Chemicals_Tested <- "https://www.myexposome.com/vopah" # USED in printing report
  testName <- "VOC Quantitative Test"
  testExplanation <- "This project focused on finding Volatile Organic Compounds (VOCs)."
  HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???  Implementing 6/30/2020
  masterParamTableName <- vopahMasterParamTableName

  # PO 221
  # resultsTableName<-"./data/F19-34 MyExposome PO#221 VOPAH-MLB.csv" ### OLD batch 2
  # resultsTableName<-"./data/F19-34 PO#221 32 VOC report v4.csv"  # THIS IS NEW version of data to be more accurate after OSU equipment failure discovered BATCH 2

  # PO 225
  # #resultsTableName<-"./data/F20-08 MyExposome PO225 VOPAH.csv"  # this is ORIGINAL version of 225, was wrong, was 94 compounds
  # resultsTableName<-"./data/F20-08 PO#225 32 VOC report v4.csv"  # THIS IS NEWER version of data from august 2020 ofter OSU equipment failure discovered BUT WRONG AGAIN is dup of 221
  # resultsTableName<-"./data/F20-08 PO#225 32 VOC report mlb_9-1-2020.csv"  # THIS IS NEWER version of data from august 2020 ofter OSU equipment failure then dup 221, then bad format delivery and maybe now finally fixed
  # resultsTableName<-"./data/MyE_PO225_VOCs_9-2-2020.csv"  # THIS IS NEWEST'est version of data from august 2020 ofter OSU equipment failure then dup 221, then bad format delivery and maybe now finally fixed NOPE and now another release to fix missing 6 data points

  ### PO 226 TWO LINES BELOW are LINKED so be careful
  # resultsTableName<-"./data/Wisconsin_MyExposome_PO_206_VOC_CoA.csv"  # FIRST Wisconsin Data (originally run as VOC but now I'm running as VOPAH cause 32 in VOPAH and 42 in VOC)
  # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

  ###  PO 221 plus PO 225 plus PO 226
  ###  LINES BELOW ARE LINKED SO be careful
  # resultsTableName <- "./data/WI_Combined_1_2_3_PO206_PO221_PO225.csv" # 3 batches Wisconsin Data (1st batch originally run as VOC now combined run all as VOPAH)
  # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE
  # allowDifferentParameterCounts <- FALSE # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

  ###  PO 221 plus PO 225 plus PO 226 plus PO 231
  ###  LINES BELOW ARE LINKED SO be careful
  resultsTableName <- "./data/WI_VOCPAH+Combined_1_2_3_4_PO206_PO221_PO225_PO231.csv" # 4 batches Wisconsin Data (1st batch originally run as VOC now combined run all as VOPAH)
  # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE
  allowDifferentParameterCounts <- FALSE # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

  # subject<-"A180359"   #PAH this is one of WISCONSIN
  # subject<-"A191134"  #PAH this is one of WISCONSIN from 2nd batch
  # subject<-"A200555"  # this is one of WISCONSIN from 3rd batch
  subject <- "A210328" # this is one of WISCONSIN from 4rd batch
} else if (RMD_type == "VOC_2024") { # THIS has undergone weird changes.. is JUST VOC as of 2024
  ###### URL_of_Chemicals_Tested <- "https://www.myexposome.com/vopah" # USED in printing report
  URL_of_Chemicals_Tested <- "Not Yet Up on Website but current full list is in show in Report" # USED in printing report
  testName <- "VOC 2024 Quantitative Test"
  testExplanation <- "This project focused on finding Volatile Organic Compounds (VOCs)."
  HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???  Implementing 6/30/2020
  masterParamTableName <- VOC_2024_MasterParamTableName

  # resultsTableName <- "./data/F24-07 MyExposome PO 251 CoA report_CSV_VOC.csv" #  New VOC_2024 Test and data   BUT was missing 3 compounds
  resultsTableName <- "./data/F24-07 MyExposome PO 251 CoA report_Updated_VOC.csv" #  New VOC_2024 Test and data after ADDING back those 3 compounds-AND THEN a FIX and REDO (first time was bad, now OK)
  # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE
  allowDifferentParameterCounts <- FALSE # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

  subject <- "A240027" # this is one of WISCONSIN from april 2024 batch
} else if (RMD_type == "PEST") { # THESE 4 LINES are for PEST TEST
  URL_of_Chemicals_Tested <- "https://www.myexposome.com/pest" # USED in printing report
  testName <- "Pesticide Quantitative Analysis"
  testExplanation <- "This project focused on finding Pesticides."

  masterParamTableName <- pestMasterParameterTable

  # resultsTableName<-"./data/PO_206_Pesticide_OSU_MyE_report.csv"
  # resultsTableName<-"./data/F19-34 MyExposome PO#221 Pesticides-MLB.csv"
  # resultsTableName<-"./data/F20-08 MyExposome PO225 Pesticides.csv"
  # resultsTableName <- "./data/F21-10 MyExposome P.O.#231_PEST.csv"
  # resultsTableName <- "./data/F23-10_MyExpoP.0#245_Pesticide_CoA_Univision.csv" # Univision Pest Info
  # resultsTableName <- "./data/F23-10_MyExpoP.0#245_Pesticide_CoA_UnivisionWEIRD UNIT PROBLEM_Fix_By_Hand.csv" # Univision Pest Info
  resultsTableName <- "./data/PO245 CSV version redone.csv" # Univision Pest Info  Reissue after having ONE bad datapoint...

  # subject<-"A180359"   #PEST this is one of WISCONSIN
  # subject<-"A191134"  #PAH this is one of WISCONSIN from 2nd batch
  # subject<-"A200555"  #PAH this is one of WISCONSIN from 3rd batch
  # subject <- "A210328" # this is one of WISCONSIN from 4rd batch
  subject <- "A230476" # Randome one from Univision
} else if (RMD_type == "FLAME") { # THESE 4 LINES are for FLAME TEST  (NOT firefighters!!!)
  URL_of_Chemicals_Tested <- "https://www.myexposome.com/flame" # USED in printing report
  # testingTypeText<- "Flame Retardants"
  testName <- "Flame Retardant Quantitative Analysis"
  testExplanation <- "This project focused on finding Flame Retardants."

  masterParamTableName <- flameMasterParamTableName
  resultsTableName <- "./data/TestResultsFlame.csv" # THIS IS FOR Flame?

  subject <- "B241284" # FLAME example
} else if (RMD_type == "DRS") { # THESE 4 LINEsE are for 1528/drs test
  URL_of_Chemicals_Tested <- "https://www.myexposome.com/fullscreen" # USED in printing report
  # testingTypeText<- "our broadest spectrum of compounds"
  testName <- "Full Screen Quantitative Analysis"

  testExplanation <- "This project provides a broad screen to identify compounds across many chemical groups with a focus on Personal Care Products, Pesticides, Chemicals in Commerce, Flame Retardants and other chemicals of interest."
  ###  WE COULD LIST THESE HERE:    Consumer & Personal Care Products,  Industrial & Commercial Chemicals (Including VOCs), Agricultural & Pharmaceutical Chemicals, Persistent Organic Pollutants (POPs), Flame Retardants, Polycyclic Aromatic Hydrocarbons (PAHs)

  HideClassificationInformation <- FALSE # Set to TRUE for every report EXCEPT DRS and maybe ???

  allowDifferentParameterCounts <- TRUE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters

  masterParamTableName <- drsMasterParamTableName
  # resultsTableName <- "./data/CSV Format MyE A170185_A170186-6-8-2017_fix2_REDO.csv" #this line is for the TWO from Senator
  # resultsTableName <- "./data/CSV_Format_Data_MyE14_MyE15_fixed_v2_PLUS_38_MORE.csv" #this line is for the TWO from Senator PLUS 38 more EDF + +

  # resultsTableName <- "./data/CSV_Format_Data_MyE14_MyE15_fixed_v2_PLUS_38_MORE_made_random_values.csv" #2 from senator + 38 w/ random values NOT "1"

  # THE ONE BELOW w/ soozie may not work
  # resultsTableName<-"./data/F16-20-MyExposome_EDF12_plus_Soozie.csv"  #THIS LINE is for EDF_new2016 PLUS soozie
  # subject<-"A150196"  # This is random one from EDF
  # subject<-"A161423" # This is EDF Parking Valet
  ######### subject<-"A150201"  # This is random one from EDF

  # resultsTableName<-"./data/F18-09_F18-14_OSU_MyE_Report_LOREAL.csv"
  # resultsTableName<-"./data/Fake_Data_78.csv"  #dartmouth modified version...
  # resultsTableName<-"./data/Rocio-Canada as delivered.csv"  #Rocio Canada Data As Deliver (3 WB)

  # resultsTableName<-"./data/F21-07_MyExposome_P.0.#230_CoA_RYAN.csv"   # group of dartmouth   #NOTE:  is NOW the 13

  # resultsTableName<-"./data/F19-26 MyExposome PO# 218 final_plus_1_from_222_plus_1_old_from_batch216_UCSF.csv"   # First group from UCSF plus one-special-adder (NOT including weird older one special?)
  # resultsTableName<-"./data/F19-13 MyExposome PO216 CSV_Client - UCSF.csv" # ONE USCF wristband for reporter/or influencer
  # subject<-"A190349" #ONE ucsf subject reporter influencer

  # resultsTableName<-"./data/F19-13 MyExposome PO216 CSV_Client - UofBuffalo.csv" # 10 or so wristbands for BUffalo
  # subject<-"A190350" # one subject from BUFFALO   7/1/2019

  # resultsTableName<-"./data/F19-13 MyExposome PO216 CSV_Client _UCSF_plus_Random_10.csv" # 10 or so wristbands for BUffalo
  # resultsTableName<-"./data/Big_Dartmouth_Loreal_Buffalo_UCSF.csv" # One UCSF plus MANY mas15 tests dartmouth plus loreal plus buffalo
  # resultsTableName<-"./data/F21-19_MyExpoP.O.#232_CoA_UMT.csv" # University of Montana 6 wristbands for testing
  # subject<-"A210743"  # RANDOM choice from UMT Montana

  # resultsTableName<-"./data/F21-26_MyExpo PO#233_CoA_12_UNM.csv" # University of NEW MEXICO 12 wristbands
  # subject<-"A211319"  # RANDOM choice from UNM  NEW MEXICO

  # I"M GOING TO Do 2nd batch from UNM Univ New Mexico but NOT MERGE WITH FIRST BATCH.... just do this additional 20
  # resultsTableName<-"./data/F23-09_03_P.O#244_MyExpo_CoA_UNM.csv" # University of NEW MEXICO 20 wristbands
  # subject<-"A230400"  # RANDOM choice from UNM  NEW MEXICO 2nd batch

  # resultsTableName<-"./data/F21-31_MyExpo_P.O#236_CoA_Colorado.csv" # Colorado FIRST BATCH
  # subject<-"A211668" #Randome one from Colorado

  # resultsTableName<-"./data/F23-16_MyExpo_P.O.#247_CoA - Colorado.csv" # Colorado BATCH2
  # resultsTableName<-"./data/F23-16_MyExpo_P.O.#247_CoA - Colorado_fixed_Units.csv" # Colorado BATCH2 WITH FIXED UNITS

  # resultsTableName<-"./data/F21-31_MyExpo_P.O#236_plus_#247_CoA_Colorado_batch1and2.csv" # Colorado BATCH1 and BATCH2
  # subject<-"A231218" #Randome one from Colorado BATcH 2

  # resultsTableName<-"./data/F23-22_MyExposome #249_CoA_France.csv" # FRANCE LILLE
  # subject<-"A232671" #Randome one from LILLE

  #  resultsTableName<-"./data/Dartmouth/Dartmouth 1_2_etc_13_etc_20_21_22_23_24_25_26_for_1166_FixPO246units.csv" # Probably FINAL Dartmouth
  #  subject <- "A200961" # THIS is random one in DARTMOUTH BATCh 13

  # resultsTableName<-"./data/F22-09_MyExpoP.O#238_CoA_UCDavis35.csv" # UC Davis
  # subject<-"A220461" #Randome one from UC Davis  A220453  A220432

  # resultsTableName<-"./data/F21-31_MyExpo_P.O#236_CoA_Chicago.csv" # CHICAGO
  # subject<-"A211685" #Randome one from Chicago

  # resultsTableName<-"./data/F21-32_MyExpo_P.O.#237_CoA_Georgetown.csv" # Georgetown

  #resultsTableName <- "./data/CombinedTestData/Dart_Chic_Col_George.csv" # Georgetown
  # subject<-"A211733" #Randome one from Georgetown

  # subject <- "A170186-DC" # This is My15 from Senator A170186 (DC)

  #  subject <- "A211668" # Randome one from CombinedTEstData

  # resultsTableName<-"./data/F23-10_MyExpoP.O#245_MASV15_CoA_UnivisionFIX.csv" # Univision (first try had wrong units this try sould work?)
  # subject<-"A230476" #Randome one from Univision

  # resultsTableName<-"./data/F23-21_MyExposome P.O.#248_CoA_UCONN.csv" # Connecticut
  # resultsTableName<-"./data/F23-21_MyExpo_P.O.#248_CoA_reissue_ngPerWB_UCONN.csv" # Connecticut reissued with correct UNITS
  # subject<-"A231970" #Random one from CONNECTICUT

  # resultsTableName<-"./data/F24-14_MyExpo_P.O.#253_CoA.csv" # BOSTON
  # subject<-"A240466" #Random one from BOSTON

  # resultsTableName<-"./data/F24-20_MyExpo_PO257_OSU_MyE_Report.csv" # UFL Florida
  # subject<-"A240871" #Random one from UFL Florida

  # resultsTableName<-"./data/F24-05_MyExposomeP.O.#250_CoA_Louisville.csv" #
  # subject<-"A240020" #Random one from Louisville

  resultsTableName<-"./data/F24-22_MyExpoP.O.#259_CoA-WBdata.csv" #  NOW DOING SBIR Phase 2 first group of 71
  subject<-"A241133" #Random one from SBIR P2 Group 1 of 71


  }

#### THESE PARAMETERS are CONSISTENT across to each TEST TYPE and EACH specific RESULT within that test
#                                       BUT may vary as tests and lists are upgraded with new info
#
# classificationTableName<-"./data/EDF_Phase1_DRS_Classifications.csv"
# classificationTableName<-"./data/MASV15_classifications.csv"

# OLD was 6/26, newer (i think but don't remember, is 7/18...testing that)
# classificationTableName<-"./data/MASV15_Classifications_6-26-17.csv" ### "MASV_classiifications_7-18-17.csv"
# classificationTableName<-"./data/MASV_classifications_7-18-17.csv"
# classificationTableName<-"./data/MASV_classifications_10-8-18_fixed_Celestolide.csv"   #HACKED ONE LINE
# classificationTableName<-"./data/MASV_classifications_10-8-18_fixed_Celestolide_2_15_2019_fixed_one_PAH.csv"   #HACKED ONE LINE
# classificationTableName<-"./data/MASV_classifications_10-8-18_fixed_Celestolide_2_15_2019_fixed_one_PAH_then_fix_Cypermethrin.csv"   #HACKED ONE LINE
### NOTE THAT this classification table is really ONLY for MAS method.  Marc hacking in code to add other classifications.
classificationTableName <- "./data/masv_classifications_3-7-19.csv" # Totally new version.  We now do all OUR fixes in CODE and some are already in this file
classificationTableName_SBIR <- "./data/masv_classifications_3-7-19_MOD_FOR_SBIR.csv" # Totally new version.  We now do all OUR fixes in CODE and some are already in this file

# FOR SBIR we need to enhance the classification for a few new compounds
if (RMD_type == "SBIR_P1_DRS_plus") {
  classificationTableName <- classificationTableName_SBIR
}

# riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk.csv"   # I edited to change NAMING
#riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk_Marc_Tweak.csv"
riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk_Marc_Tweak2.csv"

epaIrisTableName <- "./data/MASV15_epa_iris_risk.csv"
IARCRiskTableName <- "./data/MASV15_who_iarc_risk.csv"

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
###

# airConcentrationTable <- "./data/AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs.csv"
# airConcentrationTable <- "./data/AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs_UPDATED.csv"
airConcentrationTable <- "./data/AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs_UPDATED_v2_Sept19_2022.csv" # added a few values

airNioshOshaTable <- "./data/AirConcentration_CombinedParameter_voc_pah_vopah_pest_flame_drs_UPDATED_NIOSH_OSHA.csv"

# SETUP key environment stuff:
#      Setup data directory
if (!file.exists("data")) {
  dir.create("data")
}
# NEED to set a JAVA_HOME for some things to load/compile
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre7")

# Set up name of R CODE files
#
r_code <- "MyExp_Base_Code_v6.R"
##                CAUTION:  CHeck the WORD DOC set at the TOP of the RMD file is correct: MyExposomeFormat_1527_v3?
data.load.R.filename <- "MyExp_data.load.functions_1527_v6.R" # Name of R file to load data
support.functions.filename <- "MyExp_support_functions_1527_v6.R" # Name of R file to SUPPORT the rMarkdown functions

# NAME OF FILE to write OUTPUT to in specialized format
# The Reference Document defines an MSWORD "template" file to use for styling headers/footers/etc
# IF YOU CHANGE THIS also change it in ALL THE RMD file headers
refDoc <- "MyExposomeFormat_1527_v6.docx"

#### DOWN BELOW HERE we have only COMMENTS of older values for these settings
