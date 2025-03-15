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
# install.packages("plyr")
# install.packages("sinaplot")

# library("plyr")
# library("sinaplot")
# library("dplr)

MyExp_Base_Code_v6_R_Code_was_run <- TRUE  # Use this test if this code was run...

# Load required packages
# Function to check, install, and load packagesf
load_package <- function(packages) {
  not_installed <- packages[!packages %in% installed.packages()[,"Package"]]
  if(length(not_installed)) install.packages(not_installed)
  invisible(lapply(packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

# List of required packages
# PLOTLY is used to create interactive plots,   Added to help w/ VIOLIN charts
# Added HTMLTOOLS because it allows better generation of html files and separate items in report (first used in violin)
required_packages <- c(
  # "plyr",
  # "dplyr",  # Explicitly loading plyr before dplyr and plyr is needed for sinaplot
  # "sinaplot",
  "tidyverse", "knitr", "rmarkdown" ,
  "scriptName", "grid",
  "ggforce", ## UNCLEAR
  "png", "ggplot2", "ggtext", "plotly", "htmltools", "RColorBrewer", "reshape2", "pander", "scales",
  "rlang",
  "styler", # Added styler to allow manual styling of code using the "addins" menu item
  "readxl",
  "bsplus", # Added BSPLUS to allow popup bottons
  "patchwork", # added to do side-by-side graphi layout
  "here", # USE the HERE package to locate the LOGO and provide an absolute path instead of a relative path cause relative path messes up print-all-subjects
  "base64enc",
  "gt", # for printing fancier tables
  "downloadthis",
  "glue" # for string interpolation

  # "reactable"  # added as test to do display of table data you can sort filter download     ### DELETED THIS CAUSE IT DIDN"T WORK
)

other_packages <- c("sqldf", "RSQLite", "gplots") # DO NOT LOAD THESE unless needed,  move then to required packages


# unloadNamespace("sinaplot")
# unloadNamespace("plyr")
# unloadNamespace("dplyr")


# Load all required packages
load_package(required_packages)


rm(load_package, other_packages, required_packages)

# Additional setup
options(warn = -1) # Turn off all warning messages
### TIDY with dplyr (i think) in update to 3.0.0 or something added weird warning messages about how group / ungroup / summarize works
#       and actually... i didn't understand before that after a GROUP on multiple things "summarize" and maybe other functions
#           auto-ungroups on the LAST of the grouped things but LEAVES everything grouped other than that "last" (last in list) item
###
### SO the line below STOPS the new error messages BUT to understand group / ungroup /summarize see this: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform = FALSE)

here::here() # If you’re using the here package for relative paths

#----------------put refactored code BELOW here
#----------------put refactored code BELOW here


# Simplified/Refactored R Code
#
# NOTE: This version removes extensive commented-out code, debug statements (e.g., cat to debug_log.txt),
# and some older references to gather() and partial removed functions.
# It preserves the main logic, domain-specific function calls, and key checks.
#
# ================================================================================

# --- Environment Setup ---

# Check if key variables file is loaded (indicates environment is set up)
if (!exists("MyExp_set_key_variables_R_file_is_loaded")) {
  # If not, load it from a known path (here::here helps construct the path)
  source(here::here("R", "MyExp_set_key_variables.R"))
}

# Check if the 'customer_Output' function is available; if not, source it
if (!exists("customer_Output")) {
  source(setRdirectory("customer_Output.R"))
}

# Check if 'load.masterParam' function is loaded; if not, source it
if (!exists("load.masterParam")) {
  source(data.load.R.filename)
}

# Re-enable default warning behavior
options(warn = 0)

# --- MasterParam & Classification Data ---

# Load masterParam data (contains parameter metadata)
masterParam <- load.masterParam(masterParamTableName, DropSpecificChemicals, uppercaseFirst)

# Load raw classification table (usually wide format with many classification columns)
classificationTextStrings <- list(
  PAH = PAH_text_string,
  flameRetardant = flameRetardant_text_string,
  PCB = PCB_text_string,
  pharmacological = pharmacological__text_string,
  personalCare = personalCare_text_string,
  industrial = industrial_text_string,
  pest = pest_text_string,
  consumerProduct = consumerProduct_text_string,
  dioxinsAndFurans = dioxinsAndFurans_text_string
)
classification <- load.classification(classificationTableName, classificationTextStrings)
# Remove loader functions from environment to avoid naming conflicts
#rm(load.classification, classificationTableName)

# Optionally keep a copy of classification if needed for debugging/testing
# class_TEMP_hold <- classification  # DELETE AFTER TESTING if desired

# Convert classification from wide to long for easier filtering and joining.
#   - pivot_longer(...) makes each classification a separate row.
class_L <- classification %>%
  pivot_longer(
    cols = -ParameterID,          # Exclude ParameterID from pivot.
    names_to = "classification",  # Name of new column that will store classification name.
    values_to = "value",         # Name of new column that stores the old column’s value.
    values_drop_na = TRUE         # Discard rows where the classification would be NA.
  ) %>%
  filter(value == 1) %>%         # Keep only rows where classification is marked as 1.
  select(ParameterID, classification) %>%
  mutate(ParameterID = as.character(ParameterID)) %>% # Ensure ParameterID is character for joins.
  semi_join(masterParam, by = "ParameterID")          # Only keep ParameterIDs found in masterParam.

# classification object no longer needed.
#rm(classification)
#str(class_L)

# Update 'class_L' by calling custom functions that handle domain-specific classification additions.
# Each call typically adds or modifies classification rows.
# New calls
class_L <- updateWithClassSpecificMasterParam(
  PAH_text_string,
  pahMasterParameterTable,
  class_L,
  DropSpecificChemicals,
  setMASTERPARAM_CLASS_RISKSdirectory,
  load.masterParam,
  uppercaseFirst
)
class_L <- updateWithClassSpecificMasterParam(
  VOC_2024_text_string,
  VOC_2024_MasterParamTableName,
  class_L,
  DropSpecificChemicals,
  setMASTERPARAM_CLASS_RISKSdirectory,
  load.masterParam,
  uppercaseFirst
)
class_L <- updateWithClassSpecificMasterParam(
  pest_text_string,
  pestMasterParameterTable,
  class_L,
  DropSpecificChemicals,
  setMASTERPARAM_CLASS_RISKSdirectory,
  load.masterParam,
  uppercaseFirst
)
class_L <- updateWithClassSpecificMasterParam(
  flameRetardant_text_string,
  flameMasterParamTableName,
  class_L,
  DropSpecificChemicals,
  setMASTERPARAM_CLASS_RISKSdirectory,
  load.masterParam,
  uppercaseFirst
)
class_L <- updateWithClassSpecificMasterParam(
  PHTH_text_string,
  PHTHmasterParameterTable,
  class_L,
  DropSpecificChemicals,
  setMASTERPARAM_CLASS_RISKSdirectory,
  load.masterParam,
  uppercaseFirst
)
class_L <- updateWithClassSpecificMasterParam(
  FRAGRANCE_text_string,
  FRAGRANCEmasterParameterTable,
  class_L,
  DropSpecificChemicals,
  setMASTERPARAM_CLASS_RISKSdirectory,
  load.masterParam,
  uppercaseFirst
)

# Remove the function object now that updates are done.
rm(load.masterParam)

# Remove all additional masterParam table variables that are no longer needed.
rm(
  pahMasterParameterTable, VOC_2024_MasterParamTableName, pestMasterParameterTable,
  flameMasterParamTableName, PHTHmasterParameterTable, SBIR_p1_MasterParamTableName,
  vocMasterParamTableName, vopahMasterParamTableName, drsMasterParamTableName,
  FRAGRANCEmasterParameterTable
)

# Remove the update function reference.
rm(updateWithClassSpecificMasterParam)

# Manual classification fixes
#   - Add or remove certain ParameterID->classification pairs based on domain knowledge.
class_L <- class_L %>%
  add_row(ParameterID = "301467", classification = personalCare_text_string) %>% # Add Celestolide as Personal Care
  add_row(ParameterID = "301467", classification = consumerProduct_text_string) %>% # Also Celestolide as Consumer Product
  add_row(ParameterID = "301765", classification = PAH_text_string) %>%  # Mark 1,3-dimethylnaphthalene as PAH
  distinct() %>%
  filter(!(ParameterID == 301467 & classification == PAH_text_string)) %>%             # Remove Celestolide as PAH
  filter(!(ParameterID == 301449 & classification == PAH_text_string)) %>%             # Remove Cashmeran as PAH
  filter(!(ParameterID == 301478 & classification == PAH_text_string)) %>%             # Remove Phantolide as PAH
  filter(!(ParameterID == 481 & classification == pharmacological__text_string)) %>%   # Remove Fluoranthene as pharmacological
  filter(!(ParameterID == 300639 & classification == VOC_text_string))  %>%               # Remove Tributyl phosphate as VOC
  distinct()

# Convert classification to a new, reduced set of categories.
class_L <- unique(
  convert_to_new_reduced_classifications(class_L, class_conversion_table_name)
)

# Remove the function reference.
rm(convert_to_new_reduced_classifications)

# --- Load Additional Info & Test Results ---

# Load chem source/mitigation data (used for a subset of compounds).
### GO TO Google doc and find tab and download the whole thing make sure right SHEET NAME
chemSourceMitigation <- load.chemSourceMitigation2(chemSourceMitigationInfoTableName2, chemSourceSheetName2, uppercaseFirst)

# Remove references to keep environment clean.
rm(load.chemSourceMitigation2, chemSourceSheetName2)

# Next, load test results in two parts: raw table, then final processed data.

# 1) Read raw table, store in testResultsRawTable.
testResultsRawTable <- load.testResults_justReadTable(resultsTableName, DropSpecificChemicals, uppercaseFirst)

# Remove the loader function reference.
rm(load.testResults_justReadTable)

# Handle Wisconsin-specific fixups if needed.
#   - E.g., drop certain compounds from the raw table and masterParam for certain RMD_type.
if (WisconsinFixup == TRUE & RMD_type == "VOPAH") {
  testResultsRawTable <- testResultsRawTable %>%
    filter(!ParameterID %in% "300639")
  masterParam <- masterParam %>%
    filter(!ParameterID %in% "300639")
}

# Additional fixups if WisconsinFixup is TRUE and the RMD_type is VOPAH, PAH, or PEST.
if (WisconsinFixup == TRUE && (RMD_type == "VOPAH" || RMD_type == "PAH" || RMD_type == "PEST")) {
  # Filter raw table so it only includes ParameterIDs found in masterParam.
  testResultsRawTable <- testResultsRawTable %>%
    filter(ParameterID %in% masterParam$ParameterID)

  # If PEST or PAH but WisconsinFixup is FALSE, remove certain ParameterIDs from masterParam.
  if (RMD_type == "PEST" && WisconsinFixup == FALSE) {
    masterParam <- masterParam %>%
      filter(!ParameterID %in% "300243")
  }
  if (RMD_type == "PAH" && WisconsinFixup == FALSE) {
    masterParam <- masterParam %>%
      filter(!ParameterID %in% "300409")
  }
}

# Compare param counts from masterParam vs testResultsRawTable.
chemicalsTestedFromMasterParam <- nrow(masterParam)
uniqueChemicalsInTestResults <- testResultsRawTable %>%
  distinct(ParameterName) %>%
  count() %>%
  pull(n)

# Create a message to show if there's a mismatch.
mismatchPrompt <- paste(
  "chemicalsTestedFromMasterParam =", chemicalsTestedFromMasterParam,
  "; uniqueChemicalsInTestResults =", uniqueChemicalsInTestResults
)

# If not in certain Wisconsin scenarios, do an exact mismatch check.
if (!(WisconsinFixup == TRUE && RMD_type == "PAH") && !(WisconsinFixup == TRUE && RMD_type == "PEST")) {
  if (chemicalsTestedFromMasterParam != uniqueChemicalsInTestResults) {
    if (!allowDifferentParameterCounts) {
      stop(
        "Stopped because MISMATCH COUNT -- ", mismatchPrompt,
        "  Current file name is = MyExp_Base_Code_???"
      )
    }
  }
} else {
  # Loose mismatch check if the scenario is PAH or PEST in Wisconsin.
  if (abs(chemicalsTestedFromMasterParam - uniqueChemicalsInTestResults) > 0) {
    if (!allowDifferentParameterCounts) {
      stop(
        "Stopped because MISMATCH COUNT > 0 -- ", mismatchPrompt,
        "  Current file name is = MyExp_Base_Code_???"
      )
    }
  }
}

# Remove temporary mismatchPrompt.
rm(mismatchPrompt)

# Load final processed testResults, applying domain logic like unit conversions.
testResults <- load.testResults(
  testResultsRawTable, masterParam, ExpectedUnits,
  DropAllZeroSampleNumbers, DropSpecificChemicals,
  current_filename
)

# Remove objects and functions no longer needed.
rm(testResultsRawTable, allowDifferentParameterCounts, load.testResults, ExpectedUnits, DropAllZeroSampleNumbers)

# Optional fixup logic for special cases.
# New call
if (FixupForAnyone) {
  if (!is.null(FixupFile)) {
    # Create a customer config list with all the needed settings
    customerConfig <- list(
      GEORGETOWNFixUp = GEORGETOWNFixUp,
      RMD_type = RMD_type,
      DartmouthFixup = DartmouthFixup,
      UCSF2020Fixup = UCSF2020Fixup,
      CombinedTestData = CombinedTestData,
      WisconsinFixup = WisconsinFixup,
      LorealFixup = LorealFixup,
      BuffaloFixup = BuffaloFixup,
      UCSFplusRandom10Fixup = UCSFplusRandom10Fixup,
      UniVisionFixup = UniVisionFixup,
      wristbands_time_adjusted_one_week_not_weight = wristbands_time_adjusted_one_week_not_weight,
      UNMFixup = UNMFixup,
      COLORADOFixUp = COLORADOFixUp,
      ULILLEFRANCEFixup = ULILLEFRANCEFixup,
      UCONNFixUp = UCONNFixUp,
      CHICAGOFixUp = CHICAGOFixUp,
      SBIR_P1_May2022Fixup = SBIR_P1_May2022Fixup,
      UC_DAVISFixup = UC_DAVISFixup,
      BostonFixup = BostonFixup,
      UFL_FloridaFixup = UFL_FloridaFixup,
      LouisvilleFixup = LouisvilleFixup,
      SBIR_P2_Part1_71_FixUp = SBIR_P2_Part1_71_FixUp,
      SBIR_P2_Part2_35_FixUp = SBIR_P2_Part2_35_FixUp,
      SBIR_P2_Part1and2_35and71_FixUp = SBIR_P2_Part1and2_35and71_FixUp,
      FixupForAnyone = FixupForAnyone
    )

    # If a fixup file is provided, do final tweaks on testResults.
    testResults <- fixUpTestResults(testResults, FixupFile, customerConfig, current_filename)
  }
} else if (doAIRplusNioshOSHAreporting) {
  # If we needed fixups for air calculations but didn't do them, throw an error.
  stop("Not doing fixup, air calc won't work.")
}

# Remove references for fixup variables.
rm(
  Big_Mas15_List_Fixup, BostonFixup, BuffaloFixup, CHICAGOFixUp,
  COLORADOFixUp, FixupFile, FixupForAnyone, fixUpTestResults,
  GEORGETOWNFixUp, LorealFixup, LouisvilleFixup, SBIR_P1_May2022Fixup,
  UC_DAVISFixup, UCONNFixUp, UCSF2020Fixup, UCSFplusRandom10Fixup,
  UFL_FloridaFixup, ULILLEFRANCEFixup, UMTFixup, UniVisionFixup,
  UNMFixup, WisconsinFixup
)

# Demo data subset logic: if we want to create a smaller or anonymized dataset.
if (makeIntoDemoData) {
  # subject <- minMaxTR$SampleNumber[2] ## HaRD CODE TO one less than MAX SUBJECT for DEMO DATA
  # subject <- minMaxTR$SampleNumber[round(nrow(minMaxTR)/2,0)]   ## Hard code to average # of compounds
  # subject <- minMaxTR$SampleNumber[nrow(minMaxTR)-1]  ## ## Hard code to one less than MIN # of compounds
  # subject <- "AA90GCC"  ### Hardcoded to make into the one person with highest of a compound in agricultural and pharm products..
  #               .Bis(2-ethylhexyl)phthalate   AA90GCC   856000   Agricultural & Pharmaceutical Chemicals
  testResults <- makeIntoDemoDataResults(testResults, howManyDemoResults)
  rm(howManyDemoResults)

  # Replace numeric sample numbers with letters.
  testResults$SampleNumber <- testResults$SampleNumber %>%
    str_replace_all(c("1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"))

  # Pick an example subject if needed for demonstration.
  subject <- testResults %>%
    slice(1) %>%
    pull(SampleNumber)
}

# Remove function reference.
rm(makeIntoDemoDataResults)

# Remove zero results from testResults. (Assumes 0 means no detection)
testResults <- testResults %>%
  filter(Result > 0)

# If we're doing a Pre/Post analysis, subset those.
if (testing_PRE_POST) {
  testResultsPrePost <- testResults %>%
    filter(PrePost != "") %>%
    filter(PartName != "")

  # Identify PartNames that actually have both pre and post.
  testPartNamesWithPairs <- testResultsPrePost %>%
    select(PartName, PrePost) %>% unique() %>%
    group_by(PartName) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    select(PartName) %>% unique()

  # Keep only PartNames that appear in both pre and post.
  testResultsPrePost <- testResultsPrePost %>%
    filter(PartName %in% testPartNamesWithPairs$PartName)

  rm(testPartNamesWithPairs)
}

# For air calculations, augment testResults with concentration info and NIOSH/OSHA fields.
if (doAIRplusNioshOSHAreporting) {
  testResults <- addAirCalculationInformation(testResults, airConcentrationTable, cm3VolumeSiliconeOfOneGram, ExpectedUnits)
  testResults <- addAirNioshOsha(testResults, airNioshOshaTable)
  # MaxNioshOsha is the maximum of NIOSH vs OSHA limit for each row.
  testResults$MaxNioshOsha <- pmax(testResults$NIOSH, testResults$OSHA, na.rm = TRUE)
}

# Remove references to air-related functions.
rm(addAirCalculationInformation, airConcentrationTable, airNioshOshaTable, addAirNioshOsha)

# If we only want a subset of data for certain batch numbers, do that here.
if (subsetBasedOnBatchNumber) {
  testResults <- onlyPickSomeBatchesFromBiggerData(testResults, batchNumbers)
}

# Remove these subset references.
rm(subsetBasedOnBatchNumber, onlyPickSomeBatchesFromBiggerData)

# Output CSV if needed (only once per session)
if (!exists("result_file_output_done")) {
  result_file_output_done <- FALSE
}

if (!result_file_output_done) {
  result_file_output_done <- TRUE
  # Write out the testResults (and possibly classification, masterParam) to file.
  customer_Output(
    testResults,
    class_L,
    masterParam,
    DataFile_and_OutputFile_Prepend,
    DartmouthFixup,
    output_directory = here::here("results_output", DataFile_and_OutputFile_Prepend)

  )
  rm(customer_Output)
}

# Load additional risk data sets (various regulatory or hazard references)
riskCalifProp65 <- load.riskCalifProp65(riskCalifProp65TableName, masterParam)
rm(load.riskCalifProp65, riskCalifProp65TableName)

epaIris <- load.epaIris(epaIrisTableName)
rm(load.epaIris, epaIrisTableName)

IARCRisk <- load.IARCRisk(IARCRiskTableName, riskIARCdecodeTableName, masterParam)
rm(load.IARCRisk, IARCRiskTableName, riskIARCdecodeTableName)

# Remove leftover table name variables.
rm(
  masterParamTableName,
  data.load.R.filename,
  r_code
)

# Prepare classification data for summary

# Summaries of testResults.
SubClassScores <- testResults %>%
  filter(Result > 0) %>%  # Only keep positive results.
  left_join(class_L, by = "ParameterID", relationship = "many-to-many") %>% # Add classification info.
  select(classification, SampleNumber) %>%
  count(classification, SampleNumber) %>%
  arrange(SampleNumber) %>%
  dplyr::rename(aggScore = n)

# results_W is a wide version of testResults, with each SampleNumber as a column.
results_W <- testResults %>%
  select(SampleNumber, ParameterName, Result) %>%
  pivot_wider(
    names_from = SampleNumber,
    values_from = Result,
    values_fill = 0
  )


# Move parameter names into row names.
#   > rownames(results_W) <- results_W$ParameterName
# Warning message:
#   Setting row names on a tibble is deprecated.

### 2/26/2025  STOP putting in ROWNAMES???
#rownames(results_W) <- results_W$ParameterName
#results_W$ParameterName <- NULL

# Create final testResults.big adding zero-rows for each tested chemical.

# 1) Optionally hold partial data if needed.
# testResults.hold <- testResults

# 2) Build a lookup from SampleNumber to PureSampleName.
sampleLookup <- testResults %>%
  select(SampleNumber, PureSampleName) %>%
  filter(PureSampleName != "NA") %>%
  unique()

# 3) Reintroduce missing (ParameterID, SampleNumber) combos with 0 as default.
testResults <- testResults %>%
  complete(ParameterID, SampleNumber, fill = list(Result = 0, ResultOriginal = 0, Days_worn = 0, size_factor = 0, week_factor = 0))

# Overwrite ParameterName, CASNumber from masterParam (these are presumably more official or authoritative).
testResults$ParameterName <- NULL
testResults$CASNumber   <- NULL

testResults <- testResults %>%
  left_join(masterParam, by = "ParameterID")

# Overwrite PureSampleName using the sampleLookup.
testResults$PureSampleName <- NULL

testResults <- testResults %>%
  left_join(sampleLookup, by = "SampleNumber")

# Build final testResults.big
# (Creating testResults.big) We transform testResults into a 'testResults.big' data frame by:
# 1) Normalizing results by parameter (norm_Result)
# 2) Computing quartiles for each parameter
# 3) Flagging the maximum values

#####
testResults.big <- testResults %>%
  group_by(ParameterID) %>%

  # Normalize 'Result' within each 'ParameterID' group
  mutate(norm_Result = Result / max(Result)) %>%

  # Remove entire group if all values are zero
  filter(!sum(Result) == 0) %>%

  # Count how many distinct nonzero values exist
  mutate(distinct_nonzero = length(unique(Result[Result > 0]))) %>%

  # Assign quartile values while handling edge cases
  mutate(
    quartile = case_when(

      # 🟢 If all nonzero values are identical, assign quartile = 4
      distinct_nonzero < 2 & Result > 0 ~ 4,

      # 🟢 Keep zeros in their own category (quartile = 0)
      Result == 0 ~ 0,

      # 🟢 Otherwise, assign quartiles using `cut()`, ensuring at least 2 breakpoints
      TRUE ~ as.numeric(
        cut(
          x = Result,

          # 🔹 Ensure at least 2 unique breakpoints in `cut()`
          breaks = sort(unique(c(0, quantile(
            Result[Result > 0],  # Consider only nonzero values
            probs = 0:4 / 4,     # Compute quartiles
            na.rm = TRUE
          )))),

          include.lowest = TRUE,  # Include lowest range in binning
          labels = FALSE          # Assign quartile numbers (1, 2, 3, 4)
        )
      )
    )
  ) %>%
  # 2.5) Mark rows whose Result is the highest in this group
  mutate(is_this_max = Result == max(Result, na.rm = TRUE)) %>%
  # 3) Shift so the top bin is always 4, unless there's only one distinct non-zero value
  mutate(
    param_max = max(quartile, na.rm = TRUE),
    quartile = if_else(
      distinct_nonzero >= 2 & quartile > 0,
      quartile + (4 - param_max),
      quartile
    )
  ) %>%
  ungroup() %>%
  # Round the Result values
  mutate(Result = round(Result, 2)) %>%
  # Clean up temporary columns and fill any NAs
  select(-param_max, -distinct_nonzero) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate_if(is.character, ~ replace_na(., "not_found"))



# Remove the original testResults since we're done building testResults.big.
rm(testResults)

# Load Silent Spring health effects data.
healthEffects <- load.healthEffects(healthEffectsTableName) %>%
  rename(CASNumber = cas_rn) %>%   # Align naming with testResults.
  distinct()

# Merge the healthEffects data into testResults.big by CASNumber.
testResults.big <- testResults.big %>%
  left_join(healthEffects, by = "CASNumber")

# Remove loading function references.
rm(load.healthEffects, healthEffectsTableName)



# Read CSV instead of read.table (better for modern workflows)
classExplainTable <- load.classificationTable(class_explain_table_name) %>%
  rename(`Chemical Group` = Classification)  # Rename for clarity

# Remove loading function references.
rm(load.classificationTable)

# Summaries
#   - howManyWristbandsTested tracks how many distinct samples (wristbands) are in the data.
howManyWristbandsTested <- length(unique(testResults.big$SampleNumber))

# SubClassAtleastOne checks how many wristbands had at least one detect in each classification.
SubClassAtleastOne <- testResults.big %>%
  mutate(Result = case_when(
    Flag == "Y" ~ 100,   # If 'Flag' is 'Y', treat it as a high value (100).
    TRUE         ~ Result
  )) %>%
  filter(Result > 0) %>%  # Only keep positive (or flagged) results.
  inner_join(class_L, by = "ParameterID", relationship = "many-to-many") %>% # Add classification.
  select(classification, SampleNumber) %>%
  distinct() %>%
  group_by(classification) %>%
  summarize(WristbandsWithThisClassification = dplyr::n()) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  arrange(classification) %>%
  mutate(
    percentageWBwithClassification = scales::percent(round(WristbandsWithThisClassification / howManyWristbandsTested, 2))
  ) %>%
  rename(
    "Classification"                 = classification,
    "Wristbands with This Classification" = WristbandsWithThisClassification,
    "Percentage with this Classification" = percentageWBwithClassification
  )

# Count how many unique chemicals were detected across all wristbands.
howManyUniqueChemFoundAcrossAllWristbands <- length(unique(testResults.big$ParameterID))

# minMaxTR: Summarize how many detects each SampleNumber has.
minMaxTR <- testResults.big %>%
  mutate(Result = case_when(
    Flag == "Y" ~ 100,  # If flagged, treat as definitely present.
    TRUE         ~ Result
  )) %>%
  select(SampleNumber, ParameterID, Result) %>%
  group_by(SampleNumber) %>%
  summarise(Count = sum(Result > 0)) %>%
  arrange(desc(Count))

# If we are still in a demo context, pick a subject from ~20% into the dataset.
if (makeIntoDemoData) {
  subject <- minMaxTR %>%
    slice(round(n() * 0.2)) %>%
    pull(SampleNumber)
}

rm(makeIntoDemoData)

# Done.



#----------------put refactored code ABOVE here
#----------------put refactored code ABOVE here



# cat("A 333 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

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
## OLD CODE:   replaced with 2 lines below   statSummary <- testResults.big[testResults.big$Result > 0, ]
statSummary <- testResults.big %>%
  filter(Result > 0) %>%
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
CombinedTestDataStatSummary <- read.csv(setCOMBINED_DRSdirectory("StatSummary682.csv")) %>%
  replace(is.na(.), "Undefined")
# Add ParameterID to table
### NOTE that all BATCH # < 100 are dartmoutnh... and then we have 200 and 300 and 400 and then are CHICAGO, COLORADO, and GEORGETOWN probalby in taht order.


# cat("A 444 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

# write.csv(statSummary, file = setCOMBINED_DRSdirectory("StatSummary682.csv"), row.names=FALSE)


# NOW try to combine statSummary with CombinedTestDataStatSummary
bigStatSummary <- CombinedTestDataStatSummary %>% left_join(statSummary, by = "Chemical_Name")
# bigStatSummary$Full_ST_Dev<- as.character(bigStatSummary$Full_ST_Dev)
bigStatSummary[is.na(bigStatSummary)] <- ""


if (CombinedTestData) {
  bigStatSummaryJustTheseChemicals <- statSummary %>% left_join(CombinedTestDataStatSummary, by = "Chemical_Name")
  # bigStatSummaryJustTheseChemicals$Full_ST_Dev        <- as.character(bigStatSummaryJustTheseChemicals$Full_ST_Dev       )
  bigStatSummaryJustTheseChemicals[is.na(bigStatSummaryJustTheseChemicals)] <- ""
}

# cat("933 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)

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
CombinedTestDataClass_StatSummary <- read.csv(setCOMBINED_DRSdirectory("csSummary682.csv")) %>%
  rename(combinedTestDataCountMeanResult = countMeanResult, combinedTestDataMeanSumResult = meanSumResult)

csSummary <- CombinedTestDataClass_StatSummary %>%
  left_join(csSummary, by = "classification") %>%
  replace(is.na(.), 0)

# cat("944 in BASE CODE...\n", file = "debug_log.txt", append = TRUE)
###

# rm(csSummary,csSummary1,csSummary2   )
# write.csv(csSummary, file = setCOMBINED_DRSdirectory("csSummary682.csv"), row.names=FALSE)
# CombinedTestDataStatSummaryClassifications<-read.csv(setCOMBINED_DRSdirectory("csSummary682.csv"))%>%
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
