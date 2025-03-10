#
# BE SURE TO CLEAR EVERYTHING AFTER THIS RUNS or at least before running it AGAIN
# THIS EXPLAINS how to do this stuff:  http://yihui.name/knitr/demo/pandoc/
#
# This file automates the process of producing multiple reports
#
# NOTE that originally we did a LOOP which wrote out a config.pandoc file and then ran commandline using that file
#   BUT that was not really the right way... better to use the RENDER package directly not KNIT then PANDOC but just RENDER
#   see BOOK for more info (Dynamic DOcuments with R, 2nd Edition)
## SEE THIS INFO:  https://pkgs.rstudio.com/rmarkdown/reference/render.html     for more on RENDER



# FIRST clean up environment a little and set some local variables
rm(list=ls())  # clear things out
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library("here")
setwd(here::here())


# SUBJECT SHOULD NEVER EXIST on runing this since we JUST deleted everything with the "rm" command
# so... this loads all the "global variables"

### THIS is  a global variable... probably not great idea but it works
In_Print_All_Subjects <- TRUE # USE this when setting key variables below to ALWAYS set  customiization-per-user to be ON

### THIS is a  function to actually render the reports.  Define it here to make things clear what variables it depends on
#' Render Reports for Multiple Subjects
#'
#' This function generates reports for each subject in the `subjectsToProcess` dataframe.
#'
#' @param subjectsToProcess A dataframe containing subjects to process.
#' @param outputFileType The file type for the output (e.g., "html", "docx").
#' @param output_directory The directory to save the output files.
#' @param docType The document type for rendering (e.g., "html_document").
#' @param rmd_code The path to the RMarkdown template.
#' @param logo_path The path to the logo image.
#' @return None
render_reports <- function(subjectsToProcess, outputFileType, output_directory, docType, rmd_code, logo_path) {
  ## I replaced using "wideAllSubjects" with "subjecsToProcess" which is slightly confusing cause obviously I'm not needing to use wideAllSubjects at ALL
  ## So really i should get RID of wide all subjects
  for (i in 1:nrow(subjectsToProcess)) {
    #  i<-1
    pure_sample_name <- subjectsToProcess$PureSampleName[i]
    subject <- subjectsToProcess$SampleNumber[i]
    outputFileName <- paste0("MyExposome_Report_", pure_sample_name, ".", outputFileType)

    rmarkdown::render(
      input = rmd_code,
      output_file = outputFileName,
      output_dir = output_directory,
      params = list(logo_path = logo_path)
    )
  }
}

#if (!exists("subject")) {   # STOP USING SUBJECT to see if MyExp_set_key_variables.R is loaded
if (!exists("MyExp_set_key_variables_R_file_is_loaded")) { ##  NEED TO HARD CODE  R Directory Location
  #source("MyExp_set_key_variables.R") # source() actually RUNS IT not just loads it
  ### Here we need to hardcode "R" cause function not set yet
  source(here::here("R", "MyExp_set_key_variables.R"))
}

###WE need to define any LOCAL TO THIS file variables NOW because
### They get overwritten by the loading of the source somehow.

#EDFoutputFile <- "Pah_fakeOutputFile.csv"
# Where to put OUTPUT FILES   ### REALLY i should do this using the prepend variable for consistency BUT i didn't
#
#  IS THERE A LOGIC ERROR BELOW OF ME NOT TESTING IF A VARIABLE EXISTS BEFORE SEEING IF IT IS SET TO TRUE?
#
if (exists("SBIR_P2_Part1_71_FixUp")){
  if (SBIR_P2_Part1_71_FixUp |SBIR_P2_Part1and2_35and71_FixUp ) {
    output_directory <- here::here("results_output","SBIR_Results_Output")
  } else {
    output_directory <- here::here("results_output")
  }
} else {
  output_directory <- here::here("results_output")
}

### Call the SOURCE
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
### THIS if test is SILLY cause we JUST above did rm(list=ls()) and then just loaded the key variables
###   so there is no way masterParam exists
### STOP using masterParam and start using MyExp_Base_Code_v6_R_Code_was_run which is "r_code"
#if (!exists("masterParam")) {
if (!exists("MyExp_Base_Code_v6_R_Code_was_run")) {
  source(r_code)  # source() actually RUNS IT not just loads it r_code=MyExp_Base_Code_v?.R
}


# #Setup data directory
# if (!file.exists(output_directory)) {
#   dir.create(output_directory)
# }

#now loop through all the subjects who have any results in testResults
# that list of subjects can be found with
#allSubjects<- unique(testResults.big[testResults.big$Result>0,]$SampleNumber)

# basically i am just grabbing all the names from testResults BUT i will later be redoing whole loop...
#   just using this as way to do one-by-one reports not for data
# This is never used other than as a way to set the value of subjectsToProcess
wideAllSubjects <- testResults.big %>%
  filter(Result > 0) %>%
  distinct(PureSampleName, SampleNumber, Lab_Submission_Batch)

#subjectsToProcess <- wideAllSubjects   # JUst for some versions just set equal.
#   BUT below use other system to subset which things to print (differentiate between what we include in the data and which we print)


### for SBIR_P2_Part1and2_35and71_FixUp   I am only going to PRINT from the newest batch even while i process all 71+31 I'm only generating 31 (actually 35 not 31)
subjectsToProcess <- wideAllSubjects %>%
  filter(Lab_Submission_Batch=="PO 262")

### AS FAR AS I SEEE, allSubjects is NEVER USED??
#allSubjects<- unique(testResults.big[testResults.big$Result>0,]$PureSampleName)  # On 10/17/2024 I switched to using PureSampleName instead of FSESID

### LET"S PICK just 10 from this list
### make sure they are "WB" samples:
#wideAllSubjects <- wideAllSubjects %>%  filter(str_ends(SampleNumber, "-WB")) %>%  sample_n(10)

docType<-"html_document"
outputFileType<-"html"
logo_path <- here::here("images", "myExposomeLogo_with_transparent_padding25.png")

#THESE SETTINGS FOR WORD DOC do NOT HANDLE BIG TABLES WELL YET.
#docType<-"word_document"
#outputFileType<-"docx"

###THESE SETTINGS for producing PDF document do NOT handle tables properly and FAIL probably on table width.
#docType<-"pdf_document"
#outputFileType<-"pdf"

# TEST for not existing
if (!file.exists(rmd_code)) {
  stop("RMarkdown template not found: ", rmd_code)
}

# Call the function to render all the reports
# THIS does all the work here
# everything below is commented out and old
render_reports(
  subjectsToProcess=subjectsToProcess,
  outputFileType = outputFileType,
  output_directory = output_directory,
  docType = docType,
  rmd_code = rmd_code,
  logo_path = logo_path
)

rm(list=ls())  # clear things out


