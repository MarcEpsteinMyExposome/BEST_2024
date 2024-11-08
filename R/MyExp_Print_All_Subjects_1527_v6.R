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
render_reports <- function(wideAllSubjects, outputFileType, output_directory, docType, rmd_code, logo_path) {
  for (i in 1:nrow(wideAllSubjects)) {
    pure_sample_name <- wideAllSubjects$PureSampleName[i]
    subject <- wideAllSubjects$SampleNumber[i]
    outputFileName <- paste0("MyExposome_Report_", pure_sample_name, ".", outputFileType)

    rmarkdown::render(
      input = rmd_code,
      output_file = outputFileName,
      output_dir = output_directory,
      params = list(logo_path = logo_path)
    )
  }
}

if (!exists("subject")) {
  #source("MyExp_set_key_variables.R") # source() actually RUNS IT not just loads it
  ### Here we need to hardcode "R" cause function not set yet
  source(here("R", "MyExp_set_key_variables.R"))
}

###WE need to define any LOCAL TO THIS file variables NOW because
### They get overwritten by the loading of the source somehow.

#EDFoutputFile <- "Pah_fakeOutputFile.csv"
# Where to put OUTPUT FILES   ### REALLY i should do this using the prepend variable for consistency BUT i didn't
if (exists("SBIR_P2_Part1_71_FixUp")){
  if (SBIR_P2_Part1_71_FixUp){
    output_directory <- here("results_output","SBIR_Results_Output")
  } else {
    output_directory <- here("results_output")
  }
} else {
  output_directory <- here("results_output")
}

### Call the SOURCE
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
### THIS if test is SILLY cause we JUST above did rm(list=ls()) and then just loaded the key variables
###   so there is no way masterParam exists
if (!exists("masterParam")) {
  source(r_code)  # source() actually RUNS IT not just loads it r_code=MyExp_Base_Code_v?.R
}


# #Setup data directory
# if (!file.exists(output_directory)) {
#   dir.create(output_directory)
# }

#now loop through all the subjects who have any results in testResults
# that list of subjects can be found with
#allSubjects<- unique(testResults.big[testResults.big$Result>0,]$SampleNumber)

wideAllSubjects <- testResults.big %>%
  filter(Result > 0) %>%
  select(PureSampleName,SampleNumber) %>%
  unique()

### AS FAR AS I SEEE, allSubjects is NEVER USED??
#allSubjects<- unique(testResults.big[testResults.big$Result>0,]$PureSampleName)  # On 10/17/2024 I switched to using PureSampleName instead of FSESID

### LET"S PICK just 10 from this list
### make sure they are "WB" samples:
#wideAllSubjects <- wideAllSubjects %>%  filter(str_ends(SampleNumber, "-WB")) %>%  sample_n(10)


#random10 <- sample_n(wideAllSubjects, 10)
#wideAllSubjects<-random10

docType<-"html_document"
outputFileType<-"html"
logo_path <- here("images", "myExposomeLogo_with_transparent_padding25.png")

#THESE SETTINGS FOR WORD DOC do NOT HANDLE BIG TABLES WELL YET.
#docType<-"word_document"
#outputFileType<-"docx"

###THESE SETTINGS for producing PDF document do NOT handle tables properly and FAIL probably on table width.
#docType<-"pdf_document"
#outputFileType<-"pdf"


# Call the function to render all the reports
# THIS does all the work here
# everything below is commented out and old
render_reports(
  wideAllSubjects = wideAllSubjects,
  outputFileType = outputFileType,
  output_directory = output_directory,
  docType = docType,
  rmd_code = rmd_code,
  logo_path = logo_path
)

#Delete anything we created
rm(list=ls())



