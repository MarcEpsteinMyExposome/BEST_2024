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
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
rm(list=ls())  # clear things out  ADD A COMMENT

# SUBJECT SHOULD NEVER EXIST on runing this since we JUST deleted everything with the "rm" command
# so... this loads all the "global variables"

In_Print_All_Subjects <- TRUE # USE this when setting key variables below to ALWAYS set  customiization-per-user to be ON

if (!exists("subject")) {
  source(here("R","MyExp_set_key_variables.R")) # source() actually RUNS IT not just loads it
}

# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
### THIS if test is SILLY cause we JUST above did rm(list=ls()) and then just loaded the key variables
###   so there is no way masterParam exists
if (!exists("masterParam")) {
  source(here("R", r_code))  # source() actually RUNS IT not just loads it r_code=MyExp_Base_Code_v?.R
}

###WE need to define any LOCAL TO THIS file variables NOW because
### They get overwritten by the loading of the source somehow.

#EDFoutputFile <- "Pah_fakeOutputFile.csv"
# Where to put OUTPUT FILES
output_directory <- "results_output\\SBIR_Results_Output"

#Setup data directory
if (!file.exists(output_directory)) {
  dir.create(output_directory)
}


#now loop through all the subjects who have any results in testResults
# that list of subjects can be found with
#allSubjects<- unique(testResults.big[testResults.big$Result>0,]$SampleNumber)

wideAllSubjects <- testResults.big %>%
  filter(Result > 0) %>%
  select(PureSampleName,SampleNumber) %>%
  unique()

allSubjects<- unique(testResults.big[testResults.big$Result>0,]$PureSampleName)  # On 10/17/2024 I switched to using PureSampleName instead of FSESID

#  UNCOMMENT Line below to BYPASS long loop
#COULD override setting subject and then just one-line-at-a-time Run the FOR loop for testing
#allSubjects <- allSubjects[1]  #THIS does just ONE
#allSubjects <-"MYEXP5"
#allSubjects <-"MYEXPONEROW"
#allSubjects <-"A180237C"  #PAH tester
#allSubjects <-"A170229"  #PAH real fireman example first in list

docType<-"html_document"
outputFileType<-"html"

#THESE SETTINGS FOR WORD DOC do NOT HANDLE BIG TABLES WELL YET.
#docType<-"word_document"
#outputFileType<-"docx"

###THESE SETTINGS for producing PDF document do NOT handle tables properly and FAIL probably on table width.
#docType<-"pdf_document"
#outputFileType<-"pdf"
#i=2
#TRY RENDER
#for (WideSubject in wideAllSubjects) {
#for (i in nrow(wideAllSubjects) {
for (i in 1:nrow(wideAllSubjects)) {
  #i<-10  # JUST FOR TESTING
  # Access PureSampleName for the current row
  pure_sample_name <- wideAllSubjects$PureSampleName[i]
    # Access SampleNumber for the current row
  subject <- wideAllSubjects$SampleNumber[i]
  #WideSubject<-wideAllSubjects[1,]
  # set name of output file.  Change it for each subject

  # Define the image path using here::here()
  logo_path <- here("images", "myExposomeLogo_with_transparent_padding25.png")
  outputFileName<-paste0("MyExposome_Report_",pure_sample_name,".",outputFileType)
  # NOTE could wrap rmarkdown::render call in try() to handle ERROR condition better?
  #rmarkdown::render(rmd_code,docType,output_file=outputFileName,output_dir=output_directory)
  #rmarkdown::render(rmd_code,output_file=outputFileName,output_dir=output_directory)

  # Render the Rmd file with specified parameters
  # USE the logo_path param into the javascript to pass the base64 encoding we create here using the absolute path
  rmarkdown::render(
    input = rmd_code,                # Path to your Rmd file
    output_file = outputFileName,     # Name of the output HTML file
    output_dir = output_directory,    # Directory for the output file
    params = list(logo_path = logo_path)  # Pass logo_path as a parameter
  )

}


#Delete anything we created
rm(list=ls())

