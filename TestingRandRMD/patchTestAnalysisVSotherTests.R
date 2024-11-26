#THIS CODE IS TO look at the files marc scraped from the patch test site and compare
# the CAS numbers
#     probably could also manually look them up by name?
#
# Set all key variables.  Use the existance (or not) of "subject" to decide if they need to be loaded
if (!exists("subject")) {
  source(here::here("R","MyExp_set_key_variables.R"))
}
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
if (!exists("masterParam")) {
  source(r_code)
}

# Clean up environment a little
rm(list=ls()[!ls() %in% c("masterParam")])

getPatchData<-function(patchTestType) {
  patchFileName <- paste0(patchTestType,".xlsx")
  # Now read in one of the patchtest files
  patchTestQualifiedFileName <- here::here("data","patchTestData",patchFileName)
  patchTestData<-read_xlsx(patchTestQualifiedFileName)
  if ("CAS Number" %in% names(patchTestData)) {
    patchTestData <-patchTestData %>%
      rename(CASNumber = "CAS Number")
  }
  return(patchTestData)
}


findMatchingRows<-function(myExposomeTable,patchTable){
  intersectingCAS<-intersect(patchTable$CASNumber,myExposomeTable$CASNumber)
  intersectRows <- patchTable[patchTable$CASNumber %in% intersectingCAS,]
  return(intersectRows)
}
