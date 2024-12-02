#THIS CODE IS TO look at the files marc scraped from the patch test site and compare
# the CAS numbers
#     probably could also manually look them up by name?
#
# Set all key variables.  Use the existance (or not) of "subject" to decide if they need to be loaded
if (!exists("subject")) {
  source(here::here("R","MyExp_set_key_variables.R"))
}

#results_W_CustName_NEW<-"nothing"

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

###  STUFF BELOW IS TO EXPERIMENT... shoujld all be commented out!!!

if(TRUE) {
  #findMatchingRows(getPatchData("AC-1000"),masterParam)
  #ac1000<-getPatchData("AC-1000")
  #f1000<-getPatchData("F-1000")
  #length(unique(ac1000$CASNumber))
  #length(unique(f1000$CASNumber))
  howManyCompoundsInTest <- function(patchTestType){
    patchData<- getPatchData(patchTestType)
    return(length(unique(patchData$CASNumber)))
  }

  howManyCompoundsIn2Tests <- function(patchTestType1,patchTestType2){
    patchData1<- getPatchData(patchTestType1)
    patchData2<- getPatchData(patchTestType2)
    return(length(union(unique(patchData1$CASNumber),unique(patchData2$CASNumber) )))
  }


  howManyCompoundsMatchTest <- function(patchTestType){
    match_patch<-findMatchingRows(masterParam,getPatchData(patchTestType))
    return(length(unique(match_patch$CASNumber)))
  }

  howManyCompoundsMatch2Tests <- function(patchTestType1,patchTestType2){
    match_patch1<-findMatchingRows(masterParam,getPatchData(patchTestType1))
    match_patch2<-findMatchingRows(masterParam,getPatchData(patchTestType2))
    return(length(union(unique(match_patch1$CASNumber), unique(match_patch2$CASNumber))))
  }

  #howManyCompoundsInTest("F-1000")
  #howManyCompoundsMatchTest("F-1000")

  #howManyCompoundsMatch2Tests("AC-1000","F-1000")

  #NumUniqueCASNumbers_AC_and_F <-length(unique(union((unique(ac1000$CASNumber)),(unique(f1000$CASNumber)))))
  #match_ac1000<-findMatchingRows(masterParam,getPatchData("AC-1000"))
  #match_f1000<-findMatchingRows(masterParam,getPatchData("F-1000"))

  #NumUniqueCASNumbers_AC_and_F_matching <- length(union(unique(match_ac1000$CASNumber), unique(match_f1000$CASNumber)))
}

