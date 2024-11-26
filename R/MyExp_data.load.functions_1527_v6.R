###  MARC is setting up some separate FUNCTIONS just as a way to
###  > Start using functiosn (duh)
###  > isolate load-and-b asic-clean-and-organize of data DIFF
#
# VARIOUS OLD NOTES:
#  HERE ARE GOOD URLs
#  http://mannheimiagoesprogramming.blogspot.com/2012/06/drawing-heatmaps-in-r-with-heatmap2.html
#  http://biostat.jhsph.edu/~jleek/code/sotu2011-2012comparison.R
#  PER MORT:  Geom charts   http://docs.ggplot2.org/0.9.2.1/geom_tile.html
#    good GGPLOT legends and such:  http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
#  GREAT for legends/axis/ticks in graphs:  http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#change-size-of-tick-text-axis.text.x
#  HOW to use rmarkdown with R  http://zevross.com/blog/2014/07/09/making-use-of-external-r-code-in-knitr-and-r-markdown/
#
#  BEST use of R markdown w/ Pander:  http://galahad.well.ox.ac.uk/repro/
#              #  http://rapporter.github.io/pander/#table-and-cell-width
#              #   http://cran.r-project.org/web/packages/pander/pander.pdf
#
# # HOW TO PLOT:  http://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

#  DOCUMENTATION on KNITR:  http://cran.r-project.org/web/packages/knitr/knitr.pdf

# masterParamTableName <-"./data/MASV15_parameters.csv"

# masterParamTableName <-"./data/MASVtest2.csv"

# READ IN the data
load.masterParam <- function(masterParamTableName, DropSpecificChemicals) {
  masterParam <- read.table(
    masterParamTableName,
    sep = ",",
    header = TRUE,
    colClasses = "character" # Import all as character
    ,
    comment.char = "" # Don't allow use of "#" as comment in input
    ,
    quote = "\"",
    fileEncoding = "UTF-8-BOM"
  )

  # TEST that there are no columns where the columns where chemID is na
  # masterParam[is.na(masterParam$CasNumber),]
  # TEST that there are no columns where the columns where chemID is null
  #  masterParam[is.null(masterParam$CasNumber),]
  # TEST that there are no columns where the columns where chemID is "NULL"
  #  masterParam[(masterParam$CasNumber=="NULL"),]
  # TEST FOR WHICH COLUMNS the columns where chemID is ""
  #  masterParam[(masterParam$CasNumber==""),]
  # FOUND THERE WERE FOUR WITH BLANK CASNUmber
  ##  FOUND THESE COLUMNS... so use the ParameterNAME as the CASNUmber

  ## SOMETIMES the MasterParam data file comes with CASNumber instead of CasNumber and if so rename the column to fix
  if (is.null(masterParam$CasNumber) &
    !is.null(masterParam$CASNumber)) {
    masterParam <- masterParam %>% dplyr::rename(CasNumber = CASNumber)
  }


  # Set any blank CASNumber to be equal to ParameterName
  masterParam[(masterParam$CASNumber == ""), "CASNumber"] <-
    masterParam[(masterParam$CASNumber == ""), "ParameterName"]
  # ALSO found that some DUPLICATES exist in masterParam table (NONE in PAH)
  # masterParam[duplicated(masterParam$ParameterID, incomparables = FALSE),]
  # THESE TWO ROWS appear to be duplicated
  # ParameterID            ParameterName  CASNumber
  # 300219     4,4'-Dibromobiphenyl        92-86-4
  # 301598     PBB 52 Tetrabrombiphenyl    59080-37-4

  # FOR this NEW of 1527/1528 set there are TWO duplicates...
  # ParameterID               ParameterName Abbreviation  CASNumber
  # 176 4,6-Dinitro-o-cresol (DNOC)                534-52-1
  # 160         645                Phosphamidon              13171-21-6

  # ELIMINATE ROWS with duplicate ParameterIDs
  if (any(duplicated(masterParam$ParameterID))) {
    stop(
      "Marc Error:  STOP now -- NO WAY there should be duplicated ParameterID's",
      "  Current file name is = ",
      current_filename()
    )
  }
  # JUSTIN CASE we're deleting them somehow.....
  masterParam <-
    masterParam[!duplicated(masterParam$ParameterID, incomparables = FALSE), ]

  # Select ONLY those columns we want and RENAME the columns as we wish
  masterParam <-
    select(masterParam, ParameterID, ParameterName, CASNumber = CasNumber)

  # We would really like to capitalize the FIRST letter of each chemical (skipping numbers/spaces/etc)
  # So we define a FUNCTION which takes any string and uppercases the first letter
  uppercaseFirst <- function(txt) {
    pos <- regexpr("[A-z]", txt)
    char1 <- substr(txt, pos, pos)
    uC <- toupper(char1)
    sub("[A-z]", uC, txt)
  }
  # THEN we apply taht new function to the ParameterName mcolumn
  masterParam[, "ParameterName"] <-
    sapply(masterParam[, "ParameterName"], uppercaseFirst)

  # IF any chemicals are set to be IGNORED by being put into "DropSpecificChemicals"
  # THEN drop those chemicals from the masterParam dataset
  masterParam <- masterParam %>%
    filter(!ParameterID %in% DropSpecificChemicals)

  masterParam
}

# Read in Classification File
# Read in Classification File
# Read in Classification File
load.classification <- function(classificationTableName) {
  classification <- read.table(
    classificationTableName,
    sep = ",",
    header = TRUE,
    colClasses = "character" # Import all as character
    ,
    comment.char = "" # Don't allow use of "#" as comment in input
    ,
    quote = "\"",
    fileEncoding = "UTF-8-BOM"
  )
  # REPLACE any string="NULL" values with 0
  classification <-
    replace(classification, classification == "NULL", "0")
  # Rename masterParameterID column to ParameterID

  # names(classification)[names(classification)=="masterParameterID"] <- "ParameterID"

  # Convert Numbers to Numeric
  classification <-
    data.frame(sapply(classification, function(x) {
      as.numeric(as.character(x))
    }))

  # Name the ROWS of the classification matrix
  row.names(classification) <- classification$ParameterID

  ## ELIMINATE FIRST 1 COLUMN (was an older version)
  #  NEW, eliminate  a column named 'recordID' if it exists, if not do nothing
  classification[c("recordID")] <- list(NULL)

  ## NOW let's collapse the PESTS into one thing
  classification$pest <- pmax(
    classification$pestInsecticide,
    classification$pestRodenticide,
    classification$pestGeneral,
    classification$pestFungicide,
    classification$pestHerbicide,
    classification$pestProduct
  )
  # ELIMINATE all the columns beginning with "pest" (except exactly "pest")
  classification <-
    classification[, -grep("pest[A-z]+", names(classification))]
  ### NOW also eliminate the "deuterated" and "uncatergorized" and "natural" column
  classification$deuterated <- NULL # THIS IS Always ZERO
  classification$uncategorized <- NULL # THIS IS Always ZERO

  ### NOTE:  for 1418 and 1527/1528 these are NEVER used without some other value in the other classicications so OK to ignore.
  classification$natural <-
    NULL # this is never the ONLY classification so ignore it
  classification$phenol <-
    NULL # This defines some chemical property or other (not important) , is never the ONLY classification
  classification$aniline <-
    NULL # This defines some chemical property or other (not important) , is never the ONLY classification

  ### NOW collapse PBB and PBDE into Flame Retardant column
  ## on 6/19/2019 also start collapsing OPFR into flame retardant
  classification$flameRetardant <-
    pmax(
      classification$flameRetardant,
      classification$PBB,
      classification$PBDE,
      classification$OPFR
    ) # added 6/19/2019
  classification$PBB <- NULL
  classification$PBDE <- NULL
  classification$OPFR <- NULL # added 6/19/2019

  ### NOW collapse PAH and OPAH into PAH column
  ## on 6/19/2019 added this collapse
  classification$PAH <-
    pmax(
      classification$PAH,
      classification$OPAH
    )
  classification$OPAH <- NULL




  ### NOW collapse haloEthers and pulpAndPaper into industrial column (Chemicals in Commerce)
  classification$industrial <- pmax(
    classification$haloEthers,
    classification$pulpAndPaper,
    classification$industrial
  )
  classification$haloEthers <- NULL
  classification$pulpAndPaper <- NULL

  c <- classification
  classification <- c
  ##
  ## NOW rename all columns (except not ParameterID)
  classification <-
    plyr:::rename(
      # NOTE: using the PLYR version of rename cause it easier than "select"
      classification,
      c(
        "PAH" = PAH_text_string,
        # "OPAH" = OPAH_text_string,   # NOT USING OPAH any more after 6/19/2019
        "flameRetardant" = flameRetardant_text_string,
        "PCB" = PCB_text_string,
        "pharmacological" = pharmacological__text_string,
        "personalCare" = personalCare_text_string,
        "industrial" = industrial_text_string,
        "pest" = pest_text_string,
        # "consumer"="Consumer Products",         #NOTE the OLD pre-3/6 data has this name
        "consumerProduct" = consumerProduct_text_string,
        # NOTE the new 3/6 data has new columnName
        "dioxinsAndFurans" = dioxinsAndFurans_text_string
        # NOTE the 6/2017 data has 2 new columns we will IGNORE
      )
    )

  classification
}


### CONTINUE TO FIX UP CLASSIFICATIONS:  This builds on the data load of classifications and collapses set further.  could merge the two later
convert_to_new_reduced_classifications <- function(class_L, class_conversion_table_name) {
  # Read in the new classification mapping file to adjust the classifications down
  class_conversion_table <- read.csv(class_conversion_table_name) %>%
    select(Classification, CurrentClassifications)

  # Create a long format of the class_conversion_table
  class_conversion_long <- class_conversion_table %>%
    mutate(CurrentClassifications = strsplit(CurrentClassifications, ",\\s*")) %>%
    unnest(CurrentClassifications) %>%
    mutate(CurrentClassifications = str_trim(str_replace_all(CurrentClassifications, '[\\"]', "")))

  rm(class_conversion_table)

  # Function to find the appropriate classification
  find_classification <- function(class_OLD) {
    match <- class_conversion_long %>%
      filter(CurrentClassifications == class_OLD) %>%
      pull(Classification)

    if (length(match) > 0) {
      return(match[1])
    } else {
      print(paste("No match for:", class_OLD))
      return(NA)
    }
  }

  # Update class_L
  class_L_new <- class_L %>%
    rename(classification_OLD = classification) %>%
    rowwise() %>%
    mutate(classification = find_classification(str_trim(classification_OLD))) %>%
    ungroup()

  # Select the desired columns
  class_L_new <- class_L_new %>%
    select(ParameterID, classification)

  class_L_new
}


# Fix class_L by adding all VOC data using VOC MasterParameterTable
#  and also adding all PAH data from PAH MasterParamTable and same for FLAME and Pest (and now also VOPAH)
### FIRST create a FUNCTION which updates the class_L information with new unique rows
updateWithClassSpecificMasterParam <- function(classSpecificTitle, classSpecificMasterParamTable, class_L, DropSpecificChemicals) {
  classSpecifcMasterParam <-
    load.masterParam(
      setMASTERPARAM_CLASS_RISKSdirectory(classSpecificMasterParamTable),
      DropSpecificChemicals
    ) # Read in new parameter Table
  classSpecifcMasterParam$classification <- classSpecificTitle # hard-code value
  class_L$classification <- as.character(class_L$classification) # temporarily convert to char for union'ing
  classSpecifcMasterParam <- classSpecifcMasterParam %>% select(ParameterID, classification) # pick columns i need
  class_L <- union(classSpecifcMasterParam, class_L) # MERGE existing class_L with new masterParam
  class_L$classification <- as.factor(class_L$classification) # convert back to factor, undoing convert to CHAR above
  class_L
}







###  NOW READ IN THE RESULTS TABLE...
### SET in calling program OR SET IT HERE
# resultsTable<- "./data/ResultsTable.csv"
# resultsTableName<- "./data/ResultsTableMarc_and_Fake.csv"
# resultsTableName<- "./data/F17-03-ResultsV2.csv" # VERSION of good maimi data STILL has zero-WBs and has Flag field
# JUST IN CASE the calling program doesn't set resultsTable, here is default value

### PROBLEM:  NO RESULTS TABLE??? in 1/28/2015 delivery???
#### this is NOT a problem cause the results table is skinny and hence new columns in classification don't hurt it
#### BUT there is no results that will hit some of the new features... so need to add more results!

load.testResults_justReadTable <- function(resultsTableName, DropSpecificChemicals) {
  testResults <-
    read.table(
      resultsTableName,
      sep = ",",
      header = TRUE,
      colClasses = "character" # Import all as character
      ,
      comment.char = "" # Don't allow use of "#" as comment in input
      ,
      quote = "\"",
      fileEncoding = "UTF-8-BOM"
    )

  # If any chemicals are set as "ignore/drop these chemicals for this run of the data"
  # Then reset testResult to ignore those chemicals.
  #  ALSO need to drop those chem from masterParameter
  testResults <- testResults %>%
    filter(!ParameterID %in% DropSpecificChemicals)


  # We would really like to capitalize the FIRST letter of each chemical (skipping numbers/spaces/etc)
  # So we define a FUNCTION which takes any string and uppercases the first letter
  uppercaseFirst <- function(txt) {
    pos <- regexpr("[A-z]", txt)
    char1 <- substr(txt, pos, pos)
    uC <- toupper(char1)
    sub("[A-z]", uC, txt)
  }
  # THEN we apply taht new function to the ParameterName mcolumn
  testResults[, "ParameterName"] <-
    sapply(testResults[, "ParameterName"], uppercaseFirst)

  testResults # Return the exact table read
}

load.testResults <- function(testResultsRawTable, masterParam, ExpectedUnits, DropAllZeroSampleNumbers, DropSpecificChemicals) {
  # We read in the raw table elsewhere, now we're going to clean-it-up
  testResults <- testResultsRawTable

  # Set Test results so that anywhere it says BLOD (below limit of Detection) we now use ZERO
  testResults[testResults[, "Result"] == "BLOD", "Result"] <- 0
  testResults[testResults[, "Result"] == "BLOQ", "Result"] <- 0

  # ADD a FLAG column if none exists cause originally we didn't have that column so some old data doesn't have it
  if (!(c("Flag") %in% names(testResults))) {
    testResults$Flag <- ""
  }

  # Set testResult FLAG to contain the "Y" flag which is being used in the RESULT column.. this moves it to the FLAG column where it belongs
  #     --- IDEA is that now the FLAG column will tell us if we had a DETECTED chemical but NOT a quantification possible
  #     --- NOTE that I did this PASTE stuff to append the Y to whatever other flag found just in CASE but... always so far by itself anyway
  #### was doing THIS: testResults[testResults[, "Result"] == "Y", ]$Flag<- paste(testResults[testResults[, "Result"] == "Y","Flag" ],"Y",sep="")
  #### but NO... i'm just going to OVERWRITE any other flags with the Y flag for sure.
  if (nrow(testResults[testResults[, "Result"] == "Y", ]) > 0) {
    # testResults[testResults[, "Result"] == "Y", ]$Flag<- "Y"
    testResults[testResults[, "Result"] == "Y", "Flag"] <- "Y"
    # Set Test results so that anywhere it says Y in the result field we now use ZERO  THIS is moved, above, into the FLAG field where it belows
    # BUT NOTE that zero is NOT EXACTLY RIGHT since it is really "unknown"
    # BUT we must set to zero so we can process the data mathematically without a failure on non-numeric...
    testResults[testResults[, "Result"] == "Y", "Result"] <- 0
  }



  # Set Test results so that anywhere it says "P" we substitute ZERO.  In Dartmouth-1 results from OSU we had a "P" sometimes, unclear why
  # Mac is just DROPPING "P" cause we can't use it but... not sure how "P" is different from ZERO
  # CONCLUSION:  Per Kim "P" means we detected but is Below Limit of Detection SOOOOO we're treating like not found at all
  testResults[testResults[, "Result"] == "P", "Result"] <- 0

  # Set Test results so that anywhere it says "N.A." we DROP the line completely.  NOT SURE why "N.A." is now used but... there it goes...
  # Mac is just DROPPING "N.A." cause we can't use it but... not sure how "P" is different from ZERO
  testResults[testResults[, "Result"] == "N.A.", "Result"] <- 0





  ### Try to protect against getting MULTIPLE units in one data file or wrong-units vs expected...
  ###     IMPORTANT for doing fixup process for weight/time and for the TEXT we output explaining what the units are
  ### we should ASSUME that everything is in ng/WB (or at least that everything is COMMON FORMAT? )
  ###      for NOW make sure EVERY row says ng/WB and STOP if it does not!!!
  ###      <reset ExpectedUnits value when we get data w/ that new units but reset it in as specific a spot as possible for that test restult

  if (!all(testResults$Units == ExpectedUnits)) {
    ##### NOTENOTE NOTE NOTE_- The error catch below stops execution of the problem
    #
    stop(
      "MyExp DEBUG:  We are assuming data should all be in ",
      ExpectedUnits,
      " but that is not what we're seeing in the UNITS field ",
      " Current file name is = ",
      current_filename()
    )
  }

  if ("Flag" %in% colnames(testResults)) {
    # Test if we have a FLAG field.  We should for ALL NEW DATA
    # SO, we have a FLAG field and if flag=U then set Result to ZERO on that row,
    # if flag=J or flag is blank keep the row as is
    testResults <- testResults %>%
      mutate(Result = ifelse(Flag == "U", 0, Result)) %>%
      mutate(Result = ifelse(is.na(Flag), 0, Result)) %>%
      mutate(Flag = ifelse(is.na(Flag), "", Flag))

    # sum(is.na(testResults$Flag))
    #      testResults$Flag<-NULL   # was thinking of getting rid of FLAG field but it doesn't hurt anything here, delete later
    #
  } else {
    #### we SOMETIMES get data with no "Flag" field but instead the "J" and the "U" inside the numeric field.  If so, this code below runs...
    # Keep this version for processing OLD data
    # THE LINE BELOW let's me KEEP and include all J and assume J is accurate ENOUGH
    testResults$Result <- gsub("J", "", testResults$Result)

    ### If the results contain the letter 'U' then this is BELOW the limit of detection so IS NOT FOUND
    #         SO:  Set all those to ZERO
    testResults$Result[grepl("U", testResults$Result, fixed = TRUE)] <-
      0
  }

  # IN SOME DATA we get a blank field (NOT exactly a NULL, but rather a "") as a value... so if we get THAT we set any "" to ZERO
  # Saw this problem in Dartmouth 2nd group (25 bands)
  if ((nrow(testResults[testResults$Result == "", ]) > 0)) {
    testResults[testResults$Result == "", "Result"] <- 0
  }




  # Make sure there are NO non-numeric values in Result.  If there are.. STOP HERE
  if (suppressWarnings(any(is.na(as.numeric(
    as.character(testResults$Result)
  ))))) {
    stop(
      "Marc notes ERROR:  There remain non-numeric values in Result field",
      "  Current file name is = ",
      current_filename()
    )
  }
  # testResults[testResults$SampleNumber=="A170241",]$Result

  # NOW convert results to Numeric  THIS SHOULD NEVER  FAIL since we have a TEST ABOVE for non-numeric
  testResults$Result <-
    as.numeric(as.character(testResults$Result))

  # NOW if our GLOBAL FLAG is set to DROP ALL ZERO VALUES which means
  #   drop all Wristbands that had NO RESULTS of any kind (i.e. do NOT report on them at ALL)
  if (DropAllZeroSampleNumbers) {
    testResults <- testResults %>%
      group_by(SampleNumber) %>%
      filter(sum(Result) > 0) # KEEP only WB's that have SOME non-zero results
  }

  # MOVED THIS TO READ RAW:
  # IF any specific wristbands are to be DROPPED then they are set in the list in set_key_variables
  #   and then this code will eliminate those "to be dropped" wristbands
  # testResults <- testResults %>%
  #  filter(!SampleNumber %in% DropSpecificWristbands)  # Allow throuh in NOT in the dropped list

  # If any chemicals are set as "ignore/drop these chemicals for this run of the data"
  # Then reset testResult to ignore those chemicals.
  #  ALSO need to drop those chem from masterParameter
  testResults <- testResults %>%
    filter(!ParameterID %in% DropSpecificChemicals)

  # Sort Test Results by ParameterName
  testResults <- arrange(testResults, ParameterName)

  # Eliminate ParameterName Column.  Just use ParameterID as the key to get this
  testResults$ParameterName <- NULL

  ### TEST to see that masterParam has all the rows needed to link to FLAME list
  # length(masterParam[,"ParameterID"])
  # length(testResults[,"ParameterID"])
  # intersect(masterParam[,"ParameterID"], testResults[,"ParameterID"])
  # setdiff(masterParam[,"ParameterID"], testResults[,"ParameterID"])
  # setdiff( testResults[,"ParameterID"], masterParam[,"ParameterID"])
  # union(testResults[,"ParameterID"], masterParam[,"ParameterID"])

  # SELECT the columns we actually WANT

  testResults <-
    testResults[, c(
      "ParameterID",
      "PureSampleName",
      "SampleNumber",
      "Result",
      "Flag"
    )]

  # Add the ParameterName and the CASNumber to the results table
  testResults <-
    merge(testResults, masterParam[, c("ParameterID", "ParameterName", "CASNumber")],
      by =
        "ParameterID"
    )

  # Clean up unneeded variable
  # rm(list=c("resultsTable"))
  # rm(resultsTableName)

  # Eliminate DUPLICATE ROWS in test result where same wristband, same compound
  # NOTE THIS IS TERRIBLE... SHOULD NEVER HAPPEN IN THE INCOMING DATA!!!!!!!
  # This happened due to bad run at OSU but they fixed and re-delivered so.... I removed this "fix"
  # testResults <- testResults [!duplicated(testResults[c("SampleNumber","ParameterName")]),]

  #### MARC working 2/2/2020 to see if both sampleNumber and PureSample name have same count...
  ## THEY DO NOT have the same count... but who cares!!! we're NOT using pure sample name for anything (is that TRUE????)  We're using CUstomer_WB_ID
  # testResults %>%
  #   select(SampleNumber,PureSampleName) %>%
  #   unique() %>%
  #   group_by(PureSampleName) %>%
  #   filter(n()>1)
  #
  # testResults %>%
  #   select(SampleNumber,PureSampleName) %>%
  #   unique() %>%
  #   group_by(SampleNumber) %>%
  #   filter(n()>1)




  # 359  length(unique(testResults$SampleNumber))
  # 358  length(unique(testResults$PureSampleName))

  # 359  length(unique(testResults$SampleNumber))
  # 358  length(unique(testResults$PureSampleName))




  testResults
}



fixUpTestResults <- function(testResults, FixupFile) {
  # THIS RELIES on a stupid # of global variables defined in set-variables initial R file.  THis is not ideal.  What i shold do is refactor as follows:
  # Use a Strategy Pattern for Customer-Specific Fixups
  # If the main reason for so many variables is the different customer-specific fixes (like DartmouthFixup, UCSF2020Fixup, etc.),
  # consider refactoring to use a strategy pattern.
  # Create a set of "fixup" functions, each implementing the adjustments for a specific customer or for generic customer
  # Then, fixUpTestResults would simply select the appropriate function based on the customer:
  #   dartmouthFixup <- function(testResults, FixupFile) {
  #     # Dartmouth-specific fixup code here
  #   }
  #
  # ucsfFixup <- function(testResults, FixupFile) {
  #   # UCSF-specific fixup code here
  # }
  #
  # fixUpTestResults <- function(testResults, FixupFile, customer) {
  #   fixupFunction <- switch(customer,
  #                           "Dartmouth" = dartmouthFixup,
  #                           "UCSF" = ucsfFixup,
  #                           # Add other customers as needed
  #                           stop("Unknown customer"))
  #   fixupFunction(testResults, FixupFile)
  # }
  ## ALSO i'm overloading concept of FIXUP.  really i just need to declare which CUSTOMER, which TEST, and which Date-or-version-or-RUN and then follow that logic thru

  # HOW to fix up varies from customer to customer... at the TOP here we have the initial-stuff we need to do that is common to every customer
  ##   READ IN the FixUpResultsFile!  NOTE that we ONLY call this function if we are DOING a fixup so this is safe to assume we have a fixupfile set
  if (is.null(FixupFile)) {
    testResults$ResultOriginal <- testResults$Result
    return(testResults)
  }
  fixUpResults <-
    read.table(
      FixupFile,
      # USE a VARIABLE to decide what the FIXUP File looks like.
      sep = ",",
      header = TRUE,
      colClasses = "character" # Import all as character
      ,
      comment.char = "" # Don't allow use of "#" as comment in input
      ,
      quote = "\"",
      fileEncoding = "UTF-8-BOM" # THIS gets rid of the weird characters near the beginning of the first line.
    )

  # Lets make srue the FIXUP file has all the same things we need to DO the fixup....
  # We're going to merge the fixup file with the testResults file based on "FSES_ID" (aka "SampleNumber" )

  # 359  length(unique(testResults$SampleNumber))
  # 358  length(unique(testResults$PureSampleName))
  # 361  length(unique(fixUpResults$FSES_ID))

  # USing setdiff with parameters in the right order shows us if every test result is represented in the fixup file
  #   SO if the lenth of that setdiff > 0 then we MISSING SOME VALUES in the fixup file
  if (length(setdiff(
    unique(testResults$SampleNumber),
    unique(fixUpResults$FSES_ID)
  )) > 0) {
    ##### NOTENOTE NOTE NOTE_- The error catch below stops execution of the problem
    #
    stop("MyExp DEBUG:  Found missing lookup values in fixUpResult$FSES_ID ")
  }
  #
  # IF we're doing fixups then for AIR CONCENTRATION we need Days_worn
  #
  # SO i'm going to make sure we always have it so stop if we don't
  # and if we do, make it numeric
  if ("Days_worn" %in% colnames(fixUpResults)) {
    fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn)
  } else {
    stop(
      "Marc Error:  STOP now -- need Days_Worn in test results to do air concentration... why not here? ",
      "  Current file name is = ",
      current_filename()
    )
  }

  if (SBIR_P2_Part1_71_FixUp) {
    fixUpResults$week_factor <- as.numeric(fixUpResults$week_factor) # NOT using this for SBIR P2
    fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn) ### USE DaysWORN as same as "days_factor"
    fixUpResults$size_factor <- as.numeric(fixUpResults$size_factor)

    # Suppress warnings during numeric conversion
    fixUpResults$hours_worn <- suppressWarnings(as.numeric(fixUpResults$hours_worn))
    # Replace NA values with 24
    fixUpResults$hours_worn[is.na(fixUpResults$hours_worn)] <- 24


    ##### NOTENOTE NOTE NOTE_- The error catch below is GREAT
    #
    if (min(fixUpResults$week_factor) < 0.05) {
      stop(
        "MyExp DEBUG:  Found less that .05 weeks, that can't be right, min value found = ",
        min(fixUpResults$week_factor)
      )
    }

    ##### NOTENOTE NOTE NOTE_- The error catch below is GREAT
    #
    if (min(fixUpResults$size_factor) < 3.00) {
      stop(
        "MyExp DEBUG:  Found less that 3 grams in WB weight, that can't be right, min weight found = ",
        min(fixUpResults$size_factor)
      )
    }


    testResults2 <-
      merge(fixUpResults,
        testResults,
        by.x = "FSES_ID",
        by.y = "SampleNumber"
      )

    #  SET ResultOriginal to the un-weighted-un-modfied original value from lab
    #   WHICH could be ng/g or ng/WB depending.... that is confusing
    #   IF it is ng/g then I need to UPSCALE IT to what it WOULD HAVE BEEN
    #     if it were ng/WB
    #   set RESULT
    #
    testResults2$ResultOriginal <- testResults2$Result

    testResults2$Result <-
      testResults2$Result / testResults2$Days_worn
    testResults2$Result <-
      signif(testResults2$Result / testResults2$size_factor, 3)
    testResults2 <- testResults2 %>%
      rename(SampleNumber = FSES_ID) %>%
      rename(Customer_Batch_Number = Batch_Num) %>%
      rename(PureSampleName = PureSampleName.x) %>%
      rename(Lab_Submission_Batch = Customer_Batch_Number)
    testResults2 <- testResults2 %>%
      # rename(Start_Wearing = Start, End_Wearing = End) %>%   # Don't have start and end dates for DAY fixup
      # rename(Wristband_Size = size) %>%   # Don't have wristband_Size
      select(
        SampleNumber,
        PureSampleName,
        Days_worn,
        ParameterID,
        Result,
        ResultOriginal,
        ParameterName,
        CASNumber,
        Flag,
        MyE_Received,
        size_factor,
        week_factor,
        Lab_Submission_Batch
      )


    #### NOW because marc is confused on 2/2/2020 about PureSampleName becuase sometimes (Dartmouth) it is not Uniqe...I'll append Customer_WB_id to PureSampleName just to be totally safe...
    # But I don't think i ever really use PureSmpleName in any meaningful way so...
    #### MaRC commented this out since not needed for SBIR P2 I believe
    # testResults2$PureSampleName <-
    #   paste(testResults2$PureSampleName,
    #         testResults2$Customer_WB_id,
    #         sep = "_")


    testResults <- testResults2

    rm(testResults2, fixUpResults)
    testResults
  } else if (DartmouthFixup ||
    UCSF2020Fixup ||
    CombinedTestData) {
    # added UCSF2020 fixup to this... hopefully that works?  Uses same fixupfile format as dartmouth now
    ### this weird section is JUST for the    SET TO TRUE FOR DARTMOUTH FIXUP  (was also wisconsin but now separating to deal w/ new lookup tables)
    #           to nanograms per gram of wristband normalized for one week.
    #

    # Select only the columns I want from fixup file
    #     NOTE:  The other columns MIGHT BE USEFUL SOMEDAY to output lookup tables and such but that differs customer to customer
    #    fixUpResults <- fixUpResults %>%
    #      select(SampleNumber,week.factor,size.factor)

    fixUpResults$week_factor <- as.numeric(fixUpResults$week_factor)
    fixUpResults$size_factor <- as.numeric(fixUpResults$size_factor)


    ##### NOTENOTE NOTE NOTE_- The error catch below is GREAT
    #
    if (min(fixUpResults$week_factor) < 0.05) {
      stop(
        "MyExp DEBUG:  Found less that .05 weeks, that can't be right, min value found = ",
        min(fixUpResults$week_factor)
      )
    }

    ##### NOTENOTE NOTE NOTE_- The error catch below is GREAT
    #
    if (min(fixUpResults$size_factor) < 3.00) {
      stop(
        "MyExp DEBUG:  Found less that 3 grams in WB weight, that can't be right, min weight found = ",
        min(fixUpResults$size_factor)
      )
    }


    testResults2 <-
      merge(fixUpResults,
        testResults,
        by.x = "FSES_ID",
        by.y = "SampleNumber"
      )

    #  SET ResultOriginal to the un-weighted-un-modfied original value from lab
    #   WHICH could be ng/g or ng/WB depending.... that is confusing
    #   IF it is ng/g then I need to UPSCALE IT to what it WOULD HAVE BEEN
    #     if it were ng/WB
    #   set RESULT
    #
    testResults2$ResultOriginal <- testResults2$Result

    testResults2$Result <-
      testResults2$Result / testResults2$week_factor
    testResults2$Result <-
      signif(testResults2$Result / testResults2$size_factor, 3)
    testResults2 <- testResults2 %>%
      rename(SampleNumber = FSES_ID) %>%
      rename(Customer_Batch_Number = Batch_Num) %>%
      rename(Start_Wearing = Start, End_Wearing = End) %>%
      rename(Wristband_Size = size) %>%
      select(
        ParameterID,
        PureSampleName,
        Result,
        ResultOriginal,
        ParameterName,
        CASNumber,
        Flag,
        Customer_Batch_Number,
        Customer_WB_id,
        SampleNumber,
        MyE_Received,
        Start_Wearing,
        End_Wearing,
        Wristband_Size,
        Days_worn,
        size_factor,
        week_factor,
        Lab_Submission_Batch
      )


    #### NOW because marc is confused on 2/2/2020 about PureSampleName becuase sometimes (Dartmouth) it is not Uniqe...I'll append Customer_WB_id to PureSampleName just to be totally safe...
    # But I don't think i ever really use PureSmpleName in any meaningful way so...
    testResults2$PureSampleName <-
      paste(testResults2$PureSampleName,
        testResults2$Customer_WB_id,
        sep = "_"
      )


    testResults <- testResults2

    rm(testResults2, fixUpResults)
    testResults
  } else if (WisconsinFixup) {
    ### this weird section is JUST for the    SET TO TRUE FOR  Wisconsin
    fixUpResults <- fixUpResults %>%
      # select(FSES_ID,week_factor,size_factor,Days_worn,PartName,PrePost) %>%   # I used to subset everything now use full thing?
      rename(SampleNumber = FSES_ID)

    # Select only the columns I want from fixup file
    #     NOTE:  The other columns MIGHT BE USEFUL SOMEDAY to output lookup tables and such but that differs customer to customer
    #    fixUpResults <- fixUpResults %>%
    #      select(SampleNumber,week.factor,size.factor)
    testResults2 <-
      merge(fixUpResults,
        testResults,
        by.x = "SampleNumber",
        by.y = "SampleNumber"
      )

    #  SET ResultOriginal to the un-weighted-un-modfied original value from lab
    #   WHICH could be ng/g or ng/WB depending.... that is confusing
    #   IF it is ng/g then I need to UPSCALE IT to what it WOULD HAVE BEEN
    #     if it were ng/WB
    #   set RESULT
    #

    testResults2$ResultOriginal <- testResults2$Result


    fixUpResults$week_factor <- as.numeric(fixUpResults$week_factor)
    fixUpResults$size_factor <- as.numeric(fixUpResults$size_factor)

    testResults2 <-
      merge(fixUpResults, testResults, by = "SampleNumber") ## HERE is where I add important values...
    testResults2$ResultOriginal <-
      testResults2$Result # Save unmodified value
    testResults2$Result <-
      testResults2$Result / testResults2$week_factor

    # as;dflkasd;fk:  NEED TO  NEVER do SIZE adjustment if in NG/G already yes???

    # testResults2$Result <- signif(testResults2$Result / testResults2$size.factor, 3)   #DO NOT DO SIZE FIXUP cause already in ng/g starting w/ PO 211
    #### NEED TO FIGURE OUT when when when to upscale the original value
    #  if original value in ng/g then... for AirConcentration need to use ONE GRAM as size
    #   if original value in ng/WB then need to use actual WB size
    #     SO can do the OVERRIDE when... in AIR CONCENTRATOR CALCULATOR we do this calculation!!!
    # if (ExpectedUnits == "ng/g") {
    #   testResults2$ResultOriginal <- testResults2$ResultOriginal * week_factor
    # }


    # testResults2$TImeNormalFactor <- NULL
    # testResults2$WrisbandWeight <- NULL
    #
    #
    #
    #
    testResults2 <- testResults2 %>%
      # rename(SampleNumber=FSES_ID) %>%
      rename(Customer_Batch_Number = Batch_Num) %>%
      rename(Start_Wearing = Start, End_Wearing = End) %>%
      rename(Wristband_Size = size) %>%
      select(
        ParameterID,
        PureSampleName,
        Result,
        ResultOriginal,
        ParameterName,
        CASNumber,
        Flag,
        Customer_Batch_Number,
        SampleNumber,
        MyE_Received,
        Start_Wearing,
        End_Wearing,
        Wristband_Size,
        Days_worn,
        size_factor,
        week_factor,
        Lab_Submission_Batch,
        PrePost,
        PartName
      )




    testResults <- testResults2
    rm(testResults2, fixUpResults)
    testResults
  } else if (LorealFixup) {
    ### this weird section is JUST for the    SET TO TRUE FOR LOREAL
    #           FIRST DRS run for Dartmouth to fix values
    #           from nanograms per wristband for however long worn TO
    #           to nanograms per gram of wristband normalized for one DAY
    if ("Days_worn" %in% colnames(fixUpResults)) {
      fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn)
    }


    testResults2 <-
      merge(fixUpResults, testResults, by = "SampleNumber")

    ### CAREFUL:  This maybe messed EVERYTHING UP cause RESULT and OriginalResult need to be correct

    if ("Days_worn" %in% colnames(testResults2)) {
      testResults2$Result <-
        signif(testResults2$Result / testResults2$Days_Worn, 3)
    }

    # testResults2$TImeNormalFactor <- NULL
    testResults <- testResults2
    rm(testResults2, fixUpResults)
    testResults
  } else if (BuffaloFixup) {
    ### this weird section is JUST for the
    #       NOW just Buffalo as working on Dartmouth special formats above

    # Select only the columns I want from fixup file
    #     NOTE:  The other columns MIGHT BE USEFUL SOMEDAY to output lookup tables and such but that differs customer to customer

    fixUpResults <- fixUpResults %>%
      select(
        FSES_ID,
        week_factor,
        size_factor,
        PartName,
        PrePost,
        Days_worn
      ) %>%
      rename(SampleNumber = FSES_ID)

    fixUpResults$week_factor <- as.numeric(fixUpResults$week_factor)
    fixUpResults$size_factor <- as.numeric(fixUpResults$size_factor)
    if ("Days_worn" %in% colnames(fixUpResults)) {
      fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn)
    }


    testResults2 <-
      merge(fixUpResults, testResults, by = "SampleNumber")
    testResults2$Result <-
      testResults2$Result / testResults2$week_factor
    testResults2$Result <-
      signif(testResults2$Result / testResults2$size_factor, 3)
    # testResults2$TImeNormalFactor <- NULL
    # testResults2$WrisbandWeight <- NULL
    testResults <- testResults2
    rm(testResults2, fixUpResults)
    testResults
  } else if (UCSFplusRandom10Fixup) {
    ### this weird section is JUST for the
    #       NOW just UCSF

    # Select only the columns I want from fixup file
    #     NOTE:  The other columns MIGHT BE USEFUL SOMEDAY to output lookup tables and such but that differs customer to customer
    fixUpResults <- fixUpResults %>%
      select(FSES_ID, week_factor, size_factor, Days_worn) %>%
      rename(SampleNumber = FSES_ID)

    fixUpResults$week_factor <- as.numeric(fixUpResults$week_factor)
    fixUpResults$size_factor <- as.numeric(fixUpResults$size_factor)
    if ("Days_worn" %in% colnames(fixUpResults)) {
      fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn)
    }


    testResults2 <-
      merge(fixUpResults, testResults, by = "SampleNumber")
    testResults2$Result <-
      testResults2$Result / testResults2$week_factor
    testResults2$Result <-
      signif(testResults2$Result / testResults2$size_factor, 3)
    # testResults2$TImeNormalFactor <- NULL
    # testResults2$WrisbandWeight <- NULL
    testResults <- testResults2
    rm(testResults2, fixUpResults)
    testResults
  } else if (UniVisionFixup && (RMD_type == "PEST") && wristbands_time_adjusted_one_week_not_weight) { ## NOTE REALLY ONLY NEED TO TEST FOR  wristbands_time_adjusted_one_week_not_weight to make generic
    #### Worked for THIS IS to ONLY adjust for TIME and NOT for WEIGHT/SIzE of wristband
    fixUpResults <- fixUpResults %>%
      select(FSES_ID, week_factor, size_factor, Days_worn) %>% ### ADDED THIS LINE cause otherwise things broke
      rename(SampleNumber = FSES_ID)

    fixUpResults$week_factor <-
      as.numeric(fixUpResults$week_factor) # Adjust for week
    fixUpResults$size_factor <-
      as.numeric(fixUpResults$size_factor) # adjust for size
    if ("Days_worn" %in% colnames(fixUpResults)) {
      fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn)
    }
    testResults2 <-
      merge(fixUpResults, testResults, by = "SampleNumber")



    #  SET ResultOriginal to the un-weighted-un-modfied original value from lab
    #   WHICH could be ng/g or ng/WB depending.... that is confusing
    #   IF it is ng/g then I need to UPSCALE IT to what it WOULD HAVE BEEN
    #     if it were ng/WB
    #   set RESULT
    #
    testResults2$ResultOriginal <- testResults2$Result

    testResults2$Result <-
      testResults2$Result / testResults2$week_factor
    testResults2$Result <-
      # signif(testResults2$Result / testResults2$size_factor, 3)  ### THIS LINE is to adjust for SIZE FACTOR
      signif(testResults2$Result, 3) ### This line is to   NOT adjust for SIZE/WEIGHT of wristband
    testResults <- testResults2
    rm(testResults2, fixUpResults)
    testResults
  } else if (UNMFixup |
    COLORADOFixUp |
    ULILLEFRANCEFixup |
    UCONNFixUp |
    (CHICAGOFixUp &&
      RMD_type == "DRS") |
    (GEORGETOWNFixUp &&
      RMD_type == "DRS") |
    SBIR_P1_May2022Fixup |
    UC_DAVISFixup |
    BostonFixup |
    UFL_FloridaFixup |
    LouisvilleFixup |
    (UniVisionFixup &&
      RMD_type == "DRS")) {
    #### Worked for UNMFixup...trying to make it generic?
    ### THIS IS generic when doing FULL FIXUP meaning WEEK and SIZE
    # Select only the columns I want from fixup file
    #     NOTE:  The other columns MIGHT BE USEFUL SOMEDAY to output lookup tables and such but that differs customer to customer
    fixUpResults <- fixUpResults %>%
      # select(FSES_ID,week_factor,size_factor) %>%
      select(FSES_ID, week_factor, size_factor, Days_worn) %>% ### ADDED THIS LINE cause otherwise things broke
      rename(SampleNumber = FSES_ID)

    fixUpResults$week_factor <-
      as.numeric(fixUpResults$week_factor) # Adjust for week
    fixUpResults$size_factor <-
      as.numeric(fixUpResults$size_factor) # adjust for size
    if ("Days_worn" %in% colnames(fixUpResults)) {
      fixUpResults$Days_worn <- as.numeric(fixUpResults$Days_worn)
    }
    testResults2 <-
      merge(fixUpResults, testResults, by = "SampleNumber")



    #  SET ResultOriginal to the un-weighted-un-modfied original value from lab
    #   WHICH could be ng/g or ng/WB depending.... that is confusing
    #   IF it is ng/g then I need to UPSCALE IT to what it WOULD HAVE BEEN
    #     if it were ng/WB
    #   set RESULT
    #
    testResults2$ResultOriginal <- testResults2$Result

    testResults2$Result <-
      testResults2$Result / testResults2$week_factor
    testResults2$Result <-
      signif(testResults2$Result / testResults2$size_factor, 3)
    # testResults2$TImeNormalFactor <- NULL
    # testResults2$WrisbandWeight <- NULL
    testResults <- testResults2
    rm(testResults2, fixUpResults)
    testResults
  } else if (FixupForAnyone) {
    stop(
      "MyExp DEBUG:  We are trying to do a FIXUP but no fixup type is setup which seems WRONG,  SET IT ub data.load.functions ",
      " Current file name is = ",
      current_filename()
    )
  } else {
    # NO FIXUP NEEDED so just return testResults (SHOULD NEVER GET HERE????)
    testResults
  }
}

##### WE NOW want to add information, when available, about the AVERAGE AIR CONCENTRATION each person was exposed to....
### this is in BETA
###
addAirCalculationInformation <- function(tr, airConcentrationTable, cm3VolumeSiliconeOfOneGram, ExpectedUnits) {
  ### FIRST read in the LOOKUP table for air concentration
  # airConcentrationLookup table has ParameterID and BoilingPoint where BoilingPoint is from TEST unless it is from Opera AND has NOT_FOUND if neither
  airConcentrationLookup <-
    read.table(
      airConcentrationTable,
      # USE a VARIABLE to decide what the FIXUP File looks like.
      sep = ",",
      header = TRUE,
      colClasses = "character" # Import all as character
      ,
      comment.char = "" # Don't allow use of "#" as comment in input
      ,
      quote = "\"",
      fileEncoding = "UTF-8-BOM" # THIS gets rid of the weird characters near the beginning of the first line.
    )
  ### Then calculate each column of STevens spreadsheet
  # tr<-testResults
  ### THen JOIN the looku table to testResults
  tr <- tr %>% left_join(airConcentrationLookup, by = "ParameterID")

  ### My input data has a value of "NOT_FOUND" if there is no valid boiling point found so STOP if we hit that in restResults
  ### My input data has a value of "NOT_FOUND" if there is no valid boiling point found so STOP if we hit that in restResults
  if (any(tr$Test_Or_Opera == "NOT_FOUND")) {
    stop(
      "Marc Error:  STOP now -- We hit a compound in testResults for which we have no boiling point LOOKUP CSV FILE and add TEST-HACK",
      "  Current file name is = ",
      current_filename()
    )
  }


  # Make Numerica
  tr$BoilingPoint <-
    as.numeric(tr$BoilingPoint) ### NOTE that this USED TO creat NA's but I fixed that above by temp setting BP toooo HIGH

  # Make sure we're now in NG/WB and not in NG/G in terms of the results
  if (ExpectedUnits == "ng/g") {
    # HERE i need to just pretend the "size of the wristband" is 1 gram so the VOLUME of that will be 1 gram * "cubic centimeters / gram" ==> Cubic centimeters
    tr$volume_factor <-
      cm3VolumeSiliconeOfOneGram # Just use volumne of ONE GRAM as size of wristband cause data is ng/g
  } else if (ExpectedUnits == "ng/WB") {
    tr$volume_factor <-
      tr$size_factor * cm3VolumeSiliconeOfOneGram # so just MULTIPLY size factor (grams) by cmVolume (cubic centimeters per gram   )
  } else {
    stop(
      "Marc Error:  STOP now -- NO WAY are we ready for NON ng/g and NON ng/WB results YET YET YET",
      "  Current file name is = ",
      current_filename()
    )
  }

  # The data in RESULT contains "size_factor" but this is the NANOGRAMS of SILICONE found in the full wristband
  # We need to make sure that we are NOT using nanograms of silicone in wristband but VOLUME of silicone in wristband BUT we don't track that
  # SO we figure out that the VOLUME in cubic centimeters of SILICONE in ONE GRAM of silicone is .8936 and the
  #       (and similarly, not used here, the WEIGHT in grams of one cubic centimeter of silicone is 1.1191)
  ## SO:  Marc added volume_factor as a new variable which should be used


  #  tr<-testResults
  tr <- tr %>%
    mutate(
      Mixed_log_Ksa_BP = case_when(
        Test_Or_Opera == "TEST" ~
          (0.02 * BoilingPoint) + 0.527,
        Test_Or_Opera == "TEST-HACK" ~
          (0.02 * BoilingPoint) + 0.527,

        # This is column (was U) "Single Best Model Ksa
        Test_Or_Opera == "OPERA" ~
          (0.019 * BoilingPoint) + 0.829,
        # This is column (is T) "Second Best Model Ksa
        TRUE ~ NA_real_
      )
    )
  if (!is.numeric(tr$Mixed_log_Ksa_BP)) {
    stop("MyExp DEBUG:  Non Numeric Mixed_log_KSA_BP, that can't be right")
  }


  # USING THE TEST formula
  #  tr$Mixed_log_Ksa_BP = (0.0202*testResults$BoilingPoint)+0.527  # This is column (was U) "Single Best Model Ksa
  # USING THE OPERA FORUMLA
  # =(0.0202*I5)+0.591
  # tr$Mixed_log_Ksa_BP = (0.0202*testResults$BoilingPoint)+0.591  # This is column (is T) "Second Best Model Ksa

  # IFERROR(($O5/($M5*(10^U5)*(1-EXP(-(($AA5*1000)*$N5)/($M5*(10^U5))))))*1000,"")
  # ($O5/   ($M5*   (10^U5) * (1-EXP(-(($AA5*1000)*$N5)/($M5*(10^U5)))))         )        *       1000
  # SAME AS:
  # ($O5*1000 )    /   ($M5*   (10^U5) * (1-EXP(-(($AA5*1000)*$N5)/($M5*(10^U5)))))
  #
  # U5 is Mixed_log_Ksa_BP, AA5 is (M5/1000)*(10^U5)*(10^Z5) and is called  "Rs (L/day), N5 is Days_Worn, M5 is Volume(again), and U5 is MixedLog_KSA(again)
  # O5 is ResultOriginal
  # M5 is volume_factor
  # N5 is Days_worn



  # =====================================================================OLD???#
  # START OVER:  STUFF way below is too confusing.  We need to create AK which means we need to create AE
  # AE = (($O5*1000       /          (    $M5*(10^U5) *   (1-      EXP        (  (-(($AA5*1000)*$N5))   /        ($M5*(10^U5))        )           ))
  # AE    (   AE1         /                   AE2     *   (1-      2.71828 ^  (        AE3                 /               AE2        )           ))
  ##
  # Which means we need to decode AA
  ## which means we need to decode Z5 and Z5 is =(-0.0116*H5)+1.96
  # tr$log_Ke_BP <- (-0.0116*tr$BoilingPoint)+1.96   ## THIS IS CORRECT

  #### NOTE:  I'm using TEST-HACK to be the boiling point values I found manually and not through automated process
  tr <- tr %>%
    mutate(
      log_Ke_BP = case_when(
        Test_Or_Opera == "TEST" ~
          (-0.012 * BoilingPoint) + 2.04,
        Test_Or_Opera == "TEST-HACK" ~
          (-0.012 * BoilingPoint) + 2.04,
        Test_Or_Opera == "OPERA" ~
          (-0.009 * BoilingPoint) + 1.55,
        TRUE ~ NA_real_
      )
    )


  # and AA is therefore   #  =(M5/1000)*(10^U5)*(10^Z5)
  tr$Rs_in_L_per_Day <-
    (tr$volume_factor / 1000) * (10^tr$Mixed_log_Ksa_BP) * (10^tr$log_Ke_BP) #   AA5   ---   (M5/1000)*(10^U5)*(10^Z5)


  # AE1 = $O5*1000
  # tr$AE1 <- tr$ResultOriginal*1000
  # AE2 =  $M5*(10^U5)
  # tr$AE2 <- tr$volume_factor*(10^tr$Mixed_log_Ksa_BP)
  # AE3 =   (-(($AA5* 1000)*$N5         ))
  # tr$AE3 <- (-((tr$AA*1000)*tr$Days_worn))

  # ($O12/($M12*(10^U12)*(1-EXP(-(($AA12*1000)*$N12)/($M12*(10^U12))))))*1000
  #         ($O12*1000    /               ($M12*(10^U12)  *   ( 1-     EXP(-(($AA12*1000)*$N12)/($M12*(10^U12)))     )))
  #     (   AE1         /                   AE2       *   ( 1-     2.71828 ^  - (        AE3                 /               AE2        )           ))
  # tr$AE <- ( tr$AE1     /                 (    tr$AE2     *   ( 1-     exp(  - (        tr$AE3              /               tr$AE2     )           ))   ))

  ########### NEW TRY BELOW:
  # $O5/($M5*(10^U5)*(1-EXP(-(($AA5*1000)*$N5)/($M5*(10^U5))))))*1000


  # TRYING TO SEE WHY THERE IS AN ERROR...
  # holdValue<- (1-exp(-(tr$Rs_in_L_per_Day*1000*tr$Days_worn)/(tr$volume_factor*(10^tr$Mixed_log_Ksa_BP))   ))


  tr$Ca_for_Customer <- tr$ResultOriginal / (tr$volume_factor * (10^tr$Mixed_log_Ksa_BP) * (1 - exp(
    -(tr$Rs_in_L_per_Day * 1000 * tr$Days_worn) /
      (tr$volume_factor * (10^tr$Mixed_log_Ksa_BP))
  ))) * 1000
  # BELOW is same as above, i just tried differnt parenthes () to see if that changed anything
  # tr$Ca_for_Customer2 <- (tr$ResultOriginal / (tr$volume_factor * (10^tr$Mixed_log_Ksa_BP) * (1-exp(-((tr$Rs_in_L_per_Day*1000)*tr$Days_worn)/  ( tr$volume_factor*(10^tr$Mixed_log_Ksa_BP))   ))))*1000


  ## WE NEED TO DO SOMETHING to fix the WEIRD NOT FOUND problem but... what exactly
  ## Don't need to do anything at moment.. .just let NA happen in the data and ... so what for now.




  #### Then clean up and add/delete columns


  ###


  ### then make sure we recover from any weird errors

  tr
}


##### WE NOW want to READ IN the NIOSH/OSHA limits using ParameterID to join
### this is in BETA
###
addAirNioshOsha <- function(testResults, airNioshOshaTable) {
  ###  NEXT read in the LOOKUP table for air concentration with NIOSH OSHA info...
  # airNioshOshaLookup table has ParameterID and BoilingPoint where BoilingPoint is from TEST unless it is from Opera AND has NOT_FOUND if neither
  airNioshOshaLookup <-
    read.table(
      airNioshOshaTable,
      # USE a VARIABLE to decide what the FIXUP File looks like.
      sep = ",",
      header = TRUE,
      colClasses = "character" # Import all as character
      ,
      comment.char = "" # Don't allow use of "#" as comment in input
      ,
      quote = "\"",
      fileEncoding = "UTF-8-BOM" # THIS gets rid of the weird characters near the beginning of the first line.
    )

  ### THen JOIN the looku table to testResults
  testResults <-
    testResults %>% left_join(airNioshOshaLookup, by = "ParameterID")
  ### My input data has a value of "NOT_FOUND" if there is no valid boiling point found so STOP if we hit that in restResults
  if (any(testResults$Test_Or_Opera == "NOT_FOUND")) {
    stop(
      "Marc Error:  STOP now -- Hit some error in reading AirNioshOsha Not defined yet ",
      "  Current file name is = ",
      current_filename()
    )
  }

  ###     TEMPorary idea... we COULD instead search and find boiling point, Estimate boiling point, USE other calculation method OR just report CAN NOT FIND AIR CONCENTRATION FOR THIS COMPOUND
  if (FALSE) {
    stop(
      "Marc Error:  STOP now -- Hit some error in reading AirNioshOsha Not defined yet ",
      "  Current file name is = ",
      current_filename()
    )
  }

  # Make Numerica
  testResults$OSHA <-
    as.numeric(testResults$OSHA) ### NOTE that this creates NA what to do
  testResults$NIOSH <-
    as.numeric(testResults$NIOSH) ### NOTE that this creates NA what to do


  testResults
}



makeIntoDemoDataResults <-
  function(testResults, howManyDemoResults) {
    # Pick only top X results...
    # howManyDemoResults<-20  # THIS IS SET UP in the calling function
    allSampleNumbers <- testResults %>%
      select(SampleNumber) %>%
      unique()
    if (count(allSampleNumbers) > howManyDemoResults) {
      # topSampleNumbers<-top_n(allSampleNumbers,howManyDemoResults)   #OLDER VERSION
      topSampleNumbers <-
        slice_head(allSampleNumbers, n = howManyDemoResults) ### MAYBE newer version
      testResults <-
        pickSubsetOfResults(testResults = testResults, SampleNumbers = topSampleNumbers)
    }

    # if countOfUniqeSampleNmaes>X make table of unique names, take top X, join back to only select left join)

    # Generate X unique IDs, Transform all IDs into Unique IDS
    testResults
  }

# FILTER out all but selected sampleNumbers
pickSubsetOfResults <- function(testResults, SampleNumbers) {
  testResults <-
    filter(testResults, SampleNumber %in% SampleNumbers$SampleNumber)
}

# FILTER out all but selected batchNumber
# batchNumber<-c("5")
onlyPickSomeBatchesFromBiggerData <-
  function(testResults, batchNumbers) {
    testResults <-
      testResults %>% filter(Lab_Submission_Batch %in% batchNumbers)
  }


#
# READ in classificaiton of chemicals by RISK per california prop 65
# Read in california risk database
load.riskCalifProp65 <-
  function(riskCalifProp65TableName, masterParam) {
    riskCalifProp65 <- read.table(
      riskCalifProp65TableName,
      sep = ",",
      header = TRUE,
      colClasses = "character" # Import all as character
      ,
      comment.char = "" # Don't allow use of "#" as comment in input
      ,
      quote = "\"",
      fileEncoding = "UTF-8-BOM"
    )
    # Rename column to standard
    # Don't need to rename, name is correct already
    names(riskCalifProp65)[names(riskCalifProp65) == "masterParameterID"] <-
      "ParameterID"

    # Eliminate rows where not in the master paramater table
    # inner join - use MERGE on data table
    riskCxx <-
      merge(riskCalifProp65, masterParam, by = "ParameterID")
    # Now I have a list of all the prop 65 risks BUT some masterparam have duplicate rows.  380 unique ParameterIDs but 635 rows
    # length(riskCxx2$ParameterID)
    # length(unique(riskCxx2$ParameterID))
    # Drop all columns except 2 I care about
    riskCxx <- riskCxx[, c("ParameterID", "toxicityType")]

    # Eliminate all rows where NO RISK
    riskCxx <- riskCxx[!riskCxx$toxicityType == "NULL", ]

    # Collapse all values of Toxicity type so only one row for each parameterID
    if (nrow(riskCxx) > 0) {
      riskCxx <-
        aggregate(toxicityType ~ ParameterID,
          data = riskCxx,
          paste,
          collapse = " & "
        ) #  THIS USED TO BE &&  but I'm changing it to & and seeing if things break

      # Eliminate all rows where NO RISK
      # riskCalifProp65<-riskCalifProp65[!riskCalifProp65$toxicityType=="NULL",]
      # riskCalifProp65<-riskCalifProp65[,c("ParameterID","toxicityType")]

      # riskCalifProp65
    }


    riskCxx
  }
#
# READ in EPA IRIS chemical classification
# epaIrisTableName<-"./data/EDF_Phase1_EPA_Iris.csv"
# epaIrisTableName <- "./data/MASV15_epa_iris_risk.csv"
load.epaIris <- function(epaIrisTableName) {
  epaIris <- read.table(
    epaIrisTableName,
    sep = ",",
    header = TRUE,
    colClasses = "character" # Import all as character
    ,
    comment.char = "" # Don't allow use of "#" as comment in input
    ,
    quote = "\"",
    fileEncoding = "UTF-8-BOM"
  )
  # Eliminate all rows where IRIS ID
  # This was for OLD VERSION of this database
  # epaIris<-epaIris[!epaIris$Iris_ID=="NULL",]
  # Rename column
  # epaIris<-rename(epaIris,c("masterParameterID"="ParameterID"))
  epaIris <-
    plyr:::rename(epaIris, c("ChemicalUrl" = "IRIS_Summary"))
  # eliminste all columns except the three we want   OLD VERSION HAD EXTRA COLUMN
  # epaIris<-epaIris[ , c("ParameterID" , "IRIS_Summary"	,"Iris_ID")]
  # eliminste all columns except the TWO we want
  epaIris <- epaIris[, c("ParameterID", "IRIS_Summary")]

  epaIris
}

# READ in classificaiton of chemicals by RISK per IARC
# Read in IARC risk database
# IARCRiskTableName<-"./data/EDF_Phase1_Risk_WHO.csv"
# IARCRiskTableName<-"./data/MASV15_who_iarc_risk.csv"

load.IARCRisk <- function(IARCRiskTableName, riskIARCdecodeTableName) {
  # cat("000 in data load...\n", file = "debug_log.txt", append = TRUE)
  IARCRisk <- read.table(
    IARCRiskTableName,
    sep = ",",
    header = TRUE,
    colClasses = "character" # Import all as character
    ,
    comment.char = "" # Don't allow use of "#" as comment in input
    ,
    quote = "\"",
    fileEncoding = "UTF-8-BOM"
  )
  # cat("001 in data load...\n", file = "debug_log.txt", append = TRUE)

  # NOTE that we see some DUPLICATION
  # length(IARCRisk$masterParameterID)
  # length(unique(IARCRisk$masterParameterID))
  # Rename masterParameterID to ParameterID
  IARCRisk <- IARCRisk[, c("ParameterID", "IARCgroup")]
  colnames(IARCRisk)[colnames(IARCRisk) == "masterParameterID"] <-
    "ParameterID"
  # Make sure we have only the values 1, 2a, 2b, 3, and 4)
  # unique(IARCRisk$IARCgroup)

  IARCRisk <- merge(IARCRisk, masterParam, by = "ParameterID")
  # Eliminate any that are NULL but there are none at moment
  IARCRisk <- IARCRisk[!IARCRisk$IARCgroup == "NULL", ]

  # cat("111 in data load...\n", file = "debug_log.txt", append = TRUE)


  riskIARCdecode <- read.table(
    riskIARCdecodeTableName,
    sep = ",",
    header = TRUE,
    colClasses = "character" # Import all as character
    ,
    comment.char = "" # Don't allow use of "#" as comment in input
    ,
    quote = "\"",
    fileEncoding = "UTF-8-BOM"
  )
  # Clean up unneeded variable
  rm(IARCRiskTableName)

  IARCRisk <- IARCRisk[!IARCRisk$IARCgroup == "NULL", ]

  # New way to do this and eliminate SQLDF
  IARCRisk <-
    IARCRisk %>% inner_join(riskIARCdecode, by = c("IARCgroup" = "code"))

  # IARCRiskx <- sqldf(
  #   "select o.*, r.decode
  #   from IARCRisk as o inner join
  #   riskIARCdecode r on o.IARCgroup=r.code
  #   "
  # )
  # RENAME the columns
  IARCRisk <-
    plyr:::rename(IARCRisk, c("decode" = "IARC_Classification"))

  # Drop extra columns
  IARCRisk <-
    IARCRisk[, c("ParameterID", "IARCgroup", "IARC_Classification")]

  IARCRisk
}




#### HERE I have loaded all the results and classifications.
# testResults + classifications gives me the basic groups and is sorta the master results
# IARCRisk, epaIris, riskCalifProp65 are the RISK databases
# masterParam is maps ParameterID to ParameterName and CASNumber


#### NOW START MESSING WITH DETAILED INFO ON CHEMICALS
#
#  THIS IS OLDER WORKING ATTEMPT>.. about to make new attempt as well.
#

# load.chemSourceMitigation <- function(chemSourceMitigationInfoTableName) {
#   chemSourceMitigation <- read.table(
#     chemSourceMitigationInfoTableName,
#     sep = ",",
#     header = TRUE,
#     colClasses = "character" # Import all as character
#     ,
#     comment.char = "" # Don't allow use of "#" as comment in input
#     ,
#     quote = "\"",
#     fileEncoding = "UTF-8-BOM"
#   )
#   chemSourceMitigation <- chemSourceMitigation %>%
#     select(Chemical_Name,Summary_of_Health_Effects,Commercial_Products,Mitigation_Strategies)
#
#   # We would really like to capitalize the FIRST letter of each chemical (skipping numbers/spaces/etc)
#   # So we define a FUNCTION which takes any string and uppercases the first letter
#   uppercaseFirst <- function(txt) {
#     pos <- regexpr("[A-z]", txt)
#     char1 <- substr(txt, pos, pos)
#     uC <- toupper(char1)
#     sub("[A-z]", uC, txt)
#   }
#   # THEN we apply taht new function to the ParameterName mcolumn
#   chemSourceMitigation[, "Chemical_Name"] <-
#     sapply(chemSourceMitigation[, "Chemical_Name"], uppercaseFirst)
#
#   chemSourceMitigation
# }



#### New attempt 10/18/2024 using new table xlsx chemSourceMitigationInfoTableName2
# library(readxl)

load.chemSourceMitigation2 <- function(chemSourceMitigationInfoTableName, chemSourceSheetName) {
  chemSourceMitigation <- read_excel(chemSourceMitigationInfoTableName,
    sheet = chemSourceSheetName
  )

  chemSourceMitigation <- chemSourceMitigation %>%
    select(Chemical_Name, Summary_of_Health_Effects, Sources_of_Exposure, Mitigation_Strategies, WIKIPEDIA_ARTICLE_URL)


  # We would really like to capitalize the FIRST letter of each chemical (skipping numbers/spaces/etc)
  # So we define a FUNCTION which takes any string and uppercases the first letter
  uppercaseFirst <- function(txt) {
    pos <- regexpr("[A-z]", txt)
    char1 <- substr(txt, pos, pos)
    uC <- toupper(char1)
    sub("[A-z]", uC, txt)
  }
  # THEN we apply taht new function to the ParameterName mcolumn
  chemSourceMitigation[, "Chemical_Name"]$Chemical_Name <- sapply(chemSourceMitigation[, "Chemical_Name"]$Chemical_Name, uppercaseFirst)

  chemSourceMitigation
}




# READ in classificaiton of chemicals by RISK per IARC
# Read in IARC risk database
# IARCRiskTableName<-"./data/EDF_Phase1_Risk_WHO.csv"
# IARCRiskTableName<-"./data/MASV15_who_iarc_risk.csv"
