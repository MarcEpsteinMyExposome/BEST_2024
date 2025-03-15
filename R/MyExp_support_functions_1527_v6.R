# THIS SOURCE file will contain support functions
# The first one I'm adding builds the "mesg.v" list
#
# NEVER ADDED THIS ONE: the SECOND one i'm adding is the one to PRINT CUSTOMER OUTPUT as CSVs for large data output


# ## USES:  testResults, subject, SubClassScores, results_W
# just test


#' Build Message Vector for Individual Analysis
#'
#' Creates a vector of messages highlighting notable findings about a specific subject.
#'
#' @param testResults Data frame. The test results data.
#' @param mesg.v Character vector. Initial message vector to append to.
#' @param subject Character. The subject ID to analyze.
#' @param SubClassScores Data frame. Classification scores for each subject.
#' @param results_W Data frame. Wide-format test results.
#' @param numEPAirisFound Integer. Number of EPA IRIS database matches found.
#' @param numCalProp65Found Integer. Number of California Prop 65 database matches found.
#' @param numIARCRiskFound Integer. Number of IARC risk database matches found.
#' @param howManyWristbandsTested Integer. Total number of wristbands tested.
#' @param HideClassificationInformation Logical. Whether to hide classification information.
#' @param generateTabLink Function. Function to generate a tab link for a chemical name.
#'
#' @return A character vector of messages.
#'
buildMesgVIndividual <-
  function(testResults,
           mesg.v,
           subject,
           SubClassScores,
           results_W,
           numEPAirisFound,
           numCalProp65Found,
           numIARCRiskFound,
           howManyWristbandsTested,
           HideClassificationInformation,
           generateTabLink) {
    testResultsNonZero <- testResults[testResults$Result > 0, ]

    # Group by SampleNumber to allow easy finding of MIN and MAX values of COUNT of how many chem found per wristband
    countTestResults <- testResultsNonZero %>%
      select(SampleNumber, ParameterID) %>%
      group_by(SampleNumber) %>%
      mutate(CountChem = n()) %>%
      select(SampleNumber, CountChem) %>%
      distinct()

    if (numCalProp65Found == 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          numCalProp65Found,
          numCalProp65Found,
          "of your detected chemicals is listed in the **California Prop 65** toxicity dataset."
        )
    } else if (numCalProp65Found > 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          numCalProp65Found,
          numCalProp65Found,
          "of your detected chemicals are listed in the **California Prop 65** toxicity dataset."
        )
    }

    #  ADD TO MESG if this subject had the maximum # of chemical detected AND there were 2 or more wristbands tested
    if ((howManyWristbandsTested > 1) && (nrow(testResultsNonZero[testResultsNonZero$SampleNumber == subject, ]) == max(countTestResults$CountChem))) {
      txt <-
        "Your wristband had the maximum number of compounds detected in any of the wristbands."
      mesg.v <- cbind(mesg.v, txt)
    }

    if (howManyWristbandsTested > 1) {
      # place holder
      rw5 <- results_W

      # change everything > 0 to 1 for every column
      rw5 <- rw5 %>%
        mutate_if(is.numeric, ~ ifelse(. > 0, 1, 0)) %>%
        mutate_if(is.numeric, ~ replace(., is.na(.), 0))

      # Get the parameter names for rows where the sum is 1 (only one wristband has this compound)
      # and the subject's value is 1 (this subject has the compound)
      chems <- rw5$ParameterName[rowSums(rw5[, setdiff(names(rw5), "ParameterName")]) == 1 & rw5[[subject]] == 1]

      # Loop through all chems and assemble message
      for (chem in chems) {
        txt <-
          paste(
            chem,
            "was detected only on your wristband."
          )
        mesg.v <- cbind(mesg.v, txt)
      }
      rm(chems) # Clean Up
    }

    ### NOW let's see if YOUR BAND had the MAXIUM amount of a chemical which 2 or more people also had
    # and then let's see if that MAXIMUM is 5x greater or more than anyone else
    PrintMesgAboutSignificantlyGreaterAmountFound <-
      TRUE ## or FALSE to suppress
    if (howManyWristbandsTested < 3) {
      PrintMesgAboutSignificantlyGreaterAmountFound <- FALSE
    } # IF 2 or 1 wristband supress message
    if (PrintMesgAboutSignificantlyGreaterAmountFound) {
      # Could also change to "how many times greater" reporting
      multiplierGreater <-
        5 #  ONLY flag people whose value is 5 times greater than the next largest #

      # So... we need to find out WHAT rows as 2 or more values, where our subject was the MAX value
      #   AND where our subject's MAX value is "5 times" (or whatever we set) or more greater than next largest
      #

      # Let's find the rows of r_wide with 2 or more values>0: (thats all rows where 2 or more people had values )
      h1 <-
        results_W[rowSums(results_W > 0) > 1, ] # all rows with 2 or more values
      h2 <-
        h1[h1[, subject] == apply(h1, 1, max), ] # all the rows where "subject" has the max value of that ROW
      if (nrow(h2) > 0) {
        h3 <-
          h2[, !names(h2) %in% c(subject), drop = FALSE] # dataframe WITHOUT the column SUBJECT (drop says keey as dataframe)
        maxWithoutSubject <-
          apply(h3, 1, max) # This is the max value in the dataframe WITHOUT our main subject

        # GET rid of H4, don't need that variable.
        # h4<-h1[multiplierGreater*maxWithoutSubject < h2[,subject],] #These are all the rows where our subject is max, and is "5" times greater than next largeset
        # h4<-h2[multiplierGreater*maxWithoutSubject < h2[,subject],] #These are all the rows where our subject is max, and is "5" times greater than next largeset
        h2$subjectDividedByMaxWitoutSubject <-
          round(h2[, subject] / maxWithoutSubject)
        # NOW we can LOOP through all the rows of H4 or H2 and add to mesg.v
        # if (nrow(h4)>0) {
        if (nrow(h2) > 0) {
          for (i in 1:nrow(h2)) {
            ## FOR EVERY ROW that meets our criteria of being MAX for our subject, 2 or more readings, max is 5x next
            row <- h2[i, ] # GRAB the i'th ROW
            if (row$subjectDividedByMaxWitoutSubject >= multiplierGreater) {
              rname <- row$ParameterName

              txt <-
                paste(
                  "Your wristband had",
                  row$subjectDividedByMaxWitoutSubject,
                  "times more",
                  generateTabLink(rname),
                  "than any other tested wristband.",
                  sep = " "
                )
              mesg.v <- cbind(mesg.v, txt)
            }
          }
        }
      }
    }
    if (!HideClassificationInformation && (howManyWristbandsTested > 1)) {
      # FIND all unique classifications...
      # FIRST find all the classifications that ONLY ONE SUBJECT (any sub) found
      # BUT ignore this if there is ONLY ONE WRISTBAND IN SET
      uniqueClass <- as.data.frame(
        SubClassScores %>%
          group_by(classification) %>%
          summarise(n = dplyr::n()) %>%
          filter(n == 1) %>%
          select(classification)
      )

      uniqueClassOurSubject <- as.data.frame(
        SubClassScores %>%
          filter(classification %in% uniqueClass$classification) %>%
          filter(SampleNumber == subject)
      )

      if (nrow(uniqueClassOurSubject) > 0) {
        for (i in 1:nrow(uniqueClassOurSubject)) {
          ## FOR EVERY ROW in the list of all CLASSIFICATIONS
          # i<- 1
          row <- uniqueClassOurSubject[i, ] # GRAB the i'th ROW

          txt <-
            paste(
              "Your Wristband was the only wristband to have any",
              as.character(row[1, 1]),
              "compounds detected.",
              sep = " "
            )
          mesg.v <- cbind(mesg.v, txt)
        }
      }
    }
    mesg.v
  }




#' Build Message Vector for Group Analysis
#'
#' Creates a vector of messages highlighting notable findings about a group of subjects.
#'
#' @param testResults Data frame. The test results data.
#' @param mesg.v Character vector. Initial message vector to append to.
#' @param SubClassScores Data frame. Classification scores for each subject.
#' @param results_W Data frame. Wide-format test results.
#' @param countEPAiristHits Integer. Number of EPA IRIS database matches found.
#' @param countCalifProp65Hits Integer. Number of California Prop 65 database matches found.
#' @param countIARCHits Integer. Number of IARC risk database matches found.
#' @param HideClassificationInformation Logical. Whether to hide classification information.
#' @param classificationTextStrings List. Named list of text strings for each classification category.
#' @param generateTabLink Function. Function to generate a tab link for a chemical name.
#'
#' @return A character vector of messages.
#'
buildMesgVGroup <-
  function(testResults,
           mesg.v,
           SubClassScores,
           results_W,
           countEPAiristHits,
           countCalifProp65Hits,
           countIARCHits,
           HideClassificationInformation,
           classificationTextStrings,
           generateTabLink) {
    testResultsNonZero <- testResults[testResults$Result > 0, ]

    # Extract classification text strings from the passed parameter
    PAH_text_string <- classificationTextStrings$PAH
    flameRetardant_text_string <- classificationTextStrings$flameRetardant
    PCB_text_string <- classificationTextStrings$PCB
    pharmacological__text_string <- classificationTextStrings$pharmacological
    personalCare_text_string <- classificationTextStrings$personalCare
    industrial_text_string <- classificationTextStrings$industrial
    pest_text_string <- classificationTextStrings$pest
    consumerProduct_text_string <- classificationTextStrings$consumerProduct
    dioxinsAndFurans_text_string <- classificationTextStrings$dioxinsAndFurans

    # Group by SampleNumber to allow easy finding of MIN and MAX values of COUNT of how many chem found per wristband
    countTestResults <- testResultsNonZero %>%
      select(SampleNumber, ParameterID) %>%
      group_by(SampleNumber) %>%
      mutate(CountChem = n()) %>%
      select(SampleNumber, CountChem) %>%
      distinct()

    # place holder
    rw5 <- results_W

    # change everything > 0 to 1  for every column
    rw5 <- rw5 %>%
      mutate(across(where(is.numeric), ~ ifelse(. > 0, 1, 0))) %>%
      mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

    ## THE NEXT THREE "IF's" add messages indicating the TOTAL found elements across all the wristbands
    #     in the relevent databases

    if (countEPAiristHits == 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          countEPAiristHits,
          countEPAiristHits,
          "of the chemicals in this study group is listed in the **EPA IRIS** dataset."
        )
    } else if (countEPAiristHits > 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          countEPAiristHits,
          countEPAiristHits,
          "of the chemicals in this study group are listed in the **EPA IRIS** dataset."
        )
    }

    if (countCalifProp65Hits == 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          countCalifProp65Hits,
          countCalifProp65Hits,
          "of the chemicals in this study group is listed in the **California Prop 65** dataset."
        )
    } else if (countCalifProp65Hits > 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          countCalifProp65Hits,
          countCalifProp65Hits,
          "of the chemicals in this study group are listed in the **California Prop 65** dataset."
        )
    }

    if (countIARCHits == 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          countIARCHits,
          countIARCHits,
          "of the chemicals in this study group is listed in the **IARC** Cancer Report."
        )
    } else if (countIARCHits > 1) {
      mesg.v <-
        addMesg(
          mesg.v,
          countIARCHits,
          countIARCHits,
          "of the chemicals in this study group are listed in the **IARC** Cancer Report."
        )
    }

    ### PRINT message for every chemical detected on EVERY wristband...

    # How many total subjects can be calculated as the # of columns in r_wide
    howManySubjects <- ncol(results_W) ####
    if (howManySubjects > 2) { # only add these messages if 3 or more subjecs
      # LIST every chemical that was in EVERY wristband
      # Get names of all chems found on every wristband
      # Only those chemicals whos ROW SUMS = total number of subject have every wristband with some value
      if (howManySubjects == 1) {
        chems <- rw5$ParameterName  # Replace rownames with explicit column
      } else {
        chems <- rw5 %>%
          filter(rowSums(select(., -ParameterName)) == howManySubjects) %>%
          pull(ParameterName)
      }
      # THIS below does the same thing as line above WITHOUT using "rw5"
      # rownames(r_wide[apply(r_wide>0,1,all),])
      rm(rw5) # Clean Up

      # Loop through all chems and assemble message
      for (chem in chems) {
        txt <-
          paste(generateTabLink(chem), "was detected on every wristband.")
        #    txt<-paste(bullet,txt)

        mesg.v <- cbind(mesg.v, txt)
      }
      rm(chems) # Clean Up
    }

    if (!HideClassificationInformation) {
      #
      # STOP if one of the wristbands had NO data associated with it
      # HUH!!! THIS IS WRONG.  Do NOT need to catch this case Marc worried about nothing.. is OK if none have
      # in which case if every OTHER wristband DID have data you woudl INCORRECTLY print the "every wristband had..." message
      # if (length(unique(testResults$SampleNumber)) != length(unique(testResultsNonZero$SampleNumber))) {
      ### IF we are sure there were no wristbands with ZERO chemicals
      ## ADD XX% of wristbands detected at least one Flame Retardant
      howManyFlame <-
        sum(SubClassScores$classification == flameRetardant_text_string)
      if (howManyFlame > 0) {
        howManyFlame <- percent(round(howManyFlame / howManySubjects, 2))
        txt <-
          paste(
            howManyFlame,
            "of all wristbands had at least one Flame Retardant."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }
      ## ADD SX% of wristbands detected at least one pesticide
      howManyPest <- sum(SubClassScores$classification == pest_text_string)
      if (howManyPest > 0) {
        howManyPest <- percent(round(howManyPest / howManySubjects, 2))
        txt <-
          paste(howManyPest, "of all wristbands had at least one Pesticide.")
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one PAH
      howManyPAH <-
        sum(SubClassScores$classification == PAH_text_string)
      if (howManyPAH > 0) {
        howManyPAH <- percent(round(howManyPAH / howManySubjects, 2))
        txt <-
          paste(
            howManyPAH,
            "of all wristbands had at least one Polycyclic Aromatic Hydrocarbon (PAH)."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one VOC
      howManyVOC <-
        sum(SubClassScores$classification == VOC_text_string)
      if (howManyVOC > 0) {
        howManyVOC <- percent(round(howManyVOC / howManySubjects, 2))
        txt <-
          paste(
            howManyVOC,
            "of all wristbands had at least one Volatile Organic Compound (VOC)."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }


      ## ADD SX% of wristbands detected at least one PCB
      howManyPCB <-
        sum(SubClassScores$classification == PCB_text_string)
      if (howManyPCB > 0) {
        howManyPCB <- percent(round(howManyPCB / howManySubjects, 2))
        txt <-
          paste(
            howManyPCB,
            "of all wristbands had at least one Polychlorinated Biphenyl (PCB)."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one pharmacological
      howManyPHARMA <-
        sum(SubClassScores$classification == pharmacological__text_string)
      if (howManyPHARMA > 0) {
        howManyPHARMA <- percent(round(howManyPHARMA / howManySubjects, 2))
        txt <-
          paste(
            howManyPHARMA,
            "of all wristbands had at least one Pharmacological compound."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one Personal Care
      howManyPERSONAL <-
        sum(SubClassScores$classification == personalCare_text_string)
      if (howManyPERSONAL > 0) {
        howManyPERSONAL <- percent(round(howManyPERSONAL / howManySubjects, 2))
        txt <-
          paste(
            howManyPERSONAL,
            "of all wristbands had at least one Personal Care compound."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one Chemical in Commerce
      howManyCOMMERCE <-
        sum(SubClassScores$classification == industrial_text_string)
      if (howManyCOMMERCE > 0) {
        howManyCOMMERCE <- percent(round(howManyCOMMERCE / howManySubjects, 2))
        txt <-
          paste(
            howManyCOMMERCE,
            "of all wristbands had at least one chemical commonly found in Commerce."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one Consumer Products
      howManyCONSUMER <-
        sum(SubClassScores$classification == consumerProduct_text_string)
      if (howManyCONSUMER > 0) {
        howManyCONSUMER <- percent(round(howManyCONSUMER / howManySubjects, 2))
        txt <-
          paste(
            howManyCONSUMER,
            "of all wristbands had at least one chemical commonly found in Consumer Products."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }

      ## ADD SX% of wristbands detected at least one Dioxins and Furans
      howManyDIOXIN <-
        sum(SubClassScores$classification == dioxinsAndFurans_text_string)
      if (howManyDIOXIN > 0) {
        howManyDIOXIN <- percent(round(howManyDIOXIN / howManySubjects, 2))
        txt <-
          paste(
            howManyDIOXIN,
            "of all wristbands had at least one chemical classified as a Dioxin or Furan."
          )
        # txt<-paste(bullet,txt)
        mesg.v <- cbind(mesg.v, txt)
      }
    }

    mesg.v
  }




#' Add Message to Message Vector
#'
#' Adds a formatted message to an existing message vector if a condition is met.
#'
#' @param mesgVector Character vector. The existing message vector.
#' @param foundOrNot Numeric. Condition value determining whether to add the message.
#' @param numFound Numeric. Value to include at the start of the message.
#' @param newMesg Character. The message text to add.
#'
#' @return Updated character vector with the new message added if condition is met.
#'
#' @examples
#' \dontrun{
#' messages <- addMesg(messages, 5, 5, "chemicals were found in the database.")
#' }
#'
addMesg <- function(mesgVector, foundOrNot, numFound, newMesg) {
  if (foundOrNot > 0) {
    mesgVector <- cbind(mesgVector, paste(numFound, newMesg, sep = " "))
  }
  mesgVector
}

#####
#####

### I'm going to put/move misc support functions that don't belong other places into this R script and then call this R script somewhere
###


#' Create a Clickable URL Link
#'
#' This function takes in a URL and a clickable text as inputs and returns an HTML anchor tag
#' that creates a clickable link that opens in a new browser tab (for HTML output)
#' or a formatted text string (for non-HTML outputs).
#'
#' @param URL Character. The URL to link to.
#' @param clickableText Character. The text to display for the link.
#'
#' @return Character string. HTML link tag or formatted text depending on output format.
#'
#' @examples
#' \dontrun{
#' makeClickableURL("https://example.com", "Example Website")
#' }
#'
makeClickableURL <- function(URL, clickableText) {
  if (knitr::is_html_output()) {
    # Generate HTML anchor tag for HTML output
    paste0(
      '<a href="', URL, '" target="_blank">', clickableText, "</a>"
    )
  } else {
    # Plain text format for non-HTML outputs
    paste0(clickableText, " (", URL, ")")
  }
}


## THIS generates a TAB_ID following the rules needed by rmarkdown and tabs
##    it makes everything lower and strips out paraenthasis and replaces space with dash
##      CAUTION:  Maybe there are other special characters we'll need to address also if we find them???
##

# PREPEND an x_ in front of title cause i need it to get around weird problems with rmarkdown forcing LINK ID to be changed
#   in this case, just add the x_ for the tab title, i later use javascript to remove it.
#' Prepare Tab Title for R Markdown
#'
#' Prepends 'x_' to tab names when needed to ensure compatibility with R Markdown's tab system.
#'
#' @param tab_name Character. The original tab name.
#'
#' @return Character. The prepared tab name, possibly with 'x_' prepended.
#'
#' @examples
#' \dontrun{
#' prepareTabTitle("2,3-Dimethylpentane") # Returns "x_2,3-Dimethylpentane"
#' prepareTabTitle("Benzene") # Returns "Benzene" (no change needed)
#' }
#'
prepareTabTitle <- function(tab_name) {
  # Check if the first character will be stripped by R Markdown
  if (!grepl("^[a-zA-Z_]", tab_name)) {
    # Prepend an underscore if the first character is invalid
    return(paste0("x_", tab_name))
  }

  # Return the original name if no prepending is needed
  return(tab_name)
}


# Function to generate a clean, HTML-compatible tab ID from a string
# PREPEND an x_ in front of title cause i need it to get around weird problems with rmarkdown forcing LINK ID to be changed
#   in this case, i both prepend an x_ if the label begins with a numbe or other weird charcater aND also do some of the tricks rmarkdown does to text
#     i probably could use this "change text later" logic in some more complete way but this works
generateTabID <- function(chem_name) {
  # Step 1: Clean the name to remove unwanted characters
  clean_name <- gsub("[,()\\[\\]{}!@#$%^&*+=~`<>?/|\\\\\"';:]", "", gsub(" ", "-", chem_name), perl = TRUE)

  # Step 2: Check if the first character will be stripped
  if (!grepl("^[a-zA-Z_]", clean_name)) {
    # If the first character isn't a letter or underscore, prepend an underscore
    # clean_name <- paste0("Chemical_", clean_name)
    clean_name <- paste0("x_", clean_name)
  }

  # Step 3: Convert to lowercase for consistency
  return(tolower(clean_name))
}



# Generate a LINK to match the link auto-generated by rmarkdown based on a string
generateTabLink <- function(chem_name) {
  htmltools::HTML(
    paste0(
      '<a href="#', generateTabID(chem_name), '" onclick="openTab(\'', generateTabID(chem_name), '\')">',
      chem_name,
      "</a>"
    )
  )
}



#' Look Up Non-Zero Count for a Chemical
#'
#' Retrieves the non-zero count for a specified chemical from a lookup table.
#'
#' @param parameter_name Character. The name of the parameter (chemical) to look up.
#' @param howManyHaveParameterName Data frame. Table containing parameter names and their non-zero counts.
#'
#' @return Numeric. The non-zero count for the specified parameter.
#'
#' @examples
#' \dontrun{
#' count <- lookup_non_zero_count("Benzene", howManyHaveParameterName)
#' }
#'
lookup_non_zero_count <- function(parameter_name, howManyHaveParameterName) {
  result <- howManyHaveParameterName %>%
    filter(ParameterName == parameter_name) %>%
    pull(non_zero_count)
  return(result)
}
