# This script processes chemical test results for a given sample and generates
# a detailed report on chemical classifications and compounds detected in the sample.
# The report is based on percentile criteria and compares the sample's results to
# those of a broader dataset.


# Here we implement the headline rules defined by silent spring
# Define the function
generate_report <- function(sampleNumber, testResults.bigWithClass, debug = FALSE, debug2 = FALSE) {
  # sampleNumber <- subject
  # debug=TRUE

  # Helper function to calculate specified percentile of a numeric vector
  calculate_percentile <- function(x, percentile) {
    quantile(x, probs = percentile / 100, na.rm = TRUE)
  }

  individual_data <- testResults.bigWithClass %>% filter(SampleNumber == sampleNumber)
  # Get the list of unique classifications in the dataset
  classifications <- unique(testResults.bigWithClass$classification)

  # Initialize empty message vector
  messages <- c()
  message <- NULL

  # class<-"Flame Retardants"
  # class<-"Pesticides"
  # Iterate through each classification to generate report messages
  for (class in classifications) {
    # class <- "Consumer & Personal Care Products"   # USING THIS to step trace and debug if message is correct
    # class <- "Agricultural & Pharmaceutical Chemicals"

    if (debug) {
      cat("in classification loop, classification = ", class, "   messages= ", messages, " message = ", message, "\n")
    }


    class_data <- testResults.bigWithClass %>% filter(classification == class) # This is all the testResults that have any rows with this specific class
    sample_count <- length(unique(testResults.bigWithClass$SampleNumber)) # this is total # of samples since includes ZERO is that correct??
    compounds_meeting_criteria1 <- c()
    compounds_meeting_criteria2 <- c()
    compounds_meeting_criteria3 <- c()
    compounds_meeting_criteria4 <- c()
    compounds_meeting_criteria5 <- c()
    compounds_meeting_criteria6 <- c()
    compounds_meeting_criteria7 <- c()
    compounds_meeting_criteria8 <- c()

    # compound <-"TPP"
    # compound <-"Tributyl phosphate"
    # compound <-"PBDE 47"
    # compound <-"Tris(2-ethylhexyl) phosphate"
    # compound <-"Tricresylphosphate, meta-"
    # compound <-"TCPP"

    # compound <-"1,6-Dimethylnaphthalene"    #Pollutants from crude oil, fuel, and fires

    # compound <-"4,4'-DDE"    #Agricultural & Pharmaceutical Chemicals




    # compound <- "Fipronil" #is a Pesticide
    for (compound in unique(class_data$ParameterName)) { # Loop thru all the compounds that are in this class anywhere in the data (not just this subject)
      # compound <- "Trans-Nonachlor" # This is "Agricultural & Pharmaceutical Chemicals"
      # compound <- "Bis(2-ethylhexyl)phthalate"    # This is "Agricultural & Pharmaceutical Chemicals"
      # compound <- "Butyl benzyl phthalate"    # This is ""Consumer & Personal Care Products"
      # compound <- "Bis(2-ethylhexyl)phthalate"    # This is ""Consumer & Personal Care Products"
      # compound <- "Dimethyl phthalate"    # This is ""Consumer & Personal Care Products"
      # compound <- "Carvone"    # This is ""Consumer & Personal Care Products"

      # compound <-"4,4'-DDE"    #Agricultural & Pharmaceutical Chemicals



      if (debug) {
        cat("in compound loop, compound = ", compound, "  class = ", class, "messages= ", messages, "\n")
      }

      compound_data <- class_data %>% filter(ParameterName == compound) # Here are all the results for all the wristbands that have any score (including ZERO) for this compound
      compound_data_non_zero <- compound_data %>% filter(Result > 0)

      pct_95 <- calculate_percentile(compound_data_non_zero$Result, 95) # CHANGE THIS TO ONLY OF THE PEOPLE WHO HAVE, so 95% is only for positive results!
      pct_75 <- calculate_percentile(compound_data_non_zero$Result, 75)
      pct_50 <- calculate_percentile(compound_data_non_zero$Result, 50)
      pct_25 <- calculate_percentile(compound_data_non_zero$Result, 25)
      median_result <- median(compound_data_non_zero$Result, na.rm = TRUE)

      if (debug) {
        cat("\n Compound =", compound, " Class= ", class, "\n")
      }
      if (debug) {
        cat("pct95=", pct_95, " pct_75=", pct_75, " pct_50=", pct_50, " pct25=", pct_25, "  median=", median_result, "\n")
      }


      # Criteria 1: Result >= 95th percentile and found in <= 10% of samples
      if (any(individual_data$Result > 0 &
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_95) &&
        (length(compound_data_non_zero$Result) / sample_count) <= 0.10) {
        compounds_meeting_criteria1 <- c(compounds_meeting_criteria1, compound)
      }
      # Criteria 2: Result >= 95th percentile and > 10 times the median
      if (any(
        individual_data$Result > 0 &
          individual_data$ParameterName == compound &
          individual_data$Result >= pct_95 &
          individual_data$Result >= (10 * median_result)
      )) {
        compounds_meeting_criteria2 <- c(compounds_meeting_criteria2, compound)
      }
      # Criteria 3: Result >= 95th percentile and >10 of individuals HAD this compound
      if (any(
        individual_data$Result > 0 &
          individual_data$ParameterName == compound &
          individual_data$Result >= pct_95
      ) &&
        (length(compound_data_non_zero$Result) / sample_count) >= 0.10) {
        compounds_meeting_criteria3 <- c(compounds_meeting_criteria3, compound)
      }
      # # Criteria 4: Result >= 75th percentile and 10x median
      if (any(
        individual_data$Result > 0 &
          individual_data$ParameterName == compound &
          individual_data$Result >= pct_75 &
          (individual_data$Result > 10 * median_result)
      )) {
        compounds_meeting_criteria4 <- c(compounds_meeting_criteria4, compound)
      }


      # Criteria 5: Result > 75th percentile and 25% measurements are detects
      if (any(
        individual_data$Result > 0 &
          individual_data$ParameterName == compound &
          individual_data$Result >= pct_75
      ) &&
        (length(compound_data_non_zero$Result) / sample_count) >= 0.25) {
        compounds_meeting_criteria5 <- c(compounds_meeting_criteria5, compound)
      }

      # Criteria 6: No detected compounds from this group-- Capture ALL matches and IF NO MATCHES then "no detected compounds from this group"
      if (any(individual_data$ParameterName == compound)) {
        compounds_meeting_criteria6 <- c(compounds_meeting_criteria6, compound)
      }

      # Criteria 7: Result < 50th percentile for all compounds in group
      if (all(
        individual_data$Result > 0 &
          individual_data$ParameterName == compound &
          individual_data$Result <= pct_50
      )) {
        compounds_meeting_criteria7 <- c(compounds_meeting_criteria7, compound)
      }


      # Criteria 8: Not detected for a chemical and ≤10% of all measurements are non-detects
      if (all(individual_data$ParameterName == compound &
        individual_data$Result == 0) &&
        (sum(compound_data$Result == 0) / sample_count) <= 0.10) {
        compounds_meeting_criteria8 <- c(compounds_meeting_criteria8, compound)
      }
    }

    if (debug2) {
      cat(
        "Just left setting up Criteria in loop and Criterial 1 through 8 are", "\n",
        "crit1=", compounds_meeting_criteria1, "\n",
        "crit2=", compounds_meeting_criteria2, "\n",
        "crit3=", compounds_meeting_criteria3, "\n",
        "crit4=", compounds_meeting_criteria4, "\n",
        "crit5=", compounds_meeting_criteria5, "\n",
        "crit6=", compounds_meeting_criteria6, "\n",
        "crit7=", compounds_meeting_criteria7, "\n",
        "crit8=", compounds_meeting_criteria8, "\n",
        "message= ", message, "\n",
        "messages= ", messages, "\n",
        "END OF CRITERIA PRINTING"
      )
    }


    ### THESE ARE THE RULES from Silient spring BUT i slightly reworded AND skipped a few just to get something done.
    ###   Can add more clarity, particularly "how much higher"
    if (length(compounds_meeting_criteria1) > 1) {
      message <- paste0(
        "You had some **",
        class,
        "** chemicals detected at a high level that were not found in most other wristbands."
      )
    } else if (length(compounds_meeting_criteria1) == 1) {
      ### compound <- individual_data$ParameterName[individual_data$Result >= pct_95 & (sum(class_data$Result > 0) / sample_count) <= 0.10]   ### WHAT is this doing
      message <- paste0(
        "You had **",
        class,
        "** compound, **",
        # compound,
        generateTabLink(compounds_meeting_criteria1[1]),
        "** detected at a high level that was not found in most other wristbands."
      )
    } else if (length(compounds_meeting_criteria2) > 1) {
      message <- paste0(
        "You had higher levels of **",
        class,
        "** compared to 95% of the other people."
      )
    } else if (length(compounds_meeting_criteria2) == 1) {
      ### compound <- unique(individual_data$ParameterName[individual_data$Result >= pct_95 & individual_data$Result > 10 * median_result]  ## WHAT is this doing here?)
      message <- paste0(
        "You had a higher level of **",
        class,
        "** compound, **",
        generateTabLink(compounds_meeting_criteria2[1]),
        "**, compared to 95% of other people in the study."
      )
    } else if (length(compounds_meeting_criteria3) > 0) {
      message <- paste0(
        "You had higher levels of **",
        class,
        "** compared to 95% of the other people."
      )
    } else if (length(compounds_meeting_criteria4) > 0) {
      message <- paste0(
        "You had higher levels of **",
        class,
        "** compared to 75% of the other people. Some of your numbers are much higher than most others"
      )
    } else if (length(compounds_meeting_criteria5) > 0) {
      message <- paste0(
        "You had higher levels of **",
        class,
        "** compared to 75% of the other people."
      )
    } else if (length(compounds_meeting_criteria6) == 0) {
      message <- paste0(
        "You had no **",
        class,
        "**  detected in your wristband."
      )
    } else if (length(compounds_meeting_criteria7) > 0) {
      message <- paste0(
        "You had lower levels of **",
        class,
        "** than most others."
      )
    } else if (length(compounds_meeting_criteria8) > 0) {
      message <- paste0(
        "You did not have some **",
        class,
        "** detected in your wristband that were found in most others."
      )
    }


    # Append the message to the vector
    messages <- c(messages, message)
    message <- NULL
  }
  if (debug) {
    cat("\n about to return with messages = ", messages, "\n")
  }
  return(messages)
}


generate_report2 <- function(sampleNumber, testResults.bigWithClass, debug = FALSE, debug2 = FALSE) {
  # Helper function to calculate specified percentile of a numeric vector
  calculate_percentile <- function(x, percentile) {
    quantile(x, probs = percentile / 100, na.rm = TRUE)
  }

  individual_data <- testResults.bigWithClass %>% filter(SampleNumber == sampleNumber)
  # Get the list of unique classifications in the dataset
  classifications <- unique(testResults.bigWithClass$classification)

  # Initialize empty report table
  report_table <- data.frame(
    classification = character(),
    message = character(),
    image = character(),
    explanation = character(),
    strategies = character(),
    stringsAsFactors = FALSE
  )

  # Iterate through each classification to generate report messages
  for (class in classifications) {
    if (debug) {
      cat("in classification loop, classification = ", class, "\n")
    }

    class_data <- testResults.bigWithClass %>% filter(classification == class)
    sample_count <- length(unique(testResults.bigWithClass$SampleNumber))

    # Initialize empty vectors for various criteria
    compounds_meeting_criteria1 <- c()
    compounds_meeting_criteria2 <- c()
    compounds_meeting_criteria3 <- c()
    compounds_meeting_criteria4 <- c()
    compounds_meeting_criteria5 <- c()
    compounds_meeting_criteria6 <- c()
    compounds_meeting_criteria7 <- c()
    compounds_meeting_criteria8 <- c()

    # Loop through each compound in the classification
    for (compound in unique(class_data$ParameterName)) {
      if (debug) {
        cat("in compound loop, compound = ", compound, "  class = ", class, "\n")
      }

      compound_data <- class_data %>% filter(ParameterName == compound)
      compound_data_non_zero <- compound_data %>% filter(Result > 0)

      pct_95 <- calculate_percentile(compound_data_non_zero$Result, 95)
      pct_75 <- calculate_percentile(compound_data_non_zero$Result, 75)
      pct_50 <- calculate_percentile(compound_data_non_zero$Result, 50)
      pct_25 <- calculate_percentile(compound_data_non_zero$Result, 25)
      median_result <- median(compound_data_non_zero$Result, na.rm = TRUE)

      # Criteria 1: Result >= 95th percentile and found in <= 10% of samples
      if (any(individual_data$Result > 0 &
              individual_data$ParameterName == compound &
              individual_data$Result >= pct_95) &&
          (length(compound_data_non_zero$Result) / sample_count) <= 0.10) {
        compounds_meeting_criteria1 <- c(compounds_meeting_criteria1, compound)
      }
      # Criteria 2: Result >= 95th percentile and > 10 times the median
      if (any(
        individual_data$Result > 0 &
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_95 &
        individual_data$Result >= (10 * median_result)
      )) {
        compounds_meeting_criteria2 <- c(compounds_meeting_criteria2, compound)
      }
      # Criteria 3: Result >= 95th percentile and found in >= 10% of samples
      if (any(
        individual_data$Result > 0 &
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_95
      ) &&
      (length(compound_data_non_zero$Result) / sample_count) >= 0.10) {
        compounds_meeting_criteria3 <- c(compounds_meeting_criteria3, compound)
      }
      # Criteria 4: Result >= 75th percentile and >10 times median
      if (any(
        individual_data$Result > 0 &
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_75 &
        (individual_data$Result > 10 * median_result)
      )) {
        compounds_meeting_criteria4 <- c(compounds_meeting_criteria4, compound)
      }
      # Criteria 5: Result >= 75th percentile and at least 25% of samples detect
      if (any(
        individual_data$Result > 0 &
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_75
      ) &&
      (length(compound_data_non_zero$Result) / sample_count) >= 0.25) {
        compounds_meeting_criteria5 <- c(compounds_meeting_criteria5, compound)
      }
      # Criteria 6: Some compound detected (even if low)
      if (any(individual_data$ParameterName == compound)) {
        compounds_meeting_criteria6 <- c(compounds_meeting_criteria6, compound)
      }
      # Criteria 7: Result < 50th percentile for all compounds in group
      if (all(
        individual_data$Result > 0 &
        individual_data$ParameterName == compound &
        individual_data$Result <= pct_50
      )) {
        compounds_meeting_criteria7 <- c(compounds_meeting_criteria7, compound)
      }
      # Criteria 8: Not detected and ≤10% of measurements are non-detects
      if (all(individual_data$ParameterName == compound &
              individual_data$Result == 0) &&
          (sum(compound_data$Result == 0) / sample_count) <= 0.10) {
        compounds_meeting_criteria8 <- c(compounds_meeting_criteria8, compound)
      }
    } # end compound loop

    if (debug2) {
      cat("Criteria for classification:", class, "\n",
          "crit1=", compounds_meeting_criteria1, "\n",
          "crit2=", compounds_meeting_criteria2, "\n",
          "crit3=", compounds_meeting_criteria3, "\n",
          "crit4=", compounds_meeting_criteria4, "\n",
          "crit5=", compounds_meeting_criteria5, "\n",
          "crit6=", compounds_meeting_criteria6, "\n",
          "crit7=", compounds_meeting_criteria7, "\n",
          "crit8=", compounds_meeting_criteria8, "\n")
    }

    # Generate the message based on the criteria
    message <- ""
    if (length(compounds_meeting_criteria1) > 1) {
      message <- paste0(
        "You had some **", class, "** chemicals detected at a high level that were not found in most other wristbands."
      )
    } else if (length(compounds_meeting_criteria1) == 1) {
      message <- paste0(
        "You had **", class, "** compound, **",
        generateTabLink(compounds_meeting_criteria1[1]),
        "** detected at a high level that was not found in most other wristbands."
      )
    } else if (length(compounds_meeting_criteria2) > 1) {
      message <- paste0(
        "You had higher levels of **", class, "** compared to 95% of the other people."
      )
    } else if (length(compounds_meeting_criteria2) == 1) {
      message <- paste0(
        "You had a higher level of **", class, "** compound, **",
        generateTabLink(compounds_meeting_criteria2[1]),
        "**, compared to 95% of other people in the study."
      )
    } else if (length(compounds_meeting_criteria3) > 0) {
      message <- paste0(
        "You had higher levels of **", class, "** compared to 95% of the other people."
      )
    } else if (length(compounds_meeting_criteria4) > 0) {
      message <- paste0(
        "You had higher levels of **", class, "** compared to 75% of the other people. Some of your numbers are much higher than most others."
      )
    } else if (length(compounds_meeting_criteria5) > 0) {
      message <- paste0(
        "You had higher levels of **", class, "** compared to 75% of the other people."
      )
    } else if (length(compounds_meeting_criteria6) == 0) {
      message <- paste0(
        "You had no **", class, "** detected in your wristband."
      )
    } else if (length(compounds_meeting_criteria7) > 0) {
      message <- paste0(
        "You had lower levels of **", class, "** than most others."
      )
    } else if (length(compounds_meeting_criteria8) > 0) {
      message <- paste0(
        "You did not have some **", class, "** detected in your wristband that were found in most others."
      )
    }

    # Determine additional columns (image, explanation, strategies) based on classification
    # Lookup additional info from the classExplainTable tibble
    match_row <- classExplainTable %>% filter(`Chemical Group` == class)
    if(nrow(match_row) > 0) {
      image_file <- match_row$ImageFile[1]
      explanation_text <- match_row$Description[1]
      strategies_text <- match_row$Strategies[1]
    } else {
      image_file <- "default.png"
      explanation_text <- "No specific explanation available."
      strategies_text <- "No specific strategies available."
    }

    # Append a new row for this classification to the report table
    new_row <- data.frame(
      classification = class,
      message = message,
      image = image_file,
      explanation = explanation_text,
      strategies = strategies_text,
      stringsAsFactors = FALSE
    )
    report_table <- rbind(report_table, new_row)
  } # end classification loop

  if (debug) {
    cat("\nAbout to return report_table:\n")
    print(report_table)
  }
  return(report_table)
}


#generate_report2(sampleNumber, testResults.bigWithClass, debug = FALSE, debug2 = FALSE)


# Print the messages
# cat(paste0("* ", report_messages, collapse = "\n* "), "\n")
