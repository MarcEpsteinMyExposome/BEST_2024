# This script processes chemical test results for a given sample and generates
# a detailed report on chemical classifications and compounds detected in the sample.
# The report is based on percentile criteria and compares the sample's results to
# those of a broader dataset.


# Here we implement the headline rules defined by silent spring
# Define the function
#
#     ### THESE ARE THE RULES from Silient spring BUT i slightly reworded AND skipped a few just to get something done.
generate_report2 <- function(sampleNumber, testResults.bigWithClass, oneResult, debug = FALSE, debug2 = FALSE) {
  # sampleNumber<-subject
  # Helper function to calculate specified percentile of a numeric vector
  calculate_percentile <- function(x, percentile) {
    quantile(x, probs = percentile / 100, na.rm = TRUE)
  }

  # so individual_data will be all the data for this one user sample
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
    countConcern = integer(), # count of compounds in classification for this sample that are chemicals-of-concern
    stringsAsFactors = FALSE
  )

  # Iterate through each classification to generate report messages
  for (class in classifications) {
    # class <- "Pollutants from burning fuels, soot, smoke"

    # get the rows ONLY that match this classification
    individual_class_data <- individual_data %>% filter(classification == class)

    if (debug) {
      cat("in classification loop, classification = ", class, "\n")
    }

    # get the rows ONLY that match this classification.  NOTE now we have a way to see all data for this class AND all individual_data for this class
    class_data <- testResults.bigWithClass %>% filter(classification == class) # this is the data for the current classification

    # how many chemicals of concern did this subject have for this classification
    countChemsOfConcern <- oneResult %>%
      left_join(class_L, by = "ParameterID") %>%
      filter(classification == class) %>%
      pull(ParameterName) %>%
      length()

    # # Get the list of unique samples in the dataset
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

    # Loop through each compound in the classification if it is in this individual data or NOT
    for (compound in unique(class_data$ParameterName)) {
      # compound <- "Naphthalene" # this is one in FUELS and burning
      if (debug) {
        cat("in compound loop, compound = ", compound, "  class = ", class, "\n")
      }

      # Get the data for this compound zero aan non-zero
      compound_data <- class_data %>% filter(ParameterName == compound)
      # Get the data for this compound that is non-zero
      compound_data_non_zero <- compound_data %>% filter(Result > 0)

      # This should be at most ONE ROW because we are ONE subject, one compound, so that should be exactly ONE
      individual_class_compound_data <- individual_class_data %>% filter(ParameterName == compound)

      # STOP with an error if       length(individual_class_compound_data$Result) not exactly 1
      if (length(individual_class_compound_data$Result) != 1) {
        stop("Error: Expected exactly one row for this subject and compound")
      }

      # SilentSpring maybe wanted these % to cover ALL zero and non-zero... i didn't do that
      pct_95 <- calculate_percentile(compound_data_non_zero$Result, 95)
      pct_75 <- calculate_percentile(compound_data_non_zero$Result, 75)
      pct_50 <- calculate_percentile(compound_data_non_zero$Result, 50)
      pct_25 <- calculate_percentile(compound_data_non_zero$Result, 25)
      median_result <- median(compound_data_non_zero$Result, na.rm = TRUE)

      # as a reminder for myself i now have lots of variables:
      #         individual_data
      #         individual_class_data
      #         individual_class_compound_data
      #         compound_data
      #         compound_data_non_zero

      # Criteria 1: Result >= 95th percentile and found in <= 10% of samples
      if (individual_class_compound_data$Result >= pct_95 &&
        (length(compound_data_non_zero$Result) / sample_count) <= 0.10) {
        compounds_meeting_criteria1 <- c(compounds_meeting_criteria1, compound)
      }

      # Criteria 2: Result >= 95th percentile and > 10 times the median
      if (
        individual_class_compound_data$Result >= pct_95 &
          individual_class_compound_data$Result >= 10 * median_result
      ) {
        compounds_meeting_criteria2 <- c(compounds_meeting_criteria2, compound)
      }

      # Criteria 3: Result >= 95th percentile and found in >= 10% of samples
      if (individual_class_compound_data$Result >= pct_95 &&
        (length(compound_data_non_zero$Result) / sample_count) >= 0.10) {
        compounds_meeting_criteria3 <- c(compounds_meeting_criteria3, compound)
      }
      # Criteria 4: Result >= 75th percentile and >10 times median
      if (
        individual_class_compound_data$Result >= pct_75 &
          individual_class_compound_data$Result > 10 * median_result
      ) {
        compounds_meeting_criteria4 <- c(compounds_meeting_criteria4, compound)
      }
      # Criteria 5: Result >= 75th percentile and at least 25% of samples detect
      if (individual_class_compound_data$Result >= pct_75 &
        (length(compound_data_non_zero$Result) / sample_count) >= 0.25) {
        compounds_meeting_criteria5 <- c(compounds_meeting_criteria5, compound)
      }
      # Criteria 6: Not detected for all compounds in a group
      # so here what i want to do is check if a compound IS detected for this subject because then if ANY compound is we do NOT meet criteria 6
      # NOTE i could more easily just check here if   countChemsOfConcern   is equal 0...that should be same and doesn't need to be in this loop but...
      if (individual_class_compound_data$Result != 0) {
        compounds_meeting_criteria6 <- c(compounds_meeting_criteria6, compound)
      }
      # Criteria 7: Result < 50th percentile for all compounds in group
      #     and we know that at least ONE compound is FOUND for this group from test 6 above
      #    so we can just check if this compound is GREATER that 50% and if it is then add it to list and that means we don't meet criteria 7
      if (individual_class_compound_data$Result > pct_50) {
        compounds_meeting_criteria7 <- c(compounds_meeting_criteria7, compound)
      }
      # Criteria 8: Not detected and ≤20% of measurements are non-detects
      if (individual_class_compound_data$Result == 0 &&
        (sum(compound_data$Result == 0) / sample_count) <= 0.20) {
        compounds_meeting_criteria8 <- c(compounds_meeting_criteria8, compound)
      }
    } # end compound loop

    if (debug2) {
      cat(
        "Criteria for classification:", class, "\n",
        "crit1=", compounds_meeting_criteria1, "\n",
        "crit2=", compounds_meeting_criteria2, "\n",
        "crit3=", compounds_meeting_criteria3, "\n",
        "crit4=", compounds_meeting_criteria4, "\n",
        "crit5=", compounds_meeting_criteria5, "\n",
        "crit6=", compounds_meeting_criteria6, "\n",
        "crit7=", compounds_meeting_criteria7, "\n",
        "crit8=", compounds_meeting_criteria8, "\n"
      )
    }

    # Generate a sanitized modal id using  existing function
    #  THIS SAME modal id is used generating the BS MODAL dialog in an "html" section in the top of the main Rmd file
    modal_id <- generateTabID(class)

    message <- ""
    if (length(compounds_meeting_criteria1) > 1) {
      message <- paste0(
        "You had some ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " chemicals detected at a high level that were not found in most other wristbands."
      )
    } else if (length(compounds_meeting_criteria1) == 1) {
      message <- paste0( #               THIS IS WHAT I"M MESSING WITH NOW THIS IS WHAT I"M MESSING WITH NOW THIS IS WHAT I"M MESSING WITH NOW THIS IS WHAT I"M MESSING WITH NOW
        "You had an ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " compound, ",
        generateTabLink(compounds_meeting_criteria1[1]),
        " detected at a high level that was not found in most other wristbands."
      )
    } else if (length(compounds_meeting_criteria2) > 1) {
      message <- paste0(
        "You had higher levels of ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " compared to 95% of the other people."
      )
    } else if (length(compounds_meeting_criteria2) == 1) {
      message <- paste0(
        "You had a higher level of ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " compound, ",
        generateTabLink(compounds_meeting_criteria2[1]),
        ", compared to 95% of other people in the study."
      )
    } else if (length(compounds_meeting_criteria3) > 0) {
      message <- paste0(
        "You had higher levels of ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " compared to 95% of the other people."
      )
    } else if (length(compounds_meeting_criteria4) > 0) {
      message <- paste0(
        "You had higher levels of ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " compared to 75% of the other people. Some of your numbers are much higher than most others."
      )
    } else if (length(compounds_meeting_criteria5) > 0) {
      message <- paste0(
        "You had higher levels of ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " compared to 75% of the other people."
      )
    } else if (length(compounds_meeting_criteria6) == 0) {
      message <- paste0(
        "You had no ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " detected in your wristband."
      )
    } else if (length(compounds_meeting_criteria7) == 0) { # We only added compounds to this list if they were GREATER than 50% so if we have any then we don't meet criteria 7
      message <- paste0(
        "You had lower levels of ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " than most others."
      )
    } else if (length(compounds_meeting_criteria8) > 0) {
      message <- paste0(
        "You did not have some ",
        "<a href='#' data-toggle='modal' data-target='#",
        modal_id,
        "'>",
        "<strong>",
        class,
        "</strong></a>",
        " detected in your wristband that were found in most others."
      )
    }

    # Determine additional columns (image, explanation, strategies) based on classification
    # Lookup additional info from the classExplainTable tibble
    match_row <- classExplainTable %>% filter(`Chemical Group` == class)

    # classExplainTable$`Chemical Group`[6]
    # class

    if (nrow(match_row) > 0) {
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
      countConcern = countChemsOfConcern,
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


# generate_report2(sampleNumber, testResults.bigWithClass, debug = FALSE, debug2 = FALSE)


# Print the messages
# cat(paste0("* ", report_messages, collapse = "\n* "), "\n")
