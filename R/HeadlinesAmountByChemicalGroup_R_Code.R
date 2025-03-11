# This script processes chemical test results for a given sample and generates
# a detailed report on chemical classifications and compounds detected in the sample.
# The report is based on percentile criteria and compares the sample's results to
# those of a broader dataset.

#
# HERE ARE SAMPLES OF THE 3 variables being passed into function:
#   > str(subject)
# chr "A241576"
# > str(testResults.bigWithClass)
# tibble [15,158 × 5] (S3: tbl_df/tbl/data.frame)
# $ ParameterID   : chr [1:15158] "1023" "1023" "1023" "1023" ...
# $ ParameterName : chr [1:15158] "Tri-p-tolyl phosphate" "Tri-p-tolyl phosphate" "Tri-p-tolyl phosphate" "Tri-p-tolyl phosphate" ...
# $ SampleNumber  : chr [1:15158] "A241133" "A241134" "A241137" "A241139" ...
# $ Result        : num [1:15158] 0 0 0 0 0 0 0 0 0 0 ...
# $ classification: chr [1:15158] "Industrial & Commercial Chemicals" "Industrial & Commercial Chemicals" "Industrial & Commercial Chemicals" "Industrial & Commercial Chemicals" ...
# > str (oneResult)
# tibble [20 × 26] (S3: tbl_df/tbl/data.frame)
# $ ParameterID           : chr [1:20] "69" "303000" "301134" "303324" ...
# $ SampleNumber          : chr [1:20] "A241576" "A241576" "A241576" "A241576" ...
# $ Days_worn             : num [1:20] 1.25 1.25 1.25 1.25 1.25 1.25 1.25 1.25 1.25 1.25 ...
# $ Result                : num [1:20] 1.08 21.7 28.9 37.9 253 560 155 325 470 3250 ...
# $ ResultOriginal        : num [1:20] 6 120 160 210 1400 3100 860 1800 2600 18000 ...
# $ Flag                  : chr [1:20] "J" "" "" "" ...
# $ MyE_Received          : chr [1:20] "8/21/2024" "8/21/2024" "8/21/2024" "8/21/2024" ...
# $ size_factor           : num [1:20] 4.43 4.43 4.43 4.43 4.43 4.43 4.43 4.43 4.43 4.43 ...
# $ week_factor           : num [1:20] 0.18 0.18 0.18 0.18 0.18 0.18 0.18 0.18 0.18 0.18 ...
# $ Lab_Submission_Batch  : chr [1:20] "PO 262" "PO 262" "PO 262" "PO 262" ...
# $ ParameterName         : chr [1:20] "1-Methylnaphthalene" "B-Ionone" "Benzophenone" "Benzothiazole" ...
# $ CASNumber             : chr [1:20] "90-12-0" "79-77-6" "119-61-9" "95-16-9" ...
# $ PureSampleName        : chr [1:20] "175-WB" "175-WB" "175-WB" "175-WB" ...
# $ norm_Result           : num [1:20] 0.06136 0.00756 0.04412 0.05074 0.00948 ...
# $ quartile              : num [1:20] 1 1 1 1 2 3 3 3 2 4 ...
# $ is_this_max           : logi [1:20] FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ endocrine_toxicity    : num [1:20] 0 0 0 0 0 0 1 0 1 0 ...
# $ respiratory_toxicity  : num [1:20] 0 0 0 0 0 0 0 0 0 0 ...
# $ carcinogenicity       : num [1:20] 0 0 1 0 0 0 0 0 0 0 ...
# $ genotoxicity          : num [1:20] 0 0 0 0 0 0 0 0 0 0 ...
# $ reproductive_toxicity : num [1:20] 0 0 0 0 0 0 1 0 1 0 ...
# $ developmental_toxicity: num [1:20] 0 0 0 0 0 0 1 0 1 0 ...
# $ neurotoxicity         : num [1:20] 1 0 0 0 0 0 0 0 0 0 ...
# $ pbt                   : num [1:20] 0 0 0 0 0 0 0 0 0 0 ...
# $ calSaferURL           : chr [1:20] "https://calsafer.dtsc.ca.gov/cms/candidate-chemical/?rid=22381" NA "https://calsafer.dtsc.ca.gov/cms/candidate-chemical/?rid=20641" NA ...
# $ tot_haz               : num [1:20] 1 0 1 0 0 0 3 0 3 0 ...




# Here we implement the headline rules defined by silent spring
# Define the function
#
#     ### THESE ARE THE RULES from Silient spring BUT i slightly reworded AND skipped a few just to get something done.
#' Generate Chemical Classification Report
#'
#' This function processes chemical test results for a sample and generates
#' a detailed report on chemical classifications detected in the sample.
#' The report includes messages, images, explanations, and strategies for each
#' classification based on various criteria.
#'
#' @param sampleNumber Character. The ID of the sample to analyze.
#' @param testResults.bigWithClass Tibble. Dataset containing test results for all samples.
#' @param oneResult Tibble. Dataset containing detailed results for the specific sample.
#' @param classExplainTable Tibble. Lookup table containing descriptions, strategies, and image files for each chemical classification.
#' @param class_L Tibble. Lookup table mapping chemical parameter IDs to their classifications.
#' @param debug Logical. If TRUE, prints additional diagnostic information.
#' @param debug2 Logical. If TRUE, prints detailed criteria evaluation information.
#'
#' @return A data frame (tibble) with the following columns:
#'   \item{classification}{Character. The chemical classification group.}
#'   \item{message}{Character. The personalized message for this classification.}
#'   \item{image}{Character. Filename of the associated image.}
#'   \item{explanation}{Character. Explanation text for this classification.}
#'   \item{strategies}{Character. Recommended strategies for this classification.}
#'   \item{countConcern}{Integer. Count of compounds of concern in this classification.}
#'
#' @examples
#' # Generate report for sample "A241576"
#' report <- generate_report2("A241576", testResults.bigWithClass, oneResult,
#'                            classExplainTable, class_L)
generate_report2 <- function(sampleNumber, testResults.bigWithClass, oneResult, classExplainTable, class_L , debug = FALSE, debug2 = FALSE) {
  # sampleNumber<-subject
  # Helper function to calculate specified percentile of a numeric vector
  calculate_percentile <- function(x, percentile) {
    quantile(x, probs = percentile / 100, na.rm = TRUE)
  }


  # Add validation for input data
  if (nrow(testResults.bigWithClass) == 0 || is.null(testResults.bigWithClass)) {
    stop("Error: testResults.bigWithClass is empty or NULL")
  }

  if (nrow(oneResult) == 0 || is.null(oneResult)) {
    stop("Error: oneResult is empty or NULL")
  }

  # Validate required columns exist
  required_cols_big <- c("SampleNumber", "ParameterName", "Result", "classification")
  required_cols_one <- c("SampleNumber", "ParameterID", "ParameterName")

  if (!all(required_cols_big %in% colnames(testResults.bigWithClass))) {
    stop("Error: testResults.bigWithClass is missing required columns")
  }

  if (!all(required_cols_one %in% colnames(oneResult))) {
    stop("Error: oneResult is missing required columns")
  }

  # Validate that the sampleNumber exists in oneResult
  if (!(sampleNumber %in% oneResult$SampleNumber)) {
    stop(paste("Error: sampleNumber", sampleNumber, "not found in oneResult data"))
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
    # Initialize flags at the beginning of each classification loop
    criteria6_all_compounds_not_detected = TRUE  # For criteria 6.  Originally did it by adding compound to list but that was confusing and inverted
    criteria7_all_compounds_below_50th = TRUE    # For criteria 7 Originally did it by adding compound to list but that was confusing and inverted
    #compounds_meeting_criteria6 <- c()
    #compounds_meeting_criteria7 <- c()
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
        criteria6_all_compounds_not_detected = FALSE  # If any compound is detected, criteria 6 is not met
      }
      # Criteria 7: Result < 50th percentile for all compounds in group
      #     and we know that at least ONE compound is FOUND for this group from test 6 above
      #    so we can just check if this compound is GREATER that 50% and if it is then add it to list and that means we don't meet criteria 7
      if (individual_class_compound_data$Result > pct_50) {
        criteria7_all_compounds_below_50th = FALSE  # If any compound is above 50th, criteria 7 is not met
      }
      # Criteria 8: Not detected and ≤20% of measurements are non-detects
      if (individual_class_compound_data$Result == 0 &&
        (sum(compound_data$Result == 0) / sample_count) <= 0.20) {
        compounds_meeting_criteria8 <- c(compounds_meeting_criteria8, compound)
      }
    } # end compound loop


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
    } else if (criteria6_all_compounds_not_detected) {
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
    } else if (criteria7_all_compounds_below_50th) { #
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
