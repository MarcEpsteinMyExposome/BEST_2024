# Clean up environment a little
#rm(list=ls()[!ls() %in% c("masterParam")])

rm(list=ls()[!ls() %in% c("testResults.bigWithClass")])

#
# This only gets run ONCE each time there is new data to hadd to the historica dataset
#
# HERE IS CORRECT WAY TO DO IT
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
}
library(here)
setwd(here::here())

#
suppressMessages(library("tidyverse"))

#head(testResults.bigWithClass)


# Generate a set of test data that triggers different conditions in your code

library(dplyr)

# Generate a smaller tibble with specific values to test different conditions
generate_test_data <- function() {
  tibble(
    ParameterID = c(
      "1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003", "1004", "1004", "1004",
      "1005", "1005", "1005", "1006", "1006", "1006", "1007", "1007", "1007"
    ),
    ParameterName = c(
      "Compound_A", "Compound_A", "Compound_A",
      "Compound_B", "Compound_B", "Compound_B",
      "Compound_C", "Compound_C", "Compound_C",
      "Compound_D", "Compound_D", "Compound_D",
      "Compound_E", "Compound_E", "Compound_E",
      "Compound_F", "Compound_F", "Compound_F",
      "Compound_G", "Compound_G", "Compound_G"
    ),
    SampleNumber = c(
      "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
      "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21"
    ),
    Result = c(
      0, 50, 100,
      0, 500, 1000,
      2000, 5000, 10000,
      0, 20, 10,
      5, 20, 100,
      10, 50, 200,
      0, 500, 750
    ),
    classification = c(
      "Industrial & Commercial Chemicals", "Industrial & Commercial Chemicals", "Industrial & Commercial Chemicals",
      "Flame Retardants", "Flame Retardants", "Flame Retardants",
      "Agricultural & Pharmaceutical Chemicals", "Agricultural & Pharmaceutical Chemicals", "Agricultural & Pharmaceutical Chemicals",
      "Pollutants from crude oil, fuel, and fires", "Pollutants from crude oil, fuel, and fires", "Pollutants from crude oil, fuel, and fires",
      "Consumer & Personal Care Products", "Consumer & Personal Care Products", "Consumer & Personal Care Products",
      "Flame Retardants", "Flame Retardants", "Flame Retardants",
      "Pollutants from crude oil, fuel, and fires", "Pollutants from crude oil, fuel, and fires", "Pollutants from crude oil, fuel, and fires"
    )
  )
}

# Create a sample tibble with the generated data
test_data <- generate_test_data()

# Display the test data
print(test_data)

# Explanation of the values:
# 1. Multiple SampleNumbers for each compound to enable comparison across samples.
# 2. Each compound has a range of values across different SampleNumbers to test whether a value is relatively high or low compared to other instances of the same compound.
# 3. For example:
#    - Compound_A has values 0, 50, 100 across SampleNumbers A1, A2, A3.
#    - Compound_B has values 0, 500, 1000 across SampleNumbers A4, A5, A6.
#    - Compound_C has values 2000, 5000, 10000 across SampleNumbers A7, A8, A9.
#
# Expected results for each SampleNumber:
# 1. A1 (Compound_A, Result = 0): Non-detect, should not be flagged as high.
# 2. A2 (Compound_A, Result = 50): Moderate value, expected to be below 75th percentile.
# 3. A3 (Compound_A, Result = 100): High value, expected to be above the median.
# 4. A4 (Compound_B, Result = 0): Non-detect, should not be flagged as high.
# 5. A5 (Compound_B, Result = 500): Moderate value, expected to be around the median.
# 6. A6 (Compound_B, Result = 1000): High value, expected to be above the 95th percentile.
# 7. A7 (Compound_C, Result = 2000): Lower value for this compound, should be below median.
# 8. A8 (Compound_C, Result = 5000): Moderate value, expected to be close to the median.
# 9. A9 (Compound_C, Result = 10000): High value, expected to be above the 95th percentile.
# 10. A10 (Compound_D, Result = 0): Non-detect, should not be flagged.
# 11. A11 (Compound_D, Result = 20): Low value, expected to be below median.
# 12. A12 (Compound_D, Result = 10): Very low value, should not be flagged as high.
# 13. A13 (Compound_E, Result = 5): Low value, expected to be below median.
# 14. A14 (Compound_E, Result = 20): Moderate value, expected to be above some other values but not high.
# 15. A15 (Compound_E, Result = 100): High value, should be flagged as above the 95th percentile.
# 16. A16 (Compound_F, Result = 10): Low value, expected to be below median.
# 17. A17 (Compound_F, Result = 50): Moderate value, expected to be around the median.
# 18. A18 (Compound_F, Result = 200): High value, expected to be above the 95th percentile.
# 19. A19 (Compound_G, Result = 0): Non-detect, should not be flagged as high.
# 20. A20 (Compound_G, Result = 500): Moderate value, expected to be around the median.
# 21. A21 (Compound_G, Result = 750): High value, expected to be above the 75th percentile.
# This updated dataset ensures that each compound appears in multiple sample numbers, enabling percentile calculations and relative comparisons for each compound individually.


debug<-FALSE
debug2<-FALSE
#sampleNumber<-"A1"

testLogic(test_data, "A1", debug,  debug2)
testLogic(test_data, "A2", debug,  debug2)
testLogic(test_data, "A3", debug,  debug2)
testLogic(test_data, "A4", debug,  debug2)
testLogic(test_data, "A5", debug,  debug2)
testLogic(test_data, "A6", debug,  debug2)
testLogic(test_data, "A7", debug,  debug2)
testLogic(test_data, "A8", debug,  debug2)
testLogic(test_data, "A9", debug,  debug2)
testLogic(test_data, "A10", debug,  debug2)
testLogic(test_data, "A11", debug,  debug2)
testLogic(test_data, "A12", debug,  debug2)
testLogic(test_data, "A13", debug,  debug2)
testLogic(test_data, "A14", debug,  debug2)
testLogic(test_data, "A15", debug,  debug2)


#sampleNumber<-"A2"
#sampleNumber<-"A3"
#sampleNumber<-"A4"
#sampleNumber<-"A5"
#sampleNumber<-"A6"
#sampleNumber<-"A7"
#sampleNumber<-"A8"
#sampleNumber<-"A9"
#sampleNumber<-"A10"
#sampleNumber<-"A11"
#ampleNumber<-"A12"
#ampleNumber<-"A13"
#sampleNumber<-"A14"
#sampleNumber<-"A15"

testLogic <- function(test_data, sampleNumber, debug,  debug2) {

  # Helper function to calculate specified percentile of a numeric vector
  calculate_percentile <- function(x, percentile) {
    quantile(x, probs = percentile / 100, na.rm = TRUE)
  }

  #individual_data <- testResults.bigWithClass %>% filter(SampleNumber == sampleNumber)

  individual_data <- test_data %>% filter(SampleNumber == sampleNumber)


  # Get the list of unique classifications in the dataset
  classifications <- unique(test_data$classification)

  # Initialize empty message vector
  messages <- c()
  message <- NULL

  #class<-"Flame Retardant"
  # class<-"Pesticides"
  # Iterate through each classification to generate report messages
  for (class in classifications) {
    # class <- "Consumer & Personal Care Products"   # USING THIS to step trace and debug if message is correct
    # class <- "Agricultural & Pharmaceutical Chemicals"
    #class<-"Flame Retardant"


    if (debug) {
      cat("in classification loop, classification = ", class, "   messages= ", messages, " message = ", message, "\n")
    }


    class_data <- test_data %>% filter(classification == class) # This is all the testResults that have any rows with this specific class
    sample_count <- length(unique(test_data$SampleNumber)) # this is total # of samples since includes ZERO is that correct??
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
      # compound <-"Compound_C"
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

      pct_95 <- calculate_percentile(compound_data_non_zero$Result, 95)   # CHANGE THIS TO ONLY OF THE PEOPLE WHO HAVE, so 95% is only for positive results!
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
        individual_data$Result < pct_50
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
        #compound,
        compounds_meeting_criteria1[1],
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
        compounds_meeting_criteria2,
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

  messages

}
