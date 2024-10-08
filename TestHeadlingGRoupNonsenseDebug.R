# Test various headline group thigns

HOLD_testResults.bigWithClass<-testResults.bigWithClass
str(testResults.bigWithClass)



#testResults <- testResults.bigWithClass
#sampleNumber <- "AAG0FDC"
# One thing to note is that && only works on single logical values,
#i.e., logical vectors of length 1
#(like you would pass into an if condition),
#but & also works on vectors of length greater than 1.

#testResults <- testResults.bigWithClass
#sampleNumber <- "AAG0FDC"

debug=FALSE
debug2=FALSE

generate_report <- function(sampleNumber, testResults,debug=FALSE, debug2=FALSE) {
  #debug=TRUE

  calculate_percentile <- function(x, percentile) {
    quantile(x, probs = percentile / 100, na.rm = TRUE)
  }

  individual_data <- testResults %>% filter(SampleNumber == sampleNumber)
  classifications <- unique(testResults$classification)
  messages <- c()

  #class<-"Flame Retardant"
  #class<-"Pesticides"
  for (class in classifications) {

    if(debug){cat("in classification loop, classification = ",class, "   messages= ",messages, " message = ",message,"\n")}


    class_data <- testResults %>% filter(classification == class)
    sample_count <- length(unique(testResults$SampleNumber))
    compounds_meeting_criteria1 <- c()
    compounds_meeting_criteria2 <- c()
    compounds_meeting_criteria3 <- c()
    compounds_meeting_criteria4 <- c()
    compounds_meeting_criteria5 <- c()
    compounds_meeting_criteria6 <- c()
    compounds_meeting_criteria7 <- c()
    compounds_meeting_criteria8 <- c()

    #compound <-"TPP"
    #compound <-"Tributyl phosphate"
    #compound <-"PBDE 47"
    #compound <-"Tris(2-ethylhexyl) phosphate"
    #compound <-"Tricresylphosphate, meta-"
    #compound <-"TCPP"



    #compound <- "Fipronil" #is a Pesticide
    for (compound in unique(class_data$ParameterName)) {

      if(debug){cat("in compound loop, compound = ",compound, "  class = ",class,  "messages= ", messages, "\n")}

      compound_data <- class_data %>% filter(ParameterName == compound)
      pct_95 <- calculate_percentile(compound_data$Result, 95)
      pct_75 <- calculate_percentile(compound_data$Result, 75)
      pct_50 <- calculate_percentile(compound_data$Result, 50)
      pct_25 <- calculate_percentile(compound_data$Result, 25)
      median_result <- median(compound_data$Result, na.rm = TRUE)

      if(debug){cat("\n Compound =",compound," Class= ",class,"\n")}
      if(debug){cat("pct95=", pct_95, " pct_75=", pct_75, " pct_50=", pct_50, " pct25=", pct_25,"  median=",median_result,"\n")}


      # Criteria 1: Result >= 95th percentile and found in <= 10% of samples
      if (any(individual_data$ParameterName == compound &
              individual_data$Result >= pct_95) &&
          (sum(compound_data$Result > 0) / sample_count) <= 0.10) {
        compounds_meeting_criteria1 <- c(compounds_meeting_criteria1, compound)
      }
      # Criteria 2: Result >= 95th percentile and > 10 times the median
      if (any(
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_95 &
        individual_data$Result >= (10 * median_result)
      ))    {
        compounds_meeting_criteria2 <- c(compounds_meeting_criteria2, compound)
      }
      # Criteria 3: Result >= 95th percentile and >10 of individuals HAD this compound
      if (any(individual_data$ParameterName == compound &
              individual_data$Result >= pct_95) &&
          (sum(compound_data$Result > 0) / sample_count) >= 0.10)      {
        compounds_meeting_criteria3 <- c(compounds_meeting_criteria3, compound)
      }
      # # Criteria 4: Result >= 75th percentile and 10x median
      if (any(
        individual_data$ParameterName == compound &
        individual_data$Result >= pct_75 &
        (individual_data$Result > 10 * median_result)
      ))      {
        compounds_meeting_criteria4 <- c(compounds_meeting_criteria4, compound)
      }


      # Criteria 5: Result > 75th percentile and 25% measurements are detects
      if (any(individual_data$ParameterName == compound &
              individual_data$Result >= pct_75) &&
          (sum(compound_data$Result > 0) / sample_count) >= 0.25)      {
        compounds_meeting_criteria5 <- c(compounds_meeting_criteria5, compound)
      }

      # Criteria 6: No detected compounds from this group
      if (!any(individual_data$ParameterName == compound)) {
        compounds_meeting_criteria6 <- c(compounds_meeting_criteria6, compound)
      }

      # Criteria 7: Result < 50th percentile for all compounds in group
      if (all(individual_data$ParameterName == compound &
              individual_data$Result <= pct_50)) {
        compounds_meeting_criteria7 <- c(compounds_meeting_criteria7, compound)
      }


      # Criteria 8: Not detected for a chemical and ≤10% of all measurements are non-detects
      if (all(individual_data$ParameterName == compound &
              individual_data$Result <= pct_50) &&
          (sum(compound_data$Result > 0) / sample_count) <= 0.10)   {
        compounds_meeting_criteria8 <- c(compounds_meeting_criteria8, compound)
      }

    }

    if(debug2){cat("Just left setting up Criteria in loop and Criterial 1 through 8 are" ,"\n",
        "crit1=",compounds_meeting_criteria1, "\n",
        "crit2=",compounds_meeting_criteria2 ,"\n",
        "crit3=",compounds_meeting_criteria3,"\n",
        "crit4=",compounds_meeting_criteria4,"\n",
        "crit5=",compounds_meeting_criteria5,"\n",
        "crit6=",compounds_meeting_criteria6,"\n",
        "crit7=",compounds_meeting_criteria7,"\n",
        "crit8=",compounds_meeting_criteria8 ,"\n",
        "message= ",message, "\n",
        "messages= ",messages, "\n",

        "END OF CRITERIA PRINTING")}



    if (length(compounds_meeting_criteria1) > 1) {
      message <- paste(
        "You had some",
        class,
        " detected in your wristband that were not found in most others."
      )
    } else if (length(compounds_meeting_criteria1) == 1) {
      compound <- individual_data$ParameterName[individual_data$Result >= pct_95 &
                                                  (sum(class_data$Result > 0) / sample_count) <= 0.10]
      message <- paste(
        "You had exactly one",
        class,
        "compound,",
        compound,
        ". This compound was not found in most others."
      )
    } else if (length(compounds_meeting_criteria2) > 1) {
      message <- paste(
        "You had higher levels of",
        class,
        "compared to 95% of the other people in the study."
      )
    } else if (length(compounds_meeting_criteria2) == 1) {
      compound <- individual_data$ParameterName[individual_data$Result >= pct_95 &
                                                  individual_data$Result > 10 * median_result]
      message <- paste(
        "You had exactly exactly one",
        class,
        "compound,",
        compound,
        " that was higher than 95% of the other people in this study."
      )
    } else if (length(compounds_meeting_criteria3) > 0) {
      message <- paste(
        "You had higher levels of",
        class,
        "compared to 95% of the other people in the study."
      )
    } else if (length(compounds_meeting_criteria4) > 0) {
      message <- paste(
        "You had higher levels of",
        class,
        "compared to 75% of the other people in the study. Some of your numbers are much higher than most others"
      )
    } else if (length(compounds_meeting_criteria5) > 0) {
      message <- paste(
        "You had higher levels of",
        class,
        "compared to 75% of the other people in the study."
      )
    } else if (length(compounds_meeting_criteria6) > 0) {
      message <- paste("YYou had no ",
                       class,
                       " {group name} detected in your wristband.")
    } else if (length(compounds_meeting_criteria7) > 0) {
      message <- paste("You had lower levels of",
                       class,
                       "than most others in the studyy.")
    } else if (length(compounds_meeting_criteria8) > 0) {
      message <- paste(
        "You did not have some ",
        class,
        "detected in your wristband that were found in most others."
      )
    } else {
      message <- paste(
        "No significant findings for ",
        class,
        "I WILL ELIMINATE THIS MESSAGE LATER LEAVING IN FOR Debug"
      )
    }

    if(debug){cat( "\n","Just Left if then else setting messages loop, message = ",message," messages = ",messages, "  compound = ", compound, "  class= ",class,"\n")}

    # Append the message to the vector
    messages <- c(messages, message)
    message <-NULL
  }
  if(debug){cat ("\n about to return with messages = ",messages,"\n")}
  return(messages)
}


# Call the function
report_messages <- generate_report(subject, testResults.bigWithClass)

# Print the messages
#cat(paste0("* ", report_messages, collapse = "\n* "), "\n")

# Print the messages
for (message in report_messages) {
  cat("* ", message, "\n")
}





### FIRST CLEAR OUT OTHER VARIABLES
message<-NULL
messages<-NULL




# LET'S SET ALL RESULTS = 1... then NOTHING should print I think
testResults.bigWithClass$Result <- 1
generate_report("AAG0FDC", testResults.bigWithClass)   #

# LET'S SET ALL RESULTS = 0... then NOTHING should print I think
testResults.bigWithClass$Result <- 0
generate_report("AAG0FDC", testResults.bigWithClass)   #


testResults.bigWithClass$Result <- sample(100, size = nrow(testResults.bigWithClass), replace = TRUE)
generate_report("AAG0FDC", testResults.bigWithClass)   #



#=====================================

debug=FALSE
debug2=FALSE


#### ok... try setting EVERYTHING for EVERYBODY to RANDOM and then set one person   "AAG0FDC" differently
### first set everthing to RANDOM
testResults.bigWithClass$Result <- sample(100, size = nrow(testResults.bigWithClass), replace = TRUE)


### Then set all pesticides for AAG0FDC to 300 but set everyone elses first to zero
testResults.bigWithClass$Result[testResults.bigWithClass$classification == 'Pesticides'] <- 0 # EVERYONE gets a zero pesticidef
testResults.bigWithClass$Result[testResults.bigWithClass$SampleNumber ==  "AAG0FDC" & testResults.bigWithClass$classification == 'Pesticides'] <- 300

#Then set all Flame Retardants for AAG0FDC to ZERO
### BUT BUT BUT this needs to set all compounds that are flame retardants to zero EVEN  if they also are other classifications
# Step 1: Set Result to zero for Flame Retardant classification for AAG0FDC
testResults.bigWithClass$Result[testResults.bigWithClass$SampleNumber == "AAG0FDC" &
                                  testResults.bigWithClass$classification == 'Flame Retardant'] <- 0
# Step 2: Identify all ParameterIDs for the above condition
flame_retardant_param_ids <- unique(testResults.bigWithClass$ParameterID[testResults.bigWithClass$SampleNumber == "AAG0FDC" &
                                                                           testResults.bigWithClass$classification == 'Flame Retardant'])
# Step 3: Set Result to zero for rows with SampleNumber AAG0FDC and identified ParameterIDs
testResults.bigWithClass$Result[testResults.bigWithClass$SampleNumber == "AAG0FDC" &
                                  testResults.bigWithClass$ParameterID %in% flame_retardant_param_ids] <- 0







#Then set all VOCs for AAG0FDC to 80
testResults.bigWithClass$Result[testResults.bigWithClass$SampleNumber ==  "AAG0FDC" & testResults.bigWithClass$classification == "Volatile Organic Compounds (VOCs)" ] <- 80

generate_report("AAG0FDC", testResults.bigWithClass)   #


str(testResults.bigWithClass)
