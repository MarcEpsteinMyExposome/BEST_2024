##
## The customer output function uses the data in testResults, class_L, and masterParam to write out
##    various CSV files which have the results of the data run.
##
##    NOTE this relys on various global variables that talk about file and path names
##      file path stuff is messy code, i switched to using "here/here" a bit but not fully.... fix someday
##
#' Output Customer Data Files
#'
#' Writes out various CSV files with test results and metadata for customer use.
#'
#' @param testResults Data frame. The test results data.
#' @param class_L Data frame. Classification lookup table.
#' @param masterParam Data frame. Master parameter table with chemical metadata.
#' @param DataFile_and_OutputFile_Prepend Character. Prefix for output file names.
#' @param DartmouthFixup Logical. Whether to apply Dartmouth-specific formatting.
#' @param output_directory Character. Directory where output files should be saved.
#'
#' @return NULL. Produces files on disk as a side effect.
#'
#' @examples
#' \dontrun{
#' customer_Output(testResults, class_L, masterParam, "UCSF", FALSE, "results_output")
#' }
#'
customer_Output <-
  function(testResults,
           class_L,
           masterParam,
           DataFile_and_OutputFile_Prepend,
           DartmouthFixup,
           output_directory = NULL) {
    # Setup output directory
    if (is.null(output_directory)) {
      output_directory <- here::here("results_output", DataFile_and_OutputFile_Prepend)
    }

    if (!DartmouthFixup) {
      ## NOT doing dartmouth special format

      results_W_CustName <- testResults %>%
        select(ParameterName, CASNumber, PureSampleName, Result) %>%
        spread(PureSampleName, Result, fill = 0) %>%  # Convert LONG table to WIDE for printing/analysis
        dplyr::rename(Chemical = ParameterName, CASRN = CASNumber) %>%
        mutate(rsum = rowSums(across(where(is.numeric)))) %>%  # Dynamically select all numeric columns
        filter(rsum > 0) %>%
        select(-rsum)  # Drop the temporary column

      # Write CSV in R of _ouput in their format
      result_OutputFile <- here::here(output_directory, "ResultsOutputFile.csv")

      write.csv(results_W_CustName,
                file = result_OutputFile,
                row.names = FALSE)

      ###############################################################################################
      # NOW WRITE OUT LONG FORM OF THIS OUTPUT: With Raw and Modified versions of data
      #Write CSV of full testResults including RAW and not-raw data
      ## this writes out too much info and it is in LONG non-matrix format
      # THIS was to write out the RAW info for dartmouth
      if (!("ResultOriginal" %in% colnames(testResults))){   # PRobably i fixed this elsewhere and this line is unnedded but it doesn't hurt
        testResults$ResultOriginal <- testResults$Result
      }
      testResults_with_Raw <- testResults %>%
        select(PureSampleName,ResultOriginal,Result,ParameterName,CASNumber,SampleNumber)

      # Set up the file name and location to write the raw data out into
      results_Raw_and_Modified_full_and_long <- here::here(output_directory, "results_Raw_and_Modified_full_and_long.csv")

      write.csv(
        testResults_with_Raw,
        file = results_Raw_and_Modified_full_and_long,
        row.names = FALSE
      )
      rm(testResults_with_Raw)
      ###############################################################################################

      # Create, then write out, Lookup table converting one set of names to MyExposome names
      #create table of unique PureSampleName (i.e. EDF names),and SampleNumber (i.e. OSU names)
      lookupTable <- testResults %>%
        select(PureSampleName, SampleNumber) %>%
        unique() %>%    #Find only the PAIRS of our-name / cust-name
        arrange(SampleNumber) %>%
        dplyr::rename(Customer_Name = PureSampleName, MyExposome_Name = SampleNumber) #change column names for easier reading
      #Write that table out
      write.csv(
        lookupTable,
        file = here::here(output_directory, "CustomerLookupTable.csv"),
        row.names = FALSE
      )
      rm(lookupTable, result_OutputFile)
      #
      # NOW print out CLASSIFICATIONS of every chem FOUND
      class_W <-
        bind_cols(class_L, value = rep(1, nrow(class_L))) %>%  # add a ONES column
        inner_join(masterParam, by = "ParameterID") %>%
        semi_join(testResults %>% filter(Result > 0)   #Get rid of all Zero Rows in testResults
                  , by = "ParameterID") %>%  #semi_Join only picks
        #the valuese from the left that match valuese on the right taking NOTHING from the right
        select(ParameterName, CASNumber,  classification, value) %>% #pick only 3 columns
        spread(classification, value, fill = 0) %>%  # spread to leave the rows be ParameterName, columns pulled from classiciation, filled w/ value
        arrange(ParameterName) %>%
        rename('Compound Name' = ParameterName)

      #Write that table out
      write.csv(
        class_W,
        file = here::here(output_directory, "ChemicalClassifications.csv"),
        row.names = FALSE
      )
    }

    if (DartmouthFixup) {
      ## DOING dartmouth
      #
      results_W_CustNameSampleNumber <- testResults %>%
        select(ParameterName, CASNumber, SampleNumber, Result) %>%
        spread(SampleNumber, Result, fill = 0) %>%   #Convert LONG table to WIDE for printing/analysis
        dplyr::rename(Chemical = ParameterName, CASRN = CASNumber) %>%
        mutate(rsum = rowSums(.[3:ncol(.)])) %>%  #Create temp sum to allow Delete rows (chemicals) where no WB had that chem
        #  NOTE:  This whole thing creating RSUM may not be  needed cause with DRS we only get positive hits BUT
        #       MAYBE for some other non-DRS methods we get some EVERY crosstab of tested-for-chem for each band
        #       in which case we would get WILD LONG output and that ain't best plan
        filter(rsum > 0) %>%
        select(-rsum)
      #
      results_W_CustNameSampleNumber_Transposed <- testResults %>%
        #mutate(CASNumber_Chemical=paste0("CASN_",CASNumber,"_",ParameterName)) %>%   #THIS line creates TOO LONG a column name
        mutate(CASNumber_Chemical = paste0("M_", ParameterID, "_", gsub("-", "_", CASNumber))) %>%   #THIS line is shorter column name BUT uses "ParameterID" in name
        select(CASNumber_Chemical, SampleNumber, Result) %>%
        spread(CASNumber_Chemical, Result, fill = 0)   #Convert LONG table to WIDE for printing/analysis BUT columns are chemicals, rows are wristbands

      # Write CSV in R of _ouput in their format
      result_OutputFileSampleNumber <- here::here(output_directory, "ResultsOutputFileSampleNumber.csv")

      write.csv(results_W_CustNameSampleNumber,
                file = result_OutputFileSampleNumber,
                row.names = FALSE)

      # Write CSV in R of _ouput in their format
      results_OutputFileSampleNumber_Transposed <- here::here(output_directory, "ResultsOutputFileSampleNumber_Transposed.csv")

      write.csv(
        results_W_CustNameSampleNumber_Transposed,
        file = results_OutputFileSampleNumber_Transposed,
        row.names = FALSE
      )
      #
      #Write CSV of full testResults including RAW and not-raw data
      ## this writes out too much info and it is in LONG non-matrix format
      # THIS was to write out the RAW info for dartmouth
      testResults_with_Raw <- testResults %>%
        select(PureSampleName,ResultOriginal,Result,ParameterName,CASNumber,Customer_WB_id,SampleNumber,Start_Wearing,End_Wearing,Wristband_Size,Days_worn,size_factor,week_factor,Lab_Submission_Batch,Customer_Batch_Number)

      # Set up the file name and location to write the raw data out into
      results_Raw_and_Modified_full_and_long <- here::here(output_directory, "results_Raw_and_Modified_full_and_long.csv")

      write.csv(
        testResults_with_Raw,
        file = results_Raw_and_Modified_full_and_long,
        row.names = FALSE
      )
      rm(testResults_with_Raw)
      #

      #
      #
      # Do special lookup table for Dartmouth CUSTOMER INFO
      #     ###Also need to include
      # UNCLEAR: the date the analysis is completed = ReportDate/Time/////5

      lookupTable <- testResults %>%
        select(
          PureSampleName,
          SampleNumber,
          Customer_WB_id,
          MyE_Received,
          Start_Wearing,
          End_Wearing,
          Days_worn,
          Wristband_Size,
          Lab_Submission_Batch
        ) %>%
        unique() %>%    #Find only the PAIRS of our-name / cust-name
        arrange(SampleNumber) %>%
        dplyr::rename(Customer_Name = PureSampleName, MyExposome_Name = SampleNumber) #change column names for easier reading
      #Write that table out
      write.csv(
        lookupTable,
        file = here::here(output_directory, "Dartmouth_CustomerLookupTable.csv"),
        row.names = FALSE
      )
      class_W_Dartmouth <-
        bind_cols(class_L, value = rep(1, nrow(class_L))) %>%  # add a ONES column
        inner_join(masterParam, by = "ParameterID") %>%
        semi_join(testResults %>% filter(Result > 0)   #Get rid of all Zero Rows in testResults
                  , by = "ParameterID") %>%  #semi_Join only picks
        #the valuese from the left that match valuese on the right taking NOTHING from the right
        mutate(UNIQUE_ID__CASN = paste0("M_", ParameterID, "_", gsub("-", "_", CASNumber))) %>%   #THIS line is shorter column name BUT uses "ParameterID" in name
        select(UNIQUE_ID__CASN,
               ParameterName,
               CASNumber,
               classification,
               value) %>% #pick only 3 columns
        spread(classification, value, fill = 0) %>%  # spread to leave the rows be ParameterName, columns pulled from classiciation, filled w/ value
        arrange(ParameterName) %>%
        rename('Compound Name' = ParameterName)


      #Write that table out
      write.csv(
        class_W_Dartmouth,
        file = here::here(output_directory, "Dartmouth_Classification_Table.csv"),
        row.names = FALSE
      )
    }

    # Return NULL since this function is used for its side effects
    return(NULL)
  }
