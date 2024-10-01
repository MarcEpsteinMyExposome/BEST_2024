##
## The customer output function uses the data in testResults, class_L, and masterParam to write out
##    various CSV files which have the results of the data run.
##
customer_Output <-
  function(testResults ,
           class_L,
           masterParam,
           DataFile_and_OutputFile_Prepend,
           DartmouthFixup) {

        # SETUP text string(s) to point to correct place to put output directory

    if (!exists("output_directory")) {
      output_directory <- "results_output/"
    }

    if (!DartmouthFixup) {
      ## NOT doing dartmouth special format
      results_W_CustName <- testResults %>%
        select(ParameterName, CASNumber, PureSampleName, Result) %>%
        spread(PureSampleName, Result, fill = 0) %>%   #Convert LONG table to WIDE for printing/analysis
        dplyr::rename(Chemical = ParameterName, CASRN = CASNumber) %>%
        mutate(rsum = rowSums(.[3:ncol(.)])) %>%  #Create temp sum to allow Delete rows (chemicals) where no WB had that chem
        filter(rsum > 0) %>%
        select(-rsum)
      # Write CSV in R of _ouput in their format
      if (!exists("result_OutputFile")) {
        result_OutputFile <-
          paste0(output_directory,
                 DataFile_and_OutputFile_Prepend,
                 "ResultsOutputFile.csv")
      }
      write.csv(results_W_CustName,
                file = result_OutputFile,
                row.names = FALSE)
      ###############################################################################################
      # NOW WRITE OUT LONG FORM OF THIS OUTPUT: With Raw and Modified versions of data
      #Write CSV of full testResults including RAW and not-raw data
      ## this writes out too much info and it is in LONG non-matrix format
      # THIS was to write out the RAW info for dartmouth
      testResults_with_Raw <- testResults %>%
        select(PureSampleName,ResultOriginal,Result,ParameterName,CASNumber,SampleNumber)
      if (!exists("results_Raw_and_Modified_full_and_long")) { ## SET UP the file name and location to write the raw data out into
        results_Raw_and_Modified_full_and_long <-
          paste0(
            output_directory,
            DataFile_and_OutputFile_Prepend,
            "results_Raw_and_Modified_full_and_long.csv"
          )
      }
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
        file = paste0(
          output_directory,
          DataFile_and_OutputFile_Prepend,
          "CustomerLookupTable.csv"
        ),
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
        file = paste0(
          output_directory,
          DataFile_and_OutputFile_Prepend,
          "ChemicalClassifications.csv"
        ),
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
      if (!exists("result_OutputFileSampleNumber")) {
        result_OutputFileSampleNumber <-
          paste0(
            output_directory,
            DataFile_and_OutputFile_Prepend,
            "ResultsOutputFileSampleNumber.csv"
          )
      }
      write.csv(results_W_CustNameSampleNumber,
                file = result_OutputFileSampleNumber,
                row.names = FALSE)

      # Write CSV in R of _ouput in their format
      if (!exists("results_OutputFileSampleNumber_Transposed")) {
        results_OutputFileSampleNumber_Transposed <-
          paste0(
            output_directory,
            DataFile_and_OutputFile_Prepend,
            "ResultsOutputFileSampleNumber_Transposed.csv"
          )
      }
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
      if (!exists("results_Raw_and_Modified_full_and_long")) { ## SET UP the file name and location to write the raw data out into
        results_Raw_and_Modified_full_and_long <-
          paste0(
            output_directory,
            DataFile_and_OutputFile_Prepend,
            "results_Raw_and_Modified_full_and_long.csv"
          )
      }
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
        file = paste0(
          output_directory,
          DataFile_and_OutputFile_Prepend,
          "Dartmouth_CustomerLookupTable.csv"
        ),
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
        file = paste0(
          output_directory,
          DataFile_and_OutputFile_Prepend,
          "Dartmouth_Classification_Table.csv"
        ),
        row.names = FALSE
      )
      #rm(output_directory)

    }




    # BELOW testing to see if the # of distinct chemical names is same as # of distinct CAS Numbers
    #nCompounds <- testResults %>% select(ParameterName) %>% distinct() %>% summarize(n=n())
    #nCasNumber <-testResults %>% select(CASNumber) %>% distinct() %>% summarize(n=n())
    # TEST if nCompunds <> nCasNumber and if so... then they have difference
    #
    #OR let's see what the max length of ParameterName is
    #  testResults %>% mutate(ParamLen=nchar(ParameterName)) %>% summarize(maxL=max(ParamLen))

    #So what I want to do... is ... create a lookup table that maps ParameterName into changed CASNumbers and then use that lookup-table to update CAS Numbers for some cases
    #testResults %>% select(ParameterName,CASNumber) %>% distinct() %>% arrange(ParameterName) %>% ????
    # BEST answer is just use a unique # (ParameterID maybe with an "M" before it) as a place-holder and add that info to special lookup file.







    # Now eliminate result_file_output but keep the result_file_output_found
  #  rm(
#      results_W_CustName,
      #results_W_CustNameSampleNumber,
      #results_W_CustNameSampleNumber_Transposed,
      #class_W,
      #output_directory
    #)
  }
