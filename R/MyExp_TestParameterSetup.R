###
###
# this will set up all the chemical-test-specific parameters but not have any customer-specific stuff
### NOTE there are commented out customer specific stuff here ... we need to move that code as needed into the customer-specific area
###
###     IDEALLY the test-specific stuff never changes unless the test changes and the customer related stuff
###           can change with every run cause either new customer, new data for existing customer, or new test for existing customer
###  #
#### THESE PARAMETERS are UNIQUE to each TEST TYPE and  It USED TO BE also EACH specific RESULT within that test
  #####     but all customer-specific stuff MOVED to customer section

  # Set all masterParameterTableNames:
  #
if (RMD_type=="FRAGRANCE"){
  rmd_code <- "MyExposome_1527_v6.Rmd"
  URL_of_Chemicals_Tested <- "Will add FRAGRANCE URL to Website.  In mean time, see data report" # USED in printing report   THIS IS WRONG WRONG WRONG
  testName <- "Fragrances Quantitative test "
  testExplanation <- "This project provides a focused screen to identify Fragrances."
  HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???
  allowDifferentParameterCounts <- FALSE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters
  masterParamTableName <- FRAGRANCEmasterParameterTable
} else if (RMD_type == "PHTH") { #
    rmd_code <- "MyExposome_1527_v6.Rmd"
    URL_of_Chemicals_Tested <- "Will add Phtalate URL to Website.  In mean time, see data report" # USED in printing report   THIS IS WRONG WRONG WRONG
    testName <- "Phthalates Quantitative test "
    testExplanation <- "This project provides a focused screen to identify Phthalates."
    HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???
    allowDifferentParameterCounts <- FALSE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters
    masterParamTableName <- PHTHmasterParameterTable

    # resultsTableName <- "F24-21_MyExpoP.O.#258_PHTH_CoA_MULITPLY_ug_by_1000.csv" # PLACEHOLDER for getting Phatlates to work.  Firs data had ug instead of ng so i converted by hand
    # resultsTableName <- "F24-21_MyExpoP.O.#258_PHTH_CoA_MULITPLY_ug_by_1000_fix_ParameterID.csv" #
    #resultsTableName <- "F24-21_MyExpo_P.O.#258_PHTH_CoA II_convert_ug_to_ng.csv" # Firs data had ug instead of ng so i converted by hand.  2nd had wrong ParamterID.
    #subject <- "A240945" # Randome one from Georgetown  BUT THIS IS new georegetown one from PHTH batch... the numbers changed!
    #ExpectedUnits <- "ng/g" ### NOT SURE why have to change this HERE but makes sense it is TEST SPECIFIC and not CUSTOMER SPECIFIC

  } else if (RMD_type == "SBIR_P1_DRS_plus") { #
    rmd_code <- "MyExposome_1527_v6 - SBIR.Rmd" # Set up name of DRS/1528 R Markdown File
    URL_of_Chemicals_Tested <- "SBIR NEEDS TO BE UPDATED -- I DELETED THIS SECTION FROM SBIR REPORT" # USED in printing report   THIS IS WRONG WRONG WRONG
    testName <- "Chemical Analysis of Personal Environmental Exposures "
    testExplanation <- "This project detects chemicals from different groups with a focus on Pesticides, Chemicals in Commerce, PCBs (a type of industrial chemical), Flame Retardants and other chemicals of interest."
    HideClassificationInformation <- FALSE # Set to TRUE for every report EXCEPT DRS and maybe ???
    allowDifferentParameterCounts <- TRUE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters
    masterParamTableName <- SBIR_p1_MasterParamTableName
    #resultsTableName <- "SBIR_longTableHasValuesWithParameterName.csv" # SBIR Phase 1

    # subject<-"SK4000WB"   # this is --> Steven
    # subject<-"SK4002WB"   # this is --> Marc
    #subject <- "SK4003WB" # this is --> Tamara
    # subject<-"SK4004WB"   # this is --> Kevin (or tracey?)
    # subject<-"SK4005WB"   # this is --> George O'Connell
    # subject<-"SK4006WB"   # this is --> Owen Epstein
    # subject<-"SK4007WB"   # this is --> Wendy Hillwalker
    # subject<-"SK4008WB"   # this is --> Marilyn Walker
    # subject<-"SK4009WB"   # this is --> Becs Epstein
    # subject<-"SK4010WB"   # this is --> Ana Sanchez Bachman
    # subject<-"SK4011WB"   # this is --> Ava Goldman
    # subject<-"SK4014WB"   # this is --> Jennifer Egner DARTMOUTH
    # subject<-"SK4016WB"   # this is --> Aimee Johnson DARTMOUTH
    # subject<-"SK4017WB"   # this is --> Cassie Huang EDF
    # subject<-"SK4018WB"   # this is --> Christina (“CJ”) Sivulka EDF
    # subject<-"SK4019WB"   # this is --> Joanna Slaney EDF
    # subject<-"SK4020WB"   # this is --> Mel Biada business contact
    # subject<-"SK4021WB"   # this is --> Peter Bessen business contact
    # subject<-"SK4022WB"   # this is --> Jacquelyn Hagermoser
    # subject<-"SK4023WB"   # this is --> Alix
    # subject<-"SK4024WB"   # this is --> Ben Pl.
    # subject<-"SK4025WB"   # this is --> Lyndsay  (this has 33 compounds)
    # subject<-"SK4026WB"   # this is --> Kathleen
    # subject<-"SK4027WB"   # this is --> Jennifer F.B.
    # subject<-"SK4028WB"   # this is --> Sarah
    # subject<-"SK4029WB"   # this is --> Sydney Evans EWG
    # subject<-"SK4030WB"   # this is --> Tasha Stoiber EWG
    # subject<-"SK4031WB"   # this is --> Tucker
    # subject<-"SK4033WB"   # this is --> Matt Perkins
    # subject<-"SK4034WB"   # this is --> Miles Naughton
  } else if (RMD_type == "VOC") { # THIS IS OLD OLD OLD VOC information... is original (2018/19) VOC list
    URL_of_Chemicals_Tested <- "https://www.myexposome.com/voc" # USED in printing report
    testName <- "VOC Quantitative Analysis"
    testExplanation <- "This project focused on finding Volitile Organic Compounds (VOCs)."
    masterParamTableName <- vocMasterParamTableName
    #resultsTableName <- "Wisconsin_MyExposome_PO_206_VOC_CoA.csv"
    #subject <- "A180359" # PAH this is one of WISCONSIN
    rmd_code <- "MyExposome_1527_v6.Rmd" # Set up name of DRS/1528 R Markdown File
    #                                     (maybe someday make RMD unique to each type of test but using this is base generic)
  } else if (RMD_type == "PAH") { # THESE 4 LINES are for PAH TEST (which includes Miami Firefighters)
    URL_of_Chemicals_Tested <- "https://www.myexposome.com/pah" # USED in printing report
    # testingTypeText<- "Polycyclic Aromatic Hydrocarbons (PAH's)"
    testName <- "PAH Quantitative Analysis"
    testExplanation <- "This project focused on finding Polycyclic Aromatic Hydrocarbons (PAH's)."
    if (Miami_Firefighters_2017) { # OVERRIDE setting of PAH for OLD Miami Firefighter Data
      masterParamTableName <- "MasterParameterTable_PAH_OLD_Firefighters" ##### OLD VERSION only use to recreate firefighter info
    } else {
      masterParamTableName <- pahMasterParameterTable
    }
    # OLD: resultsTableName<-"PahTestData4.csv"  # THIS IS FOR PAH
    # OLD:resultsTableName<-"PahTestData6.csv"  # THIS IS FOR PAH  .. I SHORTER with less data
    # OLD:resultsTableName<-"PahTestData7.csv"  # THIS IS FOR PAH has a J and a U in it for "B"
    # OLD:resultsTableName<-"PahTestData8.csv"  # THIS IS FOR PAH to see if we can do MOST AMOUNT by FAR of anyone
    # OLD:resultsTableName<-"F17-03-Results.csv"  # THIS IS FOR PAH --REAL DATA FROM MIKE (no J values?)
    # OLD:resultsTableName<-"F17-03-Results_HACK_FIX_TO_HAVE_FLAG.csv"  # THIS IS FOR PAH --REAL DATA FROM MIKE --HACKED BY MARC to combine result+FLAG
    # OLD:resultsTableName<-"F17-03-ResultsV2.marcfix.csv"  # THIS IS FOR PAH --REAL miami DATA FROM MIKE --HACKED BY MARC to combine result+FLAG
    # OLD:resultsTableName<- "F17-03-ResultsV2.csv"  #this is PAH, real Miami, WITHOUT the fix to combine result+Flag and without fix of elimn zero data
    # OLD:resultsTableName<-"F17-03-ResultsV2.marcfix_DeleteZEROs.csv" ### THIS IS FOR PAH --REAL miami DATA FROM MIKE --HACKED BY MARC to combine result+FLAG PLUS delete ZERO COLUMN from INPUT DATA
    # OLD:x`resultsTableName<-"F17-03_Fire_Fighters_data.csv"
    # resultsTableName<-"F17-03_June_July_Fire_Fighters_Data_25_52.csv"
    # resultsTableName<-"Wisconsin_PO206_PAH_OSU_MyE_Report.csv"
    # resultsTableName<-"F19-34 MyExposome PO#221 PAHs-MLB.csv" #Wisconin PAH testing 2nd batch
    # resultsTableName<-"F20-08 MyExposome PO225 PAH.csv" #Wisconin PAH testing 3rD batch
    # resultsTableName<-"Revised PO 221 and PO 225 Wisconsin for VOC Update/F19-34 PO#221 PAH.csv" #REVISED 8/27/2020 Wisconin PAH testing 2nd batch   (this is 221 same as old 221)
    # resultsTableName <- "F21-10 MyExposome P.O.#231_PAH.csv" # PAH testing 4th batch
    #resultsTableName <- "F24-07 MyExposome PO 251 CoA report_CSV_PAH.csv" # PAH testing April 2024 Wisconsin

    # subject<-"A180237B"             #PAH fake  # THIS IS FOR PAH
    # subject<-"A170251"             #PAH REAL   # THIS IS FOR PAH and is the BLANK WRISTBAND
    # subject<-"A170277"          ###   #PAH REAL   # THIS IS FOR PAH and is LOADED WITH EVERY CHEMICAL
    # subject<-"A170263"             #PAH REAL   # THIS IS FOR PAH.  Good representiatvie sample from 1st 25
    # subject<-"A170213"             #PAH REAL   # THIS IS FOR PAH.  Good representiatvie sample from 2nd 52
    # subject<-"A170241"             #PAH REAL   # Miami Data BAD READ ON 15 CHEMICALS, compromised wristband...
    # subject<-"A170229"             #PAH REAL   # THIS IS FOR PAH   BEST representative sample from 1st 25
    # rmd_code = "MyExposome_PAH_v2.Rmd"  ###   #Set up name of PAH R Markdown File (unique to each type of test)
    # subject<-"A180359"   #PAH this is one of WISCONSIN
    # subject<-"A191134"  #PAH this is one of WISCONSIN from 2nd batch
    # subject<-"A200555"  #PAH this is one of WISCONSIN from 3rd batch
    # subject <- "A240027" # this is one of WISCONSIN from 5th (april 2024 batch)
  } else if (RMD_type == "VOPAH") { # THIS has undergone weird changes.. NOW IT IS ONLY the VOC only method w/ Solvent and SPE clean
    URL_of_Chemicals_Tested <- "https://www.myexposome.com/vopah" # USED in printing report
    testName <- "VOC Quantitative Test"
    testExplanation <- "This project focused on finding Volatile Organic Compounds (VOCs)."
    HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???  Implementing 6/30/2020
    masterParamTableName <- vopahMasterParamTableName

    # PO 221
    # resultsTableName<-"F19-34 MyExposome PO#221 VOPAH-MLB.csv" ### OLD batch 2
    # resultsTableName<-"F19-34 PO#221 32 VOC report v4.csv"  # THIS IS NEW version of data to be more accurate after OSU equipment failure discovered BATCH 2

    # PO 225
    # #resultsTableName<-"F20-08 MyExposome PO225 VOPAH.csv"  # this is ORIGINAL version of 225, was wrong, was 94 compounds
    # resultsTableName<-"F20-08 PO#225 32 VOC report v4.csv"  # THIS IS NEWER version of data from august 2020 ofter OSU equipment failure discovered BUT WRONG AGAIN is dup of 221
    # resultsTableName<-"F20-08 PO#225 32 VOC report mlb_9-1-2020.csv"  # THIS IS NEWER version of data from august 2020 ofter OSU equipment failure then dup 221, then bad format delivery and maybe now finally fixed
    # resultsTableName<-"MyE_PO225_VOCs_9-2-2020.csv"  # THIS IS NEWEST'est version of data from august 2020 ofter OSU equipment failure then dup 221, then bad format delivery and maybe now finally fixed NOPE and now another release to fix missing 6 data points

    ### PO 226 TWO LINES BELOW are LINKED so be careful
    # resultsTableName<-"Wisconsin_MyExposome_PO_206_VOC_CoA.csv"  # FIRST Wisconsin Data (originally run as VOC but now I'm running as VOPAH cause 32 in VOPAH and 42 in VOC)
    # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

    ###  PO 221 plus PO 225 plus PO 226
    ###  LINES BELOW ARE LINKED SO be careful
    # resultsTableName <- "WI_Combined_1_2_3_PO206_PO221_PO225.csv" # 3 batches Wisconsin Data (1st batch originally run as VOC now combined run all as VOPAH)
    # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE
    # allowDifferentParameterCounts <- FALSE # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

    ###  PO 221 plus PO 225 plus PO 226 plus PO 231
    ###  LINES BELOW ARE LINKED SO be careful
    #resultsTableName <- "WI_VOCPAH+Combined_1_2_3_4_PO206_PO221_PO225_PO231.csv" # 4 batches Wisconsin Data (1st batch originally run as VOC now combined run all as VOPAH)
    # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE
    allowDifferentParameterCounts <- FALSE # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

    # subject<-"A180359"   #PAH this is one of WISCONSIN
    # subject<-"A191134"  #PAH this is one of WISCONSIN from 2nd batch
    # subject<-"A200555"  # this is one of WISCONSIN from 3rd batch
    #subject <- "A210328" # this is one of WISCONSIN from 4rd batch
  } else if (RMD_type == "VOC_2024") { # THIS has undergone weird changes.. is JUST VOC as of 2024
    ###### URL_of_Chemicals_Tested <- "https://www.myexposome.com/vopah" # USED in printing report
    URL_of_Chemicals_Tested <- "Not Yet Up on Website but current full list is in show in Report" # USED in printing report
    testName <- "VOC 2024 Quantitative Test"
    testExplanation <- "This project focused on finding Volatile Organic Compounds (VOCs)."
    HideClassificationInformation <- TRUE # Set to TRUE for every report EXCEPT DRS and maybe ???  Implementing 6/30/2020
    masterParamTableName <- VOC_2024_MasterParamTableName

    # resultsTableName <- "F24-07 MyExposome PO 251 CoA report_CSV_VOC.csv" #  New VOC_2024 Test and data   BUT was missing 3 compounds
    #resultsTableName <- "F24-07 MyExposome PO 251 CoA report_Updated_VOC.csv" #  New VOC_2024 Test and data after ADDING back those 3 compounds-AND THEN a FIX and REDO (first time was bad, now OK)
    # allowDifferentParameterCounts <- TRUE  # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE
    allowDifferentParameterCounts <- FALSE # SPECIFICALLY for PO_206 we are going to ALLOW different parameter counts   but all other VOPAH methods should NOT so set this to FALSE

    #subject <- "A240027" # this is one of WISCONSIN from april 2024 batch
  } else if (RMD_type == "PEST") { # THESE 4 LINES are for PEST TEST
    URL_of_Chemicals_Tested <- "https://www.myexposome.com/pest" # USED in printing report
    testName <- "Pesticide Quantitative Analysis"
    testExplanation <- "This project focused on finding Pesticides."

    masterParamTableName <- pestMasterParameterTable

    # resultsTableName<-"PO_206_Pesticide_OSU_MyE_report.csv"
    # resultsTableName<-"F19-34 MyExposome PO#221 Pesticides-MLB.csv"
    # resultsTableName<-"F20-08 MyExposome PO225 Pesticides.csv"
    # resultsTableName <- "F21-10 MyExposome P.O.#231_PEST.csv"
    # resultsTableName <- "F23-10_MyExpoP.0#245_Pesticide_CoA_Univision.csv" # Univision Pest Info
    # resultsTableName <- "F23-10_MyExpoP.0#245_Pesticide_CoA_UnivisionWEIRD UNIT PROBLEM_Fix_By_Hand.csv" # Univision Pest Info
    # resultsTableName <- "PO245 CSV version redone.csv" # Univision Pest Info  Reissue after having ONE bad datapoint...

    # subject<-"A180359"   #PEST this is one of WISCONSIN
    # subject<-"A191134"  #PAH this is one of WISCONSIN from 2nd batch
    # subject<-"A200555"  #PAH this is one of WISCONSIN from 3rd batch
    # subject <- "A210328" # this is one of WISCONSIN from 4rd batch
    #subject <- "A230476" # Randome one from Univision
  } else if (RMD_type == "FLAME") { # THESE 4 LINES are for FLAME TEST  (NOT firefighters!!!)
    URL_of_Chemicals_Tested <- "https://www.myexposome.com/flame" # USED in printing report
    # testingTypeText<- "Flame Retardants"
    testName <- "Flame Retardant Quantitative Analysis"
    testExplanation <- "This project focused on finding Flame Retardants."

    masterParamTableName <- flameMasterParamTableName
    #resultsTableName <- "TestResultsFlame.csv" # THIS IS FOR Flame?

    #subject <- "B241284" # FLAME example
  } else if (RMD_type == "DRS") { # THESE 4 LINEsE are for 1528/drs test
    URL_of_Chemicals_Tested <- "https://www.myexposome.com/fullscreen" # USED in printing report
    # testingTypeText<- "our broadest spectrum of compounds"
    testName <- "Full Screen Quantitative Analysis"

    testExplanation <- "This project provides a broad screen to identify compounds across many chemical groups with a focus on Personal Care Products, Pesticides, Chemicals in Commerce, Flame Retardants and other chemicals of interest."
    ###  WE COULD LIST THESE HERE:    Consumer & Personal Care Products,  Industrial & Commercial Chemicals (Including VOCs), Agricultural & Pharmaceutical Chemicals, Persistent Organic Pollutants (POPs), Flame Retardants, Polycyclic Aromatic Hydrocarbons (PAHs)

    HideClassificationInformation <- FALSE # Set to TRUE for every report EXCEPT DRS and maybe ???

    allowDifferentParameterCounts <- TRUE # DRS is the only method where the masterParameterTable and the resultsTable will always have different parameter counts cause DRS doesn't list all the zero parameters

    masterParamTableName <- drsMasterParamTableName
    # resultsTableName <- "CSV Format MyE A170185_A170186-6-8-2017_fix2_REDO.csv" #this line is for the TWO from Senator
    # resultsTableName <- "CSV_Format_Data_MyE14_MyE15_fixed_v2_PLUS_38_MORE.csv" #this line is for the TWO from Senator PLUS 38 more EDF + +

    # resultsTableName <- "CSV_Format_Data_MyE14_MyE15_fixed_v2_PLUS_38_MORE_made_random_values.csv" #2 from senator + 38 w/ random values NOT "1"

    # THE ONE BELOW w/ soozie may not work
    # resultsTableName<-"F16-20-MyExposome_EDF12_plus_Soozie.csv"  #THIS LINE is for EDF_new2016 PLUS soozie
    # subject<-"A150196"  # This is random one from EDF
    # subject<-"A161423" # This is EDF Parking Valet
    ######### subject<-"A150201"  # This is random one from EDF

    # resultsTableName<-"F18-09_F18-14_OSU_MyE_Report_LOREAL.csv"
    # resultsTableName<-"Fake_Data_78.csv"  #dartmouth modified version...
    # resultsTableName<-"Rocio-Canada as delivered.csv"  #Rocio Canada Data As Deliver (3 WB)

    # resultsTableName<-"F21-07_MyExposome_P.0.#230_CoA_RYAN.csv"   # group of dartmouth   #NOTE:  is NOW the 13

    # resultsTableName<-"F19-26 MyExposome PO# 218 final_plus_1_from_222_plus_1_old_from_batch216_UCSF.csv"   # First group from UCSF plus one-special-adder (NOT including weird older one special?)
    # resultsTableName<-"F19-13 MyExposome PO216 CSV_Client - UCSF.csv" # ONE USCF wristband for reporter/or influencer
    # subject<-"A190349" #ONE ucsf subject reporter influencer

    # resultsTableName<-"F19-13 MyExposome PO216 CSV_Client - UofBuffalo.csv" # 10 or so wristbands for BUffalo
    # subject<-"A190350" # one subject from BUFFALO   7/1/2019

    # resultsTableName<-"F19-13 MyExposome PO216 CSV_Client _UCSF_plus_Random_10.csv" # 10 or so wristbands for BUffalo
    # resultsTableName<-"Big_Dartmouth_Loreal_Buffalo_UCSF.csv" # One UCSF plus MANY mas15 tests dartmouth plus loreal plus buffalo
    # resultsTableName<-"F21-19_MyExpoP.O.#232_CoA_UMT.csv" # University of Montana 6 wristbands for testing
    # subject<-"A210743"  # RANDOM choice from UMT Montana

    # resultsTableName<-"F21-26_MyExpo PO#233_CoA_12_UNM.csv" # University of NEW MEXICO 12 wristbands
    # subject<-"A211319"  # RANDOM choice from UNM  NEW MEXICO

    # I"M GOING TO Do 2nd batch from UNM Univ New Mexico but NOT MERGE WITH FIRST BATCH.... just do this additional 20
    # resultsTableName<-"F23-09_03_P.O#244_MyExpo_CoA_UNM.csv" # University of NEW MEXICO 20 wristbands
    # subject<-"A230400"  # RANDOM choice from UNM  NEW MEXICO 2nd batch

    # resultsTableName<-"F21-31_MyExpo_P.O#236_CoA_Colorado.csv" # Colorado FIRST BATCH
    # subject<-"A211668" #Randome one from Colorado

    # resultsTableName<-"F23-16_MyExpo_P.O.#247_CoA - Colorado.csv" # Colorado BATCH2
    # resultsTableName<-"F23-16_MyExpo_P.O.#247_CoA - Colorado_fixed_Units.csv" # Colorado BATCH2 WITH FIXED UNITS

    # resultsTableName<-"F21-31_MyExpo_P.O#236_plus_#247_CoA_Colorado_batch1and2.csv" # Colorado BATCH1 and BATCH2
    # subject<-"A231218" #Randome one from Colorado BATcH 2

    # resultsTableName<-"F23-22_MyExposome #249_CoA_France.csv" # FRANCE LILLE
    # subject<-"A232671" #Randome one from LILLE

    ##### MOVED THESE TO THE WHICH_CUSTOMER area above#resultsTableName<-"Dartmouth/Dartmouth 1_2_etc_13_etc_20_21_22_23_24_25_26_for_1166_FixPO246units.csv" # Probably FINAL Dartmouth
    ##### MOVED THESE TO THE WHICH_CUSTOMER area above#subject <- "A200961" # THIS is random one in DARTMOUTH BATCh 13



    # resultsTableName<-"F22-09_MyExpoP.O#238_CoA_UCDavis35.csv" # UC Davis
    # subject<-"A220461" #Randome one from UC Davis  A220453  A220432

    # resultsTableName<-"F21-31_MyExpo_P.O#236_CoA_Chicago.csv" # CHICAGO
    # subject<-"A211685" #Randome one from Chicago

    # resultsTableName<-"F21-32_MyExpo_P.O.#237_CoA_Georgetown.csv" # Georgetown

    # resultsTableName <- "CombinedTestData/Dart_Chic_Col_George.csv" # Georgetown
    # subject<-"A211733" #Randome one from Georgetown

    # subject <- "A170186-DC" # This is My15 from Senator A170186 (DC)

    #  subject <- "A211668" # Randome one from CombinedTEstData

    # resultsTableName<-"F23-10_MyExpoP.O#245_MASV15_CoA_UnivisionFIX.csv" # Univision (first try had wrong units this try sould work?)
    # subject<-"A230476" #Randome one from Univision

    # resultsTableName<-"F23-21_MyExposome P.O.#248_CoA_UCONN.csv" # Connecticut
    # resultsTableName<-"F23-21_MyExpo_P.O.#248_CoA_reissue_ngPerWB_UCONN.csv" # Connecticut reissued with correct UNITS
    # subject<-"A231970" #Random one from CONNECTICUT

    # resultsTableName<-"F24-14_MyExpo_P.O.#253_CoA.csv" # BOSTON
    # subject<-"A240466" #Random one from BOSTON

    # resultsTableName<-"F24-20_MyExpo_PO257_OSU_MyE_Report.csv" # UFL Florida
    # subject<-"A240871" #Random one from UFL Florida

    # resultsTableName<-"F24-05_MyExposomeP.O.#250_CoA_Louisville.csv" #
    # subject<-"A240020" #Random one from Louisville

    ##### MOVED THESE TO THE WHICH_CUSTOMER area above
    ##### MOVED THESE TO THE WHICH_CUSTOMER area above  resultsTableName <- "F24-22_MyExpoP.O.#259_CoA-WBdata.csv" #  NOW DOING SBIR Phase 2 first group of 71
    ##### MOVED THESE TO THE WHICH_CUSTOMER area above  subject <- "A241133" # Random one from SBIR P2 Group 1 of 71
  }

