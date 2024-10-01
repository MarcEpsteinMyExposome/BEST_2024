
# Experimet withi reading XLSX data to solve Unique ID problem
#
# Try to load all MASV 1530 compounds, then use the COMTOX download to replicate what I already did in EXCEL just to see...
#
# HERE IS CORRECT WAY TO DO IT
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
}
if (!require("readxl")) {
  install.packages("readxl", dependencies = TRUE)
}
if (!require("writexl")) {
  install.packages("writexl", dependencies = TRUE)
}

suppressMessages(library("tidyverse"))
suppressMessages(library("readxl"))
suppressMessages(library("writexl"))

options(dplyr.summarise.inform = F)
#
WRITE_OUT<-FALSE

path_to_data <- "FIX_Unique_ID_Problem"
path_to_input <- paste0(path_to_data,"/input")
path_to_output <- paste0(path_to_data,"/output")

MASV_DRS_1530 <- read_excel(paste0(path_to_input,"/MASV_drs_parameter_list_2022_09_22_v1.xlsx"),)

# Read in XLSX data downloaded from COMTOX based on matching CASRN from Kim
COMTOX_CCD_data<-read_excel(paste0(path_to_input,"/CCD-Batch-Search_PLUS_MASV.xlsm"),"COMTOX")

# Delete rows that have duplicate CASRN
COMTOX_CCD_data<- COMTOX_CCD_data %>%
  filter(FOUND_BY != "Integrated Source CAS-RN - WARNING: Synonym mapped to two or more chemicals")
#
# # HERE ARE ALL THE ONES WHICH WERE propertly looked up in comtox in first pass
COMTOX_not_missing_DTXSID <-COMTOX_CCD_data %>%
  filter(!is.na(DTXSID))


# PROBLEM:  How do we now UPDATE MASV with the data from COMTOX where we have DTXSID
COMTOX_not_missing_DTXSID <- COMTOX_not_missing_DTXSID %>%
  rename(CASNumber=INPUT)

#HERE we create the FIRST MASV subset that will always have DTXSID
MASV_Comtox_1<- MASV_DRS_1530 %>% inner_join(COMTOX_not_missing_DTXSID,by="CASNumber") %>%
  select(ParameterID,ParameterName,CASNumber,AlternateName,MolecularFormula,MolecularWeight,DTXSID_URL,DTXSID,CASRN,INCHIKEY,IUPAC_NAME,SMILES,INCHI_STRING, IRIS_LINK_URL, IRIS_LINK,WIKIPEDIA_ARTICLE_URL,WIKIPEDIA_ARTICLE,BOILING_POINT_DEGC_TEST_PRED,BOILING_POINT_DEGC_OPERA_PRED)

############ SO NOW WE HAVE a MASV_Comtox_1 fixed up based on searching COMTOX for the CASNumber from KIM


# ADD a column to MAS data where we have an Alternate CAS in the AlternateName field
MASV_DRS_1530_with_alt_CAS<- MASV_DRS_1530 %>%
  mutate(CASNumber2= str_extract(AlternateName,"CAS # .*")) %>%
  mutate(CASNumber2= str_sub(CASNumber2,7,-2))

#### NOW MANUAL INSPECTION notice that ONE CHEMICAL was NOT FORMATTED properly in the ALT NAME field in terms of the text "CAS #"
#  That is:  ParrameterID= 301797, chemical alt name set to Diclocymet II (139920-32-4)
# MANUAL PATCH
MASV_DRS_1530_with_alt_CAS <- MASV_DRS_1530_with_alt_CAS %>%
  mutate(CASNumber2 = replace(CASNumber2, ParameterID==301797, "139920-32-4"))
### Another way to do replace above would have just been:
####MASV_DRS_1530[MASV_DRS_1530$ParameterID==32,""]  with proper values

# NOW find all the  ones where the alternat case # is listed in alternate name so i can look them up
MASV_DRS_missing_CAS_in_ALT <- MASV_DRS_1530_with_alt_CAS %>%
  filter (!is.na(CASNumber2))

if (WRITE_OUT) {write_xlsx(MASV_DRS_missing_CAS_in_ALT,paste0(path_to_output,"/MASV_DRS_missing_CAS_in_ALT.xlsx" ) )}
# SO now I have ALTERNATE CASRN that I can look up SOMEWHERE..

# MANUALLY LOOK UP the ones where I just got a new CAS number burried in alternate....
#  LOOK UP in comtox batch by copy/paste CASNumber2 column
#   Get result, save as XLSX download
#
# BECAUSE there are duplicate results, and because MARC is not sure it is safe to leave both... MARC is editing to DELETE the places where we got bad synonums by manually lookoup up andPICKING one
# DELETE: 70124-77-5	Integrated Source CAS-RN - WARNING: Synonym mapped to two or more chemicals	DTXSID00860909	Cyano(3-phenoxyphenyl)methyl (alphaS)-4-(difluoromethoxy)-alpha-(1-methylethyl)benzeneacetate
# DELETE: 51630-58-1	Integrated Source CAS-RN - WARNING: Synonym mapped to two or more chemicals	DTXSID3020621	(R,R)-Fenvalerate	DTXCID201475882	67614-33-9	NYPJDWWKZLNGGM-BJKOFHAPSA-N	(R)-Cyano(3-phenoxyphenyl)methyl (2R)-2-(4-chlorophenyl)-3-methylbutanoate
# DELETE:  136191-64-5	CASRN - WARNING: Synonym mapped to two or more chemicals	DTXSID301339265	Benzoic acid, 2-[(4,6-dimethoxy-2-pyrimidinyl)oxy]-6-[1-(methoxyimino)ethyl]-, methyl ester


#  NOW read and update
# Read in XLSX data downloaded from COMTOX based on matching CASRN from Kim in the ALT field
COMTOX_alt_cas_num_from_alt<-read_excel(paste0(path_to_input,"/CCD-Batch-Search_2022-09-27_01_18_08_Process_MASV_DRS_missing_CAS_in_ALT.xlsm"),"maindata") %>%
  rename(CASNumber=INPUT)


# Delete rows that have duplicate CASRN
COMTOX_alt_cas_num_from_alt<- COMTOX_alt_cas_num_from_alt %>%
  filter(FOUND_BY != "CASRN - WARNING: Synonym mapped to two or more chemicals")
#



#HERE we create the SECOND MASV subset that will always have DTXSID as best as possible
### HERE WE create the SECOND  GROUP of MASV extracts that I'm going to need (has about 52 lines) based on extrating CAS Number fro malternate name???

MASV_Comtox_2<- MASV_DRS_missing_CAS_in_ALT %>% inner_join(COMTOX_alt_cas_num_from_alt,by=c("CASNumber2"="CASNumber")) %>%
  select(ParameterID,ParameterName,CASNumber,CASNumber2,AlternateName,MolecularFormula,MolecularWeight,DTXSID_URL,DTXSID,CASRN,INCHIKEY,IUPAC_NAME,SMILES,INCHI_STRING, IRIS_LINK_URL, IRIS_LINK,WIKIPEDIA_ARTICLE_URL,WIKIPEDIA_ARTICLE,BOILING_POINT_DEGC_TEST_PRED,BOILING_POINT_DEGC_OPERA_PRED)

############ SO NOW WE HAVE a MASV_Comtox_1 fixed up based on searching COMTOX for the CASNumber from KIM
############# and we also have MASV_comtox_2 fixed up based on extracting CAS NUMBER from Alternate Name






# SO HERE i have four important files left:
# MAS_DRS_1530:  Raw KIM data
# MAS_DRS_1530-with_alt_cas:  Raw KIM data with CASNumber2
# COMTOX_CCD_data : Original lookup of 1530 by CASRN in comtox found 1433
# COMTOX_alt_cas_num_from_alt   added 39 unique but actually 42


### NOW let's see what is still missing AND THEN try to look THOSE UP by compound NAME
#
MASV_DRS_still_missing <- MASV_DRS_1530_with_alt_CAS %>%
  filter(!(CASNumber %in% COMTOX_CCD_data$CASRN)) %>%
  filter(is.na(CASNumber2 ))

# Let's Look up any ALTERNATE CHEMICAL NAMES
if (WRITE_OUT) {write_xlsx(MASV_DRS_still_missing,paste0(path_to_output,"/MASV_DRS_still_missing.xlsx" ) )}
# Convert to table, sort on Alternate name, cut/paste names into COMTOX
#
# 2,2',3,3',4,4'-Hexabromodiphenyl ether
# 2,2',3,3',4,5',6,6'-Octabromodiphenyl ether
# 2,2',3,4,5,5',6-Heptabromodiphenyl ether
# 2,2',3,4,5,6-Hexabromodiphenyl ether
# 2,2',3,4,5',6-Hexabromodiphenyl ether
# 2,2',3,4,6'-Pentabromodiphenyl ether
# 2,3,3',4,5,6-Hexabromodiphenyl ether
# 2-Bromodiphenyl ether
# 3,3',4,5,5'-Pentabromodiphenyl ether
# BjAC
# Cis Diallate
#

#  NOW read and update
# Read in XLSX data downloaded from COMTOX based on matching ALT NAME from Kim
COMTOX_alt_NAME_from_altNAME<-read_excel(paste0(path_to_input,"/CCD-Batch-Search_2022-09-27_03_32_19_UseALT_Names_to_LOOKUP.xlsm"),"maindata")
COMTOX_alt_NAME_from_altNAME <- COMTOX_alt_NAME_from_altNAME %>%
  filter(!is.na(DTXSID))

### NOTE THAT in this run we had no SYNONYMNS found so we don't need to find the text to delete  BUT BUT BUT in future might need to do some filter on synonymn

############ SO NOW WE HAVE a MASV_Comtox_1 fixed up based on searching COMTOX for the CASNumber from KIM
############# and we also have MASV_comtox_2 fixed up based on extracting CAS NUMBER from Alternate Name
######## NOW we create the MASV_Comtox_3 where we used the actual ALT NAME from the Alternate Name
MASV_Comtox_3<- MASV_DRS_still_missing %>% inner_join(COMTOX_alt_NAME_from_altNAME,by=c("AlternateName"="INPUT")) %>%
  select(ParameterID,ParameterName,CASNumber,CASNumber2,AlternateName,MolecularFormula,MolecularWeight,DTXSID_URL,DTXSID,CASRN,INCHIKEY,IUPAC_NAME,SMILES,INCHI_STRING, IRIS_LINK_URL, IRIS_LINK,WIKIPEDIA_ARTICLE_URL,WIKIPEDIA_ARTICLE,BOILING_POINT_DEGC_TEST_PRED,BOILING_POINT_DEGC_OPERA_PRED)



####################### COMMENT BELWO TILL I FIGURE OUT IF NEEDED
####################### COMMENT BELWO TILL I FIGURE OUT IF NEEDED
####################### COMMENT BELWO TILL I FIGURE OUT IF NEEDED
# # NOW also delete from MASV_DRS_still_missing  the new ones from from ALT NAME
# MASV_DRS_still_missing2 <- MASV_DRS_1530_with_alt_CAS %>%
#   filter(!(CASNumber %in% COMTOX_CCD_data$CASRN)) %>%
#   filter(is.na(CASNumber2 )) %>%
#   filter(!(AlternateName %in% COMTOX_alt_NAME_from_altNAME$INPUT))
#
#
# write_xlsx(MASV_DRS_still_missing2,paste0(path_to_data,"/MASV_DRS_still_missing2.xlsx" ) )


# NOW use the 11 that STEVEN looked up...
Fixing_BPs_FROM_STEVEN <- read_excel(paste0(path_to_input,"/Fixing BPs_FROM_STEVEN_tweaked.xlsx"),"betterbps")

Fixing_BPs_FROM_STEVEN <- Fixing_BPs_FROM_STEVEN %>%
  rename(ParameterID='Parameter ID') %>%
  select(ParameterID,CASNumberBEST) %>%
  filter(!is.na(ParameterID))

MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS<- MASV_DRS_1530_with_alt_CAS %>%
  left_join(Fixing_BPs_FROM_STEVEN,by='ParameterID')

COMTOX_CASNumberBest_Steven <- read_excel(paste0(path_to_input,"/CCD-Batch-Search_2022-09-27_04_40_56_COMTOX_search_using_STEVEN_CASRN_CASNumberBEST.xlsm"),"maindata")


MASV_Comtox_4<-  MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS  %>% inner_join(COMTOX_CASNumberBest_Steven,by=c("CASNumberBEST"="INPUT")) %>%
  select(ParameterID,ParameterName,CASNumber, CASNumber2,CASNumberBEST,AlternateName,MolecularFormula,MolecularWeight,DTXSID_URL,DTXSID,CASRN,INCHIKEY,IUPAC_NAME,SMILES,INCHI_STRING, IRIS_LINK_URL, IRIS_LINK,WIKIPEDIA_ARTICLE_URL,WIKIPEDIA_ARTICLE,BOILING_POINT_DEGC_TEST_PRED,BOILING_POINT_DEGC_OPERA_PRED)

############ SO NOW WE HAVE a MASV_Comtox_1 fixed up based on searching COMTOX for the CASNumber from KIM
############# and we also have MASV_comtox_2 fixed up based on extracting CAS NUMBER from Alternate Name
######## NOW we create the MASV_Comtox_3 where we used the actual ALT NAME from the Alternate Name
######## NOW we create the MASV_Comtox_4 where we used the actual CASNumberBEST name created/added by Steven


MASV_DRS_still_missing3 <- MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS %>%
  filter(!(CASNumber %in% COMTOX_CCD_data$CASRN)) %>%
  filter(is.na(CASNumber2 )) %>%
  filter(!(AlternateName %in% COMTOX_alt_NAME_from_altNAME$INPUT)) %>%
  filter(is.na(CASNumberBEST))

if (WRITE_OUT) {write_xlsx(MASV_DRS_still_missing3,paste0(path_to_output,"/MASV_DRS_still_missing3.xlsx" ) )}



# SO HERE i have five important files left:
# OLD:  MAS_DRS_1530:  Raw KIM data
# OLD  MAS_DRS_1530-with_alt_cas:  Raw KIM data with CASNumber2
# MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS:  Raw Kim with CASNumber2 plus Steven Fix
# COMTOX_CCD_data : Original lookup of 1530 by CASRN in comtox found 1433
# COMTOX_not_missing_DTXSID   this is the 1433 found
# COMTOX_alt_cas_num_from_alt   added 39 unique but actually 43
# COMTOX_alt_NAME_from_altNAME added 2 more
# MASV_DRS_still_missing3  original 1530 less alt CASRN less CASNumber2 less CASNumberBest and a manual fix

# NOW using Still Missing 3... and pulling the CHEMICALNAMES instead of the CAS numbers...
#
# I searched in COMTOX by the 55 chemical names in the CHEMICAL NAME field (ParameterName)
#
COMTOX_Looking_Up_Chemical_Names_after_Missing3_LookupCHEM_Names <- read_excel(paste0(path_to_input,"/CCD-Batch-Search_2022-09-27_04_18_47_COMTOX search using Chemical Names REmaining in Missing3.xlsm"),"maindata") %>%
  filter(!is.na(DTXSID))

# Delete rows that have duplicate CASRN or rather DUP lookup which in this case is/was NAME
COMTOX_Looking_Up_Chemical_Names_after_Missing3_LookupCHEM_Names<- COMTOX_Looking_Up_Chemical_Names_after_Missing3_LookupCHEM_Names %>%
  filter(FOUND_BY != "Synonym - WARNING: Synonym mapped to two or more chemicals")
#

MASV_Comtox_5<- MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS %>% inner_join(COMTOX_Looking_Up_Chemical_Names_after_Missing3_LookupCHEM_Names,by=c("ParameterName"="INPUT")) %>%
  select(ParameterID,ParameterName,CASNumber,AlternateName,MolecularFormula,MolecularWeight,DTXSID_URL,DTXSID,CASRN,INCHIKEY,IUPAC_NAME,SMILES,INCHI_STRING, IRIS_LINK_URL, IRIS_LINK,WIKIPEDIA_ARTICLE_URL,WIKIPEDIA_ARTICLE,BOILING_POINT_DEGC_TEST_PRED,BOILING_POINT_DEGC_OPERA_PRED)
############ SO NOW WE HAVE a MASV_Comtox_1 fixed up based on searching COMTOX for the CASNumber from KIM
############# and we also have MASV_comtox_2 fixed up based on extracting CAS NUMBER from Alternate Name
######## NOW we create the MASV_Comtox_3 where we used the actual ALT NAME from the Alternate Name
######## NOW we create the MASV_Comtox_4 where we used the actual CASNumberBEST name created/added by Steven
######## NOW we create the MASV_Comtox_5 where we used the actual ParamenterName to look up



MASV_DRS_still_missing4 <- MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS %>%
  filter(!(CASNumber %in% COMTOX_CCD_data$CASRN)) %>%
  filter(is.na(CASNumber2 )) %>%
  filter(!(AlternateName %in% COMTOX_alt_NAME_from_altNAME$INPUT)) %>%
  filter(is.na(CASNumberBEST)) %>%
  filter(!(ParameterName %in% COMTOX_Looking_Up_Chemical_Names_after_Missing3_LookupCHEM_Names$PREFERRED_NAME))

### SO NOW the trick is to look up the remaining from MASV_DRS_still_missing4 in PUBCHEM or elsewhere
if (WRITE_OUT) {write_xlsx(MASV_DRS_still_missing4,paste0(path_to_output,"/MASV_DRS_still_missing4.xlsx" ) )}

#and I did as many manually as i could

#### NOW what I n eed to do is MERGE the various MASV_COMTOX 1 2 3 4 5 (and maybe make a 6 from the ones i did... or just WAIT until steven fixes is better) and THEN update MASV and... see
# COMBINE 1 2 and 3 lookup results
x<-unique(bind_rows(MASV_Comtox_1,MASV_Comtox_2,MASV_Comtox_3))
# GET RID of any that are in 4 cause STEVEN is 4 and those numbers are best
x<-x %>%
  filter(!(x$ParameterID %in% MASV_Comtox_4$ParameterID))
# Clear out values for CASNumber2 if CASNumber=CASNumber2 cause that just causes unnecessary duplicates
x<-x %>%
  mutate(CASNumber2 = ifelse(CASNumber==CASNumber2,NA,CASNumber2))
# Now add in 4 and 5
final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find<- unique( bind_rows(x,MASV_Comtox_4,MASV_Comtox_5))


MASV_MISSING_From_X <- MASV_DRS_1530 %>%
  filter(!(ParameterID %in% final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find$ParameterID))

### SO now write out the ones still missing and give to steven
if (WRITE_OUT) {write_xlsx(MASV_MISSING_From_X,paste0(path_to_output,"/MASV_MISSING_From_X.xlsx" ) ) }


### ALSO are there SOME in BOILING POINT that don't have values?
MASV_no_boiling_point<- final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find %>%
  filter((is.na(BOILING_POINT_DEGC_TEST_PRED) & is.na(BOILING_POINT_DEGC_OPERA_PRED>0)))


if (WRITE_OUT) {write_xlsx(MASV_no_boiling_point,paste0(path_to_output,"/MASV_no_boiling_point.xlsx" ) )}


### TEST if there are any duplicate ParameterID's in table X
n_occur <- data.frame(table(final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find$ParameterID))
#n_occur[n_occur$Freq > 1,]


## TEST to see how KIM CASNumber matches CASRN from COMTOX
mismatch_CAS<- final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find %>%
  filter(CASNumber != CASRN)

mismatch_CAS_Selected_Columns<- mismatch_CAS %>%
  select(CASNumber, CASRN, CASNumber2, CASNumberBEST, ParameterName, AlternateName) %>%
  rename(Kim_CAS=CASNumber, Comtox_CAS=CASRN,KIM_AltName_CAS=CASNumber2,Steven_CAS=CASNumberBEST)

if (WRITE_OUT) {write_xlsx(mismatch_CAS_Selected_Columns,paste0(path_to_output,"mismatch_CAS_Selected_Columns.xlsx" ) )}

rm(x)
rm(Fixing_BPs_FROM_STEVEN)
rm(MASV_Comtox_1,MASV_Comtox_2,MASV_Comtox_3,MASV_Comtox_4,MASV_Comtox_5)
rm(COMTOX_alt_cas_num_from_alt,COMTOX_alt_NAME_from_altNAME,COMTOX_CASNumberBest_Steven,COMTOX_CCD_data,COMTOX_Looking_Up_Chemical_Names_after_Missing3_LookupCHEM_Names,COMTOX_not_missing_DTXSID)
rm(MASV_DRS_still_missing,MASV_DRS_still_missing3,MASV_DRS_still_missing4)
rm(mismatch_CAS,mismatch_CAS_Selected_Columns)
rm(MASV_DRS_1530_with_alt_CAS,MASV_no_boiling_point)
rm(MASV_DRS_1530,MASV_DRS_1530_with_alt_CAS,MASV_DRS_1530_with_alt_CAS_with_StevenBestCAS, MASV_DRS_missing_CAS_in_ALT)
#rm(path_to_data,path_to_input,path_to_output)


###
# LETS MAKE BEST COMBO SO FAR:
MASV_1530_NEW_v1<- bind_rows(final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find,MASV_MISSING_From_X)

rm(final_Full_enhanced_MASV_list_still_missing_some_cause_cant_find,MASV_MISSING_From_X)


################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################

# NOW I'm going to check out IARC  and maybe IRIS and maybe 65
# FIRST i need to get full MASV 1530 list:
#
# Take new MASV list, use CASRN so fix it up to be equal CASNumber if.... CASRN is NA
MASV_1530_NEW_v1 <- MASV_1530_NEW_v1 %>%
  mutate(CASRN = ifelse(is.na(CASRN),CASNumber,CASRN)) %>%
  mutate(CASRN= ifelse(is.na(CASRN),"N/A",CASRN))

IARC <- read_excel(paste0(path_to_data,"/Agents Classified by the IARC Monographs, Volumes 1–132_tweak_col_headings.xlsm"),"Sheet1")
MASV_1530_IARC_hits<- inner_join(MASV_1530_NEW_v1,IARC,by="CASRN") %>%
  arrange(ParameterID)

### NOTE the LOOKKUP FILE from OSU has entries for aLL their chemicals NOT just 1530 MASV so we'll need to redo this carefully for other tests
### NOTE the LOOKKUP FILE from OSU has entries for aLL their chemicals NOT just 1530 MASV so we'll need to redo this carefully for other tests
### NOTE the LOOKKUP FILE from OSU has entries for aLL their chemicals NOT just 1530 MASV so we'll need to redo this carefully for other tests

# SO in places where we have DUPLICATE PARAMETER ID ??? we have blanks?
#   This is caused by IARC having two entries for same item and then OSU and MyExposoem end up w/a  blank line which is stupid but leave for now

# Let's see how many elements in KIM list of IARC:
OSU_IARC <- read_excel(paste0(path_to_data,"/MASV15_who_iarc_risk.xlsm"))

# CUT OUT any where ParameterID not in MASV
OSU_IARC_cut <- OSU_IARC %>%
  filter((ParameterID %in% MASV_1530_NEW_v1$ParameterID))

MASV_1530_IARC_hits_missing_from_OSU_list <- MASV_1530_NEW_v1 %>%
  filter(ParameterID %in% setdiff(MASV_1530_IARC_hits$ParameterID,OSU_IARC_cut$ParameterID)) %>%
  left_join(IARC,by="CASRN")

## SO THERE ARE 5 compounds LISTED by IARC but not listed by OSU
## SO THERE ARE 5 compounds LISTED by IARC but not listed by OSU
## SO THERE ARE 5 compounds LISTED by IARC but not listed by OSU

###### NOW work on IRIS data
IRIS <- read_excel(paste0(path_to_data,"/iris_downloads_database_export_200608.xlsx"))
MASV_1530_IRIS_hits<- inner_join(MASV_1530_NEW_v1,IRIS,by="CASRN") %>%
  arrange(ParameterID)
# Let's see how many elements in KIM list of IRIS:

OSU_IRIS <- read_excel(paste0(path_to_data,"/MASV15_epa_iris_risk.xlsm"))

# CUT OUT any where ParameterID not in MASV
OSU_IRIS_cut <- OSU_IRIS %>%
  filter((ParameterID %in% MASV_1530_NEW_v1$ParameterID))

MASV_1530_IRIS_hits_missing_from_OSU_list <- MASV_1530_NEW_v1 %>%
  filter(ParameterID %in% setdiff(MASV_1530_IRIS_hits$ParameterID,OSU_IRIS_cut$ParameterID)) %>%
  left_join(IRIS,by="CASRN")

### SO OSU missing 47 observations on IRIS
### SO OSU missing 47 observations on IRIS
### SO OSU missing 47 observations on IRIS

##### NOW let's look at PROP 65 DATA:

PROP65 <- read_excel(paste0(path_to_data,"/p65chemicalslist.xlsm"))
MASV_1530_PROP65_hits<- inner_join(MASV_1530_NEW_v1,PROP65,by="CASRN") %>%
  arrange(ParameterID)
# Let's see how many elements in KIM list of IRIS:

OSU_PROP65 <- read_excel(paste0(path_to_data,"/MASV15_ca_prop65_risk_Marc_Tweak.xlsm"))

# CUT OUT any where ParameterID not in MASV
OSU_PROP65_cut <- OSU_PROP65 %>%
  filter((ParameterID %in% MASV_1530_NEW_v1$ParameterID))

MASV_1530_PROP65_hits_missing_from_OSU_list <- MASV_1530_NEW_v1 %>%
  filter(ParameterID %in% setdiff(MASV_1530_PROP65_hits$ParameterID,OSU_PROP65_cut$ParameterID)) %>%
  left_join(PROP65,by="CASRN")

### SO OSU missing 11 observations on PROP65
### SO OSU missing 11 observations on PROP65
### SO OSU missing 11 observations on PROP65

