#THIS CODE IS TO UPDATE THE riskCalifProp65 Database
#  Prop65 California Prop65 Fix

# First download the file from california website... getting : p65chemicalslist.xlsx
# THEN edit the file to remove the big header
#  THEN edit the file to remove stupid lines at the bottom
# THEN RUn th rough th is code MANUALLY STEP BY STEP

#
# Set all key variables.  Use the existance (or not) of "subject" to decide if they need to be loaded
if (!exists("subject")) {
  source(here::here("R","MyExp_set_key_variables.R"))
}
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
if (!exists("masterParam")) {
  source(here::here("R",r_code))
}

# Clean up environment a little
rm(list=ls()[!ls() %in% c("masterParam","riskCalifProp65")])

masterParamWith65<- masterParam %>%
  left_join(riskCalifProp65,by="ParameterID")

newProp65tableName<- "./data/p65chemicalslist_cleaned_marc.csv"
newProp65 <- read.table(
  newProp65tableName,
  sep = ",",
  header = TRUE,
  colClasses = "character" # Import all as character
  ,
  comment.char = "" # Don't allow use of "#" as comment in input
  ,
  quote = "\"",
  fileEncoding = "UTF-8-BOM"
)

#
newProp65 <- newProp65 %>%
  select(CAS.No.,Type.of.Toxicity) %>%
  rename(CASNumber=CAS.No.,toxicityType=Type.of.Toxicity)

# Collapse all rows with identical CAS Numbers
### NOW i want to collapse the table newProp65 in the following way:
# If there are two or more rows with the same CASNumber but different values in toxicityType
newProp65 <- newProp65 %>%
  group_by(CASNumber) %>%
  summarise(toxicityType = paste(unique(toxicityType), collapse = ", ")) %>%
  ungroup()

# Delete any rows from newProp65 that do not match the pattern of number-hyphen-number-hyphen-number (e.g., "202-33-1111" is valid)
newProp65 <- newProp65 %>%
  filter(grepl("^\\d{1,5}-\\d{1,5}-\\d{1,5}$", CASNumber))

# Clean up environment a little
rm(newProp65tableName,masterParam)

#str(masterParamWith65)
#str(newProp65)

#i want to know if there are any rows in masterParamWith65 where toxcitityType is NA but that CASNumber from that row is found in newProp65
# Ensure that the CASNumber columns in newProp65 and masterParamWith65 have consistent data types
newProp65$CASNumber <- as.character(newProp65$CASNumber)
masterParamWith65$CASNumber <- as.character(masterParamWith65$CASNumber)

#FIND ROWS that are in masterParameter that don't have a toxicity but are listed in the new Prop65 database
rows_missing_toxicity <- masterParamWith65 %>%
  filter(is.na(toxicityType) & CASNumber %in% newProp65$CASNumber)

# Output the rows with missing toxicity
# EXAMINE THEMN
rows_missing_toxicity

### HEre is output (without the word BAD) from 10/21/2024
# ParameterID                                      ParameterName CASNumber toxicityType
# 1         247                                         Anthracene  120-12-7         <NA>
#   2         388                                       Chlorpyrifos 2921-88-2         <NA>
#   3         447                                        Dichlorprop  120-36-5         <NA>   BAD
#   6      303286                                        P-toluidine  106-49-0         <NA>    BAD
#   7      303299                             2-Amino-4-chlorophenol   95-85-2         <NA>
#   8      303325                            2-Mercaptobenzothiazole  149-30-4         <NA>


### MANUALLY INSPECT all these new rows in the XSLX to see if they are actually listed BUT DELETED by crossing out on the font.
### THIS IS WEIRD in the data and another way would be to somehow load the XLSX and check if the font is a crossed out type
### but for now do this manually... look up the CASNumber from the list above...
###
# Delete rows from rows_missing_toxicity where the CASNumber is '120-36-5' or '106-49-0'
rows_missing_toxicity <- rows_missing_toxicity %>%
  filter(!(CASNumber %in% c("120-36-5", "106-49-0")))

# EXAMINE THEMN
rows_missing_toxicity


#perfect.  NOW i want to update all the rows in  rows_missing_toxicity with the toxicity value from newProp65 matching on CASNumber
# Update rows in rows_missing_toxicity with the toxicity value from newProp65 matching on CASNumber
rows_missing_toxicity <- rows_missing_toxicity %>%
  left_join(newProp65 %>% select(CASNumber, toxicityType), by = "CASNumber") %>%
  mutate(toxicityType = ifelse(is.na(toxicityType.x), toxicityType.y, toxicityType.x)) %>%
  select(-toxicityType.y, -toxicityType.x)  # Remove temporary columns used for the join

# EXAMINE THEMN
rows_missing_toxicity


## Update masterParamWith65 with the updated rows
#masterParamWith65 <- masterParamWith65 %>%
  #rows_update(rows_missing_toxicity, by = "CASNumber")

### NOTE:  There really was no point in updating the MasterParameterTable because what I need to do based on how the code works
# is update the contents of riskCalifProp65


# Ensure rows_missing_toxicity only has the same two columns as where i need to cut/paste it later (ParameterID and toxicityType)
rows_missing_toxicity <- rows_missing_toxicity %>% select(ParameterID, toxicityType)

# EXAMINE THEMN
rows_missing_toxicity

#Add a middle column  just to make easier to paste the new rows into the look up table

# Add a middle column named 'chemicalLink' in rows_missing_toxicity
rows_missing_toxicity <- rows_missing_toxicity %>%
  mutate(chemicalLink = "")  %>%
  select(ParameterID,chemicalLink,toxicityType)


# EXAMINE THEMN
rows_missing_toxicity

#  I'm writing out the CSV so you can OPEN IT from the data directory and the copy / paste the new rows into the old version and save under new name
write.csv(rows_missing_toxicity, file = "data/add_to_prop65.csv", row.names=FALSE)


# and elsewhere:  GO MANUALLY into the directory.  Open the current riskCalifProp65TableName
#   and then edit it and save it with a new number at the end (like "2" below) as a CSV
#   and then edit in set_key_variables to use that new file
# riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk.csv"   # I edited to change NAMING
#riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk_Marc_Tweak.csv"
#riskCalifProp65TableName <- "./data/MASV15_ca_prop65_risk_Marc_Tweak2.csv"



