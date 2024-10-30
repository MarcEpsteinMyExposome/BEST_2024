suppressMessages(library("tidyverse"))

# Trying ti figure out what I just got from Mike on 11/18/2022
drsMasterParamTableName <- "./data/MASV_drs_parameter_list_3-8-19_Date_Fixed_BadCharacter_at_303234_fixed_more_dates_fixed.csv" # UPdated this on 9/22/2022

mikedrsMasterParamTableName <- "./data/New_Files_From_OSU/all_parameters11-17-2022.csv" # UPdated this on 9/22/2022

drs_masterParam <- read.table(
  drsMasterParamTableName,
  sep = ",",
  header = TRUE,
  colClasses = "character" # Import all as character
  ,
  comment.char = "" # Don't allow use of "#" as comment in input
  ,
  quote = "\"",
  fileEncoding = "UTF-8-BOM"
)

mike_drs_masterParam <- read.table(
  mikedrsMasterParamTableName,
  sep = ",",
  header = TRUE,
  colClasses = "character" # Import all as character
  ,
  comment.char = "" # Don't allow use of "#" as comment in input
  ,
  quote = "\"",
  fileEncoding = "UTF-8-BOM"
)

#what is in both tables?
inters<-as_tibble(intersect(drs_masterParam$ParameterID,mike_drs_masterParam$ParameterID))
ParamID_in_mike_full_list<-as_tibble(mike_drs_masterParam$ParameterID)
ParamID_in_drsOrig<-as_tibble(drs_masterParam$ParameterID)

u_inters<-unique(inters)
u_ParamID_in_drsOrig<-unique(ParamID_in_drsOrig)


sd<- setdiff(u_ParamID_in_drsOrig,u_inters)

write.csv(u_inters, file = "./data/New_Files_From_OSU/u_inters.csv", row.names = FALSE)
write.csv(u_ParamID_in_drsOrig, file = "./data/New_Files_From_OSU/u_ParamID_in_drsOrig.csv", row.names = FALSE)
# Compare in excel two files above and find that 300302 is NOT FOUND in new full list but is found in old 1530 list???
# 300302 Naphthanthrone 3074-00-8 6H-Benzo[c,d]pyren-6-one / Benzo(cd)pyrenone  C19H10O 254.29
## SO... Naphthanthrone USED TO BE in 1530 screen but isn't anymore??
#BUT I NOW SEE... it has a CHANGED PARAMETER_ID ???
#        300419 Naphthanthrone 3074-00-8 6H-Benzo[c,d]pyren-6-one          C19H10O         254.282

# CHECK that napthanthrone is here soemhwre but just has a changed parameter ID
mike_drs_masterParam %>%
  filter(ParameterName=="Naphthanthrone")
#ParameterID  ParameterName CasNumber            AlternateName                    MolecularFormula  MolecularWeight
#1      300419 Naphthanthrone 3074-00-8           6H-Benzo[c,d]pyren-6-one        C19H10O           254.282

drs_masterParam %>%
  filter(ParameterName=="Naphthanthrone")
#ParameterID  ParameterName   CASNumber                                AlternateName                                          MolecularFormula  MolecularWeight
#1      300302 Naphthanthrone 3074-00-8                                 6H-Benzo[c,d]pyren-6-one / Benzo(cd)pyrenone          C19H10O           254.29



# SO what i need from MIKE:

#LIST of 1530 parameter IDS that are in DRS method.skeleton(
#CONFIRMATION that only DRS method ships WIHOUT very chemical tested.... so for that I need that...
# UPDATE on any new CAS Numbers you find


#### THEN I NEED TO DO THIS:
# FIX any CAS NUMbERS in whatever the master database is.... manually OR BETTER with some CODE that just does the tweaks I've identified elsewhere..
  ### make a new field called BETTER CASN and another field called FINaL CASN.... and...

## SO GOING FORWARD use the NEW 1530 which has 300419

