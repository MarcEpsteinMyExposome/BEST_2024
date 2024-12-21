# PURPOSE: cross the 112 MyExposome chemicals list with the CA candidate
# chemicals lists and BCRC list (SSI hazard screen approach)
# AUTHOR: Katie Boronow
# STARTED: 2024-08-06
# WRITTEN IN R VERSION: R version 4.3.1 (2023-06-16 ucrt)
# LAST UPDATED: 2024-12-20 shared with MyExposome
# --------------
#install.packages("janitor")





library(tidyverse)
library(janitor)
library(readxl)



rm(list=ls())
if (!exists("subject")) {
  source(here::here("R","MyExp_set_key_variables.R"))
}
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
if (!exists("masterParam")) {
  source(r_code)
}

# Clean up environment a little
rm(list=ls()[!ls() %in% c("masterParam","riskCalifProp65")])



# workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(workingdir)
# setwd("..")
# getwd()


####
## Import and Clean CA Candidate Chemicals list---------------------------------
###

# import CA Candidate Chemicals list
# download from: https://calsafer.dtsc.ca.gov/cms/search/?type=Chemical
calSAFERfile<-here::here("data","CalSAFER_CandidateChemicals_2024-12-20.xlsm")

#cclist <- read_xlsx("CalSAFER_CandidateChemicals_2024-06-05.xlsx", skip = 7)
cclist <- read_xlsx(calSAFERfile)

cclist2 <- cclist %>%
  clean_names()

# remove the exposure indicators lists (not relevant to hazard)
# remove Canada PBiTs because T for environment
# remove "non-authoritative" lists (DTSC hazard traits)

list_remove <- c("CA NLs", "CA MCLs", "CA TACs", "CWA 303(c)", "CWA 303(d)",
                 "OEHHA RELs", "CECBP - Priority Chemicals",
                 "OSPAR Priority Action Part A",
                 "CDC 4th National Exposure Report", "Canada PBiTs",
                 "Hazard Traits identified by DTSC")

cclist3 <- cclist2 %>%
  filter(!(authoritative_list %in% list_remove))

# what lists ARE we using?
unique(cclist3$authoritative_list)
cclist3 %>%
  select(authoritative_list, hazard_traits) %>%
  distinct() %>%
  arrange(hazard_traits) %>%
  print(n = 37)

# remove extra columns
cclist4 <- cclist3 %>%
  select(-date_last_modified)  # %>%
  #select(-see_details)   ## NOTE NOTE NOTE we probably want to KEEP THIS or add it back later...

# number of rows per list
table(cclist4$authoritative_list)

ggplot(cclist4) +
  geom_bar(aes(authoritative_list)) +
  coord_flip() +
  xlab("") +
  theme_bw()

# number of unique chemicals
cclist4 %>%
  select(chemical_name, cas_rn) %>%
  distinct() %>%
  nrow()

####
## Assigning hazard endpoints---------------------------------------------------
###

# Identify by major hazard endpoint:
# Cancer
# Reprotox
# Devotox
# Neurotox
# Genotox
# Respiratory
# Endocrine
# also create a column with PBT

cclist4 %>%
  pull(hazard_traits) %>%
  unique()

cclist4 %>%
  filter(str_detect(authoritative_list, "PBT")) %>%
  pull(hazard_traits) %>%
  unique()

# make a new health_endpt column where PBT hazard traits (see above) all
# condensed under "PBT"
# consider - what about "Toxicity Undefined"

cclist5 <- cclist4 %>%
  mutate(health_endpt = ifelse(str_detect(authoritative_list, "PBT"),
                               "PBT", hazard_traits))

cclist5 %>%
  select(authoritative_list, health_endpt) %>%
  distinct() %>%
  arrange(health_endpt) %>%
  print(n = 28)

table(cclist5$health_endpt) # this is double counting some chemicals

# remove duplicated hazards (from multiple lists)
# clean cas_rn format

cclist_health <- cclist5 %>%
  select(chemical_name, cas_rn, health_endpt) %>%
  distinct() %>%
  mutate(cas_rn = str_replace(cas_rn, "'",""))

cclist_lookup_url <- cclist5 %>%
  select(cas_rn,see_details)%>%
  distinct() %>%
  filter(!cas_rn=="No CAS RN")

# WRITE this out where it can be read with setMASTERPARAM_CLASS_RISKSdirectory("myexp_hazardscreenMP5_SPECIFIC DATE INFO.csv")
write_csv(cclist_lookup_url,
          here::here("data","data_MasterParams_Class_Risks",paste0("myexp_hazard_lookup_url", Sys.Date(), ".csv")))
#myexp_hazard_lookup_url2024-12-20.csv

####
## Bring in more lists ---------------------------------------------------------
###

# bring in formaldehyde releasers
# MARC- this SSI list is not published yet, so for now I've commented it out of
# the script. as I mentioned before, there were no hits on it in the 112 list.

# formrel <- read_xlsx("formaldehyde_releasers.xlsx") %>%
#   clean_names() %>%
#   mutate(cas_registry_number = str_replace(cas_registry_number, "CAS ", ""),
#          health_endpt = "Formaldehyde releaser") %>%
#   rename(chemical_name = commonly_used_name,
#          cas_rn = cas_registry_number)


# pull in the SSI breast cancer-relevant chemicals (BCRC) list

# https://ehp.niehs.nih.gov/doi/full/10.1289/EHP13233#supplementary-materials
# Tab "Excel Table S1" in file "ehp13233.s002.codeanddata.acco.zip" in the
##MANUALLY:  get the whole file, save only that one tab, delete the first row cause it unnecessary
# supplementary material
# Direct download URL: https://ehp.niehs.nih.gov/doi/suppl/10.1289/EHP13233/suppl_file/ehp13233.s002.codeanddata.acco.zip

bcrcs <- read_csv(here::here("data","BCRClist.csv"),show_col_types = FALSE)

# keep only mcs and edc+s MC = rodent mammary carcinogens, EDC+ = endocrine-disrupting compounds

bcrcs2 <- bcrcs %>%
  filter(MammaryTumorEvidence == "MC" | EDC == "EDC+") %>%
  select(CASRN, preferred_name, MammaryTumorEvidence, EDC) %>%
  pivot_longer(c(MammaryTumorEvidence, EDC),
               names_to = "name",
               values_to = "health_endpt") %>%
  filter(health_endpt == "MC" | health_endpt == "EDC+") %>%
  mutate(health_endpt = paste0("BCRC_", health_endpt)) %>%
  select(-name) %>%
  rename(chemical_name = preferred_name,
         cas_rn = CASRN)

# combine all hazard lists
# remove chemicals with no CAS number

hazlists <- bind_rows(cclist_health,
                      # formrel,
                      bcrcs2) %>%
  filter(cas_rn != "No CAS RN",
         cas_rn != "") %>%
  select(-chemical_name) %>%
  mutate(value = 1) %>%
  distinct() %>%
  pivot_wider(names_from = health_endpt,
              values_from = value,
              values_fill = 0)

# need the extra distinct above because of 5 cas_rns that have multiple
# chemical names associated
cclist_health %>%
  select(cas_rn, chemical_name) %>%
  filter(cas_rn != "No CAS RN",
         cas_rn != "") %>%
  distinct() %>%
  group_by(cas_rn) %>%
  count() %>%
  arrange(desc(n))


# get MyExp chem list ###################################################### THIS IS THE 112 1112 112
# THIS IS IN An xlsx with this info:
#   CAS	chem	class
# 115-86-6	triphenyl phosphate	FLAME
# 85-01-8	phenanthrene	PAH
# 13674-84-5	tris(1-chloro-isopropyl) phosphate	FLAME


list <- read_xlsx(here::here("data","112 Compound Mini-Screen To Share.xlsx"),
                  sheet = "Sheet1")

list2 <- list %>%
  mutate(chemical_name = tolower(chem)) %>%
  rename(cas_rn = CAS) %>%
  select(-chem)

# join MyExp list and hazard list

myexp_hazards <- left_join(list2, hazlists, by = "cas_rn") %>%
  clean_names() %>%
  rename(bcrc_edc_plus = bcrc_edc) %>%
  mutate(across(matches("city|bcrc|pbt|form"),
                ~as.numeric(str_replace_na(., replacement = "0")))) %>%
  mutate(tot_haz = select(., matches("city|bcrc|pbt|form")) %>%
           rowSums())

# subset fragrance chemicals from other PPCP chemicals
fragrance <- c("alpha-ionone", "amyl cinnamal", "beta-citronellol",
               "beta-ionone", "cinnamal", "coumarin",
               "ethylene brassylate", "exaltolide", "galaxolide",
               "lilial", "linalool", "tonalide", "trans-citral")

myexp_hazards2 <- myexp_hazards %>%
  mutate(class = case_when(
    chemical_name %in% fragrance ~ "FRAGRANCE",
    TRUE ~ class)) %>%
  arrange(class, desc(tot_haz))

# write_csv(myexp_hazards2,
#           paste0("myexp_hazardscreen_", Sys.Date(), ".csv"))

##===========NOW do the MyExposome FULL list instead of just the 112 list


list2MP<- masterParam %>%
  rename(cas_rn=CASNumber, chemical_name=ParameterName) %>%
  select(cas_rn,chemical_name)

# join MyExp list and hazard list

myexp_hazardsMP <- left_join(list2MP, hazlists, by = "cas_rn") %>%
  clean_names() %>%
  rename(bcrc_edc_plus = bcrc_edc) %>%
  mutate(across(matches("city|bcrc|pbt|form"),
                ~as.numeric(str_replace_na(., replacement = "0")))) %>%
  mutate(tot_haz = select(., matches("city|bcrc|pbt|form")) %>%
           rowSums())

# subset fragrance chemicals from other PPCP chemicals
fragrance <- c("alpha-ionone", "amyl cinnamal", "beta-citronellol",
               "beta-ionone", "cinnamal", "coumarin",
               "ethylene brassylate", "exaltolide", "galaxolide",
               "lilial", "linalool", "tonalide", "trans-citral")

### this is dealing with CLASS which I don't have in masterParam
# myexp_hazards2MP <- myexp_hazardsMP %>%
#   mutate(class = case_when(
#     chemical_name %in% fragrance ~ "FRAGRANCE",
#     TRUE ~ class)) %>%
#   arrange(class, desc(tot_haz))

myexp_hazards2MP <- myexp_hazardsMP ###  %>%
  # mutate(class = case_when(
  #   chemical_name %in% fragrance ~ "FRAGRANCE",
  #   TRUE ~ class)) %>%
  #  arrange(class, desc(tot_haz))

# write_csv(myexp_hazards2MP,
#           here::here(paste0("myexp_hazardscreenMP_", Sys.Date(), ".csv")))

# THESE are the categories Silent Spring REDUCED everything down to...
# "reproductive_toxicity"     # Reproduction and fertility: Chemicals that affect the reproductive system or people’s ability to have children.
# "neurotoxicity" # Brain and behavior: Chemicals that affect the brain, nervous system, or learning and behavior.
# "carcinogenicity" # Increased cancer risk: Chemicals that increase the risk of certain cancers.
# "endocrine_toxicity"  # Hormone disruption: Chemicals that interfere with your body’s natural hormones. Hormones are important for regulating nearly all your body’s systems.
# "developmental_toxicity"   # Development: Chemicals that affect how a baby grows and develops in childhood.
# "pbt"   # PBT: Chemicals that are Persistent, Bioaccumulative, and Toxic. This means that they are difficult to break down and eliminate from your body and the environment.
# "genotoxicity"  # Harms DNA: Chemicals that damage DNA. DNA contains all the instructions for making your body run.
# "respiratory_toxicity"   # Respiratory: Chemicals that irritate people's lungs and make asthma worse.




# I assume you add  MC = rodent mammary carcinogens to carcinogincity and , EDC+ = endocrine-disrupting compounds to endocrine
myexp_hazards3MP  <- myexp_hazards2MP %>%
  mutate(endocrine_toxicity =
           if_else( endocrine_toxicity ==1 | bcrc_edc_plus== 1, 1,0)
         ) %>%
  select(-bcrc_edc_plus)

myexp_hazards4MP  <- myexp_hazards3MP %>%
  mutate(carcinogenicity =
           if_else( carcinogenicity ==1 | bcrc_mc== 1, 1,0)
  ) %>%
  select(-bcrc_mc)

myexp_hazards5MP <- myexp_hazards4MP %>%
  select (-tot_haz) %>%
  select (-chemical_name) %>%
  distinct()
myexp_hazards5MP <- myexp_hazards5MP %>%
  filter(!cas_rn=="") %>%
  filter(!cas_rn=="N/A")



# WRITE this out where it can be read with setMASTERPARAM_CLASS_RISKSdirectory("myexp_hazardscreenMP5_SPECIFIC DATE INFO.csv")
write_csv(myexp_hazards5MP,
          here::here("data","data_MasterParams_Class_Risks",paste0("myexp_hazardscreenMP5_", Sys.Date(), ".csv")))

myexp_hazards6MP<- myexp_hazards5MP %>%
  left_join(cclist_lookup_url, by="cas_rn") %>%  # add the URL for calSAFER
  rename(calSaferURL=see_details) %>%
  mutate(tot_haz = select(.,             # add column with total value of all hazards
                          "endocrine_toxicity"  ,
                          "respiratory_toxicity",
                          "carcinogenicity"   ,
                          "genotoxicity"  ,
                          "reproductive_toxicity" ,
                          "developmental_toxicity" ,
                          "neurotoxicity" ,
                          "pbt") %>%
           rowSums())



write_csv(myexp_hazards6MP,
          here::here("data","data_MasterParams_Class_Risks",paste0("myexp_hazardscreenMP6_", Sys.Date(), ".csv")))
#myexp_hazardscreenMP6_2024-12-21.csv
