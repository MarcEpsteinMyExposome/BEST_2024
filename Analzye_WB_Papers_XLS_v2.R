### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE
### THIS IS OLD VERSION... mOVED CODE TO RMD FILE



suppressMessages(library("tidyverse"))
library(readxl)


if (!require("vtable")) {
  install.packages("vtable", dependencies = TRUE)
}
library(vtable)


if (!require("formattable")) {
  install.packages("formattable", dependencies = TRUE)
}
library(formattable)




##




### FIRST lets find the UNIQUE values in some fields and write them out
### THIS was only to eliminate duplicates within MAG and ABUND columns.... steven has now given everything a STANDARD name
###


# What are the unique values in ABUND?
WB_Paper_xls_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/WB and or silicone papers.xlsx" #
WB_Paper_Unique_Abund_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/Unique_Abund.csv" #
WB_Paper_Unique_Mag_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/Unique_Mag.csv" #

#Find Unique values in ABUND column of spreadsheet
unique_ABUND <- read_excel(WB_Paper_xls_name,
                    #sheet="OnlyWBpapers") %>%   # CHANGE THIS NAME to be the correct TAB on the spreadsheet
                    sheet="Only WB - Oct 2022") %>%   # CHANGE THIS NAME to be the correct TAB on the spreadsheet
  select(Title,"ABUND (top cmpds)") %>%
  rename(ABUNDtop ="ABUND (top cmpds)") %>%
  separate_rows(ABUNDtop,sep=";") %>%
  mutate(ABUNDtop= trimws(ABUNDtop)) %>%
  filter(substr(ABUNDtop,1,1)!="(") %>%
  distinct(ABUNDtop) %>%
  arrange(str_to_upper(ABUNDtop))

write_excel_csv(unique_ABUND,WB_Paper_Unique_Abund_name)


#Find Unique values in MAG column of spreadsheet
unique_MAG <- read_excel(WB_Paper_xls_name,
                           sheet="OnlyWBpapers") %>%
  select(Title,"MAG (top cmpds)") %>%
  rename(MAGtop ="MAG (top cmpds)") %>%
  separate_rows(MAGtop,sep=";") %>%
  mutate(MAGtop= trimws(MAGtop)) %>%
  filter(substr(MAGtop,1,1)!="(") %>%
  distinct(MAGtop) %>%
  arrange(str_to_upper(MAGtop))


write_excel_csv(unique_MAG,WB_Paper_Unique_Mag_name)

# CLEAN UP
rm(WB_Paper_xls_name, unique_MAG,unique_ABUND,WB_Paper_Unique_Abund_name,WB_Paper_Unique_Mag_name)

#
#
#
#  NOW try to create real data from Spreadsheet assuming that the unique values are as good as possible....
#
#  Make data "tidy" by converting from WIDE to LONG wherever practical

WB_Paper_xls_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/WB and or silicone papers_v2.xlsx" #  NOTE V2

dataFile <- read_excel(WB_Paper_xls_name,
                            sheet="Only WB - Oct 2022")




workingTibble <- dataFile %>%
  #select(Title,"ABUND (top cmpds)") %>%
  rename(ABUNDtop ="ABUND (top cmpds)") %>%
  ### NOW i realize I need to trim TRAILING semicolon so seaprate_rows works properly
  mutate(ABUNDtop = if_else(str_detect(ABUNDtop,";$"),str_sub(ABUNDtop,end=-2),ABUNDtop)) %>%
  separate_rows(ABUNDtop,sep=";") %>%
  mutate(ABUNDtop= trimws(ABUNDtop)) %>%
  #filter(substr(ABUNDtop,1,1)!="(") %>%  ## DO NOT need to drop any extra rows
  #
  #select(Title,"MAG (top cmpds)") %>%
  rename(MAGtop ="MAG (top cmpds)") %>%
  ### NOW i realize I need to trim TRAILING semicolon so separate_rows works properly
  mutate(MAGtop = if_else(str_detect(MAGtop,";$"),str_sub(MAGtop,end=-2),MAGtop)) %>%
  separate_rows(MAGtop,sep=";") %>%
  mutate(MAGtop= trimws(MAGtop)) %>%
  #filter(substr(MAGtop,1,1)!="(") %>%  ## DO NOT need to drop any extra rows
  #
  #select(Title,Authors) %>%     ### There are various Select statements just for as debugging to focus on each section
  separate_rows(Authors,sep=";") %>%   # Split up authors
  mutate(Authors= trimws(Authors)) %>%
  filter(Authors != "") %>%  # Some rows have extra semi-colons which on the split makes blank values.  Drop any blank author values
  #select(Title,NBRFs,OPEs,PAHs,PBDEs,PCBs,PHTHs,Pesticides,PPCPs,Other) %>%  # COMMENT OUT THIS SELECTION LINE TO GET REAL OUTPUT... THIS IS FOR TESTING FOCUS
  pivot_longer(cols = c(NBRFs,OPEs,PAHs,PBDEs,PCBs,PHTHs,Pesticides,PPCPs,Other), names_to = "chemType", values_to = "Y_N_NA_For_Chem_Classes") %>%   # Pivot_Longer uses COLUMN Names as Row Values
  mutate(Y_N_NA_For_Chem_Classes= trimws(Y_N_NA_For_Chem_Classes))  %>%
  filter(Y_N_NA_For_Chem_Classes != "N")  %>%   #  GET rid of all N values for chem classes since that means we affirmatively DO NOT have them.... Y means YES, NA means we don't know yet
  #select(Title,"Instrumental Analysis") %>%
  rename ( instrumentAnalysis = "Instrumental Analysis") %>%
  separate_rows(instrumentAnalysis,sep=",") %>%
  separate_rows(instrumentAnalysis,sep=";") %>%   # SOMETIMES the spreadsheet uses semi-colon, somtimes comma, to separate testing types
  mutate(instrumentAnalysis= trimws(instrumentAnalysis)) %>%
  #select(Title,"Chem interest") %>%
  rename ( chemInterest = "Chem interest") %>%
  separate_rows(chemInterest,sep=";") %>%
  mutate(chemInterest= trimws(chemInterest))# %>%
  #select (-c("number","Samon rev (2022)","Waclawik rev (2022)","Hamzai rev (2022)", "Hou rev (2021)","In MyE folder:","Need access?" ))

## OK:  Now we  have workingTibble which is a LONG version of the data... and we can analyze or chart from here...
## Like how many papers did Kim write:


## NOW how many papers did EACH author participate in
authorAnalysis <-
  workingTibble %>%
  select(Authors,Title) %>%
  distinct() %>%
  group_by(Authors)%>%
  summarize(count=n()) %>%
  arrange(desc(count))
authorAnalysis

## NOW how many papers had what chemical FOCUS
chemAnalysis <-workingTibble %>%
  select(chemInterest,Title) %>%
  distinct() %>%
  group_by(chemInterest)%>%
  summarize(count=n()) %>%
  arrange(desc(count))
chemAnalysis

## NOW how many papers Found each Chemical in abundance
chemABUNDtop <-workingTibble %>%
  select(ABUNDtop,Title) %>%
  distinct() %>%
  group_by(ABUNDtop)%>%
  summarize(count=n()) %>%
  arrange(desc(count))


chemABUNDtop <- chemABUNDtop %>% rename(Chem=ABUNDtop)


## NOW how many papers Found each Chemical in MAG
chemMAGtop <-workingTibble %>%
  select(MAGtop,Title) %>%
  distinct() %>%
  group_by(MAGtop)%>%
  summarize(count=n()) %>%
  arrange(desc(count))
chemMAGtop

chemMAGtop <- chemMAGtop %>% rename(Chem=MAGtop)


## NOW how many papers each type of analysis tool
instrumentType <-workingTibble %>%
  select(instrumentAnalysis,Title) %>%
  distinct() %>%
  group_by(instrumentAnalysis)%>%
  summarize(count=n()) %>%
  arrange(desc(count))
instrumentType



# let's chart papers-by-author but only those with 4 or more papers
ggplot(authorAnalysis %>%
         filter(count>=3),
       aes(reorder(Authors,count,sum),count))+
  geom_col() +
  coord_flip()

rm(chemMAGtop,chemABUNDtop, chemAnalysis,authorAnalysis,instrumentType,WB_Paper_xls_name )

#### how many people per cohort
nCount<-workingTibble %>%
  select(n,Title) %>%
  distinct() %>%
  mutate(n=as.numeric(n)) %>%
  replace(is.na(.), 0) %>%
  arrange(desc(n))

nCount %>%
  summarize(avg=mean(n), min=min(n), max=max(n))

ggplot(nCount %>%
         filter(n>=0),
       aes(reorder(Title,n,sum),n))+
  geom_col() +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 40)) +
  coord_flip()
rm(nCount)

st(workingTibble %>%  mutate(n=as.numeric(n))  ,vars=c('n'))


# Let's see how many cohort members wore wristbands for MyExposome vs Other
suppressWarnings (
  workingTibble %>%
    select(`myexpo client?`,n,Title) %>%
    distinct() %>%
    #filter(n != "NA") %>%
    mutate(n=as.numeric(n)) %>%
    replace(is.na(.), 0) %>%
    group_by(`myexpo client?`)%>%
    summarize(numberOfPapers=n(), numberOfWristbands=sum(n)) %>%
    arrange(desc(numberOfWristbands)) %>%
    mutate(across(where(is.numeric), ~ formattable::percent(./sum(.))))
)

# Let's see how many cohort members wore wristbands by year of publication
suppressWarnings (
  workingTibble %>%
    select(Year,n,Title) %>%
    distinct() %>%
    mutate(n=as.numeric(n)) %>%
    replace(is.na(.), 0) %>%
    group_by(Year)%>%
    summarize(numberOfPapers=n(), numberOfWristbands=sum(n)) %>%
    arrange(desc(Year))
)



# Let's see What the average # of days worn was
suppressWarnings (
  workingTibble %>%
    rename(days='Wearing Period [Days]') %>%
    select(days,Title) %>%
    distinct() %>%
    mutate(days=as.numeric(days)) %>%
    replace(is.na(.), 0) %>%
    summarize(count=n(), mean=mean(days)) %>%
    arrange(desc(count))
)


##### NOW REVERSE THE WIDE-to-LONG action


workingTibbleWide<-workingTibble %>%
  pivot_wider(names_from = chemType, values_from = Y_N_NA_For_Chem_Classes,values_fill='N' ) %>%
  group_by(Title) %>%
  summarise( chemInterest= paste0(unique(chemInterest),collapse=";"),
             ABUNDtop=paste0(unique(ABUNDtop),collapse=";"),
             MAGtop=paste0(unique(MAGtop),collapse=";"),
             Authors=paste0(unique(Authors),collapse=";"),
             instrumentAnalysis=paste0(unique(instrumentAnalysis),collapse=";"),
             across()
  ) %>%
  #ungroup() %>%
  distinct() %>%
  arrange(number)

sort(setdiff(colnames(dataFile), colnames(workingTibbleWide)))
sort(setdiff(colnames(workingTibbleWide), colnames(dataFile)))

WB_Paper_Unique_Abund_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/WB_Excel_Database_v3.csv" #

write_excel_csv(workingTibbleWide,WB_Paper_Unique_Abund_name)


W_abund<-workingTibble %>%
  select(ABUNDtop,chemInterest) %>%
  rename(Chem=ABUNDtop) %>%
  unique()
W_abund

W_mag<-workingTibble %>%
  select(MAGtop,chemInterest) %>%
  rename(Chem=MAGtop) %>%
  unique()

W_mag


merge(W_abund,W_mag) %>%
  unique()  %>%
  arrange(desc(Chem))
# Do a x-y chart compare ANBUD and TOP
#




### after talking to steven... think about diff format outputs


## NOW how many papers Found each Chemical in abundance
chemABUNDtop <-workingTibble %>%
  select(ABUNDtop,Title) %>%
  distinct() %>%
  group_by(ABUNDtop)%>%
  summarize(abundCount=n()) %>%
  rename(Chem=ABUNDtop) %>%
  filter(abundCount>=4) %>%
  arrange(desc(abundCount))
chemABUNDtop



## NOW how many papers Found each Chemical in MAG
chemMAGtop <-workingTibble %>%
  select(MAGtop,Title) %>%
  distinct() %>%
  group_by(MAGtop)%>%
  summarize(magCount=n()) %>%
  rename(Chem=MAGtop) %>%
  filter(magCount>=4) %>%
  arrange(desc(magCount))
chemMAGtop

merged <-merge(chemMAGtop,chemABUNDtop)

ggplot(data=merged, aes(x = magCount, y = abundCount,label="Chem")) +
  geom_point()

ggplot(data=merged, aes(x = magCount, y = abundCount,colour="gre#en", label=Chem)) +
  geom_point()+geom_text(hjust=0, vjust=0)

### NOW I'm going to do a TEST HERE but really should be higher up in this list
###  I'm going to  say "wonder if the data in ABUND and MAG had not been rationalized yet to common names
### and I'll read in Steven Lookup Table, join it to workingTibble to fix the names
### and I'll read in Steven Lookup Table, join it to workingTibble to fix the names
### and I'll read in Steven Lookup Table, join it to workingTibble to fix the names


WB_Paper_OLD2_xls_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/OLD2/WB and or silicone papers.xlsx" #

wT <-read_excel(WB_Paper_OLD2_xls_name,
           #sheet="OnlyWBpapers") %>%   # CHANGE THIS NAME to be the correct TAB on the spreadsheet
           sheet="Only WB - Oct 2022")



wT2 <- wT %>%
  #select(Title,"ABUND (top cmpds)") %>%
  rename(ABUNDtop ="ABUND (top cmpds)") %>%
  ### NOW i realize I need to trim TRAILING semicolon so seaprate_rows works properly
  mutate(ABUNDtop = if_else(str_detect(ABUNDtop,";$"),str_sub(ABUNDtop,end=-2),ABUNDtop)) %>%
  separate_rows(ABUNDtop,sep=";") %>%
  mutate(ABUNDtop= trimws(ABUNDtop)) %>%
  #filter(substr(ABUNDtop,1,1)!="(") %>%  ## DO NOT need to drop any extra rows
  #
  #select(Title,"MAG (top cmpds)") %>%
  rename(MAGtop ="MAG (top cmpds)") %>%
  ### NOW i realize I need to trim TRAILING semicolon so separate_rows works properly
  mutate(MAGtop = if_else(str_detect(MAGtop,";$"),str_sub(MAGtop,end=-2),MAGtop)) %>%
  separate_rows(MAGtop,sep=";") %>%
  mutate(MAGtop= trimws(MAGtop)) %>%
  #filter(substr(MAGtop,1,1)!="(") %>%  ## DO NOT need to drop any extra rows
  #
  #select(Title,Authors) %>%     ### There are various Select statements just for as debugging to focus on each section
  separate_rows(Authors,sep=";") %>%   # Split up authors
  mutate(Authors= trimws(Authors)) %>%
  filter(Authors != "") %>%  # Some rows have extra semi-colons which on the split makes blank values.  Drop any blank author values
  #select(Title,NBRFs,OPEs,PAHs,PBDEs,PCBs,PHTHs,Pesticides,PPCPs,Other) %>%  # COMMENT OUT THIS SELECTION LINE TO GET REAL OUTPUT... THIS IS FOR TESTING FOCUS
  pivot_longer(cols = c(NBRFs,OPEs,PAHs,PBDEs,PCBs,PHTHs,Pesticides,PPCPs,Other), names_to = "chemType", values_to = "Y_N_NA_For_Chem_Classes") %>%   # Pivot_Longer uses COLUMN Names as Row Values
  mutate(Y_N_NA_For_Chem_Classes= trimws(Y_N_NA_For_Chem_Classes))  %>%
  filter(Y_N_NA_For_Chem_Classes != "N")  %>%   #  GET rid of all N values for chem classes since that means we affirmatively DO NOT have them.... Y means YES, NA means we don't know yet
  #select(Title,"Instrumental Analysis") %>%
  rename ( instrumentAnalysis = "Instrumental Analysis") %>%
  separate_rows(instrumentAnalysis,sep=",") %>%
  separate_rows(instrumentAnalysis,sep=";") %>%   # SOMETIMES the spreadsheet uses semi-colon, somtimes comma, to separate testing types
  mutate(instrumentAnalysis= trimws(instrumentAnalysis)) %>%
  #select(Title,"Chem interest") %>%
  rename ( chemInterest = "Chem interest") %>%
  separate_rows(chemInterest,sep=";") %>%
  mutate(chemInterest= trimws(chemInterest))# %>%

stevenFixupXLS<- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/Unique_Abund_and_MAG_v4.xlsm" #

stevenFixUP<- read_excel(stevenFixupXLS,
                         sheet="Unique_Abund") %>%
  select(ABUND_and_MAG,BestName) %>%
  mutate(ABUND_and_MAG=str_trim(ABUND_and_MAG)) %>%
  mutate(BestName=str_trim(BestName))

WT3 <- wT2 %>%
  select(Title,ABUNDtop,MAGtop) %>%
  #select(Title,MAGtop) %>%
  left_join(stevenFixUP, by=c("ABUNDtop"="ABUND_and_MAG")) %>%
  mutate(ABUNDtop = if_else(is.na(BestName),ABUNDtop,BestName))%>%
  select(-BestName) %>%
  left_join(stevenFixUP, by=c("MAGtop"="ABUND_and_MAG")) %>%
  mutate(MAGtop = if_else(is.na(BestName),MAGtop,BestName))%>%
  select(-BestName) %>%
  select(Title,ABUNDtop,MAGtop)

#%>%
  #transmute(ABUNDtop=coalesce(BestName,ABUNDtop))
wT4 <- wT %>%
  #select(Title,"ABUND (top cmpds)") %>%
  rename(ABUNDtop ="ABUND (top cmpds)") %>%
  ### NOW i realize I need to trim TRAILING semicolon so seaprate_rows works properly
  mutate(ABUNDtop = if_else(str_detect(ABUNDtop,";$"),str_sub(ABUNDtop,end=-2),ABUNDtop)) %>%
  separate_rows(ABUNDtop,sep=";") %>%
  mutate(ABUNDtop= trimws(ABUNDtop)) %>%
  ## NOW I'm going to fix-up the values in the table to a common name for each chemical
  left_join(stevenFixUP, by=c("ABUNDtop"="ABUND_and_MAG")) %>%
  mutate(ABUNDtop = if_else(is.na(BestName),ABUNDtop,BestName))%>%
  select(-BestName) %>%
  ### Done with fixup
  rename(MAGtop ="MAG (top cmpds)") %>%
  ### NOW i realize I need to trim TRAILING semicolon so separate_rows works properly
  mutate(MAGtop = if_else(str_detect(MAGtop,";$"),str_sub(MAGtop,end=-2),MAGtop)) %>%
  separate_rows(MAGtop,sep=";") %>%
  mutate(MAGtop= trimws(MAGtop)) %>%
  ## NOW I'm going to fix-up the values in the table to a common name for each chemical
  left_join(stevenFixUP, by=c("MAGtop"="ABUND_and_MAG")) %>%
  mutate(MAGtop = if_else(is.na(BestName),MAGtop,BestName))%>%
  select(-BestName) %>%
  ### Done with fixup
  separate_rows(Authors,sep=";") %>%   # Split up authors
  mutate(Authors= trimws(Authors)) %>%
  filter(Authors != "") %>%  # Some rows have extra semi-colons which on the split makes blank values.  Drop any blank author values
  pivot_longer(cols = c(NBRFs,OPEs,PAHs,PBDEs,PCBs,PHTHs,Pesticides,PPCPs,Other), names_to = "chemType", values_to = "Y_N_NA_For_Chem_Classes") %>%   # Pivot_Longer uses COLUMN Names as Row Values
  mutate(Y_N_NA_For_Chem_Classes= trimws(Y_N_NA_For_Chem_Classes))  %>%
  filter(Y_N_NA_For_Chem_Classes != "N")  %>%   #  GET rid of all N values for chem classes since that means we affirmatively DO NOT have them.... Y means YES, NA means we don't know yet
  rename ( instrumentAnalysis = "Instrumental Analysis") %>%
  separate_rows(instrumentAnalysis,sep=",") %>%
  separate_rows(instrumentAnalysis,sep=";") %>%   # SOMETIMES the spreadsheet uses semi-colon, somtimes comma, to separate testing types
  mutate(instrumentAnalysis= trimws(instrumentAnalysis)) %>%
  rename ( chemInterest = "Chem interest") %>%
  separate_rows(chemInterest,sep=";") %>%
  mutate(chemInterest= trimws(chemInterest))# %>%

