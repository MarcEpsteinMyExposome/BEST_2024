suppressMessages(library("tidyverse"))
library(readxl)


# What are the unique values in ABUND?
WB_Paper_xls_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/WB and or silicone papers.xlsx" #
WB_Paper_Unique_Abund_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/Unique_Abund.csv" #
#
# WB_Paper_xls <- read_excel(WB_Paper_xls_name,
#                            sheet="OnlyWBpapers")
# WB_Paper_xls_sub<- WB_Paper_xls %>%
#   select(Title,"ABUND (top cmpds)")
#
# WB_Paper_xls_sub_tidy1 <- WB_Paper_xls_sub %>%
#   rename(ABUNDtop ="ABUND (top cmpds)") %>%
#   separate_rows(ABUNDtop,sep=";") %>%
#   mutate(ABUNDtop= trimws(ABUNDtop)) %>%
#   filter(substr(ABUNDtop,1,1)!="(")
#
#
# unique_ABUND <- as_tibble(unique(WB_Paper_xls_sub_tidy1$ABUNDtop)) %>%
#   arrange(str_to_upper(value))

#
#
#
# write_excel_csv(unique_ABUND,WB_Paper_Unique_Abund_name)
#
# rm(WB_Paper_xls_name,WB_Paper_Unique_Abund_name,WB_Paper_xls_sub,WB_Paper_xls,WB_Paper_xls_sub_tidy1,unique_ABUND)



unique_ABUND <- read_excel(WB_Paper_xls_name,
                    sheet="OnlyWBpapers") %>%
  select(Title,"ABUND (top cmpds)") %>%
  rename(ABUNDtop ="ABUND (top cmpds)") %>%
  separate_rows(ABUNDtop,sep=";") %>%
  mutate(ABUNDtop= trimws(ABUNDtop)) %>%
  filter(substr(ABUNDtop,1,1)!="(") %>%
  distinct(ABUNDtop) %>%
  arrange(str_to_upper(ABUNDtop))

write_excel_csv(unique_ABUND,WB_Paper_Unique_Abund_name)


unique_MAG <- read_excel(WB_Paper_xls_name,
                           sheet="OnlyWBpapers") %>%
  select(Title,"MAG (top cmpds)") %>%
  rename(ABUNDtop ="ABUND (top cmpds)") %>%
  separate_rows(ABUNDtop,sep=";") %>%
  mutate(ABUNDtop= trimws(ABUNDtop)) %>%
  filter(substr(ABUNDtop,1,1)!="(") %>%
  distinct(ABUNDtop) %>%
  arrange(str_to_upper(ABUNDtop))













####
####
# What are the unique values in MAG (top cmpds)?

WB_Paper_xls_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/WB and or silicone papers.xlsx" #
WB_Paper_Unique_Mag_name <- "D:/Users/Marc/Dropbox/MyExposomeSharedFolder/Data/Publication Database/Marc play copy/Unique_Mag.csv" #

WB_Paper_xls_mag <- read_excel(WB_Paper_xls_name,
                           sheet="OnlyWBpapers")
WB_Paper_xls_mag_sub<- WB_Paper_xls_mag %>%
  select(Title,"MAG (top cmpds)")

WB_Paper_xls_mag_sub_tidy1 <- WB_Paper_xls_mag_sub %>%
  rename(MAGtop ="MAG (top cmpds)") %>%
  separate_rows(MAGtop,sep=";") %>%
  mutate(MAGtop= trimws(MAGtop)) %>%
  filter(substr(MAGtop,1,1)!="(")


unique_MAG <- as_tibble(unique(WB_Paper_xls_mag_sub_tidy1$MAGtop)) %>%
  arrange(str_to_upper(value))

write_excel_csv(unique_MAG,WB_Paper_Unique_Mag_name)
rm(WB_Paper_xls_name,WB_Paper_xls_mag_sub,WB_Paper_Unique_Mag_name,WB_Paper_xls_mag,WB_Paper_xls_mag_sub_tidy1 ,unique_MAG)
