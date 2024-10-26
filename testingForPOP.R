# Testing to be sure if there are any POP in the data

#
# Set all key variables.  Use the existance (or not) of "subject" to decide if they need to be loaded
if (!exists("subject")) {
  source("MyExp_set_key_variables.R")
}
# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
if (!exists("masterParam")) {
  source(r_code)
}


# Clean up environment a little
rm(list=ls()[!ls() %in% c("masterParam","testResults.big","class_L","class_TEMP_hold")])



## OK>  The logic to create this group:  "Persistent Organic Pollutants (POPs)"  is that we MERGED "Dioxins and Furans","Polychlorinated Biphenyl"

##

allClasses<- unique(class_L$classification)   #"Persistent Organic Pollutants (POPs)"
allClasses

class_L[class_L$classification=="Persistent Organic Pollutants (POPs)",]

str(class_L)
library(tidyr)
library(dplyr)

# Assuming `class_L` is your tibble
# Assuming `class_L` is your tibble
class_L_wide <- class_L %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = classification, values_from = value, values_fill = 0) %>%
  arrange(desc(`Persistent Organic Pollutants (POPs)`)) %>%  # Sort by POPs column in descending order
  left_join(masterParam,by="ParameterID")

write.csv(class_L_wide,"All_MasterParameter_with_ClassificationMaybeError2.csv")


## So looking in the now-current classification file:
# MasterParam and class_TEMP_hold have same # of items
#so let's just join them
#first make parameterID character
class_TEMP_hold$ParameterID <- as.character(class_TEMP_hold$ParameterID )

class_FULL<- masterParam %>%
  left_join(class_TEMP_hold,by="ParameterID")
# so now POP = "Dioxins and Furans"   plus "Polychlorinated Biphenyl"

## so these are all the parameter IDs of either dioxin or PCB
dioxin_or_PCB <- union (class_FULL$ParameterID[class_FULL$"Dioxins and Furans"==1 ], class_FULL$ParameterID[ class_FULL$"Polychlorinated Biphenyl"==1])

# 257 items have one or the other  AND THIS MATCHES the classification file
length(union (class_FULL$ParameterID[class_FULL$"Dioxins and Furans"==1 ], class_FULL$ParameterID[ class_FULL$"Polychlorinated Biphenyl"==1]))

# NO INTERSECTION between PCB and dioxin/furan
intersect (class_FULL$ParameterID[class_FULL$"Dioxins and Furans"==1 ], class_FULL$ParameterID[ class_FULL$"Polychlorinated Biphenyl"==1])

# These are all the parameter IDS in the result
unique_PID<-unique(testResults.big$ParameterID)

# HUH:  There are 82 unique Parameter IDS in the result set.....  That is weird cause i thought there were 88 unique Parameter IDs in the result SET.  What is up with that?
length(unique(testResults.big$ParameterID))

### MEANWHILE.... what is the intersection of things from the results with things from POP:
intersect(dioxin_or_PCB,unique_PID)

length(intersect(dioxin_or_PCB,class_FULL$ParameterID[class_FULL$"Dioxins and Furans"==1 ]))

###############   SO EVERYTHING IS FINE EXCEPT... I found 88 chmicals before.... and.... NOW i find only 82 chemicals unique in testResults.big

# Let's check unique CAS and u nique Chemical Name
length(unique(testResults.big$ParameterName))
length(unique(testResults.big$CASNumber))



