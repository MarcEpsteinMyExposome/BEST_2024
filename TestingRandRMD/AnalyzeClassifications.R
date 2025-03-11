#need to run various other things first to create some variables, what, TBD
# run base code through creating classifaction then i manuall setop throu function to create new classification
# down through:
# Update class_L
# class_L_new <- class_L %>%
#   rename(classification_OLD = classification) %>%
#   rowwise() %>%
#   mutate(classification = find_classification(str_trim(classification_OLD))) %>%
#   ungroup()

# class_L seems to be every paramterid from every tests we do with a classification
# make class_W which will be a wide version of class_L where ParameterID is the key, classification is the columns
# and 1 is the value if it is in that classification and 0 if it is not
# but i want to do that with pivot_wider not spread


## JUMP AHEAD TO CREATING DF DF DF
## JUMP AHEAD TO CREATING DF DF DF
## JUMP AHEAD TO CREATING DF DF DF
## JUMP AHEAD TO CREATING DF DF DF
#
# class_W<-class_L %>%
#   mutate(value=1) %>%
#   pivot_wider(names_from = classification, values_from = value, values_fill = 0) %>%
#   #add a column which is TRUE if multiple elements in the row are 1 otherwise 0
#   mutate(MultipleClassifications=rowSums(select(.,-ParameterID))>1)
# # count how many are true in MultipleClassifications
# sum(class_W$MultipleClassifications)
#
# # i made class_L_new by manually stepping through the function to convert old classification to new
# # this made class_L_new which is the same as class_L but with the new classification added
# # i want to make class_W_new which is the same as class_W but with the new classification added
# # i want to do that with pivot_wider not spread
#
# class_W_new<-class_L_new %>%
#   mutate(value=1) %>%
#   pivot_wider(names_from = c(classification,classification_OLD), values_from = value, values_fill = 0)
#   #add a column which is TRUE if multiple elements in the row are 1 otherwise 0
# # count how many are true in MultipleClassifications
# sum(df_new$MultipleClassifications)
#
# str(class_L_new)



df<- class_L_new


df_old <- df %>%
  distinct(ParameterID, classification_OLD) %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = ParameterID,
    names_from = classification_OLD,
    values_from = value,
    values_fill = 0,
    names_prefix = "OLD_"  # optional prefix to distinguish old classification columns
  )

df_new <- df %>%
  distinct(ParameterID, classification) %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = ParameterID,
    names_from = classification,
    values_from = value,
    values_fill = 0,
    names_prefix = "NEW_"  # optional prefix to distinguish new classification columns
  ) %>%
  #add a column which is TRUE if multiple elements in the row are 1 otherwise 0
  mutate(MultipleClassifications=rowSums(select(.,-ParameterID))>1)

df_wide_separate <- df_old %>%
  full_join(df_new, by = "ParameterID")

setwd(here::here()) # If you’re using the here package for relative paths


OSU_MasterParam_filename <- here("data","data_MasterParams_Class_Risks","ALL_OSU_PARAMETERFILE_FOR_EVERYTHING_IN_LIMS_OSU_MyE_Parameters.csv")

OSU_MasterParam <- read_csv(OSU_MasterParam_filename,show_col_types = FALSE ) %>%
  select(ParameterID, ParameterName, CasNumber) %>%
  #make parameterID into a character
  mutate(ParameterID = as.character(ParameterID))


# left join the OSU_MasterParam to the df_wide_separate table
df_wide_separate <- df_wide_separate %>%
  left_join(OSU_MasterParam, by = "ParameterID")

# write the df_wide_separate table to a csv file
# this is a file for Steven to use to figure out how to classify the parameters as only one classification
write_csv(df_wide_separate, here("data","data_MasterParams_Class_Risks","df_wide_separate.csv"))

