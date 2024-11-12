# Experimenet with GT Tables
# rm(list=ls()[!ls() %in% c("statSummary","oneResult","HideIndividualization","testResults.big")])
#hold_statSummary <- statSummary
#statSummary <- hold_statSummary

#install.packages("gt")
library("gt")
library("tidyverse")
library("pander")
#install.packages("downloadthis")
library("downloadthis")

subject<-"A241149"
pureSubjectName <-"40-WB"

subject<-"A241164"
pureSubjectName <-"42-WB"


subject<-"A241170"
pureSubjectName <-"36-WB"


subject<-"A241263"
pureSubjectName <-"165-WB"






oneResult <- testResults.big[testResults.big$Result > 0 & testResults.big$SampleNumber == subject, ]


if ("Chemical_Name" %in% colnames(statSummary)) {
  statSummary <- statSummary %>% dplyr::rename("Chemical Name" = Chemical_Name,
                                               "Standard Deviation" = Standard_Deviation,
                                               "Smallest Value Found" = MinResult ,
                                               "Largest Value Found" = MaxResult,
                                               "Average Value Found" = MeanResult,
                                               "Middle Value Found" = MedianResult,
                                               "How Many Wristbands Had this" = Count)
}


if (HideIndividualization) {
  panderOptions("big.mark", ",") # THIS line ADDS a COMMA in MOST of the numbers (not all... )

  pandoc.table(as.data.frame(statSummary),
               caption = "Statistical Report on all Chemicals on all Wristbands",
               justify = c("right", "right", "right", "right", "right", "right", "right"),
               emphasis.rownames = TRUE,
               split.table = Inf
  )
} else {
  #
  oneResultStatSummary <- oneResult %>%
    select(ParameterName, Result) %>%
    rename("Chemical Name" = ParameterName) %>%
    #right_join(statSummary, by = "Chemical Name") %>% ## THIS RIGHT JOIN makes it for EVERY chemical found anywhere...
    left_join(statSummary, by = "Chemical Name") %>% ## THIS left JOIN makes it for ONLY chemical found on this wristband
    replace_na(list(Result = 0)) %>%
    rename(!!paste(pureSubjectName, "Result", sep = " ") := Result)

  panderOptions("big.mark", ",") # THIS line ADDS a COMMA in MOST of the numbers (not all... )

  pandoc.table(as.data.frame(oneResultStatSummary),
               caption = paste("Statistical Report on all Chemicals on ", pureSubjectName, " wristband", sep = " "),
               justify = c("right", "right", "right", "right", "right", "right", "right", "right"),
               emphasis.rownames = TRUE,
               split.table = Inf
  )

}

oneResultStatSummary %>% gt()

#######################################
#
# if (HideIndividualization) {
#   # Use gt for enhanced table display
#   statSummaryTable <- statSummary %>%
#     gt() %>%
#     fmt(columns = "Standard Deviation", fns = function(x) {
#       suppressWarnings(
#         ifelse(is.na(as.numeric(x)), x, formatC(as.numeric(x), format = "f", big.mark = ",", digits = 0))
#       )
#     }) %>%
#     cols_align(align = "right", columns = "Standard Deviation") %>% # Right-align Standard Deviation column
#     tab_header(title = "Statistical Report on all Chemicals on all Wristbands") %>%
#     fmt_number(columns = c("Smallest Value Found", "Largest Value Found", "Average Value Found", "Middle Value Found"),
#                sep_mark = ",",
#                decimals = 1) %>%                  # Format numeric values with commas
#     tab_style(
#       style = cell_text(weight = "bold"),
#       locations = cells_column_labels(everything()) # Make column labels bold
#     )
#
#   # Render the gt table
#   statSummaryTable
#
# } else {
#   # Prepare individual result summary
#   oneResultStatSummary <- oneResult %>%
#     select(ParameterName, Result) %>%
#     rename("Chemical Name" = ParameterName) %>%
#     left_join(statSummary, by = "Chemical Name") %>% ## THIS left JOIN makes it for ONLY chemical found on this wristband
#     replace_na(list(Result = 0)) %>%
#     rename(!!paste(pureSubjectName, "Result", sep = " ") := Result)
#
#   # Use gt for enhanced table display
#   oneResultStatSummaryTable <- oneResultStatSummary %>%
#     gt() %>%
#     fmt(columns = "Standard Deviation", fns = function(x) {
#       suppressWarnings(
#         ifelse(is.na(as.numeric(x)), x, formatC(as.numeric(x), format = "f", big.mark = ",", digits = 0))
#       )
#     }) %>%
#     cols_align(align = "right", columns = "Standard Deviation") %>% # Right-align Standard Deviation column
#     tab_header(title = paste("Statistical Report on all Chemicals on", pureSubjectName, "wristband")) %>%
#     fmt_number(columns = c("Smallest Value Found", "Largest Value Found", "Average Value Found", "Middle Value Found"),
#                sep_mark = ",",
#                decimals = 1) %>%         # Format numeric values with commas
#     tab_style(
#       style = cell_text(weight = "bold"),
#       locations = cells_column_labels(everything()) # Make column labels bold
#     )
#
#   # Render the gt table
#   oneResultStatSummaryTable
# }

#############################

# Function to create gt table for summary data
generate_gt_table <- function(data, title) {
  data %>%
    gt() %>%
    tab_options(row.striping.include_table_body = TRUE) %>%
    fmt(columns = "Standard Deviation", fns = function(x) {
      suppressWarnings(
        ifelse(is.na(as.numeric(x)), x, formatC(as.numeric(x), format = "f", big.mark = ",", digits = 0))
      )
    }) %>%
    cols_align(align = "right", columns = "Standard Deviation") %>% # Right-align Standard Deviation column
    tab_header(title = title) %>%
    fmt_number(columns = c("Smallest Value Found", "Largest Value Found", "Average Value Found", "Middle Value Found"),
               sep_mark = ",",
               decimals = 1) %>% # Format numeric values with commas
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything()) # Make column labels bold
    )
}

if ("Chemical_Name" %in% colnames(statSummary)) {
  statSummary <- statSummary %>% dplyr::rename("Chemical Name" = Chemical_Name,
                                               "Standard Deviation" = Standard_Deviation,
                                               "Smallest Value Found" = MinResult ,
                                               "Largest Value Found" = MaxResult,
                                               "Average Value Found" = MeanResult,
                                               "Middle Value Found" = MedianResult,
                                               "How Many Wristbands Had this" = Count)
}

if (HideIndividualization) {
  # Generate gt table for statSummary
  statSummaryTable <- generate_gt_table(statSummary, "Statistical Report on all Chemicals on all Wristbands")

  # Render the gt table
  statSummaryTable

} else {
  # Prepare individual result summary
  oneResultStatSummary <- oneResult %>%
    select(ParameterName, Result) %>%
    rename("Chemical Name" = ParameterName) %>%
    left_join(statSummary, by = "Chemical Name") %>% ## THIS left JOIN makes it for ONLY chemical found on this wristband
    replace_na(list(Result = 0)) %>%
    rename(!!paste(pureSubjectName, "Result", sep = " ") := Result)  # This is a special dplyr approach to rename a column dynamically

  # Generate gt table for oneResultStatSummary
  oneResultStatSummaryTable <- generate_gt_table(oneResultStatSummary, paste("Statistical Report on all Chemicals on", pureSubjectName, "wristband"))

  # Generate gt table for oneResultStatSummary
  oneResultStatSummaryTable <- generate_gt_table(oneResultStatSummary, paste("Statistical Report on all Chemicals on", pureSubjectName, "wristband")) %>%
    tab_style(
      style = cell_fill(color = "#e3f2fd"), # Highlight in light blue
      locations = cells_body(columns = c(!!paste(pureSubjectName, "Result", sep = " "))) # Highlight column with pureSubjectName
    )


  # Render the gt table
  oneResultStatSummaryTable
}
mtcars %>%
  download_this(
    output_name = "mtcars dataset",
    output_extension = ".csv",
    button_label = "Download data as csv",
    button_type = "default",
    has_icon = TRUE,
    icon = "fa fa-save"
  )


