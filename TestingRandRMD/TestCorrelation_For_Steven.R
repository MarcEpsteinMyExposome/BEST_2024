#########THIS FIRST SET OF LINES GETS COMMENTED OUT and NOT USED unless on MARC MACHINE
####### THE POINT is to actually read in a buncha data and "normalize" it and write it out to appropriate file.
##### HERE ARE THE FILES I INTEND TO CREATE:
##### testResults_WB_NIH_Batch1and2_106.csv           ALL 106 wristbands-only
##### testResults_N_NIH_Batch1and2_103.csv           ALL 103 necklace-only
##### testResults_WB_and_N_NIH_Batch1and2_106plus103.csv           ALL 209 WB plus N
#####
if (FALSE) {
  if (!exists("subject")) {   source(here::here("R","MyExp_set_key_variables.R")) }
    ######## This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
  if (!exists("masterParam")) {   source(r_code) }
  ######### Clean up environment a little
  rm(list=ls()[!ls() %in% c("testResults.big")])
  ######### that creates testResult.big
  ######### then select just columns wanted:
  tr<- testResults.big %>% select(SampleNumber,ParameterName,Result,PureSampleName)
  tr <- tr %>% filter(Result>0)
  ### THIS MUST BE SET to print the correct stuff vased on what I'm running...
  #write.csv(tr,"testResults_WB_NIH_Batch1and2_106.csv", row.names = FALSE)
  #write.csv(tr,"testResults_N_NIH_Batch1and2_103.csv", row.names = FALSE)
  #write.csv(tr,"testResults_WB_and_N_NIH_Batch1and2_106plus103.csv", row.names = FALSE)
}

# Load required packages
# Function to check, install, and load packages
load_package <- function(packages) {
  not_installed <- packages[!packages %in% installed.packages()[,"Package"]]
  if(length(not_installed)) install.packages(not_installed)
  invisible(lapply(packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

required_packages <- c(
 "tidyverse",   # FULL tidy dplyr tidyr etc...
  "readr",      # read csv best package
  "ggraph", "igraph",   # graphing
 "here",  #package to manage directory locations
 "pheatmap",    # HeatMap
 "ggplot2",
 "purrr",
 "ggfortify"  # autoplot() for PCA objects has been provided by the ggfortify package
 )

# Load all required packages
load_package(required_packages)

# REMOVE the stuff from environment
rm(load_package, required_packages)

# Read in data from the CSV file without displaying column type messages
# tr is "Test Results" of either just WB or just N or combined depending on what file I read-in...
##  FOR NOW i'm only reading in ALL COMBINED DATA so ALL N and all WB for both Batch 1 and Batch 2

tr <- read_csv("testResults_WB_and_N_NIH_Batch1and2_106plus103.csv", show_col_types = FALSE) %>%
  select(-SampleNumber) %>%
  mutate(WBorN = case_when(
    str_ends(PureSampleName, "-WB") ~ "WB",
    str_ends(PureSampleName, "-N")  ~ "N",
    TRUE                           ~ NA_character_
  )) %>%
  {
    if(any(is.na(.$WBorN))) stop("Error: Some PureSampleName values do not end in '-WB' or '-N'.")
    else .
  }

# Create a binary presence/absence matrix:
# - For each sample, assign 1 if 'Result' > 0 (detection), otherwise 0.
# - Reshape the data so that rows are samples and columns are parameters.
co_occurrence_matrix <- tr %>%
  mutate(Present = if_else(Result > 0, 1, 0)) %>%  # Create binary variable 'Present'
  select(-Result) %>%                              # Remove the original 'Result' column
  pivot_wider(
    names_from = ParameterName,
    values_from = Present,
    values_fill = list(Present = 0)
  ) %>%
  select(-PureSampleName)%>%                           # Remove sample identifier column
  select(-WBorN)


# Remove constant columns (no variability) to avoid issues with zero standard deviation in correlations BUT somehow i didn't need this later???
#co_occurrence_matrix <- co_occurrence_matrix %>% select_if(~ n_distinct(.) > 1)

# Compute the Pearson correlation matrix for the binary matrix
cor_matrix <- cor(co_occurrence_matrix, method = "pearson")
diag(cor_matrix) <- NA  # Remove self-correlations by setting the diagonal to NA

# Compute the co-occurrence count matrix using matrix multiplication:
# Each element (i, j) indicates the number of samples where both parameters were detected.
mat <- as.matrix(co_occurrence_matrix)
co_occurrence_count <- t(mat) %*% mat

# Convert the correlation matrix to long format:
# - Preserve row names as a column named "Var1"
# - Pivot the wide matrix to a long tibble with columns: Var1, Var2, and correlation.
cor_df <- as_tibble(cor_matrix, rownames = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

# Convert the co-occurrence count matrix to long format similarly,
# resulting in columns: Var1, Var2, and count.
occ_df <- as_tibble(co_occurrence_count, rownames = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "count")

# Merge the correlation and co-occurrence data frames by matching parameter pairs
merged_df <- inner_join(cor_df, occ_df, by = c("Var1", "Var2"))

# Define filtering criteria:
filterCount <- 4         # Minimum co-occurrence count required
minimumCorrelation <- 0.3  # Minimum correlation threshold

# Filter out pairs that do not meet the criteria and remove duplicate pairs:
# - Keep only pairs where Var1 comes alphabetically before Var2 to avoid duplicates.
top_correlations <- merged_df %>%
  filter(count >= filterCount,
         as.character(Var1) < as.character(Var2),
         correlation >= minimumCorrelation) %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 50)

# Display the top correlated parameter pairs
print(top_correlations)



# Create the graph as above
g <- graph_from_data_frame(top_correlations, directed = FALSE)



ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = correlation, edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(size = 5) +
  geom_node_label(aes(label = name), repel = TRUE,
                  label.padding = unit(0.2, "lines"),
                  label.size = 0.25,  # thickness of the label border
                  fill = "white",     # background color for the label
                  color = "black") +  # text color
  theme_void() +
  labs(edge_width = "Correlation")+
  guides(edge_alpha = "none")


### NOW TRY HEAT MAP:

# Use the correlation matrix you computed (with diag set to NA)
pheatmap(cor_matrix, na_col = "white", main = "Correlation Heatmap")

# NOW try hierarchical clustering
# For clustering based on the correlation matrix:
dist_matrix <- as.dist(1 - abs(cor_matrix))  # using absolute correlations for similarity
hc <- hclust(dist_matrix, method = "complete")
plot(hc, main = "Hierarchical Clustering of Chemicals")


## NOW do frequency
# Calculate frequency counts per chemical:
freq_df <- tr %>%
  mutate(Present = if_else(Result > 0, 1, 0)) %>%
  group_by(ParameterName) %>%
  summarise(Frequency = sum(Present)) %>%
  arrange(desc(Frequency))

ggplot(freq_df, aes(x = reorder(ParameterName, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Detection Frequency by Chemical",
       x = "Chemical", y = "Detection Count")



# PCA on the binary data:
## NO IDEA WHAT THE HECK THESE CHARTS  MEAN

pca_res <- prcomp(co_occurrence_matrix, scale. = TRUE)
#autoplot(pca_res, data = freq_df, loadings = TRUE, loadings.label = TRUE)  # THIS ERRORS with diff # of rows
autoplot(pca_res, loadings = TRUE, loadings.label = TRUE)

#PCA on the continuous data
tr_wide <- tr %>%
  pivot_wider(
    names_from = ParameterName,
    values_from = Result,
    values_fill = list(Result = 0)  # or use NA if appropriate
  ) %>%
  select(-PureSampleName) ###   %>%  # remove the sample identifier if needed
  ###select(-WBorN)

pca_res_cont <- prcomp(tr_wide, scale. = TRUE)
# scale = 0 means variables are unscaled (arrows may appear very long).
# scale = 1 is the default, tries to balance sample and variable space.
# scale = 2 emphasizes the sample points more.
autoplot(pca_res_cont, scale = 1, loadings = TRUE, loadings.label = TRUE)

# 1. Log-transform the data (using log1p to handle zeros)
tr_wide_log <- tr_wide %>%
  dplyr::mutate(across(everything(), ~ log1p(.)))

# 2. Run PCA on the log-transformed data
pca_res_cont_log <- prcomp(tr_wide_log, scale. = TRUE)

# 3. Plot the PCA results
# scale = 0 means variables are unscaled (arrows may appear very long).
# scale = 1 is the default, tries to balance sample and variable space.
# scale = 2 emphasizes the sample points more.
autoplot(pca_res_cont_log, scale = 0, loadings = TRUE, loadings.label = TRUE)

## NO IDEA WHAT THE HECK THESE above CHARTS  MEAN



# Convert from wide to long format, keeping the WBorN column and gathering all chemicals into two columns:
tr_long <- tr_wide %>%
  pivot_longer(
    cols = -WBorN,           # All columns except WBorN
    names_to = "chemical",   # New column for chemical names
    values_to = "value"      # New column for measured values
  )

summary_df <- tr_long %>%
  group_by(WBorN, chemical) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value, na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  )

print(summary_df)



# For each chemical, run a t-test comparing value ~ WBorN.
t_tests <- tr_long %>%
  group_by(chemical) %>%
  summarise(
    t_test = list(t.test(value ~ WBorN, data = cur_data())),
    .groups = "drop"
  ) %>%
  # Extract the p-value from each test:
  mutate(p_value = map_dbl(t_test, "p.value"))

print(t_tests)


library(ggplot2)

# Example: Facet the boxplot for all chemicals.
# (You might want to filter to a smaller set for clarity.)
ggplot(tr_long, aes(x = WBorN, y = value, fill = WBorN)) +
  geom_boxplot() +
  facet_wrap(~ chemical, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparison of Chemical Measurements by Group",
       y = "Measurement Value",
       x = "Group (WBorN)")


# multivariate view (i.e., whether the overall chemical profiles differ between groups), you can perform PCA.

# Remove the WBorN column, perform PCA on the numeric data,
# then add the WBorN grouping back to the PCA scores.
pca_res <- prcomp(tr_wide %>% select(-WBorN), scale. = TRUE)

pca_df <- as.data.frame(pca_res$x) %>%
  mutate(WBorN = tr_wide$WBorN)

ggplot(pca_df, aes(x = PC1, y = PC2, color = WBorN)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA of Chemical Measurements",
       x = "Principal Component 1",
       y = "Principal Component 2")

# View the loadings for PC1:
pc1_loadings <- pca_res$rotation[, "PC1"]
print(pc1_loadings)

# To see which variables contribute most (by absolute value), you can sort them:
sorted_pc1 <- sort(abs(pc1_loadings), decreasing = TRUE)
#print(sorted_pc1)

# Convert sorted_pc1 to a data frame with two columns: Chemical and Loading
df_sorted_pc1 <- data.frame(
  Chemical = names(sorted_pc1),
  Loading = sorted_pc1,
  row.names = NULL
)

# Optionally round the loading values for better display
df_sorted_pc1$Loading <- round(df_sorted_pc1$Loading, 3)

# Print the data frame
print(df_sorted_pc1)

