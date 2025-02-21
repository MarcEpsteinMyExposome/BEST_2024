# Load required packages


# Function to check, install, and load packages
load_package <- function(packages) {
  not_installed <- packages[!packages %in% installed.packages()[,"Package"]]
  if(length(not_installed)) install.packages(not_installed)
  lapply(packages, function(pkg) {
    suppressMessages(library(pkg, character.only = TRUE))
  })
}

required_packages <- c(
 "tidyverse",   # FULL tidy dplyr tidyr etc...
  "readr",      # read csv best package
  "ggraph", "igraph",   # graphing
 "pheatmap",    # HeatMap
 "ggplot2",
 "ggfortify"  # autoplot() for PCA objects has been provided by the ggfortify package
 )

# Load all required packages
load_package(required_packages)

# REMOVE the stuff from environment
rm(load_package, required_packages)

# Read in data from the CSV file without displaying column type messages
# tr is "Test Results" of all WRISTBAND ONLY data from 71 WBs
tr <- read_csv("testResultsWristband.csv", show_col_types = FALSE)

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
  select(-SampleNumber)                           # Remove sample identifier column

# Remove constant columns (no variability) to avoid issues with zero standard deviation in correlations
co_occurrence_matrix <- co_occurrence_matrix %>% select_if(~ n_distinct(.) > 1)

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
  select(-SampleNumber)  # remove the sample identifier if needed

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

