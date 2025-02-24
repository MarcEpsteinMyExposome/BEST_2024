#
# Set all key variables.  Use the existance (or not) of "subject" to decide if they need to be loaded
if (!exists("subject")) {
  source(here::here("R","MyExp_set_key_variables.R"))
}

# This test makes sure that if the SOURCE not yet run, we run it... but don't run it "again"
if (!exists("masterParam")) {
  source(r_code)
}

# Clean up environment a little
rm(list=ls()[!ls() %in% c("testResults.big")])


#first run "MyExp_Base....R"
# that creates testResult.big
# then select just columns wanted:
tr<- testResults.big %>% select(SampleNumber,ParameterName,Result)
trWithPureSampleName<- testResults.big %>% select(SampleNumber,ParameterName,Result,PureSampleName)
tr <- tr %>% filter(Result>0)
trWithPureSampleName <- trWithPureSampleName %>% filter(Result>0)

write.csv(tr,"testResultsWristband.csv", row.names = FALSE)
write.csv(trWithPureSampleName,"testResultsWristbandPureSampleName.csv", row.names = FALSE)
#tr2<-read.csv("testResultsWristband.csv")


### WAYS TO GO:
# Conclusion:
#   If you want statistical correlation, use a co-occurrence matrix.
# If you want association rule mining, use Apriori from arules.
# If you want a graph visualization, use igraph.


###
#Create a Co-Occurrence Matrix
# You can construct a binary presence/absence matrix where rows represent SampleNumbers and columns represent ParameterNames,
# then compute correlation or association metrics.

# Explanation:
#   We create a presence/absence matrix where 1 means the ParameterName was detected for a given SampleNumber.
# Compute correlation (cor()) between parameters across all samples.
# Extract the most strongly correlated pairs.

library(tidyr)
library(dplyr)

# Create a binary presence/absence matrix
co_occurrence_matrix <- tr %>%
  mutate(Present = ifelse(Result > 0, 1, 0)) %>%
  select(-Result) %>%
  spread(key = ParameterName, value = Present, fill = 0) %>%
  select(-SampleNumber)  # Removing SampleNumber to keep only the matrix

# filter constant columns
co_occurrence_matrix <- co_occurrence_matrix %>% select_if(~ n_distinct(.) > 1)


# 1. Compute the correlation matrix and remove self-correlations
cor_matrix <- cor(co_occurrence_matrix, method = "pearson")
diag(cor_matrix) <- NA

# 2. Compute the co-occurrence count matrix
# This multiplies the binary matrix (columns: parameters, rows: samples)
co_occurrence_count <- t(as.matrix(co_occurrence_matrix)) %*% as.matrix(co_occurrence_matrix)

# 3. Convert both matrices to long format (data frames)
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  rename(correlation = Freq)

occ_df <- as.data.frame(as.table(co_occurrence_count)) %>%
  rename(count = Freq)

# 4. Merge the correlation and co-occurrence data frames by the parameter pairs
merged_df <- merge(cor_df, occ_df, by = c("Var1", "Var2"))

# 5. Filter out pairs with co-occurrence count less than 2 and sort by correlation
top_correlations <- merged_df %>%
  filter(count >= 10, as.character(Var1) < as.character(Var2)) %>%  # Remove duplicate orderings
  arrange(desc(correlation)) %>%
  head(100)

top_correlations




#######
# Use Association Rules (Apriori Algorithm)
# You can use arules to detect frequent co-occurrence patterns.
# Explanation:
#   We convert the dataset into a transactions format where each SampleNumber is treated as a basket of detected ParameterNames.
# The Apriori algorithm finds frequent associations among ParameterNames.
# We sort rules by lift, a measure of dependency strength.
# Visualization using arulesViz to interpret patterns.
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

# Convert data to transactions format
tr_transactions <- as(split(tr$ParameterName, tr$SampleNumber), "transactions")

# Run Apriori Algorithm
rules <- apriori(tr_transactions, parameter = list(supp = 0.1, conf = 0.6, minlen = 2))
# OR:
rules <- apriori(tr_transactions, parameter = list(supp = 0.1, conf = 0.6, maxlen = 5))


# View the most significant rules
inspect(sort(rules, by = "lift")[1:10])

# Plot association rules
plot(rules, method = "graph", control = list(max = 100))
plot(sort(rules, by = "lift")[1:30], method = "graph")
plot(rules, method = "scatterplot", measure = "support", shading = "lift")

#rules_filtered <- subset(rules, rhs %pin% "Benzyl cinnamate" == FALSE)
inspect(sort(rules_filtered, by = "lift")[1:10])


######
#
# Network Analysis (Graph Representation)
# Visualizing co-occurrence as a graph can provide insights into frequently co-detected parameters.
# Explanation:
#   We create edges between ParameterNames appearing in the same SampleNumber.
# igraph is used to construct a network graph, where edge thickness represents frequency.
# The force-directed layout (layout_with_fr) positions frequently co-occurring parameters closer
library(igraph)

# Create edges based on co-occurrence
edges <- tr %>%
  group_by(SampleNumber) %>%
  summarise(Params = list(unique(ParameterName))) %>%
  unnest(Params) %>%
  rename(Param1 = Params) %>%
  inner_join(tr, by = "SampleNumber") %>%
  rename(Param2 = ParameterName) %>%
  filter(Param1 < Param2) %>%  # Avoid duplicates
  count(Param1, Param2, sort = TRUE)

# Create graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Plot network
plot(g, vertex.size = 5, edge.width = E(g)$n, layout = layout_with_fr, main = "Parameter Co-occurrence Network")



############################## NEW TRY:
# z/*
# n R code i have a set of data i want to investigate.
#
# the data represents the test results of testing 71 people for a bunch of different chemicals.
#
# the data is organized in a tidy tibble like this:
#
#   tibble [1,279 × 3] (S3: tbl_df/tbl/data.frame)
#
# $ SampleNumber : chr [1:1279] "A241264" "A241134" "A241137" "A241142" ...
#
# $ ParameterName: chr [1:1279] "Tri-p-tolyl phosphate" "TPP" "TPP" "TPP" ...
#
# $ Result : num [1:1279] 44.1 25.7 33.9 55.1 95.7 23.2 17.7 45.3 24.3 223 ...
#
# where sampleNumber is the person, ParameterName is the chemical found, and Result is the number of nanograms found.
#
# every person was tested for every chemical but we've stripped out the numerous zero results.. here is a glimpse:
#
# > glimpse(tr)Rows: 1,279
#
# so i'd like to see any statistically important information about this data.
# particularly looking for coorelations between chemials (like, if chem1 shows
# up the frequently chems 2 and 3 also show up) or....
# maybe (if chem1 is > 100 then chem2 will never appear) or ... similar observations.
#
#
#
# how can i do this?
# Explanation and Key Improvements:
#
#   Wide Format Conversion: The crucial first step is converting your data to a wide format using pivot_wider.
# This is essential for correlation analysis because you need each chemical's results as a separate column.
# I've added values_fn = mean to average results for duplicate ParameterName entries for a given SampleNumber.
#
# Correlation Matrix: The correlate() function from the corrr package provides an easy way to calculate correlation coefficients.
# ggcorrplot creates a nice heatmap for visualization.  hc.order = TRUE orders the chemicals based on similarity, making patterns easier to see.
#
# Scatter Plots:  Scatter plots help visualize the relationships between specific pairs of chemicals.
# The geom_smooth() function adds a trend line (linear by default, but you can change the method).
#
# Conditional Relationships: The example shows how to explore relationships based on thresholds
# (e.g., "if chemical A is high"). You can adapt this to your specific hypotheses.
#
# Clustering: Hierarchical clustering can help group chemicals that tend to appear together.
#
# Statistical Tests:  cor.test() allows you to perform formal correlation tests,
# giving you p-values to assess statistical significance.  Regression analysis can model the relationship between chemicals.
#
# Handling Zeroes: I've added code to show how to include zeroes if you need to.  H
# owever, think carefully about whether including all the zeroes is appropriate for your analysis.
# It will significantly change the data distribution and therefore the results.
# Often, in this kind of chemical testing data, the absence of a chemical is not as meaningful as its presence above a detection limit.
tr<- testResults.big %>% select(SampleNumber,ParameterName,Result)
#tr <- tr %>% filter(Result>0)



library(tidyverse)
#install.packages("corrr")

library(corrr)
#install.packages("ggcorrplot")
library(ggcorrplot)

# Your data (replace with your actual data)
# tr <- tibble(
#   SampleNumber = rep(paste0("A", 1:71), each = 18), # Assuming 18 chemicals
#   ParameterName = rep(LETTERS[1:18], 71), # Example chemical names
#   Result = runif(1278, 0, 100) # Example results (replace with your data)
# )

# 1. Data Preparation: Wide Format

# Convert to wide format for correlation analysis
tr_wide <- tr %>%
  pivot_wider(names_from = ParameterName, values_from = Result, values_fn = mean) # handles duplicate ParameterNames for a SampleNumber by averaging

# 2. Correlation Analysis

# Calculate correlations
correlations <- tr_wide %>%
  select(-SampleNumber) %>%  # Remove the SampleNumber column
  correlate()

# Visualize correlations (heatmap)
ggcorrplot(correlations, hc.order = TRUE, type = "upper",
           lab = TRUE, lab_size = 3, method = "circle",
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Chemicals")

# 3. Exploring Specific Relationships

# Scatter plots for pairs of chemicals
# Example: Relationship between chemical A and B
ggplot(tr_wide, aes(x = A, y = B)) +
  geom_point() +
  geom_smooth(method = "lm") +  # Add a trend line
  labs(title = "Relationship between Chemical A and B")

# Example: Relationship between chemical A and C
ggplot(tr_wide, aes(x = A, y = C)) +
  geom_point() +
  geom_smooth(method = "lm") +  # Add a trend line
  labs(title = "Relationship between Chemical A and C")

# 4. Conditional Relationships (e.g., if chem1 > 100 then chem2 ...)

# Example: If chemical A > a threshold, what happens to chemical B?
threshold <- 50  # Set your threshold
tr_wide <- tr_wide %>%
  mutate(A_high = ifelse(A > threshold, "High", "Low"))

ggplot(tr_wide, aes(x = A_high, y = B)) +
  geom_boxplot() +
  labs(title = "Chemical B vs. Chemical A (High/Low)")

# 5. Clustering (Optional)

# Hierarchical clustering to group similar chemicals
chemical_matrix <- tr_wide %>%
  select(-SampleNumber) %>%
  as.matrix()

distance_matrix <- dist(cor(chemical_matrix)) # Distance based on correlation
hc <- hclust(distance_matrix, method = "ward.D2")
plot(hc, main = "Hierarchical Clustering of Chemicals")

# 6. Statistical Tests

# Correlation tests (e.g., Spearman's rank correlation for non-normal data)
cor.test(tr_wide$A, tr_wide$B, method = "spearman")

# Regression analysis (if you want to predict one chemical based on others)
model <- lm(B ~ A, data = tr_wide)  # Linear regression
summary(model)

# 7.  Handling Zeroes (if you want to include them)

# If you want to include the zeroes, you can fill them in before converting to wide format.
# Be aware that this could drastically change the results of the analysis.
tr_filled <- tr %>%
  complete(SampleNumber, ParameterName, fill = list(Result = 0)) # Fills in all combinations with 0

tr_wide_filled <- tr_filled %>%
  pivot_wider(names_from = ParameterName, values_from = Result, values_fn = mean) # handles duplicate ParameterNames for a SampleNumber by averaging

# ... then proceed with the correlation and other analysis as above.

