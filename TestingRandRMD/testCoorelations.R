#first run "MyExp_Base....R"
# that creates testResult.big
# then select just columns wanted:
tr<- testResults.big %>% select(SampleNumber,ParameterName,Result)
tr <- tr %>% filter(Result>0)



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

# Compute correlations between parameters
cor_matrix <- cor(co_occurrence_matrix, method = "pearson")

# View top correlated parameter pairs
cor_matrix[upper.tri(cor_matrix)] <- NA  # Remove duplicates for readability
as.data.frame(as.table(cor_matrix)) %>%
  filter(!is.na(Freq)) %>%
  arrange(desc(Freq)) %>%
  head(10)  # Show top correlations

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

rules_filtered <- subset(rules, rhs %pin% "Benzyl cinnamate" == FALSE)
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




