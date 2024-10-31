# R code to find all dependencies and maybe display in some way
# Load necessary packages
if(!require("purrr")) install.packages("purrr")
if(!require("stringr")) install.packages("stringr")
library(stringi)
library(dplyr)
library(tibble)
library(here)
library(knitr)

# Set base directory and main RMD file
base_directory <- here()
main_rmd_file <- "MyExposome_1527_v6.Rmd"

# Function to parse an RMD file and find its children and conditions
parse_rmd_children <- function(file_path) {
  # Full path to the RMD file
  full_path <- file.path(base_directory, file_path)

  # Read content of the RMD file
  content <- readLines(full_path, warn = FALSE)

  # Initialize lists to store matches
  children <- c()
  conditions <- c()

  # Define regex patterns
  child_pattern <- "child\\s*=\\s*['\"]([^'\"]+)['\"]"
  condition_pattern <- "eval\\s*=\\s*\\(?([^\\)]+)\\)?"  # Capture condition with optional parentheses

  # Iterate through each line to find child and condition matches
  for (line in content) {
    # Attempt to match child file
    child_match <- stri_match_first_regex(line, child_pattern)
    condition_match <- stri_match_first_regex(line, condition_pattern)

    if (!is.na(child_match[1])) {
      # If a child match is found, add it to the list
      children <- c(children, child_match[2])

      # If a condition is found, use it; otherwise, mark as "Always"
      if (!is.na(condition_match[2])) {
        conditions <- c(conditions, condition_match[2])
      } else {
        conditions <- c(conditions, "Always")
      }
    }
  }

  # Return a data frame with the parent file, children, and conditions
  if (length(children) > 0) {
    return(data.frame(
      parent = rep(file_path, length(children)),
      child = children,
      condition = conditions,
      stringsAsFactors = FALSE
    ))
  } else {
    # If no children are found, return a row with only the parent
    return(data.frame(
      parent = file_path,
      child = NA,
      condition = NA,
      stringsAsFactors = FALSE
    ))
  }
}

# Recursive function to build dependency structure
build_rmd_dependency_tree <- function(file_path, visited = character()) {
  # Avoid re-parsing files and check if file exists
  full_path <- file.path(base_directory, file_path)
  if (file_path %in% visited || !file.exists(full_path)) return(tibble())

  # Parse the current RMD file for children and conditions
  dependencies <- parse_rmd_children(file_path)

  # Initialize result with current dependencies
  result <- as_tibble(dependencies)
  visited <- c(visited, file_path)

  # Recursively parse each child
  for (child in dependencies$child[!is.na(dependencies$child)]) {
    # Recursively call the function for each child file
    child_dependencies <- build_rmd_dependency_tree(child, visited)
    result <- bind_rows(result, child_dependencies)
  }

  result
}

# Generate the RMD dependency tree starting from the main RMD file
rmd_dependencies <- build_rmd_dependency_tree(main_rmd_file)

# Display the result as a table
kable(rmd_dependencies, caption = "R Markdown File Dependency Structure")




# Load necessary libraries
library(dplyr)
if (!requireNamespace("data.tree", quietly = TRUE)) {
  install.packages("data.tree")
}
library(data.tree)


# Convert rmd_dependencies to a simple tree structure, ignoring conditions
build_simple_tree <- function(dependencies) {
  # Create a root node for the tree
  root <- Node$new("Project Root")

  # Recursively add nodes to the tree
  add_to_tree <- function(parent_node, parent_name) {
    # Find the children of the current parent
    children <- dependencies %>% filter(parent == parent_name) %>% pull(child)

    # Add each child node to the current parent node
    for (child in children) {
      if (!is.na(child)) {
        # Add child node to parent
        child_node <- parent_node$AddChild(child)

        # Recursively add grandchildren (children of this child)
        add_to_tree(child_node, child)
      }
    }
  }

  # Start building the tree from the main RMD file
  main_rmd <- dependencies$parent[1]  # Assumes first entry in the data is the main RMD
  main_node <- root$AddChild(main_rmd)
  add_to_tree(main_node, main_rmd)

  return(root)
}

# Generate the tree
simple_tree <- build_simple_tree(rmd_dependencies)

# Print the tree structure
print(simple_tree)

