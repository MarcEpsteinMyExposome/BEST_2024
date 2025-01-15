library(stringr)
library(dplyr)
library(tibble)
library(data.tree)

# Define directories
root_directory <- getwd()          # Root directory for RMD files
r_directory <- file.path(root_directory, "R")  # R subdirectory

# Parse dependencies from both RMD and R files
parse_file_dependencies <- function(file_path, env = new.env()) {
  # Determine the full path to the file
  is_r_file <- grepl("\\.R$", file_path, ignore.case = TRUE)
  full_path <- if (is_r_file) {
    file.path(r_directory, file_path)  # R files in the R subdirectory
  } else {
    file.path(root_directory, file_path)  # RMD files in the root directory
  }

  # Check if the file exists
  if (!file.exists(full_path)) {
    warning(paste("File does not exist:", full_path))
    return(tibble(parent = file_path, child = NA))
  }

  # Read the content of the file
  content <- readLines(full_path, warn = FALSE)

  # Initialize list to store matches
  children <- c()

  # Regex patterns
  child_pattern_rmd <- "child\\s*=\\s*['\"]([^'\"]+)['\"]"
  source_pattern <- "source\\((.*?)\\)"
  setRdirectory_pattern <- "setRdirectory\\(['\"]([^'\"]+)['\"]\\)"

  # Iterate through lines to match patterns
  for (line in content) {
    # Match RMD child files
    child_match_rmd <- str_match(line, child_pattern_rmd)
    if (!is.na(child_match_rmd[2])) {
      children <- c(children, child_match_rmd[2])
    }

    # Match all source() calls
    source_match <- str_match(line, source_pattern)
    if (!is.na(source_match[2])) {
      # Extract the argument inside source()
      source_arg <- str_trim(source_match[2])

      # Handle dynamic path construction with setRdirectory
      setRdirectory_match <- str_match(source_arg, setRdirectory_pattern)
      if (!is.na(setRdirectory_match[2])) {
        resolved_path <- file.path("R", setRdirectory_match[2])
        children <- c(children, resolved_path)
      } else if (grepl("^[\"'].*[\"']$", source_arg)) {
        # Literal path
        resolved_path <- gsub('["\']', "", source_arg)
        if (grepl("\\.R$", resolved_path, ignore.case = TRUE) && !startsWith(resolved_path, r_directory)) {
          resolved_path <- file.path("R", resolved_path)
        }
        children <- c(children, resolved_path)
      } else if (exists(source_arg, envir = env)) {
        # Resolve variable to its value
        resolved_value <- get(source_arg, envir = env)
        if (grepl("\\.R$", resolved_value, ignore.case = TRUE) && !startsWith(resolved_value, r_directory)) {
          resolved_value <- file.path("R", resolved_value)
        }
        children <- c(children, resolved_value)
      } else {
        # Treat as unresolved dynamic dependency
        children <- c(children, source_arg)
      }
    }

    # Evaluate assignments to track variables in the environment
    if (grepl("<-|=", line)) {
      tryCatch({
        eval(parse(text = line), envir = env)
      }, error = function(e) {})
    }
  }

  # Return data frame of matches
  if (length(children) > 0) {
    return(tibble(parent = rep(file_path, length(children)), child = children))
  } else {
    return(tibble(parent = file_path, child = NA))
  }
}

# Recursive function to build the dependency tree
build_dependency_tree <- function(file_path, visited = character(), env = new.env()) {
  # Avoid re-parsing files
  if (file_path %in% visited) return(tibble())
  visited <- c(visited, file_path)

  # Parse the current file for dependencies
  dependencies <- parse_file_dependencies(file_path, env)

  # Initialize result with current dependencies
  result <- dependencies

  # Recursively parse each child
  for (child in dependencies$child[!is.na(dependencies$child)]) {
    child_dependencies <- build_dependency_tree(child, visited, env)
    result <- bind_rows(result, child_dependencies)
  }

  result
}

# Build the tree structure using data.tree
build_tree <- function(dependencies) {
  root <- Node$new("Project Root")

  add_to_tree <- function(parent_node, parent_name) {
    # Find children of the current parent
    children <- dependencies %>% filter(parent == parent_name) %>% pull(child)

    for (child in children) {
      if (!is.na(child)) {
        # Add child node to parent
        child_node <- parent_node$AddChild(child)
        # Recursively add further children
        add_to_tree(child_node, child)
      }
    }
  }

  # Start building from the root
  main_file <- dependencies$parent[1]
  main_node <- root$AddChild(main_file)
  add_to_tree(main_node, main_file)

  root
}

# Example: Start parsing from the main RMD file
main_rmd_file <- "MyExposome_1527_v6.Rmd"
all_dependencies <- build_dependency_tree(main_rmd_file)

# Build and display the tree
dependency_tree <- build_tree(all_dependencies)
print(dependency_tree)
