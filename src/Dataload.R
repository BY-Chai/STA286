library(readxl)     # For reading Excel files
library(plyr)  # Needed for arrange()
library(ggplot2)
library(tidyverse)  # Includes ggplot2 and dplyr

# Define the working directory as a constant
working_directory <- toString(getwd())

# Define file paths for dataset A and B as arrays
filepaths_a <- c("Classical Distinct Normalized.xlsx")
filepaths_b <- c("Video Distinct Normalized.xlsx")

# Function to generate full file paths
generate_filepaths <- function(filenames, directory) {
  sapply(filenames, function(filename) paste(directory, "/data/", filename, sep = ""))
}

# Generate full file paths for dataset A and B
filepaths_a <- generate_filepaths(filepaths_a, working_directory)
filepaths_b <- generate_filepaths(filepaths_b, working_directory)

# Function to read and prepare datasets
prepare_dataset <- function(filepaths, dataset_label) {
  dataset <- lapply(filepaths, function(filepath) {
    read_excel(filepath) %>%
      drop_na() %>%  # Drop rows with NA values in any column
      rename(
        reaction_time = `buzz time`,
        actual_time = `speaking time`
      ) %>%
      mutate(
        reaction = (reaction_time + actual_time) / 2,  # Calculate average reaction
        delay = (actual_time - reaction_time),         # Calculate delay time
        dataset = dataset_label                        # Label the dataset
      )
  }) %>%
    bind_rows()  # Combine all files into a single dataset
  return(dataset)
}

# Read and prepare datasets A and B
dataset_a <- prepare_dataset(filepaths_a, "classical")
dataset_b <- prepare_dataset(filepaths_b, "video")

# Check column names
print(colnames(dataset_a))