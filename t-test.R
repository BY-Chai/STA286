library(readxl)     # For reading Excel files
library(plyr)  # Needed for arrange()
library(ggplot2)
library(tidyverse)  # Includes ggplot2 and dplyr

# Define the working directory as a constant
working_directory <- toString(getwd())

# Define file paths for dataset A and B as arrays
filepaths_a <- c("Classical-General-Normalized.xlsx", "Classical-Intermediate-Normalized.xlsx")
filepaths_b <- c("Video-General-Normalized.xlsx", "Video-Intermediate-Normalized.xlsx")

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

print(
  hist(dataset_a$reaction_time, breaks = 10, main = "Classical Dataset Reaction Time", xlab = "Reaction Time")
)
print(
  hist(dataset_b$reaction_time, breaks = 10, main = "Video Dataset Reaction Time", xlab = "Reaction Time")
)

print(
  hist(dataset_a$actual_time, breaks = 10, main = "Classical Dataset Answering Time", xlab = "Reaction Time")
)
print(
  hist(dataset_b$actual_time, breaks = 10, main = "Video Dataset Answering Time", xlab = "Reaction Time")
)

print(
  hist(dataset_a$delay, breaks = 10, main = "Classical Dataset Delay Time", xlab = "Reaction Time")
)
print(
  hist(dataset_b$delay, breaks = 10, main = "Video Dataset delay Time", xlab = "Reaction Time")
)


# T-test function in R, where x and y are the independent samples to be compared.
# Perform t-tests between the two datasets

# Reaction Time Comparison
t_reaction_time <- t.test(
  dataset_a$reaction_time,
  dataset_b$reaction_time,
  alternative = "two.sided",  # Test if means are different
  var.equal = FALSE,          # Welch's t-test (recommended if variances may differ)
  conf.level = 0.95
)

# Actual Time Comparison
t_actual_time <- t.test(
  dataset_a$actual_time,
  dataset_b$actual_time,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

# Display results
print("T-test: Reaction Time")
print(t_reaction_time)

print("T-test: Actual Time")
print(t_actual_time)

if (t_reaction_time$p.value < 0.05) {
  cat("Reaction Time: Significant difference between classical and video (p =", t_reaction_time$p.value, ")\n")
} else {
  cat("Reaction Time: No significant difference (p =", t_reaction_time$p.value, ")\n")
}

if (t_actual_time$p.value < 0.05) {
  cat("Actual Time: Significant difference between classical and video (p =", t_actual_time$p.value, ")\n")
} else {
  cat("Actual Time: No significant difference (p =", t_actual_time$p.value, ")\n")
}
