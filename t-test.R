library(readxl)     # For reading Excel files
library(plyr)  # Needed for arrange()
library(ggplot2)
library(tidyverse)  # Includes ggplot2 and dplyr

filepth_a = paste(toString(getwd()), "/data/Classical-General-Normalized.xlsx", sep="")
filepth_b = paste(toString(getwd()), "/data/Video-General-Normalized.xlsx", sep="")

# Read and prepare dataset A
dataset_a <- read_excel(filepth_a) %>%
  rename(
    reaction_time = `buzz time`,
    actual_time = `speaking time`
  ) %>%
  mutate(
    reaction = (reaction_time + actual_time) / 2,  # Calculate average reaction
    delay = (actual_time - reaction_time), # Calculate delay time between button press and answering
    dataset = "classical"                          # Label the dataset
  )

# Read and prepare dataset B
dataset_b <- read_excel(filepth_b) %>%
  rename(
    reaction_time = `buzz time`,
    actual_time = `speaking time`
  ) %>%
  mutate(
    reaction = (reaction_time + actual_time) / 2,
    delay = (actual_time - reaction_time), # Calculate delay time between button press and answering
    dataset = "video"
  )

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
