source("src/Dataload.R")

plot_histogram_with_stats <- function(data, column, title, xlab, bar_color) {
  hist_data <- hist(data[[column]], breaks = "Sturges", plot = FALSE)
  mean_value <- mean(data[[column]], na.rm = TRUE)
  sd_value <- sd(data[[column]], na.rm = TRUE)
  plot(
    hist_data,
    main = title,
    xlab = xlab,
    col = bar_color,
    border = "black"
  )
  # Add mean as a red vertical line
  abline(v = mean_value, col = "red", lwd = 2)
  # Add annotation for the mean
  text(mean_value, max(hist_data$counts) * 0.9, 
       labels = paste("Mean =", round(mean_value, 2)), 
       col = "red", pos = 4)
  # Add annotation for the standard error
  se_value <- sd_value / sqrt(length(data[[column]]))
  text(mean_value, max(hist_data$counts) * 0.8, 
    labels = paste("SE =", round(se_value, 2)), 
    col = "red", pos = 4)
  # Shade the region corresponding to one standard deviation
  rect(mean_value - sd_value, 0, mean_value + sd_value, max(hist_data$counts), 
       col = rgb(1, 0, 0, 0.2), border = NA)
  print(paste("Mean of", column, "for", data$dataset[1], "dataset:", mean_value))
  print(paste("Standard deviation of", column, "for", data$dataset[1], "dataset:", sd_value))
}

# Plot histograms
plot_histogram_with_stats(dataset_a, "reaction_time", 
                          "Classical Dataset Reaction Time", "Normalized Reaction Time (s/s)", "#1f77b4")
plot_histogram_with_stats(dataset_b, "reaction_time", 
                          "Video Dataset Reaction Time", "Normalized Reaction Time (s/s)", "#ff7f0e")

plot_histogram_with_stats(dataset_a, "actual_time", 
                          "Classical Dataset Answering Time", "Normalized Answering Time (s/s)", "#1f77b4")
plot_histogram_with_stats(dataset_b, "actual_time", 
                          "Video Dataset Answering Time", "Normalized Answering Time (s/s)", "#ff7f0e")

plot_histogram_with_stats(dataset_a, "delay", 
                          "Classical Dataset Delay Time", "Normalized Delay Time (s/s)", "#1f77b4")
plot_histogram_with_stats(dataset_b, "delay", 
                          "Video Dataset Delay Time", "Normalized Delay Time (s/s)", "#ff7f0e")

plot_histogram_with_stats(dataset_a, "reaction", 
                          "Classical Dataset Agg Time", "Agg Time", "#1f77b4")
plot_histogram_with_stats(dataset_b, "reaction", 
                          "Video Dataset Agg Time", "Agg Time", "#ff7f0e")


# T-test function in R, where x and y are the independent samples to be compared.
# Perform t-tests between the two datasets
condition <- "less"  # Set the condition for the t-test

# Reaction Time Comparison
t_reaction_time <- t.test(
  dataset_a$reaction_time,
  dataset_b$reaction_time,
  alternative = condition,
  var.equal = FALSE,          # Welch's t-test (recommended if variances may differ)
  conf.level = 0.95
)

# Actual Time Comparison
t_actual_time <- t.test(
  dataset_a$actual_time,
  dataset_b$actual_time,
  alternative = condition,
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

# Calculate covariance between reaction time and actual time for each dataset
cov_classical <- cov(dataset_a$reaction_time, dataset_a$actual_time, use = "complete.obs")
cov_video <- cov(dataset_b$reaction_time, dataset_b$actual_time, use = "complete.obs")

# Display covariance results
cat("Reaction-answering time covariance: Classical dataset", cov_classical, "\n")
cat("Reaction-answering time covariance: Video game dataset", cov_video, "\n")