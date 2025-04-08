# Overlapping plots
plot_histogram_with_stats <- function(data1, data2, column, title, xlab, color1, color2, alpha = 0.5) {
  hist_data1 <- hist(data1[[column]], breaks = 5, plot = FALSE)
  hist_data2 <- hist(data2[[column]], breaks = 5, plot = FALSE)
  
  # Determine the range for the x-axis and y-axis
  x_range <- range(c(hist_data1$breaks, hist_data2$breaks))
  y_range <- range(c(hist_data1$counts, hist_data2$counts))
  
  # Plot the first histogram
  plot(
    hist_data1,
    main = title,
    xlab = xlab,
    xlim = x_range,
    ylim = y_range,
    col = adjustcolor(color1, alpha.f = alpha),
    border = "black"
  )
  
  # Overlay the second histogram
  plot(
    hist_data2,
    add = TRUE,
    col = adjustcolor(color2, alpha.f = alpha),
    border = "black"
  )
  
  # Add mean and standard error annotations for both datasets
  mean1 <- mean(data1[[column]], na.rm = TRUE)
  mean2 <- mean(data2[[column]], na.rm = TRUE)
  sd1 <- sd(data1[[column]], na.rm = TRUE)
  sd2 <- sd(data2[[column]], na.rm = TRUE)
  
  abline(v = mean1, col = color1, lwd = 2, lty = 2)
  abline(v = mean2, col = color2, lwd = 2, lty = 2)
  
  text(mean1, max(y_range) * 0.9, 
       labels = paste("Mean =", round(mean1, 2)), 
       col = color1, pos = 4)
  text(mean2, max(y_range) * 0.8, 
       labels = paste("Mean =", round(mean2, 2)), 
       col = color2, pos = 4)
}

# Plot overlapping histograms
plot_histogram_with_stats(dataset_a, dataset_b, "reaction_time", 
                          "Reaction Time Comparison", "Normalized Reaction Time (% of question)", "#1f77b4", "#ff7f0e", alpha = 0.5)
plot_histogram_with_stats(dataset_a, dataset_b, "actual_time", 
                          "Answering Time Comparison", "Normalized Answering Time (% of quesiton)", "#1f77b4", "#ff7f0e", alpha = 0.5)
plot_histogram_with_stats(dataset_a, dataset_b, "delay", 
                          "Delay Time Comparison", "Delay Time", "#1f77b4", "#ff7f0e", alpha = 0.5)
plot_histogram_with_stats(dataset_a, dataset_b, "reaction", 
                          "Agg Time Comparison", "Agg Time", "#1f77b4", "#ff7f0e", alpha = 0.5)