# Load required packages
library(readxl)   # For reading Excel files
library(ggplot2)  # For creating plots
library(dplyr)    # For data manipulation

# Read the Excel file
# Replace "your_file.xlsx" with your actual file path and sheet name if needed
data <- read_excel("C:\\Users\\hina v2\\Downloads\\General Classial Dataset.xlsx") %>%
  mutate(
    # Calculate reaction time as average of buzzer and actual time
    reaction_time = (`buzz time` + `speaking time`) / 2,
    # Use delay as uncertainty
    uncertainty = delay
  )

# Create basic density plot
basic_density <- ggplot(data, aes(x = reaction_time)) +
  geom_density(fill = "#4e79a7", alpha = 0.7, color = NA) +
  labs(title = "Density Plot of Reaction Times with Uncertainty",
       x = "Reaction Time (Average of Buzzer and Actual Time)",
       y = "Density") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

# Enhanced version showing uncertainty as shaded area
enhanced_density <- ggplot(data, aes(x = reaction_time)) +
  # Main density plot
  geom_density(fill = "#4e79a7", alpha = 0.5, color = NA) +
  
  # Add uncertainty representation (using geom_ribbon)
  geom_ribbon(
    stat = "density",
    aes(ymin = after_stat(density) - uncertainty/max(uncertainty)*0.1,
        ymax = after_stat(density) + uncertainty/max(uncertainty)*0.1),
    fill = "#f28e2b", alpha = 0.3
  ) +
  
  labs(title = "Reaction Time Density with Uncertainty Bands",
       x = "Reaction Time",
       y = "Density",
       caption = "Shaded area represents uncertainty based on delay values") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic"))

# Display both plots
basic_density
enhanced_density

# Save plots if needed
# ggsave("basic_density_plot.png", basic_density, width = 8, height = 6, dpi = 300)
# ggsave("enhanced_density_plot.png", enhanced_density, width = 8, height = 6, dpi = 300)