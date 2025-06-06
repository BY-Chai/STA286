# Load required libraries
library(tidyverse)  # Includes ggplot2 and dplyr
library(ggridges)   # For split violin plots
library(readxl)     # For reading Excel files
library(plyr)  # Needed for arrange()

library(ggplot2)

# 1. First define a proper split violin geom
geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                              position = "identity", ..., draw_quantiles = NULL,
                              trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  
  # Define the Geom class
  GeomSplitViolin <- ggproto(
    "GeomSplitViolin", GeomViolin,
    draw_group = function(self, data, ..., draw_quantiles = NULL) {
      # Calculate violin boundaries
      data <- transform(data,
                        xminv = x - violinwidth * (x - xmin),
                        xmaxv = x + violinwidth * (xmax - x))
      
      # Split by group (odd/even)
      grp <- data[1, "group"]
      newdata <- plyr::arrange(
        transform(data, x = if (grp %% 2 == 1) xminv else xmaxv),
        if (grp %% 2 == 1) y else -y
      )
      
      # Close the polygon
      newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
      
      # Draw quantiles if requested
      if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
        quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
        aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
        ggplot2:::ggname("geom_split_violin",
                         grid::grobTree(
                           GeomPolygon$draw_panel(newdata, ...),
                           GeomPath$draw_panel(quantiles, ...)
                         ))
      } else {
        ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
      }
    }
  )
  
  layer(
    geom = GeomSplitViolin, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles,
                  na.rm = na.rm, ...)
  )
}


# 1. Read and prepare the data
dataset_a <- read_excel("C:\\Users\\hina v2\\Downloads\\General Classial Dataset.xlsx") %>% 
  rename(
    reaction_time = `buzz time`,
    actual_time = `speaking time`
  ) %>%
  mutate(
    reaction = (reaction_time + actual_time) / 2,  # Calculate average reaction
    dataset = "classical"                  # Label the dataset
  )

dataset_b <- read_excel("C:\\Users\\hina v2\\Downloads\\General Video Dataset.xlsx") %>% 
  rename(
    reaction_time = `buzz time`,
    actual_time = `speaking time`
  ) %>%
  mutate(
    reaction = (reaction_time + actual_time) / 2,
    dataset = "video"
  )

# Combine both datasets
combined_data <- bind_rows(dataset_a, dataset_b)

# 2. Create the split violin plot
ggplot(combined_data, aes(x = 'General Dataset', y = reaction, fill = dataset)) +
  # Split violin plot (half for each dataset)
  geom_split_violin(alpha = 0.7, width = 0.8) +
  
  # Add error bars for delay (uncertainty)
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar",
    width = 0.1,
    position = position_dodge(0.3),
    aes(group = dataset)
  ) +
  
  # Add mean points
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(0.3),
    aes(group = dataset)
  ) +
  
  # Custom colors
  scale_fill_manual(values = c("classical" = "#1f77b4", 
                               "video" = "#ff7f0e")) +
  
  # Labels and theme
  labs(
    x = "Dataset",
    y = "Reaction Time (seconds)",
    title = "Comparison of Reaction Times Between Datasets"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
