# Import 
source("t-test.R")
# Define the working directory as a constant
working_directory <- toString(getwd())

# Define file paths for dataset A and B as arrays
filepaths_a <- c("Classical Distinct Normalized.xlsx")
filepaths_b <- c("Video Distinct Normalized.xlsx")

# Generate full file paths for dataset A and B
filepaths_a <- generate_filepaths(filepaths_a, working_directory)
filepaths_b <- generate_filepaths(filepaths_b, working_directory)

# Read and prepare datasets A and B
dataset_a <- prepare_dataset(filepaths_a, "classical")
dataset_b <- prepare_dataset(filepaths_b, "video")

print(shapiro.test(dataset_a$reaction_time))
print(shapiro.test(dataset_a$actual_time))
print(shapiro.test(dataset_b$reaction_time))
print(shapiro.test(dataset_b$actual_time))


# Function to calculate required sample size
calculate_sample_size <- function(alpha, power, effect_size) {
    # Calculate the critical t-value for the given alpha
    t_alpha <- qt(1 - alpha / 2, df = Inf)
    
    # Calculate the noncentrality parameter for the given power
    t_power <- qt(power, df = Inf, lower.tail = TRUE)
    
    # Calculate the required sample size
    n <- (t_alpha + t_power)^2 / effect_size^2
    return(ceiling(n)) # Round up to the nearest whole number
}

# Define parameters
alpha <- 0.05
power <- 0.95
effect_size <- 0.5 # Example effect size (Cohen's d)

# Calculate and print the required sample size
required_sample_size <- calculate_sample_size(alpha, power, effect_size)
print(paste("Required sample size per group:", required_sample_size))