# Import 
source("Dataload.R")

alpha = 0.05

# Compute beta (Type II error) for reaction_time and answer_time columns
calculate_beta <- function(column_a, column_b, alpha) {
    # Sample sizes
    n1 <- length(column_a)
    n2 <- length(column_b)
    # Calculate sample means
    mean_a <- mean(column_a)
    mean_b <- mean(column_b)
    s1_sq <- var(column_a, na.rm = TRUE)
    s2_sq <- var(column_b, na.rm = TRUE)
    
    # Standard error of difference
    se_diff <- sqrt(s1_sq / n1 + s2_sq / n2)

    d0 = mean_a - mean_b
    delta = d0/se_diff # Null hypothesis difference in means (d0 = 0)

    # Degrees of freedom using Welch-Satterthwaite approximation
    df <- (s1_sq / n1 + s2_sq / n2)^2 / 
        ((s1_sq / n1)^2 / (n1) + (s2_sq / n2)^2 / (n2))

    # Critical t value (two-tailed)
    t_crit <- qt(1 - alpha / 2, df)

    # Noncentrality parameter
    ncp <- (delta - d0) / se_diff

    # Compute beta using noncentral t-distribution
    beta <- pt(t_crit, df, ncp) - pt(-t_crit, df, ncp)
    return(beta)
}

# Calculate beta for reaction_time
beta_reaction_time <- calculate_beta(dataset_a$reaction_time, dataset_b$reaction_time, alpha)

# Calculate beta for answer_time
beta_answer_time <- calculate_beta(dataset_a$actual_time, dataset_b$actual_time, alpha)

# Output
cat("Beta (Type II error rate) for reaction_time:", round(beta_reaction_time, 4), "\n")
cat("Beta (Type II error rate) for answer_time:", round(beta_answer_time, 4), "\n")
