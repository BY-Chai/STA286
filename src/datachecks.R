# Import 
source("src/Dataload.R")

print(shapiro.test(dataset_a$reaction_time))
print(shapiro.test(dataset_a$actual_time))
print(shapiro.test(dataset_b$reaction_time))
print(shapiro.test(dataset_b$actual_time))