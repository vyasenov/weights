
rm(list=ls())
set.seed(4108)
library(survey)

# Create a sample dataset
data <- data.frame(
  id = 1:10,
  value = rnorm(10)
)

# Probability of selection for each unit
prob_selection <- runif(10, 0.1, 0.5)

# Sampling weights
data$samp_weight <- 1 / prob_selection

# Frequency weights (assuming some values appear multiple times)
data$freq_weight <- sample(1:5, 10, replace = TRUE)

print(data)

################
################
################

# CREATE SURVEY DESIGNS
design_samp <- svydesign(ids = ~1, data = data, weights = ~samp_weight)
design_freq <- svydesign(ids = ~1, data = data, weights = ~freq_weight)

# CALCUALTE WEIGHTED MEANS AND STANDARD ERRORS
weighted_mean_samp <- svymean(~value, design_samp)
weighted_mean_freq <- svymean(~value, design_freq)

# MANUALLY CALCUALTE WEIGHTED MEANS
weighted_mean_samp2 <- sum(data$value * data$samp_weight) / sum(data$samp_weight)  
weighted_mean_freq2 <- sum(data$value * data$freq_weight) / sum(data$freq_weight) 

# MANUALLY CALCUALTE STANDARD ERRORS
weighted_var_samp <- sum(data$samp_weight * (data$value - weighted_mean_samp2)^2) / sum(data$samp_weight)
weighted_var_freq <- sum(data$freq_weight * (data$value - weighted_mean_freq2)^2) / sum(data$freq_weight)

std_error_samp <- sqrt(weighted_var_samp / nrow(data))
std_error_freq <- sqrt(weighted_var_freq / sum(data$freq_weight))


################
################ PRINT RESULTS
################

# MEANS AND STANDARD ERRORS
print(weighted_mean_samp)
print(weighted_mean_freq)

# MANUAL MEANS
print(weighted_mean_samp2)
print(weighted_mean_freq2)

# MANUALS STANDARD ERRORS
print(std_error_samp)
print(std_error_freq)
