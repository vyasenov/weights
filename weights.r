# load libraries
library(survey)
rm(list=ls())
set.seed(681)

# generate data
n <- 100000

data <- data.frame(
  x = rnorm(n),
  prob_selection = runif(n, .1, .9),
  freq_weight = rpois(n, 3)
)
data$samp_weight <- 1 / data$prob_selection

# declare "survey" designs
design_unweight <- svydesign(ids = ~1, data = data, weights = ~1)
design_samp <- svydesign(ids = ~1, data = data, weights = ~samp_weight)
design_freq <- svydesign(ids = ~1, data = data, weights = ~freq_weight)

# estimate weighted and unweighted means
mean_unweight <- svymean(~x, design_unweight)
mean_samp <- svymean(~x, design_samp)
mean_freq <- svymean(~x, design_freq)

# print results
print(round(mean_unweight, digits=3))
print(round(mean_samp, digits=3))
print(round(mean_freq, digits=3))
