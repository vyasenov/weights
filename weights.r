
# load packages and clear workspace
library(survey)
rm(list=ls())
set.seed(731)
n <- 50

# create data
data <- data.frame(
  x = rnorm(n),
  prob_selection = runif(n),
  freq_weight = rpois(n, 3)
)

data$y <- 2 + 3 * data$x + rnorm(n)
data$samp_weight <- 1 / data$prob_selection

# declare survey designs
design_unweight <- svydesign(ids = ~1, data = data, weights = ~1)
design_samp <- svydesign(ids = ~1, data = data, weights = ~samp_weight)
design_freq <- svydesign(ids = ~1, data = data, weights = ~freq_weight)

# print means and standard errors
print(design_unweight)
print(design_samp)
print(design_freq)

# fit linear models
fit_unweight <- lm(y ~ x, data = data)
fit_samp <- svyglm(y ~ x, design = design_samp)
fit_freq <- svyglm(y ~ x, design = design_freq)

# print linear models results
summary(fit_unweight)
summary(fit_samp)
summary(fit_freq)