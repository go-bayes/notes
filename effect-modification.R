
# start here
set.seed(123) # seed

# simulate data
n_sample <- 1000
n_population <- 10000
p_z_sample <- 0.1
p_z_population <- 0.5

#sample data
z_sample <- rbinom(n_sample, 1, p_z_sample)
a_sample <- rbinom(n_sample, 1, 0.5)

# Coefficients
beta_a = 1  # Coefficient for the treatment effect
beta_az = 1.5  # Coefficient for the interaction between A and Z

# simulating the outcome with explicit coefficients
y_sample <- beta_a * a_sample + beta_az * (z_sample * a_sample) + rnorm(n_sample)


# pop data (for verification)
z_population <- rbinom(n_population, 1, p_z_population)
a_population <- rbinom(n_population, 1, 0.5)


y_population <- beta_a * a_population + beta_az * (z_population*a_population) + rnorm(n_population)

# regression standardisation on sample
model_sample <- lm(y_sample ~ a_sample + z_sample + a_sample*z_sample)
summary(model_sample)

# ATE estimation from sample
ate_sample <- mean(predict(model_sample, newdata=data.frame(a_sample=1, z_sample=z_sample))) - 
              mean(predict(model_sample, newdata=data.frame(a_sample=0, z_sample=z_sample)))

# direct comparison with population (hypothetical scenario for verification)
model_population <- lm(y_population ~ a_population + z_population + a_population*z_population)
summary(model_population)

# now try weights 

# assuming the previously defined variables and model
# calculate weights for each observation in the sample based on Z
weight_z_1 = p_z_population / p_z_sample # weight for observations with Z=1

weight_z_0 = (1 - p_z_population) / (1 - p_z_sample) # weight for observations with Z=0

# assign weights to each observation in the sample
weights = ifelse(z_sample == 1, weight_z_1, weight_z_0)
weights
# adjusted ATE estimation using weighted regression
model_weighted <- lm(y_sample ~ a_sample + z_sample + a_sample*z_sample, weights = weights)
summary(model_weighted)
summary(model_sample)
# three models look very close 
parameters::model_parameters(model_weighted )
parameters::model_parameters(model_population)
parameters::model_parameters(model_sample)


# Incorrect ATE from the weighted model
ate_weighted <- mean(predict(model_weighted, newdata=data.frame(a_sample=1, z_sample=z_sample), type="response", se.fit=FALSE)) - 
                mean(predict(model_weighted, newdata=data.frame(a_sample=0, z_sample=z_sample), type="response", se.fit=FALSE))

# Output the weighted ATE estimate
ate_weighted
ate_population
ate_sample

# Adjusted ATE estimation using weighted regression
model_weighted <- lm(y_sample ~ a_sample + z_sample + a_sample*z_sample, weights = weights)
summary(model_weighted)


# Calculate predictions for each individual in the sample under both treatment conditions
predicted_y_treatment = predict(model_weighted, newdata=data.frame(a_sample=1, z_sample=z_sample), type="response")
predicted_y_control = predict(model_weighted, newdata=data.frame(a_sample=0, z_sample=z_sample), type="response")

# calculate the weighted ATE, considering the sample imbalance in Z
# here, the weights adjust for the imbalance directly in the ATE calculation
ate_adjusted = sum(weights * (predicted_y_treatment - predicted_y_control)) / sum(weights)

# Output the adjusted weighted ATE
ate_adjusted
ate_population
ate_sample


#### By hand interaction 

set.seed(123)
# simulate data
n_sample <- 10000
n_population <- 10000
p_z_sample <- 0.1
p_z_population <- 0.5

#sample data
z_sample <- rbinom(n_sample, 1, p_z_sample)
a_sample <- rbinom(n_sample, 1, 0.5)

# Coefficients
beta_a = 1  # Coefficient for the treatment effect
beta_z = 2.5  # Coefficient for Z
beta_az = 0.5  # Coefficient for interaction 


# simulating the outcome with explicit coefficients
y_sample <- beta_a * a_sample + beta_z * z_sample + beta_az * (a_sample *  z_sample) +
rnorm(n_sample,  mean = 0, sd = 1)

y_sample

# pop data (for verification)
z_population <- rbinom(n_population, 1, p_z_population)
a_population <- rbinom(n_population, 1, 0.5)

y_population <- beta_a * a_population + beta_z * z_population + 
beta_az * (a_population *  z_population)  +  rnorm(n_population,  mean = 0, sd = 1)

mean(y_sample)
mean(y_population)

# simulate weighting based on Z distribution difference
weight_z_1 = p_z_population / p_z_sample # adjust weight for Z=1
weight_z_0 = (1 - p_z_population) / (1 - p_z_sample) # adjust weight for Z=0
weights = ifelse(z_sample == 1, weight_z_1, weight_z_0)
weights
length(weights)


data_sample = data.frame(a_sample, y_sample, z_sample)

library(tidyverse)
data_sample = data_sample |>
    mutate(weights = ifelse( z_sample == 1, weight_z_1, weight_z_0))
tail(data_sample)

data_population = data.frame (a_population, y_population, z_population)


# regression standardisation on sample
model_sample <- glm(y_sample ~ a_sample * z_sample, data = data_sample)
summary(model_sample)
    
#  weighted sample
model_weighted_sample <- glm(y_sample ~  a_sample  * z_sample, data = data_sample, weights = weights)
summary(model_weighted_sample)

# populations
model_population <- glm(y_population ~  a_population  * z_population, data = data_population)
summary(model_population)

# now try weights 

# three models look very close 
parameters::model_parameters(model_sample)
parameters::model_parameters(model_weighted_sample )
parameters::model_parameters(model_population)


library(stdReg)


fit_std_sample <- stdReg::stdGlm(model_sample, data = data_sample, X = "a_sample")

summary(fit_std_sample, 
contrast = "difference", 
reference = 0)

fit_std_weighted_sample <- stdReg::stdGlm( model_weighted_sample, data = data_sample, X = "a_sample")

summary(fit_std_sample, 
contrast = "difference", 
reference = 0)


fit_std_population <- stdReg::stdGlm( model_population, data = data_population, X = "a_population")
summary(fit_std_population, 
contrast = "difference", 
reference = 0)


fit_std_weighted_sample_weights <- stdReg::stdGlm( model_weighted_sample, data = data_sample, 
X = "a_sample")

summary(fit_std_weighted_sample_weights, 
contrast = "difference", 
reference = 0)




### BEGIN 
rm(list = ls())
library(stdReg)
simulate_ate_data_with_weights_and_noise <- function(n_sample = 10000, n_population = 10000,
                                                     p_z_sample = 0.1, 
                                                     p_z_population = 0.5,
                                                     beta_a = 1, 
                                                     beta_z = 2.5, 
                                                     beta_az = 0.5,
                                                     noise_sd = 1) {  # 
  # Generate sample data
  z_sample <- rbinom(n_sample, 1, p_z_sample)
  a_sample <- rbinom(n_sample, 1, 0.5)
  y_sample <- beta_a * a_sample + beta_z * z_sample + beta_az * (a_sample * z_sample) +
    rnorm(n_sample, mean = 0, sd = noise_sd)  # Use noise_sd for the noise term
  sample_data <- data.frame(y_sample, a_sample, z_sample)
  
  # Generate population data
  z_population <- rbinom(n_population, 1, p_z_population)
  a_population <- rbinom(n_population, 1, 0.5)
  y_population <- beta_a * a_population + beta_z * z_population + 
    beta_az * (a_population * z_population) + rnorm(n_population, mean = 0, sd = noise_sd)  # Use noise_sd for the noise term
  population_data <- data.frame(y_population, a_population, z_population)
  
  # Simulate weighting based on Z distribution difference
  weight_z_1 = p_z_population / p_z_sample # adjust weight for Z=1
  weight_z_0 = (1 - p_z_population) / (1 - p_z_sample) # adjust weight for Z=0
  weights <- ifelse(z_sample == 1, weight_z_1, weight_z_0)
  
  # Add weights to sample_data
  sample_data$weights = weights
  
  # Return list of data frames and weights
  list(sample_data = sample_data, population_data = population_data)
}

# example
data <- simulate_ate_data_with_weights_and_noise(n_sample = 10000, 
  n_population = 10000,
  p_z_sample = 0.1, 
  p_z_population = 0.5,
  beta_a = .1, 
  beta_z = 0, 
  beta_az = .5,
  noise_sd = 0.5)  # Specifying a lower noise level

# Access the generated sample data with weights and population data
sample_data <- data$sample_data
population_data <- data$population_data
# check imbalance 
table(sample_data$z_sample)

head(population_data$z_population)

# Use the function to generate data and weights
# data_with_weights <- simulate_ate_data_with_weights(n_sample = 10000, n_population = 10000,
#                                                     p_z_sample = 0.1, p_z_population = 0.5,
#                                                     beta_a = 0, beta_z = 2.5, beta_az = 0.15)

# fit models

# regression standardisation on sample
model_sample <- glm(y_sample ~ a_sample * z_sample, data = sample_data)
summary(model_sample)
    
#  weighted sample
model_weighted_sample <- glm(y_sample ~  a_sample  * z_sample, data = sample_data, weights = weights)
summary(model_weighted_sample)

# populations
model_population <- glm(y_population ~  a_population  * z_population, data = population_data)
summary(model_population)

# regression coefs all very similar
parameters::model_parameters(model_sample)
parameters::model_parameters(model_weighted_sample )
parameters::model_parameters(model_population)



## regression standardisation 
library(stdReg)
fit_std_sample <- stdReg::stdGlm( model_sample, data = sample_data, X = "a_sample")
fit_std_weighted_sample <- stdReg::stdGlm( model_weighted_sample, data = sample_data, X = "a_sample")
fit_std_population <- stdReg::stdGlm( model_population, data = population_data, X = "a_population")

# sample 

summary(
  fit_std_sample, 
  contrast = "difference", 
  reference = 0)

# population
summary(
  fit_std_population, 
  contrast = "difference", 
  reference = 0
  )

#weighted sample
summary(
  fit_std_weighted_sample, 
  contrast = "difference", 
  reference = 0
)

############ try simple 


# regression standardisation on sample
model_sample_main <- glm(y_sample ~ a_sample, data = sample_data)
summary(model_sample_main)
    
#  weighted sample
model_weighted_sample_main <- glm(y_sample ~  a_sample, data = sample_data, weights = weights)
summary(model_weighted_sample_main)

# populations
model_population_main <- glm(y_population ~  a_population, data = population_data)
summary(model_population_main)

# regression coefs all very similar
parameters::model_parameters(model_sample_main) 
parameters::model_parameters(model_weighted_sample_main)
parameters::model_parameters(model_population_main)

## regression standardisation 
library(stdReg)
fit_std_sample_main <- stdReg::stdGlm( model_sample_main, data = sample_data, X = "a_sample")
fit_std_weighted_sample_main <- stdReg::stdGlm( model_weighted_sample_main, data = sample_data, X = "a_sample")
fit_std_population_main <- stdReg::stdGlm( model_population_main, data = population_data, X = "a_population")


# doesn't matter whether include or not (law of total probability)

# sample 

summary(
  fit_std_sample_main, 
  contrast = "difference", 
  reference = 0)

# population
summary(
  fit_std_population_main, 
  contrast = "difference", 
  reference = 0
  )

#weighted sample
summary(
  fit_std_weighted_sample_main, 
  contrast = "difference", 
  reference = 0
)



# Calculate predictions for each individual in the sample under both treatment conditions
predicted_y_treatment = predict(
  model_weighted_sample, 
  newdata=data.frame(a_sample=1, sample_data$z_sample), 
  type="response")

predicted_y_control = 
  predict(model_weighted_sample, 
  newdata=data.frame(a_sample=0, sample_data$z_sample), 
  type="response")

 weights <- sample_data$weights

## works
mean( predicted_y_treatment - predicted_y_control )


# boot strap contrast

# Assuming model_weighted_sample is your fitted model and sample_data contains your data

bootstrap_contrast <- function(data, n_boot = 1000) {
  set.seed(123)  # For reproducibility
  contrasts <- numeric(n_boot)  # Initialize a vector to store bootstrap contrasts
  
  for (i in 1:n_boot) {
    # Resample data with replacement
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    
    # Fit model to bootstrap sample (adjust this step according to your specific model fitting procedure)
    boot_model <- glm(y ~ a
      # + z + a:z
      , 
      data = boot_data, weights = boot_data$weights)  # Example model formula
    
    # Predict outcomes under treatment and control for the bootstrap sample
    predicted_y_treatment_boot <- predict(boot_model, newdata = data.frame(a = 1, z = boot_data$z), type = "response")
    predicted_y_control_boot <- predict(boot_model, newdata = data.frame(a = 0, z = boot_data$z), type = "response")
    
    # Calculate and store the contrast
    contrasts[i] <- mean(predicted_y_treatment_boot - predicted_y_control_boot)
  }
  
  contrasts
}

sample_data$y <- sample_data$y_sample
sample_data$a <- sample_data$a_sample
sample_data$z <- sample_data$z_sample



# Run the bootstrap
# Ensure sample_data includes 'y', 'a', 'z', and 'weights' columns
boot_contrasts <- bootstrap_contrast(data = sample_data, n_boot = 1000)

# Calculate the standard error of the contrasts
se_contrast <- sd(boot_contrasts)

# Output the standard error
se_contrast



# library another method
library(emmeans)

# Assuming model_weighted_sample is your model
# Obtain estimated marginal means for the levels of a factor (e.g., treatment)
emm <- emmeans(model_weighted_sample, specs = "a_sample")

# Compute contrasts or comparisons
pairs(emm)

# For robust standard errors
summary(emm, vcov = vcovHC(model_weighted_sample, type = "HC1"))



### Try measurement error 
n_measurement <- 10000 
beta_a_true <- 1

a_error <- rnorm(n_measurement, .5)
a_true <- rnorm(n_measurement, 2, .5)
a_measured <-  a_error + a_true 

y_error <- rnorm(n_measurement, .5)

y_true <- beta_a_true * a_true  + rnorm(n_measurement, .1)
y_measured <-  y_error + y_true 

y_true

# truth
model_true_measure <- lm( y_true ~ a_true)

summary(model_true_measure)

# measurement error
model_error_measure <- lm( y_measured ~ a_measured)

summary(model_error_measure)

