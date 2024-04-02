
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




# START HERE ---------------------------------------------------------------------
# simulate effect-modification

# to obtain marginal effects
library(stdReg)

# to create nice tables
library(parameters)

simulate_ate_data_with_weights_and_noise <- function(n_sample = 10000,
                                                     n_population = 100000,
                                                     p_z_sample = 0.1,  # probability of effect modifier in the sample
                                                     p_z_population = 0.5, #probability of effect modifier in the population
                                                     beta_a = 1,  # coef of intervention
                                                     beta_z = 2.5, # coef  of effect-modifier
                                                     beta_az = 0.5, # effect modification of a by z
                                                     noise_sd = .5) {  # added parameter for noise standard deviation
  set.seed(123)
  
  # generate sample data
  z_sample <- rbinom(n_sample, 1, p_z_sample) # simulate data for sample z
  a_sample <- rbinom(n_sample, 1, 0.5) # for sample treatment
  
  # simulate outcome
  y_sample <- beta_a * a_sample + beta_z * z_sample + beta_az * (a_sample * z_sample) +
    rnorm(n_sample, mean = 0, sd = noise_sd)  # use noise_sd for the noise term
  
  # put data in dataframe
  sample_data <- data.frame(y_sample, a_sample, z_sample)
  
  # simulate population data, where the distribution of effect modifiers differs but treatment effect is the same
  z_population <- rbinom(n_population, 1, p_z_population)
  a_population <- rbinom(n_population, 1, 0.5) # same effect of a on y 
  y_population <- beta_a * a_population + beta_z * z_population + 
    beta_az * (a_population * z_population) + rnorm(n_population, mean = 0, sd = noise_sd)  # Use noise_sd for the noise term
  population_data <- data.frame(y_population, a_population, z_population)
  
  # simulate weighting based on z distribution difference
  weight_z_1 = p_z_population / p_z_sample # adjust weight for Z=1
  weight_z_0 = (1 - p_z_population) / (1 - p_z_sample) # adjust weight for Z=0
  weights <- ifelse(z_sample == 1, weight_z_1, weight_z_0)
  
  # add weights to sample_data
  sample_data$weights = weights
  
  # Return list of data frames and weights
  list(sample_data = sample_data, population_data = population_data)
}

# example -- you can use different parameters
data <- simulate_ate_data_with_weights_and_noise(
  n_sample = 10000,
  n_population = 100000,
  p_z_sample = 0.1,
  p_z_population = 0.5,
  beta_a = 1,
  beta_z = 2.5,
  noise_sd = 0.5
)  


# Access the generated sample data with weights and population data
sample_data <- data$sample_data
population_data <- data$population_data
# check imbalance 
table(sample_data$z_sample) # type 1 is rare
table(population_data$z_population) # type 1 is commone


# model coefficients sample
model_sample  <- glm(y_sample ~ a_sample * z_sample, data = sample_data)
parameters::model_parameters(model_sample, ci_method="wald")


# model coefficients population -- note that these coefficients are very similar. 
model_population <- glm(y_population ~ a_population * z_population, data = population_data)
parameters::model_parameters(model_population, ci_method="wald")


# model the sample weighted to the population, again note that these coefficients are very similiar 
model_weighted_sample <- glm(y_sample ~  a_sample  * z_sample, data = sample_data, weights = weights)
summary(parameters::model_parameters(model_weighted_sample, ci_method="wald"))


# What inference do we draw?  We cannot say that the models are unbiased for the marginal effect estimates. 


## regression standardisation 
library(stdReg). # to obtain marginal effects 

fit_std_sample <- stdReg::stdGlm( model_sample, data = sample_data, X = "a_sample")

summary(fit_std_sample, 
contrast = "difference", 
reference = 0)


## note the population effect is different
fit_std_population <- stdReg::stdGlm( model_population, data = population_data, X = "a_population")
summary(fit_std_population, 
contrast = "difference", 
reference = 0)

## next try weights adjusted ate where we correctly assign population weights to the sample
fit_std_weighted_sample_weights <- stdReg::stdGlm( model_weighted_sample, 
    data = sample_data, 
    X = "a_sample")

# this gives us the the right answer
summary(fit_std_weighted_sample_weights, 
    contrast = "difference", 
    reference = 0)


# Moral of the story. When we marginalise over the entire sample we need to weight estimates to the target population. 



