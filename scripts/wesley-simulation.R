# libraries
library(MASS) # simulating multivariate data
library(lme4) # model with random effects
library(parameters) # tables 
library(ggeffects) # graphs
library(ggplot2) # graphs


# seed
set.seed(123)

# no warrenties -- this is just to get data that are similar in some respects to the authors

# defining parameters
n_countries <- 28
n_surveys_per_country <- 3
n_waveYears <- 6
n_obs <- 1307  # total number of observations
n_surveys <- n_countries * n_surveys_per_country



# observations per group
n_per_country <- ceiling(n_obs / n_countries)
n_per_waveYear <- ceiling(n_obs / n_waveYears)
n_per_survey <- ceiling(n_per_country / n_surveys_per_country)

# adjust variances to ensure they are positive
var_country <- 0.25^2
var_waveYear <- 0.25^2
var_survey <- 0.20^2


# simulate random effects for countries and wave years
re_country <- mvrnorm(n_countries, mu = 0, Sigma = matrix(var_country), empirical = TRUE)
re_waveYear <- mvrnorm(n_waveYears, mu = 0, Sigma = matrix(var_waveYear), empirical = TRUE)

# simulate nested random effects for surveys within countries
re_survey_within_country <- array(dim = c(n_surveys, n_countries))
country_ids <- rep(1:n_countries, each = n_surveys_per_country)


sigma_survey <- matrix(c(.9, 0.5, 0.5, .9), nrow=2) # replace with actual values
sigma_survey

print(sigma_survey)

# simulate the random effects for surveys within countries
for (i in 1:n_countries) {
  re_survey_within_country[, i] <- mvrnorm(n=n_surveys_per_country, mu=rep(0, ncol(sigma_survey)), Sigma=sigma_survey, empirical=TRUE)
}

# cohort variable and centering
cohort10 <- runif(n_obs, 1960, 2000)
cohort10_center <- cohort10 - mean(cohort10)

# create dataframe
data <- data.frame(
  country = factor(rep(1:n_countries, each = n_per_country)[1:n_obs]),
  waveYear = factor(rep(1:n_waveYears, each = n_per_waveYear)[1:n_obs]),
  survey_id = rep(1:n_surveys, each = n_per_survey)[1:n_obs]
)
re_country

# map country and waveYear random effects to data
data$re_country <- re_country[data$country]
data$re_waveYear <- re_waveYear[data$waveYear]

# map nested random effects for surveys within countries to data
data$re_survey_within_country <- re_survey_within_country[data$survey_id]

# fixed effect for cohort
beta_cohort10 <- -0.02

# linear predictor and simulate outcome
intercept <- .64  # logit for the earliest cohort

# some checks
summary(data$re_country)
summary(data$re_waveYear)
summary(data$re_survey_within_country)
summary(beta_cohort10 * cohort10_center)


# simualte outcome
eta <- intercept + 
  data$re_country + 
  data$re_waveYear + 
  data$re_survey_within_country +
  beta_cohort10 * cohort10_center


# convert to probabilities using the plogis
p <- plogis(eta)

# simulate the binary outcome variable y
data$y <- rbinom(n_obs, size = 1, prob = p)


# model
m1 <- lme4::glmer(
  y ~ cohort10_center + (1 | country/survey_id) + (1 | waveYear/survey_id),
  data = data,
  family = "binomial"
)

# nested structure by creating random intercepts for surveys that are unique within each country and within each wave year.

# singular, but rescale etc to suit the data better

# summary
parameters::model_parameters(m1)

# graph
graph <- plot(
  ggeffects::ggpredict(m1, terms = "cohort10_center [all]"), add.data = TRUE, alpha = .3
)

# 
graph + scale_y_continuous (limits = c(0,1))
