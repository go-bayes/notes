




#Packages
library(ggplot2)
library(simcausal)

# refactor simulation code ---------------------------------------------

# libraries

# load tidyverse
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# load clarify
if (!requireNamespace("clarify", quietly = TRUE)) {
  install.packages("clarify")
}
library(clarify)

# load truncnorm
if (!requireNamespace("truncnorm", quietly = TRUE)) {
  install.packages("truncnorm")
}
library(truncnorm)

# load parameters
if (!requireNamespace("parameters", quietly = TRUE)) {
  install.packages("parameters")
}

library(parameters)


# simulate parameters ----------------------------------------------------------

# adjust simulation in ways that help to address a specific question. 
# here we ask, can we detect effects of `small_protestant_2009` on `indig_lang_2023`
# note i’m using clear, simple names for the variables

# set seed for reproducibility
set.seed(123)

# set the sample size to 100
n <- 100

# initialise initial proportion values for indig_lang and small_protestant for the year 1999

init_indig_lang_1999 <- 0.8
init_small_protestant_1999 <- 0.7

# set unexplained decay rates for indig_lang and small_protestant

decay_indig_lang <- 0.01
decay_small_protestant <- 0.02


# generate uniform random numbers for distance_to_urban, comm_tech_1999, and in_migration_1999
distance_to_urban <- runif(n, 0, 1)
comm_tech_1999 <- runif(n, 0, 1)
in_migration_1999 <- runif(n, 0, 1)

# add a coefficient for small_protestant affects on language
coef_small_protestant <- 0.05

# set a noise parameter, for example, standard deviation of 0.01
noise_sd <- 0.1 

# generate initial proportion variables for indig_lang_1999 and small_protestant_1999 using truncated normal distribution
# subtract effects of comm_tech_1999 and in_migration_1999

indig_lang_1999 <-
  rtruncnorm(n,
             a = 0, # lower bound
             b = 1, # upper bound
             mean = init_indig_lang_1999,
             sd = 0.1) - comm_tech_1999 * 0.1 - in_migration_1999 * 0.1. # assume these factors decrease language use

small_protestant_1999 <-
  rtruncnorm(n,
             a = 0,
             b = 1,
             mean = init_small_protestant_1999,
             sd = 0.1) - comm_tech_1999 * 0.1 - in_migration_1999 * 0.1 # assume these factors decrease small_protestant_1999


# calculate proportion variables for the years 2009 and 2023
# apply decay rates and add influence of distance_to_urban

# note: pmax(0, ...) ensures that the value does not go below 0. If the computed value is negative, it sets it to 0.
# pmin(1, ...) ensures that the value does not go above 1. If the computed value is greater than 1, it sets it to 1.

# indig_lang_2009

indig_lang_2009 <-
  pmax(0, pmin(
    1,
    indig_lang_1999 * (1 - decay_indig_lang) + distance_to_urban * 0.05 - coef_small_protestant * small_protestant_1999 + 
      + rnorm(length(indig_lang_1999), 0, noise_sd) # gaussian noise
  ))

small_protestant_2009 <-
  pmax(0, pmin(
    1,
    small_protestant_1999 * (1 - decay_small_protestant) + distance_to_urban * 0.05 
  ))


indig_lang_2023 <-
  pmax(0, pmin(
    1,
    indig_lang_2009 * (1 - decay_indig_lang) + distance_to_urban * 0.05  - coef_small_protestant * small_protestant_2009  + 
      rnorm(length(indig_lang_2009), 0, noise_sd) # gaussian noise, we could add noise elsewhere… 
  ))


# make dataframe, combining vars
sim_data <- data.frame(
  distance_to_urban,
  comm_tech_1999,
  in_migration_1999,
  small_protestant_1999,
  small_protestant_2009,
  indig_lang_1999,
  indig_lang_2009,
  indig_lang_2023
)



# compare different models # just using a linear model for simplicity

m_true <-
  lm(
    indig_lang_2023 ~ indig_lang_2009 + small_protestant_2009 + distance_to_urban + comm_tech_1999,
    data = sim_data
  )


# simple model, with confounding at baseline
m_simple_confounded <-
  lm(indig_lang_2023 ~ small_protestant_2009 + indig_lang_2009,
     data = sim_data)

# fully confounded
m_fully_confounded <-
  lm(indig_lang_2023 ~ small_protestant_2009, data = sim_data)


# compare models
# the simulation shows 


parameters::compare_models(m_true,
                           m_simple_confounded,
                           m_confounded)



# we can continue using the clarify package....
## INTERPRETATION 

# the omission of variables in m_simple and m_fully_confounded appears to bias the coefficients for small_protestant_2009 away from the m_true value.

# m_true and m_simple find reliable effects for the same variables. in contrast, m_fully_confounded fails to capture reliability of small_protestant_2009.

# the intercept in m_fully_confounded is much larger, likely because it absorbs the effects of the omitted variables.

# although the fully specified model (m_true) should theoretically provide the best fit, given it includes all relevant predictors, the model is inaccurate, not from unmeasured confounding but from noise


# we can continue using the clarify package....



# streamline version ------------------------------------------------------

# set seed, sample size, initial values, and decay rates
set.seed(123); n <- 100
init_vals <- list(indig_lang = 0.8, small_protestant = 0.7)
decays <- list(indig_lang = 0.01, small_protestant = 0.02)

# generate uniform random numbers
distance_to_urban <- runif(n, 0, 1)
other_factors <- runif(n, 0, 1)

# coefficients
coef_vals <- list(small_protestant = -0.05, distance_to_urban = 0.05)
noise_sd <- 0.1

# truncated normal function
trunc_norm <- function(mean_val) {
  rtruncnorm(n, a = 0, b = 1, mean = mean_val, sd = 0.1)
}

# initial proportion variables
indig_lang_1999 <- trunc_norm(init_vals$indig_lang) - other_factors * 0.1
small_protestant_1999 <- trunc_norm(init_vals$small_protestant) - other_factors * 0.1

# simulate 2009 and 2023 variables
simulate_var <- function(init_val, decay_rate, coef_val) {
  pmax(0, pmin(1, init_val * (1 - decay_rate) + distance_to_urban * coef_vals$distance_to_urban - coef_val + rnorm(length(init_val), 0, noise_sd)))
}

indig_lang_2009 <- simulate_var(indig_lang_1999, decays$indig_lang, coef_vals$small_protestant * small_protestant_1999)
small_protestant_2009 <- simulate_var(small_protestant_1999, decays$small_protestant, 0)
indig_lang_2023 <- simulate_var(indig_lang_2009, decays$indig_lang, coef_vals$small_protestant * small_protestant_2009)

# create data frame
sim_data <- data.frame(distance_to_urban, other_factors, indig_lang_1999, small_protestant_1999, indig_lang_2009, small_protestant_2009, indig_lang_2023)

# run models and compare
m_true <- lm(indig_lang_2023 ~ indig_lang_2009 + small_protestant_2009 + distance_to_urban + other_factors, data = sim_data)
m_simple_confounded <- lm(indig_lang_2023 ~ small_protestant_2009 + indig_lang_2009, data = sim_data)
m_fully_confounded <- lm(indig_lang_2023 ~ small_protestant_2009, data = sim_data)
parameters::compare_models(m_true, m_simple_confounded, m_fully_confounded)




# Simulating longitudinal data --------------------------------------------

#Variables to simulate
#Predictor:
#Proportion of smaller Protestant 1999,
#Proportion of smaller Protestant 2009
#Outcome:
#Proportion of households using indigenous language 1999,
#Proportion of households using indigenous language 2009,
#Proportion of households using indigenous language 2023,
#Controls (minimal adjustment set):
#Distance to urban (non-temporal constant),
#Communication and transport technology 1999,
#In-migration 1999

#Create function for truncated distribution between 0 and 1
set.seed(1234)

rnorm_trunc <- function(n,
                        mean,
                        sd,
                        minval = 0,
                        maxval = 1) {
  out <- rnorm(n = n, mean = mean, sd = sd)
  minval <- minval[1]
  out[out < minval] <- minval
  out[out > maxval] <- maxval
  return(out)
}

#Create DAG
dagSim <- DAG.empty() +
  node(
    "EnumAreaDistanceToUrban",
    distr = "rnorm_trunc",
    mean = 0.25,
    sd = 0.35
  ) +
  node(
    "EnumAreaAverageCommunicationAndTransportationTechnology",
    distr = "rnorm_trunc",
    mean = 0.5 + 0.2 * EnumAreaDistanceToUrban,
    sd = 0.2
  ) +
  node(
    "EnumAreaAverageInMigration",
    distr = "rnorm_trunc",
    mean = 0.005 + 0.3 * EnumAreaDistanceToUrban,
    sd = 0.1
  ) +
  node(
    "EnumAreaProportionSmallerProtestant",
    t = 0,
    distr = "rnorm_trunc",
    mean = 0.1 + 0.2 * EnumAreaDistanceToUrban,
    sd = 0.15
  ) +
  node(
    "EnumAreaProportionHouseholdsUseIndigenousLanguage",
    t = 0,
    distr = "rnorm_trunc",
    mean = 0.83+-0.3 * EnumAreaDistanceToUrban,
    sd = 0.18
  ) +
  node(
    "EnumAreaProportionSmallerProtestant",
    t = 1,
    distr = "rnorm_trunc",
    mean = EnumAreaProportionSmallerProtestant[t - 1] + 0.1 + 0.001 *
      EnumAreaDistanceToUrban + 0.1 * EnumAreaAverageCommunicationAndTransportationTechnology + 0.1 *
      EnumAreaAverageInMigration,
    #t-1 likely signifies the 1 prior time step
    sd = 0.15
  ) +
  node(
    "EnumAreaProportionHouseholdsUseIndigenousLanguage",
    t = 1,
    distr = "rnorm_trunc",
    mean = EnumAreaProportionHouseholdsUseIndigenousLanguage[t - 1]+-0.001 *
      EnumAreaDistanceToUrban+-0.01 * EnumAreaAverageCommunicationAndTransportationTechnology+-0.01 * EnumAreaAverageInMigration - EnumAreaProportionSmallerProtestant[t -
                                                                                                                                                                         1],
    sd = 0.18
  ) +
  node(
    "EnumAreaProportionHouseholdsUseIndigenousLanguage",
    t = 2,
    distr = "rnorm_trunc",
    mean = EnumAreaProportionHouseholdsUseIndigenousLanguage[t - 1]+-0.1 *
      EnumAreaDistanceToUrban+-0.1 * EnumAreaAverageCommunicationAndTransportationTechnology+-0.1 *
      EnumAreaAverageInMigration - EnumAreaProportionSmallerProtestant[t - 1],
    sd = 0.18
  )
dagSim <- set.DAG(dagSim)


# re-do






exp(.1)
#Plot variables in a DAG
plotDAG(dagSim, yjitter = 0.9)

#Simulate
simDat <- simcausal::sim(DAG = dagSim, n = 100, rndseed = 123)

#Rename variables regarding time
simDat <- simDat %>%
  rename(
    Enum_area_distance_to_urban = EnumAreaDistanceToUrban,
    Enum_area_average_communication_and_transportation_technology_1999 = EnumAreaAverageCommunicationAndTransportationTechnology,
    Enum_area_average_in_migration_1999 = EnumAreaAverageInMigration,
    Enum_area_proportion_smaller_protestant_1999 = EnumAreaProportionSmallerProtestant_0,
    Enum_area_proportion_smaller_protestant_2009 = EnumAreaProportionSmallerProtestant_1,
    Enum_area_proportion_households_use_indigenous_language_1999 = EnumAreaProportionHouseholdsUseIndigenousLanguage_0,
    Enum_area_proportion_households_use_indigenous_language_2009 = EnumAreaProportionHouseholdsUseIndigenousLanguage_1,
    Enum_area_proportion_households_use_indigenous_language_2023 = EnumAreaProportionHouseholdsUseIndigenousLanguage_2
  )




test <-
  lm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~ Enum_area_proportion_households_use_indigenous_language_2009 +  Enum_area_proportion_smaller_protestant_2009,
    simDat
  )
parameters::model_parameters(test)


#Apply logistic transformation to get log odds
hist(simDat$Enum_area_proportion_smaller_protestant_1999)


simDat$Enum_area_proportion_smaller_protestant_1999_plogis <-
  plogis(simDat$Enum_area_proportion_smaller_protestant_1999)
simDat$Enum_area_proportion_smaller_protestant_2009_plogis <-
  plogis(simDat$Enum_area_proportion_smaller_protestant_2009)

hist(simDat$Enum_area_proportion_smaller_protestant_1999_plogis)

hist(simDat$Enum_area_proportion_smaller_protestant_1999)

#Plot histograms for variables
ggplot(simDat, aes(Enum_area_distance_to_urban)) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat,
       aes(
         Enum_area_average_communication_and_transportation_technology_1999
       )) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat, aes(Enum_area_average_in_migration_1999)) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat, aes(Enum_area_proportion_smaller_protestant_1999)) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat, aes(Enum_area_proportion_smaller_protestant_2009)) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat,
       aes(
         Enum_area_proportion_households_use_indigenous_language_1999
       )) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat,
       aes(
         Enum_area_proportion_households_use_indigenous_language_2009
       )) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat,
       aes(
         Enum_area_proportion_households_use_indigenous_language_2023
       )) +
  geom_histogram(color = "black", fill = "lightgray") + xlim(-0.1, 1.05)
ggplot(simDat, aes(Enum_area_proportion_smaller_protestant_1999)) +
  geom_histogram(color = "black", fill = "lightgray") #Notice change in x-axis limits
ggplot(simDat, aes(Enum_area_proportion_smaller_protestant_2009)) +
  geom_histogram(color = "black", fill = "lightgray") #Notice change in x-axis limits

#Correlations with Enum_area_distance_to_urban
ggplot(
  simDat,
  aes(
    Enum_area_distance_to_urban,
    Enum_area_average_communication_and_transportation_technology_1999
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(simDat,
       aes(
         Enum_area_distance_to_urban,
         Enum_area_average_in_migration_1999
       )) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_distance_to_urban,
    Enum_area_proportion_smaller_protestant_1999
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_distance_to_urban,
    Enum_area_proportion_smaller_protestant_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_distance_to_urban,
    Enum_area_proportion_households_use_indigenous_language_1999
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_distance_to_urban,
    Enum_area_proportion_households_use_indigenous_language_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_distance_to_urban,
    Enum_area_proportion_households_use_indigenous_language_2023
  )
) +
  geom_point() + geom_smooth(method = "lm")

#Correlations across waves
ggplot(
  simDat,
  aes(
    Enum_area_proportion_smaller_protestant_1999,
    Enum_area_proportion_smaller_protestant_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_proportion_households_use_indigenous_language_1999,
    Enum_area_proportion_households_use_indigenous_language_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_proportion_households_use_indigenous_language_2009,
    Enum_area_proportion_households_use_indigenous_language_2023
  )
) +
  geom_point() + geom_smooth(method = "lm")

#Corrrelations for EnumAreaProportionSmallerProtestant
ggplot(
  simDat,
  aes(
    Enum_area_average_communication_and_transportation_technology_1999,
    Enum_area_proportion_smaller_protestant_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_average_in_migration_1999,
    Enum_area_proportion_smaller_protestant_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")

#Correlations between EnumAreaProportionSmallerProtestant and EnumAreaProportionHouseholdsUseIndigenousLanguage
ggplot(
  simDat,
  aes(
    Enum_area_proportion_smaller_protestant_1999,
    Enum_area_proportion_households_use_indigenous_language_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_proportion_smaller_protestant_2009,
    Enum_area_proportion_households_use_indigenous_language_2023
  )
) +
  geom_point() + geom_smooth(method = "lm")

#Correlations between EnumAreaProportionSmallerProtestant_plogis and EnumAreaProportionHouseholdsUseIndigenousLanguage
ggplot(
  simDat,
  aes(
    Enum_area_proportion_smaller_protestant_1999,
    Enum_area_proportion_households_use_indigenous_language_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")
ggplot(
  simDat,
  aes(
    Enum_area_proportion_smaller_protestant_2009,
    Enum_area_proportion_households_use_indigenous_language_2023
  )
) +
  geom_point() + geom_smooth(method = "glm")



ggplot(
  simDat,
  aes(
    Enum_area_proportion_households_use_indigenous_language_2009,
    Enum_area_proportion_smaller_protestant_2009
  )
) +
  geom_point() + geom_smooth(method = "lm")

ggplot(simDat, aes(, Enum_area_proportion_smaller_protestant_2009)) +
  geom_point() + geom_smooth(method = "lm")

ggplot(
  simDat,
  aes(
    Enum_area_proportion_smaller_protestant_2009,
    Enum_area_proportion_households_use_indigenous_language_2023
  )
) +
  geom_point() + geom_smooth(method = "glm")



hist(simDat$Enum_area_proportion_households_use_indigenous_language_2023)

# Modelling causal effect -------------------------------------------------

#Basic model
m1 <-
  lm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~ Enum_area_proportion_smaller_protestant_2009_plogis,
    data = simDat
  )
summary(m1)

#Model for g-computation
m2 <-
  lm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~ Enum_area_proportion_smaller_protestant_2009_plogis * (
      Enum_area_proportion_households_use_indigenous_language_1999 + Enum_area_proportion_smaller_protestant_1999_plogis + Enum_area_distance_to_urban + Enum_area_average_communication_and_transportation_technology_1999 + Enum_area_average_in_migration_1999
    ),
    data = simDat
  )
summary(m2)

#New data frame for prediction with average Enum_area_proportion_smaller_protestant_2009_plogis
simDat_average <- simDat
simDat_average$Enum_area_proportion_smaller_protestant_2009_plogis <-
  mean(simDat$Enum_area_proportion_smaller_protestant_2009_plogis)

#New data frame for prediction with +1 sd Enum_area_proportion_smaller_protestant_2009_plogis
simDat_plus_one <- simDat
simDat_plus_one$Enum_area_proportion_smaller_protestant_2009_plogis <-
  mean(simDat$Enum_area_proportion_smaller_protestant_2009_plogis) + sd(simDat$Enum_area_proportion_smaller_protestant_2009_plogis)

#Predict Enum_area_proportion_households_use_indigenous_language_2023 with average exposure
predicted_EnumAreaProportionHouseholdsUseIndigenousLanguage_with_average_exposure <-
  predict(m1, newdata = simDat_average)

#Predict Enum_area_proportion_households_use_indigenous_language_2023 with +1 sd of average exposure
predicted_EnumAreaProportionHouseholdsUseIndigenousLanguage_with_plus_sd_exposure <-
  predict(m1, newdata = simDat_plus_one)

#Calculate average causal effect as a contrast of these predictions
average_causal_effect <-
  mean(
    predicted_EnumAreaProportionHouseholdsUseIndigenousLanguage_with_plus_sd_exposure - predicted_EnumAreaProportionHouseholdsUseIndigenousLanguage_with_average_exposure
  )

# Modelling causal effect with confidence intervals -----------------------

# change scale of exposure - make a 1-unit change meaningful, here, in the rescaled data a 1-unit change represents a proportional change of 10/100 points# note, range of scale is prop .5 to .7, at at transformation range is 5 to 7
simDat$Enum_area_proportion_smaller_protestant_2009_plogis_times_10 <-
  simDat$Enum_area_proportion_smaller_protestant_2009_plogis * 10

# run model, note we could simplify the regression by removing the interaction term if we do not have a large N. Here it doesn’t make much of a difference.
m3_rescaled <-
  lm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~ Enum_area_proportion_smaller_protestant_2009_plogis_times_10 * (
      Enum_area_proportion_households_use_indigenous_language_1999 + Enum_area_proportion_smaller_protestant_1999_plogis + Enum_area_distance_to_urban + Enum_area_average_communication_and_transportation_technology_1999 + Enum_area_average_in_migration_1999
    ),
    data = simDat
  )

# summary of model -- ignore coefficients. Generally, the summary is useless. However, it can be useful to observed whether there are any NAs
summary(m3_rescaled) # useless

#definitions
nsims = 1000
X_3 = "Enum_area_proportion_smaller_protestant_2009_plogis_times_10"
cores = parallel::detectCores()

# simulate standard errors
sim_model_3 <- clarify::sim(m3_rescaled, n = nsims, vcov = "HC1")

# simulate effect as modified
sim_estimand_CI_3 <- sim_ame(sim_model_3,
                             var = X,
                             cl = cores,
                             verbose = TRUE)

# effect is mostly negative
summary(sim_estimand_CI_3)

# graph: effect is mostly negative.
plot(sim_estimand_CI_3)




# Another approach --------------------------------------------------------

### Fractional Logit Model

# directly models the proportional outcome
#\[
#  \log\left(\frac{p}{1-p}\right) = X\beta
#  \]

2 * .9



# Coefficients directly relate to changes in the proportion, simplifying interpretation.

# accounts for overdispersion through the quasi-binomial family.

set.seed(1234)

m4a <-
  glm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~
      splines::bs(Enum_area_proportion_smaller_protestant_2009_plogis) +
      splines::bs(
        Enum_area_proportion_households_use_indigenous_language_1999
      ) +
      splines::bs(Enum_area_proportion_smaller_protestant_1999_plogis) +
      splines::bs(Enum_area_distance_to_urban) +
      splines::bs(
        Enum_area_average_communication_and_transportation_technology_1999
      ) +
      splines::bs(Enum_area_average_in_migration_1999),
    family = quasibinomial(),
    data = simDat
  )
summary(m4)


summary(m4)



m5 <-
  glm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~
      Enum_area_proportion_smaller_protestant_2009_plogis +
      Enum_area_proportion_households_use_indigenous_language_1999,
    # accounts for overdispersion through the quasi-binomial family.
    family = quasibinomial(),
    data = simDat
  )
summary(m5)



# print
# average_causal_effect

#definitions

m_4 <-
  glm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~
      Enum_area_proportion_smaller_protestant_2009 *
      (
        Enum_area_proportion_households_use_indigenous_language_1999 +
          Enum_area_proportion_smaller_protestant_1999 +
          Enum_area_distance_to_urban +
          Enum_area_average_communication_and_transportation_technology_1999 +
          Enum_area_average_in_migration_1999
      ),
    family = quasibinomial(),
    data = simDat
  )

# note coeffs are on the log-odds scale.
# note we have *under-dispersion*
summary(m4)
hist(simDat$Enum_area_proportion_smaller_protestant_2009)

# m4 <- glm(Enum_area_proportion_households_use_indigenous_language_2023 ~
#             Enum_area_proportion_smaller_protestant_2009+
#             Enum_area_proportion_households_use_indigenous_language_1999_plogis +
#             Enum_area_proportion_smaller_protestant_1999  +
#             Enum_area_distance_to_urban +
#             Enum_area_average_communication_and_transportation_technology_1999 +
#             Enum_area_average_in_migration_1999,
#           family = quasibinomial(), data = simDat)

# note coeffs are on the log-odds scale.
# note we have *under-dispersion*
summary(m4)


# remember to set seed!
set.seed(1234)

m_4 <-
  glm(
    Enum_area_proportion_households_use_indigenous_language_2023 ~
      Enum_area_proportion_smaller_protestant_2009 * (
        Enum_area_proportion_households_use_indigenous_language_1999 +
          Enum_area_proportion_smaller_protestant_1999 +
          Enum_area_distance_to_urban +
          Enum_area_average_communication_and_transportation_technology_1999 +
          Enum_area_average_in_migration_1999
      ),
    family = quasibinomial(),
    data = simDat
  )


# note we have underdispersion
summary(m_4)


m_5 <-
  betareg::betareg(
    Enum_area_proportion_households_use_indigenous_language_2023 ~
      Enum_area_proportion_smaller_protestant_2009 * (
        Enum_area_proportion_households_use_indigenous_language_1999 +
          Enum_area_proportion_smaller_protestant_1999 +
          Enum_area_distance_to_urban +
          Enum_area_average_communication_and_transportation_technology_1999 +
          Enum_area_average_in_migration_1999
      ),
    data = simDat
  )


# note we have underdispersion
summary(m_5)


nsims = 1000
X = "Enum_area_proportion_smaller_protestant_2009"
cores = parallel::detectCores()

# simulate standard errors
sim_model <- clarify::sim(m_5, n = nsims, vcov = "HC1")

# simulate effect as modified
sim_estimand_CI <- sim_ame(sim_model,
                           var = "Enum_area_proportion_smaller_protestant_2009",
                           cl = cores,
                           verbose = TRUE)

# synnart
summary(sim_estimand_CI)

# graph:
plot(sim_estimand_CI)

sim_graph <- sim_setx(
  sim_model,
  x = list(Enum_area_proportion_smaller_protestant_2009 = 0:1),
  verbose = FALSE
)

plot(sim_graph, var = "Enum_area_proportion_smaller_protestant_2009", ci = TRUE)
