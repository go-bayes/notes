# load necessary packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("geepack")) install.packages("geepack")
library(dplyr)
library(tidyr)
library(geepack)


## seed
set.seed(123)

# define the number of cultures and the number of obs for each group
n_cultures <- 200
obs_counts <- c(rep(1, 10), rep(3, 190))  # some cultures have only one observation

# init empty data frame
data <- data.frame(id = integer(), year_measured = integer(), beliefs_big_gods = integer(), 
                   social_complexity = integer(), confounder = numeric())

# generate data
for (i in 1:n_cultures) {
  id <- i
  n_obs <- obs_counts[i]
  years <- seq(1, n_obs)
  
  # generate confounder values (normal distribution)
  confounder <- rnorm(n_obs)
  
  # initialise beliefs and social complexity
  beliefs_big_gods <- rep(0, n_obs)
  social_complexity <- rep(0, n_obs)
  
  for (j in 2:n_obs) {
    # prob of believing in big gods
    p_big_gods <- plogis(0.2 + 0.1 * confounder[j])
    beliefs_big_gods[j] <- rbinom(1, 1, p_big_gods)
    
    # prob of social complexity
    p_social_complexity <- plogis(0.3 + 0.4 * beliefs_big_gods[j] + 0.1 * confounder[j])
    social_complexity[j] <- rbinom(1, 1, p_social_complexity)
  }
  
  # combine data
  culture_data <- data.frame(id = rep(id, n_obs), year_measured = years, 
                             beliefs_big_gods = beliefs_big_gods, 
                             social_complexity = social_complexity, 
                             confounder = confounder)
  
  # append 
  data <- rbind(data, culture_data)
}

nrow(data)


# hack to get the lead of social complexity as the correct value ( JAKE YOU WON'T NEED TO DO THIS)
data <- data |>
  group_by(id) |>
  mutate(lead_social_complexity = social_complexity) |>
  ungroup()

nrow(data)

# create the lost indicator -- JAKE YOU"LL NEED TO GET A CENSORING INDICATOR IN YOUR DATA 
# convert your data to long format and do this. 
data <- data |>
  group_by(id) |>
  mutate(lost = ifelse(n() == 1, 1, 0)) |>
  ungroup()

# filter cases where beliefs_big_gods and social_complexity are 0 at the first observation
filtered_data <- data |>
  group_by(id) |>
  filter(beliefs_big_gods[1] == 0 & social_complexity[1] == 0) |>
  ungroup()


# check rows -- remember this is in long formate
nrow(filtered_data)

table(filtered_data$beliefs_big_gods)
table(filtered_data$social_complexity)


# you can do something like this if needed to get leads and lags in your dataframe. 
# # duplicate cases if the lead value by year_measured is not NA and both big gods and social complexity are zer
# duplicated_data <- filtered_data |>
#   group_by(id) |>
#   mutate(lead_beliefs_big_gods = lead(beliefs_big_gods),
#          lead_social_complexity = lead(social_complexity)) |>
#   filter(!is.na(lead_beliefs_big_gods) & lead_beliefs_big_gods == 0 & lead_social_complexity == 0) |>
#   mutate(id = id + max(id)) |>
#   select(-lead_beliefs_big_gods, -lead_social_complexity) |>
#   ungroup()

# combine original and duplicated data
# final_data <- bind_rows(filtered_data, duplicated_data)

# create lead value of social complexity
# final_data <- final_data |>
#   group_by(id) |>
#   arrange(year_measured) |>
#   mutate(lead_social_complexity = lead(social_complexity)) |>
#   ungroup()

# create lag value of confounder
final_data <- filtered_data |>
  group_by(id) |>
  arrange(year_measured) |>
  mutate(lag_confounder = lag(confounder),
         lag_confounder_impute = ifelse(is.na(lag_confounder), confounder, lag_confounder)) |>
  ungroup()

# print first few rows of the final data frame

head(final_data)


# check
head(final_data)


# est mod
summary (fit <- glm(lead_social_complexity ~ beliefs_big_gods + lag_confounder_impute,  
                    family = "poisson", data = final_data) ) # when outcome is not rare do not use binomial models



# risk ratio approximation when outcome is not rare

exp(coef(fit)["beliefs_big_gods"])
# approximate risk ratio
exp(coef(gee_msm)["beliefs_big_gods"])


# msm ---------------------------------------------------------------------

# deal with clustering

# for data clustered data MSM
if (!require("geepack")) install.packages("geepack")
library(geepack)

# Check for NA values in the predictor variables and filter them out
complete_cases <- final_data %>%
  filter(!is.na(lag_confounder_impute) & !is.na(year_measured) & !is.na(beliefs_big_gods))


#fit a logistic regression / estimate the probability of beliefs_big_gods
iptw_model <- glm(beliefs_big_gods ~ lag_confounder_impute + year_measured, family = binomial, data = complete_cases)

# predict probabilities on the complete cases
complete_cases <- complete_cases |>
  mutate(prob_beliefs_big_gods = predict(iptw_model, newdata = complete_cases, type = "response"))

# calculate IPTWs
complete_cases <- complete_cases |>
  mutate(iptw = ifelse(beliefs_big_gods == 1, 1 / prob_beliefs_big_gods, 1 / (1 - prob_beliefs_big_gods)))

# check summary statistics for IPTWs
summary(complete_cases$iptw)


# Fit the GEE model --  WE GET THE WRONG SIGN :)
gee_msm <- geeglm(
  lead_social_complexity ~ beliefs_big_gods, # no need to add covariates in an MSM
  id = id,
  data = complete_cases,
  family = poisson,  # Use poisson family for risk ratio approximation
  corstr = "exchangeable",
  weights = iptw
)

# summary of the MSM model
summary(gee_msm)

# approximate risk ratio accounting for non indpendence. 
exp(coef(gee_msm)["beliefs_big_gods"])


