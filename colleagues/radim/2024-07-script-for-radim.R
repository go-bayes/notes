set.seed(123)
# radim take july 2024


# Outcome variables
# at_least_2_kids_in_2013 = binary for having two children and more at the end of data collection
# kids_2013                               = number of children at the end of data collection
 
# Baselines
# t0_ritual_scale             = raw continuous/ordered categorical variable for ritual
# t0_ritual_1aweek        = dichotomized variable for weekly ritual
# t0_faith_scale               = raw continuous/ordered categorical variable for faith
# t0_faith_full                    = dichotomized variable for extreme faith (scoring 5 on scale 1-5)
# t0_age                               = age
# t0_parent_income      = [numeric] 11 intervals for parental income in t0 (0-10K, 10-20K … 100K and more)
# t0_female                        = 1 if female             
# t0_religion_cat              = [character] I kept the more informative categories
# t0_parent_education = [character] categories for parental education
# t0_race                             = [character] categories for ethnicity
 
# Exposures
# t1_ritual_scale      = raw continuous/ordered categorical variable for ritual
# t1_ritual_1aweek = dichotomized variable for weekly ritual
# t1_faith_scale       = raw continuous/ordered categorical variable for faith
# t1_faith_full            = dichotomized variable for extreme faith (scoring 5 on scale 1-5)
 
# For additional information
# JB this makes no sense, ignore

# kids_2005 = number of children in 2005 – first wave of data collection. Note that the variables related to fertility were created from the W4 data. Hence we have kids_2005 = NA for participants not included in W4.   

# wave1 = was participant in w1 (2003)
# wave2 = was participant in w2 (2005)
# wave4 = was participant in w4 (2013)
# waves_1_2_4 = was participant in all 3 waves that we use in our study? Note that attrition was not linear. Some participants were not present in W2 but then they were in W4.
# caregiver1 = informs about the identity of caregiver; they are mostly parents  # JB when? 

 
 
# here I respond to your questions
# I am using the dataset that you included in this thread.  I  think that we have dropped the missing data from waves that are > time_1. 
# This reduces efficiency and may lead to bias if there is a systematic relationship between missingness and the covariates/treatment and coverates x treatment.
 
# COULD YOU CREAT A DATA SET WITH THE SAME COLUMNS BUT IN WHICH MISSING VALUES ARE RETAINED?
 
# The data I am attaching have all 3,370 observations.
 
# A few variables that are continues have been made binary.  That is generally OK for a treatment or and outcome (depending on our intereset) but we will may get more efficient etimates from continuous variables. This includes the total counts of children.

# COULD YOU CREATE COLUMNS WITH THE CONTINUOUS VARIABLES WHERE AVAILABLE?
 
# I include the original ordered categorical variables for ritual and faith and the variable for number of children at the end of data collection (W4, year 2013).
 
# We do not get the model to converge when we include the religion categories at time 1.  In particular, “other religion” causes the models to blow up.  Is there a way to recategorize into larger groupings? This is of course tricky… In the present study I have left the denominations out, and this can make sense if denomination is not a common cause of regular religious service and the number of children people have *independently of* regular religious service.   In any case, the model you wanted won’t quite work.
 
# This seems very problematic because denominations definitely affect both attendance (think of Catholics vs Evangelicals) and there is lot of older literature showing differences between denominations in children counts. I am sending religion variable that includes the original NSYR categories.  
 
 
# It is a little strange that there are nearly identical proportions of males and females at T1.   How did that happen?
 
# The data should be representative to the US adolescents so I guess gender balance is expectable. When doing the analyses, can you also do one for interaction between exposure and gender. My previous findings suggest that whatever effect of exposure is there, it is driven by men.
 
# We cannot really estimate fertility in early waves, e.g. t3_ because there are too few cases. We are chasing after noise I think.
 
# Fine, I only include outcomes of ‘≥2kids in 2013’ and ‘total number of children in 2013’.
 
 
# Thank you again for your time on it.
# Best
# R
 



# link to data
pull_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-radim-usa/24-05-data_for_joe.csv"
)

# SET TO YOUR DIRECTORY FOR SAVING VARIABLES
push_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-radim-usa"
)
  

# get devtools if needed
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# get 'margot' from my github (make sure to update)
devtools::install_github("go-bayes/margot")


# check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

# use p_load to load / install the packages
pacman::p_load(
  skimr,
  naniar,
  WeightIt,
  clarify,
  MatchThem,
  cobalt,
  MatchIt,
  kableExtra,
  janitor,
  #  SuperLearner,
  ranger,
  xgboost,
  glmnet,
  doParallel,
  ggplot2,
  here,
  naniar,
  gtsummary,
  grf,
  progressr,
  tidyverse,
  ggplot2,
  parameters,
  kableExtra,
  tidyr,
  stringr,
  patchwork
)
# update.packages()

# import data
dat_raw <- read.csv(pull_mods)

# check
head(dat_raw)

table(dat_raw$wave1)
# create weights for male/not male in t0
# note, please use either "male" or "female" or "not_male"  so that we know what the indicator means




# sample weights balanced male / female-------------------------------------
# balance on gender weights

# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <-
  0.5  # target proportion of males in the population
prop_female_population <-
  0.5  # target proportion of females in the population

# slighly fewer females
table(dat_raw$t0_female)
# math
prop_female_sample <- mean(dat_raw$t0_female)
prop_male_sample <- 1 - prop_female_sample

# math
gender_weight_male <- prop_male_population / prop_male_sample
gender_weight_female <- prop_female_population / prop_female_sample

# inspect -- hmmm, basically even
gender_weight_male
gender_weight_female


# sample weights (optional)
dat_raw$sample_weights <-
  ifelse(dat_raw$t0_female == 1, gender_weight_female, gender_weight_male) 

#
# i think this is id (r to confirm)
dat_init <- dat_raw |> 
  filter(!is.na( t0_ritual_1aweek)) |>  # no missing data for the exposure variable at baseline
  rename(id = X,
# use meaningful consistent names
  t0_not_lost = wave2,   # not lost in the subsequent in  treatment wave is 2005
  t1_not_lost = wave4,  # outcome wave is 2013,
  t2_two_or_more_kids = at_least_2_kid_in_2013,
  t2_children_count = kids_2013
  ) |> 
  # unsure about what the 'caregiver' variable means
  select(-c( waves_1_2_4, caregiver1, kids_2005, at_least_2_kids_in_2013, wave1) ) 
# clean



# single imputation ------------------------------------------------------

# data must be a dataframe
dat_init <- as.data.frame(dat_init )

# prepare data
dat_init <- margot::remove_numeric_attributes(dat_init)


# test total n in the data
n_total <- nrow(dat_init)

# get comma in number
n_total <- prettyNum(n_total, big.mark = ",")

# check n total
n_total

# save n for manuscript
margot::here_save(n_total, "n_total")

# name of exposure

# check missing values
skimr::skim(dat_init) |> arrange(n_missing)


# single impute missing values at baseline 
library(mice)

# only impute pre-exposure data using pre-exposure information
table(dat_init$t0_parent_education )

# define the levels of t0_parent_education in the desired order
education_levels <- c("Less than 12th grade", "High school", "Beyond high school")

# convert t0_parent_education to an ordered factor
dat_baseline <- dat_init |>
  select(starts_with("t0_")) |>
  mutate(
    t0_religion_cat = as.factor(t0_religion_cat),
    t0_parent_education = factor(t0_parent_education, levels = education_levels, ordered = TRUE),
    t0_race = as.factor(t0_race)
  )


# perform the imputation
impute_dat_baseline <- mice(dat_baseline, m = 1, seed = 123)

# convert the imputed data to a dataframe
imputed_data <- complete(impute_dat_baseline)

# get the column names of the imputed data
names_imputed_data <- colnames(imputed_data)

# find the column names that are not in the imputed data
not_imputed_cols <- setdiff(colnames(dat_init), names_imputed_data)

# subset the original data to get the columns that were not imputed
not_imputed_data <- dat_init[ , not_imputed_cols, drop = FALSE]

# combine the imputed data with the non-imputed columns
combined_data <- cbind(imputed_data, not_imputed_data)

# display the combined dataframe
head(combined_data)

# arrange nicely also, if any missing values in outcome make sure R knows these are censored (even if a person participated)

t0_na_condition <- rowSums(is.na(select(combined_data, starts_with("t1_")))) > 0

df_clean <- combined_data |>
  dplyr::mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) |>
  dplyr::mutate(t0_lost = 1 - t0_not_lost) |> 
  relocate("t0_not_lost", .before = starts_with("t1_")) |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))


# checks
naniar::vis_miss(df_clean, warn_large_data = FALSE)



# make weights for loss to follow up -------------------------------------



# weights for treatment ----------------------------------------------------
baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0_"), -t0_not_lost) |> colnames() # note

baseline_vars_models  # predictors of loss at treatment wave

# create fresh dataset
df_clean_pre <- df_clean[baseline_vars_models]

# checks
str(df_clean_pre)

# if this variable were not a factor, make sure it is
# df_clean_pre$t0_eth_cat <- as.factor(df_clean_pre$t0_eth_cat)


# perform one-hot encoding using model.matrix
# we need factors to be 0 or 1
encoded_vars <- model.matrix(~ t0_religion_cat + t0_parent_education + t0_race  - 1, data = df_clean_pre)
head(encoded_vars)


# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df |>
  janitor::clean_names()

# View the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe
str( df_clean$t0_parent_education)
# ensure to remove original categorical variables to avoid duplication

df_clean_hot <- df_clean |>
  select(-c(t0_religion_cat, t0_parent_education, t0_race)) |>
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)




# get baseline variable set without factors
baseline_vars_set <- setdiff(names(df_clean_pre), 
                             c("t0_lost", 
                               "id", 
                               "t0_religion_cat",
                               "t0_parent_education", 
                               "t0_race"))

# check
baseline_vars_set

# add the new encoded column names
full_predictor_vars <- c(baseline_vars_set, new_encoded_colnames)

# check
full_predictor_vars

# check
str(df_clean_hot)

# set up super learner


# censoring weights t1 ---------------------------------------------------
library(SuperLearner)

# library for multicore processing
library(doParallel)

# learners
listWrappers()

# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# Set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# you can probably just use "SL.glmnet", but no harm using more
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger", "SL.earth", "SL.polymars")

# run super learner
sl <- SuperLearner(
  Y = df_clean_hot$t0_lost,
  X = df_clean_hot[full_predictor_vars],
  # use specified predictors
  SL.library = match_lib,
  family = binomial(),
  method = "method.NNloglik",
  cvControl = list(V = 10)
)

# stop the cluster
stopCluster(cl)

# save your super learner model
here_save(sl, "sl")

# read 
sl <- here_read("sl")

# check outputs
# summary of the SuperLearner output
print(sl)

#a detailed summary, including cross-validated risks
summary(sl)                #

# examination of cross-validated performance
# cross-validated risks for each learner
sl$cvRisk

# weights assigned to each learner in the final ensemble
sl$coef

# generate predictions
predictions <- predict(sl, newdata = df_clean_hot[full_predictor_vars], type = "response")

# extract predictions from the 'pred' component and ensure it's a vector
df_clean_hot$pscore <- predictions$pred[, 1]

# check the structure of the predictions
str(df_clean_hot$pscore)

# check pscore
hist(df_clean_hot$pscore)

# make censoring weights
df_clean_hot$weights <- ifelse(df_clean_hot$t0_lost == 1,
                               1 / df_clean_hot$pscore,
                               1 / (1 - df_clean_hot$pscore))

# check 
hist(df_clean_hot$weights)# nothing extreme

# obtain stablise weights
# marginal_not_lost <- mean(df_clean_hot$t0_lost)

# # check (fyi)
# marginal_not_lost


# # stabalised weights
# df_clean_hot$weights_stabilised <- ifelse(
#   df_clean_hot$t0_lost == 1,
#   marginal_not_lost / df_clean_hot$pscore,
#   (1 - marginal_not_lost) / (1 - df_clean_hot$pscore)
# )

# checks
# hist(df_clean_hot$weights_stabilised)
# max(df_clean_hot$weights_stabilised)
# min(df_clean_hot$weights_stabilised)

# save output of hot code dataset
here_save(df_clean_hot, "df_clean_hot")
df_clean_hot <- here_read( "df_clean_hot")

# get weights into the model
# new weights by combining censor and sample weights, using stabalised weights
df_clean$t0_combo_weights = df_clean_hot$weights * df_clean$sample_weights

# checks
min(df_clean$t0_combo_weights)
max(df_clean$t0_combo_weights)

# check distrobution of weights
hist(df_clean$t0_combo_weights)

# next remove those who were lost between t0 and t1
df_clean_t1 <- df_clean |> filter(t0_lost == 0) |>
  select(
    -t0_lost, 
    -sample_weights) |>
  relocate("t0_combo_weights", .before = starts_with("t1_"))

# check
hist(df_clean_t1$t0_combo_weights)

# checks
max(df_clean_t1$t0_combo_weights)
min(df_clean_t1$t0_combo_weights)

# at exposure 
nrow(df_clean_t1)

# number of weighted sample at t1, again check
n_not_lost_sample <- nrow(df_clean_t1)
n_not_lost_sample <- prettyNum(n_not_lost_sample, big.mark = ",")

# check
n_not_lost_sample

# save output for manuscript
here_save(n_not_lost_sample, "n_not_lost_sample")


# no one missing in exposure
# check
table((df_clean_t1$t1_not_lost)) # none

# gets us the correct df for weights

# check column oder and missing ness
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)

#check # looks good
nrow(df_clean_t1)


margot::here_save(df_clean_t1, "df_clean_t1")
df_clean_t1 <- margot::here_read("df_clean_t1")

# get correct censoring -----------------------------------------
# THIS CODE IS NOT REDUNDANT 
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights

df_clean_t2 <- df_clean_t1 %>%
  # select(-t0_alert_level_combined_lead) |>
  mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) %>%
  mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .))) |>
  # mutate(t0_lost = 1 - t0_not_lost) |>
  mutate(t1_lost = 1 - t1_not_lost) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  select(-t1_lost, -t0_not_lost)

# test
nrow(df_clean_t2)
colnames(df_clean_t2)
# checks
hist(df_clean_t2$t0_combo_weights)

# outcomes
naniar::vis_miss(df_clean_t2, warn_large_data = F)

# save
here_save(df_clean_t2, "df_clean_t2")


# check propensity scores -------------------------------------------------
# imbalance plot ----------------------------------------------------------
df_clean_t2 <- here_read("df_clean_t2")
# if you are comparing 2 x subgroups out of n > 2 groups,  do this
# df_female<-df_clean_t2 |> filter(t0_female == 1) |> droplevels()

# check
colnames(df_clean_t2)

# check
str(df_clean_t2)



# names of vars for modelling
names_base <-
  df_clean_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
names_base

# get outcome names for checks
names_outcomes <-
  df_clean_t2 |> select(starts_with("t2")) |> colnames()

# check
names_outcomes

# obsessively check
names(df_clean_t2)

# check against this
names_base
str(df_clean_t2)


# lets one hot encode any categorical vars, here only t0_eth_cat

# this code is the same as above
encoded_vars <- model.matrix(~ t0_religion_cat + t0_parent_education + t0_race  - 1, 
                             data = df_clean_t2)


head(encoded_vars)


# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df |>
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe

# ensure to remove original categorical variables to avoid duplication

# note new data `df_clean_t2`
df_clean_hot_t2 <- df_clean_t2 |>
  select(-c( t0_religion_cat, t0_parent_education, t0_race)) |>
  bind_cols(encoded_df) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  #  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

# check names
colnames(df_clean_hot_t2)

# # extract and print the new column names for encoded variables
# new_encoded_colnames_t2 <- colnames(encoded_df)
# print(new_encoded_colnames_t2)
#
# print(new_encoded_colnames_t2)
# # get baseline variable set without factors
#
# baseline_vars_set_t2 <- setdiff(names(df_clean_hot_t2), c("id","t0_eth_cat"))
# set_final_names <- c(baseline_vars_set_t2, new_encoded_colnames_t2)



# set names for analysis
set_final_names <-
  df_clean_hot_t2 |>
  select(
    starts_with("t0"), -t0_combo_weights,
    # redundant
    -t0_ritual_1aweek
  ) |>
  colnames()


# check
set_final_names

# add the new encoded column names

# check
colnames(df_clean_hot_t2)



colnames( df_clean_hot_t2 ) 




# inclusion criteria ------------------------------------------------------
# propensity score model

match_ebal_ate <- margot::match_mi_general(data = df_clean_hot_t2, 
                                           X = "t1_ritual_1aweek", 
                                           baseline_vars = set_final_names, 
                                           estimand = "ATE",  
                                           #   focal = "1", #for ATT
                                           method = "ebal", 
                                           sample_weights = "sample_weights")


# propensity score summary
propensity_score_summary <- summary(match_ebal_ate)

propensity_score_summary

#view weights
plot(propensity_score_summary)

love_plot_marginal <-
  love.plot(
    match_ebal_ate,
    binary = "std",
    thresholds = c(m = .1),
    wrap = 50,
    position = "bottom",
    size = 3
  )

# inspect imbalance on exposure
love_plot_marginal




# models -----------------------------------------------------------------

# estimate models ---------------------------------------------------------
library(lmtp)
library(SuperLearner)
library(xgboost)
library(ranger)
library(future)
# model charitable giving
# this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
plan(multisession)

# hopefully you have 10 :). if not, consider decreasing the number of folds (for this assessment)
n_cores <- parallel::detectCores() - 2

#### SET VARIABLE NAMES
#  model

A <- c("t1_ritual_1aweek")  # EXPOSURE VARIABLE **************
C <- c("t1_not_lost")
W <- set_final_names


#  checks
colnames(df_final)
naniar::vis_miss(df_final, warn_large_data = F)


# Save 
margot::here_save(df_final, "df_final")
margot::here_save(W, "W")


# really start here -------------------------------------------------------


# ANALYSIS ----------------------------------------------------------------
W <- margot::here_read("W")

df_final <- margot::here_read("df_final")
#
#
gain_A <- function(data, trt) {
  data[[trt]]  = 1  # increase all by 20 up to 80 hours per week
}

# # shift function
# gain_A <- function(data, trt) {
#   ifelse(data[[trt]] < max_data - 1, data[[trt]] + 1, max_data)
# }

loss_A <- function(data, trt) {
   data[[trt]] <-  0
}



# examples of other shift functions 
# # changing your function to be fixed at 7 if you like...
# fixed_shift_to_7 <- function(data, trt) {
#   ifelse(data[[trt]] != 7, 7, data[[trt]])
# }

# # changing your function to be fixed at 0 if you like...
# fixed_shift_to_0 <- function(data, trt) {
#   ifelse(data[[trt]] != 0, 0, data[[trt]])
# }



# set libraries
sl_lib <- c("SL.glmnet", "SL.xgboost", "SL.ranger")

sl_lib <- match_lib
# view superlearners
listWrappers()
# test data


# testing data set

# df_clean_slice <- df_final |>
#   slice_head(n = 1000) |>
#   as.data.frame()




# gain and loss models ---------------------------------------------------
t2_two_or_more_kids_gain_test  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)

margot::here_save(t2_two_or_more_kids_gain,"t2_two_or_more_kids_gain")

t2_two_or_more_kids_gain_test$fits_r
t2_two_or_more_kids_gain_test$fits_m


t2_two_or_more_kids_loss  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W,
  shift = loss_A,
  data = df_cledf_finalan_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(t2_two_or_more_kids_loss,"t2_two_or_more_kids_loss")

t2_two_or_more_kids_loss$fits_r
t2_two_or_more_kids_loss$fits_m


# evaluate
contrast_t2_two_or_more_kids <- lmtp::lmtp_contrast(t2_two_or_more_kids_gain, ref = t2_two_or_more_kids_los, type = "rr")

# table
tab_contrast_t2_two_or_more_kids <-
  margot::margot_lmtp_evalue(contrast_t2_two_or_more_kids,
                             scale = "RR",
                             new_name = "Smoker")

# save
here_save(tab_contrast_t2_two_or_more_kids, "tab_contrast_t2_two_or_more_kids")









# extra validation IGNORE -------------------------------------------------


# EXTRA STUFF
# try highly adaptive lasso (g-computation)
  library(hal9001)
set.seed(123)
df <- dt_hot_coded_prep
Y_hal <- df$t8_at_least_2_kids_in
X_hal <-  df |> select("t0_male", "t0_age_z", "t0_race_binary", "t0_ritual_1aweek", 
                       "t1_ritual_1aweek")
head(X_hal)

# check
colnames(df[new_var_names])

# make hal formula
formula <- formula_hal(
  Y_hal ~h(t0_male,  k=2) + h(t0_race_binary,  k=2) + h(t0_age_z,  k=2) + h(t0_ritual_1aweek,  k=2) + h(t1_ritual_1aweek,  k=6),X = X_hal,
  smoothness_orders = 20, num_knots = 20
)

# check missingness
naniar::vis_miss(df)

# fit model
m_hall <-  fit_hal(
  X= X_hal,
  Y= Y_hal,
  formula = formula, 
  family = "binomial",
  fit_control = list(cv_select = TRUE)
)
summary(m_hall)

# predict from model
X_hal_1 <- X_hal
X_hal_0 <- X_hal
X_hal_1 <- X_hal$t1_ritual_1aweek <- 1
X_hal_0 <- X_hal$t1_ritual_1aweek <- 0

# predictions
pred_0 <- predict(m_hall, new_data = X_hal_0)
pred_1 <- predict(m_hall, new_data = X_hal_1)

# inspect means
mean(pred_0)
mean(pred_1)

# compute risk ratio - stronger than standard parameteric analysis, but not doubly robust
mean(pred_1)/mean(pred_0)

# try defaults for hal
m_hall <-  fit_hal(
  X= X_hal,
  Y= Y_hal,
  family = "binomial",
  fit_control = list(cv_select = TRUE)
)

# predict form default model
X_hal_1 <- X_hal
X_hal_0 <- X_hal
X_hal_1 <- X_hal$t1_ritual_1aweek <- 1
X_hal_0 <- X_hal$t1_ritual_1aweek <- 0
pred_0 <- predict(m_hall, new_data = X_hal_0)
pred_1 <- predict(m_hall, new_data = X_hal_1)

# view mean under each intervention
mean(pred_0)
mean(pred_1)

# compute risk ratio - reduced somewhat
mean(pred_1)/mean(pred_0)

# as compared with our previous results
mod_2$tab_results

# next try semi-parameteric estimation. 
# note none of this is valid without obtaining correct censoring weights
# and the approach might not be valid for a smallish sample however...
library(lmtp)
library(progressr) 
library(future)
library(parallel)
# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 5

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)

# set cores for estimation
library(future)
plan(multisession)

# no longer implimented in lmtp, but in case it comes back
n_cores <- parallel::detectCores() - 2 # save two cores for other work while these models run

# check
n_cores

# set exposure
A  <-"t1_ritual_1aweek"

# set outcome
Y <- "t8_at_least_2_kids_in"

# shift functions 
gain_A <- function(data, trt) {
  ifelse(data[[trt]] != 1, 1,  data[[trt]])
}
zero_A <- function(data, trt) {
  ifelse(data[[trt]] != 0, 0,  data[[trt]])
}

# note lmtp's unconventional use of "censored"
#C <- c("t1_not_lost")

# superlearner libraries
sl_lib <- c(#"SL.glmnet",
  "SL.ranger", # forests
  "SL.glmnet", #
  "SL.xgboost") 

# check covariates
W

# check they are in the datafame
colnames(df)

# none parametric estimation 
gain_church <- lmtp_tmle(
  data = df,
  trt = A,
  baseline = W,
  outcome = Y,
 # cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
 # weights = df$weights...,
  learners_trt = sl_lib,
  learners_outcome =sl_lib
)

margot::here_save(gain_church, "gain_church")

no_church <- lmtp_tmle(
  data = df,
  trt = A,
  baseline = W,
  outcome = Y,
  # cens = C,
  shift = zero_A,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  # weights = df$weights...,
  learners_trt = sl_lib,
  learners_outcome =sl_lib
)

# save model
margot::here_save(no_church, "no_church")

# causal contrast on RR scale
contrast <- lmtp_contrast(gain_church, 
                          ref = no_church, type = "rr")

# view
contrast

# make table
# interpret
lmtp_estimate <- margot::margot_lmtp_evalue(contrast, scale = "RR", new_name= "RR 2 x kids (semi-parametric)")

margot::margot_interpret_table(output, causal_scale = "risk_ratio", estimand = "ATE")

semi_parametric_group_tab <- group_tab(lmtp_estimate)
regression_group_tab <- group_tab(mod_2$tab_result)
combo_tab <- rbind(semi_parametric_group_tab,
                   regression_group_tab)

# compare - semi-parametric is more efficient, but valid errors? 
margot_plot(combo_tab, 
            title = "Causal Effect of Regular Religious Service on Fertilty 7 Years Later", 
            subtitle= "Outcome is Binary (Has Two Children Yes/No)", 
            type = "RR",
           # order = "alphabetical",
            x_lim_hi = 5,
            x_lim_lo = -1, 
            x_offset = -1,
            estimate_scale  = 1)
