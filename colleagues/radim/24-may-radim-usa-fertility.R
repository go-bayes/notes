set.seed(123)

# joseph bulbulia -- july 2024
# analysis for Radim
# contact joseph.bulbulia@gmail.come



# notes
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
 
# exposures
# t1_ritual_scale  = raw continuous/ordered categorical variable for ritual
# t1_ritual_1aweek = dichotomized variable for weekly ritual
# t1_faith_scale   = raw continuous/ordered categorical variable for faith
# t1_faith_full    = dichotomized variable for extreme faith (scoring 5 on scale 1-5)
 
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

library(margot)
# check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}


# install.packages('maq')
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
  SuperLearner,
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
  patchwork,
  margot,
  future,
  progressr,
  maq
)
# update.packages()

library(future)
# model charitable giving
# this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
plan(multisession)

# import data
dat_raw_0 <- read.csv(pull_mods)

# check
head(dat_raw_0)
# create weights for male/not male in t0
# note, please use either "male" or "female" or "not_male"  so that we know what the indicator means


ids_baseline <- dat_raw_0 |>
  dplyr::filter(!is.na(t0_ritual_1aweek)) |> # criteria, no missing in baseline exposure
  pull(X)

dat_raw <- dat_raw_0 |> 
  filter(X %in% ids_baseline)


# sample weights balanced male / female-------------------------------------
# balance on gender weights

# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <-
  0.5  # target proportion of males in the population
prop_female_population <-
  0.5  # target proportion of females in the population

# slighly fewer females
table(dat_raw$t0_female)


table(dat_raw$t1_ritual_1aweek)
table(dat_raw$t0_ritual_1aweek)
table(dat_raw$t1_ritual_scale)


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
  select(-t0_faith_full, -t1_faith_full, -t1_ritual_scale) |>  ## irrelevant 
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


## View missing values
dat_init_miss <- dat_init |> 
  select(-id, -sample_weights) |> 
  relocate(starts_with('t0'), .before = starts_with('t1')) |> 
  relocate(starts_with('t1'), .before = starts_with('t2'))

 missing_total <- naniar::vis_miss(dat_init_miss)

#view
missing_total
here_save(missing_total, 'missing_total')


# single imputation ------------------------------------------------------

# data must be a dataframe
dat_init <- as.data.frame(dat_init )

# prepare data
dat_init <- margot::remove_numeric_attributes(dat_init)
# test total n in the data

n_total <- nrow(dat_raw_0)

# get comma in number
n_total <- prettyNum(n_total, big.mark = ",")

# check
n_total

# save n for manuscript
margot::here_save(n_total, "n_total")

# test total n in the data
n_participants <- nrow(dat_init)

# get comma in number
n_participants <- prettyNum(n_participants, big.mark = ",")

# check n total
n_participants

# save n for manuscript
margot::here_save(n_participants, "n_participants")

# name of exposure

# check missing values
skimr::skim(dat_init) |> arrange(n_missing)


# single impute missing values at baseline 
library(mice)

# only impute pre-exposure data using pre-exposure information
table(dat_init$t0_parent_education )

# define the levels of t0_parent_education in the desired order
education_levels <- c("Less than 12th grade", "High school", "Beyond high school")



# transition matrix ------------------------------------------------------

exposure_dat <- dat_init |> 
  select(id, t0_ritual_1aweek, t1_ritual_1aweek) |>
  pivot_longer(
    cols = starts_with("t"),
    names_to = c("wave", "measure"),
    names_pattern = "t(\\d+)_(.*)",
    values_to = "value"
  ) |>
  mutate(
    wave = as.integer(wave)
  )

exposure_dat_2 <- dat_init |>
  select(id, t0_ritual_scale) |>
  pivot_longer(
    cols = starts_with("t"),
    names_to = c("wave", "measure"),
    names_pattern = "t(\\d+)_(.*)",
    values_to = "value"
  ) |>
  mutate(
    wave = as.integer(wave)
  )


print(exposure_dat) 
head(exposure_dat)

print(exposure_dat_2) 
head(exposure_dat_2)
   

# create transition matrix

out <-margot::create_transition_matrix(
  data = exposure_dat, 
  state_var = "value", 
  id_var = "id"
)
219 + 176

t_labels <- c("< weekly", ">= weekly")
# transition table

transition_table  <- margot::transition_table(out, state_names = t_labels)


transition_table
# for import later
margot::here_save(transition_table, "transition_table")


# second matrix

out_2 <-margot::create_transition_matrix(
  data = exposure_dat_2, 
  state_var = "value", 
  id_var = "id"
)


t_tab_2_labels <- #c("< weekly", ">= weekly")
# transition table

transition_table_2  <- margot::transition_table(out_2)
transition_table_2
# for import later
margot::here_save(transition_table_2, "transition_table_2")



# baseline table ---------------------------------------------------------
# prepare df

dat_baseline <- dat_init |>
  select(starts_with("t0_")) |>
  mutate(
    t0_religion_cat = as.factor(t0_religion_cat),
    t0_parent_education = factor(t0_parent_education, levels = education_levels, ordered = TRUE),
    t0_race = as.factor(t0_race)
  )


dat_baseline_use  <- dat_baseline |> select(starts_with("t0"), - t0_not_lost) |> 
  # remove t0 label
  rename_with(~ str_remove(., "^t0_")) |> 
    mutate(ritual_weekly = as.factor(ritual_1aweek)) |> 
  select(-ritual_1aweek) #|> 
  #relocate(ritual_weekly, .after = ritual_scale)
  

# sort column names
dat_baseline_sorted <- sort(
  colnames(dat_baseline_use)
)

dat_baseline_use

# table1::table1( ~ age + faith_scale + parent_income + parent_education + ritual_weekly + race|female , data = dat_baseline_use)

#check
#selected_base_cols <- setdiff(selected_base_cols)
# baseline table

library(gtsummary)
table_baseline <- dat_baseline_use |> 
select(all_of(dat_baseline_sorted)) |>
  janitor::clean_names(case = "title") |> 
  tbl_summary(
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", # Mean and SD
        "{min}, {max}", # Range (Min, Max)
        "{p25}, {p75}" # IQR (25th percentile, 75th percentile)
      )
    ),
    type = all_continuous() ~ "continuous2"
  ) |>
  modify_header(label = "**Baseline Exposure + Demographic Variables**") |> # update the column header
  bold_labels() 


table_baseline
# save baseline 
# here_save(table_baseline, "table_baseline")
# 

here_save(table_baseline, 'table_baseline')


library(tidyr)



# cross table



# exposure table ---------------------------------------------------------


# reshaping and separating into distinct columns for ritual_weekly and ritual_scale
exposure_dat_table <- dat_init |>
  select(id, t0_ritual_1aweek, t1_ritual_1aweek, 
  #  t0_ritual_scale, t1_ritual_scale
  ) |>
  pivot_longer(
    cols = starts_with("t"),
    names_to = c("wave", "measure"),
    names_pattern = "t(\\d+)_(.*)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = value,
    names_glue = "ritual_{measure}"
  ) |>
  mutate(
    wave = as.integer(wave)
  ) |>
  rename(
    ritual_weekly = ritual_ritual_1aweek
  ) |> 
    # rename(
    #   ritual_scale = ritual_ritual_scale
    # ) |> 
  mutate(ritual_weekly = as.factor(ritual_weekly))

print(exposure_dat_table)

head(dat_init)


# make tables
table_exposures <- exposure_dat_table|>
  select(-id) |> 
  janitor::clean_names(case = "title")|> 
  labelled::to_factor()|>  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  )|>
  #  add_n()|>  # Add column with total number of non-missing observations
  modify_header(label = "**Exposure Variables by Wave**")|>  # Update the column header
  bold_labels()

table_exposures

here_save(table_exposures, 'table_exposures')

# not needed, done above
# transition_table_dat <- exposure_dat_table |> 
#  # filter(!is.na(ritual_weekly)) |> 
#   mutate(ritual_weekly = as.numeric(as.character(ritual_weekly)))
# transition_table_dat

# # transition_table

# out <-margot::create_transition_matrix(
#   data = transition_table_dat, 
#   state_var = "ritual_weekly", 
#   id_var = "id"
# )

transition_table

# tt_labels <- c("< weekly", ">= weekly")
# # transition table

# transition_table  <- margot::transition_table(out)
# transition_table
# # for import later

# margot::here_save(transition_table, "transition_table")



# outcome table ----------------------------------------------------------

outcome_dat  <- dat_init |>
  select(t2_two_or_more_kids, t2_children_count)|>
  rename_with(~ str_remove(., "^t2_")) |> 
  mutate(two_or_more_kids = as.factor(two_or_more_kids))


names_outcomes_sorted <- colnames(outcome_dat)

names_outcomes_sorted


# names_outcomes_final
# better names
selected_outcome_cols <-
  names_outcomes_sorted 

# checks
str(selected_outcome_cols)
colnames(selected_outcome_cols)

table_outcomes <- outcome_dat %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
   # by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_outcomes


here_save(table_outcomes, "table_outcomes")

# sums correctly 
# 156 + 48 + 16 + 2

# convert t0_parent_education to an ordered factor


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

here_save(df_clean, "df_clean")

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
                               "t0_race", 
                              "t0_ritual_1aweek"))# Redundant

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
# compute censoring weights for loss to follow up, after time 1
library(SuperLearner)

# library for multicore processing
library(doParallel)

# learners
listWrappers()

# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# you can probably just use "SL.glmnet", but no harm using more
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger", "SL.earth", "SL.polymars")
sl_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger", "SL.earth", "SL.polymars")


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
hist(df_clean_hot$sample_weights)# nothing extreme
#obtain stablised weights, if needed
marginal_not_lost <- mean(df_clean_hot$t0_lost)

# check (fyi)
marginal_not_lost


# stabalised weights
df_clean_hot$weights_stabilised <- ifelse(
  df_clean_hot$t0_lost == 1,
  marginal_not_lost / df_clean_hot$pscore,
  (1 - marginal_not_lost) / (1 - df_clean_hot$pscore)
)

#checks
#hist(df_clean_hot$weights_stabilised)
max(df_clean_hot$weights_stabilised)
min(df_clean_hot$weights_stabilised)

# save output of hot code dataset
here_save(df_clean_hot, "df_clean_hot")
df_clean_hot <- here_read( "df_clean_hot")

nrow(df_clean)
nrow(df_clean_hot)

# get weights into the model
# new weights by combining censor and sample weights, using stabalised weights
df_clean$t0_combo_weights = df_clean_hot$weights_stabilised * df_clean$sample_weights

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


# THIS CODE IS **NOT** REDUNDANT 
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights

df_clean_t2 <- df_clean_t1|>
  mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost))|>
  mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost))|>
  mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)))|>
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

df_clean_no_na_treatment_one <- df_clean_t2 |> filter(!is.na(t1_ritual_1aweek))

names_propensity  <- df_clean_t2 |> select(c( starts_with('t0_'))) |> colnames()

names_propensity_use  <- setdiff( names_propensity, c("t0_combo_weights", 't0_ritual_1aweek'))

names_propensity_use


match_ebal_one<- margot::match_mi_general(data = df_clean_no_na_treatment_one,
                              X = "t1_ritual_1aweek",
                              baseline_vars = names_propensity_use,
                              estimand = "ATE",
                              #  focal = 0, #for ATT
                              method = "ebal",
                              sample_weights = "t0_sample_weights")

# save
here_save(match_ebal_one, "match_ebal_one")



bal.tab(match_ebal_one)
love_plot_one <- love.plot(match_ebal_one, binary = "std", thresholds = c(m = .1),
                       wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-1, 2))) 
love_plot_one
here_save(love_plot_one, "love_plot_one")

# consider results 
summary_match_ebal_one <- summary(match_ebal_one)
summary_match_ebal_one
here_save(summary_match_ebal_one, "summary_match_ebal_one")

plot(summary_match_ebal_one)



# For trimmed weights e.g.
#trim if needed (weights > 10 might be a problem)
match_ebal_trim <- WeightIt::trim(match_ebal_one, at = .99)
#bal.tab(match_ebal_trim_health)
summary_match_ebal_trim<- summary(match_ebal_trim)
plot(summary_match_ebal_trim)



# set variable names ------------------------------------------------------
# check
colnames(df_clean_t2)

# check
str(df_clean_t2)



# names of vars for modelling
names_base <-
  df_clean_t2 |> select(starts_with("t0"), -t0_combo_weights, -t0_ritual_1aweek) |> colnames()

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


# final dataset 
df_final <- df_clean_hot_t2

colnames(df_final)
naniar::vis_miss(df_final, warn_large_data = F)


# Save 
margot::here_save(df_final, "df_final")

# models -----------------------------------------------------------------

# estimate models ---------------------------------------------------------
library(lmtp)
library(SuperLearner)
library(xgboost)
library(ranger)
library(future)
library(polspline)
#
progressr::handlers(global = TRUE)

# set seed for reproducing results
plan(multisession)

# hopefully you have 10 :). if not, consider decreasing the number of folds (for this assessment)
n_cores <- parallel::detectCores() - 2

#### SET VARIABLE NAMES
#  model

A <- c("t1_ritual_1aweek")  # EXPOSURE VARIABLE **************
C <- c("t1_not_lost")
set_final_names
W <- set_final_names
W

# save
margot::here_save(W, "W")

# really start here -------------------------------------------------------

# ANALYSIS ----------------------------------------------------------------
W <- margot::here_read("W")

df_final <- margot::here_read("df_final")
#
#
A <- c("t1_ritual_1aweek")  # EXPOSURE VARIABLE **************
C <- c("t1_not_lost")

gain_A <- function(data, trt) {
  data[[trt]]  = 1  # increase all by 20 up to 80 hours per week
}

loss_A <- function(data, trt) {
   data[[trt]] <-  0
}

citation(package = 'glmnet')
# set libraries
sl_lib  <- c("SL.glmnet", "SL.xgboost", "SL.ranger", "SL.polymars", "SL.earth")





# gain and loss models ---------------------------------------------------
two_or_more_kids_gain  <- lmtp::lmtp_tmle(
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
margot::here_save(two_or_more_kids_gain,"two_or_more_kids_gain")

two_or_more_kids_gain$fits_r
two_or_more_kids_gain$fits_m

two_or_more_kids_gain


two_or_more_kids_loss  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W,
  shift = loss_A,
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
margot::here_save(two_or_more_kids_loss,"two_or_more_kids_loss")

two_or_more_kids_loss$fits_r
two_or_more_kids_loss$fits_m

two_or_more_kids_loss

# evaluate
two_or_more_kids_gain <- here_read("two_or_more_kids_gain")
two_or_more_kids_loss <- here_read('two_or_more_kids_loss')

contrast_t2_two_or_more_kids <- lmtp::lmtp_contrast(two_or_more_kids_gain, 
  ref = two_or_more_kids_loss, type = "rr")

contrast_t2_two_or_more_kids
# table

tab_contrast_t2_two_or_more_kids <-
  margot::margot_lmtp_evalue(contrast_t2_two_or_more_kids,
                             scale = "RR",
                             new_name = "Weekly Religious Service Attendance: ATE")

                      
tab_contrast_t2_two_or_more_kids
# save
margot::here_save(tab_contrast_t2_two_or_more_kids, "tab_contrast_t2_two_or_more_kids")


margot::margot_interpret_table(tab_contrast_t2_two_or_more_kids, causal_scale = "risk_ratio", estimand = "ATE")


group_tab_contrast_t2_two_or_more_kids <- margot::group_tab(tab_contrast_t2_two_or_more_kids)

# compare - semi-parametric is more efficient, but valid errors? 
margot::margot_plot(group_tab_contrast_t2_two_or_more_kids, 
            title = "Causal Effect of Regular Religious Service on Fertility 8 Years Later", 
            subtitle= "Outcome is Binary (Has Two Children Yes/No)", 
            type = "RR",
           # order = "alphabetical",
            x_lim_hi = 3,
            x_lim_lo = -1, 
            x_offset = -1,
            estimate_scale  = 1)




tab_contrast_t2_two_or_more_kids<- margot::here_read('tab_contrast_t2_two_or_more_kids')
# model subset -----------------------------------------------------------

# data male only 
df_final <- here_read("df_final")

# read predictor vars
W <- here_read("W")

# get rid of female
W_sub = setdiff(W, "t0_female")

W_sub
df_final_male <- df_final |> filter(t0_female == 0)
df_final_female <- df_final |> filter(t0_female == 1)


# gain and loss models ---------------------------------------------------
male_two_or_more_kids_gain  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W_sub,
  shift = gain_A,
  data = df_final_male,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final_male$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(male_two_or_more_kids_gain,"male_two_or_more_kids_gain")

male_two_or_more_kids_gain$fits_r
male_two_or_more_kids_gain$fits_m

male_two_or_more_kids_gain


 male_two_or_more_kids_loss  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W_sub,
  shift = loss_A,
  data = df_final_male,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final_male$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(male_two_or_more_kids_loss,"male_two_or_more_kids_loss")

male_two_or_more_kids_loss$fits_r
male_two_or_more_kids_loss$fits_m

male_two_or_more_kids_loss

# evaluate
contrast_male_t2_two_or_more_kids <- lmtp::lmtp_contrast(male_two_or_more_kids_gain, ref = male_two_or_more_kids_loss, type = "rr")

contrast_male_t2_two_or_more_kids
# table

tab_contrast_male_t2_two_or_more_kids <-
  margot::margot_lmtp_evalue(contrast_male_t2_two_or_more_kids,
                             scale = "RR",
                             new_name = "Weekly Religious Service Attendance: Males")

                      
tab_contrast_male_t2_two_or_more_kids
# save
margot::here_save(tab_contrast_male_t2_two_or_more_kids, "tab_contrast_male_t2_two_or_more_kids")


margot::margot_interpret_table(tab_contrast_male_t2_two_or_more_kids, causal_scale = "risk_ratio", estimand = "ATE")

# group_tab_contrast_male_t2_two_or_more_kids <- margot::group_tab(tab_contrast_male_t2_two_or_more_kids)

# compare - semi-parametric is more efficient, but valid errors? 
margot::margot_plot(tab_contrast_male_t2_two_or_more_kids, 
            title = "Causal Effect of Regular Religious Service on Fertility 8 Years Later", 
            subtitle= "Outcome is Binary (Has Two Children Yes/No)", 
            type = "RR",
           # order = "alphabetical",
            x_lim_hi = 7,
            x_lim_lo = -1, 
            x_offset = -1,
            estimate_scale  = 1)



# female stratify --------------------------------------------------------


female_two_or_more_kids_gain  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W_sub,
  shift = gain_A,
  data = df_final_female,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final_female$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(female_two_or_more_kids_gain,"female_two_or_more_kids_gain")

female_two_or_more_kids_gain$fits_r
female_two_or_more_kids_gain$fits_m

female_two_or_more_kids_gain


 female_two_or_more_kids_loss  <- lmtp::lmtp_tmle(
  outcome = "t2_two_or_more_kids",
  baseline = W_sub,
  shift = loss_A,
  data = df_final_female,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final_female$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)

female_two_or_more_kids_loss

margot::here_save(female_two_or_more_kids_loss,"female_two_or_more_kids_loss")

female_two_or_more_kids_loss$fits_r
female_two_or_more_kids_loss$fits_m

female_two_or_more_kids_loss

# evaluate
contrast_female_t2_two_or_more_kids <- lmtp::lmtp_contrast(female_two_or_more_kids_gain, ref = female_two_or_more_kids_loss, type = "rr")

contrast_female_t2_two_or_more_kids
# table

tab_contrast_female_t2_two_or_more_kids <-
  margot::margot_lmtp_evalue(contrast_female_t2_two_or_more_kids,
                             scale = "RR",
                             new_name = "Weekly Religious Service Attendance: Females")
# save
margot::here_save(tab_contrast_female_t2_two_or_more_kids, "tab_contrast_female_t2_two_or_more_kids")


margot::margot_interpret_table(tab_contrast_female_t2_two_or_more_kids, causal_scale = "risk_ratio", estimand = "ATE")


# compare - semi-parametric is more efficient, but valid errors? 
margot::margot_plot(tab_contrast_female_t2_two_or_more_kids, 
            title = "Causal Effect of Regular Religious Service on Fetility 8 Years Later", 
            subtitle= "Outcome is Binary (Has Two Children Yes/No)", 
            type = "RR",
           # order = "alphabetical",
            x_lim_hi = 3,
            x_lim_lo = -1, 
            x_offset = -1,
            estimate_scale  = 1)


# compute the relative risk ratio between the two groups
sub_group_compare_binary <- margot::compute_difference(contrast_male_t2_two_or_more_kids, contrast_female_t2_two_or_more_kids, type = "RR")

sub_group_compare_binary


here_save(sub_group_compare_binary, "sub_group_compare_binary")

str(contrast_male_t2_two_or_more_kids)
str(contrast_female_t2_two_or_more_kids)
contrast_male_t2_two_or_more_kids$theta


# compute the difference in means between the two groups
result <- compute_rrr(contrast_male_t2_two_or_more_kids, contrast_female_t2_two_or_more_kids)

# display the result
print(result$results)
print(result$interpretation)
compute_difference_means_test(contrast_male_t2_two_or_more_kids, contrast_female_t2_two_or_more_kids)

contrast_female_t2_two_or_more_kids$std.error
contrast_female_t2_two_or_more_kids
contrast_female_t2_two_or_more_kids$std.error


contrast_male_t2_two_or_more_kids



# plots of the subgroups -------------------------------------------------
tab_contrast_female_t2_two_or_more_kids <- here_read("tab_contrast_female_t2_two_or_more_kids")
tab_contrast_male_t2_two_or_more_kids <- here_read("tab_contrast_male_t2_two_or_more_kids")
tab_contrast_t2_two_or_more_kids <- here_read("tab_contrast_t2_two_or_more_kids")

df_bind_results <- rbind(tab_contrast_female_t2_two_or_more_kids, tab_contrast_male_t2_two_or_more_kids, tab_contrast_t2_two_or_more_kids)

group_bind_results <- margot::group_tab(df_bind_results)


margot::margot_plot(group_bind_results, 
  title = "Causal Effect of Regular Religious Service on Fertility 8 Years Later", 
  subtitle= "Outcome is Binary (Has Two Children Yes/No)", 
  type = "RR",
  order = "alphabetical",
  x_lim_hi = 7,
  x_lim_lo = -7, 
  x_offset = -1.75,
  estimate_scale  = 4)




# total children -----------------------------------------------


# model total number of children  ----------------------------------------
children_count_gain   <- lmtp::lmtp_tmle(
  outcome = "t2_children_count",
  baseline = W,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(children_count_gain,"children_count_gain")

children_count_gain$fits_r
children_count_gain$fits_m

children_count_gain



children_count_loss  <- lmtp::lmtp_tmle(
  outcome = "t2_children_count",
  baseline = W,
  shift = loss_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt =  sl_lib,
  learners_outcome = sl_lib,
)
margot::here_save(children_count_loss,"children_count_loss")

children_count_loss$fits_r
children_count_loss$fits_m

children_count_loss

# evaluate

children_count_gain <- here_read('children_count_gain')
children_count_loss<- here_read('children_count_loss')

contrast_t2_children_count <- lmtp::lmtp_contrast(children_count_gain, ref = children_count_loss, type = "additive")

contrast_t2_children_count
# table


tab_contrast_t2_children_count <-
  margot::margot_lmtp_evalue(contrast_t2_children_count,
                             scale = "RD",
                             new_name = "Weekly Service Total Children: ATE")

tab_contrast_t2_children_count                      
# save
margot::here_save(tab_contrast_t2_children_count, "tab_contrast_t2_children_count")


margot::margot_interpret_table(tab_contrast_t2_children_count, causal_scale = "causal_difference", estimand = "ATE")

# group_tab_contrast_t2_children_count <- margot::group_tab(tab_contrast_t2_children_count, type = "RD")

# compare - semi-parametric is more efficient, but valid errors? 

margot::margot_plot(tab_contrast_t2_children_count, 
            title = "Causal Effect of Regular Religious Service on Fertility 7 Years Later", 
            subtitle= "Outcome is Continuous (Number of Children)", 
            type = "RD",
           # order = "alphabetical",
            estimate_scale  = 1)



# total children men ---------------------------------------------------


# model total number of children  ----------------------------------------
children_count_gain_men   <- lmtp::lmtp_tmle(
  outcome = "t2_children_count",
  baseline = W_sub,
  shift = gain_A,
  data = df_final_male,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final_male$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(children_count_gain_men,"children_count_gain_men")

children_count_gain_men$fits_r
children_count_gain_men$fits_m

children_count_gain_men


children_count_loss_men  <- lmtp::lmtp_tmle(
  outcome = "t2_children_count",
  baseline = W_sub,
  shift = loss_A,
  data = df_final_male,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final_male$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(children_count_loss_men,"children_count_loss_men")

children_count_loss_men$fits_r
children_count_loss_men$fits_m

children_count_loss_men



# counts women loss ------------------------------------------------------

# model total number of children  ----------------------------------------
children_count_gain_female   <- lmtp::lmtp_tmle(
  outcome = "t2_children_count",
  baseline = W_sub,
  shift = gain_A,
  data = df_final_female,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final_female$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(children_count_gain_female,"children_count_gain_female")

children_count_gain_female$fits_r
children_count_gain_female$fits_m


children_count_loss_female  <- lmtp::lmtp_tmle(
  outcome = "t2_children_count",
  baseline = W_sub,
  shift = loss_A,
  data = df_final_female,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final_female$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome =  sl_lib,
)
margot::here_save(children_count_loss_female,"children_count_loss_female")

children_count_loss_female$fits_r
children_count_loss_female$fits_m

children_count_loss_female

# evaluate

contrast_t2_children_count_men <- lmtp::lmtp_contrast(children_count_gain_men, ref = children_count_loss_men, type = "additive")

contrast_t2_children_count_men
# table

tab_contrast_t2_children_count_men <-
  margot::margot_lmtp_evalue(contrast_t2_children_count_men,
                             scale = "RD",
                             new_name = "Weekly Service Total Children in Males")

                      
# save
margot::here_save(tab_contrast_t2_children_count_men, "tab_contrast_t2_children_count_men")


# female

contrast_t2_children_count_female <- lmtp::lmtp_contrast(children_count_gain_female, 
  ref =children_count_loss_female, type = "additive")

contrast_t2_children_count_female

# table

tab_contrast_t2_children_count_female <-
  margot::margot_lmtp_evalue(contrast_t2_children_count_female,
                             scale = "RD",
                             new_name = "Weekly Service Total Children in Females")

                      
# save
margot::here_save(tab_contrast_t2_children_count_female, "tab_contrast_t2_children_count_female")

contrast_t2_children_count_female
contrast_t2_children_count_men


sub_group_compare_count <- margot::compute_difference(contrast_t2_children_count_men, contrast_t2_children_count_female, type = "RD")
sub_group_compare_count




tab_contrast_t2_children_count_men
tab_contrast_t2_children_count_female

# graphs


rbind_total <- rbind( 
  tab_contrast_t2_children_count, 
  tab_contrast_t2_children_count_men,
  tab_contrast_t2_children_count_female) 


  # compare - semi-parametric is more efficient, but valid errors? 
  
  margot::margot_plot(rbind_total, 
              title = "Causal Effect of Regular Religious Service on Fertility 7 Years Later", 
              subtitle= "Outcome is Continuous (Number of Children)", 
              type = "RD",
             # order = "alphabetical",
              x_lim_hi = 1,
              x_lim_lo = -1, 
              x_offset = -1,
              estimate_scale  = 1)
  

# GRF model for effect heterogeneity -------------------------------------

# HETEROGENEITY -----------------------------------------------------------

# IGNORE BELOW -- THIS IS EXTRA FOR INVESTIGATING HETEROGENEITY -----------

# EXTRA: heterogeneity with GRF -------------------------------------------
# see: https://grf-labs.github.io/grf/
#devtools::install_github("grf-labs/grf", subdir = "r-package/grf")
library(grf)
library(margot)
library(tidyverse)
# read data
colnames (df_final)

# read data
df_grf <- margot::here_read("df_final")


df_grf <- df_grf |> 
  select(-id)


nrow(df_grf)
# get baseline names
names_grf <- margot::here_read("W")

names_grf <- setdiff(W, "t0_ritual_scale")

names_grf <- c(names_grf, "t0_ritual_1aweek")

names_grf
head(W)
# check all indicators are numeric or binary
colnames(df_grf)
str(df_grf)# we won't use "id"
a

# check

# set up data


# # set up superlearner
# cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# # Set up parallel back end
# no_cores <- detectCores()
# cl <- makeCluster(no_cores - 1)
# registerDoParallel(cl)


# # you can probably just use "SL.glmnet"  # defined above
# match_lib <- c("SL.glmnet", "SL.xgboost", "SL.ranger", "SL.polymars", "SL.earth")


# sl_2 <- SuperLearner(
#   Y = df_grf$t1_lost,
#   X = cen_X,
#   # use specified predictors
#   SL.library = match_lib,
#   family = binomial(),
#   method = "method.NNloglik",
#   cvControl = list(V = 10)
# )



# # save your super learner model
# here_save(sl_2, "sl_2")
# sl_2 <-  here_read("sl_2")

# # stop the cluster
# stopCluster(cl)


# # check outputs
# print(sl_2)                  # summary of the SuperLearner output
# summary(sl_2)                # a detailed summary, including cross-validated risks

# # examination of cross-validated performance
# sl_2$cvRisk                  # cross-validated risks for each learner
# sl_2$coef                    # weights assigned to each learner in the final ensemble


# plogis(sl_2$cvRisk)
# cen_X
# # generate predictions

# predictions_super <- predict(sl_2, newdata = cen_X, type = "response")

# predictions_super


# # very similar to grf
# mean(predictions_super$pred[, 1])
# sd(predictions_super$pred[, 1])


# # extract predictions from the 'pred' component and ensure it's a vector
# df_grf$super_pscore <- predictions_super$pred[, 1]

# # check the structure of the predictions
# str(df_grf$super_pscore)
# mean(df_grf$super_pscore)  # same as grf

# # check pscore
# hist(df_grf$super_pscore)

# # make censoring weights
# df_grf$super_weights <- ifelse(t1_lost == 1, 1 / df_grf$super_pscore, 1 / (1 - df_grf$super_pscore))

# # check

# hist(df_grf$super_weights, breaks = 50)


# # stabalise
# df_grf$weights_stabilised_super <- ifelse(
#   df_grf$t1_lost == 1,
#   marginal_censored / df_grf$super_pscore,
#   (1 - marginal_censored) / (1 - df_grf$super_pscore)
# )



# # checks
# hist(df_grf$weights_stabilised_super , breaks = 50)
# max(df_grf$weights_stabilised_super)
# min(df_grf$weights_stabilised_super)

# # compare with causal forest
# hist(df_grf$weights_stabilised , breaks = 50)
# max(df_grf$weights_stabilised)
# min(df_grf$weights_stabilised)

# # Load necessary library for evaluation
# library(pROC)

# # Compute ROC curve and AUC
# roc_curve <- roc(Y, predicted_probs)
# auc_value <- auc(roc_curve)
# print(auc_value)

# # Plot ROC curve
# plot(roc_curve)
# # lets use superlearner (it gives us forests and more)


# ok for combo weights to be labelled t0 because we just have a point estimate
df_grf$t0_combo_weights_w2 <- df_grf$weights_stabilised  * df_grf$t0_combo_weights

hist(df_grf$t0_combo_weights_w2 , breaks = 50)
max(df_grf$t0_combo_weights_w2)
min(df_grf$t0_combo_weights_w2)

colnames(df_grf)
margot::here_save(df_grf, "df_grf")

df_grf <- margot::here_read("df_grf")

df_grf_t2 <- df_grf |>
  filter(t1_lost == 0) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |> 
  select(-c(
    #super_pscore, 
    # super_weights, 
    # cen_weights,
    # weights_stabilised, 
    # weights_stabilised_super, 
    t1_lost,
     t1_not_lost)) |> 
  droplevels()

colnames(df_grf_t2)

vis_miss(df_grf)

# good
vis_miss(df_grf_t2)


# save data with weights
here_save(df_grf_t2, "df_grf_t2")

# check
table(df_grf$t1_ritual_1aweek)

#test how many males at end of study
df_test <- df_grf |> 
  filter(t1_lost == 0)

table(df_test$t0_female) # more females



# variables need to be in matrix form

# make it so that large is good
g_W  = matrix(df_grf_t2$t1_ritual_1aweek)
#t2_two_or_more_kids = matrix(df_grf_t2$t2_two_or_more_kids)

# make it so that large is good
g_Y = matrix(df_grf_t2$t2_two_or_more_kids)

g_weights <- df_grf_t2$t0_combo_weights_w2

temp_df<- data.frame( df_grf_t2[ names_grf ] ) 

temp_df <- margot::remove_numeric_attributes(temp_df)

colnames(temp_df)

# data
g_X <- temp_df
colnames(g_X)

naniar::vis_miss(g_W)

table(is.na(g_Y))
# checks
str(g_W)
str(g_X)
str(g_W)
str(g_Y)
str(g_weights)
g_Y


average_treatment_effect(csf)


# model 
tau_forest_outcome <- grf::causal_forest(
  X = g_X,
  Y = g_Y,
  W = g_W,
  sample.weights = g_weights
)

# save
here_save(
  tau_forest_outcome,
  'tau_forest_outcome'
)


# view
tau_forest_outcome
colnames(g_X)
# ATE
forest_ate <- average_treatment_effect(tau_forest_outcome, target.sample = "all")


# save
here_save(forest_ate, "forest_ate")

# check out
forest_ate

# model 

tau_forest_y_binary <- grf::causal_forest(
  X = g_X,
  Y = g_Y,
  W = g_W,
  sample.weights = g_weights
)

# save
here_save(tau_forest_y_binary, 'tau_forest_y_binary')


# ATE
bin_y_forest_ate <- average_treatment_effect(tau_forest_y_binary,
   target.sample = "overlap")
bin_y_forest_ate

# save
here_save(bin_y_forest_ate, "bin_y_forest_ate")


# not possible with a continuous treatement, but this is the code for binary treatments
bin_att <- average_treatment_effect(tau_forest_y_binary, target.sample = "treated")
bin_att

table( g_W )
table(df_final$t1_ritual_1aweek)
# get a histogram that shows heterogeniety
tau.hat.oob <- predict(tau_forest_y_binary)

# show
hist(tau.hat.oob$predictions)

# description of heterogeneity
best_linear_projection(tau_forest_y_binary, g_X)

names_grf

# this only works for binary treatments
rate <- rank_average_treatment_effect(tau_forest_y_binary, g_X[, "t0_female"])

#
plot(rate, ylab = "Female", main = "TOC: ranked by decreasing ")

# #
forest.W <- regression_forest(g_X, g_W, tune.parameters = "all")
# #
W.hat <- predict(forest.W)$predictions
#
# #
forest.Y <- regression_forest(g_X, g_Y, tune.parameters = "all")
#
# #
Y.hat <- predict(forest.Y)$predictions


forest.Y.varimp <- variable_importance(tau_forest_y_binary)
#forest.Y.varimp
selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.99)
selected.vars

colnames( g_X[, selected.vars])

# obtain treatment effect in the most important predictors
tau.forest <- causal_forest(
  g_X[, selected.vars],
  g_Y,
  g_W,
  W.hat = W.hat,
  Y.hat = Y.hat,
  tune.parameters = "all",
  sample.weights = g_weights
)

# not must different
average_treatment_effect(tau.forest, target.sample = "all")


# training sample
n <- nrow(g_X) # n in sample

set.seed(123)
train <- sample(1:n, n / 2) # get half sampl
train

# training sample
train.forest <- causal_forest(g_X[train, ], g_Y[train], g_W[train], sample.weights = g_weights[train])

# eavaluation sample
eval.forest <- causal_forest(g_X[-train, ], g_Y[-train], g_W[-train], sample.weights = g_weights[-train])

# rank on new data (ony supports binary treatment effects)
rate <- rank_average_treatment_effect(eval.forest, predict(train.forest, g_X[-train, ])$predictions)
plot(rate)
g_X
eval.forest
head(g_X)

g_X[ ,t0_female ]
g_X$t0_female

rate <- rank_average_treatment_effect(eval.forest, g_X[ ,t0_female ])
plot(rate, ylab = "Days until first catch", main = "TOC: ranked by decreasing weight")

plot(rate, ylab = "Days until first catch", main = "TOC: ranked by decreasing weight")

#tau.hat <- predict(tau.forest, X.test, estimate.variance = TRUE)
# paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))


##
library(policytree)
library(DiagrammeR)

# get ate
ate <- average_treatment_effect(tau_forest_y_binary)

# check for overlap
hist(tau_forest_y_binary$W.hat)

# quick eval
varimp <- variable_importance(tau_forest_y_binary)
varimp
ranked.vars <- order(varimp, decreasing = TRUE)
ranked.vars


# access the column names from your data frame using these indices
ranked.cols <- colnames(g_X)[ranked.vars]

# display the ordered
ranked.cols


# not much evidence for heterogeneity!
#best_linear_projection(tau_forest_y_binary, g_X[ranked.vars[1:5]])


# Compute doubly robust scores
dr.scores <- grf::get_scores(tau_forest_y_binary)


# will only work for binary variables
dr.scores <- double_robust_scores(tau_forest_y_binary)
dr.scores

# # Use as the ATE as a "cost" of program treatment to find something non-trivial
# cost <- ate[["estimate"]]
# cost
# -dr.scores
# #
# dr.rewards <- cbind.data.frame(control = -dr.scores,
#                      treat = dr.scores - cost)
# dr.rewards
# # plot overlap
use_X <- g_X[, selected.vars]
head(use_X)
tree <- policy_tree(use_X, dr.scores, depth = 2)
tree_full <- policy_tree(g_X, dr.scores, depth = 3)

tree_full_2 <- policy_tree(g_X, dr.scores, depth = 2)

plot( tree_full_2 )

png("tree_full", width = 1200, height = 800)
plot(tree_full)

#save
here_save(tree, "tree")
here_save(tree_full, "tree_full")

print(tree)
plot(tree)
dev.off()
print(tree_full)
plot(tree_full)


tree_full

print(tree_full_2)
plot(tree_full_2)

# Predict the treatment assignment {1, 2} for each sample.
# predicted <- predict(tree_full, g_X)
# plot(X[, 1], X[, 2], col = predicted)
# legend("topright",
#        c("control", "treat"),
#        col = c(1, 2),
#        pch = 19)
# abline(0, -1, lty = 2)
# dev.off()
# node.id <- predict(tree_full, g_X, type = "node.id")
# node.id
# values <- aggregate(
#   dr.scores,
#   by = list(leaf.node = node.id),
#   FUN = function(x)
#     c(mean = mean(x), se = sd(x) / sqrt(length(x)))
# )
# print(values, digits = 2)


# eval grf fit ------------------------------------------------------------


# eval fit

# The overlap assumption requires a positive probability of treatment for each 𝑋𝑖
# . We should not be able to deterministically decide the treatment status of an individual based on its covariates, meaning none of the estimated propensity scores should be close to one or zero. One can check this with a histogram:
hist(e.hat <- tau.forest$W.hat)

W = g_W
# One can also check that the covariates are balanced across the treated and control group by plotting the inverse-propensity weighted histograms of all samples, overlaid here for each feature (done with ggplot2 which supports weighted histograms):
IPW <- ifelse(W == 1, 1 / e.hat, 1 / (1 - e.hat))

min(IPW)

#Make long

df <- cbind.data.frame(g_W, g_X, IPW)
df
head(df)
table(df$g_W)

# Load the necessary library
library(tidyr)

# Reshape the dataframe
df_long <- df|>
  pivot_longer(cols = starts_with("t0_"),
               names_to = "variable",
               values_to = "value") |>
  mutate(W = factor(g_W))

df_long$value

ggplot(df_long, aes(x = value, weight = IPW, fill = W)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 30) +
  facet_wrap( ~ variable, ncol = 2)


ggplot(df,
       aes(
         x = t0_ritual_1aweek,
         weight = IPW,
         fill = as.factor(g_W)
       )) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 30)






# try a quini curve ------------------------------------------------------

# training sample
train_forest <- causal_forest(g_X[train, ], g_Y[train], g_W[train], sample.weights = g_weights[train])

# eavaluation sample
eval_forest <- causal_forest(g_X[-train, ], g_Y[-train], g_W[-train], sample.weights = g_weights[-train])

# predict CATEs on test set.
test <- -train
tau_hat <- predict(eval_forest, g_X[test, ])$predictions

g_WW = as.factor(g_W)
# 3) Form a multi-armed Qini curve based on IPW (convenience function part of `maq`).
ipw_scores <- get_ipw_scores(g_Y[test], g_WW[test])

# 
cost <- 1

# A Qini curve for a multi-armed policy.
ma_qini <- maq(tau_hat, cost, ipw_scores, R = 200)

# A "baseline" Qini curve that ignores covariates.
ma_qini_baseline <- maq(tau_hat, cost, ipw_scores, target.with.covariates = FALSE, R = 200)

# plot
plot(ma_qini)
plot(ma_qini_baseline, add = TRUE, lty = 2, ci.args = NULL)


difference_gain(ma_qini, ma_qini_baseline, spend = 0.5)
difference_gain(ma_qini, ma_qini_baseline, spend = .1)

integrated_difference(ma_qini, ma_qini_baseline, spend = 0.5)


# 
dev.off()
