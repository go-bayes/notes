# radim take 2 7 may 2024


# t0......baseline in 2003
# t1......exposure in 2005

# "at_least_1_kid_in_2007"............short-term (biological offspring)
# "at_least_2_kids_in_2013"...........long-term (biological offspring)
# "intended_at_least_2_in_2008".......intended fertility reported in wave 3 (2008) 
# "t0_ritual_any".....................1 if church attendance = few times a year or more (options 2-7 on scale 1-7)
# "t1_ritual_any".....................1 if church attendance = few times a year or more (options 2-7 on scale 1-7)            
# "t0_ritual_1aweek"..................1 if church attendance = once a year or more (options 6,7 on scale 1-7)
# "t1_ritual_1aweek"..................1 if church attendance = once a a year or more (options 6,7 on scale 1-7)
# "t0_faith_any"......................1 if faith importance = somewhat important or more (options 3,4,5 on scale 1-5)
# "t1_faith_any"......................1 if faith importance = somewhat important or more (options 3,4,5 on scale 1-5)
# "t0_faith_full".....................1 if faith importance = extremely important (option 5 on scale 1-5)
# "t1_faith_full".....................1 if faith importance = extremely important (option 5 on scale 1-5)
# "t0_age"............................age
# "t0_parent_income"..................household income reported by primary caregiver (in 90% parent) on the scale 1-11
# "t0_gender_binary"..................gender
# "t0_religion_cat"...................religion
# "t0_parent_education"...............education of the primary caregiver on the scale 1,2,3
# "t0_race_binary"....................1 if white
# "at_least_2_kid_in_2003"............1 if 2+ children in 2003 (this is for survival analysis)                 
# "at_least_2_kid_in_2004"............1 if 2+ children in 2004 (this is for survival analysis) 
# "at_least_2_kid_in_2005"............1 if 2+ children in 2005 (this is for survival analysis) 
# "at_least_2_kid_in_2006"............1 if 2+ children in 2006 (this is for survival analysis) 
# "at_least_2_kid_in_2007"............1 if 2+ children in 2007 (this is for survival analysis) 
# "at_least_2_kid_in_2008"............1 if 2+ children in 2008 (this is for survival analysis) 
# "at_least_2_kid_in_2009"............1 if 2+ children in 2009 (this is for survival analysis) 
# "at_least_2_kid_in_2010"............1 if 2+ children in 2010 (this is for survival analysis) 
# "at_least_2_kid_in_2011"............1 if 2+ children in 2011 (this is for survival analysis) 
# "at_least_2_kid_in_2012"............1 if 2+ children in 2012 (this is for survival analysis) 
# "at_least_2_kid_in_2013"............1 if 2+ children in 2013 (this is for survival analysis) 
# "wave1".............................1 if the respondent was interviewed in wave 1               
# "wave2".............................1 if the respondent was interviewed in wave 2                      
# "wave3".............................1 if the respondent was interviewed in wave 3
# "wave4".............................1 if the respondent was interviewed in wave 4
# "all_waves".........................1 if the respondent was included in all four waves                  
# "waves_1_2_4".......................1 if the respondent was included in waves 1, 2, and 4 (these are the waves we use to analyses except 
#                                     analysis on intended fertility)
# "kids_2005".........................number of children in 2005 (wave 2). Use this to filter sample for analyses becasue we do not want include
#                                     respondents who already had children during the religious shift period

# Define labels for all tables 
# labels <- c(
#   "(Intercept)"              = "Intercept",
#   
#   # Predictors
#   "t0_ritual_any"               = "2003: church attendance [few times a year and more]",
#   "t1_ritual_any"               = "2005: church attendance [few times a year and more]",
#   "t0_ritual_1aweek"            = "2003: church attendance [once a week and more]",
#   "t1_ritual_1aweek"            = "2005: church attendance [once a week and more]",
#   "t1_ritual_1aweek_f1"         = "2005: church attendance [once a week and more]",
#   "t0_faith_any"                = "2003: faith importance [somewhat important and more]",
#   "t1_faith_any"                = "2005: faith importance [somewhat important and more]",
#   "t0_faith_full"               = "2003: faith importance [extremely important]",  
#   "t1_faith_full"               = "2005: faith importance [extremely important]",
#   "t1_faith_full_f1"            = "2005: faith importance [extremely important]",
#   "intended_at_least_2_in_2008" = "2008: intended fertility",
#   
#   # Covariates
#   "t0_age"                   = "2003: age",
#   "t0_parent_income"         = "2003: household income",          
#   "t0_gender_binary"         = "2003: gender [female]",       
#   "t0_gender_binary_fWoman"  = "2003: gender [female]", 
#   "t0_race_binary"           = "2003: ethnicity [white]",
#   "t0_parent_education"      = "2003: caregiver's education",
#   
#   # Interactions
#   "t1_ritual_1aweek_f1:t0_gender_binary_fWoman"	        = "2005: church attendance X 2003: gender",
#   "t1_faith_full_f1:t0_gender_binary_fWoman"            = "2005: faith importance X 2003: gender"
# )  



# link to data
pull_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-radim-usa/data_in_shape.csv"
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
  #ranger,
  #xgboost,
  #glmnet,
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
  stringr
)

# import data
dat_init <- read.csv(pull_mods)

# check
head(dat_init)


# create weights for male/not male in t0
# note, please use either "male" or "female" or "not_male"  so that we know what the indicator means

table(dat_init$t0_gender_binary) # 

# check this and reverse the code "male" if you mean female
dat_init <- dat_init |> rename(t0_male = t0_gender_binary)


# sample weights balanced male / female-------------------------------------
# balance on gender weights

# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <-
  0.5  # target proportion of males in the population
prop_female_population <-
  0.5  # target proportion of females in the population

# math
prop_male_sample <- mean(dat_init$t0_male)
prop_female_sample <- 1 - prop_male_sample

# math
gender_weight_male <- prop_male_population / prop_male_sample
gender_weight_female <- prop_female_population / prop_female_sample

# create
dat_init$t0_sample_weights <-
  ifelse(dat_init$t0_male == 1, gender_weight_male, gender_weight_female)

# we will upweight males and down weight non-males to obtain a balance of gender in the *target* population


# NOTE THERE IS EXTREME GENDER BALANCE AT t0 -- check this???
table(round(dat_init$t0_sample_weights, 3))


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

# rename X as id
dat_init <- dat_init |>  dplyr::rename(id = X)

# count those in all waves
table(dat_init$all_waves) # those in all waves = 1681


# inclusion criteria ------------------------------------------------------
# inclusion criteria : no kids in 2005
table(dat_init$kids_2005)



# problem with missing vals? ----------------------------------------------
# remove irrelevant variables 


dat_inclusion <- dat_init |>  
  dplyr::filter(kids_2005 == 0) |>    ## NOTE SOMETHING GOES WRONG HERE, ALL FUTURE VALUES ARE DROPPED
  
  # check with radim
  select(-c(starts_with("wave")| ends_with("wave")| ends_with("waves"))) |> 
  dplyr::filter(!is.na(t1_ritual_any)) |> 
  dplyr::filter(!is.na(t0_ritual_any)) |> 
  dplyr::select(-at_least_2_kid_in_2005, -at_least_2_kid_in_2003,-at_least_2_kid_in_2004) |> 
  droplevels()

# names
colnames(dat_inclusion)

#. No missing vals :)
naniar::vis_miss(dat_inclusion)


#  data is a mess, let's get it into shape :) -----------------------------

# update the prefix_years function to correctly handle existing prefixed columns
prefix_years <- function(name) {
  sapply(name, function(x) {
    if (grepl("^t[0-9]_", x)) {
      x  # skip already correctly prefixed columns
    } else if (grepl("_20[0-1][0-9]$", x)) {
      year <- as.numeric(sub(".*_(20[0-1][0-9])$", "\\1", x))
      year_index <- year - 2005
      sub("(.*)_(20[0-1][0-9])$", paste0("t", year_index, "_\\1"), x)
    } else {
      x
    }
  })
}

# apply function to column names
new_col_names <- prefix_years(colnames(dat_inclusion))

# rename dataframe columns
colnames(dat_inclusion) <- new_col_names

# remove year suffixes from the new names, careful to avoid affecting existing tX_ prefixes
colnames(dat_inclusion) <- gsub("(.+?)_(20[0-1][0-9])$", "\\1", colnames(dat_inclusion))

# arrange
dat_inclusion <- dat_inclusion|>
  select(id, sort(setdiff(names(dat_inclusion), "id")))

# View the restructured dataframe
print(head(dat_inclusion))


# check na in exposure at baseline
table( is.na(dat_inclusion$t0_ritual_any)) 


# check na in exposure wave
# consider how to handle this?
table( is.na(dat_inclusion$t1_ritual_any)) 


# n in study
nrow(dat_inclusion)

# lets make inclusion criteria no NA in the exposure (we can come back to this)
# remove nas
dt_exposure_all <- dat_inclusion |> 
  filter(!is.na(t0_ritual_1aweek) & !is.na(t1_ritual_1aweek) ) |> 
  # not biologically plausible
  select(-t1_at_least_2_kid_in)

# n participants
nrow(dt_exposure_all)
colnames(dt_exposure_all)

# this doesn't make sense??
naniar::vis_miss(dt_exposure_all)

# how much did regular church attendance change between t0 and t1? 


# checks ------------------------------------------------------------------

dt_baseline_exposure <- dt_exposure_all |> 
  select(id, t0_ritual_1aweek, t1_ritual_1aweek)

# check
head(dt_baseline_exposure)


# make data long
dt_long <- pivot_longer(
  dt_baseline_exposure,
  cols = starts_with("t"),
  names_to = c("wave", ".value"),
  names_pattern = "(t[0-9]+)_(.*)"
)



# transition table - useful for the publication ---------------------------

# THIS SHOWS HOW MANY PEOPLE CHANGED IN REGULAR ATTENDANCE BETWEEN WAVE 0 and WAVE 1

# make wave numeric
dt_long$wave <- as.numeric(sub("t", "", dt_long$wave))

# make table
out <- margot::create_transition_matrix(dt_long, id_var = "id", state_var = "ritual_1aweek")

transition_table <- margot::transition_table(out)

# this shows loss and gain
transition_table$table
transition_table$explanation


# look at time 3 ----------------------------------------------------------
# select vars
dt_exposure_prep_t3 <- dt_exposure_all |>
  select(starts_with("t0_"), starts_with("t1_"), starts_with("t3_at_least_2"))
# obtain censoring indicator
naniar::vis_miss(dt_exposure_prep_t3)


# view data
skimr::skim(dt_exposure_prep_t3)

#
table( dt_exposure_prep_t3$t0_parent_education )
table( dt_exposure_prep_t3$t0_ritual_1aweek )
table( dt_exposure_prep_t3$t3_at_least_2_kid_in ) # only 11 cases!



# try t8_
dt_exposure_prep_t3 <- dt_exposure_all |>
  select(c(starts_with("t0_"), starts_with("t1_"), "t8_at_least_2_kids_in"))

colnames(dt_exposure_prep_t3)

# check
table( dt_exposure_prep_t3$t8_at_least_2_kids_in ) # 124 -- this is better, and the outcome is rare, so we can obtain Risk Ratios from logistic regression, if needed


# more appropriate name for data frame
dt_t8_prep<- dt_exposure_prep_t3

# view
naniar::vis_miss(dt_t8_prep)

# let's get rid of the faith_full indicator
dt_t8_prep_2 <- dt_t8_prep |> 
  # missing data in faith indicators for t2
  select(-t0_faith_any, -t0_faith_full, -t1_ritual_any, t1_faith_any, t1_faith_full) # use continuous indiators in t0 of faith


# view not good, why no attrition in t8?
naniar::vis_miss(dt_t8_prep_2)

# make numeric for model 
str(dt_t8_prep_2$t0_religion_cat)

# convert to factor
dt_t8_prep_2$t0_religion_cat <- as.factor(dt_t8_prep_2$t0_religion_cat)


table(dt_t8_prep_2$t0_religion_cat)
# mapping to view
# View mapping of factor levels to numeric codes
factor_levels <- levels(dt_t8_prep_2$t0_religion_cat)
factor_codes <- seq_along(factor_levels)
names(factor_codes) <- factor_levels
factor_codes

# make numeric
dt_t8_prep_2$t0_religion_cat_numeric <- as.integer(dt_t8_prep_2$t0_religion_cat)


# what is income? categorical? 
table( dt_t8_prep_2$t0_parent_income)


# make censoring variable for whether someone lost in t8
# note there SHOULD BE PEOPLE WHO LEAVE THE STUDY so we need include missing value indicators
t1_na_condition <- rowSums(is.na(select(dt_t8_prep_2, starts_with("t8_")))) > 0


# data wrangling
dt_t8_prep_3 <- dt_t8_prep_2 |>
  select(-t0_religion_cat, -t0_kids) |> # not relevant
  # standardise age and include
  mutate(t0_age_z = scale(t0_age)) |>
  # remove age
  select(-t0_age) |>
  mutate(t1_not_lost = ifelse(t1_na_condition == 1, 0, 1)) |> # use if censored at t1
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate(starts_with("t1_"), .before = starts_with("t8_"))
# standardise age and include

#remove attributes
dt_t8_prep_3<- margot::remove_numeric_attributes(dt_t8_prep_3)

# one hot encoding
str(dt_t8_prep_3)

# one hot encoding
dt_t8_prep_3$t0_parent_education <- as.factor(dt_t8_prep_3$t0_parent_education)
dt_t8_prep_3$t0_religion_cat <- as.factor(dt_t8_prep_3$t0_religion_cat)


# creating dummy variables
parent_education_dummies <- model.matrix(~ t0_parent_education - 1, data = dt_t8_prep_3)
religion_cat_dummies <- model.matrix(~ t0_religion_cat - 1, data = dt_t8_prep_3)

# convert matrices to data frames for easier handling
parent_education_dummies <- as.data.frame(parent_education_dummies)
religion_cat_dummies <- as.data.frame(religion_cat_dummies)


# combine with original data
dt_t8_prep_4 <- cbind(dt_t8_prep_3, parent_education_dummies, religion_cat_dummies)


# remove irrelevant cols
dt_hot_coded <- dt_t8_prep_4 |> 
  select(-t0_religion_cat, t0_parent_education, -t1_faith_any, -t1_faith_full, -t0_religion_cat, -t0_religion_cat_numeric,
         -t0_parent_education) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t1_not_lost", .before = starts_with("t8_")) 

# remove the original categorical columns
colnames(dt_hot_coded)

# check again
naniar::vis_miss(dt_hot_coded, warn_large_data = FALSE)



## consider imbalance on the exposure
baseline_vars_models = dt_hot_coded |>
  dplyr::select(starts_with("t0"), -t0_sample_weights) |> colnames()

# check
baseline_vars_models

skimr::skim( dt_hot_coded)
summary(dt_hot_coded)
# set exposure
X = "t1_ritual_1aweek"
  

# propensity score model
match_ebal_ate <- margot::match_mi_general(data = dt_hot_coded, 
                                           X = X, 
                                           baseline_vars = baseline_vars_models, 
                                           estimand = "ATE",  
                                           #   focal = "tile_3", #for ATT
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


# doubly robust marginal model --------------------------------------------

# set data frame; output of match_mi_general model
dt_hot_coded$t0_combo_weights <- match_ebal_ate$weights

# we do not need this var
dt_hot_coded <- dt_hot_coded |> 
  select(-t0_sample_weights)

# double check
colnames(dt_hot_coded)


# set treatment level
treat_0 = 0 # lowest quartile
treat_1 = 1 # third quartile

# bootstrap simulations ( generally use 1000)
nsims <- 1000

# cores
cl =  parallel::detectCores () 

estimand = "ATE"

# as specified
vcov = "HC3" # robust standard errors. 

# cores
cores = parallel::detectCores () # use all course

# Example call to the function

dt_hot_coded$t8_at_least_2_kids_in


# propensity score only model

## WON'T CONVERGE RANK DIFFICIENT 

propensity_mod_fit_t8_kids <-margot::double_robust_marginal(
  df = dt_hot_coded,
  Y = "t8_at_least_2_kids_in",
  X = X, 
  baseline_vars = baseline_vars_models, # we are not regressing with any covariates
  treat_1 = treat_1,
  treat_0 = treat_0,
  nsims = 1000,
  cores = cores,
  family = "quasibinomial",
  weights = "t0_combo_weights",
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type_causal = "RR",  
  type_tab = "RR",    
  vcov = vcov,         
  new_name = "At Least 2 x Children Time 8",
  delta = 1,
  sd = 1
)

# find the problem cases
model <- lm(t8_at_least_2_kids_in ~ ., data = dt_hot_coded)

# religion cat 9 and parent education are not valid - go back and remove and redo prop score
model

# fix singularities
dt_hot_coded_prep <- dt_hot_coded |> 
  select(-t1_not_lost, -starts_with("t0_parent_education"),  -starts_with("t0_religion_cat"))

colnames(dt_hot_coded_prep)

# find the problem cases - check again
model <- lm(t8_at_least_2_kids_in ~ ., data = dt_hot_coded_prep)

# works
summary(model)


#new var names
new_var_names <- dt_hot_coded_prep |> 
  select(starts_with("t0_"), -t0_combo_weights) |> colnames()

# check
dt_hot_coded_prep$t8_at_least_2_kids_in



# key results -------------------------------------------------------------


##### MARGINAL EFFECT ESTIMATE ON RISK RATIO SCALE #########

mod_fit_t8_kids <- margot::causal_contrast_marginal(
  df = dt_hot_coded_prep,
  Y = "t8_at_least_2_kids_in",
  X = X, 
  baseline_vars = new_var_names, # we are not regressing with any covariates
  treat_1 = 1,
  treat_0 = 0,
  nsims = 1000,
  cores = cores,
  family = "quasibinomial",
  weights = "t0_combo_weights",
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type = "RR",  
  vcov = vcov
)


# GET RESULT IN FORM OF TABLE
output <- margot::tab_engine_marginal(mod_fit_t8_kids, new_name = "Regular Religious Service (parametric)",  type = "RR")

output

# interpretation 
margot::margot_interpret_table(output, causal_scale = "risk_ratio", estimand = "ATE")



# using another function I wrote ------------------------------------------




### ANOTHER FUNCTION 

mod_2 <- margot::double_robust_marginal(
  df = dt_hot_coded_prep,
  Y = "t8_at_least_2_kids_in",
  X = X,
  baseline_vars = new_var_names, # we are not regressing with any covariates
  treat_1 = 1,
  treat_0 = 0,
  nsims = 1000,
  cores = cores,
  family = "quasibinomial",
  weights = "t0_combo_weights",
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type_causal = "RR",  
  type_tab = "RR",    
  vcov = vcov,         
  new_name = "At Least 2 x Children Time 8",
  delta = 1,
  sd = 1
)

margot_interpret_table(mod_2$tab_results, causal_scale = "risk_ratio", estimand = "ATE")

margot_plot(mod_2$tab_results, 
            title = "Causal Effect of Regular Religious Service on Fertilty 7 Years Later", 
            subtitle= "Outcome is Binary (Has Two Children Yes/No)", 
            type = "RR",
            x_lim_hi = 5,
            x_lim_lo = -1, 
            x_offset = -1,
            estimate_scale  = 1)



mod_2$tab_results

###################################################################
## END ###
###################################################################

# RADIM ignore this stuff below, we will come back to it --------


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
