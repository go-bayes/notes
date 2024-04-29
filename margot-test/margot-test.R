# test margot package
push_mods <- '/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/notes'

push_mods

library(margot)
library(tidyverse)
# eliminate haven labels
df_nz <- as.data.frame(df_nz)
df_nz <- haven::zap_formats(df_nz)
df_nz <- haven::zap_label(df_nz)
df_nz <- haven::zap_widths(df_nz)

# read functions
source("/Users/joseph/GIT/templates/functions/funs.R")

# experimental functions (more functions)
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )
# # set exposure name

name_exposure <-  "perfectionism"
skimr::skim(df_nz) |> arrange(n_missing)
# obtain ids for individuals who participated in 2018 and have no missing baseline exposure
ids_2018 <- df_nz %>%
  dplyr::filter(year_measured == 1, wave == 2018) %>%
 # dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing
#  dplyr::filter(!is.na(eth_cat)) |> # criteria, no missing
  pull(id)


# obtain ids for individuals who participated in 2019
ids_2019 <- df_nz %>%
  dplyr::filter(year_measured == 1, wave == 2019) %>%
  dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing
  pull(id)

# intersect IDs from 2018 and 2019 to ensure participation in both years
ids_2018_2019 <- intersect(ids_2018, ids_2019)
# data wrangling

dat_long_test <- df_nz |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  # 
  dplyr::filter(id %in% ids_2018_2019 &
                  wave %in% c(2018, 2019, 2020))

n_unique(dat_long_test$id)


dat_long <- df_nz |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  # 
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  arrange(id, wave) |>
  select(
    "id",
    "wave",
    "year_measured",
    "age",
    "male",
    "born_nz",
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # are you currently employed? (this includes self-employment or casual work)
    "edu",
    # "gen_cohort",
    "household_inc",
    "partner",
    # 0 = no, 1 = yes
    "parent",
    "alert_level_combined_lead", # see bibliography
    # 0 = no, 1 = yes
    "political_conservative", # see nzavs sheet
    "hours_exercise", # see nzavs sheet
    "agreeableness", 
    # Mini-IPIP6 Agreeableness (also modelled as empathy facet)
    # Sympathize with others' feelings.
    # Am not interested in other people's problems.
    # Feel others' emotions.
    # Am not really interested in others.
    "conscientiousness",
    # see mini ipip6
    # Get chores done right away.
    # Like order.
    # Make a mess of things.
    # Often forget to put things back in their proper place.
    "extraversion",
    # Mini-IPIP6 Extraversion
    # Am the life of the party.
    # Don't talk a lot.
    # Keep in the background.
    # Talk to a lot of different people at parties.
    "honesty_humility",
    # see mini ipip6
    # Would like to be seen driving around in a very expensive car.
    # Would get a lot of pleasure from owning expensive luxury goods.
    # Feel entitled to more of everything.
    # Deserve more things in life.
    "openness",
    # see mini ipip6
    # Have a vivid imagination.
    # Have difficulty understanding abstract ideas.
    # Do not have a good imagination.
    # Am not interested in abstract ideas.
    "neuroticism",
    # see mini ipip6
    # Have frequent mood swings.
    # Am relaxed most of the time.
    # Get upset easily.
    # Seldom feel blue.
    "modesty",
    # # see mini ipip6
    # # I want people to know that I am an important person of high status,
    # # I am an ordinary person who is no better than others.
    # # I wouldn’t want people to treat me as though I were superior to them.
    # # I think that I am entitled to more respect than the average person is
    #"w_gend_age_ethnic",
    "neighbourhood_community",
    # #I feel a sense of community with others in my local neighbourhood.
    "belong", # see nzavs sheet
    "rural_gch_2018_l",# see nzavs sheet
    "support",
    # "support_help",
    # # 'There are people I can depend on to help me if I really need it.
    # "support_turnto",
    # # There is no one I can turn to for guidance in times of stress.
    # "support_rnoguidance",
    #There is no one I can turn to for guidance in times of stress.
    "perfectionism",
    "religion_religious",
    "kessler_latent_depression",
    "kessler_latent_anxiety"
  ) |>
  mutate(
    #initialize 'censored'
    censored = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    censored =  ifelse(is.na(censored) &
                         year_measured == 1, 1, censored),
    # create urban binary variable
    
    urban = ifelse(rural_gch_2018_l == 1, 1, 0)
    
  ) |>
  select(-c(year_measured, rural_gch_2018_l) )|>
  dplyr::mutate(
    # rescale these variables, to get all variables on a similar scale
    # otherwise your models can blow up, or become uninterpretable. 
    household_inc_log = log(household_inc + 1),
    hours_exercise_log = log(hours_exercise + 1)  ) |>
  dplyr::select(
    -c(
      household_inc,
      hours_exercise)
  ) |>
  droplevels() |>
  # dplyr::rename(sample_weights = w_gend_age_ethnic,
  #               sample_origin =  sample_origin_names_combined) |>
  arrange(id, wave) |>
  mutate(
    urban = as.numeric(as.character(urban)),
    #   parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    censored = as.numeric(as.character(censored)),
    employed = as.numeric(as.character(employed))
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

skimr::skim(dat_long)

# balance on gender weights

# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <- 0.5  # target proportion of males in the population
prop_female_population <- 0.5  # target proportion of females in the population

prop_male_sample <- mean(dat_long$male)
prop_female_sample <- 1 - prop_male_sample

gender_weight_male <- prop_male_population / prop_male_sample
gender_weight_female <- prop_female_population / prop_female_sample

dat_long$sample_weights <- ifelse(dat_long$male == 1, gender_weight_male, gender_weight_female)

hist(dat_long$sample_weights)



# create ordered varaible based on quartiles at time 11 (exposure wave)
# note these values may change from year to year because "quartile" is relative. So we pick the exposure year to define our units
# 
# dat_long_t <- create_ordered_variable_custom(dat_long, "lifemeaning", c(1, 5, 5.5, 6.5,  7), lifemeaning_labels)
# 
# table( dat_long_t$lifemeaning_coarsen )




baseline_vars = c("age", "male", "edu", "eth_cat", "partner", "employed", "born_nz", "neighbourhood_community", "household_inc_log", "parent", "religion_religious", "urban", "employed", "alert_level_combined_lead", "sample_weights")

# treatment
exposure_vars = c("perfectionism") 

# outcome, can be many
outcome_vars = c("kessler_latent_anxiety", "kessler_latent_depression")


# make long data wide no imputations
prep_dat_wide <- margot_wide(dat_long, 
                             baseline_vars = baseline_vars, 
                             exposure_var = exposure_vars,
                             outcome_vars = outcome_vars)


# grf ---------------------------------------------------------------------


# GRF MODELS --------------------------------------------------------------
# see: https://grf-labs.github.io/grf/
#devtools::install_github("grf-labs/grf", subdir = "r-package/grf")
library(grf)


# data wrangle: create binary indicator

# table(df_clean$t0_religion_church_round_z)
# df_use_full_f <- df_clean |> 
#   mutate(t1_reg_church_attends = ifelse( t1_religion_church_round >= 4, 1, 0)) |> 
#   # mutate(t0_reg_church_attends = ifelse( t0_religion_church_round_z <= 4, 1, 0)) |> 
#   mutate(t0_has_siblings  = ifelse(t0_total_siblings_factor == 0, 0, 1)) |> 
#   select( -t0_total_siblings_factor)


exposure_vars <- c("perfectionism", "censored")
baseline_vars<-setdiff(baseline_vars, "sample_weights")
df_impute_base<- margot::margot_wide_impute_baseline(dat_long, baseline_vars = baseline_vars, 
                                             exposure_var = exposure_vars, outcome_vars = outcome_vars)

here_save(df_impute_base, "df_impute_base")
dt_18 <- dat_long |> filter(wave == 2018)

# add sample weights
df_impute_base$t0_sample_weights = dt_18$sample_weights

# save
#here_save(df_impute_base,"df_impute_base")
# train causal forest

# sample weights
t0_sample_weights <- df_no_impute_base$t0_sample_weights



# correct
df_impute_base[, c("t0_male", "t0_sample_weights")]

t0_sample_weights
# get key data features 
baseline_vars



colnames( df_impute_base )
nrow( df_impute_base )

# get correct censoring 
t0_na_condition <-
  rowSums(is.na(select(df_impute_base, starts_with("t1_")))) > 0
# t1_na_condition <-
#   rowSums(is.na(select(df_impute_base, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights


df_clean <- df_impute_base %>%
  select(-t0_alert_level_combined_lead) |> 
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
 # mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
 # mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        !t0_sample_weights & 
      #  !t0_alert_level_combined_lead &
        !t1_censored,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_sample_weights,
   # t0_alert_level_combined_lead,
    t0_sample_weights,
    t0_censored,
    t1_censored,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t0_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))



# checks - Lost means Lost in the next wave
table(df_clean$t1_lost)
table(df_clean$t0_lost)

table(df_clean$t0_censored)

test <- df_impute_base |> filter(t0_censored == 1)
nrow(test)
# 
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)
str( df_clean )

nrow(df_clean)


naniar::vis_miss(df_clean, warn_large_data = FALSE)

# weights for treament ----------------------------------------------------

baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"),-t0_censored, -t0_lost, -t0_sample_weights)|> colnames() # note, we ear

baseline_vars_models
# fit proponsity score model 

library(MatchIt)
library(WeightIt)


#test
match_mi_general_dev <- function(data,
                             X,
                             baseline_vars,
                             estimand,
                             method,
                             focal = NULL,
                             sample_weights = NULL,
                             stabilize = FALSE,
                             include.obj = FALSE,
                             keep.mparts = TRUE,
                             ...) {
  
  # Check input data type
  data_class <- class(data)
  if (!data_class %in% c("mids", "data.frame")) {
    stop("Input data must be either 'mids' or 'data.frame' object")
  }
  
  # Construct the formula
  formula_str <- as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))
  
  # Choose appropriate function based on data type
  weight_function <- if (data_class == "mids") MatchThem::weightthem else WeightIt::weightit
  
  # Function to perform matching
  perform_matching <- function(data) {
    if (is.null(sample_weights)) {
      weight_function(
        formula = formula_str,
        data = data,
        estimand = estimand,
        stabilize = stabilize,
        method = method,
        focal = focal,
        include.obj = include.obj,
        keep.mparts = keep.mparts,
        ...
      )
    } else {
      weight_function(
        formula = formula_str,
        data = data,
        estimand = estimand,
        stabilize = stabilize,
        method = method,
        sample_weights = sample_weights,
        focal = focal,
        include.obj = include.obj,
        keep.mparts = keep.mparts,
        ...
      )
    }
  }
  
  # Perform matching on the entire dataset
  dt_match <- perform_matching(data)
  
  return(dt_match)
}



match_censoring <- match_mi_general_dev(data = df_clean, 
                                            X = "t0_lost", 
                                            baseline_vars = baseline_vars_models, 
                                            estimand = "ATE",  
                                            superlearner = TRUE,
                                            method = "ebal",
                                            # focal = "< >", for ATT
                                            method = "super",
                                           #SL.library = c("SL.ranger"),
                                            SL.library = c("SL.glmnet", "SL.ranger",
                                                       "SL.xgboost","SL.polymars"),
                                            sample_weights = "sample_weights")

# save output
here_save( match_censoring, "match_censoring") 

bal.tab(match_censoring)
love.plot(match_censoring)


min( match_censoring$weights )
max( match_censoring$weights )
# new weights
df_clean$t0_combo_weights = match_censoring$weights * df_clean$t0_sample_weights

min( df_clean$t0_combo_weights)


df_clean_t1 <- df_clean |> filter(t0_lost == 0)

#
nrow(df_clean_t1)

table(is.na(df_clean_t1$t1_perfectionism_z))

# gets us the correct df for weights



# next get data for t1

# get correct censoring 

# redundant but OK
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

# use
t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights


df_clean_t2 <- df_clean_t1 %>%
 # select(-t0_alert_level_combined_lead) |> 
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
 # mutate(t0_lost = 1 - t0_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_")) |> 
  select(-t1_lost, -t0_lost)


# test 
nrow(df_clean_t2)

# checks 
hist(df_clean_t2$t0_combo_weights)

# outcomes
naniar::vis_miss(df_clean_t2, warn_large_data = F)







#############

# reduce covariates
df_use_full <-
  df_clean |> select(
    starts_with("t1"),
    starts_with("t2"),
    starts_with("t0"),
    -starts_with("t0_warm"),
  )



colnames( df_use_full )
nrow( df_use_full )


# data wrangle: create binary indicator

table(df_clean$t0_religion_church_round_z)


# train causal forest

# sample weights
t0_sample_weights <- df_use_full$t0_sample_weights

# get censoring indicator
D <- df_use_full$t1_lost

# Causal forest weights
# grf_names_base <- df_use_full |> select( starts_with("t0"), -starts_with(c("t1","t2")),  -t0_sample_weights)|> colnames()

grf_names_base <- df_use_full |> select( starts_with("t0")), -starts_with(c("t1", "t2")),  -t0_sample_weights)|> colnames()

nrow(df_use_full)

# convert NAs to a factor level
df_use_full$t0_eth_cat <- addNA(df_use_full$t0_eth_cat, ifany = TRUE)


# convert factors to dummy variables for specific columns
dummy_vars <- model.matrix(~ t0_eth_cat - 1, data = df_use_full)

nrow(dummy_vars)
# Combine the new dummy variables with the original dataframe
data_new <- cbind(df_use_full, dummy_vars) |> select(-t0_eth_cat)

baseline_predictors <- data_new |> colnames()

baseline_predictors






# GRF MODELS --------------------------------------------------------------
# see: https://grf-labs.github.io/grf/
#devtools::install_github("grf-labs/grf", subdir = "r-package/grf")
library(grf)


# data wrangle: create binary indicator

table(df_clean$t0_religion_church_round_z)
df_use_full_f <- df_clean |> 
  mutate(t1_reg_church_attends = ifelse( t1_religion_church_round >= 4, 1, 0)) |> 
  # mutate(t0_reg_church_attends = ifelse( t0_religion_church_round_z <= 4, 1, 0)) |> 
  mutate(t0_has_siblings  = ifelse(t0_total_siblings_factor == 0, 0, 1)) |> 
  select( -t0_total_siblings_factor)




# train causal forest

# sample weights
t0_sample_weights <- df_use_full_f$t0_sample_weights

# get censoring indicator
D <- df_use_full_f$t1_not_lost

# get key data features 

# reduce covariates
df_use_full <-
  df_use_full_f |> select(
    starts_with("t1"),
    starts_with("t2"),
    starts_with("t0"),
    -starts_with("t0_warm"),-t0_hlth_disability_z,
    -t0_hours_family_sqrt_round,
    -t0_hours_friends_sqrt_round,-t0_sfhealth_z,
    -t0_has_siblings,
    -t0_hlth_bmi_z,
    -t0_born_nz_z,
    -t0_friends_time_z,-t0_family_time_z,
    -t0_sample_origin,
    -t0_vengeful_rumin_z,
    -t0_religion_perceive_religious_discrim_z,
    -t0_gratitude_z,
    -t0_support_z
  )



colnames( df_use_full )
nrow( df_use_full )


# Causal forest weights
grf_names_base <- df_use_full |> select( starts_with("t0"), -starts_with(c("t1","t2")), - t0_sample_weights,-t0_not_lost )|> colnames()

selected_W = matrix( df_use_full$t1_reg_church_attends )
selected_Y = matrix( df_use_full$t2_charity_donate )

selected_X <- df_use_full %>% select(all_of(grf_names_base)) |> 
  mutate(across(everything(), ~ {
    x <- .
    attributes(x) <- NULL
    x
  }))


# Y var is censoring time
Y = D + 1
D
sf_censor <- survival_forest(cbind(selected_X, selected_W), Y = Y, D = 1-D, prediction.type="Nelson-Aalen")
summary(sf_censor)

# K <- 1/predict(sf_censor, failure.times=pmin(Y,D), prediction.times="time")$predictions
# 
# hist(K)
here_save(sf_censor, "sf_censor")
censoring_prob <- sf_censor$predictions
hist(censoring_prob)

observed_events <- (D == 1)

# compute sample weights # not  quite right?
grf_sample_weights <- 1 / censoring_prob[observed_events]


grf_sample_weights
# inspect
length(grf_sample_weights)
hist(grf_sample_weights)


# try another method
# library(WeightIt)
# library(cobalt)
# 
# 
# 
# grf_names_base
# # propensity score matching using ebalance -- generally very good
# 
# 
# grf_names_base_2 <- c(grf_names_base, 't1_reg_church_attends')
# 
# grf_names_base_2
# 
# cen_ebal <- match_mi_general(data = df_use_full, 
#                                       X = "t1_not_lost", 
#                                       baseline_vars = grf_names_base_2, 
#                                       estimand = "ATE",  
#                                       # focal = "< >", for ATT
#                                       method = "ebal", 
#                                       sample_weights = "sample_weights")
# 
# summary(cen_ebal)
# bal.tab(cen_ebal, un = TRUE)
# love.plot(cen_ebal, binary = "std", thresholds = c(m = .1),
#           wrap = 50, position = "bottom", size =2) 
# 
# # get weights
# df_use_full$w_weights <- cen_ebal$weights
# df_use_full

df_use_full <- df_use_full |> filter(t1_not_lost == 1) |> 
  mutate(weights = grf_sample_weights * t0_sample_weights) 



g_weights = matrix( df_use_full$weights)
hist(g_weights)

#  One-hot encoding to make cat vars continuous
# Load necessary library
library(dplyr)

# rename dataframe
df <- df_use_full
colnames(df)
# identify categorical variables (excluding 'id')
categorical_vars <- sapply(df, is.factor) 
#categorical_vars["id"] <- FALSE

# Apply one-hot encoding to categorical variables and combine results
df_encoded <- df %>% 
  select(which(!categorical_vars)) %>% 
  bind_cols(
    lapply(names(df)[categorical_vars], function(col) {
      model.matrix(~ . - 1, data = df[col])
    })
  )


head(df_encoded)

# Reorder the columns in df_encoded
df_encoded <- df_encoded %>%
  select(t0_religion_church_round_z, t0_hours_charity_z,t0_charity_donate_z, everything())

head(df_encoded)



g_W  = matrix( df_encoded$t1_reg_church_attends )
t2_charity_donate = matrix( df_encoded$t2_charity_donate )
t2_hours_charity = matrix( df_encoded$t0_hours_charity_z )



use_names_base <- df_encoded |> select( starts_with("t0"), - t0_sample_weights,-t0_not_lost )|> colnames()
use_names_base


g_X <- df_encoded %>% select(all_of(use_names_base)) |> 
  mutate(across(everything(), ~ {
    x <- .
    attributes(x) <- NULL
    x
  }))


# g_XX <- g_X |> 
#   select(t0_religion_church_round_z,t0_charity_donate_z, t0_eth_euro, t0_age_z, t0_partner_z, t0_urban_z, t0_hours_work_log_z, t0_religion_church_round_z, t0_education_level_coarsen_z, t0_household_inc_log_z) 

str(g_W)
str(g_X)
str(g_W)
str(g_Y)
str(g_weights)

# model charity
tau_forest_t2_charity_donate <- grf::causal_forest(X= g_X, Y= t2_charity_donate, W = g_W, sample.weights = g_weights)

# save
here_save(tau_forest_t2_charity_donate, 'tau_forest_t2_charity_donate')


# view
tau_forest_t2_charity_donate

## ATE
average_treatment_effect(tau_forest_t2_charity_donate, target.sample = "all")

633.9836

average_treatment_effect(tau_forest_t2_charity_donate, target.sample = "treated")


tau.hat.oob <- predict(tau_forest_t2_charity_donate)
hist(tau.hat.oob$predictions)

best_linear_projection(tau_forest_t2_charity_donate, g_X)
rate <- rank_average_treatment_effect(tau_forest_t2_charity_donate, g_X[, "t0_religion_church_round_z"])
plot(rate, ylab = "Church", main = "TOC: ranked by decreasing weight")
forest.W <- regression_forest(g_X, g_W, tune.parameters = "all")
W.hat <- predict(forest.W)$predictions

g_Y <- t2_charity_donate
forest.Y <- regression_forest(g_X, g_Y, tune.parameters = "all")
Y.hat <- predict(forest.Y)$predictions


forest.Y.varimp <- variable_importance(tau_forest_t2_charity_donate)
forest.Y.varimp
selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.95)
selected.vars
colnames(g_X)

# Forest in most import vars
tau.forest <- causal_forest(g_X[, selected.vars], g_Y, g_W,
                            W.hat = W.hat, Y.hat = Y.hat,
                            tune.parameters = "all", sample.weights = g_weights)

average_treatment_effect(tau.forest, target.sample = "all")



# get vec for key params

n<-nrow(g_X)
n

train <- sample(1:n, n / 2)
train
train.forest <- causal_forest(g_X[train, ], g_Y[train], g_W[train],  sample.weights = g_weights[train])
eval.forest <- causal_forest(g_X[-train, ], g_Y[-train], g_W[-train], sample.weights = g_weights[-train])
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(train.forest, g_X[-train, ])$predictions)
plot(rate)

average_treatment_effect(train.forest, target.sample = "all")
average_treatment_effect(eval.forest, target.sample = "all")


# tau.hat <- predict(tau_forest, X.test, estimate.variance = TRUE)
# paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))


##
library(policytree)
library(DiagrammeR)

# get ate
ate <- average_treatment_effect(tau_forest_t2_charity_donate)


# quick eval
varimp <- variable_importance(tau_forest_t2_charity_donate)
varimp
ranked.vars <- order(varimp, decreasing = TRUE)
ranked.vars
colnames(g_X)


best_linear_projection(tau_forest_t2_charity_donate, g_X[ranked.vars[1:5]])


# Compute doubly robust scores
dr.scores <- grf::get_scores(tau_forest_t2_charity_donate)
dr.scores
# dr.scores <- double_robust_scores(tau_forest_t2_charity_donate)
# dr.scores
# # Use as the ATE as a "cost" of program treatment to find something non-trivial
# cost <- ate[["estimate"]]
# dr.rewards <- cbind(control=-dr.scores, treat=dr.scores - cost)

# plot overlap
use_X <- g_X[, selected.vars]

tree <- policy_tree(use_X, dr.scores, depth = 2)
tree_full <- policy_tree(g_X, dr.scores, depth = 2)
here_save(tree_full,"tree_full")
print(tree)
plot(tree)

print(tree)
plot(tree_full)

# Predict the treatment assignment {1, 2} for each sample.
predicted <- predict(tree, g_X)
plot(X[, 1], X[, 2], col = predicted)
legend("topright", c("control", "treat"), col = c(1, 2), pch = 19)
abline(0, -1, lty = 2)

node.id <- predict(tree, X, type = "node.id")

values <- aggregate(dr.scores, by = list(leaf.node = node.id),
                    FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))
print(values, digits = 2)


# eval grf fit ------------------------------------------------------------


# eval fit

# The overlap assumption requires a positive probability of treatment for each 𝑋𝑖
# . We should not be able to deterministically decide the treatment status of an individual based on its covariates, meaning none of the estimated propensity scores should be close to one or zero. One can check this with a histogram:
hist(e.hat <- tau.forest$W.hat)

W = g_W
# One can also check that the covariates are balanced across the treated and control group by plotting the inverse-propensity weighted histograms of all samples, overlaid here for each feature (done with ggplot2 which supports weighted histograms):
IPW <- ifelse(W == 1, 1 / e.hat, 1 / (1 - e.hat))

#Make long

df <- cbind(g_W, g_XX,IPW)

head(df)
table(df$g_W)

# Load the necessary library
library(tidyr)

# Reshape the dataframe
df_long <- df %>%
  pivot_longer(
    cols = starts_with("t0_"), 
    names_to = "variable", 
    values_to = "value"
  ) |> 
  mutate(W = factor(g_W))

df_long$value

ggplot(df_long, aes(x = value, weight = IPW, fill = W)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  facet_wrap( ~ variable, ncol = 2)


ggplot(df, aes(x = t0_religion_church_round_z, weight = IPW, fill = as.factor(g_W))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) 

