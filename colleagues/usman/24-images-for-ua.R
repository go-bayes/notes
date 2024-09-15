# Images for talks - Usman
# joseph.bulbulia@gmail.com
# sept 2024

# reproducibility
set.seed(123)

# path
push_mods <- here::here("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/00-talks")


# add measures to database (if any)
# measures_path = here::here("boilerplate", 'data')
# boilerplate::boilerplate_manage_measures(measures_path = measures_path)
# here we added

# load libraries ---------------------------------------------------------
## install and load 'margot' package
# if (!require(margot, quietly = TRUE)) {
#   devtools::install_github("go-bayes/margot")
# }
detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")
# devtools::install_github("go-bayes/boilerplate")
library(margot)
library(boilerplate)

packageVersion(pkg = "margot")
packageVersion(pkg = "boilerplate")

# load necessary libraries
pacman::p_load(
  clarify, # sensitivity analysis for causal inference
  cobalt, # covariate balance tables and plots
  DiagrammeR, # graph and network visualization
  doParallel, # parallel processing with foreach
  fastDummies, # fast creation of dummy variables
  fs, # cross-platform file system operations
  ggbeeswarm, # data visualisation
  ggplot2, # data visualisation
  glmnet, # lasso and elastic-net regularized models
  grf, # generalized random forests
  gt, # html tables for data frames
  gtsummary, # summary tables for regression models
  here, # simple and robust file referencing
  janitor, # data cleaning and validation
  kableExtra, # advanced table formatting
  lmtp, # longitudinal targeted maximum likelihood estimation
  margot, # functions for casual inference
  MatchIt, # matching methods for causal inference
  MatchThem, # matching methods for multiply imputed datasets
  naniar, # handling and visualization of missing data
  parameters, # parameters and performance metrics
  policytree, # causal inference with policy trees
  progressr, # progress reporting for R
  ranger, # fast implementation of random forests
  skimr, # summary statistics for data frames
  SuperLearner, # ensemble learning
  tidyverse, # collection of R packages for data science
  WeightIt, # weighting methods for covariate balancing
  xgboost, # extreme gradient boosting
  EValue, # compute Evalues
  data.table, # fast data wrangling
  maq, # qini curves
  purrr, # data wrangling
  patchwork, # multiple plots
  labelled,
  tidyr,
  crayon,
  boilerplate,
  ggokabeito
)
# pak::pak(c(
#   "clarify",      # sensitivity analysis for causal inference
#   "cobalt",       # covariate balance tables and plots
#   "DiagrammeR",   # graph and network visualization
#   "doParallel",   # parallel processing with foreach
#   "fastDummies",  # fast creation of dummy variables
#   "fs",           # cross-platform file system operations
#   "ggbeeswarm",   # data visualisation
#   "ggplot2",      # data visualisation
#   "glmnet",       # lasso and elastic-net regularized models
#   "grf",          # generalized random forests
#   "gt",           # html tables for data frames
#   "gtsummary",    # summary tables for regression models
#   "here",         # simple and robust file referencing
#   "janitor",      # data cleaning and validation
#   "kableExtra",   # advanced table formatting
#   "lmtp",         # longitudinal targeted maximum likelihood estimation
#   "margot",       # functions for causal inference
#   "MatchIt",      # matching methods for causal inference
#   "MatchThem",    # matching methods for multiply imputed datasets
#   "naniar",       # handling and visualization of missing data
#   "parameters",   # parameters and performance metrics
#   "policytree",   # causal inference with policy trees
#   "progressr",    # progress reporting for R
#   "ranger",       # fast implementation of random forests
#   "skimr",        # summary statistics for data frames
#   "SuperLearner", # ensemble learning
#   "tidyverse",    # collection of R packages for data science
#   "WeightIt",     # weighting methods for covariate balancing
#   "xgboost",      # extreme gradient boosting
#   "EValue",       # compute Evalues
#   "data.table",   # fast data wrangling
#   "maq",          # qini curves
#   "purrr",        # data wrangling
#   "patchwork",    # multiple plots
#   "labelled",
#   "tidyr"
# ))
# set paths for data
pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs")

# review
push_mods

# function ----------------------------------------------------------------

# read and preprocess data ------------------------------------------------
# import data
dat <- qs::qread(here::here(pull_path))

# view column names
sort(colnames(dat))

# env_efficacy_action_feeling
# env_efficacy_action_belief
# env_climate_chg_concern
# env_climate_chg_cause
# env_climate_chg_real
# env_sat_nz_environment
# envefficacy


# get names
warm_columns <- grep("^warm", colnames(dat), value = TRUE)
warm_columns
# create a formula string with all selected column names
table_formula <- paste(warm_columns, collapse = " + ")


get_outcomes <- paste(warm_columns, collapse = " , ")
get_outcomes
table_formula
# add a response variable if needed, e.g., outcome ~ env1 + env2 + ...
table_1_formula <- as.formula(paste("table_formula ~", table_formula))
table_1_formula

# find vars
table1::table1(data = dat, ~ warm_nz_euro + warm_maori + warm_pacific + warm_asians + 
                 warm_immigrants + warm_chinese + warm_overweight + warm_indians + 
                 warm_muslims + warm_elderly + warm_refugees + warm_mental_illness + 
                 warm_disabled + warm_lgbtq| wave, transpose = TRUE)

# calculate total n in dataset
n_total <- skimr::n_unique(dat$id)
n_total <- margot::pretty_number(n_total)
margot::here_save(n_total, "n_total")

# save total N
n_total_participants <- n_total
here_save(
  n_total_participants, "n_total_participants",
  dir_path = push_mods
)


# display total n in nzavs
n_total

# prepare data frame
dat_prep <- dat |>
  arrange(id, wave) |>
  as.data.frame() |>
  haven::zap_formats() |>
  haven::zap_label() |>
  haven::zap_widths() |>
  margot::remove_numeric_attributes() |>
  mutate(not_heterosexual = sexual_orientation_l1 - 1) |>
  rename(sample_weights = w_gend_age_ethnic) |>
  rename(short_form_health = sfhealth) |>
  # reverse score exposure so that it represents loss in confidence in science
  # mutate(
  #   trust_science_low_confidence_scientific_community = 8 - trust_science_high_confidence_scientific_community
  # ) |>
  # create not lost variable (can be done later if needed)
  # mutate(
  #   not_lost = ifelse(lead(year_measured) == 1, 1, 0),
  #   not_lost = ifelse(is.na(not_lost) & year_measured == 1, 1, not_lost),
  #   not_lost = ifelse(is.na(not_lost), 0, not_lost)
  # ) |>
  arrange(id, wave) |>
  droplevels()

# base name for the exposure variable
# name_exposure <- "trust_science_high_confidence_scientific_community"
# 
# # define variable names using paste0
# name_exposure_cat <- paste0(name_exposure, "_cat")
# name_exposure_binary <- paste0(name_exposure, "_binary")
# 
# # define wide variable names
# baseline_exposure_continuous <- paste0("t0_", name_exposure)
# baseline_exposure_cat <- paste0("t0_", name_exposure, "_cat")
# baseline_exposure_binary <- paste0("t0_", name_exposure, "_binary")
# 
# t1_name_exposure_continuous <- paste0("t1_", name_exposure, "_z")
# t1_name_exposure_cat <- paste0("t1_", name_exposure, "_cat")
# t1_name_exposure_binary <- paste0("t1_", name_exposure, "_binary")
# t1_name_exposure <- paste0("t1_", name_exposure)


# scale ranges ------------------------------------------------------------

scale_range_exposure = c(1,7)
# scale_ranges_outcomes = c(0,10)

# define waves and variables ---------------------------------------------

# # set wave identifiers
# baseline_wave <- "2020"
# exposure_waves <- c("2021") # in this case, one wave
# outcome_wave <- "2022"
# 
# # for individual plots
# all_waves <- c(baseline_wave,exposure_waves,outcome_wave)
# baseline_and_exposure_waves <- c(baseline_wave,exposure_waves)
# baseline_and_outcome_waves <- c(baseline_wave,outcome_wave)
# 
# # save
# here_save(baseline_wave, "baseline_wave")
# here_save(exposure_waves, "exposure_waves")
# here_save(outcome_wave, "outcome_wave")
# here_save(all_waves, "all_waves")
# here_save(baseline_and_exposure_waves, "baseline_and_exposure_waves")
# here_save(baseline_and_outcome_waves, "baseline_and_outcome_waves")


# define baseline variables
baseline_vars <- c(
  "age",
  "agreeableness",
  "alcohol_frequency",
  "alcohol_intensity",
  "belong",
  "born_nz_binary",
  "conscientiousness",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "extraversion",
  "hlth_disability_binary",
  "honesty_humility",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  "male_binary",
  "neuroticism",
  "not_heterosexual_binary",
  "nz_dep2018",
  "nzsei_13_l",
  "openness",
  "parent_binary",
  "partner_binary",
  "political_conservative",
  "power_no_control_composite",
  "religion_identification_level",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",
  "short_form_health",
  "smoker_binary",
  "support",
  "free_speech",
  "pol_politician_trust",
  "police_trust",
  "trust_science_our_society_places_too_much_emphasis_reversed"
)

# define baseline variables without log transformation
baseline_vars_no_log <- c(
  "age",
  "agreeableness",
  "alcohol_frequency",
  "alcohol_intensity",
  "belong",
  "born_nz_binary",
  "conscientiousness",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "extraversion",
  "hlth_disability_binary",
  "honesty_humility",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "hours_children",
  "hours_commute",
  "hours_exercise",
  "hours_housework",
  "household_inc",
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  "male_binary",
  "neuroticism",
  "not_heterosexual_binary",
  "nz_dep2018",
  "nzsei_13_l",
  "openness",
  "parent_binary",
  "partner_binary",
  "political_conservative",
  "power_no_control_composite",
  "religion_identification_level",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",
  "short_form_health",
  "smoker_binary",
  "support",
  "free_speech",
  "pol_politician_trust",
  "police_trust",
  "trust_science_our_society_places_too_much_emphasis_reversed"
)

# sort baseline variables
baseline_vars <- sort(baseline_vars)
baseline_vars_log <- sort(baseline_vars_no_log)

# define exposure variables
exposure_var <- c(name_exposure, name_exposure_cat, "not_lost")


# INSTITUTIONAL TRUST OUTCOMES
outcome_vars <- c(
  # warm_nz_euro , 
  # warm_maori , 
  # warm_pacific , 
  # warm_asians , 
  # warm_immigrants , 
  # warm_chinese ,
  # warm_overweight , 
  # warm_indians , 
  "warm_muslims" #, 
  # warm_elderly , 
  # warm_refugees , 
  # warm_mental_illness , 
  # warm_disabled , 
  # warm_lgbtq
)

# apply paste0 to each outcome variable
# t2_outcome_vars_z <- paste0("t2_", outcome_vars, "_z")
# 
# # view the result
# here_save(t2_outcome_vars_z, "t2_outcome_vars_z")
# 

# outcome_vars_no_log <- c(
#   "env_climate_chg_concern",
#   "env_climate_chg_cause",
#   "env_climate_chg_real",
#   "env_sat_nz_environment",
#   "envefficacy"
# )

# outcome_vars <- sort(outcome_vars)
# outcome_vars_no_log <- sort(outcome_vars_no_log)
# 
# extra_vars <- c("id", "wave", "year_measured", "not_lost", "sample_weights", "gender_weights")
# all_vars <- c(baseline_vars, exposure_var, outcome_vars, extra_vars)
# 
# extra_vars_table <- c("id", "wave", "year_measured", "not_lost", "sample_weights")
# not_all_vars <- c(baseline_vars_no_log, exposure_var, outcome_vars_no_log, extra_vars_table)
# 
# all_vars <- sort(all_vars)
# not_all_vars <- sort(not_all_vars)

# define columns that will later be handled in a special way
# define continuous columns that we will not standardise
# continuous_columns_keep <- c("t0_sample_weights", "t0_gender_weights")
# 
# # define ordinal columns that we will expand into binary variables
# ordinal_columns <- c("t0_education_level_coarsen", "t0_eth_cat", "t0_rural_gch_2018_l")
# 
# # eligibility  ------------------------------------------------------------
# select baseline and exposure ids based on eligibility criteria
ids_baseline <- dat_prep |>
  filter(year_measured == 1, wave == baseline_wave) |>
  pull(id)

# optional: require no missing exposure values
# ids_exposure <- dat |>
#   filter(year_measured == 1, wave %in% exposure_waves) |> # this should be expanded if there are more than one exposure wave
#   filter(!is.na(!!sym(name_exposure))) |>
#   pull(id)


# if we are allowing missing values in the exposure then ids_study are the ids at baseline
ids_study <- ids_baseline

# else
# ids_study <- intersect(ids_baseline, ids_exposure)



# test function -----------------------------------------------------------




# make initial dataframe -------------------------------------------------
dat_long_1 <- dat_prep |>
  filter(id %in% ids_study & wave %in% c(baseline_wave, exposure_waves, outcome_wave)) |>
  droplevels() # note that we might have more than one exposure wave

# evaluate exposure variable/s
dat_long_exposure <- dat_long_1 |>
  filter(wave == exposure_waves) |>
  droplevels()

head(dat_long_exposure)

# check
name_exposure

# check
exposure_var
usethis
  
  # new graphs ---------------------------------------------------------


# you can check quantile breaks this way
# # ********* CHECK ************** #
# quantile(dat_long_exposure[[name_exposure]], na.rm = TRUE, probs = seq(0, 1, .25))
# # ********* CHECK ************** #


# another approach (more robust)
histogram_waves <- margot_plot_histogram(
  data = dat,
  col_names = "warm_muslims",
  id_col = "id",
  wave_col = "wave",
  waves = c(2012:2022),
  binwidth = 0.5,
  facet_scales = "free",
  color_palette = "gray",
  mean_line_color = "darkred",  # 
  sd_line_color = "darkred",
  save_path = here::here(push_mods)
)
library(plotly)

plotly_histogram_waves<- ggplotly(histogram_waves)

plotly_histogram_waves

histogram_trust_science_high_confidence_scientific_community_by_wave

plot_histogram_exposure <- here_read_qs('histogram_trust_science_high_confidence_scientific_community_by_wave', push_mods)
# 
# 
# # graph quantiles
# categorical_trust_science_high_confidence_scientific_community <- margot_plot_categorical( dat_long_exposure,
#                                                                                            col_name = name_exposure,
#                                                                                            cutpoint_inclusive = "lower",
#                                                                                            n_divisions = 3,
#                                                                                            save_path = here::here(push_mods),
#                                                                                            # subtitle  = '',
#                                                                                            # legend_position = "top",
#                                                                                            # custom_breaks = c(1,2,4,7),
#                                                                                            binwidth = .5)
# 
# 
# # view
# categorical_trust_science_high_confidence_scientific_community
# 
# # note size, make sure you have storage
# margot_size(categorical_trust_science_high_confidence_scientific_community)
# 
# # read graph back for manuscript
# # plot_histogram_cat_exposure<- here_read_qs("categorical_trust_science_high_confidence_scientific_community", push_mods)
# 
# 
# binary_categorical_trust_science_high_confidence_scientific_community <- margot_plot_categorical(
#   dat_long_exposure,
#   col_name = name_exposure,
#   cutpoint_inclusive = "lower",
#   # n_divisions = 3,
#   save_path = here::here(push_mods),
#   # subtitle  = '',
#   # legend_position = "top",
#   custom_breaks = c(1, 5, 7),
#   binwidth = .5,
#   file_prefix = "binary"
# )
# 
# # view
# binary_categorical_trust_science_high_confidence_scientific_community

# for manuscript
# plot_histogram_binary_exposure <- here_read_qs('binary_categorical_trust_science_high_confidence_scientific_community', push_mods)

# save plot
shift_trust_science_high_confidence_scientific_community_up <- margot_plot_shift(
#   dat_long_exposure,
#   col_name = name_exposure,
#   shift = "up",
#   range_highlight = c(6.1, 7),
#   binwidth = .5,
#   save_path = here::here(push_mods),
#   show_avg_line = TRUE
# )
# 
# # view
shift_trust_science_high_confidence_scientific_community_up

# for manuscript
# shift_trust_science_high_confidence_scientific_community_up <- here_read_qs('shift_trust_science_high_confidence_scientific_community_up', push_mods)

# # saved as shift_trust_science_high_confidence_scientific_community_up
# 
# shift_trust_science_high_confidence_scientific_community_down <- margot_plot_shift(
#   dat_long_exposure,
#   col_name = name_exposure,
#   shift = "down",
#   range_highlight = c(1, 1.9),
#   binwidth = .5,
#   save_path = here::here(push_mods),
#   show_avg_line = TRUE
# )
# 
# # view
# shift_trust_science_high_confidence_scientific_community_down
# 
# 
# # shift_trust_science_high_confidence_scientific_community_down <- here_read_qs('shift_trust_science_high_confidence_scientific_community_down', push_mods)

# old descriptive plot methods, clunky but still work ---------------------


# graph the exposure if using a shift intervention: down
# graph_shift_down <- margot::coloured_histogram_shift(
#   dat_long_exposure,
#   col_name = name_exposure,
#   shift = "down",
#   range_highlight = c(1, 1.9),
#   binwidth = .5,
#   show_avg_line = TRUE
# )
# 
# # show plot
# graph_shift_down

# create categorical variable (if desired) ------------------------------
# dat_long_2 <- create_ordered_variable(
#   dat_long_1,
#   var_name = name_exposure,
#   n_divisions = 3,
#   cutpoint_inclusive = "lower"
# )
# 
# 
# # test 
# sort( names( dat ) )
# conspiracy_columns <- dat %>%
#   select(contains("conspiracy"))
# head( conspiracy_columns)


###  ************** MANUALLY ENTERED **************
# table(is.na(
#   dat_long_2[[name_exposure]]
# ))
# 
# # convert binary factors to 0, 1 -----------------------------------------
# # we do this using a custom function
# dat_long_3 <- margot_process_binary_vars(dat_long_2)
# 
# 
# 
# # make 'not lost' variable ------------------------------------------------
dat_long_4 <- dat_long_3 |>
  arrange(id, wave) |>
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    not_lost = ifelse(is.na(not_lost) & year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  droplevels()

# log-transform 'hours_' variables ----------------------------------------

dat_long_table <- margot_log_transform_vars(
  dat_long_4,
  vars = c(starts_with("hours_"), "household_inc"),
  #  exceptions = "hours_work", no exceptions
  prefix = "log_",
  keep_original = TRUE ## Set to FALSE
) |>
  select(all_of(not_all_vars)) |>
  droplevels()


# boxplots using dat_long_table -------------------------------------------




plot_boxplot_muslims <-  margot_plot_boxplot(
  dat,
  y_vars = c("warm_muslims", "warm_immigrants", "warm_nz_euro", "warm_chinese"),
  wave = c(2012:2022),
  save_path = here::here(push_mods),
  prefix = "all_waves",
  coord_flip = FALSE,
  notch = TRUE,
  id_col = "id"
)

# view
plot_boxplot_muslims

# for manuscript
# boxplot_exposure <- here_read_qs("baseline_and_exposure_waves_boxplot_trust_science_high_confidence_scientific_community.qs", push_mods)

# baseline outcome waves --------------------------------------------------


boxplot_outcomes <-  margot_plot_boxplot(
  dat_long_table,
  y_vars = outcome_vars,
  # Changed from y_var to y_vars
  wave = baseline_and_outcome_waves,
  save_path = here::here(push_mods),
  # prefix = "baseline_and_outcome_waves",
  coord_flip = TRUE,
  id_col = "id"
)
# view
boxplot_outcomes

# for manuscript
# boxplot_outcomes <- here_read_qs("boxplot_env_climate_chg_cause_env_climate_chg_concern_env_climate_chg_real_env_sat_nz_environment_envefficacy", push_mods)

# summary tables ----------------------------------------------------------
# IMPORTANT FUNCTION  creates summary tables in one place
summary_tables <- margot_summary_tables(
  data = dat_long_table,
  baseline_wave = baseline_wave,
  exposure_waves = exposure_waves,
  outcome_wave = outcome_wave,
  name_exposure = name_exposure,
  baseline_vars = baseline_vars_no_log,
  outcome_vars = outcome_vars_no_log,
  extra_vars = extra_vars,
  create_plots = TRUE,
  plot_type = "boxplot"
)

summary_tables$plots$exposure_separate

y_vars = list("warm_muslims", "warm_asians", "warm_indians", "warm_immigrants"),

outcome_vars

exposure_waves
dat_long_table$wave
print(outcome_vars)
names(dat_long_table)


plot_boxplot_allwaves_outcome = margot_plot_boxplot(dat_long_table, 
                                                    y_vars = outcome_vars,  # Changed from y_var to y_vars
                                                    wave = all_waves,
                                                    #  save_path = here::here(push_mods),
                                                    id_col = "id", coord_flip = TRUE)

plot_boxplot_allwaves_outcome


summary_tables$exposure_summary 
# show details
summary_tables$baseline_table

summary_tables$exposure_table |> kbl("markdown")
summary_tables$outcome_table
summary_tables$n_participants

summary_tables$plots$exposure_distribution
summary_tables$plots$exposure_separatex
summary_tables$plots$outcome_plot

# check size
margot_size(summary_tables) # quite large


# key details for exposure
summary_tables$exposure_summary[[2]]$mean_exposure
summary_tables$exposure_summary[[2]]$sd_exposure

# save
margot::here_save_qs(summary_tables, "summary_tables", push_mods)

# check
# summary_tables <- margot::here_read_qs("summary_tables", push_mods)

summary_tables$exposure_summary[[2]]$mean_exposure
summary_tables$exposure_summary[[2]]$sd_exposure

# check size
margot_size(summary_tables)

push_mods
# graph individual trajectories --------------------------------------------------


individual_responses_plot_exposure <- margot_plot_individual_responses(dat,
                                                                       y_vars = "warm_muslims",
                                                                       id_col = "id",
                                                                       waves = c(2012:2022),
                                                                       theme = theme_classic(),
                                                                       random_draws = 50,
                                                                       title = NULL,
                                                                       y_label = NULL,
                                                                       x_label = NULL,
                                                                       color_palette = NULL,
                                                                       include_timestamp = FALSE,
                                                                       save_path = here::here(push_mods),
                                                                       width = 16,
                                                                       height = 8,
                                                                       seed = 123,
                                                                       scale_range = scale_range_exposure)

individual_responses_plot_exposure



margot_plot_individual_responses

individual_responses_plot_outcomes <- margot_plot_individual_responses(dat_long_table,
                                                                       y_vars = outcome_vars,
                                                                       id_col = "id",
                                                                       waves = all_waves,
                                                                       theme = theme_classic(),
                                                                       random_draws = 100,
                                                                       title = NULL,
                                                                       y_label = NULL,
                                                                       x_label = NULL,
                                                                       color_palette = NULL,
                                                                       include_timestamp = FALSE,
                                                                       save_path = here::here(push_mods),
                                                                       width = 16,
                                                                       height = 8,
                                                                       seed = 123,
                                                                       eligibility_all = TRUE,
                                                                       scale_range = scale_ranges_outcomes)

individual_responses_plot_outcomes
