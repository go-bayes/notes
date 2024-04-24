# test margot package

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
  dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing
  dplyr::filter(!is.na(eth_cat)) |> # criteria, no missing
  pull(id)


# obtain ids for individuals who participated in 2019
ids_2019 <- df_nz %>%
  dplyr::filter(year_measured == 1, wave == 2019) %>%
  dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing
  pull(id)

# intersect IDs from 2018 and 2019 to ensure participation in both years
ids_2018_2019 <- intersect(ids_2018, ids_2019)
# data wrangling
dat_long <- df_nz |>
  dplyr::filter(id %in% ids_2018_2019 &
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


# df_19 <- df_nz |> filter(wave == 2019) |> 
#   select(forgiveness) |>   drop_na()
# 
# 
# coloured_histogram_quantiles <- coloured_histogram_quantiles(
#    df = df_19,
#    col_name = "forgiveness",
#    n_quantiles = 2,
#    binwidth = 0.1 # Adjust binwidth as needed
#   )
# 
# 
# 

# summary(dat_long$perfectionism)
# table(dt_test$perfectionism_3tile)
# 
# 
# dt_test <- create_ordered_variable_custom(df_nz, "hours_exercise", 
#                                           c(1, 2, 7, Inf), c("[1_2)", "[2_7)", "[7_up]")) #
# 
# 
create_ordered_variable <- function(df, var_name, n_divisions = NULL) {
  if (is.null(n_divisions)) {
    stop("Please specify the number of divisions.")
  }
  
  # Ensure that the variable exists in the dataframe
  if (!var_name %in% names(df)) {
    stop("The specified variable does not exist in the dataframe.")
  }
  
  # Handle NA values in data: ignore NAs for quantile computation but keep them in the final output
  data_without_na <- na.omit(df[[var_name]])
  
  # Calculate initial quantile breaks
  quantile_breaks <- quantile(data_without_na, probs = seq(0, 1, length.out = n_divisions + 1), na.rm = TRUE)
  
  # Ensure that breaks are unique and cover the full range of data
  if (length(unique(quantile_breaks)) != length(quantile_breaks)) {
    warning("Quantile breaks are not unique; adjusting to handle ties or insufficient unique values.")
    unique_breaks <- unique(quantile_breaks)
    while (length(unique_breaks) < n_divisions + 1) {
      # Extend the breaks slightly beyond the max value to ensure coverage
      unique_breaks <- sort(c(unique_breaks, max(unique_breaks) + diff(range(unique_breaks))/100))
    }
    quantile_breaks <- unique_breaks
  }
  
  # Adjust the last break to be slightly greater than the max value to ensure inclusion of the max value
  quantile_breaks[length(quantile_breaks)] <- max(quantile_breaks[length(quantile_breaks)], max(df[[var_name]], na.rm = TRUE)) + .Machine$double.eps*100
  
  # Create labels for each segment based on the actual number of breaks
  cut_labels <- paste0("tile_", seq_len(length(quantile_breaks) - 1))
  
  # Assign the ordered factor to the dataframe
  df[[paste0(var_name, "_", n_divisions, "tile")]] <- cut(
    df[[var_name]],
    breaks = quantile_breaks,
    labels = cut_labels,
    ordered_result = TRUE,
    include.lowest = TRUE
  )
  
  return(df)
}

dat_long <- create_ordered_variable(dat_long, "perfectionism", n_divisions = 4) #
table(is.na(dat_long$perfectionism_4tile))
dat_long$perfectionism_4tile


# view scale 
print(quantile(dat_long$perfectionism, probs = seq(0, 1, 1/4), na.rm = TRUE))
table(dat_long$perfectionism_4tile)


# 
# # not suited to quartiles 
# max(dt_test$meaning_sense, na.rm=TRUE)


labels <- levels( dat_long$perfectionism_4tile )
labels


# create ordered varaible based on quartiles at time 11 (exposure wave)
# note these values may change from year to year because "quartile" is relative. So we pick the exposure year to define our units
# 
# dat_long_t <- create_ordered_variable_custom(dat_long, "lifemeaning", c(1, 5, 5.5, 6.5,  7), lifemeaning_labels)
# 
# table( dat_long_t$lifemeaning_coarsen )




baseline_vars = c("age", "male", "edu", "eth_cat", "partner", "employed", "born_nz", "neighbourhood_community", "household_inc_log", "parent", "religion_religious", "urban", "employed", "alert_level_combined_lead", "sample_weights")

# treatment
exposure_vars = c("perfectionism_4tile") 

# outcome, can be many
outcome_vars = c("kessler_latent_anxiety", "kessler_latent_depression")


# make long data wide
prep_dat_wide <- margot_wide(dat_long, 
                             baseline_vars = baseline_vars, 
                             exposure_var = exposure_vars,
                             outcome_vars = outcome_vars)


# filter data, so that we impute within quartiles
list_filtered_df <-
  margot::margot_filter(prep_dat_wide, exposure_vars = "t1_perfectionism_4tile", sort_var = "id")

# checks 
a <- nrow( list_filtered_df$tile_1)
b <- nrow( list_filtered_df$tile_2)
c <- nrow( list_filtered_df$tile_3)
d <-nrow( list_filtered_df$tile_4)

# must sum to equal
a + b + c + d == nrow(prep_dat_wide)


# visually inspect missingness
naniar::vis_miss(prep_dat_wide, warn_large_data = FALSE)

# check for collinear vars
mice:::find.collinear(prep_dat_wide)

# impute by quartile
mice_health <- margot::impute_and_combine(list_filtered_df,  m = 5 )

# post-impute arrange
mice_health_mids <- mice_health %>%
  arrange(.imp, id) |>
  rename(sample_weights = t0_sample_weights) |>
dplyr::mutate(
  across(
    where(is.numeric) & !t0_alert_level_combined_lead &
      !sample_weights,
    ~ scale(.x),
    .names = "{col}_z"
  )
) %>%
  select(-.imp_z, -.id_z) %>%
  select(where(is.factor),
         sample_weights,
         ends_with("_z"),
         .imp,
         .id) |>
  relocate(sample_weights, .before = starts_with("t1_"))  %>%
  relocate(id, .before = sample_weights)  %>%
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  arrange(.imp, id) |>
  droplevels() |>
  mutate_if(is.matrix, as.vector) |>
  as.mids()

# create long version
mice_health_long <-mice::complete(mice_health_mids, "long", inc = TRUE)

# create


# save
saveRDS(mice_health_mids, here::here("dump", "mice_health_mids"))
saveRDS(mice_health_long, here::here("dump", "mice_health_long"))
mice_health_mids <- readRDS(here::here("dump", "mice_health_mids"))
mice_health_long <- readRDS(here::here("dump", "mice_health_long"))



# propensity scors --------------------------------------------------------


# set exposure
X = "t1_perfectionism_4tile"

#
estimand = "ATE"

baseline_vars_models = mice_health_long |>  # post process of impute and combine
  dplyr::select(starts_with("t0"))|> colnames() # note, we earlier change name of `t0_sample_weights` to `sample weights`


# obtain propensity scores
match_ebal_ate <- margot::match_mi_general(data = mice_health_mids, 
                                      X = X, 
                                      baseline_vars = baseline_vars_models, 
                                      estimand = estimand,  
                                      # focal = "< >", for ATT
                                      method = "ebal", 
                                      sample_weights = "sample_weights")


# check balance
bal.tab(match_ebal_ate)

love.plot(match_ebal_ate, binary = "std", thresholds = c(m = .1),
          wrap = 50, position = "bottom", size =3) 

# consider results 
sum_ebal_match_ebal_ate <- summary(match_ebal_ate)
sum_ebal_match_ebal_ate
plot(sum_ebal_match_ebal_ate)


# trimmed weights
match_ebal_ate_trim <- WeightIt::trim(match_ebal_ate, at = .99)

# check balanc
bal.tab(match_ebal_ate_trim)

# summary
summary_match_ebal_ate_trim <- summary(match_ebal_ate_trim)

# check - extreme weights gone
plot(summary_match_ebal_ate_trim)


# plot for balance
love.plot(match_ebal_ate_trim, binary = "std", thresholds = c(m = .1),
          wrap = 50, position = "bottom", size =2, limits = list(m = c(-.5, .5)))

# here_save(match_ebal_ate_trim, "match_ebal_ate_trim")
# match_ebal_ate_trim <- here_read( "match_ebal_ate_trim")
# 

# set data frame; output of match_mi_general model
df_ate = match_ebal_ate_trim

# remind self of levels
levels(mice_health_long$t1_perfectionism_4tile)

# set treatment level
treat_0 = "tile_1" # lowest quartile
treat_1 = "tile_3" # third quartile

# bootstrap simulations ( generally use 1000)
nsims <- 200

# cores
cl =  parallel::detectCores () 

estimand = "ATE"

# as specified
vcov = "HC2" # robust standard errors. 

# cores
cores = parallel::detectCores () # use all course
# 

# Example call to the function
mod_fit_t2_kessler_latent_anxiety_z <-margot::causal_contrast_marginal(
  df = df_ate,
  Y = "t2_kessler_latent_anxiety_z",
  X = X,
  baseline_vars = baseline_vars_models,
  treat_1 = treat_1,
  treat_0 = treat_0,
  nsims = 200,
  cores = cores,
  family = "gaussian",
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type = "RD",
  vcov = vcov
)

mod_fit_t2_kessler_latent_anxiety_z


out_tab_engine <- margot::tab_engine_marginal(mod_fit_t2_kessler_latent_anxiety_z,
                                      new_name = "test",
                                      delta = 1,
                                      sd = 1,
                                      type = "RD",
                                      continuous_X = FALSE)
out_tab_engine



fit_1  <- margot::double_robust_marginal(
  df = df_ate,
  Y = "t2_kessler_latent_anxiety_z",
  X = X,
  baseline_vars = baseline_vars_models,
  treat_1 = treat_1,
  treat_0 = treat_0,
  nsims = 200,
  cores = cores,
  family = "gaussian",
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type_causal = "RD",  
  type_tab = "RD",    
  vcov = vcov,         
  new_name = "test",
  delta = 1,
  sd = 1
)

fit_1$tab_results

fit_1$causal

# conditional effects -----------------------------------------------------
na_count <- sum(is.na(mice_health_mids$data$t0_eth_cat))
print(paste("Number of NA values in 't0_eth_cat':", na_count))

df_test<- as.data.frame(mice_health_mids[[2]])

df_test

library(mice)
library(MatchIt)
library(MatchThem)

perform_analysis <- function(data, formula, method, estimand) {
  # Function to perform matching or weighting
  if (method == "ps") {
    return(weightthem(formula = formula, data = data, estimand = estimand, method = "ps"))
  } else {
    match_out <- matchit(formula = formula, data = data, method = method, estimand = estimand)
    return(match.data(match_out))
  }
}

process_subgroups <- function(data, X, baseline_vars, subgroup, estimand, method) {
  # Helper function to process each subgroup within a given dataset
  formula_str <- as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))
  levels_list <- unique(data[[subgroup]])
  results_list <- vector("list", length(levels_list))
  
  for (i in seq_along(levels_list)) {
    level <- levels_list[i]
    sub_data <- subset(data, data[[subgroup]] == level, drop = FALSE)
    if (nrow(sub_data) > 0) {
      results_list[[i]] <- perform_analysis(sub_data, formula_str, method, estimand)
    } else {
      warning(paste("No data available for subgroup:", level))
      results_list[[i]] <- NULL
    }
  }
  names(results_list) <- levels_list
  return(results_list)
}

match_mi_subgroup <- function(data, X, baseline_vars, estimand, method, subgroup = NULL) {
  data_class <- class(data)
  
  if ("mids" %in% data_class) {
    # Handle mids object: process each imputation separately
    results <- lapply(1:mice::nimp(data), function(imp) {
      imp_data <- mice::complete(data, action = imp)
      if (!is.null(subgroup)) {
        process_subgroups(imp_data, X, baseline_vars, subgroup, estimand, method)
      } else {
        perform_analysis(imp_data, as.formula(paste(X, "~", paste(baseline_vars, collapse = "+"))), method, estimand)
      }
    })
    return(results)
  } else if ("data.frame" %in% data_class) {
    # Handle regular dataframe
    if (!is.null(subgroup)) {
      return(process_subgroups(data, X, baseline_vars, subgroup, estimand, method))
    } else {
      return(perform_analysis(data, as.formula(paste(X, "~", paste(baseline_vars, collapse = "+"))), method, estimand))
    }
  } else {
    stop("Input data must be either 'mids' or 'data.frame' object")
  }
}



# Ensure that you adjust the names and parameters according to your actual data structure and needs.
match_ebal_cate <- match_mi_subgroup(data = mice_health_mids, 
                                    X = X, 
                                    baseline_vars = baseline_vars_models, 
                                    estimand = "ATT",  
                                    method = "ebal", 
                                    subgroup = "t0_eth_cat",
                                    sample_weights = "sample_weights")
print(match_ebal_cate)

mice_health_long$t0_eth_cat


baseline_vars_models = mice_health_long |>  # post process of impute and combine
  dplyr::select(starts_with("t0"))|> colnames() # note, we earlier change name of `t0_sample_weights` to `sample weights`


# obtain propensity scores
match_ebal_cate <- match_mi_general(data = mice_health_mids, 
                                   X = X, 
                                   baseline_vars = baseline_vars_models, 
                                   estimand = estimand,  
                                   # focal = "< >", for ATT
                                   method = "ebal", 
                                   subgroup = "t0_eth_cat",
                                   sample_weights = "sample_weights")
match_ebal_cate

baseline_vars_models_no_eth <- setdiff(baseline_vars_models, "t0_eth_cat")
match_mi_general()


dt_match_cate <- match_mi_general(
  data = mice_health_mids,
  X = X,
  baseline_vars = baseline_vars_models,
  estimand = "ATE",
  method = "ebal",
  subgroup = "t0_eth_cat"
)


# check balance
bal.tab(match_ebal_cate$euro) #  good
bal.tab(dt_match_super$māori) # not good

# save data
saveRDS(dt_match_super, here::here("data", "dt_match_super"))

# check balance
bal.tab(dt_match$euro)
bal.tab(dt_match$māori)

# code for summar
sum_e <- summary(dt_match$euro)
sum_m <- summary(dt_match$māori)
# sum_p <- summary(dt_match$pacific)
# sum_a <- summary(dt_match$asian)
sum_e
sum_m

plot(sum_e)
plot(sum_m)
#plot(sum_p)
#plot(sum_a)

love.plot(dt_match$euro,
          binary = "std",
          thresholds = c(m = .1))

love.plot(dt_match$māori,
          binary = "std",
          thresholds = c(m = .1))

love.plot(dt_match$pacific,
          binary = "std",
          thresholds = c(m = .1))
love.plot(dt_match$asian,
          binary = "std",
          thresholds = c(m = .1))


# prepare data
dt_ref_e <- subset(dt_8, t0_eth_cat == "euro")
dt_ref_e$weights <- dt_match$euro$weights

# prepare data
dt_ref_m <- subset(dt_8, t0_eth_cat == "māori")
dt_ref_m$weights <- dt_match$māori$weights

# combine
dt_ref_all <- rbind(dt_ref_e, dt_ref_m)

# call dataframe `df`
df = dt_ref_all


# Let's calculate the ATE for the entire group, ignoring the subclasses.
# let's make the contrasts between low and high perfectionism.
baseline_vars_reflective_propensity
baseline_vars_full

#  GENERAL ATE (Not adjusting for subgroups)
mod_ref_meaning   <- gcomp_sim(
  df = df,
  # note change
  Y = "t2_meaning_z",
  X = X,
  baseline_vars = baseline_vars_full,
  treat_1 = "high",
  treat_0 = "low",
  estimand = "ATE",
  scale = "RD",
  type = "RD",
  nsims = 1000,
  cores = 8,
  family = gaussian,
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  new_name = "t2_meaning_z (composite)"
)

# ATE. we will cover "evalues" next week


### SUBGROUP analysis
df = dt_ref_all
Y = "t2_meaning_z"
X = "t1_perfectionism_coarsen"
baseline_vars = baseline_vars_reflective_propensity
treat_0 = "low"
treat_1 = "high"
estimand = "ATE"
scale = "RD"
nsims = 1000
family = "gaussian"
continuous_X = FALSE
splines = FALSE
cores = parallel::detectCores()
subclass = "t0_eth_cat"

# not we interact the subclass X treatment X covariates

formula_str <-
  paste(
    Y,
    "~",
    S,
    "*",
    "(",
    X ,
    "*",
    "(",
    paste(baseline_vars_reflective_propensity, collapse = "+"),
    ")",
    ")"
  )

formula_str


# fit model
fit_all_all  <- glm(
  as.formula(formula_str),
  weights = weights,
  # weights = if (!is.null(weight_var)) weight_var else NULL,
  family = family,
  data = df
)



# simulate coefficients
sim_model_all <- sim(fit_all_all, n = nsims, vcov = "HC1")


# simulate effect as modified in europeans
sim_estimand_all_e <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "euro", would need to specify 
  verbose = FALSE
)

sim_estimand_all_e <-
  transform(sim_estimand_all_e, RD = `E[Y(low)]` - `E[Y(high)]`)
sim_estimand_all_e


# simulate effect as modified in māori
sim_estimand_all_m <- sim_ame(
  sim_model_all,
  var = X,
  cl = cores,
  subset = t0_eth_cat == "māori",  # would need to 
  verbose = FALSE
)

# combine
sim_estimand_all_m <-
  transform(sim_estimand_all_m, RD = `E[Y(low)]` - `E[Y(high)]`)


# summary
summary(sim_estimand_all_e)
summary(sim_estimand_all_m)

# rearrange
names(sim_estimand_all_e) <-
  paste(names(sim_estimand_all_e), "e", sep = "_")

names(sim_estimand_all_m) <-
  paste(names(sim_estimand_all_m), "m", sep = "_")


est_all <- cbind(sim_estimand_all_m, sim_estimand_all_e)
est_all <- transform(est_all, `RD_m - RD_e` = RD_m - RD_e)


# view summary
summary(est_all)



summary(fit_all_all)

coefs <- coef(fit_all_all)
table(is.na(coefs))#     t0_eth_catmāori:t1_perfectionism_coarsen.Q:t0_gen_cohort.C

# #FALSE  TRUE
# 344     4

insight::get_varcov(fit_all_all)



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
df_impute_base<- margot_wide_impute_baseline(dat_long, baseline_vars = baseline_vars, 
                                             exposure_var = exposure_vars, outcome_vars = outcome_vars)

dt_18 <- dat_long |> filter(wave == 2018)

# add sample weights
df_impute_base$t0_sample_weights = dt_18$sample_weights

# save
saveRDS(df_impute_base, here::here("dump", "df_impute_base"))
# train causal forest

# sample weights
t0_sample_weights <- df_impute_base$t0_sample_weights

# get censoring indicator
D <- df_impute_base$t1_censored

# get key data features 
baseline_vars
# 
# df_use_full <-
#   df_use_full_f |> select(
#     starts_with("t1"),
#     starts_with("t2"),
#     starts_with("t0"),
#     -starts_with("t0_warm"),-t0_hlth_disability_z,
#     -t0_hours_family_sqrt_round,
#     -t0_hours_friends_sqrt_round,-t0_sfhealth_z,
#     -t0_has_siblings,
#     -t0_hlth_bmi_z,
#     -t0_born_nz_z,
#     -t0_friends_time_z,-t0_family_time_z,
#     -t0_sample_origin,
#     -t0_vengeful_rumin_z,
#     -t0_religion_perceive_religious_discrim_z,
#     -t0_gratitude_z,
#     -t0_support_z
#   )



colnames( df_impute_base )
nrow( df_impute_base )

# get correct censoring 
t0_na_condition <-
  rowSums(is.na(select(df_impute_base, starts_with("t1_")))) > 0
t1_na_condition <-
  rowSums(is.na(select(df_impute_base, starts_with("t2_")))) > 0
baseline_vars
df_impute_base$t0_alert_level_combined_lead


df_clean <- df_impute_base %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        !t0_sample_weights & 
        !t0_alert_level_combined_lead &
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
    t0_alert_level_combined_lead,
    t0_sample_weights,
    t0_censored,
    t1_censored,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t1_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))



# checks
table(df_clean$t1_lost)
table(df_clean$t1_censored)

# 
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)
str( df_clean )

nrow(df_clean)


# Causal forest weights
grf_names_base <- df_clean |> select( starts_with("t0"), -starts_with(c("t1","t2")), - t0_sample_weights,-t0_censored, t0_lost, -t0_alert_level_combined_lead )|> colnames()

# eval grf fit ------------------------------------------------------------

# censoring ---------------------------------------------------------------

baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"), - t0_alert_level_combined_lead,-t0_censored, -t0_lost, -t0_sample_weights)|> colnames() # note, we ear

baseline_vars_models


summary(cen_ebal)
bal.tab(cen_ebal, un = TRUE)
love.plot(cen_ebal, binary = "std", thresholds = c(m = .1),
          wrap = 50, position = "bottom", size =2)

# assuming cen_ebal$weights are the propensity scores of being censored
lost_prob <- cen_ebal$weights

min(lost_prob)

test <- (cen_ebal$weights * df_clean$t0_sample_weights)


# rename sample weights 
df_clean$sample_weights <- (cen_ebal$weights * df_clean$t0_sample_weights)


df_clean_filtered<- df_clean_filtered |> 
  mutate(t1_perfectionism_high = ifelse(t1_perfectionism_z >= 1, 1, 0))

df_clean <- df_clean |>
  relocate("sample_weights", .before = starts_with("t0_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))


df_clean_filtered <- df_clean |> 
  filter(t1_censored == 1) 

nrow(df_clean_filtered)
df_clean_filtered$sample_weights

# next propensity scores by groups 

df_clean_filtered$t1_perfectionism_z
levels(df_clean_filtered$t0_eth_cat)

test_df <-df_clean_filtered |> filter(t0_eth_cat == "maori" | t0_eth_cat == "euro") |> droplevels()
levels(test_df$t0_eth_cat)
table(test_df$t0_eth_cat)

baseline_vars_models_sans_eth <- setdiff(baseline_vars_models, "t0_eth_cat")

string <- formula_str <- as.formula(paste("t1_perfectionism_high", "~", paste(baseline_vars_models_sans_eth, collapse = "+")))
baseline_vars_models_sans_eth
table(test_df$t0_eth_cat)



W1 <- weightit(string,   
               method = "super",
               estimand = "ATT",
               weights = "sample_weights",
               SL.library = c("SL.ranger","SL.glmnet", "SL.polymars", "SL.xgboost"),
               super = TRUE,
               data = df_clean_filtered)
summary(W1)

W2 <- weightit(
  string,
  method = "super",
  estimand = "ATT",
  by = "t0_eth_cat",
  weights = "sample_weights",
  super = TRUE, 
  SL.library = c("SL.ranger","SL.glmnet", "SL.polymars", "SL.xgboost"),
  data = df_clean_filtered
)

summary(W2)

# test diff

S <- sbps(W1, W2)

test_df$t0_eth_cat


bal.tab(W1, un = TRUE)
love.plot(W1, binary = "std", thresholds = c(m = .1),
          wrap = 50, position = "bottom", size =2)

bal.tab(W2, cluster = "t0_eth_cat")


