# adela

### ALWAYS RESTART R IN A FRESH SESSION ####
listWrappers()
push_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-adela"
)

#devtools::install_github("go-bayes/margot")
#devtools::install_github("nt-williams/lmtp@devel")

# get devtools
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# get 'margot' from my github (make sure to update)
# devtools::install_github("go-bayes/margot")



# Check if pacman is installed; if not, install it
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
  lmtp,
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
  kableExtra
)

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
# source("/Users/joseph/GIT/templates/functions/libs2")


# set data path
pull_path <-
  fs::path_expand(
    #"/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

# import data
dat <- qs::qread(here::here(pull_path))

# fetch names 
names <- colnames(dat)

# arrange alphabetically 
names <- sort(names)

# check names
names

# get data for those in 
summary(dat$nation_is_nz)



# get 'margot' from my github (make sure to update)
# devtools::install_github("go-bayes/margot")


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
  margot,
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

name_exposure_raw <- "nation_is_nz"
## se;ect data
ids_2018 <- dat %>%
  filter(year_measured == 1, wave == 2018) %>%
  filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
  pull(id)

# Obtain IDs for individuals who participated in 2019
# ids_2019 <- dat %>%
#   filter(year_measured == 1, wave == 2019) %>%
#   filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
#   pull(id)
# 
# # Intersect IDs from 2018 and 2019 to ensure participation in both years
# ids_2018_2019 <- intersect(ids_2018, ids_2019)



# filter the original dataset for these IDs three waves
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)
str(dat)

name_exposure_raw = "nation_is_nz"
table(dat$wave)
ids_2018 <- dat %>%
  filter(year_measured == 1, wave == 2018) %>%
  filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
  pull(id)

dat$sample_origin

dat_long <- dat |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020, 2021, 2022)) |>
  arrange(id, wave) |>
  select(
    "id",
    "wave",
    "year_measured",
    "nation_is_nz",
    "rural_gch_2018_l",
    # "edu",
    "male",
    "age",
    "education_level_coarsen",
    # factors
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    #"bigger_doms", #religious denomination
    "sample_origin_names_combined",
    "nz_dep2018",
    "nzsei13",
    "born_nz",
    "hlth_disability",
    # "hlth_bmi",
    # "pwi", # pwi
    # "kessler6_sum",
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    "support",
    #soc support
    "belong",
    # social belonging
    "has_siblings",
    #  "smoker", # smoker
    # "sfhealth",
    # "alcohol_frequency", measured with error
    # "alcohol_intensity",
    # "hours_family_log",
    # "hours_friends_log",
    # "hours_community_log",
    # "hours_community_sqrt_round",
    # "lifemeaning",
    "household_inc",
    # added: measured with error but OK for imputations
    "partner",
    "employed",
    # "parent",  # newly changed - have information in child number
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    # Sample origin names combined
    "urban",
    "children_num",
    "hours_children",
    # new
    "hours_work",
    # new
    "hours_housework",
    #new
    "hours_exercise",
    "agreeableness",
    "conscientiousness",
    "extraversion",
    "honesty_humility",
    "openness",
    "neuroticism",
    "modesty",
    "hours_community",
    # I want people to know that I am an important person of high status, I am an ordinary person who is no better than others. , I wouldn’t want people to treat me as though I were superior to them. I think that I am entitled to more respect than the average person is.
    # "religion_religious", # Do you identify with a religion and/or spiritual group?
    # "religion_identification_level", #How important is your religion to how you see yourself?"  # note this is not a great measure of virtue, virtue is a mean between extremes.
    "religion_church",
    # "religion_religious", #
    "religion_spiritual_identification",
    "religion_identification_level",
    "w_gend_age_euro",
    #  "religion_religious",
    #  "religion_church_binary",
    #  "religion_prayer_binary",
    #  "religion_scripture_binary",
    #"religion_believe_god",
    #"religion_believe_spirit",
    "alcohol_frequency",
    # health
    "alcohol_intensity",
    # health
    "hlth_bmi",
    # health
    "hours_exercise",
    # health
    "sfhealth",
    # health
    "sfhealth_your_health",
    # "In general, would you say your health is...
    # "sfhealth_get_sick_easier",#\nI seem to get sick a little easier than other people.
    # "sfhealth_expect_worse_health",
    "hlth_sleep_hours",
    # health
    "smoker",
    # health
    "hlth_fatigue",
    # embodied
    "rumination",
    # embodied
    # "kessler6_sum",
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    # embodied
    "bodysat",
    #ego
    # "vengeful_rumin",
    #ego
    ## Am satisfied with the appearance, size and shape of my body.
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "perfectionism",
    # # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
    # I am hardly ever satisfied with my performance.
    "power_no_control_composite",
    # "power_self_nocontrol",
    # I do not have enough power or control over\nimportant parts of my life.
    #"power_others_control",
    # Other people have too much power or control over\nimportant parts of my life
    "self_esteem",
    # "selfesteem_satself", #  On the whole am satisfied with myself.
    # "selfesteem_postiveself",# Take a positive attitude toward myself
    # "selfesteem_rfailure", # Am inclined to feel that I am a failure.
    "sexual_satisfaction",
    "self_control_have_lots",
    #In general, I have a lot of self-control.
    "self_control_wish_more_reversed",
    #I wish I had more self-discipline.(r)
    "emotion_regulation_out_control",
    # When I feel negative emotions, my emotions feel out of control. w10 - w13
    # "emotion_regulation_hide_neg_emotions",
    # When I feel negative emotions, I suppress or hide my emotions. w10 - w13
    # "emotion_regulation_change_thinking_to_calm",#,#, # When I feel negative emotions, I change the way I think to help me stay calm. w10 - w13
    # "emp_work_life_balance"# I have a good balance between work and other important things in my life.
    #"respect_self",
    # "vengeful_rumin",
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
    "pwb_your_health",
    #Your health.
    "pwb_your_relationships",
    #Your personal relationships.
    "pwb_your_future_security",
    #Your future security.
    "pwb_standard_living",
    #Your standard of living.
    "lifesat",
    # "lifesat_satlife",# I am satisfied with my life.
    # "lifesat_ideal"#,# In most ways my life is close to ideal.
    #"lifemeaning",
    "meaning_purpose",# My life has a clear sense of purpose.
    "meaning_sense", # I have a good sense of what makes my life meaningful.
    "permeability_individual",
    #I believe I am capable, as an individual\nof improving my status in society.
    #"impermeability_group",
    #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
    "neighbourhood_community",
    #I feel a sense of community with others in my local neighbourhood.
    "belong",
    "support",
    "alert_level_combined_lead",
    "alert_level_combined") |>
  mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
  # mutate(hours_community_round = round(ifelse(hours_community >= 24, 24, hours_community), 0)) |>
  mutate(
    #initialize 'censored'
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    not_lost =  ifelse(is.na(not_lost) &
                         year_measured == 1, 1, not_lost)
    
    # # Apply the case_when condition for setting 'censored' based on 'wave' and the dynamic column specified by 'nzavs_exposure'
    # censored = case_when(
    #   # Add this condition to keep previous modifications unless the specific condition is met!is.na(censored) ~ censored,
    #
    #   # Then check if 'wave' is 2019 and the specified exposure is NA, adjusting the condition to reflect the accurate logic
    #   wave == 2019 & !is.na(!!sym(nzavs_exposure)) ~ 1,
    #
    #   # Default case if none of the above apply; might not be necessary if all possibilities are covered
    #   TRUE ~ 0
    # )
  ) |>
  dplyr::mutate(
    # someone gave neg number
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    household_inc_log = log(household_inc + 1),
    #  hours_charity_log = log(hours_charity + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_children_log = log(hours_children + 1)
  ) |>
  dplyr::select(
    -c(
      hours_work,
      hours_housework,
      household_inc,
      hours_exercise,
      hours_children,
      has_siblings,
      hours_community
      #    children_num,
      #    total_siblings
    )
  ) |>
  droplevels() |>
  ungroup() |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  arrange(id, wave) |>
  droplevels() |>
  arrange(id, wave) |>
  #   mutate(
  #   religion_church_coarsen = cut(
  #     religion_church,
  #     breaks = c(-Inf, 0, 1, 3.99, Inf),
  #     labels = c("zero", "one", "less_four", "four_up"),
  #     include.lowest = TRUE,
  #     right = TRUE,
  #     ordered = TRUE
  #   )
  # ) %>%
  # mutate(
  #   religion_church_coarsen_n = as.numeric(religion_church_coarsen) - 1,
  #   religion_church_binary_n = as.numeric(religion_church_binary)
  # ) |>
  mutate(
    born_nz = as.numeric(born_nz), 
    hlth_disability = as.numeric(hlth_disability), 
    partner = as.numeric(partner),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)#, 
    # alcohol_frequency = as.numeric(alcohol_frequency),
    # smoker = as.numeric(smoker)
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  mutate(
    rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
    #   parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    not_lost = as.numeric(as.character(not_lost)),
    employed = as.numeric(as.character(employed)),
    hlth_disability = as.numeric(as.character(hlth_disability))
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

# check

d_measured <- dat |> 
  filter(year_measured ==1)


table1::table1(~ factor(nation_is_nz) | wave, data = d_measured,transpose = F
               )

n_participants <- n_unique(dat_long$id)
n_participants <- prettyNum(n_participants,big.mark=",")

margot::here_save(n_participants, n_participants)
n_participants


numbers <- c(1000, 50000, 1234567)

margot::pretty_number(numbers)

n_participants <- n_unique(dat_long$id)
n_participants <- prettyNum( n_participants,big.mark=",")

margot::here_save(n_participants, n_participants)
n_participants



# create sample weights for male female -----------------------------------

# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <- 0.5  # target proportion of males in the population
prop_female_population <- 0.5  # target proportion of females in the population

prop_male_sample <- mean(dat_long$male)
prop_female_sample <- 1 - prop_male_sample

gender_weight_male <- prop_male_population / prop_male_sample
gender_weight_female <- prop_female_population / prop_female_sample

dat_long$sample_weights <- ifelse(dat_long$male == 1, gender_weight_male, gender_weight_female)

hist(dat_long$sample_weights)

# check male are upweighted
head(dat_long[, c("male", "sample_weights")])




# baseline vars -----------------------------------------------------------
str(dat_long)
# check
table(dat_long$not_lost)

# select vars for baseline
dat_long_colnames <- colnames(dat_long)


dat_long_colnames <- sort(dat_long_colnames)
dat_long_colnames
baseline_vars <- setdiff(dat_long_colnames, c("id", "wave", "alert_level_combined", "alert_level_combined_lead", "sample_weights", "year_measured", "sample_weights","not_lost"))

baseline_vars


# set baseline exposure and outcomes --------------------------------------

exposure_var = c("nation_is_nz",
                 "not_lost"#,
) #


# set outcomes for prosocial domain
# save prejudice for separate paper
outcome_vars = c(
  "alcohol_frequency",
  # health
  "alcohol_intensity",
  # health
  "hlth_bmi",
  # health
  "hours_exercise_log",
  # health
  "sfhealth",
  # health
  "sfhealth_your_health",
  # "In general, would you say your health is...
  # "sfhealth_get_sick_easier",#\nI seem to get sick a little easier than other people.
  # "sfhealth_expect_worse_health",
  "hlth_sleep_hours",
  # health
  "smoker",
  # health
  "hlth_fatigue",
  # embodied
  "rumination",
  # embodied
  # "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  # embodied
  "bodysat",
  #ego
  # "vengeful_rumin",
  #ego
  ## Am satisfied with the appearance, size and shape of my body.
  # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
  "perfectionism",
  # # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
  # I am hardly ever satisfied with my performance.
  "power_no_control_composite",
  # "power_self_nocontrol",
  # I do not have enough power or control over\nimportant parts of my life.
  #"power_others_control",
  # Other people have too much power or control over\nimportant parts of my life
  "self_esteem",
  # "selfesteem_satself", #  On the whole am satisfied with myself.
  # "selfesteem_postiveself",# Take a positive attitude toward myself
  # "selfesteem_rfailure", # Am inclined to feel that I am a failure.
  "sexual_satisfaction",
  "self_control_have_lots",
  #In general, I have a lot of self-control.
  "self_control_wish_more_reversed",
  #I wish I had more self-discipline.(r)
  "emotion_regulation_out_control",
  # When I feel negative emotions, my emotions feel out of control. w10 - w13
  # "emotion_regulation_hide_neg_emotions",
  # When I feel negative emotions, I suppress or hide my emotions. w10 - w13
  # "emotion_regulation_change_thinking_to_calm",#,#, # When I feel negative emotions, I change the way I think to help me stay calm. w10 - w13
  # "emp_work_life_balance"# I have a good balance between work and other important things in my life.
  #"respect_self",
  # "vengeful_rumin",
  "gratitude",
  ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
  "pwb_your_health",
  #Your health.
  "pwb_your_relationships",
  #Your personal relationships.
  "pwb_your_future_security",
  #Your future security.
  "pwb_standard_living",
  #Your standard of living.
  "lifesat",
  # "lifesat_satlife",# I am satisfied with my life.
  # "lifesat_ideal"#,# In most ways my life is close to ideal.
  #"lifemeaning",
  "meaning_purpose",# My life has a clear sense of purpose.
  "meaning_sense", # I have a good sense of what makes my life meaningful.
  "permeability_individual",
  #I believe I am capable, as an individual\nof improving my status in society.
  #"impermeability_group",
  #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
  "neighbourhood_community",
  #I feel a sense of community with others in my local neighbourhood.
  "belong",
  "support"
)

# c(outcome_vars, 'id', 'wave'))
baseline_vars
baseline_vars <- sort(baseline_vars)

baseline_vars

# just core baseline variables
base_var <-
  setdiff(baseline_vars, outcome_vars)
base_var


#community at baseline
n_participants <-
  n_unique(dat_long$id) #47939 # reports hours with
n_participants


#margot::here_save(n_participants, "n_participants")

# double check path
push_mods

# check col names
colnames(dat)

# assess positivity
dat_long$wave

dt_positivity_19 <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  select(wave, id, nation_is_nz,year_measured) |> 
  filter(year_measured == 1) |> 
  droplevels()

n_unique(dt_positivity_19$id)

dt_positivity_20 <- dat_long |>
  filter(wave == 2019 | wave == 2020) |>
  select(wave, id, nation_is_nz,year_measured) |> 
  filter(year_measured == 1)|> 
  droplevels()

n_unique(dt_positivity_20$id)




dt_positivity_21 <- dat_long |>
  filter(wave == 2020 | wave == 2021) |>
  select(wave, id, nation_is_nz,year_measured) |> 
  filter(year_measured == 1)|> 
  droplevels()

n_unique(dt_positivity_21$id)


dt_positivity_22 <- dat_long |>
  filter(wave == 2021 | wave == 2022) |>
  select(wave, id, nation_is_nz,year_measured) |> 
  filter(year_measured == 1)|> 
  droplevels()

n_unique(dt_positivity_22$id)



# create transition matrix
out_19 <-margot::create_transition_matrix(data = dt_positivity_19, state_var = "nation_is_nz", id_var = "id")
out_20 <-margot::create_transition_matrix(data = dt_positivity_20, state_var = "nation_is_nz", id_var = "id")
out_21 <-margot::create_transition_matrix(data = dt_positivity_21, state_var = "nation_is_nz", id_var = "id")
out_22 <-margot::create_transition_matrix(data = dt_positivity_22, state_var = "nation_is_nz", id_var = "id")
out_19
out_20
out_21
out_22

# t_tab_2_labels <- c("< weekly", ">= weekly")
# transition table

transition_table_19  <- margot::transition_table(out_19)


transition_table_20  <- margot::transition_table(out_20)

transition_table_21   <- margot::transition_table(out_21)

transition_table_22   <- margot::transition_table(out_22)


transition_table_19
transition_table_20
transition_table_21
transition_table_22

transition_table_19$table
transition_table_20$table
transition_table_21$table
transition_table_22$table

# for import later
margot::here_save(transition_table, "transition_table")



out_church_zero <- margot::create_transition_matrix(data = dt_positivity_full, state_var = "religion_church_shift_zero", id_var = "id")


t_tab_2_labels_zero <- c("0", "> 0")

transition_table_binary_zero <-
  margot::transition_table(out_church_zero,
                           state_names = t_tab_2_labels_zero)

transition_table_binary_zero

# for import later
here_save(transition_table_binary_zero,
          "transition_table_binary_zero")



# transition table
out_church_gain <- margot::create_transition_matrix(data = dt_positivity_full, state_var = "religion_church_shift_gain", id_var = "id")


t_tab_2_labels_gain <- c(">=4", "< 4")

transition_table_binary_gain <-
  margot::transition_table(out_church_gain,
                           state_names = t_tab_2_labels_gain)

transition_table_binary_gain

# for import later
here_save(transition_table_binary_gain,
          "transition_table_binary_gain")


# sd values ---------------------------------------------------------------

dt_outcome <-
  dat_long |>
  filter(wave == 2020)


dt_outcome$religion_church_round
mean_donations <-
  mean(dt_outcome$charity_donate, na.rm = TRUE)
mean_volunteer <-
  mean(dt_outcome$hours_charity, na.rm = TRUE)


mean_donations
margot::here_save(mean_donations, "mean_donations")
mean_volunteer
margot::here_save(mean_volunteer, "mean_volunteer")

push_mods


sd_donations <-
  sd(dt_outcome$charity_donate, na.rm = TRUE)
sd_volunteer <-
  sd(dt_outcome$hours_charity, na.rm = TRUE)



# save for manuscript
here_save(sd_donations, "sd_donations")
here_save(sd_volunteer, "sd_volunteer")

# read
sd_donations <-
  here_read("sd_donations")
sd_volunteer <-
  here_read("sd_volunteer")


sd_donations
sd_volunteer

baseline_vars

# check
baseline_vars

#
# check associations only -------------------------------------------------

dt_18 <- dat_long|>
  filter(wave == 2018) 


table(dt_18$censored)



# check association only
#dt_18_miss$sample_weights

dt_18_miss <- dt_18 |> 
  mutate(hours_charity_z = scale(hours_charity))

naniar::vis_miss(dt_18_miss, warn_large_data = F)

table((dt_18_miss$hours_religious_community))
dev.off()

# base_vars set above
fit_church_on_charity_donate <-
  margot::regress_with_covariates(
    dt_18_miss,
    outcome = "charity_donate",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_church_on_charity_donate, ci_method="wald")[2, ] 
margot::here_save(fit_church_on_charity_donate, "fit_church_on_charity_donate")

#fit_church_on_hours_charity
#(0.198274  + 0.168511) * 60

fit_church_on_hours_charity <-
  margot::regress_with_covariates(
    dt_18_miss,
    outcome = "hours_charity",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_church_on_hours_charity, ci_method="wald")[2, ]
margot::here_save(fit_church_on_hours_charity, "fit_church_on_hours_charity")

fit_church_on_community_time_binary <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "community_time_binary",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    family = "poisson"
  )
parameters::model_parameters(fit_church_on_community_time_binary, ci_method="wald")[2, ]
here_save(fit_church_on_community_time_binary, "fit_church_on_community_time_binary")


fit_church_on_community_money_binary <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "community_money_binary",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    family = "poisson"
  )
parameters::model_parameters(fit_church_on_community_money_binary, ci_method="wald")[2, ]

margot::here_save(fit_church_on_community_money_binary, "fit_church_on_community_money_binary")

fit_church_on_charity_donate<- margot::here_read('fit_church_on_charity_donate')
fit_church_on_hours_charity<- margot::here_read('fit_church_on_hours_charity')
fit_church_on_community_time_binary<- margot::here_read('fit_church_on_community_time_binary')
fit_church_on_community_money_binary<- margot::here_read('fit_church_on_community_money_binary')


# run once then comment out

# lm_coef_fit_church_on_charity_donate <- tbl_regression(fit_church_on_charity_donate)
# b_church_on_charity_donate <-inline_text(lm_coef_fit_church_on_charity_donate, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")
# # # #
# b_church_on_charity_donate
# here_save(b_church_on_charity_donate, "b_church_on_charity_donate")
# 
# lm_coef_fit_church_on_hours_charity <- tbl_regression(fit_church_on_hours_charity)
# lm_coef_fit_church_on_hours_charity
# b_church_on_hours_charity <-inline_text(lm_coef_fit_church_on_hours_charity, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")
# b_church_on_hours_charity
# # b_church_on_charity_donate
# here_save(b_church_on_hours_charity, "b_church_on_hours_charity")

# tables ------------------------------------------------------------------
library(gtsummary)

# table baseline ----------------------------------------------------------
# get names

selected_base_cols <-
  dt_18 |> select(all_of(base_var))

#check
colnames(selected_base_cols)

#chck
#selected_base_cols <- setdiff(selected_base_cols)
# baseline table

library(gtsummary)
table_baseline <- selected_base_cols |> 
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
  modify_header(label = "**Exposure + Demographic Variables**") |> # update the column header
  bold_labels() 



table_baseline
# save baseline
here_save(table_baseline, "table_baseline")
# 

# table exposure ----------------------------------------------------------

# get first and second wave
dt_18_19 <- dat_long_full |> 
  dplyr::filter(wave == 2018 | wave == 2019) |> 
  droplevels()

# get vars.
selected_exposure_cols <-
  dt_18_19 %>% select(
    c(
      "religion_church_round",
      "alert_level_combined",
      "wave"
    )
  )

# check
str(selected_exposure_cols)


library(gtsummary)

table_exposures <- selected_exposure_cols %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Exposure Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_exposures


# save baseline
here_save(table_exposures, "table_exposures")

table_exposures


# outcome table -----------------------------------------------------------
dt_18_20 <- dat_long_full |> 
  dplyr::filter(wave == 2018 | wave == 2020) |> 
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_18_20)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

names_outcomes_final

names_outcomes_final



# names_outcomes_final
# better names
selected_outcome_cols <-
  dt_18_20 %>% select(all_of(names_outcomes_final),
                      wave) |>
  mutate(Volunteers_binary = factor(ifelse(hours_charity > 0, "yes", "no"),
                                    levels = c("no", "yes"))) |> 
  rename(
    Social_belonging = belong,
    Annual_charity = charity_donate,
    Volunteering_hours = hours_charity,
    Community_gives_money_binary = community_money_binary,
    Community_gives_time_binary = community_time_binary,
    Family_gives_money_binary = family_money_binary,
    Family_gives_time_binary = family_time_binary,
    Friends_give_money_binary = friends_money_binary,
    Friends_give_time = friends_time_binary,
    Social_support = support,
    Sense_neighbourhood_community = neighbourhood_community
  )

# order names correctly
selected_outcome_cols <- selected_outcome_cols %>%
  select(sort(names(selected_outcome_cols)))

# checks
str(selected_outcome_cols)
colnames(selected_outcome_cols)

table_outcomes <- selected_outcome_cols %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_outcomes

here_save(table_outcomes, "table_outcomes")
table_outcomes <- here_read("table_outcomes")
table_outcomes
# histogram exposure ------------------------------------------------------

dt_19 <- dat_long |> 
  filter(wave == 2019)

library(ggplot2)
library(dplyr)
#
# # generate bar plot
graph_density_of_exposure_up <- margot::coloured_histogram_shift(
  dt_19,
  col_name = "religion_church_round",
  binwidth = .5, 
  range_highlight = c(0,3.9)
)
graph_density_of_exposure_up


graph_density_of_exposure_down <- margot::coloured_histogram_shift(
  dt_19,
  shift = "down",
  col_name = "religion_church_round",
  binwidth = .5, 
  range_highlight = c(1,8)
)
graph_density_of_exposure_up

graph_density_of_exposure_down

here_save(graph_density_of_exposure_up, "graph_density_of_exposure_up")
here_save(graph_density_of_exposure_down, "graph_density_of_exposure_down")
#
# graph_density_of_exposure
#
# here_save(graph_density_of_exposure, "graph_density_of_exposure")
# ggsave(
#   graph_density_of_exposure_up,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "graph_density_of_exposure_up.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
# 
# ggsave(
#   graph_density_of_exposure_down,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "graph_density_of_exposure_down.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
# 



# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
# source("/Users/joseph/GIT/templates/functions/funs.R")

# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )
# # devtools::install_github("go-bayes/margot")


# impute baseline ---------------------------------------------------------
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome
#
#devtools::install_github("go-bayes/margot")

dat_long$sample_weights
dat_long_df <- data.frame(dat_long)

my_data_filtered <- as.data.frame(dat_long_df)
my_data_filtered <- haven::zap_formats(dat_long_df)
my_data_filtered <- haven::zap_label(dat_long_df)
my_data_filtered <- haven::zap_widths(dat_long_df)

# needs to be a data frame
dat_long_df <- data.frame(dat_long)

# check before committing
colnames(dat_long_df)
baseline_vars
prep_coop_all <-
  margot_wide_impute_baseline(
    dat_long_df,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars
  )



# return variables to the imputed data frame
# sample weights
prep_coop_all$t0_sample_weights <- dt_18$sample_weights

# alert_level
prep_coop_all$t0_alert_level_combined_lead <- dt_18$alert_level_combined_lead

prep_coop_all$t0_sample_weights
prep_coop_all$t0_alert_level_combined
prep_coop_all$t1_alert_level_combined

margot::here_save(prep_coop_all, "prep_coop_all")

# save function -- will save to your "push_mod" directory
margot::here_save(prep_coop_all, "prep_coop_all")

# Warning messages:
#   1: Number of logged events: 190 
# 2: Using an external vector in selections was deprecated in tidyselect 1.1.0.
# ℹ Please use `all_of()` or `any_of()` instead.
# # Was:
# data %>% select(t0_column_order)
# 
# # Now:
# data %>% select(all_of(t0_column_order))
# 
# See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.








# spit shine --------------------------------------------------------------


df_wide_censored <- prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen)
  ) |>
  relocate("t0_censored", .before = starts_with("t1_")) |>
  relocate("t1_censored", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

# check
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)




# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
#t1_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0

# check
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)


# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <-
  rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
# t1_na_condition <-
#   rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0
df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  # mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  select(-c(ends_with("_volunteers_binary"))) |> 
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        # !t0_nzsei_13_l & 
        !t0_sample_weights & 
        !t0_rural_gch_2018_l & 
        # !t0_nzsei_13_l& 
        !t0_sample_frame_opt_in & 
        # !t0_total_siblings & 
        # !t0_volunteers_binary &
        !t0_family_time_binary &
        !t0_friends_time_binary &
        !t0_community_time_binary &
        !t0_family_money_binary &
        !t0_friends_money_binary &
        !t0_community_money_binary &
        !t0_religion_church_round &
        !t0_alert_level_combined_lead &
        #  !t0_charity_donate & !t0_sample_weights &
        !t1_religion_church_round &
        #    !t1_alert_level_combined &
        !t1_censored &
        # !t2_charity_donate &!t2_volunteers_binary &
        !t2_family_time_binary &
        !t2_friends_time_binary &
        !t2_community_time_binary &
        !t2_family_money_binary &
        !t2_friends_money_binary &
        !t2_community_money_binary,
      #  !t2_hours_charity,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_sample_weights,
    t0_rural_gch_2018_l,
    #  t0_nzsei_13_l,
    t0_sample_frame_opt_in,
    t0_alert_level_combined_lead,
    #  t0_volunteers_binary,
    t0_family_time_binary,
    t0_friends_time_binary,
    t0_community_time_binary,
    t0_family_money_binary,
    t0_friends_money_binary,
    t0_community_money_binary,
    t0_sample_weights,
    # t0_total_siblings,
    t0_religion_church_round,
    t0_censored,
    #  t1_alert_level_combined,
    t1_religion_church_round,
    #t1_hours_community_round,
    t1_censored,
    # t2_charity_donate,
    # t2_hours_charity,
    # t2_volunteers_binary,
    t2_family_time_binary,
    t2_friends_time_binary,
    t2_community_time_binary,
    t2_family_money_binary,
    t2_friends_money_binary,
    t2_community_money_binary,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t0_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))


#check
naniar::vis_miss(df_clean, warn_large_data = FALSE)


# checks
table(df_clean$t1_lost)
table(df_clean$t0_lost)

test <- df_wide_censored |> filter(t0_censored == 1)
nrow(test)

table(df_clean$t0_censored)

test <- df_wide_censored |> filter(t0_censored == 1)
nrow(test)

# 
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)
str( df_clean )

nrow(df_clean)

# checks
naniar::vis_miss(df_clean, warn_large_data = FALSE)


here_save(df_clean, "df_clean")
# read data --  start here if previous work already done


# imputed data already from previous study
df_clean <- here_read("df_clean")
colnames(df_clean)
str(df_clean)
# names of vars for modelling


names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     #  -t0_volunteers_binary, # redundant
                     -t0_censored) |> colnames()

names_base

here_save(names_base, "names_base")
names_base <- here_read("names_base")

push_mods
names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

here_save(names_outcomes, "names_outcomes")
names_outcomes <- here_read("names_outcomes")

names_base
df_clean$t0_sample_weights



# weights for treament ----------------------------------------------------
df_clean <- here_read("df_clean")


baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"),-t0_censored, -t0_lost, -t0_sample_weights, -t0_alert_level_combined_lead, - t0_sample_weights)|> colnames() # note, we ear

# check
baseline_vars_models


# clean vars
df_clean_pre <- df_clean[baseline_vars_models]

df_clean_pre$t0_rural_gch_2018_l <- as.factor(df_clean_pre$t0_rural_gch_2018_l)
levels(df_clean_pre$t0_sample_origin)

str(df_clean_pre)

# perform one-hot encoding using model.matrix
encoded_vars <- model.matrix(~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l  - 1, data = df_clean_pre)

# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)


# make better names
encoded_df <- encoded_df %>% 
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe
# ensure to remove original categorical variables to avoid duplication
df_clean_hot_code <- df_clean %>%
  select(-c(id, t0_education_level_coarsen, t0_eth_cat, t0_sample_origin, t0_rural_gch_2018_l, t0_rural_gch_2018_l, t0_alert_level_combined_lead)) %>%
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)


# combine with base list of predictors
baseline_vars_set <- setdiff(names(df_clean_pre), c("t0_lost", "id", "t0_education_level_coarsen", "t0_eth_cat", "t0_rural_gch_2018_l", "t0_sample_origin"))

# Add the new encoded column names
full_predictor_vars <- c(baseline_vars_set, new_encoded_colnames)


full_predictor_vars

# no factors
str(df_clean_hot_code)


cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


library(doParallel)
library(SuperLearner)


# parallel backend
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


str(df_clean_hot_code[full_predictor_vars])

match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")


sl <- SuperLearner(
  Y = df_clean_hot_code$t0_lost, 
  X = df_clean_hot_code[full_predictor_vars],  # Use all specified predictors
  SL.library = match_lib,
  family = binomial(), 
  method = "method.NNloglik", 
  cvControl = list(V = 10)
)
here_save(sl, "sl")

# stop the cluster
stopCluster(cl)


# CHECKS 
print(sl)                  # Prints the summary of the SuperLearner output
summary(sl)                # Provides a detailed summary, including cross-validated risks

# For detailed examination of cross-validated performance
sl$cvRisk                  # Cross-validated risks for each learner
sl$coef                    # Weights assigned to each learner in the final ensemble



# Generate predictions
predictions <- predict(sl, newdata = df_clean_hot_code[full_predictor_vars], type = "response")

# Extract predictions from the 'pred' component and ensure it's a vector
df_clean_hot_code$pscore <- predictions$pred[, 1]

# Check the structure of the predictions
str(predictions)


# IPCW
df_clean_hot_code$weights <- ifelse(df_clean_hot_code$t0_lost == 1, 1 / df_clean_hot_code$pscore, 1 / (1 - df_clean_hot_code$pscore))


# check 
hist(df_clean_hot_code$weights)
min(df_clean_hot_code$weights)
max(df_clean_hot_code$weights)
# obtain stabalizing var (no need)
#df_clean_hot_code <- mean(df_clean_hot_code$t0_lost)

# stabalized weights
# #df_clean_hot_code$weights_stabilized <- ifelse(df_clean_hot_code$t0_lost == 1, 
#                                   marginal_censored / df_clean_hot_code$pscore, 
#                                   (1 - marginal_censored) / (1 - df_clean_hot_code$pscore))






#
head(df_clean_hot_code)

df_clean

# new weights
df_clean$t0_combo_weights = df_clean_hot_code$weights * df_clean$t0_sample_weights
df_clean$t0_alert_level_combined_lead <- df_clean$t0_alert_level_combined_lead

min( df_clean$t0_combo_weights)
max( df_clean$t0_combo_weights)

hist( df_clean$t0_combo_weights)

df_clean_t1 <- df_clean |> filter(t0_lost == 0)


hist( df_clean_t1$t0_combo_weights )

max(( df_clean_t1$t0_combo_weights ))
min(df_clean_t1$t0_combo_weights)

#
nrow(df_clean_t1)

table(is.na(df_clean_t1$t1_religion_church_round)) # 33198

# gets us the correct df for weights
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)

# nrow full
nrow(test)

# correct
nrow(df_clean_t1)

# next get data for t1
hist(df_clean_t1$t0_combo_weights)

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

#
here_save(df_clean_t2, "df_clean_t2")

# START HERE --------------------------------------------------------------
# read data --  start here if previous work already done
df_clean_t2 <- here_read("df_clean_t2")

colnames(df_clean_t2)
str(df_clean_t2)
# names of vars for modelling

names_base <-
  df_clean_t2 |> select(starts_with("t0"),
                        -t0_sample_weights, -t0_combo_weights,
                        -t0_censored) |> colnames()

names_base


names_outcomes <-
  df_clean_t2 |> select(starts_with("t2")) |> colnames()

names_outcomes

table( is.na( df_clean_t2$t0_alert_level_combined ))

names(df_clean_t2)
# explan names

names_base

df_final_base  <- df_clean_t2[names_base]


df_final_base$t0_rural_gch_2018_l <- as.factor(df_final_base$t0_rural_gch_2018_l)

# perform one-hot encoding using model.matrix
encoded_vars_final <- model.matrix(~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l  +  t0_alert_level_combined_lead - 1, data = df_final_base)

# convert matrix to data frame
encoded_vars_final <- as.data.frame(encoded_vars_final)

encoded_vars_final

# make better names
encoded_vars_final <- encoded_vars_final %>% 
  janitor::clean_names()

# View the first few rows to confirm structure
head(encoded_vars_final)

# bind the new one-hot encoded variables back to the original dataframe
# ensure to remove original categorical variables to avoid duplication
df_clean_t2_hot_code <- df_clean_t2 %>%
  select(-c(t0_education_level_coarsen, t0_eth_cat, t0_sample_origin, t0_rural_gch_2018_l, t0_alert_level_combined_lead)) %>%
  bind_cols(encoded_vars_final)

# extract and print the new column names for encoded variables
new_encoded_colnames_final <- colnames(encoded_vars_final)
print(new_encoded_colnames_final)


#  base list of predictors
baseline_vars_set_final <- setdiff(names(df_final_base), c("t0_lost", "id", "t0_education_level_coarsen", "t0_eth_cat", "t0_religion_church_round", "t0_rural_gch_2018_l", "t0_sample_origin", "t0_alert_level_combined_lead"))

# Add the new encoded column names
full_predictor_vars_final <- c(baseline_vars_set_final, new_encoded_colnames_final)


df_clean_t2_hot_code <-  df_clean_t2_hot_code |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

colnames(df_clean_t2_hot_code)
#cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


library(SuperLearner)



#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)
library(future)
library(SuperLearner)
plan(multisession)





# matching ----------------------------------------------------------------


# check imbalance ---------------------------------------------------------

df_clean_no_na_treatment_one <- df_clean_t2_hot_code |> filter(!is.na(t1_religion_church_round))

# df_clean_no_na_treatment_base <- df_clean |> filter(!is.na(t0_religion_church_round)) 
# 
# 
# 
df_clean_t2_hot_code
names_base_base <-df_clean_t2_hot_code |> select(starts_with("t0_"), -t0_sample_weights, -t0_censored, -starts_with("t0_alert_level_combined")) |> colnames()

names_base_base
# names_base_base
# 
# 
# match_ebal_base<- margot::match_mi_general(data = df_clean_no_na_treatment_base,
#                                            X = "t0_religion_church_round",
#                                            baseline_vars = names_base_base,
#                                            estimand = "ATE",
#                                            #  focal = 0, #for ATT
#                                            method = "ebal",
#                                            sample_weights = "t0_sample_weights")
# 
# love_plot_base <- love.plot(match_ebal_base, binary = "std", thresholds = c(m = .1),
#                             wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-.5, 1))) 

# love_plot_base
# love_plot_base
# here_save(love_plot_base, "love_plot_base")

# summary_match_ebal_base <- summary(match_ebal_base)
# summary_match_ebal_base
# here_save(summary_match_ebal, "summary_match_ebal")


match_ebal_one<- match_mi_general(data = df_clean_t2_hot_code,
                                  X = "t1_religion_church_round",
                                  baseline_vars = names_base_base,
                                  estimand = "ATE",
                                  #  focal = 0, #for ATT
                                  method = "ebal",
                                  sample_weights = "t0_sample_weights")

match_ebal_one
# save
here_save(match_ebal_one, "match_ebal_one")



bal.tab(match_ebal_one)
love_plot_one <- love.plot(match_ebal_one, binary = "std", thresholds = c(m = .1),
                           wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-.5, 1))) 
love_plot_one
here_save(love_plot_one, "love_plot_one")

# consider results 
summary_match_ebal_one <- summary(match_ebal_one)
summary_match_ebal_one
here_save(summary_match_ebal_one, "summary_match_ebal_one")

plot(summary_match_ebal)



# For trimmed weights e.g.
# #trim if needed (weights > 10 might be a problem)
# match_ebal_trim <- WeightIt::trim(match_ebal_one, at = .99)
# #bal.tab(match_ebal_trim_health)
# summary_match_ebal_trim<- summary(match_ebal_trim)
# plot(summary_match_ebal_trim)
# 















# sw$Nation.T10
# value                                                  label
# 0                                 Inadequately Described
# 1                                                 At Sea
# 4                                    Passengers' Effects
#      5                                          Ships' Stores
# 6                                       Ships' Bunkering
#      8                               Destination Unknown - EU
#      9                           Destination Unknown - Non-EU
#   1000           Oceania and Antarctica (not further defined)
#   1101                                              Australia
#   1102                                         Norfolk Island
#   1199                    Australian External Territories nec
#   1201                                            New Zealand
#   1300                        Melanesia (not further defined)
#   1301                                          New Caledonia
#   1302                                       Papua New Guinea
#   1303                                        Solomon Islands
#   1304                                                Vanuatu
#   1401                                                   Guam
#   1402                                               Kiribati
#   1403                                       Marshall Islands
#   1404                        Micronesia, Federated States of
#   1405                                                  Nauru
#   1406                               Northern Mariana Islands
#   1407                                                  Palau
#   1500      Polynesia (excludes Hawaii) (not further defined)
#   1501                                           Cook Islands
#   1502                                                   Fiji
#   1503                                       French Polynesia
#   1504                                                   Niue
#   1505                                                  Samoa
#   1506                                        Samoa, American
#   1507                                                Tokelau
#   1508                                                  Tonga
#   1511                                                 Tuvalu
#   1512                                      Wallis and Futuna
#   1513                                        Pitcairn Island
#   1599                        Polynesia (excludes Hawaii) nec
#   1601                                             Antarctica
#   2000                North-West Europe (not further defined)
#   2100                   United Kingdom (not further defined)
#   2101                                        Channel Islands
#   2102                                                England
#   2103                                            Isle of Man
#   2104                                       Northern Ireland
#   2105                                               Scotland
#   2106                                                  Wales
#   2201                                                Ireland
#   2300                   Western Europe (not further defined)
#   2301                                                Austria
#   2302                                                Belgium
#   2303                                                 France
#   2304                                                Germany
#   2305                                          Liechtenstein
#   2306                                             Luxembourg
#   2307                                                 Monaco
#   2308                                            Netherlands
#   2311                                            Switzerland
#   2400                  Northern Europe (not further defined)
#   2401                                                Denmark
#   2402                                         Faeroe Islands
#   2403                                                Finland
#   2404                                              Greenland
#   2405                                                Iceland
#   2406                                                 Norway
#   2407                                                 Sweden
#   3000      Southern and Eastern Europe (not further defined)
#   3100                  Southern Europe (not further defined)
#   3101                                                Andorra
#   3102                                              Gibraltar
#   3103                                     Vatican City State
#   3104                                                  Italy
#   3105                                                  Malta
#   3106                                               Portugal
#   3107                                             San Marino
#   3108                                                  Spain
#   3200             South Eastern Europe (not further defined)
#   3201                                                Albania
#   3202                                 Bosnia and Herzegovina
#   3203                                               Bulgaria
#   3204                                                Croatia
#   3205                                                 Cyprus
#   3206          Former Yugoslav Republic of Macedonia (FYROM)
#   3207                                                 Greece
#   3208                                                Moldova
#   3211                                                Romania
#   3212                                               Slovenia
#   3214                                             Montenegro
#   3215                                                 Serbia
#   3216                                                 Kosovo
#   3217                                             Yugoslavia
#   3300                   Eastern Europe (not further defined)
#   3301                                                Belarus
#   3302                                         Czech Republic
#   3303                                                Estonia
#   3304                                                Hungary
#   3305                                                 Latvia
#   3306                                              Lithuania
#   3307                                                 Poland
#   3308                                                 Russia
#   3311                                               Slovakia
#   3312                                                Ukraine
#   3318                                         Czechoslovakia
#   4000 North Africa and the Middle East (not further defined)
#   4100                     North Africa (not further defined)
#   4101                                                Algeria
#   4102                                                  Egypt
#   4103                                                  Libya
#   4104                                                Morocco
#   4105                                                  Sudan
#   4106                                                Tunisia
#   4107                                         Western Sahara
#   4108                                   Spanish North Africa
#   4111                                            South Sudan
#   4200                      Middle East (not further defined)
#   4201                                                Bahrain
#   4202                         Gaza Strip/Palestine/West Bank
#   4203                                                   Iran
#   4204                                                   Iraq
#   4205                                                 Israel
#   4206                                                 Jordan
#   4207                                                 Kuwait
#   4208                                                Lebanon
#   4211                                                   Oman
#   4212                                                  Qatar
#   4213                                           Saudi Arabia
#   4214                                                  Syria
#   4215                                                 Turkey
#   4216                                   United Arab Emirates
#   4217                                                  Yemen
#   5000                  South-East Asia (not further defined)
#   5100         Mainland South-East Asia (not further defined)
#   5101                                                Myanmar
#   5102                                               Cambodia
#   5103                                                   Laos
#   5104                                               Thailand
#   5105                                               Viet Nam
#   5200         Maritime South-East Asia (not further defined)
#   5201                                      Brunei Darussalam
#   5202                                              Indonesia
#   5203                                               Malaysia
#   5204                                            Philippines
#   5205                                              Singapore
#   5206                                            Timor-Leste
#   6100                  North-East Asia (not further defined)
#   6101                            China, People's Republic of
# 6102              Hong Kong (Special Administrative Region)
# 6103                                                  Japan
# 6104                 Korea, Democratic People's Republic of
#   6105                                     Korea, Republic of
#   6106                  Macau (Special Administrative Region)
#   6107                                               Mongolia
#   6108                                                 Taiwan
#   7000        Southern and Central Asia (not further defined)
#   7100                    Southern Asia (not further defined)
#   7101                                             Bangladesh
#   7102                                                 Bhutan
#   7103                                                  India
#   7104                                               Maldives
#   7105                                                  Nepal
#   7106                                               Pakistan
#   7107                                              Sri Lanka
#   7200                     Central Asia (not further defined)
#   7201                                            Afghanistan
#   7202                                                Armenia
#   7203                                             Azerbaijan
#   7204                                                Georgia
#   7205                                             Kazakhstan
#   7206                                             Kyrgyzstan
#   7207                                             Tajikistan
#   7208                                           Turkmenistan
#   7211                                             Uzbekistan
#   8000                     The Americas (not further defined)
#   8100                 Northern America (not further defined)
#   8101                                                Bermuda
#   8102                                                 Canada
#   8103                                 St Pierre and Miquelon
#   8104                               United States of America
#   8200                    South America (not further defined)
#   8201                                              Argentina
#   8202                                                Bolivia
#   8203                                                 Brazil
#   8204                                                  Chile
#   8205                                               Colombia
#   8206                                                Ecuador
#   8207                                       Falkland Islands
#   8208                                          French Guiana
#   8211                                                 Guyana
#   8212                                               Paraguay
#   8213                                                   Peru
#   8214                                               Suriname
#   8215                                                Uruguay
#   8216                                              Venezuela
#   8299                                      South America nec
#   8300                  Central America (not further defined)
#   8301                                                 Belize
#   8302                                             Costa Rica
#   8303                                            El Salvador
#   8304                                              Guatemala
#   8305                                               Honduras
#   8306                                                 Mexico
#   8307                                              Nicaragua
#   8308                                                 Panama
#   8400                        Caribbean (not further defined)
#   8401                                               Anguilla
#   8402                                    Antigua and Barbuda
#   8403                                                  Aruba
#   8404                                                Bahamas
#   8405                                               Barbados
#   8406                                         Cayman Islands
#   8407                                                   Cuba
#   8408                                               Dominica
#   8411                                     Dominican Republic
#   8412                                                Grenada
#   8413                                             Guadeloupe
#   8414                                                  Haiti
#   8415                                                Jamaica
#   8416                                             Martinique
#   8417                                             Montserrat
#   8421                                            Puerto Rico
#   8422                                     St Kitts and Nevis
#   8423                                               St Lucia
#   8424                          St Vincent and the Grenadines
#   8425                                    Trinidad and Tobago
#   8426                               Turks and Caicos Islands
#   8427                                Virgin Islands, British
#   8428                          Virgin Islands, United States
#   8433                                                Curacao
#   8434                                St Maarten (Dutch Part)
#   9000               Sub-Saharan Africa (not further defined)
#   9100          Central and West Africa (not further defined)
#   9101                                                  Benin
#   9102                                           Burkina Faso
#   9103                                               Cameroon
#   9104                                             Cape Verde
#   9105                               Central African Republic
#   9106                                                   Chad
#   9107                                                  Congo
#   9108                  Congo, the Democratic Republic of the
#   9111                                          Côte d'Ivoire
# 9112                                      Equatorial Guinea
# 9113                                                  Gabon
# 9114                                                 Gambia
# 9115                                                  Ghana
# 9116                                                 Guinea
# 9117                                          Guinea - Bissau
# 9118                                                Liberia
# 9121                                                   Mali
# 9122                                             Mauritania
# 9123                                                  Niger
# 9124                                                Nigeria
# 9125                                  Sao Tome and Principe
# 9126                                                Senegal
# 9127                                           Sierra Leone
# 9128                                                   Togo
# 9200         Southern and East Africa (not further defined)
# 9201                                                 Angola
# 9202                                               Botswana
# 9203                                                Burundi
# 9204                                                Comoros
# 9205                                               Djibouti
# 9206                                                Eritrea
# 9207                                               Ethiopia
# 9208                                                  Kenya
# 9211                                                Lesotho
# 9212                                             Madagascar
# 9213                                                 Malawi
# 9214                                              Mauritius
# 9215                                                Mayotte
# 9216                                             Mozambique
# 9217                                                Namibia
# 9218                                                Reunion
# 9221                                                 Rwanda
# 9222                                              St Helena
# 9223                                             Seychelles
# 9224                                                Somalia
# 9225                                           South Africa
# 9226                                              Swaziland
# 9227                                               Tanzania
# 9228                                                 Uganda
# 9231                                                 Zambia
# 9232                                               Zimbabwe
# 9299                           Southern and East Africa nec
# 9998                                               Not sure
# 9999                                             Not Stated