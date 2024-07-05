# sept 30 2023
# joseph bulbulia : joseph.bulbulia@gmail.com
# outcome-wide-analysis-template


# reproducibility
set.seed(123)

# set path to save models
push_mods <-
  here::here('/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-jm-hours-work')

pull_path <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

# read data: note that you need use the arrow package in R
dat <- qs::qread(here::here(pull_path))

library(margot)
library(tidyverse)

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

# read data: note that you need use the arrow package in R


dat <- qs::qread(here::here(pull_path))


# zap haven labels
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)


colnames(dat)

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods <-   fs::path_expand("/Users/john/Dropbox/exercise_forgiveness_template/mods")
push_mods <-   fs::path_expand("/Users/kempthjo/Dropbox/exercise_forgiveness_template/mods")
push_mods


# total nzavs participants
n_total <- skimr::n_unique(dat$id)

# get comma in number
n_total <- prettyNum(n_total, big.mark = ",")

# check n total
n_total

# save n for manuscript
margot::here_save(n_total, "n_total")



# set exposure here
nzavs_exposure <- "hours_exercise"


# define exposures --------------------------------------------------------
# define exposure
A <- "t1_hours_exercise"

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("hours_exercise", "not_lost") #


# shift one pont up if under 6
# f_1 <- function (data, trt) data[[trt]] + 1

# #  move to 5 hours
f <- function(data, trt) {
  ifelse(data[[trt]] <= 5, 5,  data[[trt]])
}


# shift functions
#  decrease everyone by one point, contrasted with what they would be anyway.
# f <- function(data, trt) {
#   ifelse(data[[trt]] >= min_score + one_point_in_sd_units, data[[trt]] - one_point_in_sd_units,  min_score)
# }
# # 
# # f



# #  increase everyone by one point, contrasted with what they would be anyway.
# f_1 <- function(data, trt) {
#   ifelse(data[[trt]] <= max_score - one_point_in_sd_units, data[[trt]] + one_point_in_sd_units,  max_score)
# }




# see second function below

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 5

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
seed <- 0112358
set.seed(seed)

# set cores for estimation
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger",
            "SL.xgboost")

#Improve speed (if needed)
# sl_lib_args <- list(
#   SL.glmnet = list(nalpha = 5, nlambda = 20),
#   SL.ranger = list(num.threads = 4),
#   SL.xgboost = list(nthread = 4,
#                     nrounds = 50,
#                     early_stopping_rounds = 10,
#                     max_depth = 6,
#                     colsample_bytree = 0.8,
#                     subsample = 0.8,
#                     eta = 0.01,
#                     tree_method = 'hist')
# )

# superlearner libraries
library(SuperLearner)
library(ranger)
library(xgboost)
library(glmnet)

# boost speed
#SL.xgboost = list(tree_method = 'gpu_hist')


# check options
listWrappers()








# TEST SECTION ------------------------------------------------------------
name_exposure_raw = 'hours_exercise'

# Obtain baseline ids
ids_baseline<- dat %>%
  dplyr::filter(year_measured == 1, wave == 2020) %>%
  dplyr::filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
  pull(id)

# Exposure IDs
ids_exposure <- dat %>%
  dplyr::filter(year_measured == 1, wave == 2021) %>%
  dplyr::filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
  pull(id)

# Intersect IDs from 2018 and 2019 to ensure participation in both years
ids_baseline_exposure <- intersect(ids_baseline, ids_exposure) # not used



# filter the original dataset for these IDs three waves
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)
str(dat)





dat_long_test <- dat |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  dplyr::filter(id %in% ids_baseline_exposure &
                  wave %in% c(2020:2022)) |>
  arrange(id, wave)

length(unique(dat_long$id))
length(unique(dat_long_test$id))
# kessler 6 ---------------------------------------------------------------


dat_long <- dat |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  dplyr::filter(id %in% ids_baseline_exposure &
                  wave %in% c(2020:2022)) |>
  arrange(id, wave) |>
  mutate(forgiveness = 8 - vengeful_rumin) |> # reverse score veng rumination 
  rowwise(wave) |>
  mutate(power_no_control_composite = mean(c(
    power_self_nocontrol, power_others_control
  ), na.rm = TRUE)) |>
  mutate(kessler_latent_depression =  mean(
    c(kessler_depressed, kessler_hopeless, kessler_worthless),
    na.rm = TRUE
  )) |>
  mutate(kessler_latent_anxiety  = mean(c(
    kessler_effort, kessler_nervous, kessler_restless
  ), na.rm = TRUE)) |>
  ungroup() |>
  mutate(power_no_control_composite_reversed = 8 - power_no_control_composite) |>
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    # "edu",
    "sample_origin_names_combined",
    # Sample origin names combined
    #"alert_level_combined_lead",  not needed because all receive all levels by the point the outcome is measured
    # covid alert levels -> 2019-2020
    "education_level_coarsen",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    # 0 = female, 0.5 = neither female nor male, 1 = male.
    "age",
    "born_nz",
    "hlth_disability",
    #these four below are exercise-specific
    "loc_health",
    "emotion_regulation_out_control",
    "power_no_control_composite",
    "hlth_hlth_care_access",
    "self_control",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc",
    # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
    # see nzavs materials
    "nzsei13",
    # see nzavs materials
    "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "urban",
    # see NZAVS,
    "have_siblings",
    #Do you have siblings?
    "total_siblings",
    # sum siblings
    "number_sisters_older",
    #How many older sisters do you have?
    "number_sisters_younger",
    #	How many younger sisters do you have?
    "number_brothers_older",
    #	How many older brothers do you have?
    "number_brothers_younger",
    #	How many older brothers do you have?
    "children_num",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    #Hours - Looking after children
    "hours_work",
    #Hours - Working in paid employment
    "hours_housework",
    # Hours - Housework/cooking
    "hours_exercise",
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
    # see mini ipip6
    # I want people to know that I am an important person of high status,
    # I am an ordinary person who is no better than others.
    # I wouldn’t want people to treat me as though I were superior to them.
    # I think that I am entitled to more respect than the average person is
    # "sdo",
    # "rwa",
    # "brk_relationship",
    # "began_relationship",
    "religion_religious",
    # Do you identify with a religion and/or spiritual group?
    # "religion_religious_not",  # reverse this indicator
    "religion_identification_level",
    #How important is your religion to how you see yourself?"
    "religion_prayer",
    # How many times did you pray in the last week?
    "religion_scripture",
    # How many times did you read religious scripture in the last week?
    "religion_church",
    # How many times did you attend a church or place of worship in the last month?
    "religion_believe_spirit",
    #Do you believe in some form of spirit or lifeforce?
    "religion_believe_spirit",
    #inverse believe in god
    "religion_believe_god",
    #Do you believe in a God
    "religion_believe_god_not",
    #inverse believe in god
    "religion_spiritual_identification",
    #w8,w10,w12-13 "I identify as a spiritual person."
    "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    "w_gend_age_euro",
    # sample_weights.
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of peopl
    "modesty",
    # see above
    "vengeful_rumin",
    "forgiveness",
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    # "issue_same_sex_marriage", not in range
    "support",
    # three items as below
    # "support_help",
    # # 'There are people I can depend on to help me if I really need it.
    # "support_turnto",
    # # There is no one I can turn to for guidance in times of stress.
    # "support_rnoguidance",
    #There is no one I can turn to for guidance in times of stress.
    "family_time",
    "friends_time",
    "community_time",
    "family_money",
    "friends_money",
    "community_money",
    #Please estimate how much help you have received from the following sources in the last week?
    # Received help and support - hours
    # family
    # friends
    # others in my community
    # Received help and support - money
    # family
    # friends
    # others in my community
    # outcomewide,
    #w8,w10,w12-13 "I identify as a spiritual person."
    "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    # sample_weights
    "alcohol_frequency",
    #"How often do you have a drink containing alcohol?"
    "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    "hlth_bmi",
    # " What is your height? (metres)\nWhat is your weight? (kg)\nKg
    "hours_exercise",
    # Hours spent … exercising/physical activity
    "sfhealth",
    "sfhealth_your_health",
    # "In general, would you say your health is...
    "sfhealth_get_sick_easier_reversed",
    #\nI seem to get sick a little easier than other people.
    "sfhealth_expect_worse_health_reversed",
    #\nI expect my health to get worse." ****
    "hlth_sleep_hours",
    #During the past month, on average, how many hours of actual sleep did you get per night?
    "smoker",
    #Do you currently smoke?
    "hlth_fatigue",
    #During the last 30 days, how often did.... you feel exhausted?
    "rumination",
    "kessler6_sum",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    # During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
    "kessler_depressed",
    #During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
    "kessler_effort",
    #During the last 30 days, how often did.... you feel that everything was an effort?
    "kessler_hopeless",
    # During the last 30 days, how often did.... you feel hopeless?
    "kessler_nervous",
    #During the last 30 days, how often did.... you feel nervous?
    "kessler_restless",
    #During the last 30 days, how often did.... you feel restless or fidgety?
    "kessler_worthless",
    # During the last 30 days, how often did.... you feel worthless?
    "sexual_satisfaction",
    #  How satisfied are you with your sex life?
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "perfectionism",
    # # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
    # I am hardly ever satisfied with my performance.
    "power_no_control_composite",
    "power_self_nocontrol",
    "power_no_control_composite_reversed",
    # I do not have enough power or control over\nimportant parts of my life.
    "power_others_control",
    # Other people have too much power or control over\nimportant parts of my life
    "self_esteem",
    "selfesteem_satself",
    #  On the whole am satisfied with myself.
    "selfesteem_postiveself",
    # Take a positive attitude toward myself
    "selfesteem_failure_reversed",
    # Am inclined to feel that I am a failure.
    #  "self_control",
    "self_control_have_lots",
    #In general, I have a lot of self-control.
    "self_control_wish_more_reversed",
    #I wish I had more self-discipline.(r)
    "emotion_regulation_out_control",
    # When I feel negative emotions, my emotions feel out of control. w10 - w13
    "emotion_regulation_hide_neg_emotions",
    # When I feel negative emotions, I suppress or hide my emotions. w10 - w13
    "emotion_regulation_change_thinking_to_calm",
    # When I feel negative emotions, I change the way I think to help me stay calm. w10 - w13
    # "emp_work_life_balance",# I have a good balance between work and other important things in my life. # not measured at baseline
    # "respect_self",  #If they knew me, most NZers would respect what I have accomplished in life. Missing at T12
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
    #"pwi",
    "pwb_your_health",
    # #Your health.
    "pwb_your_relationships",
    # #Your personal relationships.
    "pwb_your_future_security",
    # #Your future security.
    "pwb_standard_living",
    #Your standard of living.
    "lifesat",
    "lifesat_satlife",
    # I am satisfied with my life.
    "lifesat_ideal",
    # In most ways my life is close to ideal.
    "lifemeaning",
    # average meaning_purpose, meaning_sense
    "meaning_purpose",
    # My life has a clear sense of purpose.
    "meaning_sense",
    # I have a good sense of what makes my life meaningful.
    "permeability_individual",
    #I believe I am capable, as an individual\nof improving my status in society.
    "impermeability_group",
    #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
    "neighbourhood_community",
    #I feel a sense of community with others in my local neighbourhood.
    "support",
    "support_help",
    # 'There are people I can depend on to help me if I really need it.
    "support_turnto",
    # There is no one I can turn to for guidance in times of stress.
    "support_noguidance_reversed",
    #There is no one I can turn to for guidance in times of stress.
    "belong",
    "belong_accept",
    #Know that people in my life accept and value me.
    "belong_routside_reversed",
    # Feel like an outsider.
    "belong_beliefs",
    # Know that people around me share my attitudes and beliefs.
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "nwi",
    # The economic situation in New Zealand./# The social conditions in New Zealand. # Business in New Zealand.
    "emp_job_sat",
    # How satisfied are you with your current job? #Eisenbarth, H., Hart, C. M., Zubielevitch, E., Keilor, T., Wilson, M. S., Bulbulia, J. A., Sibley, C. G., &
    #Sedikides, C. (in press). Aspects of psychopathic personality relate to lower subjective and objective professional success. Personality and Individual Differences, 186, 111340.
    "emp_job_secure",
    #only for employed people
    "emp_job_valued",
    "rural_gch2018",
    "hours_community",
    "hours_friends",
    "hours_family",
    "alert_level_combined_lead",
    "alert_level_combined",
    # Hours spent … socialising with family
    # Hours spent … socialising with friends
    # Hours spent … socialising with community groups
    # Hours spent … socialising with religious groups (only for religion only studies)
  ) |>
  # select variables
  # mutate(across(where(is.double), as.numeric)) |>
  mutate(
    hours_community_log = log(hours_community + 1),
    hours_friends_log = sqrt(hours_friends + 1),
    hours_family_log = sqrt(hours_family + 1)
  ) |>
  mutate(male = as.numeric(male)) |>
  mutate(total_siblings_factor = ordered(round(
    ifelse(total_siblings > 7, 7, total_siblings), 0
  ))) |>
  mutate(religion_prayer_binary = ifelse(religion_prayer > 0, 1, 0)) |>
  mutate(religion_church_binary = ifelse(religion_church > 0, 1, 0)) |>
  mutate(religion_church_f = ifelse(religion_church >= 21, 21, 0)) |>
  mutate(religion_scripture_binary = ifelse(religion_scripture > 0, 1, 0)) |>
  mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
  mutate(hours_community_round = round(ifelse(hours_community >= 24, 24, hours_community), 0)) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  # dplyr::filter((wave == 2020 & year_measured  == 1) |
  #                 (wave == 2021  &
  #                    year_measured  == 1) |
  #                 (wave == 2022)) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  # group_by(id) |>
  # ## MAKE SURE YOU HAVE ELIGIBILITY CRITERIA
  # dplyr::mutate(meets_criteria_baseline = ifelse(year_measured == 1 &
  #                                                  !is.na(!!sym(nzavs_exposure)), 1, 0)) |>  # using R lang #idk how to put in disability here so ill just put in as covariate
  # dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  # arrange(id) |>
  # filter((wave == 2020 & year_measured == 1) |
  #          (wave == 2021 & year_measured == 1) |
  #          (wave == 2022)) %>%
  # group_by(id) |>
  # mutate(k_18 = ifelse(wave == 2020 &
  #                        meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  # mutate(h_18 = mean(k_18, na.rm = TRUE)) %>%
  # mutate(k_19 = ifelse(wave == 2021 &
  #                        meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  # mutate(h_19 = mean(k_19, na.rm = TRUE)) %>%
  # dplyr::filter(h_18 > 0) |>  # hack to enable repeat of baseline
  # dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
  # ungroup() %>%
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  ungroup() |>
  dplyr::mutate(
    friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    household_inc_log = log(household_inc + 1),
    hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  dplyr::mutate(sample_origin = as.factor( sample_origin_names_combined)) |>  #shorter name
  arrange(id, wave) |>
  droplevels() |>
  # select(-h_18, -k_18, -h_19, -k_19) |>
  ungroup() %>%
  mutate(time = as.numeric(wave)) |>
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
  education_level_coarsen = as.integer(education_level_coarsen), 
  alcohol_frequency = as.numeric(alcohol_frequency),
  smoker = as.numeric(smoker)
) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()



# total nzavs participants
n_participants <- skimr::n_unique(dat_long$id)

n_participants <- prettyNum(n_participants, big.mark = ",")

n_participants # 27,194
# save n for manuscript
margot::here_save(n_participants, "n_participants")



# #zap haven labels
dat_long <- as.data.frame(dat_long)
dat_long <- haven::zap_formats(dat_long)
dat_long <- haven::zap_label(dat_long)
dat_long <- haven::zap_widths(dat_long)



# exploration -------------------------------------------------------------

# df_test <- dat_long %>% 
#   filter( hours_exercise < 10)
# 
# hist(df_test$hours_exercise)
# 
# table(dat_long$rumination)
# 
# 
# summary( 
#   lm( rumination ~ hours_exercise, data = df_test)
#   )


# name variables

baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  "sample_origin",
  "nz_dep2018",
  "nzsei13",
  "born_nz",
  "hlth_disability",
  "hlth_bmi",
  # "pwi", # pwi
  # "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  "support",
  "belong",
  #"total_siblings_factor",
  #  "smoker", # smoker
  "sfhealth",
  # "alcohol_frequency", measured with error
  # "alcohol_intensity",
  # "hours_family_log",
  # "hours_friends_log",
  # "hours_community_log",
  # "hours_community_sqrt_round",
  # "lifemeaning",
  "household_inc_log",
  # added: measured with error but OK for imputations
  "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban",
  "children_num",
  "hours_children_log",
  # new
  "hours_work_log",
  # new
  "hours_housework_log",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty",
  "self_control",
  "loc_health",
  "emotion_regulation_out_control",
  "power_no_control_composite",
  "hlth_hlth_care_access",
  "hlth_fatigue",
  "bodysat",
  # I want people to know that I am an important person of high status, I am an ordinary person who is no better than others. , I wouldn’t want people to treat me as though I were superior to them. I think that I am entitled to more respect than the average person is.
  # "religion_religious", # Do you identify with a religion and/or spiritual group?
  # "religion_identification_level", #How important is your religion to how you see yourself?"  # note this is not a great measure of virtue, virtue is a mean between extremes.
  #"religion_church_round",
  # "religion_religious", #
  "religion_spiritual_identification",
  "religion_identification_level",
  #  "religion_religious",
  "sample_weights"
  #"alert_level_combined_lead"
)


# check
baseline_vars

# check
exposure_var

# outcomes
outcome_vars = c(
  # health
  "alcohol_frequency",
  # health
  "alcohol_intensity",
  # health
  "hlth_bmi",
  # health
  #"hours_exercise_log",
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
  "vengeful_rumin",
  #ego
  ## Am satisfied with the appearance, size and shape of my body.
  # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
  "perfectionism",
  # # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
  # I am hardly ever satisfied with my performance.
  # "power_no_control_composite",
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
  #"emotion_regulation_out_control",
  # When I feel negative emotions, my emotions feel out of control. w10 - w13
  # "emotion_regulation_hide_neg_emotions",
  # When I feel negative emotions, I suppress or hide my emotions. w10 - w13
  # "emotion_regulation_change_thinking_to_calm",#,#, # When I feel negative emotions, I change the way I think to help me stay calm. w10 - w13
  # "emp_work_life_balance"# I have a good balance between work and other important things in my life.
  #"respect_self",
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
  #"permeability_individual",
  #I believe I am capable, as an individual\nof improving my status in society.
  #"impermeability_group",
  #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
  "neighbourhood_community",
  #I feel a sense of community with others in my local neighbourhood.
  "belong",
  "support"
)



exposure_var
baseline_vars <-
  setdiff(baseline_vars,
          c("id","wave", #"alert_level_combined_lead",
            "sample_weights"))

# c(outcome_vars, 'id', 'wave'))
baseline_vars
baseline_vars <- sort(baseline_vars)

baseline_vars

# just core baseline variables
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights",# "alert_level_combined_lead", 
                           outcome_vars))
base_var


dt_positivity_full <- dat_long |>
  filter(wave == 2020 | wave == 2021) |>
  select(wave, id, hours_exercise) |>
  mutate(hours_exercise_who = ifelse(hours_exercise >= 5, 1, 0),
         who_stops_exercise = ifelse(hours_exercise > 0, 1, 0))



# create transition matrix
out <-
  margot::create_transition_matrix(data=dt_positivity_full,  id_var = "id",
                           state_var = "hours_exercise_who")

labels_who_five <- c("<5 hours", "5+ hours")

transition_table  <- margot::transition_table(out, labels_who_five)
transition_table

# for import later
margot::here_save(transition_table, "transition_table")

out_0 <-
  margot::create_transition_matrix(data=dt_positivity_full,  id_var = "id",
                                   state_var = "who_stops_exercise")

labels_who_0 <- c("No exercise", "Exercise")


transition_table_0  <- margot::transition_table(out_0, labels_who_0)
transition_table_0

# for import later
margot::here_save(transition_table_0, "transition_table_0")


# sd values ---------------------------------------------------------------

dt_outcome <-
  dat_long |>
  filter(wave == 2020)


# dt_outcome$religion_church_round
# mean_donations <-
#   mean(dt_outcome$charity_donate, na.rm = TRUE)
# mean_volunteer <-
#   mean(dt_outcome$hours_charity, na.rm = TRUE)
# 
# 
# mean_donations
# margot::here_save(mean_donations, "mean_donations")
# mean_volunteer
# margot::here_save(mean_volunteer, "mean_volunteer")
# 
# push_mods
# 
# 
# sd_donations <-
#   sd(dt_outcome$charity_donate, na.rm = TRUE)
# sd_volunteer <-
#   sd(dt_outcome$hours_charity, na.rm = TRUE)
# 
# 
# 
# # save for manuscript
# here_save(sd_donations, "sd_donations")
# here_save(sd_volunteer, "sd_volunteer")
# 
# # read
# sd_donations <-
#   here_read("sd_donations")
# sd_volunteer <-
#   here_read("sd_volunteer")
#
# check associations only -------------------------------------------------

dt_baseline <- dat_long|>
  filter(wave == 2020)


# table(dt_baseline$censored)



# check association only
# #dt_baseline_miss$sample_weights

#missingness at baseline
dt_baseline_miss <- dt_baseline |>
  mutate(hours_exercise_z = scale(hours_exercise)) |>
  select(baseline_vars)

naniar::vis_miss(dt_baseline_miss, warn_large_data = F)

table((dt_baseline_miss$sfhealth))
dev.off()

# base_vars set above
# fit_exercise_on_sfhealth <-
#   margot::regress_with_covariates(
#     dt_baseline,
#     outcome = "sfhealth",
#     exposure = "hours_exercise",
#     baseline_vars = base_var,
#     sample_weights = "sample_weights"
#   )
# parameters::model_parameters(fit_exercise_on_sfhealth, ci_method="wald")[2, ]
# margot::here_save(fit_exercise_on_sfhealth, "fit_exercise_on_sfhealth")

#fit_church_on_hours_charity
#(0.198274  + 0.168511) * 60

# fit_exercise_on_body_satisfaction <-
#   margot::regress_with_covariates(
#     dt_baseline,
#     outcome = "bodysat",
#     exposure = "hours_exercise_z",
#     baseline_vars = base_var,
#     sample_weights = "sample_weights"
#   )
# parameters::model_parameters(fit_exercise_on_body_satisfaction, ci_method="wald")[2, ]
# margot::here_save(fit_exercise_on_body_satisfaction, "fit_exercise_on_body_satisfaction")
# 
# fit_exercise_on_sexual_satisfaction <-
#   margot::regress_with_covariates(
#     dt_baseline,
#     outcome = "sexual_satisfaction",
#     exposure = "hours_exercise_z",
#     baseline_vars = base_var,
#     family = "poisson"
#   )
# parameters::model_parameters(fit_exercise_on_sexual_satisfaction, ci_method="wald")[2, ]
# here_save(fit_exercise_on_sexual_satisfaction, "fit_exercise_on_sexual_satisfaction")
# 
# #warning on sleep being non-integers using poisson distribution
# fit_exercise_on_sleep <-
#   margot::regress_with_covariates(
#     dt_baseline,
#     outcome = "hlth_sleep_hours",
#     exposure = "hours_exercise_z",
#     baseline_vars = base_var,
#     #family = "poisson",
#     sample_weights = "sample_weights"
#   )
# parameters::model_parameters(fit_exercise_on_sleep, ci_method="wald")[2, ]
# 
# margot::here_save(fit_exercise_on_sleep, "fit_exercise_on_sleep")

# fit_church_on_charity_donate<- margot::here_read('fit_church_on_charity_donate')
# fit_church_on_hours_charity<- margot::here_read('fit_church_on_hours_charity')
# fit_church_on_community_time_binary<- margot::here_read('fit_church_on_community_time_binary')
# fit_church_on_community_money_binary<- margot::here_read('fit_church_on_community_money_binary')
# 

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
  dt_baseline |> select(all_of(base_var))

#check
colnames(selected_base_cols)

#chck
#selected_base_cols <- setdiff(selected_base_cols)
# baseline table

str(selected_base_cols)

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


gt::gtsave(as_gt(table_baseline), path = here::here(here::here(push_mods, "figs")), filename = "table_baseline.png")
dev.off()


table_baseline
# save baseline
here_save(table_baseline, "table_baseline")
# 

# all baselines


all_base_cols <-
  dt_baseline |> select(all_of(baseline_vars))


library(gtsummary)
table_all_baseline <- all_base_cols |> 
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
table_all_baseline

# table exposure ----------------------------------------------------------

# get first and second wave
dt_baseline_exposure <- dat_long |> 
  dplyr::filter(wave == 2020 | wave == 2021) |> 
  droplevels()

# get vars.
selected_exposure_cols <-
  dt_baseline_exposure %>% select(
    c(
      "hours_exercise",
      #"alert_level_combined",
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
    statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Exposure Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_exposures

gt::gtsave(as_gt(table_exposures), path = here::here(here::here(push_mods, "figs")), filename = "table_exposures.png")
dev.off()
# save baseline
here_save(table_exposures, "table_exposures")

table_exposures


# outcome table -----------------------------------------------------------
dt_baseline_outcome <- dat_long |> 
  dplyr::filter(wave == 2020 | wave == 2022) |> 
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_baseline_outcome)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

names_outcomes_final

names_outcomes_final
setdiff(outcome_vars,names_outcomes_final)
setdiff(unique(outcome_vars), outcome_vars)
outcome_vars
length(outcome_vars)
length(names_outcomes_final)

# names_outcomes_final
# better names
selected_outcome_cols <-
  dt_baseline_outcome %>% select(all_of(names_outcomes_final),
                      wave) |>
 rename(
    Social_belonging = belong,
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
    statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()
table_outcomes

gt::gtsave(as_gt(table_outcomes), path = here::here(here::here(push_mods, "figs")), filename = "table_outcomes.png")

here_save(table_outcomes, "table_outcomes")
table_outcomes <- here_read("table_outcomes")
table_outcomes
# histogram exposure ------------------------------------------------------



# make shift intervention data --------------------------------------------
dt_exposure <- dat_long |>
  filter(year_measured ==1 & wave == 2021) #|>
  #mutate(hours_exercise_z = scale(hours_exercise))


dt_exposure |> 
  select(hours_exercise) |> 
  filter(hours_exercise < 5) |> 
  count(n = n())
dt_exposure |> 
  select(hours_exercise) |> 
  filter(hours_exercise > 5) |> 
  count(n = n())

dt_exposure |> 
  select(hours_exercise) |> 
  filter(hours_exercise >=2  &  hours_exercise <=6) |> 
  count(n = n())


hist(dt_exposure$hours_exercise)
table(dt_exposure$hours_exercise)
dev.off()


# test --------------------------------------------------------------------

mean_exposure <- mean(dt_exposure$hours_exercise,
                      na.rm = TRUE)

# just to view, do not use in function
mean_exposure

# make sure to use the sd
min_score <- min(dt_exposure$hours_exercise, na.rm = TRUE)

max_score <- max(dt_exposure$hours_exercise, na.rm = TRUE)
max_score

sd_exposure <- sd(dt_exposure$hours_exercise,
                  na.rm = TRUE)
sd_exposure

one_point_in_sd_units <- 1/sd_exposure
one_point_in_sd_units

# half_sd <- sd_exposure / 2
# half_sd



#  increase everyone by one point, contrasted with what they would be anyway.
# only use this function for raw scores
#f_1
f <- function(data, trt) {
  ifelse(data[[trt]] <= 5, 5,  data[[trt]])
}

f


#not sure how much utility there is here, only like 1 person works out 80 hrs a week
graph_density_of_exposure_mean <- margot::coloured_histogram_shift(
  dt_exposure,
  range_highlight = c(0, 5),
  col_name = "hours_exercise",
  binwidth = .5
)
graph_density_of_exposure_mean
here_save(graph_density_of_exposure_mean, "graph_density_of_exposure_mean")

ggsave(
  graph_density_of_exposure_mean,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure_mean.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()

#
# # generate bar plot
graph_density_of_exposure_up <- margot::coloured_histogram(
  dt_exposure,
  col_name = "hours_exercise",
  highlight_range = "lowest",
  unit_of_change = .001,
  #scale_max = 30,
  binwidth = .5
)
graph_density_of_exposure_up

ggsave(
  graph_density_of_exposure_up,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure_up.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()

push_mods
margot::here_save(graph_density_of_exposure_up, "graph_density_of_exposure_up")


# impute baseline ---------------------------------------------------------
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome
#
#devtools::install_github("go-bayes/margot")

dat_long$sample_weights
dat_long_df <- data.frame(dat_long)

dat_long_df <- as.data.frame(dat_long_df)
dat_long_df <- haven::zap_formats(dat_long_df)
dat_long_df <- haven::zap_label(dat_long_df)
dat_long_df <- haven::zap_widths(dat_long_df)

# needs to be a data frame
dat_long_df <- data.frame(dat_long_df)
str(dat_long_df)

#if need to remove factors before imputation, don't think this is necessary

# this looks OK
df_test_0 <- dat_long_df |> filter(wave == 2020)
df_test <- df_test_0[baseline_vars]


vis_miss(df_test, warn_large_data = FALSE)

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

vis_miss(prep_coop_all, warn_large_data = FALSE)

# return variables to the imputed data frame
# sample weights
prep_coop_all$t0_sample_weights <- dt_baseline$sample_weights

# alert_level
#prep_coop_all$t0_alert_level_combined_lead <- dt_baseline$alert_level_combined_lead

prep_coop_all$t0_alert_level_combined
prep_coop_all$t0_alert_level_combined_lead
prep_coop_all$t1_alert_level_combined
prep_coop_all$t1_alert_level_combined_lead



# save function -- will save to your "push_mod" directory
margot::here_save(prep_coop_all, "prep_coop_all")
push_mods
prep_coop_all <- margot::here_read("prep_coop_all")
prep_coop_all
# arrange data for analysis -----------------------------------------------
# spit and shine
df_wide_censored <-
  prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_smoker_binary = as.integer(ifelse(t0_smoker > 0, 1, 0)),
    t2_smoker_binary = as.integer(ifelse(t2_smoker > 0, 1, 0)),
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_"))
#check
head(df_wide_censored)
dim(df_wide_censored)
str(df_wide_censored)

A
# spit and shine
df_clean <- df_wide_censored %>%
  mutate(t2_na_flag = rowSums(is.na(select(
    ., starts_with("t2_")
  ))) > 0) %>%
  mutate(t1_not_lost = ifelse(t2_na_flag, 0, t1_not_lost)) %>%
  # select(-t2_na_flag) %>%
  filter(!rowSums(is.na(select(
    ., starts_with("t0_")
  )))) |>
  dplyr::mutate(
    across(
      where(is.numeric) &
        !t0_not_lost &
        !t1_not_lost &
        !t0_sample_weights &
        !t0_hours_exercise &
        !t1_hours_exercise &
        !t0_smoker_binary,       
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    t0_smoker_binary,
    t0_not_lost,
    t0_sample_weights,
    t0_hours_exercise,
    t1_hours_exercise, # make sure to change for each study
    t1_not_lost,
    t2_smoker_binary,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  mutate(t0_sample_weights = as.numeric(t0_sample_weights)) |>
  data.frame()

dim(df_clean)

#NAs in missing columns instead of 0%, might be a problem
naniar::vis_miss(df_clean, warn_large_data = FALSE)
dev.off()


# again check path
push_mods
# save
here_save(df_clean, "df_clean")

# read if needed
df_clean <- here_read("df_clean")

str(df_clean)
df_clean <- as.data.frame(df_clean)

#check n
nrow(df_clean)

colnames(df_clean)
# get names
names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost,
                     -t0_smoker_z) |> colnames()

names_base

names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

colnames(df_clean)


# imbalance plot ----------------------------------------------------------

match_ebal_one<- match_mi_general(data = df_clean,
                                  X = "t1_hours_exercise",
                                  baseline_vars = names_base,
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
dev.off()
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




# set variables for models ------------------------------------------------

#### SET VARIABLE NAMES: Customise for each outcomewide model
#  model
A


C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))

# check
print(W)


# check shift
f

f_1 <- function(data, trt) {
  ifelse(data[[trt]] >= 0, 0,  data[[trt]])
}

#just make f_1 loss of exercise for everyone
f_1

# make test data (if needed)
df_clean_test <- df_clean |>
  slice_head(n = 2000)

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.

# test --------------------------------------------------------------------


names_base_t2_bodysat_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_bodysat_z")
# names_base_t2_bodysat_z
# 
# names_base_t2_reapeated_outcome_name <- names_base_t2_alcohol_frequency_z[-length(names_base_t2_alcohol_frequency_z)]
# names_base_t2_reapeated_outcome_name
## Am satisfied with the appearance, size and shape of my body.

t2_bodysat_z_test <- lmtp_tmle(
  data = df_clean_test,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_bodysat_z_test



# health models -----------------------------------------------------------


# smoker binary
#Do you currently smoke?

# select_and_rename_cols <- function(names_base, baseline_vars, outcome) {
#   # Select columns that match with baseline_vars
#   selected_cols <- names_base[grepl(paste(baseline_vars, collapse = "|"), names_base)]
#
#   # Rename the outcome variable prefix from t2 to t0
#   outcome_renamed <- gsub("t2_", "t0_", outcome)
#   # Append the renamed outcome to selected columns
#   final_cols <- c(selected_cols, outcome_renamed)
#
#   return(final_cols)
# }


names_base_t2_smoker_binary <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_smoker_binary")
names_base_t2_smoker_binary

timing_info <- system.time({
  t2_smoker_binary <- lmtp_tmle(
    data = df_clean,
    trt = A,
    baseline = names_base_t2_smoker_binary,
    outcome = "t2_smoker_binary",
    cens = C,
    shift = f,
    mtp = TRUE,
    folds = 5,
    # trim = 0.99, # if needed
    # time_vary = NULL,
    outcome_type = "binomial",
    #  id = "id",
    weights = df_clean$t0_sample_weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    parallel = n_cores
  )
})



t2_smoker_binary
here_save(t2_smoker_binary, "t2_smoker_binary")
# t2_smoker_binary <-here_read( "t2_smoker_binary")
# t2_smoker_binary

# print timing info
print(paste("Time taken: ", round(timing_info['elapsed'], 2), " seconds"))


timing_info <- system.time({
  t2_smoker_binary_1 <- lmtp_tmle(
    data = df_clean,
    trt = A,
    baseline = names_base_t2_smoker_binary,
    outcome = "t2_smoker_binary",
    cens = C,
    shift = f_1,
    mtp = TRUE,
    folds = 5,
    # trim = 0.99, # if needed
    # time_vary = NULL,
    outcome_type = "binomial",
    #  id = "id",
    weights = df_clean$t0_sample_weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    parallel = n_cores
  )
})



t2_smoker_binary_1
here_save(t2_smoker_binary_1, "t2_smoker_binary_1")


#Do you currently smoke?
t2_smoker_binary_null  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_smoker_binary,
  outcome = "t2_smoker_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_smoker_binary_null
here_save(t2_smoker_binary_null, "t2_smoker_binary_null")

#skipping smoking for now

names_base_t2_alcohol_frequency_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_alcohol_frequency_z")
names_base_t2_alcohol_frequency_z


#"How often do you have a drink containing alcohol?"

t2_alcohol_frequency_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_alcohol_frequency_z,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)



t2_alcohol_frequency_z
here_save(t2_alcohol_frequency_z, "t2_alcohol_frequency_z")



#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_alcohol_frequency_z,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)



t2_alcohol_frequency_z_1
here_save(t2_alcohol_frequency_z_1, "t2_alcohol_frequency_z_1")



#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_alcohol_frequency_z,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_alcohol_frequency_z_null
here_save(t2_alcohol_frequency_z_null, "t2_alcohol_frequency_z_null")




names_base_t2_alcohol_intensity_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_alcohol_intensity_z")
names_base_t2_alcohol_intensity_z

# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_alcohol_intensity_z,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_alcohol_intensity_z
here_save(t2_alcohol_intensity_z, "t2_alcohol_intensity_z")




# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_alcohol_intensity_z,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_alcohol_intensity_z_1
here_save(t2_alcohol_intensity_z_1, "t2_alcohol_intensity_z_1")



# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_alcohol_intensity_z,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_alcohol_intensity_z_null
here_save(t2_alcohol_intensity_z_null, "t2_alcohol_intensity_z_null")




# names_base_t2_sfhealth_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_sfhealth_z")
# names_base_t2_sfhealth_z

# "In general, would you say your health is...
# "I seem to get sick a little easier than other people."
# "I expect my health to get worse." ****

# t2_sfhealth_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_sfhealth_z,
#   outcome = "t2_sfhealth_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
#
# t2_sfhealth_z
# here_save(t2_sfhealth_z, "t2_sfhealth_z")
#
#
# names_base_t2_sfhealth_your_health_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_sfhealth_your_health_z")
# names_base_t2_sfhealth_your_health_z


# "In general, would you say your health is...
t2_sfhealth_your_health_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_sfhealth_your_health_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_sfhealth_your_health_z
here_save(t2_sfhealth_your_health_z, "t2_sfhealth_your_health_z")


# "In general, would you say your health is...
t2_sfhealth_your_health_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_sfhealth_your_health_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_sfhealth_your_health_z_1
here_save(t2_sfhealth_your_health_z_1, "t2_sfhealth_your_health_z_1")





# "In general, would you say your health is...
t2_sfhealth_your_health_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_sfhealth_your_health_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_sfhealth_your_health_z_null
here_save(t2_sfhealth_your_health_z_null,
          "t2_sfhealth_your_health_z_null")



# names_base_t2_hours_exercise_log_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hours_exercise_log_z")
# names_base_t2_hours_exercise_log_z
# 
# 
# # Hours spent … exercising/physical activity
# t2_hours_exercise_log_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_hours_exercise_log_z,
#   outcome = "t2_hours_exercise_log_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_hours_exercise_log_z
# here_save(t2_hours_exercise_log_z, "t2_hours_exercise_log_z")
# 
# # Hours spent … exercising/physical activity
# t2_hours_exercise_log_z_1 <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_hours_exercise_log_z,
#   outcome = "t2_hours_exercise_log_z",
#   cens = C,
#   shift = f_1,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_hours_exercise_log_z_1
# here_save(t2_hours_exercise_log_z_1, "t2_hours_exercise_log_z_1")
# 
# 
# 
# 
# # Hours spent … exercising/physical activity
# t2_hours_exercise_log_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_hours_exercise_log_z,
#   outcome = "t2_hours_exercise_log_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_hours_exercise_log_z_null
# here_save(t2_hours_exercise_log_z_null,
#           "t2_hours_exercise_log_z_null")




names_base_t2_hlth_sleep_hours_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_hlth_sleep_hours_z")
names_base_t2_hlth_sleep_hours_z


#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_hlth_sleep_hours_z,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
t2_hlth_sleep_hours_z
here_save(t2_hlth_sleep_hours_z, "t2_hlth_sleep_hours_z")



#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_hlth_sleep_hours_z,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
t2_hlth_sleep_hours_z_1
here_save(t2_hlth_sleep_hours_z_1, "t2_hlth_sleep_hours_z_1")




#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_hlth_sleep_hours_z,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_hlth_sleep_hours_z_null
here_save(t2_hlth_sleep_hours_z_null, "t2_hlth_sleep_hours_z_null")



# names_base_t2_hlth_bmi_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hlth_bmi_z")
# names_base_t2_hlth_bmi_z


# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_hlth_bmi_z
here_save(t2_hlth_bmi_z, "t2_hlth_bmi_z")



# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_hlth_bmi_z_1
here_save(t2_hlth_bmi_z_1, "t2_hlth_bmi_z_1")



# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_hlth_bmi_z_null
here_save(t2_hlth_bmi_z_null, "t2_hlth_bmi_z_null")



# embodied models ----------------------------------------------------------------


## Am satisfied with the appearance, size and shape of my body.

# names_base_t2_bodysat_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_bodysat_z")
# names_base_t2_bodysat_z


## Am satisfied with the appearance, size and shape of my body.

t2_bodysat_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_bodysat_z
here_save(t2_bodysat_z, "t2_bodysat_z")


## Am satisfied with the appearance, size and shape of my body.



t2_bodysat_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_bodysat_z_1
here_save(t2_bodysat_z_1, "t2_bodysat_z_1")


## Am satisfied with the appearance, size and shape of my body.
t2_bodysat_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_bodysat_z_null
here_save(t2_bodysat_z_null,
          "t2_bodysat_z_null")



# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?
# During the last 30 days, how often did.... you feel worthless?



# names_base_t2_kessler_latent_anxiety_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_kessler_latent_anxiety_z")
# 
# # check
# names_base_t2_kessler_latent_anxiety_z


# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?



t2_kessler_latent_anxiety_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_kessler_latent_anxiety_z
here_save(t2_kessler_latent_anxiety_z, "t2_kessler_latent_anxiety_z")




t2_kessler_latent_anxiety_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_kessler_latent_anxiety_z_1
here_save(t2_kessler_latent_anxiety_z_1, "t2_kessler_latent_anxiety_z_1")




#
# t2_kessler_latent_anxiety_z_clinical
# here_save(t2_kessler_latent_anxiety_z_clinical, "t2_kessler_latent_anxiety_z_clinical")

# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?

t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

# test
here_save(t2_kessler_latent_anxiety_z_null,
          "t2_kessler_latent_anxiety_z_null")


# depression

# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?


# names_base_t2_kessler_latent_depression_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_kessler_latent_depression_z")
# 
# # check
# names_base_t2_kessler_latent_depression_z


# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?

t2_kessler_latent_depression_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_kessler_latent_depression_z
here_save(t2_kessler_latent_depression_z,
          "t2_kessler_latent_depression_z")




t2_kessler_latent_depression_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_kessler_latent_depression_z_1
here_save(t2_kessler_latent_depression_z_1,
          "t2_kessler_latent_depression_z_1")






t2_kessler_latent_depression_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_kessler_latent_depression_z_null
here_save(t2_kessler_latent_depression_z_null,
          "t2_kessler_latent_depression_z_null")




# During the last 30 days, how often did.... you feel exhausted?

# names_base_t2_hlth_fatigue_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hlth_fatigue_z")
# names_base_t2_hlth_fatigue_z

# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


here_save(t2_hlth_fatigue_z, "t2_hlth_fatigue_z")

# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_hlth_fatigue_z_1
here_save(t2_hlth_fatigue_z_1, "t2_hlth_fatigue_z_1")




# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_hlth_fatigue_z_null
here_save(t2_hlth_fatigue_z_null, "t2_hlth_fatigue_z_null")



# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?


names_base_t2_rumination_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_rumination_z")
names_base_t2_rumination_z
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_rumination_z,
  outcome = "t2_rumination_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_rumination_z
here_save(t2_rumination_z, "t2_rumination_z")




# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_rumination_z,
  outcome = "t2_rumination_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_rumination_z_1
here_save(t2_rumination_z_1, "t2_rumination_z_1")




# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_rumination_z,
  outcome = "t2_rumination_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_rumination_z_null
here_save(t2_rumination_z_null, "t2_rumination_z_null")

t2_rumination_z <- here_read("t2_rumination_z")
t2_rumination_z_1 <- here_read("t2_rumination_z_1")
t2_rumination_z_null <- here_read("t2_rumination_z_null")

t2_rumination_z_1$fits_r
t2_rumination_z$fits_r

t2_rumination_z$fits_m

t2_rumination_z_null$fits_r

# ego models --------------------------------------------------------------



names_base_t2_sexual_satisfaction_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_sexual_satisfaction_z")
names_base_t2_sexual_satisfaction_z

#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_sexual_satisfaction_z,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_sexual_satisfaction_z
here_save(t2_sexual_satisfaction_z, "t2_sexual_satisfaction_z")


#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_sexual_satisfaction_z,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_sexual_satisfaction_z_1
here_save(t2_sexual_satisfaction_z_1, "t2_sexual_satisfaction_z_1")

#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_sexual_satisfaction_z,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_sexual_satisfaction_z_null
here_save(t2_sexual_satisfaction_z_null,
          "t2_sexual_satisfaction_z_null")



# # 
# names_base_t2_power_no_control_composite_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_power_no_control_composite_z")
# names_base_t2_power_no_control_composite_z
# 
# # I do not have enough power or control over\nimportant parts of my life.
# # Other people have too much power or control over\nimportant parts of my life.
# t2_power_no_control_composite_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_power_no_control_composite_z,
#   outcome = "t2_power_no_control_composite_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_power_no_control_composite_z
# here_save(t2_power_no_control_composite_z,
#           "t2_power_no_control_composite_z")
# 
# 
# t2_power_no_control_composite_z_1 <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_power_no_control_composite_z,
#   outcome = "t2_power_no_control_composite_z",
#   cens = C,
#   shift = f_1,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_power_no_control_composite_z_1
# here_save(t2_power_no_control_composite_z_1,
#           "t2_power_no_control_composite_z_1")
# 
# 
# 
# # I do not have enough power or control over\nimportant parts of my life.
# # Other people have too much power or control over\nimportant parts of my life.
# t2_power_no_control_composite_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_power_no_control_composite_z,
#   outcome = "t2_power_no_control_composite_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_power_no_control_composite_z_null
# here_save(t2_power_no_control_composite_z_null,
#           "t2_power_no_control_composite_z_null")
# 



names_base_t2_self_esteem_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_self_esteem_z")
names_base_t2_self_esteem_z


#  On the whole am satisfied with myself.
#  Take a positive attitude toward myself
#  Am inclined to feel that I am a failure.
t2_self_esteem_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_self_esteem_z,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_esteem_z
here_save(t2_self_esteem_z, "t2_self_esteem_z")



t2_self_esteem_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_self_esteem_z,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_esteem_z_1
here_save(t2_self_esteem_z_1, "t2_self_esteem_z_1")




#  On the whole am satisfied with myself.
#  Take a positive attitude toward myself
#  Am inclined to feel that I am a failure.
t2_self_esteem_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_self_esteem_z,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_esteem_z_null
here_save(t2_self_esteem_z_null, "t2_self_esteem_z_null")





# names_base_t2_self_control_have_lots_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_self_control_have_lots_z")
# names_base_t2_self_control_have_lots_z

# In general, I have a lot of self-control.
t2_self_control_have_lots_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_control_have_lots_z
here_save(t2_self_control_have_lots_z, "t2_self_control_have_lots_z")


t2_self_control_have_lots_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_control_have_lots_z_1
here_save(t2_self_control_have_lots_z_1, "t2_self_control_have_lots_z_1")




#In general, I have a lot of self-control.
t2_self_control_have_lots_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_control_have_lots_z_null
here_save(t2_self_control_have_lots_z_null,
          "t2_self_control_have_lots_z_null")





# names_base_t2_self_control_wish_more_reversed_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_self_control_wish_more_reversed_z")
# names_base_t2_self_control_wish_more_reversed_z


# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_control_wish_more_reversed_z
here_save(t2_self_control_wish_more_reversed_z,
          "t2_self_control_wish_more_reversed_z")



# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_control_wish_more_reversed_z_1
here_save(t2_self_control_wish_more_reversed_z_1,
          "t2_self_control_wish_more_reversed_z_1")



# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_self_control_wish_more_reversed_z_null
here_save(
  t2_self_control_wish_more_reversed_z_null,
  "t2_self_control_wish_more_reversed_z_null"
)






# 
# names_base_t2_permeability_individual_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_permeability_individual_z")
# names_base_t2_permeability_individual_z
# 
# # I believe I am capable, as an individual\nof improving my status in society.
# t2_permeability_individual_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_permeability_individual_z,
#   outcome = "t2_permeability_individual_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_permeability_individual_z
# here_save(t2_permeability_individual_z,
#           "t2_permeability_individual_z")
# 
# 
# 
# # I believe I am capable, as an individual\nof improving my status in society.
# t2_permeability_individual_z_1 <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_permeability_individual_z,
#   outcome = "t2_permeability_individual_z",
#   cens = C,
#   shift = f_1,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_permeability_individual_z_1
# here_save(t2_permeability_individual_z_1,
#           "t2_permeability_individual_z_1")
# 
# 
# 
# # I believe I am capable, as an individual\nof improving my status in society.
# t2_permeability_individual_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_permeability_individual_z,
#   outcome = "t2_permeability_individual_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_permeability_individual_z_null
# here_save(t2_permeability_individual_z_null,
#           "t2_permeability_individual_z_null")
# 
# 
# names_base_t2_emotion_regulation_out_control_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_emotion_regulation_out_control_z")
# names_base_t2_emotion_regulation_out_control_z
# 
# 
# # emotional regulation
# # When I feel negative emotions, my emotions feel out of control. w10 - w13
# t2_emotion_regulation_out_control_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_emotion_regulation_out_control_z,
#   outcome = "t2_emotion_regulation_out_control_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_emotion_regulation_out_control_z
# here_save(t2_emotion_regulation_out_control_z,
#           "t2_emotion_regulation_out_control_z")
# 
# 
# 
# # When I feel negative emotions, my emotions feel out of control. w10 - w13
# t2_emotion_regulation_out_control_z_1 <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_emotion_regulation_out_control_z,
#   outcome = "t2_emotion_regulation_out_control_z",
#   cens = C,
#   shift = f_1,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_emotion_regulation_out_control_z_1
# here_save(t2_emotion_regulation_out_control_z_1,
#           "t2_emotion_regulation_out_control_z_1")
# 
# 
# 
# 
# t2_emotion_regulation_out_control_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_emotion_regulation_out_control_z,
#   outcome = "t2_emotion_regulation_out_control_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# t2_emotion_regulation_out_control_z_null
# here_save(
#   t2_emotion_regulation_out_control_z_null,
#   "t2_emotion_regulation_out_control_z_null"
# )
# 

#
# Not relevant
# names_base_t2_impermeability_group_z <- select_and_rename_cols(names_base = names_base, baseline_vars = baseline_vars, outcome = "t2_impermeability_group_z")
# names_base_t2_impermeability_group_z
#
# # The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
# ##(NEG CONTROL)
# t2_impermeability_group_z<- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_impermeability_group_z,
#   outcome = "t2_impermeability_group_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
#
# t2_impermeability_group_z
# here_save(t2_impermeability_group_z, "t2_impermeability_group_z")
#
# # The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
# ##(NEG CONTROL)
# t2_impermeability_group_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_impermeability_group_z,
#   outcome = "t2_impermeability_group_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
#
# t2_impermeability_group_z_null
# here_save(t2_impermeability_group_z_null, "t2_impermeability_group_z_null")
#



names_base_t2_perfectionism_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_perfectionism_z")
names_base_t2_perfectionism_z

# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_perfectionism_z,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_perfectionism_z
here_save(t2_perfectionism_z, "t2_perfectionism_z")



# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_perfectionism_z,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_perfectionism_z_1
here_save(t2_perfectionism_z_1, "t2_perfectionism_z_1")






# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_perfectionism_z,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_perfectionism_z_null
here_save(t2_perfectionism_z_null, "t2_perfectionism_z_null")



# reflective models --------------------------------------------------------------


names_base_t2_gratitude_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_gratitude_z")
names_base_t2_gratitude_z



## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_gratitude_z,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_gratitude_z
here_save(t2_gratitude_z, "t2_gratitude_z")


## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_gratitude_z,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_gratitude_z_1
here_save(t2_gratitude_z_1, "t2_gratitude_z_1")



## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_gratitude_z,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_gratitude_z_null
here_save(t2_gratitude_z_null, "t2_gratitude_z_null")


# 
# 
# names_base_t2_vengeful_rumin_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_vengeful_rumin_z")
# names_base_t2_vengeful_rumin_z
# 
# 
# 
# 
# # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
# t2_vengeful_rumin_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_vengeful_rumin_z,
#   outcome = "t2_vengeful_rumin_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_vengeful_rumin_z
# here_save(t2_vengeful_rumin_z, "t2_vengeful_rumin_z")
# 
# 
# 
# 
# # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
# t2_vengeful_rumin_z_1 <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_vengeful_rumin_z,
#   outcome = "t2_vengeful_rumin_z",
#   cens = C,
#   shift = f_1,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_vengeful_rumin_z_1
# here_save(t2_vengeful_rumin_z_1, "t2_vengeful_rumin_z_1")
# 
# # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
# t2_vengeful_rumin_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_vengeful_rumin_z,
#   outcome = "t2_vengeful_rumin_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_vengeful_rumin_z_null
# here_save(t2_vengeful_rumin_z_null, "t2_vengeful_rumin_z_null")
# 
# 


names_base_t2_pwb_your_health_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_pwb_your_health_z")
names_base_t2_pwb_your_health_z


# Your health.
t2_pwb_your_health_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_health_z,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_health_z
here_save(t2_pwb_your_health_z, "t2_pwb_your_health_z")



# Your health.
t2_pwb_your_health_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_health_z,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_health_z_1
here_save(t2_pwb_your_health_z_1, "t2_pwb_your_health_z_1")



# Your health.
t2_pwb_your_health_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_health_z,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_pwb_your_health_z_null
here_save(t2_pwb_your_health_z_null, "t2_pwb_your_health_z_null")




names_base_t2_pwb_your_future_security_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_pwb_your_future_security_z")
names_base_t2_pwb_your_future_security_z


# #Your future security.
t2_pwb_your_future_security_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_future_security_z,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_future_security_z
here_save(t2_pwb_your_future_security_z,
          "t2_pwb_your_future_security_z")


# #Your future security.
t2_pwb_your_future_security_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_future_security_z,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_future_security_z_1
here_save(t2_pwb_your_future_security_z_1,
          "t2_pwb_your_future_security_z_1")



# #Your future security.
t2_pwb_your_future_security_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_future_security_z,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_future_security_z_null
here_save(t2_pwb_your_future_security_z_null,
          "t2_pwb_your_future_security_z_null")



names_base_t2_pwb_your_relationships_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_pwb_your_relationships_z")
names_base_t2_pwb_your_relationships_z


# Your personal relationships.
t2_pwb_your_relationships_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_relationships_z,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_relationships_z
here_save(t2_pwb_your_relationships_z, "t2_pwb_your_relationships_z")




# Your personal relationships.
t2_pwb_your_relationships_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_relationships_z,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_relationships_z_1
here_save(t2_pwb_your_relationships_z_1, "t2_pwb_your_relationships_z_1")



# Your personal relationships.
t2_pwb_your_relationships_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_your_relationships_z,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_your_relationships_z_null
here_save(t2_pwb_your_relationships_z_null,
          "t2_pwb_your_relationships_z_null")




names_base_t2_pwb_standard_living_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_pwb_standard_living_z")
names_base_t2_pwb_standard_living_z


# Your standard of living.
t2_pwb_standard_living_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_standard_living_z,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_standard_living_z
here_save(t2_pwb_standard_living_z, "t2_pwb_standard_living_z")


# Your standard of living.
t2_pwb_standard_living_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_standard_living_z,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_standard_living_z_1
here_save(t2_pwb_standard_living_z_1, "t2_pwb_standard_living_z_1")


# Your standard of living.
t2_pwb_standard_living_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_pwb_standard_living_z,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_pwb_standard_living_z_null
here_save(t2_pwb_standard_living_z_null,
          "t2_pwb_standard_living_z_null")




# names_base_t2_lifemeaning_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_lifemeaning_z")
# names_base_t2_lifemeaning_z


# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.
# t2_lifemeaning_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_lifemeaning_z,
#   outcome = "t2_lifemeaning_z",
#   cens = C,
#   shift = f,
#   mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# 
# t2_lifemeaning_z
# here_save(t2_lifemeaning_z, "t2_lifemeaning_z")
# 
# 
# # My life has a clear sense of purpose.
# # I have a good sense of what makes my life meaningful.
# t2_lifemeaning_z_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base_t2_lifemeaning_z,
#   outcome = "t2_lifemeaning_z",
#   cens = C,
#   shift = NULL,
#   # mtp = TRUE,
#   folds = 5,
#   outcome_type = "continuous",
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_lifemeaning_z_null
# here_save(t2_lifemeaning_z_null, "t2_lifemeaning_z_null")
# 

# My life has a clear sense of purpose.

names_base_t2_meaning_purpose_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_meaning_purpose_z")
names_base_t2_meaning_purpose_z

t2_meaning_purpose_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_meaning_purpose_z,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_meaning_purpose_z
here_save(t2_meaning_purpose_z, "t2_meaning_purpose_z")




t2_meaning_purpose_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_meaning_purpose_z,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_meaning_purpose_z_1
here_save(t2_meaning_purpose_z_1, "t2_meaning_purpose_z_1")




# My life has a clear sense of purpose.
t2_meaning_purpose_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_meaning_purpose_z,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_meaning_purpose_z_null
here_save(t2_meaning_purpose_z_null, "t2_meaning_purpose_z_null")



# I have a good sense of what makes my life meaningful.


names_base_t2_meaning_sense_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_meaning_sense_z")
names_base_t2_meaning_sense_z

# I have a good sense of what makes my life meaningful.

t2_meaning_sense_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_meaning_sense_z,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_meaning_sense_z
here_save(t2_meaning_sense_z, "t2_meaning_sense_z")




t2_meaning_sense_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_meaning_sense_z,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_meaning_sense_z_1
here_save(t2_meaning_sense_z_1, "t2_meaning_sense_z_1")



# I have a good sense of what makes my life meaningful.
t2_meaning_sense_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_meaning_sense_z,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_meaning_sense_z_null
here_save(t2_meaning_sense_z_null, "t2_meaning_sense_z_null")







names_base_t2_lifesat_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_lifesat_z")
names_base_t2_lifesat_z


# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_lifesat_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_lifesat_z,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_lifesat_z
here_save(t2_lifesat_z, "t2_lifesat_z")


t2_lifesat_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_lifesat_z,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_lifesat_z_1
here_save(t2_lifesat_z_1, "t2_lifesat_z_1")

# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_lifesat_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_lifesat_z,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_lifesat_z_null
here_save(t2_lifesat_z_null, "t2_lifesat_z_null")



# social models -----------------------------------------------------------


# names_base_t2_support_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_support_z")
# names_base_t2_support_z


# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress (r)
# There is no one I can turn to for guidance in times of stress.
t2_support_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_support_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_support_z
here_save(t2_support_z, "t2_support_z")


t2_support_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_support_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_support_z_1
here_save(t2_support_z_1, "t2_support_z_1")




# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress (r)
# There is no one I can turn to for guidance in times of stress.
t2_support_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_support_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_support_z_null
here_save(t2_support_z_null, "t2_support_z_null")




names_base_t2_neighbourhood_community_z <-
  select_and_rename_cols(names_base = names_base,
                         baseline_vars = baseline_vars,
                         outcome = "t2_neighbourhood_community_z")
names_base_t2_neighbourhood_community_z

# I feel a sense of community with others in my local neighbourhood.
t2_neighbourhood_community_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_neighbourhood_community_z,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_neighbourhood_community_z
here_save(t2_neighbourhood_community_z,
          "t2_neighbourhood_community_z")


t2_neighbourhood_community_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_neighbourhood_community_z,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_neighbourhood_community_z_1
here_save(t2_neighbourhood_community_z_1,
          "t2_neighbourhood_community_z_1")


# I feel a sense of community with others in my local neighbourhood.
t2_neighbourhood_community_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_neighbourhood_community_z,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_neighbourhood_community_z_null
here_save(t2_neighbourhood_community_z_null,
          "t2_neighbourhood_community_z_null")




# names_base_t2_belong_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_belong_z")
# names_base_t2_belong_z

#RERUN FROM EHRE

# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.
t2_belong_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_belong_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_belong_z
here_save(t2_belong_z, "t2_belong_z")



t2_belong_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_belong_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_belong_z_1
here_save(t2_belong_z_1, "t2_belong_z_1")


# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.
t2_belong_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base_t2_reapeated_outcome_name,
  outcome = "t2_belong_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_belong_z_null
here_save(t2_belong_z_null, "t2_belong_z_null")


# contrasts health ---------------------------------------------------------------
A
f
f_1
# smoker
# t2_smoker_binary <- here_read("t2_smoker_binary")
# t2_smoker_binary_1 <- here_read("t2_smoker_binary_1")
# t2_smoker_binary_null <-
#   here_read("t2_smoker_binary_null")


# first contrast
# contrast_t2_smoker_binary <-
#   lmtp_contrast(t2_smoker_binary,
#                 ref = t2_smoker_binary_null,
#                 type = "rr")
# 
# 
# tab_contrast_t2_smoker_binary <-
#   margot_lmtp_tab(contrast_t2_smoker_binary,
#                   scale = "RR",
#                   new_name = "Smoker")
# 
# tab_contrast_t2_smoker_binary
# 
# 
# out_tab_contrast_t2_smoker_binary <-
#   lmtp_evalue_tab(tab_contrast_t2_smoker_binary,
#                   scale = c("RR"))
# 
# out_tab_contrast_t2_smoker_binary
# 
# here_save(out_tab_contrast_t2_smoker_binary, "out_tab_contrast_t2_smoker_binary")
# 
# # second contrast
# contrast_t2_smoker_binary_1 <-
#   lmtp_contrast(t2_smoker_binary_1,
#                 ref = t2_smoker_binary_null,
#                 type = "rr")
# 
# 
# 
# tab_contrast_t2_smoker_binary_1 <-
#   margot_lmtp_tab(contrast_t2_smoker_binary_1,
#                   scale = "RR",
#                   new_name = "Smoker")
# 
# tab_contrast_t2_smoker_binary_1
# 
# 
# out_tab_contrast_t2_smoker_binary_1 <-
#   lmtp_evalue_tab(tab_contrast_t2_smoker_binary_1,
#                   scale = c("RR"))
# 
# out_tab_contrast_t2_smoker_binary_1
# 
# here_save(out_tab_contrast_t2_smoker_binary_1, "out_tab_contrast_t2_smoker_binary_1")

# # sf health
# t2_sfhealth_z <- here_read("t2_sfhealth_z")
# t2_sfhealth_z_null <- here_read("t2_sfhealth_z_null")
#
#
# contrast_t2_sfhealth_z <- lmtp_contrast(t2_sfhealth_z,
#                                         ref = t2_sfhealth_z_null,
#                                         type = "additive")
#
# tab_contrast_t2_sfhealth_z <-
#   margot_lmtp_tab(contrast_t2_sfhealth_z,
#                   scale = "RD",
#                   new_name = "Short form health")
#
#
# out_tab_contrast_t2_sfhealth_z <-
#   lmtp_evalue_tab(tab_contrast_t2_sfhealth_z,
#                   scale = c("RD"))
#
# out_tab_contrast_t2_sfhealth_z


# sf health, your health
t2_sfhealth_your_health_z <- here_read("t2_sfhealth_your_health_z")
t2_sfhealth_your_health_z_1 <- here_read("t2_sfhealth_your_health_z_1")

t2_sfhealth_your_health_z_null <-
  here_read("t2_sfhealth_your_health_z_null")

# first contrast 
contrast_t2_sfhealth_your_health_z <-
  lmtp_contrast(t2_sfhealth_your_health_z,
                ref = t2_sfhealth_your_health_z_null,
                type = "additive")

tab_contrast_t2_sfhealth_your_health_z <-
  margot_lmtp_tab(contrast_t2_sfhealth_your_health_z,
                  scale = "RD",
                  new_name = "Short form health, your health")


out_tab_contrast_t2_sfhealth_your_health_z <-
  lmtp_evalue_tab(tab_contrast_t2_sfhealth_your_health_z,
                  scale = c("RD"))

out_tab_contrast_t2_sfhealth_your_health_z

# second contrast

contrast_t2_sfhealth_your_health_z_1 <-
  lmtp_contrast(t2_sfhealth_your_health_z_1,
                ref = t2_sfhealth_your_health_z_null,
                type = "additive")


tab_contrast_t2_sfhealth_your_health_z_1 <-
  margot_lmtp_tab(contrast_t2_sfhealth_your_health_z_1,
                  scale = "RD",
                  new_name = "Short form health, your health")


out_tab_contrast_t2_sfhealth_your_health_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_sfhealth_your_health_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_sfhealth_your_health_z_1


# third contrast

contrast_t2_sfhealth_your_health_z_0 <-
  lmtp_contrast(t2_sfhealth_your_health_z,
                ref = t2_sfhealth_your_health_z_1,
                type = "additive")


tab_contrast_t2_sfhealth_your_health_z_0 <-
  margot_lmtp_tab(contrast_t2_sfhealth_your_health_z_0,
                  scale = "RD",
                  new_name = "Short form health, your health")


out_tab_contrast_t2_sfhealth_your_health_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_sfhealth_your_health_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_sfhealth_your_health_z_0



# # excercise
# t2_hours_exercise_log_z <-
#   here_read("t2_hours_exercise_log_z")
# t2_hours_exercise_log_z_1 <-
#   here_read("t2_hours_exercise_log_z_1")
# 
# t2_hours_exercise_log_z_null <-
#   here_read("t2_hours_exercise_log_z_null")
# 
# # first contrast
# contrast_t2_hours_exercise_log_z <-
#   lmtp_contrast(t2_hours_exercise_log_z,
#                 ref = t2_hours_exercise_log_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_hours_exercise_log_z <-
#   margot_lmtp_tab(contrast_t2_hours_exercise_log_z,
#                   scale = "RD",
#                   new_name = "Hours excercise")
# 
# 
# out_tab_contrast_t2_hours_exercise_log_z <-
#   lmtp_evalue_tab(tab_contrast_t2_hours_exercise_log_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_hours_exercise_log_z
# 
# # second contrast
# contrast_t2_hours_exercise_log_z_1 <-
#   lmtp_contrast(t2_hours_exercise_log_z_1,
#                 ref = t2_hours_exercise_log_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_hours_exercise_log_z_1 <-
#   margot_lmtp_tab(contrast_t2_hours_exercise_log_z_1,
#                   scale = "RD",
#                   new_name = "Hours excercise")
# 
# 
# out_tab_contrast_t2_hours_exercise_log_z_1 <-
#   lmtp_evalue_tab(tab_contrast_t2_hours_exercise_log_z_1,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_hours_exercise_log_z_1

# alcohol freq

t2_alcohol_frequency_z <-
  here_read("t2_alcohol_frequency_z")

t2_alcohol_frequency_z_1 <-
  here_read("t2_alcohol_frequency_z_1")

t2_alcohol_frequency_z_null <-
  here_read("t2_alcohol_frequency_z_null")

# first contrast
contrast_t2_alcohol_frequency_z <-
  lmtp_contrast(t2_alcohol_frequency_z,
                ref = t2_alcohol_frequency_z_null,
                type = "additive")


tab_contrast_t2_alcohol_frequency_z <-
  margot_lmtp_tab(contrast_t2_alcohol_frequency_z ,
                  scale = "RD",
                  new_name = "Alcohol frequency")


out_tab_contrast_t2_alcohol_frequency_z <-
  lmtp_evalue_tab(tab_contrast_t2_alcohol_frequency_z,
                  scale = c("RD"))

out_tab_contrast_t2_alcohol_frequency_z

# second contrast
contrast_t2_alcohol_frequency_z_1 <-
  lmtp_contrast(t2_alcohol_frequency_z_1,
                ref = t2_alcohol_frequency_z_null,
                type = "additive")


tab_contrast_t2_alcohol_frequency_z_1 <-
  margot_lmtp_tab(contrast_t2_alcohol_frequency_z_1,
                  scale = "RD",
                  new_name = "Alcohol frequency")


out_tab_contrast_t2_alcohol_frequency_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_alcohol_frequency_z_1,
                  scale = c("RD"))


out_tab_contrast_t2_alcohol_frequency_z_1

# third contrast
contrast_t2_alcohol_frequency_z_0 <-
  lmtp_contrast(t2_alcohol_frequency_z,
                ref = t2_alcohol_frequency_z_1,
                type = "additive")


tab_contrast_t2_alcohol_frequency_z_0 <-
  margot_lmtp_tab(contrast_t2_alcohol_frequency_z_0,
                  scale = "RD",
                  new_name = "Alcohol frequency")


out_tab_contrast_t2_alcohol_frequency_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_alcohol_frequency_z_0,
                  scale = c("RD"))


out_tab_contrast_t2_alcohol_frequency_z_0


# alcohol intensity

t2_alcohol_intensity_z <-
  here_read("t2_alcohol_intensity_z")

t2_alcohol_intensity_z_1 <-
  here_read("t2_alcohol_intensity_z_1")

t2_alcohol_intensity_z_null <-
  here_read("t2_alcohol_intensity_z_null")

# first contrast
contrast_t2_alcohol_intensity_z <-
  lmtp_contrast(t2_alcohol_intensity_z,
                ref = t2_alcohol_intensity_z_null,
                type = "additive")


tab_contrast_t2_alcohol_intensity_z <-
  margot_lmtp_tab(contrast_t2_alcohol_intensity_z,
                  scale = "RD",
                  new_name = "Alcohol intensity")


out_tab_contrast_t2_alcohol_intensity_z <-
  lmtp_evalue_tab(tab_contrast_t2_alcohol_intensity_z,
                  scale = c("RD"))

out_tab_contrast_t2_alcohol_intensity_z

# second contrast
contrast_t2_alcohol_intensity_z_1 <-
  lmtp_contrast(t2_alcohol_intensity_z_1,
                ref = t2_alcohol_intensity_z_null,
                type = "additive")


tab_contrast_t2_alcohol_intensity_z_1 <-
  margot_lmtp_tab(contrast_t2_alcohol_intensity_z_1,
                  scale = "RD",
                  new_name = "Alcohol intensity")


out_tab_contrast_t2_alcohol_intensity_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_alcohol_intensity_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_alcohol_intensity_z_1

# third contrast
contrast_t2_alcohol_intensity_z_0 <-
  lmtp_contrast(t2_alcohol_intensity_z,
                ref = t2_alcohol_intensity_z_1,
                type = "additive")


tab_contrast_t2_alcohol_intensity_z_0 <-
  margot_lmtp_tab(contrast_t2_alcohol_intensity_z_0,
                  scale = "RD",
                  new_name = "Alcohol intensity")


out_tab_contrast_t2_alcohol_intensity_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_alcohol_intensity_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_alcohol_intensity_z_0


# hour sleep
t2_hlth_sleep_hours_z <- here_read("t2_hlth_sleep_hours_z")
t2_hlth_sleep_hours_z_1 <- here_read("t2_hlth_sleep_hours_z_1")


t2_hlth_sleep_hours_z_null <-
  here_read("t2_hlth_sleep_hours_z_null")

# first contrast
contrast_t2_hours_sleep_z <-
  lmtp_contrast(t2_hlth_sleep_hours_z,
                ref = t2_hlth_sleep_hours_z_null,
                type = "additive")

tab_contrast_t2_hours_sleep_z <-
  margot_lmtp_tab(contrast_t2_hours_sleep_z,
                  scale = "RD",
                  new_name = "Hours sleep")


out_tab_contrast_t2_hours_sleep_z <-
  lmtp_evalue_tab(tab_contrast_t2_hours_sleep_z,
                  scale = c("RD"))

out_tab_contrast_t2_hours_sleep_z

# second contrast
contrast_t2_hours_sleep_z_1 <-
  lmtp_contrast(t2_hlth_sleep_hours_z_1,
                ref = t2_hlth_sleep_hours_z_null,
                type = "additive")

tab_contrast_t2_hours_sleep_z_1 <-
  margot_lmtp_tab(contrast_t2_hours_sleep_z_1,
                  scale = "RD",
                  new_name = "Hours sleep")


out_tab_contrast_t2_hours_sleep_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_hours_sleep_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_hours_sleep_z_1

# third contrast
contrast_t2_hours_sleep_z_0 <-
  lmtp_contrast(t2_hlth_sleep_hours_z,
                ref = t2_hlth_sleep_hours_z_1,
                type = "additive")

tab_contrast_t2_hours_sleep_z_0 <-
  margot_lmtp_tab(contrast_t2_hours_sleep_z_0,
                  scale = "RD",
                  new_name = "Hours sleep")


out_tab_contrast_t2_hours_sleep_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_hours_sleep_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_hours_sleep_z_0


# bmi
t2_hlth_bmi_z <- here_read("t2_hlth_bmi_z")
t2_hlth_bmi_z_1 <- here_read("t2_hlth_bmi_z_1")

t2_hlth_bmi_z_null <- here_read("t2_hlth_bmi_z_null")

# first contrast
contrast_t2_bmi_z <- lmtp_contrast(t2_hlth_bmi_z,
                                   ref = t2_hlth_bmi_z_null,
                                   type = "additive")

tab_contrast_t2_bmi_z <-
  margot_lmtp_tab(contrast_t2_bmi_z, scale = "RD", new_name = "BMI")


out_tab_contrast_t2_bmi_z <-
  lmtp_evalue_tab(tab_contrast_t2_bmi_z,
                  scale = c("RD"))

out_tab_contrast_t2_bmi_z


# second contrast
contrast_t2_bmi_z_1 <- lmtp_contrast(t2_hlth_bmi_z_1,
                                     ref = t2_hlth_bmi_z_null,
                                     type = "additive")

tab_contrast_t2_bmi_z_1  <-
  margot_lmtp_tab(contrast_t2_bmi_z_1, scale = "RD", new_name = "BMI")


out_tab_contrast_t2_bmi_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_bmi_z_1, scale = c("RD"))

out_tab_contrast_t2_bmi_z_1 

# third contrast
contrast_t2_bmi_z_0 <- lmtp_contrast(t2_hlth_bmi_z,
                                     ref = t2_hlth_bmi_z_1,
                                     type = "additive")

tab_contrast_t2_bmi_z_0  <-
  margot_lmtp_tab(contrast_t2_bmi_z_0, scale = "RD", new_name = "BMI")


out_tab_contrast_t2_bmi_z_0  <-
  lmtp_evalue_tab(tab_contrast_t2_bmi_z_0, scale = c("RD"))

out_tab_contrast_t2_bmi_z_0 


# contrast embodied -------------------------------------------------------

# bodysat
t2_bodysat_z <- here_read("t2_bodysat_z")
t2_bodysat_z_1 <- here_read("t2_bodysat_z_1")

t2_bodysat_z_null <- here_read("t2_bodysat_z_null")

# first contrast
contrast_t2_bodysat_z <- lmtp_contrast(t2_bodysat_z,
                                       ref = t2_bodysat_z_null,
                                       type = "additive")
contrast_t2_bodysat_z
tab_contrast_t2_bodysat_z <-
  margot_lmtp_tab(contrast_t2_bodysat_z, scale = "RD", new_name = "Body satisfaction")


out_tab_contrast_t2_bodysat_z <-
  lmtp_evalue_tab(tab_contrast_t2_bodysat_z,
                  scale = c("RD"))
out_tab_contrast_t2_bodysat_z


# second contrast
contrast_t2_bodysat_z_1 <- lmtp_contrast(t2_bodysat_z_1,
                                         ref = t2_bodysat_z_null,
                                         type = "additive")
contrast_t2_bodysat_z_1
tab_contrast_t2_bodysat_z_1 <-
  margot_lmtp_tab(contrast_t2_bodysat_z_1, scale = "RD", new_name = "Body satisfaction")


out_tab_contrast_t2_bodysat_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_bodysat_z_1,
                  scale = c("RD"))
out_tab_contrast_t2_bodysat_z_1

# third contrast
contrast_t2_bodysat_z_0 <- lmtp_contrast(t2_bodysat_z,
                                         ref = t2_bodysat_z_1,
                                         type = "additive")
contrast_t2_bodysat_z_0
tab_contrast_t2_bodysat_z_0 <-
  margot_lmtp_tab(contrast_t2_bodysat_z_0, scale = "RD", new_name = "Body satisfaction")


out_tab_contrast_t2_bodysat_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_bodysat_z_0,
                  scale = c("RD"))
out_tab_contrast_t2_bodysat_z_0


# kessler 6

# t2_kessler6_sum_z <- here_read("t2_kessler6_sum_z")
# t2_kessler6_sum_z_null <-
#   here_read("t2_kessler6_sum_z_null")
#
# contrast_t2_kessler6_sum_z <-
#   lmtp_contrast(t2_kessler6_sum_z,
#                 ref = t2_kessler6_sum_z_null,
#                 type = "additive")
#
# tab_contrast_t2_kessler6_sum_z <-
#   margot_lmtp_tab(contrast_t2_kessler6_sum_z,
#                   scale = "RD",
#                   new_name = "Kessler 6 distress")
#
#
# out_tab_contrast_t2_kessler6_sum_z <-
#   lmtp_evalue_tab(tab_contrast_t2_kessler6_sum_z,
#                   scale = c("RD"))
#
# out_tab_contrast_t2_kessler6_sum_z


# depression

t2_kessler_latent_depression_z <-
  here_read("t2_kessler_latent_depression_z")

t2_kessler_latent_depression_z_1 <-
  here_read("t2_kessler_latent_depression_z_1")

t2_kessler_latent_depression_z_null <-
  here_read("t2_kessler_latent_depression_z_null")

t2_kessler_latent_depression_z_null

# first contrast
contrast_t2_kessler_latent_depression_z <-
  lmtp_contrast(t2_kessler_latent_depression_z,
                ref = t2_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z <-
  margot_lmtp_tab(contrast_t2_kessler_latent_depression_z,
                  scale = "RD",
                  new_name = "Kessler 6 depression")


out_tab_contrast_t2_kessler_latent_depression_z <-
  lmtp_evalue_tab(tab_contrast_t2_kessler_latent_depression_z,
                  scale = c("RD"))

out_tab_contrast_t2_kessler_latent_depression_z


# second contrast
contrast_t2_kessler_latent_depression_z_1 <-
  lmtp_contrast(t2_kessler_latent_depression_z_1,
                ref = t2_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z_1 <-
  margot_lmtp_tab(contrast_t2_kessler_latent_depression_z_1,
                  scale = "RD",
                  new_name = "Kessler 6 depression")


out_tab_contrast_t2_kessler_latent_depression_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_kessler_latent_depression_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_kessler_latent_depression_z_1

# third contrast
contrast_t2_kessler_latent_depression_z_0 <-
  lmtp_contrast(t2_kessler_latent_depression_z,
                ref = t2_kessler_latent_depression_z_1,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z_0 <-
  margot_lmtp_tab(contrast_t2_kessler_latent_depression_z_0,
                  scale = "RD",
                  new_name = "Kessler 6 depression")


out_tab_contrast_t2_kessler_latent_depression_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_kessler_latent_depression_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_kessler_latent_depression_z_0


#anxiety

t2_kessler_latent_anxiety_z <-
  here_read("t2_kessler_latent_anxiety_z")

t2_kessler_latent_anxiety_z_1 <-
  here_read("t2_kessler_latent_anxiety_z_1")

t2_kessler_latent_anxiety_z_null <-
  here_read("t2_kessler_latent_anxiety_z_null")

# first contrast
contrast_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_kessler_latent_anxiety_z,
                ref = t2_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z <-
  margot_lmtp_tab(contrast_t2_kessler_latent_anxiety_z,
                  scale = "RD",
                  new_name = "Kessler 6 anxiety")


out_tab_contrast_t2_kessler_latent_anxiety_z <-
  lmtp_evalue_tab(tab_contrast_t2_kessler_latent_anxiety_z,
                  scale = c("RD"))

out_tab_contrast_t2_kessler_latent_anxiety_z

# second contrast
contrast_t2_kessler_latent_anxiety_z_1 <-
  lmtp_contrast(t2_kessler_latent_anxiety_z_1,
                ref = t2_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z_1 <-
  margot_lmtp_tab(contrast_t2_kessler_latent_anxiety_z_1,
                  scale = "RD",
                  new_name = "Kessler 6 anxiety")


out_tab_contrast_t2_kessler_latent_anxiety_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_kessler_latent_anxiety_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_kessler_latent_anxiety_z_1

# third contrast
contrast_t2_kessler_latent_anxiety_z_0 <-
  lmtp_contrast(t2_kessler_latent_anxiety_z,
                ref = t2_kessler_latent_anxiety_z_1,
                type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z_0 <-
  margot_lmtp_tab(contrast_t2_kessler_latent_anxiety_z_0,
                  scale = "RD",
                  new_name = "Kessler 6 anxiety")


out_tab_contrast_t2_kessler_latent_anxiety_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_kessler_latent_anxiety_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_kessler_latent_anxiety_z_0


# fatigue
t2_hlth_fatigue_z <- here_read("t2_hlth_fatigue_z")

t2_hlth_fatigue_z_1 <- here_read("t2_hlth_fatigue_z_1")

t2_hlth_fatigue_z_null <-
  here_read("t2_hlth_fatigue_z_null")

# first contrast
contrast_t2_hlth_fatigue_z <-
  lmtp_contrast(t2_hlth_fatigue_z,
                ref = t2_hlth_fatigue_z_null,
                type = "additive")


tab_contrast_t2_hlth_fatigue_z <-
  margot_lmtp_tab(contrast_t2_hlth_fatigue_z ,
                  scale = "RD",
                  new_name = "Fatigue")


out_tab_contrast_t2_hlth_fatigue_z <-
  lmtp_evalue_tab(tab_contrast_t2_hlth_fatigue_z,
                  scale = c("RD"))

out_tab_contrast_t2_hlth_fatigue_z


# second contrast
contrast_t2_hlth_fatigue_z_1 <-
  lmtp_contrast(t2_hlth_fatigue_z_1,
                ref = t2_hlth_fatigue_z_null,
                type = "additive")


tab_contrast_t2_hlth_fatigue_z_1  <-
  margot_lmtp_tab(contrast_t2_hlth_fatigue_z_1,
                  scale = "RD",
                  new_name = "Fatigue")


out_tab_contrast_t2_hlth_fatigue_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_hlth_fatigue_z_1 ,
                  scale = c("RD"))

out_tab_contrast_t2_hlth_fatigue_z_1 

# third contrast
contrast_t2_hlth_fatigue_z_0 <-
  lmtp_contrast(t2_hlth_fatigue_z,
                ref = t2_hlth_fatigue_z_1,
                type = "additive")


tab_contrast_t2_hlth_fatigue_z_0  <-
  margot_lmtp_tab(contrast_t2_hlth_fatigue_z_0,
                  scale = "RD",
                  new_name = "Fatigue")


out_tab_contrast_t2_hlth_fatigue_z_0  <-
  lmtp_evalue_tab(tab_contrast_t2_hlth_fatigue_z_0 ,
                  scale = c("RD"))

out_tab_contrast_t2_hlth_fatigue_z_0 

# rumination
t2_rumination_z <- here_read("t2_rumination_z")
t2_rumination_z_1 <- here_read("t2_rumination_z_1")
t2_rumination_z_null <-
  here_read("t2_rumination_z_null")

# first contrast
contrast_t2_rumination_z <-
  lmtp_contrast(t2_rumination_z,
                ref = t2_rumination_z_null,
                type = "additive")

tab_contrast_t2_rumination_z <-
  margot_lmtp_tab(contrast_t2_rumination_z ,
                  scale = "RD",
                  new_name = "Rumination")

out_tab_contrast_t2_rumination_z <-
  lmtp_evalue_tab(tab_contrast_t2_rumination_z,
                  scale = c("RD"))

out_tab_contrast_t2_rumination_z                  
# second contrast

contrast_t2_rumination_z_1 <-
  lmtp_contrast(t2_rumination_z_1,
                ref = t2_rumination_z_null,
                type = "additive")

tab_contrast_t2_rumination_z_1 <-
  margot_lmtp_tab(contrast_t2_rumination_z_1 ,
                  scale = "RD",
                  new_name = "Rumination")


out_tab_contrast_t2_rumination_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_rumination_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_rumination_z_1

# third contrast

contrast_t2_rumination_z_0 <-
  lmtp_contrast(t2_rumination_z,
                ref = t2_rumination_z_1,
                type = "additive")

tab_contrast_t2_rumination_z_0 <-
  margot_lmtp_tab(contrast_t2_rumination_z_0 ,
                  scale = "RD",
                  new_name = "Rumination")


out_tab_contrast_t2_rumination_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_rumination_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_rumination_z_0




# sex sat
t2_sexual_satisfaction_z <-
  here_read("t2_sexual_satisfaction_z")

t2_sexual_satisfaction_z_1 <-
  here_read("t2_sexual_satisfaction_z_1")

t2_sexual_satisfaction_z_null <-
  here_read("t2_sexual_satisfaction_z_null")

# first contrast
contrast_t2_sexual_satisfaction_z <-
  lmtp_contrast(t2_sexual_satisfaction_z,
                ref = t2_sexual_satisfaction_z_null,
                type = "additive")


tab_contrast_t2_sexual_satisfaction_z <-
  margot_lmtp_tab(contrast_t2_sexual_satisfaction_z,
                  scale = "RD",
                  new_name = "Sexual satisfaction")


out_tab_contrast_t2_sexual_satisfaction_z <-
  lmtp_evalue_tab(tab_contrast_t2_sexual_satisfaction_z,
                  scale = c("RD"))

out_tab_contrast_t2_sexual_satisfaction_z


# second contrast
contrast_t2_sexual_satisfaction_z_1 <-
  lmtp_contrast(t2_sexual_satisfaction_z_1,
                ref = t2_sexual_satisfaction_z_null,
                type = "additive")


tab_contrast_t2_sexual_satisfaction_z_1 <-
  margot_lmtp_tab(contrast_t2_sexual_satisfaction_z_1,
                  scale = "RD",
                  new_name = "Sexual satisfaction")


out_tab_contrast_t2_sexual_satisfaction_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_sexual_satisfaction_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_sexual_satisfaction_z_1

# third contrast
contrast_t2_sexual_satisfaction_z_0 <-
  lmtp_contrast(t2_sexual_satisfaction_z,
                ref = t2_sexual_satisfaction_z_1,
                type = "additive")


tab_contrast_t2_sexual_satisfaction_z_0 <-
  margot_lmtp_tab(contrast_t2_sexual_satisfaction_z_0,
                  scale = "RD",
                  new_name = "Sexual satisfaction")


out_tab_contrast_t2_sexual_satisfaction_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_sexual_satisfaction_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_sexual_satisfaction_z_0



# contrasts ego -----------------------------------------------------------

# power no control
# t2_power_no_control_composite_z <-
#   here_read("t2_power_no_control_composite_z")
# 
# t2_power_no_control_composite_z_1 <-
#   here_read("t2_power_no_control_composite_z_1")
# 
# t2_power_no_control_composite_z_null <-
#   here_read("t2_power_no_control_composite_z_null")

# first contrast
# contrast_t2_power_no_control_composite_z <-
#   lmtp_contrast(t2_power_no_control_composite_z,
#                 ref = t2_power_no_control_composite_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_power_no_control_composite_z <-
#   margot_lmtp_tab(contrast_t2_power_no_control_composite_z,
#                   scale = "RD",
#                   new_name = "Power no control")
# 
# 
# out_tab_contrast_t2_power_no_control_composite_z <-
#   lmtp_evalue_tab(tab_contrast_t2_power_no_control_composite_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_power_no_control_composite_z



# second contrast
# contrast_t2_power_no_control_composite_z_1  <-
#   lmtp_contrast(t2_power_no_control_composite_z_1,
#                 ref = t2_power_no_control_composite_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_power_no_control_composite_z_1  <-
#   margot_lmtp_tab(contrast_t2_power_no_control_composite_z_1 ,
#                   scale = "RD",
#                   new_name = "Power no control")
# 
# 
# out_tab_contrast_t2_power_no_control_composite_z_1  <-
#   lmtp_evalue_tab(tab_contrast_t2_power_no_control_composite_z_1,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_power_no_control_composite_z_1 


# self esteem

t2_self_esteem_z <- here_read("t2_self_esteem_z")
t2_self_esteem_z_1 <- here_read("t2_self_esteem_z_1")
t2_self_esteem_z_null <-
  here_read("t2_self_esteem_z_null")


# first contrast
contrast_t2_self_esteem_z <-
  lmtp_contrast(t2_self_esteem_z,
                ref = t2_self_esteem_z_null,
                type = "additive")


tab_contrast_t2_self_esteem_z <-
  margot_lmtp_tab(contrast_t2_self_esteem_z,
                  scale = "RD",
                  new_name = "Self esteem")


out_tab_contrast_t2_self_esteem_z <-
  lmtp_evalue_tab(tab_contrast_t2_self_esteem_z,
                  scale = c("RD"))

out_tab_contrast_t2_self_esteem_z



# second contrast
contrast_t2_self_esteem_z_1 <-
  lmtp_contrast(t2_self_esteem_z_1,
                ref = t2_self_esteem_z_null,
                type = "additive")

tab_contrast_t2_self_esteem_z_1 <-
  margot_lmtp_tab(contrast_t2_self_esteem_z_1,
                  scale = "RD",
                  new_name = "Self esteem")

out_tab_contrast_t2_self_esteem_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_self_esteem_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_self_esteem_z_1

#third contrast

contrast_t2_self_esteem_z_0 <-
  lmtp_contrast(t2_self_esteem_z,
                ref = t2_self_esteem_z_1,
                type = "additive")

tab_contrast_t2_self_esteem_z_0 <-
  margot_lmtp_tab(contrast_t2_self_esteem_z_0,
                  scale = "RD",
                  new_name = "Self esteem")

out_tab_contrast_t2_self_esteem_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_self_esteem_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_self_esteem_z_0

# perfectionism
t2_perfectionism_z <- here_read("t2_perfectionism_z")
t2_perfectionism_z_1 <- here_read("t2_perfectionism_z_1")
t2_perfectionism_z_null <-
  here_read("t2_perfectionism_z_null")

# first contrast
contrast_t2_perfectionism_z <-
  lmtp_contrast(t2_perfectionism_z,
                ref = t2_perfectionism_z_null,
                type = "additive")


tab_contrast_t2_perfectionism_z <-
  margot_lmtp_tab(contrast_t2_perfectionism_z ,
                  scale = "RD",
                  new_name = "Perfectionism")


out_tab_contrast_t2_perfectionism_z <-
  lmtp_evalue_tab(tab_contrast_t2_perfectionism_z,
                  scale = c("RD"))

out_tab_contrast_t2_perfectionism_z


# second contrast
contrast_t2_perfectionism_z_1 <-
  lmtp_contrast(t2_perfectionism_z_1,
                ref = t2_perfectionism_z_null,
                type = "additive")


tab_contrast_t2_perfectionism_z_1 <-
  margot_lmtp_tab(contrast_t2_perfectionism_z_1,
                  scale = "RD",
                  new_name = "Perfectionism")


out_tab_contrast_t2_perfectionism_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_perfectionism_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_perfectionism_z_1

# third contrast
contrast_t2_perfectionism_z_0 <-
  lmtp_contrast(t2_perfectionism_z,
                ref = t2_perfectionism_z_1,
                type = "additive")


tab_contrast_t2_perfectionism_z_0 <-
  margot_lmtp_tab(contrast_t2_perfectionism_z_0,
                  scale = "RD",
                  new_name = "Perfectionism")


out_tab_contrast_t2_perfectionism_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_perfectionism_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_perfectionism_z_0


# self control have
t2_self_control_have_lots_z <-
  here_read("t2_self_control_have_lots_z")

t2_self_control_have_lots_z_1 <-
  here_read("t2_self_control_have_lots_z_1")

t2_self_control_have_lots_z_null <-
  here_read("t2_self_control_have_lots_z_null")

# first contrast
contrast_t2_self_control_have_lots_z <-
  lmtp_contrast(t2_self_control_have_lots_z,
                ref = t2_self_control_have_lots_z_null,
                type = "additive")


tab_contrast_t2_self_control_have_lots_z <-
  margot_lmtp_tab(contrast_t2_self_control_have_lots_z ,
                  scale = "RD",
                  new_name = "Self control have")


out_tab_contrast_t2_self_control_have_lots_z <-
  lmtp_evalue_tab(tab_contrast_t2_self_control_have_lots_z,
                  scale = c("RD"))

out_tab_contrast_t2_self_control_have_lots_z


# second contrast
contrast_t2_self_control_have_lots_z_1 <-
  lmtp_contrast(t2_self_control_have_lots_z_1,
                ref = t2_self_control_have_lots_z_null,
                type = "additive")


tab_contrast_t2_self_control_have_lots_z_1 <-
  margot_lmtp_tab(contrast_t2_self_control_have_lots_z_1,
                  scale = "RD",
                  new_name = "Self control have")


out_tab_contrast_t2_self_control_have_lots_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_self_control_have_lots_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_self_control_have_lots_z_1

# third contrast
contrast_t2_self_control_have_lots_z_0 <-
  lmtp_contrast(t2_self_control_have_lots_z,
                ref = t2_self_control_have_lots_z_1,
                type = "additive")


tab_contrast_t2_self_control_have_lots_z_0 <-
  margot_lmtp_tab(contrast_t2_self_control_have_lots_z_0,
                  scale = "RD",
                  new_name = "Self control have")


out_tab_contrast_t2_self_control_have_lots_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_self_control_have_lots_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_self_control_have_lots_z_0




# self control wish
t2_self_control_wish_more_reversed_z <-
  here_read("t2_self_control_wish_more_reversed_z")

t2_self_control_wish_more_reversed_z_1 <-
  here_read("t2_self_control_wish_more_reversed_z_1")

t2_self_control_wish_more_reversed_z_null <-
  here_read("t2_self_control_wish_more_reversed_z_null")

# first contrast
contrast_t2_self_control_wish_more_reversed_z <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z,
                ref = t2_self_control_wish_more_reversed_z_null,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z <-
  margot_lmtp_tab(
    contrast_t2_self_control_wish_more_reversed_z,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )


out_tab_contrast_t2_self_control_wish_more_reversed_z <-
  lmtp_evalue_tab(tab_contrast_t2_self_control_wish_more_reversed_z,
                  scale = c("RD"))

out_tab_contrast_t2_self_control_wish_more_reversed_z


# second contrast
contrast_t2_self_control_wish_more_reversed_z_1 <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z_1,
                ref = t2_self_control_wish_more_reversed_z_null,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z_1 <-
  margot_lmtp_tab(
    contrast_t2_self_control_wish_more_reversed_z_1,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )


out_tab_contrast_t2_self_control_wish_more_reversed_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_self_control_wish_more_reversed_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_self_control_wish_more_reversed_z_1

# third contrast
contrast_t2_self_control_wish_more_reversed_z_0 <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z,
                ref = t2_self_control_wish_more_reversed_z_1,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z_0 <-
  margot_lmtp_tab(
    contrast_t2_self_control_wish_more_reversed_z_0,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )


out_tab_contrast_t2_self_control_wish_more_reversed_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_self_control_wish_more_reversed_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_self_control_wish_more_reversed_z_0


# emotional regulation
# t2_emotion_regulation_out_control_z <-
#   here_read("t2_emotion_regulation_out_control_z")
# 
# t2_emotion_regulation_out_control_z_1 <-
#   here_read("t2_emotion_regulation_out_control_z_1")
# 
# t2_emotion_regulation_out_control_z_null <-
#   here_read("t2_emotion_regulation_out_control_z_null")
# 
# # first contrast
# contrast_t2_emotion_regulation_out_control_z <-
#   lmtp_contrast(t2_emotion_regulation_out_control_z,
#                 ref = t2_emotion_regulation_out_control_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_emotion_regulation_out_control_z <-
#   margot_lmtp_tab(
#     contrast_t2_emotion_regulation_out_control_z ,
#     scale = "RD",
#     new_name = "Emotional regulation (out of control)"
#   )
# 
# 
# out_tab_contrast_t2_emotion_regulation_out_control_z <-
#   lmtp_evalue_tab(tab_contrast_t2_emotion_regulation_out_control_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_emotion_regulation_out_control_z
# 
# # second contrast
# contrast_t2_emotion_regulation_out_control_z_1 <-
#   lmtp_contrast(t2_emotion_regulation_out_control_z_1,
#                 ref = t2_emotion_regulation_out_control_z_null,
#                 type = "additive")
# 
# 
# 
# tab_contrast_t2_emotion_regulation_out_control_z_1 <-
#   margot_lmtp_tab(
#     contrast_t2_emotion_regulation_out_control_z_1 ,
#     scale = "RD",
#     new_name = "Emotional regulation (out of control)"
#   )
# 
# 
# out_tab_contrast_t2_emotion_regulation_out_control_z_1 <-
#   lmtp_evalue_tab(tab_contrast_t2_emotion_regulation_out_control_z_1,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_emotion_regulation_out_control_z_1



# #  permeability individual
# t2_permeability_individual_z <-
#   here_read("t2_permeability_individual_z")
# 
# t2_permeability_individual_z_1 <-
#   here_read("t2_permeability_individual_z_1")
# 
# t2_permeability_individual_z_null <-
#   here_read("t2_permeability_individual_z_null")
# 
# # first contrast
# contrast_t2_permeability_individual_z <-
#   lmtp_contrast(t2_permeability_individual_z,
#                 ref = t2_permeability_individual_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_permeability_individual_z <-
#   margot_lmtp_tab(contrast_t2_permeability_individual_z ,
#                   scale = "RD",
#                   new_name = "Permeability self")
# 
# 
# out_tab_contrast_t2_permeability_individual_z <-
#   lmtp_evalue_tab(tab_contrast_t2_permeability_individual_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_permeability_individual_z
# 
# 
# # second contrast
# contrast_t2_permeability_individual_z_1 <-
#   lmtp_contrast(t2_permeability_individual_z_1,
#                 ref = t2_permeability_individual_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_permeability_individual_z_1 <-
#   margot_lmtp_tab(contrast_t2_permeability_individual_z_1,
#                   scale = "RD",
#                   new_name = "Permeability self")
# 
# 
# out_tab_contrast_t2_permeability_individual_z_1 <-
#   lmtp_evalue_tab(tab_contrast_t2_permeability_individual_z_1,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_permeability_individual_z_1



# more a political view
# t2_impermeability_group_z<- here_read("t2_impermeability_group_z")
# t2_impermeability_group_z_null <- here_read("t2_impermeability_group_z_null")
#
# contrast_t2_impermeability_group_z <- lmtp_contrast(
#   t2_impermeability_group_z,
#   ref = t2_impermeability_group_z_null,
#   type = "additive")


# contrasts reflective ----------------------------------------------------

# gratitude
t2_gratitude_z <- here_read("t2_gratitude_z")
t2_gratitude_z_1 <- here_read("t2_gratitude_z_1")

t2_gratitude_z_null <- here_read("t2_gratitude_z_null")
t2_gratitude_z_null
# first contrast
contrast_t2_gratitude_z <- lmtp_contrast(t2_gratitude_z,
                                         ref = t2_gratitude_z_null,
                                         type = "additive")
tab_contrast_t2_gratitude_z <-
  margot_lmtp_tab(contrast_t2_gratitude_z,
                  scale = "RD",
                  new_name = "Gratitude")


out_tab_contrast_t2_gratitude_z <-
  lmtp_evalue_tab(tab_contrast_t2_gratitude_z,
                  scale = c("RD"))

out_tab_contrast_t2_gratitude_z

# second contrast
contrast_t2_gratitude_z_1 <- lmtp_contrast(t2_gratitude_z_1,
                                           ref = t2_gratitude_z_null,
                                           type = "additive")
tab_contrast_t2_gratitude_z_1 <-
  margot_lmtp_tab(contrast_t2_gratitude_z_1 ,
                  scale = "RD",
                  new_name = "Gratitude")


out_tab_contrast_t2_gratitude_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_gratitude_z_1 ,
                  scale = c("RD"))

out_tab_contrast_t2_gratitude_z_1 

# third contrast
contrast_t2_gratitude_z_0 <- lmtp_contrast(t2_gratitude_z,
                                           ref = t2_gratitude_z_1,
                                           type = "additive")
tab_contrast_t2_gratitude_z_0 <-
  margot_lmtp_tab(contrast_t2_gratitude_z_0 ,
                  scale = "RD",
                  new_name = "Gratitude")


out_tab_contrast_t2_gratitude_z_0  <-
  lmtp_evalue_tab(tab_contrast_t2_gratitude_z_0 ,
                  scale = c("RD"))

out_tab_contrast_t2_gratitude_z_0 


# 
# # vengence / forgive
# t2_vengeful_rumin_z <- here_read("t2_vengeful_rumin_z")
# t2_vengeful_rumin_z_1 <- here_read("t2_vengeful_rumin_z_1")
# 
# t2_vengeful_rumin_z_null <-
#   here_read("t2_vengeful_rumin_z_null")
# 
# # first contrast
# contrast_t2_vengeful_rumin_z <-
#   lmtp_contrast(t2_vengeful_rumin_z,
#                 ref = t2_vengeful_rumin_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_vengeful_rumin_z <-
#   margot_lmtp_tab(contrast_t2_vengeful_rumin_z,
#                   scale = "RD",
#                   new_name = "Vengefulness (forgiveness)")
# 
# 
# out_tab_contrast_t2_vengeful_rumin_z <-
#   lmtp_evalue_tab(tab_contrast_t2_vengeful_rumin_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_vengeful_rumin_z
# 
# 
# # second contrast
# contrast_t2_vengeful_rumin_z_1 <-
#   lmtp_contrast(t2_vengeful_rumin_z_1,
#                 ref = t2_vengeful_rumin_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_vengeful_rumin_z_1  <-
#   margot_lmtp_tab(contrast_t2_vengeful_rumin_z_1 ,
#                   scale = "RD",
#                   new_name = "Vengefulness (forgiveness")
# 
# 
# out_tab_contrast_t2_vengeful_rumin_z_1  <-
#   lmtp_evalue_tab(tab_contrast_t2_vengeful_rumin_z_1 ,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_vengeful_rumin_z_1 
# 

# pwb your health

t2_pwb_your_health_z <-
  here_read("t2_pwb_your_health_z")

t2_pwb_your_health_z_1 <-
  here_read("t2_pwb_your_health_z_1")

t2_pwb_your_health_z_null <-
  here_read("t2_pwb_your_health_z_null")

# first contrast
contrast_t2_pwb_your_health_z <-
  lmtp_contrast(t2_pwb_your_health_z,
                ref = t2_pwb_your_health_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_health_z <-
  margot_lmtp_tab(contrast_t2_pwb_your_health_z,
                  scale = "RD",
                  new_name = "PWB your health")


out_tab_contrast_t2_pwb_your_health_z <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_health_z,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_health_z

# second contrast
contrast_t2_pwb_your_health_z_1 <-
  lmtp_contrast(t2_pwb_your_health_z_1,
                ref = t2_pwb_your_health_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_health_z_1 <-
  margot_lmtp_tab(contrast_t2_pwb_your_health_z_1,
                  scale = "RD",
                  new_name = "PWB your health")


out_tab_contrast_t2_pwb_your_health_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_health_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_health_z_1

#third contrast

contrast_t2_pwb_your_health_z_0 <-
  lmtp_contrast(t2_pwb_your_health_z,
                ref = t2_pwb_your_health_z_1,
                type = "additive")

tab_contrast_t2_pwb_your_health_z_0 <-
  margot_lmtp_tab(contrast_t2_pwb_your_health_z_0,
                  scale = "RD",
                  new_name = "PWB your health")


out_tab_contrast_t2_pwb_your_health_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_health_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_health_z_0

# pwb your furture security

t2_pwb_your_future_security_z <-
  here_read("t2_pwb_your_future_security_z")

t2_pwb_your_future_security_z_1 <-
  here_read("t2_pwb_your_future_security_z_1")

t2_pwb_your_future_security_z_null <-
  here_read("t2_pwb_your_future_security_z_null")

# first contrast
contrast_t2_pwb_your_future_security_z <-
  lmtp_contrast(t2_pwb_your_future_security_z,
                ref = t2_pwb_your_future_security_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z <-
  margot_lmtp_tab(contrast_t2_pwb_your_future_security_z,
                  scale = "RD",
                  new_name = "PWB your future security")


out_tab_contrast_t2_pwb_your_future_security_z <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_future_security_z,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_future_security_z

# second contrast
contrast_t2_pwb_your_future_security_z_1 <-
  lmtp_contrast(t2_pwb_your_future_security_z_1,
                ref = t2_pwb_your_future_security_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z_1 <-
  margot_lmtp_tab(contrast_t2_pwb_your_future_security_z_1,
                  scale = "RD",
                  new_name = "PWB your future security")


out_tab_contrast_t2_pwb_your_future_security_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_future_security_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_future_security_z_1

#third contrast

contrast_t2_pwb_your_future_security_z_0 <-
  lmtp_contrast(t2_pwb_your_future_security_z,
                ref = t2_pwb_your_future_security_z_1,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z_0 <-
  margot_lmtp_tab(contrast_t2_pwb_your_future_security_z_0,
                  scale = "RD",
                  new_name = "PWB your future security")


out_tab_contrast_t2_pwb_your_future_security_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_future_security_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_future_security_z_0


# pwb your relationships

t2_pwb_your_relationships_z <-
  here_read("t2_pwb_your_relationships_z")

t2_pwb_your_relationships_z_1 <-
  here_read("t2_pwb_your_relationships_z_1")

t2_pwb_your_relationships_z_null <-
  here_read("t2_pwb_your_relationships_z_null")

# first contrast
contrast_t2_pwb_your_relationships_z <-
  lmtp_contrast(t2_pwb_your_relationships_z,
                ref = t2_pwb_your_relationships_z_null,
                type = "additive")


tab_contrast_t2_pwb_your_relationships_z <-
  margot_lmtp_tab(contrast_t2_pwb_your_relationships_z ,
                  scale = "RD",
                  new_name = "PWB your relationships")


out_tab_contrast_t2_pwb_your_relationships_z <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_relationships_z,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_relationships_z


# second contrast
contrast_t2_pwb_your_relationships_z_1 <-
  lmtp_contrast(t2_pwb_your_relationships_z_1 ,
                ref = t2_pwb_your_relationships_z_null,
                type = "additive")


tab_contrast_t2_pwb_your_relationships_z_1  <-
  margot_lmtp_tab(contrast_t2_pwb_your_relationships_z_1 ,
                  scale = "RD",
                  new_name = "PWB your relationships")


out_tab_contrast_t2_pwb_your_relationships_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_relationships_z_1 ,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_relationships_z_1 

#third contrast

contrast_t2_pwb_your_relationships_z_0 <-
  lmtp_contrast(t2_pwb_your_relationships_z,
                ref = t2_pwb_your_relationships_z_1,
                type = "additive")


tab_contrast_t2_pwb_your_relationships_z_0  <-
  margot_lmtp_tab(contrast_t2_pwb_your_relationships_z_0 ,
                  scale = "RD",
                  new_name = "PWB your relationships")


out_tab_contrast_t2_pwb_your_relationships_z_0  <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_your_relationships_z_0 ,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_your_relationships_z_0 


# pwb your standard of living

t2_pwb_standard_living_z <-
  here_read("t2_pwb_standard_living_z")

t2_pwb_standard_living_z_1 <-
  here_read("t2_pwb_standard_living_z_1")

t2_pwb_standard_living_z_null <-
  here_read("t2_pwb_standard_living_z_null")

# first contrast
contrast_t2_pwb_standard_living_z <-
  lmtp_contrast(t2_pwb_standard_living_z,
                ref = t2_pwb_standard_living_z_null,
                type = "additive")

tab_contrast_t2_pwb_standard_living_z <-
  margot_lmtp_tab(contrast_t2_pwb_standard_living_z ,
                  scale = "RD",
                  new_name = "PWB your standard living")


out_tab_contrast_t2_pwb_standard_living_z <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_standard_living_z,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_standard_living_z


# second contrast
contrast_t2_pwb_standard_living_z_1 <-
  lmtp_contrast(t2_pwb_standard_living_z_1,
                ref = t2_pwb_standard_living_z_null,
                type = "additive")

tab_contrast_t2_pwb_standard_living_z_1 <-
  margot_lmtp_tab(contrast_t2_pwb_standard_living_z_1 ,
                  scale = "RD",
                  new_name = "PWB your standard living")


out_tab_contrast_t2_pwb_standard_living_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_standard_living_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_standard_living_z_1

#third contrast
contrast_t2_pwb_standard_living_z_0 <-
  lmtp_contrast(t2_pwb_standard_living_z,
                ref = t2_pwb_standard_living_z_1,
                type = "additive")

tab_contrast_t2_pwb_standard_living_z_0 <-
  margot_lmtp_tab(contrast_t2_pwb_standard_living_z_0 ,
                  scale = "RD",
                  new_name = "PWB your standard living")


out_tab_contrast_t2_pwb_standard_living_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_pwb_standard_living_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_pwb_standard_living_z_0



# life meaning

# t2_lifemeaning_z <- here_read("t2_lifemeaning_z")
# t2_lifemeaning_z_null <-
#   here_read("t2_lifemeaning_z_null")
# contrast_t2_lifemeaning_z <-
#   lmtp_contrast(t2_lifemeaning_z,
#                 ref = t2_lifemeaning_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_lifemeaning_z <-
#   margot_lmtp_tab(contrast_t2_lifemeaning_z,
#                   scale = "RD",
#                   new_name = "Meaning in life")
# 
# 
# out_tab_contrast_t2_lifemeaning_z <-
#   lmtp_evalue_tab(tab_contrast_t2_lifemeaning_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_lifemeaning_z



t2_meaning_purpose_z <- here_read("t2_meaning_purpose_z")
t2_meaning_purpose_z_1 <- here_read("t2_meaning_purpose_z_1")
t2_meaning_purpose_z_null <-
  here_read("t2_meaning_purpose_z_null")

# first contrast
contrast_t2_meaning_purpose_z <-
  lmtp_contrast(t2_meaning_purpose_z,
                ref = t2_meaning_purpose_z_null,
                type = "additive")

tab_contrast_t2_meaning_purpose_z <-
  margot_lmtp_tab(contrast_t2_meaning_purpose_z,
                  scale = "RD",
                  new_name = "Meaning: clear sense of purpose")


out_tab_contrast_t2_meaning_purpose_z <-
  lmtp_evalue_tab(tab_contrast_t2_meaning_purpose_z,
                  scale = c("RD"))

out_tab_contrast_t2_meaning_purpose_z

# second contrast
contrast_t2_meaning_purpose_z_1 <-
  lmtp_contrast(t2_meaning_purpose_z_1 ,
                ref = t2_meaning_purpose_z_null,
                type = "additive")

tab_contrast_t2_meaning_purpose_z_1  <-
  margot_lmtp_tab(contrast_t2_meaning_purpose_z_1 ,
                  scale = "RD",
                  new_name = "Meaning: clear sense of purpose")


out_tab_contrast_t2_meaning_purpose_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_meaning_purpose_z_1 ,
                  scale = c("RD"))

out_tab_contrast_t2_meaning_purpose_z_1 

#third contrast

contrast_t2_meaning_purpose_z_0 <-
  lmtp_contrast(t2_meaning_purpose_z,
                ref = t2_meaning_purpose_z_1,
                type = "additive")

tab_contrast_t2_meaning_purpose_z_0  <-
  margot_lmtp_tab(contrast_t2_meaning_purpose_z_0 ,
                  scale = "RD",
                  new_name = "Meaning: clear sense of purpose")


out_tab_contrast_t2_meaning_purpose_z_0  <-
  lmtp_evalue_tab(tab_contrast_t2_meaning_purpose_z_0 ,
                  scale = c("RD"))

out_tab_contrast_t2_meaning_purpose_z_0 


# meaning sense

t2_meaning_sense_z <- here_read("t2_meaning_sense_z")
t2_meaning_sense_z_1 <- here_read("t2_meaning_sense_z_1")

t2_meaning_sense_z_null <-
  here_read("t2_meaning_sense_z_null")

# first contrast
contrast_t2_meaning_sense_z <-
  lmtp_contrast(t2_meaning_sense_z,
                ref = t2_meaning_sense_z_null,
                type = "additive")
#changed from old name, was too long
tab_contrast_t2_meaning_sense_z <-
  margot_lmtp_tab(contrast_t2_meaning_sense_z,
                  scale = "RD",
                  #new_name = "Meaning: good sense of what makes my life meaningful"
                  new_name = "Meaning: sense meaningfulness in life")


out_tab_contrast_t2_meaning_sense_z <-
  lmtp_evalue_tab(tab_contrast_t2_meaning_sense_z,
                  scale = c("RD"))

out_tab_contrast_t2_meaning_sense_z


# second contrast
contrast_t2_meaning_sense_z_1 <-
  lmtp_contrast(t2_meaning_sense_z_1,
                ref = t2_meaning_sense_z_null,
                type = "additive")

tab_contrast_t2_meaning_sense_z_1 <-
  margot_lmtp_tab(contrast_t2_meaning_sense_z_1,
                  scale = "RD",
                  #new_name = "Meaning: good sense of what makes my life meaningful"
                  new_name = "Meaning: sense meaningfulness in life")


out_tab_contrast_t2_meaning_sense_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_meaning_sense_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_meaning_sense_z_1

# third contrast

contrast_t2_meaning_sense_z_0 <-
  lmtp_contrast(t2_meaning_sense_z,
                ref = t2_meaning_sense_z_1,
                type = "additive")

tab_contrast_t2_meaning_sense_z_0 <-
  margot_lmtp_tab(contrast_t2_meaning_sense_z_0,
                  scale = "RD",
                  #new_name = "Meaning: good sense of what makes my life meaningful"
                  new_name = "Meaning: sense meaningfulness in life")


out_tab_contrast_t2_meaning_sense_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_meaning_sense_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_meaning_sense_z_0



# lifesat

t2_lifesat_z <- here_read("t2_lifesat_z")
t2_lifesat_z_1 <- here_read("t2_lifesat_z_1")

t2_lifesat_z_null <- here_read("t2_lifesat_z_null")


# first contrast
contrast_t2_lifesat_z <- lmtp_contrast(t2_lifesat_z,
                                       ref = t2_lifesat_z_null,
                                       type = "additive")

tab_contrast_t2_lifesat_z <-
  margot_lmtp_tab(contrast_t2_lifesat_z, scale = "RD", new_name = "Satisfaction with life")


out_tab_contrast_t2_lifesat_z <-
  lmtp_evalue_tab(tab_contrast_t2_lifesat_z,
                  scale = c("RD"))

out_tab_contrast_t2_lifesat_z

# second contrast
contrast_t2_lifesat_z_1 <- lmtp_contrast(t2_lifesat_z_1,
                                         ref = t2_lifesat_z_null,
                                         type = "additive")

tab_contrast_t2_lifesat_z_1 <-
  margot_lmtp_tab(contrast_t2_lifesat_z_1, scale = "RD", new_name = "Satisfaction with life")


out_tab_contrast_t2_lifesat_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_lifesat_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_lifesat_z_1

# third contrast

contrast_t2_lifesat_z_0 <- lmtp_contrast(t2_lifesat_z,
                                         ref = t2_lifesat_z_1,
                                         type = "additive")

tab_contrast_t2_lifesat_z_0 <-
  margot_lmtp_tab(contrast_t2_lifesat_z_0, scale = "RD", new_name = "Satisfaction with life")


out_tab_contrast_t2_lifesat_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_lifesat_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_lifesat_z_0

# contrasts social --------------------------------------------------------

# social support
t2_support_z <- here_read("t2_support_z")
t2_support_z_1 <- here_read("t2_support_z_1")
t2_support_z_null <- here_read("t2_support_z_null")

# first contrast
contrast_t2_support_z <- lmtp_contrast(t2_support_z,
                                       ref = t2_support_z_null,
                                       type = "additive")
tab_contrast_t2_support_z <-
  margot_lmtp_tab(contrast_t2_support_z, scale = "RD", new_name = "Social support")


out_tab_contrast_t2_support_z <-
  lmtp_evalue_tab(tab_contrast_t2_support_z,
                  scale = c("RD"))

out_tab_contrast_t2_support_z

# second contrast
contrast_t2_support_z_1 <- lmtp_contrast(t2_support_z_1,
                                         ref = t2_support_z_null,
                                         type = "additive")
tab_contrast_t2_support_z_1 <-
  margot_lmtp_tab(contrast_t2_support_z_1, scale = "RD", new_name = "Social support")


out_tab_contrast_t2_support_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_support_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_support_z_1

#third contrast
contrast_t2_support_z_0 <- lmtp_contrast(t2_support_z,
                                         ref = t2_support_z_1,
                                         type = "additive")
tab_contrast_t2_support_z_0 <-
  margot_lmtp_tab(contrast_t2_support_z_0, scale = "RD", new_name = "Social support")


out_tab_contrast_t2_support_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_support_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_support_z_0


# neighbourhood community

t2_neighbourhood_community_z <-
  here_read("t2_neighbourhood_community_z")

t2_neighbourhood_community_z_1 <-
  here_read("t2_neighbourhood_community_z_1")

t2_neighbourhood_community_z_null <-
  here_read("t2_neighbourhood_community_z_null")

# first contrast
contrast_t2_neighbourhood_community_z <-
  lmtp_contrast(t2_neighbourhood_community_z,
                ref = t2_neighbourhood_community_z_null,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z <-
  margot_lmtp_tab(contrast_t2_neighbourhood_community_z,
                  scale = "RD",
                  new_name = "Neighbourhood community")


out_tab_contrast_t2_neighbourhood_community_z <-
  lmtp_evalue_tab(tab_contrast_t2_neighbourhood_community_z,
                  scale = c("RD"))

out_tab_contrast_t2_neighbourhood_community_z

# second contrast
contrast_t2_neighbourhood_community_z_1 <-
  lmtp_contrast(t2_neighbourhood_community_z_1,
                ref = t2_neighbourhood_community_z_null,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z_1 <-
  margot_lmtp_tab(contrast_t2_neighbourhood_community_z_1,
                  scale = "RD",
                  new_name = "Neighbourhood community")


out_tab_contrast_t2_neighbourhood_community_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_neighbourhood_community_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_neighbourhood_community_z_1

#third contrast

contrast_t2_neighbourhood_community_z_0 <-
  lmtp_contrast(t2_neighbourhood_community_z,
                ref = t2_neighbourhood_community_z_1,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z_0 <-
  margot_lmtp_tab(contrast_t2_neighbourhood_community_z_0,
                  scale = "RD",
                  new_name = "Neighbourhood community")


out_tab_contrast_t2_neighbourhood_community_z_0 <-
  lmtp_evalue_tab(tab_contrast_t2_neighbourhood_community_z_0,
                  scale = c("RD"))

out_tab_contrast_t2_neighbourhood_community_z_0

# social belong
t2_belong_z <- here_read("t2_belong_z")
t2_belong_z_1 <- here_read("t2_belong_z_1")
t2_belong_z_null <- here_read("t2_belong_z_null")

# first contrast
contrast_t2_belong_z <- lmtp_contrast(t2_belong_z,
                                      ref = t2_belong_z_null,
                                      type = "additive")


tab_contrast_t2_belong_z <-
  margot_lmtp_tab(contrast_t2_belong_z, scale = "RD",
                  new_name = "Social belonging")


out_tab_contrast_t2_belong_z <-
  lmtp_evalue_tab(tab_contrast_t2_belong_z,
                  scale = c("RD"))

out_tab_contrast_t2_belong_z

# second contrast
contrast_t2_belong_z_1 <- lmtp_contrast(t2_belong_z_1 ,
                                        ref = t2_belong_z_null,
                                        type = "additive")


tab_contrast_t2_belong_z_1  <-
  margot_lmtp_tab(contrast_t2_belong_z_1 , scale = "RD",
                  new_name = "Social belonging")


out_tab_contrast_t2_belong_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_belong_z_1 ,
                  scale = c("RD"))

out_tab_contrast_t2_belong_z_1 

#third contrast

contrast_t2_belong_z_0 <- lmtp_contrast(t2_belong_z ,
                                        ref = t2_belong_z_1,
                                        type = "additive")


tab_contrast_t2_belong_z_0  <-
  margot_lmtp_tab(contrast_t2_belong_z_0 , scale = "RD",
                  new_name = "Social belonging")


out_tab_contrast_t2_belong_z_0  <-
  lmtp_evalue_tab(tab_contrast_t2_belong_z_0 ,
                  scale = c("RD"))

out_tab_contrast_t2_belong_z_0 


# make tables -------------------------------------------------------------

# don't forget to report smoking


f_1
# bind individual tables

# tab_health_smoker <- rbind(
#   out_tab_contrast_t2_smoker_binary
# )
# tab_health


tab_health <- rbind(
  #out_tab_contrast_t2_hours_exercise_log_z,
  out_tab_contrast_t2_alcohol_frequency_z,
  out_tab_contrast_t2_alcohol_intensity_z,
  out_tab_contrast_t2_bmi_z,
  out_tab_contrast_t2_hours_sleep_z,
  out_tab_contrast_t2_sfhealth_your_health_z
)
tab_health


here_save(tab_health , "tab_health")

tab_body <- rbind(
  out_tab_contrast_t2_bodysat_z,
  out_tab_contrast_t2_kessler_latent_anxiety_z,
  out_tab_contrast_t2_kessler_latent_depression_z,
  out_tab_contrast_t2_hlth_fatigue_z,
  out_tab_contrast_t2_rumination_z,
  out_tab_contrast_t2_sexual_satisfaction_z
)
tab_body


here_save(tab_body, "tab_body")

tab_ego <- rbind(
  #out_tab_contrast_t2_emotion_regulation_out_control_z,
  #out_tab_contrast_t2_permeability_individual_z,
  out_tab_contrast_t2_perfectionism_z,
  #out_tab_contrast_t2_power_no_control_composite_z,
  out_tab_contrast_t2_self_control_have_lots_z,
  out_tab_contrast_t2_self_control_wish_more_reversed_z,
  out_tab_contrast_t2_self_esteem_z
)

tab_ego

here_save(tab_ego, "tab_ego")

tab_reflective <- rbind(
  out_tab_contrast_t2_gratitude_z,
  out_tab_contrast_t2_meaning_purpose_z,
  out_tab_contrast_t2_meaning_sense_z,
  out_tab_contrast_t2_pwb_your_future_security_z,
  out_tab_contrast_t2_pwb_your_health_z,
  out_tab_contrast_t2_pwb_your_relationships_z,
  out_tab_contrast_t2_pwb_standard_living_z,
  out_tab_contrast_t2_lifesat_z
  # out_tab_contrast_t2_vengeful_rumin_z
)
tab_reflective

here_save(tab_reflective,"tab_reflective")

tab_social <- rbind(
  out_tab_contrast_t2_belong_z,
  out_tab_contrast_t2_neighbourhood_community_z,
  out_tab_contrast_t2_support_z
)
tab_social

here_save(tab_social,"tab_social")

# make group table
group_tab_health <- group_tab(tab_health  , type = "RD")

# save
here_save(group_tab_health, "group_tab_health")


# make group table
group_tab_body <- group_tab(tab_body , type = "RD")

# save
here_save(group_tab_body, "group_tab_body")

# make group table
group_tab_ego <- group_tab(tab_ego, type = "RD")

# save
here_save(group_tab_ego, "group_tab_ego")




# make group table
group_tab_reflective <- group_tab(tab_reflective, type = "RD")
tab_reflective
# save
here_save(group_tab_reflective, "group_tab_reflective")

# make group table
group_tab_social <- group_tab(tab_social, type = "RD")

# save
here_save(group_tab_social, "group_tab_social")

group_tab_health <- here_read("group_tab_health")
group_tab_body <- here_read("group_tab_body")
group_tab_ego <- here_read("group_tab_ego")
group_tab_reflective <- here_read("group_tab_reflective")
group_tab_social <- here_read("group_tab_social")


# create plots -------------------------------------------------------------

# check N
n_participants
sub_title = "Exercise intervention: shift weekly exercise up to at least 5 hours, else take expected natural value, N = 27,194"


# graph health
plot_group_tab_health <- margot_plot(
  group_tab_health,
  type = "RD",
  title = "Health effects",
  subtitle = sub_title,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 15,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_health
dev.off()
# save graph
push_mods
ggsave(
  plot_group_tab_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_health.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_body <- margot_plot(
  group_tab_body,
  type = "RD",
  title = "Body effects",
  subtitle = sub_title,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_body
dev.off()
# save graph
ggsave(
  plot_group_tab_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)




# graph ego
plot_group_tab_ego <- margot_plot(
  group_tab_ego,
  type = "RD",
  title = "Ego effects",
  subtitle = sub_title,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_ego

# save graph
ggsave(
  plot_group_tab_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_group_tab_ego

# graph reflective
plot_group_tab_reflective <- margot_plot(
  group_tab_reflective,
  type = "RD",
  title = "Reflective effects",
  subtitle = sub_title,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_reflective

# save graph
ggsave(
  plot_group_tab_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social <- margot_plot(
  group_tab_social,
  type = "RD",
  title = "Social effects",
  subtitle = sub_title,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_group_tab_social

# save graph
ggsave(
  plot_group_tab_social,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# make tables shift one up -------------------------------------------------------------

# don't forget to report smoking
# bind individual tables
tab_health_1 <- rbind(
  #out_tab_contrast_t2_hours_exercise_log_z_1,
  out_tab_contrast_t2_alcohol_frequency_z_1,
  out_tab_contrast_t2_alcohol_intensity_z_1,
  out_tab_contrast_t2_bmi_z_1,
  out_tab_contrast_t2_hours_sleep_z_1,
  out_tab_contrast_t2_sfhealth_your_health_z_1
)
tab_health_1

here_save(tab_health_1, "tab_health_1")

tab_body_1 <- rbind(
  out_tab_contrast_t2_bodysat_z_1,
  out_tab_contrast_t2_kessler_latent_anxiety_z_1,
  out_tab_contrast_t2_kessler_latent_depression_z_1,
  out_tab_contrast_t2_hlth_fatigue_z_1,
  out_tab_contrast_t2_rumination_z_1,
  out_tab_contrast_t2_sexual_satisfaction_z_1
)
tab_body_1

here_save(tab_body_1, "tab_body_1")

tab_ego_1 <- rbind(
  #out_tab_contrast_t2_emotion_regulation_out_control_z_1,
  #out_tab_contrast_t2_permeability_individual_z_1,
  out_tab_contrast_t2_perfectionism_z_1,
  #out_tab_contrast_t2_power_no_control_composite_z_1,
  out_tab_contrast_t2_self_control_have_lots_z_1,
  out_tab_contrast_t2_self_control_wish_more_reversed_z_1,
  out_tab_contrast_t2_self_esteem_z_1
)

tab_ego_1

here_save(tab_ego_1, "tab_ego_1")

tab_reflective_1 <- rbind(
  out_tab_contrast_t2_gratitude_z_1,
  out_tab_contrast_t2_meaning_purpose_z_1,
  out_tab_contrast_t2_meaning_sense_z_1,
  out_tab_contrast_t2_pwb_your_future_security_z_1,
  out_tab_contrast_t2_pwb_your_health_z_1,
  out_tab_contrast_t2_pwb_your_relationships_z_1,
  out_tab_contrast_t2_pwb_standard_living_z_1,
  out_tab_contrast_t2_lifesat_z_1
  # out_tab_contrast_t2_vengeful_rumin_z_1
)
tab_reflective_1

here_save(tab_reflective_1, "tab_reflective_1")

tab_social_1 <- rbind(
  out_tab_contrast_t2_belong_z_1,
  out_tab_contrast_t2_neighbourhood_community_z_1,
  out_tab_contrast_t2_support_z_1
)
tab_social_1
here_save(tab_social_1, "tab_social_1")

# make group table
group_tab_health_1 <- group_tab(tab_health_1, type = "RD")

# save
here_save(group_tab_health_1, "group_tab_health_1")


# make group table
group_tab_body_1 <- group_tab(tab_body_1 , type = "RD")

# save
here_save(group_tab_body_1, "group_tab_body_1")

# make group table
group_tab_ego_1 <- group_tab(tab_ego_1, type = "RD")

# save
here_save(group_tab_ego_1, "group_tab_ego_1")

# make group table
group_tab_reflective_1 <-
  group_tab(tab_reflective_1, type = "RD")

# save
here_save(group_tab_reflective_1, "group_tab_reflective_1")

# make group table
group_tab_social_1 <- group_tab(tab_social_1, type = "RD")

# save
here_save(group_tab_social_1, "group_tab_social_1")


group_tab_health_1 <- here_read("group_tab_health_1")
group_tab_body_1 <- here_read("group_tab_body_1")
group_tab_ego_1 <- here_read("group_tab_ego_1")
group_tab_reflective_1 <- here_read("group_tab_reflective_1")
group_tab_social_1 <- here_read("group_tab_social_1")

A

# create plots -------------------------------------------------------------

f
f_1
# check N
n_participants
sub_title_1 = "Stop exercise: for those that exercise, shift weekly exercise down to 0 hours, else maintain expected exercise levels, N = 27,194"

# graph health
plot_group_tab_health_1 <- margot_plot(
  group_tab_health_1,
  type = "RD",
  title = "Health effects",
  subtitle = sub_title_1,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 15,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

library(patchwork)
plot_group_tab_health / plot_group_tab_health_1
dev.off()

# save graph
ggsave(
  plot_group_tab_health_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_health_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_body_1 <- margot_plot(
  group_tab_body_1,
  type = "RD",
  title = "Body effects",
  subtitle = sub_title_1,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_group_tab_body / plot_group_tab_body_1
# save graph
ggsave(
  plot_group_tab_body_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_body_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_body_1

# graph ego
plot_group_tab_ego_1 <- margot_plot(
  group_tab_ego_1,
  type = "RD",
  title = "Ego effects",
  subtitle = sub_title_1,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)
plot_group_tab_ego_1


plot_group_tab_ego / plot_group_tab_ego_1

# save graph
ggsave(
  plot_group_tab_ego_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_ego_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_group_tab_ego_1

# graph reflective
plot_group_tab_reflective_1 <- margot_plot(
  group_tab_reflective_1,
  type = "RD",
  title = "Reflective effects",
  subtitle = sub_title_1,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_reflective / plot_group_tab_reflective_1

# save graph
ggsave(
  plot_group_tab_reflective_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_reflective_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social_1 <- margot_plot(
  group_tab_social_1,
  type = "RD",
  title = "Social effects",
  subtitle = sub_title_1,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)

plot_group_tab_social / plot_group_tab_social_1

# save graph
ggsave(
  plot_group_tab_social_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_social_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# comparative intervention graphs -----------------------------------------

# combo graphs

plot_compare_health <- plot_group_tab_health / plot_group_tab_health_1 + plot_annotation(title = 
                                                                                           "Shift Intervention Comparisions", tag_level = "A")

plot_compare_health
ggsave(
  plot_compare_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_health.png",
  device = 'png',
  dpi = 600
)
dev.off()


plot_compare_body <- plot_group_tab_body + plot_group_tab_body_1  + plot_annotation(title = 
                                                                                      "Shift Intervention Comparisions", tag_level = "A")

plot_compare_body
ggsave(
  plot_compare_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()



plot_compare_ego <- plot_group_tab_ego + plot_group_tab_ego_1+ plot_annotation(title = 
                                                                                 "Shift Intervention Comparisions", tag_level = "A")


plot_compare_ego
ggsave(
  plot_compare_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()



plot_compare_reflective <- plot_group_tab_reflective + plot_group_tab_reflective_1+ plot_annotation(title = 
                                                                                                      "Shift Intervention Comparisions", tag_level = "A")

plot_compare_reflective
ggsave(
  plot_compare_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()




plot_compare_social  <-plot_group_tab_social + plot_group_tab_social_1+ plot_annotation(title = 
                                                                                          "Shift Intervention Comparisions", tag_level = "A")

plot_compare_social
ggsave(
  plot_compare_social,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()

# make tables shift 5 up vs 0 -------------------------------------------------------------

# don't forget to report smoking
# bind individual tables
out_tab_contrast_t2_alcohol_intensity_z_0

tab_health_0 <- rbind(
  #out_tab_contrast_t2_hours_exercise_log_z_0,
  out_tab_contrast_t2_alcohol_frequency_z_0,
  out_tab_contrast_t2_alcohol_intensity_z_0,
  out_tab_contrast_t2_bmi_z_0,
  out_tab_contrast_t2_hours_sleep_z_0,
  out_tab_contrast_t2_sfhealth_your_health_z_0
)
tab_health_0

here_save(tab_health_0, "tab_health_0")

tab_body_0 <- rbind(
  out_tab_contrast_t2_bodysat_z_0,
  out_tab_contrast_t2_kessler_latent_anxiety_z_0,
  out_tab_contrast_t2_kessler_latent_depression_z_0,
  out_tab_contrast_t2_hlth_fatigue_z_0,
  out_tab_contrast_t2_rumination_z_0,
  out_tab_contrast_t2_sexual_satisfaction_z_0
)
tab_body_0

here_save(tab_body_0, "tab_body_0")

tab_ego_0 <- rbind(
  #out_tab_contrast_t2_emotion_regulation_out_control_z_0,
  #out_tab_contrast_t2_permeability_individual_z_0,
  out_tab_contrast_t2_perfectionism_z_0,
  #out_tab_contrast_t2_power_no_control_composite_z_0,
  out_tab_contrast_t2_self_control_have_lots_z_0,
  out_tab_contrast_t2_self_control_wish_more_reversed_z_0,
  out_tab_contrast_t2_self_esteem_z_0
)

tab_ego_0

here_save(tab_ego_0, "tab_ego_0")

tab_reflective_0 <- rbind(
  out_tab_contrast_t2_gratitude_z_0,
  out_tab_contrast_t2_meaning_purpose_z_0,
  out_tab_contrast_t2_meaning_sense_z_0,
  out_tab_contrast_t2_pwb_your_future_security_z_0,
  out_tab_contrast_t2_pwb_your_health_z_0,
  out_tab_contrast_t2_pwb_your_relationships_z_0,
  out_tab_contrast_t2_pwb_standard_living_z_0,
  out_tab_contrast_t2_lifesat_z_0
  # out_tab_contrast_t2_vengeful_rumin_z_0
)
tab_reflective_0

here_save(tab_reflective_0, "tab_reflective_0")

tab_social_0 <- rbind(
  out_tab_contrast_t2_belong_z_0,
  out_tab_contrast_t2_neighbourhood_community_z_0,
  out_tab_contrast_t2_support_z_0
)
tab_social_0
here_save(tab_social_0, "tab_social_0")

# make group table
group_tab_health_0 <- group_tab(tab_health_0, type = "RD")

# save
here_save(group_tab_health_0, "group_tab_health_0")


# make group table
group_tab_body_0 <- group_tab(tab_body_0 , type = "RD")

# save
here_save(group_tab_body_0, "group_tab_body_0")

# make group table
group_tab_ego_0 <- group_tab(tab_ego_0, type = "RD")

# save
here_save(group_tab_ego_0, "group_tab_ego_0")

# make group table
group_tab_reflective_0 <-
  group_tab(tab_reflective_0, type = "RD")

# save
here_save(group_tab_reflective_0, "group_tab_reflective_0")

# make group table
group_tab_social_0 <- group_tab(tab_social_0, type = "RD")

# save
here_save(group_tab_social_0, "group_tab_social_0")


group_tab_health_0 <- here_read("group_tab_health_0")
group_tab_body_0 <- here_read("group_tab_body_0")
group_tab_ego_0 <- here_read("group_tab_ego_0")
group_tab_reflective_0 <- here_read("group_tab_reflective_0")
group_tab_social_0 <- here_read("group_tab_social_0")





# create plots -------------------------------------------------------------

# check N
n_participants
sub_title_0 = "Exercise at least 5 hours a week vs. no exercise, N = 27,194"



# graph health
plot_group_tab_health_0 <- margot_plot(
  group_tab_health_0,
  type = "RD",
  title = "Health effects",
  subtitle = sub_title_0,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 15,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)

library(patchwork)
plot_group_tab_health_0

plot_group_tab_health / plot_group_tab_health_1
dev.off()

# save graph
ggsave(
  plot_group_tab_health_0,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_health_0.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_body_0 <- margot_plot(
  group_tab_body_0,
  type = "RD",
  title = "Body effects",
  subtitle = sub_title_0,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)

plot_group_tab_body_0

plot_group_tab_body / plot_group_tab_body_1
# save graph
ggsave(
  plot_group_tab_body_0,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_body_0.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_body_0

# graph ego
plot_group_tab_ego_0 <- margot_plot(
  group_tab_ego_0,
  type = "RD",
  title = "Ego effects",
  subtitle = sub_title_0,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)
plot_group_tab_ego_0


plot_group_tab_ego / plot_group_tab_ego_1

# save graph
ggsave(
  plot_group_tab_ego_0,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_ego_0.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_group_tab_ego_0

group_tab_reflective_0
# graph reflective
plot_group_tab_reflective_0 <- margot_plot(
  group_tab_reflective_0,
  type = "RD",
  title = "Reflective effects",
  subtitle = sub_title_0,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)
plot_group_tab_reflective_0

plot_group_tab_reflective / plot_group_tab_reflective_1

# save graph
ggsave(
  plot_group_tab_reflective_0,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_reflective_0.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social_0 <- margot_plot(
  group_tab_social_0,
  type = "RD",
  title = "Social effects",
  subtitle = sub_title_0,
  # xlab = "",
  # ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.3,
  x_lim_lo = -.3,
  x_lim_hi =  .3
)
plot_group_tab_social_0

plot_group_tab_social / plot_group_tab_social_1

# save graph
ggsave(
  plot_group_tab_social_0,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_social_0.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# comparative intervention graphs -----------------------------------------

# combo graphs
#not updating rn

plot_compare_health <- plot_group_tab_health / plot_group_tab_health_1 + plot_annotation(title = 
                                                                                           "Shift Intervention Comparisions", tag_level = "A")

plot_compare_health
ggsave(
  plot_compare_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_health.png",
  device = 'png',
  dpi = 600
)
dev.off()


plot_compare_body <- plot_group_tab_body + plot_group_tab_body_1  + plot_annotation(title = 
                                                                                      "Shift Intervention Comparisions", tag_level = "A")

plot_compare_body
ggsave(
  plot_compare_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()



plot_compare_ego <- plot_group_tab_ego + plot_group_tab_ego_1+ plot_annotation(title = 
                                                                                 "Shift Intervention Comparisions", tag_level = "A")


plot_compare_ego
ggsave(
  plot_compare_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()



plot_compare_reflective <- plot_group_tab_reflective + plot_group_tab_reflective_1+ plot_annotation(title = 
                                                                                                      "Shift Intervention Comparisions", tag_level = "A")

plot_compare_reflective
ggsave(
  plot_compare_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()




plot_compare_social  <-plot_group_tab_social + plot_group_tab_social_1+ plot_annotation(title = 
                                                                                          "Shift Intervention Comparisions", tag_level = "A")

plot_compare_social
ggsave(
  plot_compare_social,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# Outcome table -----------------------------------------------------------

## Making tables --------------------------------------------------------------

push_mods
#read the data
group_tab_health_0 <- here_read("group_tab_health_0")
group_tab_body_0 <- here_read("group_tab_body_0")
group_tab_ego_0 <- here_read("group_tab_ego_0")
group_tab_reflective_0 <- here_read("group_tab_reflective_0")
group_tab_social_0 <- here_read("group_tab_social_0")


group_tab_reflective_0 <- group_tab_reflective_0 %>%
  mutate(outcome = case_when(outcome == "PWB your relationships" ~ "Relationship satisfaction (PWB)", #text asks "Please rate your level of satisfaction with the following aspects of your life and New Zealand."
                             outcome == "PWB your health" ~ "Health satisfaction (PWB)",
                             outcome == "PWB your standard living" ~ "Standard of living satisfaction (PWB)",
                             outcome == "PWB your future security" ~ "Future security satisfaction (PWB)",
                             #outcome == "Vengefulness (forgiveness" ~ "Vengefulness",
                             TRUE ~ outcome))

group_tab_health_0 <- group_tab_health_0 %>%
  mutate(outcome = case_when(outcome == "Short form health, your health" ~ "Subjective health",
                             outcome == "BMI" ~ "Body-Mass Index (BMI)",
                             TRUE ~ outcome))

group_tab_ego_0 <- group_tab_ego_0 %>%
  mutate(outcome = case_when(outcome == "Self esteem" ~ "Self-esteem",
                             outcome == "Permeability self"  ~ "Social mobility (permeability)",
                             outcome == "Self control have" ~ "Self-control (have)",
                             outcome == "Self control wish more (reversed)" ~ "Self-control (wish more, reversed)",
                             outcome == "Emotional regulation (out of control)" ~"Emotional dysregulation",
                             TRUE ~ outcome))
group_tab_body_0 <- group_tab_body_0 %>%
  mutate(outcome = case_when(outcome == "Kessler 6 anxiety" ~ "Anxiety (Kessler-6)",
                             outcome == "Kessler 6 depression" ~ "Depression (Kessler-6)",
                             TRUE ~ outcome))

# clean
tab_all_grouped <- rbind(group_tab_reflective_0, group_tab_health_0, group_tab_social_0, group_tab_ego_0, group_tab_body_0)
tab_all_grouped
tab_all <- tab_all_grouped %>% 
  select(-Estimate, -estimate_lab#,-group
  ) %>% 
  rename(Estimate = `E[Y(1)]-E[Y(0)]`) %>%
  rename(Outcome = outcome) %>%
  rename("E-Value" = E_Value) %>%
  rename("E-Value Limit" = E_Val_bound)

tab_all


# merging columns for table---------------------------------------------------------

input_char <- tab_all
#Converts columns to strings and .3f means it gives 3 decimal places
input_char$Estimate <- sprintf("%.2f", tab_all$Estimate)
input_char$`2.5 %` <- sprintf("%.2f", tab_all$`2.5 %`)
input_char$`97.5 %` <- sprintf("%.2f", tab_all$`97.5 %`)
input_char <- unite(input_char, col="Estimate (95% CI)", c("Estimate", "2.5 %", "97.5 %"), sep=', ')
input_char$`Estimate (95% CI)` <- sub(",", " (", input_char$`Estimate (95% CI)`)
#new_tab$Estimand <- gsub('^(.{22})(.*)$', '\\1)\\2', new_tab$Estimand)                               # Print new string
input_char$`Estimate (95% CI)` <- paste0(input_char$`Estimate (95% CI)`, ")")
tab_all <- input_char
tab_all


kable(tab_all, format = "html", #caption = "Table 2. Belonging effects on multi-dimensional well-being", 
      booktabs = T) %>%
  #kable_classic(full_width = F, html_font = "Times New Roman") %>% 
  kable_styling(full_width = F) %>%
  kableExtra::group_rows("Reflective", 1,8) %>%
  kableExtra::group_rows("Health", 9,13) %>%
  kableExtra::group_rows("Social", 14,16) %>%
  kableExtra::group_rows("Self", 17,20) %>%
  kableExtra::group_rows("Body", 21,26) #%>% #uncomment this to save png
save_kable(here::here(push_mods, "results_table.png"))

tab_all


# interpretations ---------------------------------------------------------

margot_interpret_table(tab_all_grouped, estimand = "PATE", causal_scale = "causal_difference")
?margot_interpret_table
