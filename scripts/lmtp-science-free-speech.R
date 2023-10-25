# sept 13 2023
# science/ speech
# joseph bulbulia : joseph.bulbulia@gmail.com


# outcome-wide-analysis-template
# Sept 11, 2023

# preliminaries -----------------------------------------------------------


# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")


# WARNING: UNCOMMENT THIS AND DOWNLOAD THE LIBRARIES FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# WARNING: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# experimental functions
source(
  "/Users/joseph/GIT/templates/functions/experimental_funs.R"
)

## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE.
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
  )

# read data: note that you need use the arrow package in R
dat <- arrow::read_parquet(pull_path)

### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/notes/23-lmtp-science-free-speech"
  )

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods

# set exposure here
nzavs_exposure <-
  "trust_science_high_confidence_scientific_community"

# define exposure
A <-
  "t1_trust_science_high_confidence_scientific_community"

A_1 <- "t1_free_speech"

# define exposure
A_z <-
  "t1_trust_science_high_confidence_scientific_community_z"

A_1_z  <- "t1_free_speech_z"



# define shift function (if any one is average make them average, otherwise leave alone)
# f <- function(data, trt) {
#   ifelse(data[[trt]] <= 0, 0,  data[[trt]])
# }


# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 5

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)

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

# boost spped
SL.xgboost = list(tree_method = 'gpu_hist')


# check options
listWrappers()

n_unique(dat$id)



# import data and wrangle-------------------------------------------------

dat_long  <- dat |>
  rowwise(wave) |>
  mutate(power_no_control_composite =
           mean(c(
             power_self_nocontrol, power_others_control
           ), na.rm = TRUE)) |>
  rename(
    trust_science_high_confidence_scientific_community = science_trust01,
    trust_science_our_society_places_too_much_emphasis_reversed =
      science_trust02r
  ) |>
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
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    "free_speech",
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
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "warm_asians",
    "warm_chinese",
    #"warm_disabled" ,  missing at time 0
    # begins w9
    "warm_immigrants",
    "warm_indians",
    "warm_elderly",
    # warm_lgbtq starts w12
    "warm_maori",
    "warm_mental_illness",
    "warm_muslims",
    "warm_nz_euro",
    "warm_overweight",
    "warm_pacific",
    "warm_refugees",
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
    "support_noguidance_reverseed",
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
    # "I have a high degree of confidence in the scientific community",
    "trust_science_high_confidence_scientific_community",
    #Our society places too much emphasis on science.
    "trust_science_our_society_places_too_much_emphasis_reversed",
    "alert_level_combined"
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
  dplyr::filter((wave == 2019 & year_measured  == 1) |
                  (wave == 2020  &
                     year_measured  == 1) |
                  (wave == 2021)) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  group_by(id) |>
  ## MAKE SURE YOU HAVE ELIGIBILITY CRITERIA
  dplyr::mutate(meets_criteria_baseline = ifelse(year_measured == 1 &
                                                   !is.na(!!sym(nzavs_exposure)) 
                                                   & !is.na(free_speech), 1, 0)) |>  # using R lang
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id) |>
  filter((wave == 2019 & year_measured == 1) |
           (wave == 2020 & year_measured == 1) |
           (wave == 2021)) %>%
  group_by(id) |>
  mutate(k_19 = ifelse(wave == 2019 &
                         meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_19 = mean(k_19, na.rm = TRUE)) %>%
  mutate(k_20 = ifelse(wave == 2020 &
                         meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_20 = mean(k_20, na.rm = TRUE)) %>%
  dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
  dplyr::filter(h_20 > 0) |>  # hack to enable repeat of baseline
  ungroup() |> 
  arrange(id, wave) |> 
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
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
  select(-h_19, -k_19, -h_20, -k_20) |>
  data.frame() |>
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
  # eth_cat = as.integer(eth_cat),
  urban = as.numeric(urban),
  education_level_coarsen = as.integer(education_level_coarsen)
) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()


# check n
N <- n_unique(dat_long$id) # 31557
N

table(dat_long$wave)


N <- n_unique(dat_long$id)
N


# factors 
#
# dt_only_k6 <- dt_19 |> select(kessler_depressed, kessler_effort,kessler_hopeless,
#                                  kessler_worthless, kessler_nervous,
#                                  kessler_restless)
#
#
# # check factor structure
# performance::check_factorstructure(dt_only_k6)
#
# # explore a factor structure made of 3 latent variables
# efa <- psych::fa(dt_only_k6, nfactors = 2) %>%
#   model_parameters(sort = TRUE, threshold = "max")
#
# efa
#
#
# n <- n_factors(dt_only_k6)
#
# # plot
# plot(n) + theme_classic()
#
# # CFA
# part_data <- datawizard::data_partition(dt_only_k6, traing_proportion = .7, seed = seed)
#
#
# # set up training data
# training <- part_data$p_0.7
# test <- part_data$test
#
#
# # one factor model
# structure_k6_one <- psych::fa(training, nfactors = 1) |>
#   efa_to_cfa()
#
# # two factor model model
# structure_k6_two <- psych::fa(training, nfactors = 2) |>
#   efa_to_cfa()
#
# # three factor model
# structure_k6_three <- psych::fa(training, nfactors = 3) %>%
#   efa_to_cfa()
#
# # inspect models
# structure_k6_one
# structure_k6_two
# structure_k6_three
#
#
# # Next we perform the confirmatory factor analysis.
#
#
# one_latent <-
#   suppressWarnings(lavaan::cfa(structure_k6_one, data = test))
#
# # two latents model
# two_latents <-
#   suppressWarnings(lavaan::cfa(structure_k6_two, data = test))
#
# # three latents model
# three_latents <-
#   suppressWarnings(lavaan::cfa(structure_k6_three, data = test))
#
#
# # compare models
# compare <-
#   performance::compare_performance(one_latent, two_latents, three_latents, verbose = FALSE)
#
# # view as html table
# as.data.frame(compare) |>
#   kbl(format = "markdown")
#

dt_19 <- dat_long |>
  filter(year_measured == 1 & wave == 2020) |> 
  mutate(trust_science_high_confidence_scientific_community_z = scale(trust_science_high_confidence_scientific_community), 
         free_speech_z = scale(free_speech))

hist(dt_19$trust_science_high_confidence_scientific_community_z)
hist(dt_19$free_speech)

hist(dt_19$trust_science_high_confidence_scientific_community)

table(dt_19$trust_science_high_confidence_scientific_community_z)


mean_exposure_science <- mean(dt_19$trust_science_high_confidence_scientific_community,
                      na.rm = TRUE)

# just to view, do not use in function
mean_exposure_science



mean_exposure_free_speech <- mean(dt_19$trust_science_high_confidence_scientific_community,
                              na.rm = TRUE)

# just to view, do not use in function
mean_exposure_free_speech 



# make sure to use the sd

min_score_science_z <- min(dt_19$trust_science_high_confidence_scientific_community_z, na.rm = TRUE)
min_score_science_z

max_score_science_z <- max(dt_19$trust_science_high_confidence_scientific_community_z, na.rm = TRUE)
max_score_science_z

sd_exposure_science <- sd(dt_19$trust_science_high_confidence_scientific_community,
                  na.rm = TRUE)
sd_exposure_science

one_point_in_sd_units_science <- 1/sd_exposure_science
one_point_in_sd_units_science


min_score_free_speech_z <- min(dt_19$free_speech_z, na.rm = TRUE)
min_score_free_speech_z

max_score_free_speech_z<- max(dt_19$free_speech_z, na.rm = TRUE)
max_score_free_speech_z

sd_exposure_free_speech <- sd(dt_19$free_speech,
                          na.rm = TRUE)
one_point_in_sd_units_free_speech

one_point_in_sd_units_free_speech <- 1/one_point_in_sd_units_free_speech
one_point_in_sd_units_free_speech



# functions 



# generate bar plot
graph_density_of_exposure <- coloured_histogram(dt_19, 
                                                col_name = "trust_science_high_confidence_scientific_community", 
                                                scale_min = min_score, scale_max = max_score)

graph_density_of_exposure

ggsave(
  graph_density_of_exposure,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

graph_density_of_exposure_free_speech <- coloured_histogram(dt_19, 
                                                col_name = "free_speech", 
                                                scale_min = min_score, scale_max = max_score)

graph_density_of_exposure_free_speech

ggsave(
  graph_density_of_exposure_free_speech,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure_free_speech.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# check sample 
N <-n_unique(dat_long$id) #31557 
N

# double check path
push_mods

dev.off()
# check
dt_check_exposure <- dat_long |> filter(wave == 2019| wave == 2020)

nzavs_exposure
nzavs_exposure
variable_to_check <- sym("trust_science_high_confidence_scientific_community")
variable_to_check
# makes sure all is false

missing_values_table <- table(is.na(dt_check_exposure$trust_science_high_confidence_scientific_community))
missing_values_table

# makes sure all is false
dt_positivity_full <- dat_long |>
  filter(wave == 2019 | wave == 2020) |>
  select(wave, id, trust_science_high_confidence_scientific_community, free_speech,sample_weights) |> 
  mutate(trust_science_high_confidence_scientific_community_round = round(trust_science_high_confidence_scientific_community, 0))

dt_positivity_full

# check sample weights NA - will return to this after impute
table (is.na(dt_positivity_full$trust_science_high_confidence_scientific_community)) # 

# test positivity
out <-
  msm::statetable.msm(trust_science_high_confidence_scientific_community, id, data = dt_positivity_full)
out
# transition table

t_tab <- transition_table(out, state_names = NULL)
t_tab


out <-
  msm::statetable.msm(free_speech, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab



nzavs_exposure
dt_check_exposure <- dat_long |>   filter(wave == 2018 | wave == 2019) 

# makes sure all is false
table (is.na(dt_check_exposure$religion_identification_level))

dt_positivity_full <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  select(wave, id, religion_identification_level, religion_identification_level) |> 
  mutate(religion_identification_level_round = round(religion_identification_level, 0))

dt_positivity_full

# no need for the rounded variable
table(dt_positivity_full$religion_identification_level)

# check sample weights NA - will return to this after impute
table (is.na(dt_positivity_full$sample_weights)) # 

# test positivity
out <-
  msm::statetable.msm(religion_identification_level, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab

# set variables for baseline exposure and outcome -------------------------


baseline_vars = c(
  "male",
  "age",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  "sample_origin",
  "nz_dep2018",
  "nzsei13",
  #"total_siblings_factor",
  "born_nz",
  "education_level_coarsen",
  #"hlth_disability",
  #  "hlth_bmi",
  # bmi
  # "pwi", # pwi
 # "kessler6_sum",
  #  "support", #soc support
  #  "belong", # social belonging
  #  "smoker", # smoker
  # "sfhealth",
  #
  # "alcohol_frequency", measured with error
  # "alcohol_intensity",
  # "hours_family_log",
  #  "hours_friends_log",
  #  "hours_community_log",
  # "hours_community_sqrt_round",
  # "lifemeaning",
  "household_inc_log",
  # added: measured with error but OK for imputations
  "partner",
  "parent",  # newly changed - have information in child number
  "political_conservative",
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban",
 # "children_num",
  "parent",
  #  "hours_children_log",
  # new
  #  "hours_work_log",
  # new
  # "hours_housework_log",
  #new
  #  "hours_exercise_log",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty",
  # I want people to know that I am an important person of high status, I am an ordinary person who is no better than others. , I wouldn’t want people to treat me as though I were superior to them. I think that I am entitled to more respect than the average person is.
  # "religion_religious", # Do you identify with a religion and/or spiritual group?
  # "religion_identification_level", #How important is your religion to how you see yourself?"  # note this is not a great measure of virtue, virtue is a mean between extremes.
  #  "religion_church_round",
  # "religion_religious", #
  #  "religion_spiritual_identification", Not measured
  "religion_identification_level",
  #"religion_religious",
 # "religion_church_binary",
  #  "religion_prayer_binary",
  #  "religion_scripture_binary",
  # "religion_believe_god",
 # "religion_believe_spirit",
  # "I have a high degree of confidence in the scientific community",
 # "trust_science_high_confidence_scientific_community",
  ##Our society places too much emphasis on science.
  #"trust_science_our_society_places_too_much_emphasis_reversed",
  #"free_speech",
  "sample_weights",
  "alert_level_combined"
)
# check
baseline_vars

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("trust_science_our_society_places_too_much_emphasis_reversed", 
                 "trust_science_high_confidence_scientific_community",
                 "free_speech", 
                 "not_lost") #


# outcomes
outcome_vars = c(
  "free_speech",
  "trust_science_our_society_places_too_much_emphasis_reversed", 
  "trust_science_high_confidence_scientific_community"
)




# make data wide and impute baseline missing values -----------------------

# custom function
rm(prep_coop_all)
prep_coop_all <- margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)


# check mi model
# outlist <-
#   row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
# 
# length(outlist)
# 
# # checks. We do not impute with weights: area of current research
# head(prep_coop_all$loggedEvents, 3)
# 
# logged_events <- prep_coop_all$loggedEvents
# print(logged_events)

push_mods
# save function -- will save to your "push_mod" directory
here_save(prep_coop_all, "prep_coop_all")

# read function
prep_coop_all <- here_read("prep_coop_all")

table( prep_coop_all$t1_not_lost )

head(prep_coop_all)


# find  columns with NA in t0
cols_with_na <- prep_coop_all %>%
  select(starts_with("t2_")) %>%
  summarise(across(everything(), ~any(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "col_name", values_to = "has_na") %>%
  filter(has_na == TRUE) %>%
  pull(col_name)

# show results
print(cols_with_na)


#hack 

naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)
dev.off()


#check must be a dataframe
str(prep_coop_all)
nrow(prep_coop_all)
colnames(prep_coop_all)


# arrange data for analysis -----------------------------------------------
# spit and shine


# uncomment if speed is not an issue
# unique_levels <- sort(unique(prep_coop_all$t0_education_level_coarsen))
# prep_coop_all$t0_education_level_coarsen <- factor(prep_coop_all$t0_education_level_coarsen,
#                                                    levels = unique_levels,
#                                                    ordered = TRUE)

str(prep_coop_all$t0_education_level_coarsen)

df_wide_censored <-
  prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat)    
    #   t0_smoker_binary = as.integer(ifelse(t0_smoker > 0, 1, 0))#,
    #    t2_smoker_binary = as.integer(ifelse(t2_smoker > 0, 1, 0)),
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_"))
#check
head(df_wide_censored)
dim(df_wide_censored)
str(df_wide_censored)

exposure_var
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
       !t0_education_level_coarsen,
      #  !t0_smoker_binary & 
      #  !t1_trust_science_our_society_places_too_much_emphasis_reversed,
      #   !t2_smoker_binary,
    #  !t1_trust_science_our_society_places_too_much_emphasis_reversed  &
     # !t1_trust_science_high_confidence_scientific_community  &
    #  !t1_free_speech,
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    #  t0_smoker_binary,
    t0_not_lost,
    t0_sample_weights,
    t0_education_level_coarsen,
  #  t1_trust_science_our_society_places_too_much_emphasis_reversed,
    t1_trust_science_high_confidence_scientific_community,
    t1_free_speech,
    t1_not_lost,
    #   t2_smoker_binary,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  mutate(t0_sample_weights = as.numeric(t0_sample_weights)) |>
  data.frame()

dim(df_clean)
naniar::vis_miss(df_clean, warn_large_data = FALSE)

table(df_clean$t1_trust_science_high_confidence_scientific_community)


# check path
push_mods

# save
here_save(df_clean, "df_clean")

# read if needed
df_clean <- here_read("df_clean")


#check n
nrow(df_clean) #32737

df_clean<- data.frame(df_clean)

colnames(df_clean)
# get names
names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost ) |>  colnames()

names_base
names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()


names_outcomes
# set variables for models ------------------------------------------------

#### SET VARIABLE NAMES: Customise for each outcomewide model
#  model
A <- "t1_trust_science_high_confidence_scientific_community"
A_z <-"t1_trust_science_high_confidence_scientific_community_z"
C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))
head( df_clean$t1_trust_science_our_society_places_too_much_emphasis_reversed_z )

# check
print(W)

# check function
# shift function -- what if everyone increased by .5 standard deviation, except those above 2

# SHIFT FUNCTION
# simple shift, everyone goes to church at least 4 times per week

# f <- function(data, trt) {
#   ifelse(data[[trt]] <= .5, .5,  data[[trt]])
# }
# simple function # add 1 to all
# f_1 <- function (data, trt) data[[trt]] + 1



f <- function(data, trt) {
  ifelse(data[[trt]] <= 7 - 1, data[[trt]] + 1,  data[[trt]])
}


# shift all to at least the mean
f_z <- function(data, trt) {
  ifelse(data[[trt]] <= 0,  0,  data[[trt]])
}


# check assignments
f
A
names_outcomes
C

names_base

baseline_vars

A
A_1

## QUICK TEST

# linear model ------------------------------------------------------------

# create censoring variables, censoring weights
covariates <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost#, 
                   #  -t0_alert_level_combined,
                  #   -t0_sample_origin,
                  #   -t0_education_level_coarsen
                  ) |>  colnames()

covariates

# for regression
covariate_formula <- paste(covariates, collapse = " + ")
covariate_formula

covariate_formula
full_formula_t1_not_lost <- paste("t1_not_lost ~", covariate_formula)

fit_t1 <- glm(as.formula(full_formula_t1_not_lost), family = binomial(link = "logit"), data = df_clean)

# 
model_parameters(fit_t1, ci_method = "wald", exponentiate = TRUE)[25,]

# get iptw
library(ipw)

# Your Numerator and Denominator
numerator_formula <- ~ 1
denominator_formula <- as.formula(paste("~", covariate_formula))
denominator_formula
# Run ipwpoint
# Run ipwpoint
ipw_t1 <- ipw::ipwpoint(
  exposure = t1_not_lost, 
  family = "binomial",
  link = "logit",
  denominator =  ~t0_eth_cat + t0_male_z + t0_age_z + t0_nz_dep2018_z + t0_nzsei13_z + 
    t0_born_nz_z + t0_household_inc_log_z + t0_partner_z + t0_parent_z + 
    t0_political_conservative_z + t0_urban_z + t0_agreeableness_z + 
    t0_conscientiousness_z + t0_extraversion_z + t0_honesty_humility_z + 
    t0_openness_z + t0_neuroticism_z + t0_modesty_z + t0_religion_identification_level_z + 
 #   t0_trust_science_our_society_places_too_much_emphasis_reversed_z + 
    t0_trust_science_high_confidence_scientific_community_z + 
    t0_free_speech_z,
  data = df_clean
)

df_clean$ipw_t1 <- ipw_t1$ipw

df_clean$t0_composite_weight <- df_clean$ipw_t1 * df_clean$t0_sample_weights

hist( df_clean$t0_composite_weight)




## CROSS SECTIONAL 
mr_0 <- lm(t0_trust_science_high_confidence_scientific_community_z  ~t0_eth_cat + 
             t0_alert_level_combined + 
             t0_education_level_coarsen + 
             t0_male_z + t0_age_z + t0_nz_dep2018_z + t0_nzsei13_z + t0_born_nz_z + 
             t0_household_inc_log_z + t0_partner_z + t0_parent_z + t0_political_conservative_z + 
             t0_urban_z + t0_agreeableness_z + t0_conscientiousness_z + 
             t0_extraversion_z + t0_honesty_humility_z + t0_openness_z + 
             t0_neuroticism_z + t0_modesty_z + t0_religion_identification_level_z + 
            # t0_trust_science_our_society_places_too_much_emphasis_reversed_z + 
             t0_free_speech_z, weights = t0_composite_weight,
           data = df_clean)

#model_parameters( mr_0)[2,]

plot(model_parameters( mr_0))


mr_1 <- lm(t0_free_speech_z  ~t0_eth_cat + 
             t0_alert_level_combined + 
             t0_education_level_coarsen + 
             t0_male_z + t0_age_z + t0_nz_dep2018_z + t0_nzsei13_z + t0_born_nz_z + 
             t0_household_inc_log_z + t0_partner_z + t0_parent_z + t0_political_conservative_z + 
             t0_urban_z + t0_agreeableness_z + t0_conscientiousness_z + 
             t0_extraversion_z + t0_honesty_humility_z + t0_openness_z + 
             t0_neuroticism_z + t0_modesty_z + t0_religion_identification_level_z + 
             #  t0_trust_science_our_society_places_too_much_emphasis_reversed_z + 
             t0_trust_science_high_confidence_scientific_community_z, weights = t0_composite_weight,
           data = df_clean)

#model_parameters( mr_0)[2,]

plot(model_parameters( mr_1))



mr_1 <- lm( t0_modesty_z ~t0_eth_cat + 
             t0_alert_level_combined + 
             t0_education_level_coarsen + 
             t0_male_z + t0_age_z + t0_nz_dep2018_z + t0_nzsei13_z + t0_born_nz_z + 
             t0_household_inc_log_z + t0_partner_z + t0_parent_z + t0_political_conservative_z + 
             t0_urban_z + t0_agreeableness_z + t0_conscientiousness_z + 
             t0_extraversion_z + t0_honesty_humility_z + t0_openness_z + 
             t0_neuroticism_z + t0_modesty_z  + 
             t0_religion_identification_level_z + 
             t0_trust_science_high_confidence_scientific_community_z, weights = t0_composite_weight,
           data = df_clean)

#model_parameters( mr_0)[2,]

plot(model_parameters( mr_1))








# nothing 
mr_1 <- lm(t2_trust_science_high_confidence_scientific_community_z ~t1_free_speech *
                 (t0_eth_cat + t0_male_z + t0_age_z + t0_nz_dep2018_z + t0_nzsei13_z + 
                    t0_born_nz_z + t0_household_inc_log_z + t0_partner_z + t0_parent_z + 
                    t0_political_conservative_z + t0_urban_z + t0_agreeableness_z + 
                    t0_conscientiousness_z + t0_extraversion_z + t0_honesty_humility_z + 
                    t0_openness_z + t0_neuroticism_z + t0_modesty_z + t0_religion_identification_level_z + 
                    t0_trust_science_our_society_places_too_much_emphasis_reversed_z + 
                    t0_trust_science_high_confidence_scientific_community_z + 
                    t0_free_speech_z) , weights = t0_composite_weight,
                                         data = df_clean)

model_parameters( mr_1 )[2,]

plot(model_parameters( mr_1))




mr_2 <- lm(t2_free_speech_z ~ t1_trust_science_high_confidence_scientific_community_z *
           (t0_eth_cat + t0_male_z + t0_age_z + t0_nz_dep2018_z + t0_nzsei13_z + 
              t0_born_nz_z + t0_household_inc_log_z + t0_partner_z + t0_parent_z + 
              t0_political_conservative_z + t0_urban_z + t0_agreeableness_z + 
              t0_conscientiousness_z + t0_extraversion_z + t0_honesty_humility_z + 
              t0_openness_z + t0_neuroticism_z + t0_modesty_z + t0_religion_identification_level_z + 
              t0_trust_science_our_society_places_too_much_emphasis_reversed_z + 
              t0_trust_science_high_confidence_scientific_community_z + 
              t0_free_speech_z) , weights = t0_composite_weight,
         data = df_clean)

# nothing
model_parameters( mr_1 )[2,]

plot(model_parameters( mr_2))

dev.off()

dat_19 <- dat_long |> 
  filter(wave == 2019)

model_parameters(
  mt_0 <-  lm(t2_trust_science_high_confidence_scientific_community_z ~ bs(t1_free_speech_z), data = dat_19)
)

plot( ggeffects::ggeffect(mt_0, terms = c("t1_free_speech_z")))


model_parameters(
  mt_0 <-  lm(t2_trust_science_high_confidence_scientific_community_z ~ bs(t1_free_speech_z), data = dat_19)
)

plot( ggeffects::ggeffect(mt_0, terms = c("t1_free_speech_z")))




model_parameters(
  mt_2  <- lm(t2_free_speech_z ~ bs(t1_trust_science_high_confidence_scientific_community),  
              weights = t0_composite_weight,data = df_clean)
)




model_parameters(
mt_1 <-  lm(t2_trust_science_high_confidence_scientific_community_z ~ bs(t1_free_speech_z), data = df_clean)
)

plot( ggeffects::ggeffect(mt_1, terms = c("t1_free_speech_z")))


model_parameters(
  mt_2  <- lm(t2_free_speech_z ~ bs(t1_trust_science_high_confidence_scientific_community),  
     weights = t0_composite_weight,data = df_clean)
)

plot( ggeffects::ggeffect(mt_2, terms = c("t1_trust_science_high_confidence_scientific_community")))


model_parameters(
   lm(trust_science_high_confidence_scientific_community ~ time, data = dat_long_2)
)


model_parameters(
  lm(
    free_speech ~ time, data = dat_long_2)
)

model_parameters(
  lm(
    free_speech ~ trust_science_high_confidence_scientific_community * time, data = dat_long_2)
)

model_parameters(
  lm(
    trust_science_high_confidence_scientific_community ~ free_speech * time, data = dat_long_2)
)



# test
model_parameters(
  lm(
    t2_free_speech_z ~ t1_trust_science_high_confidence_scientific_community *
      (t0_trust_science_high_confidence_scientific_community_z + t0_free_speech_z +
      t0_age_z + 
      t0_male_z + 
      t0_political_conservative_z), data = df_clean)
)

model_parameters(
  lm(
    t2_trust_science_high_confidence_scientific_community_z ~ t1_free_speech *
      (t0_trust_science_high_confidence_scientific_community_z + t0_free_speech_z +
      t0_age_z + 
      t0_male_z + 
      t0_political_conservative_z), data = df_clean)
)

dat_long_2 <-  dat_long |> mutate(time = as.numeric(wave)-1)

# this 

model_parameters(
  m_time_science_confidence<- lm(
    trust_science_high_confidence_scientific_community ~ bs(time), 
    data = dat_long_2
  )
)

plot( ggeffects::ggeffect(m_time_science_confidence, terms = c("time")))


model_parameters(
  m_time_speech <- lm(
    free_speech ~ bs(time), 
    data = dat_long_2
  )
)

plot( ggeffects::ggeffect(m_time_science_confidence, terms = c("time"))) + scale_y_continuous(limits = c(1,7))
plot( ggeffects::ggeffect(m_time_speech, terms = c("time"))) + scale_y_continuous(limits = c(1,7))




model_parameters(
 m1<- lm(
    trust_science_high_confidence_scientific_community ~ bs(free_speech) * bs(time), 
    data = dat_long_2
  )
)

plot( ggeffects::ggeffect(m1, terms = c( "free_speech", "time"))) + scale_y_continuous(limits = c(1,7))



model_parameters(
 m2 <-  lm(
    free_speech ~ trust_science_high_confidence_scientific_community * time, 
    data = dat_long_2
  )
)


model_parameters(
  m3 <-  lm(
    free_speech ~ bs(trust_science_high_confidence_scientific_community) * bs(time), 
    data = dat_long_2
  )
)

plot( ggeffects::ggeffect(m3, terms = c( "trust_science_high_confidence_scientific_community", "time"))) + scale_y_continuous(limits = c(1,7))



m4 <-  lm(
    trust_science_high_confidence_scientific_community ~ bs(free_speech) * bs(time), 
    data = dat_long_2
  )
)

plot( ggeffects::ggeffect(m4, terms = c( "free_speech", "time")))+ scale_y_continuous(limits = c(1,7))



# this looks like attrition 
plot( ggeffects::ggeffect(m1, terms = c( "free_speech", "time"))) + scale_y_continuous(limits = c(1,7))


# this looks like attrition 
plot( ggeffects::ggeffect(m2, terms = c("trust_science_high_confidence_scientific_community", "time")) )  + scale_y_continuous(limits = c(1,7))






# test --------------------------------------------------------------------

df_clean_test <- df_clean |>
  slice_head(n = 2000)






# FREE SPEECH
# Censorship and freedom of speech
# People who hold opinions that are harmful or offensive to minority groups should be banned from expressing those views publicly.
# Although I may disagree with the opinions that other people hold, they should be allowed to express those views publicly.




t2_free_speech_z_trt_A <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_free_speech_z",
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


t2_free_speech_z_trt_A
t2_free_speech_z_trt_A
here_save(t2_free_speech_z_trt_A, "t2_free_speech_z_trt_A")

t2_free_speech_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_free_speech_z",
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

t2_free_speech_z_null
here_save(t2_free_speech_z_null, "t2_free_speech_z_null")

# no relationship
lmtp_contrast(t2_free_speech_z_trt_A,
              ref = t2_free_speech_z_null,
              type = "additive")


# speech: z-shift low to mean ---------------------------------------------


# FREE SPEECH
# Censorship and freedom of speech
# People who hold opinions that are harmful or offensive to minority groups should be banned from expressing those views publicly.
# Although I may disagree with the opinions that other people hold, they should be allowed to express those views publicly.

f_z
A_z

t2_free_speech_z_trt_A_z <- lmtp_tmle(
  data = df_clean,
  trt = A_z,
  baseline = names_base,
  outcome = "t2_free_speech_z",
  cens = C,
  shift = f_z,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)



t2_free_speech_z_trt_A_z
here_save(t2_free_speech_z_trt_A_z, "t2_free_speech_z_trt_A_z")

t2_free_speech_z_null_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_free_speech_z",
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

t2_free_speech_z_null
here_save(t2_free_speech_z_null, "t2_free_speech_z_null")

# no relationship
lmtp_contrast(t2_free_speech_z_trt_A,
              ref = t2_free_speech_z_null,
              type = "additive")







# SCIENCE CREDIBILITY -----------------------------------------------------



## SCIENCE CREDIBILITY 
# "I have a high degree of confidence in the scientific community",

f_z
A_1_z

t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z <- lmtp_tmle(
  data = df_clean,
  trt = A_1_z,
  baseline = names_base,
  outcome = "t2_trust_science_high_confidence_scientific_community_z",
  cens = C,
  shift = f_z,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z
t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z
here_save(t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z, 
          "t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z")
# 
# #"How often do you have a drink containing alcohol?"
# t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null <- lmtp_tmle(
#   data = df_clean,
#   trt = A_1,
#   baseline = names_base,
#   outcome = "t2_trust_science_high_confidence_scientific_community_z",
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
# t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null
# here_save(t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null, 
#           "t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null")





# CONTRASTS ---------------------------------------------------------------

t2_free_speech_z_trt_A <- 
  here_read( "t2_free_speech_z_trt_A")

t2_free_speech_z_null <- 
  here_read("t2_free_speech_z_null")

t2_free_speech_z_trt_A_z <- 
  here_read( "t2_free_speech_z_trt_A_z")

t2_free_speech_z_trt_A
t2_free_speech_z_trt_A_z


#1 

contrast_t2_free_speech_z_trt_A <-
  lmtp_contrast(t2_free_speech_z_trt_A,
                ref = t2_free_speech_z_null,
                type = "additive")


tab_contrast_t2_free_speech_z_trt_A<-
  margot_tab_lmtp(contrast_t2_free_speech_z_trt_A,
                  scale = "RD",
                  new_name = "Free Speech")


out_tab_contrast_t2_free_speech_z_trt_A <-
  lmtp_evalue_tab(tab_contrast_t2_free_speech_z_trt_A,
                  scale = c("RD"))

out_tab_contrast_t2_free_speech_z_trt_A





#1
contrast_t2_free_speech_z_trt_A_z <-
  lmtp_contrast(t2_free_speech_z_trt_A_z,
                ref = t2_free_speech_z_null,
                type = "additive")


tab_contrast_t2_free_speech_z_trt_A_z<-
  margot_tab_lmtp(contrast_t2_free_speech_z_trt_A_z,
                  scale = "RD",
                  new_name = "Free Speech")


out_tab_contrast_t2_free_speech_z_trt_A_z <-
  lmtp_evalue_tab(tab_contrast_t2_free_speech_z_trt_A_z,
                  scale = c("RD"))

out_tab_contrast_t2_free_speech_z_trt_A_z


## Science confidence


t2_trust_science_high_confidence_scientific_community_z_trt_A_1 <- 
  here_read("t2_trust_science_high_confidence_scientific_community_z_trt_A_1")
t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z <- 
  here_read("t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z")
t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null <- 
  here_read("t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null")

t2_trust_science_high_confidence_scientific_community_z_trt_A_1
t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z

contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1 <-
  lmtp_contrast(t2_trust_science_high_confidence_scientific_community_z_trt_A_1,
                ref = t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null,
                type = "additive")

contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1
tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1 <-
  margot_tab_lmtp(contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1,
                  scale = "RD",
                  new_name = "Free Speech")


out_tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1 <-
  lmtp_evalue_tab(tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1,
                  scale = c("RD"))

out_tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1


t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z
t2_trust_science_high_confidence_scientific_community_z_trt_A_1


# 2
contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z <-
  lmtp_contrast(t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z,
                ref = t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null,
                type = "additive")


tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z<-
  margot_tab_lmtp(contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z,
                  scale = "RD",
                  new_name = "Free Speech")


out_tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z<-
  lmtp_evalue_tab(tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z,
                  scale = c("RD"))

out_tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z


out_tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1
out_tab_contrast_t2_trust_science_high_confidence_scientific_community_z_trt_A_1_z

# make tables -------------------------------------------------------------
# don't forget to report smoking

# bind individual tables
tab_health <- rbind(
  # out_tab_contrast_t2_sfhealth_z,
  t2_trust_science_high_confidence_scientific_community_z_trt_A_1,
  t2_trust_science_high_confidence_scientific_community_z_trt_A_1_null,
)

tab_body <- rbind(
  out_tab_contrast_t2_bodysat_z,
  out_tab_contrast_t2_kessler6_sum_z,
)

# make group table
group_tab_health <- group_tab(tab_health  , type = "RD")

# save
here_save(group_tab_health, "group_tab_health")


# make group table
group_tab_body <- group_tab(tab_body , type = "RD")

# save
here_save(group_tab_body, "group_tab_body")


# create plots -------------------------------------------------------------
N 
sub_title = "Science Credibility: shift + 1 point everyone (up to max 1), N = 31,577"



# graph health
plot_group_tab_health <- margot_plot(
  group_tab_health,
  type = "RD",
  title = "Freedom of Speech Effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

# save graph
ggsave(
  plot_group_tab_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_group_tab_health.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_group_tab_health



####



# graph body
plot_group_tab_body <- margot_plot(
  group_tab_body,
  type = "RD",
  title = "Body effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

# save graph
ggsave(
  plot_group_tab_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_group_tab_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_body

# graph ego
plot_group_tab_ego <- margot_plot(
  group_tab_ego,
  type = "RD",
  title = "Ego effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
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
  width = 8,
  height = 6,
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
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
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
  width = 8,
  height = 6,
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
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
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
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_group_tab_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
