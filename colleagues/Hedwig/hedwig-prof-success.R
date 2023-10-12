# HEDWIG 2023 paper on psycopathy and prof success
# Thoughs by joseph.bulbulia@vuw.ac.nz

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# experimental functions
source("/Users/joseph/GIT/templates/functions/experimental_funs.R")


## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE. DO NOT USE THIS PATH
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
  )

# read data: note that you need use the arrow package in R
dat <- arrow::read_parquet(pull_path)


# use aron folder 
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/notes/23-lmtp-aaron"
  )


# define exposures --------------------------------------------------------
# define exposure
A <- "t1_psychopathy_scale_z"

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("psychopathy_scale", "coldheartedness_sans_sdo", "agreeableness_reversed", "not_lost") #
# shift one pont up if under 6
# f_1 <- function (data, trt) data[[trt]] + 1

#  move to mean of greater than mean
# f <- function(data, trt) {
#   ifelse(data[[trt]] >= 0, 0,  data[[trt]])
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



# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE LIBRARIES FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")
#
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")
#
#
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R")


## measures (from aaron's paper)

# •	Fearless dominance – “I am the life of the party”;
#Pers.E_IPIP01.T10	Am the life of the party.

# “I keep in the background” (-);
#Pers.E_IPIP03r.T11	Keep in the background.

# “I talk to a lot of different people at parties”;
#Pers.E_IPIP04.T11	Talk to a lot of different people at parties.

# “I am relaxed most of the time”;
#Pers.N_IPIP02r.T11	Am relaxed most of the time.

#“I get upset easily” (-)
#Pers.N_IPIP03.T11	Get upset easily.

#	Self-centered impulsivity –
#“I make a mess of things”;
#Pers.C_IPIP03r.T11	Make a mess of things.

#“I would like to be seen driving around in a very expensive car”;
#Pers.HonHum03r.T11	Would like to be seen driving around in a very expensive car.

#“I feel entitled to more of everything”;
#Pers.Narc01r.T11	Feel entitled to more of everything.

#“I have frequent mood swings”;
#Pers.N_IPIP01.T11	Have frequent mood swings.

#“To get ahead in life, it is sometimes okay to step on other groups”
#SDO03.T10	To get ahead in life, it is sometimes okay to step on other groups.
#SDO03
#### •	Coldheartedness –

#“I sympathize with others’ feelings” (-);
#Pers.A_IPIP01.T11	Sympathise with others' feelings.

# “I feel others’ emotions” (-);
# Pers.A_IPIP03.T11	Feel others' emotions.

# “We should do what we can to equalize conditions for different groups” (-);
#SDO06r.T10	We should do what we can to equalise conditions for different groups.

# “I am not really interested in others”;
#Pers.A_IPIP04r.T11	Am not really interested in others.

# “I’m not interested in other people’s problems”
#Pers.A_IPIP02r.T11	Am not interested in other people's problems.

# import data and create variables.

# note that a better approach would be to use passive imputation in MICE.

# transform names into the janitor package names
library(stringr)
library(janitor)

# create the vector of names

column_names <- c(
  "Questionnaire.Num",
  "Rel.Num",
  "Age",
  "GendAll",
  "Pers.A_IPIP01",
  "Pers.E_IPIP01",
  "Pers.E_IPIP03r",
  "Pers.E_IPIP04",
  "Pers.N_IPIP02r",
  "Pers.N_IPIP03",
  "Pers.C_IPIP03r",
  "Pers.HonHum03r",
  "Pers.Narc01r",
  "Pers.N_IPIP01",
  "SDO03",
  "Pers.A_IPIP03",
  "SDO06r",
  "Pers.A_IPIP04r",
  "Pers.A_IPIP02r",
  "Pers.A_IPIP03",
  "SDO06r",
  "NZSEI18",
  "Rel.Satisfaction",
  "Emp.CurrentJobYEARS",
  "Emp.JobSat",
  "Rel.Conflict",
  "CharityDonate",
  "Hours.Charity"
)

# define the function to clean names
clean_custom_names <- function(name_vector) {
  name_vector %>%
    str_to_lower() %>%  # make all characters lowercase
    str_replace_all("[^[:alnum:]]", "_") %>%  # replace all non-alphanumeric characters with '_'
    str_replace_all("_+$", "")  %>%  # remove trailing underscores
    str_replace_all("^_+", "")  %>%  # remove leading underscores
    str_replace_all("_+", "_")  # replace multiple consecutive underscores with a single one
}

# apply the function
cleaned_names <- clean_custom_names(column_names)

# get names for dplyr
cleaned_names_string <- paste(cleaned_names, collapse = ", ")
cleaned_names_string
# check

table(is.na(dat$sexual_orientation_l1))
table(is.na(dat$sexual_orientation))


# transformations
dat_2 <- dat |> 
  mutate(
    pers_n_ipip02r_reversed = 8 - pers_n_ipip02r,
    pers_n_ipip03_reversed = 8 - pers_n_ipip03,
    pers_c_ipip03r_reversed = 8 - pers_c_ipip03r,
    pers_hon_hum03r_reversed = 8 - pers_hon_hum03r,
    pers_narc01r_reversed = 8 - pers_narc01r,
    pers_a_ipip01_reversed = 8 - pers_a_ipip01,
    pers_a_ipip03_reversed = 8 - pers_a_ipip03,
    pers_a_ipip04r_reversed = 8 - pers_a_ipip04r,
    pers_a_ipip02r_reversed = 8 - pers_a_ipip02r,
    agreeableness_reversed = 8 - agreeableness,
    conscientiousness_reversed = 8 - conscientiousness
  ) |>
  rowwise(wave) |>
  mutate(kessler_latent_depression =  mean(
    c(kessler_depressed, kessler_hopeless, kessler_worthless),
    na.rm = TRUE
  )) |>
  mutate(kessler_latent_anxiety  = mean(c(
    kessler_effort, kessler_nervous, kessler_restless
  ), na.rm = TRUE)) |>
  dplyr::mutate(
    fearless_dominance = mean(
      c(
        pers_e_ipip01, #Fearless dominance – “I am the life of the party”;
        pers_e_ipip03r, # Pers.E_IPIP03r.T11	Keep in the background.
        pers_e_ipip04, # Talk to a lot of different people at parties.
        pers_n_ipip02r_reversed, # Am relaxed most of the time.
        pers_n_ipip03_reversed, # Get upset easily.
        na.rm = TRUE
      )
    ),
    sc_impulsivity = mean(
      c(
        pers_c_ipip03r_reversed,  # Make a mess of things. 
        pers_hon_hum03r_reversed, # Would like to be seen driving around in a very expensive car.
        pers_narc01r_reversed, # Feel entitled to more of everything.
        pers_n_ipip01, # Have frequent mood swings.
        sdo03, # To get ahead in life, it is sometimes okay to step on other groups. # check with cold heartedness?  # check
        na.rm = TRUE
      )
    ),
    coldheartedness = mean(
      c(
        pers_a_ipip01_reversed, # Sympathise with others' feelings.
        pers_a_ipip03_reversed, # Feel others' emotions.
        sdo06r, #  We should do what we can to equalise conditions for different groups.
        pers_a_ipip04r_reversed, # Am not really interested in others.
        pers_a_ipip02r_reversed, # Am not interested in other people's problems.
        na.rm = TRUE
      )
    ),
    psychopathy_scale = mean(
      c(
      coldheartedness,
      sc_impulsivity,
      fearless_dominance,
      na.rm = TRUE
    )
    ),
  psychopathy_personality_scale = mean(c(
    extraversion,
    agreeableness_reversed,
    conscientiousness_reversed,
    na.rm = TRUE
  )
  )) |> 
  mutate(coldheartedness_sans_sdo = mean(
    c(
      pers_a_ipip01_reversed, # Sympathise with others' feelings.
      pers_a_ipip03_reversed, # Feel others' emotions.
    #  sdo06r, #  We should do what we can to equalise conditions for different groups.
      pers_a_ipip04r_reversed, # Am not really interested in others.
      pers_a_ipip02r_reversed, # Am not interested in other people's problems.
      na.rm = TRUE
    )
  ))|>  
  # select(
  #   -c(
  #     pers_n_ipip02r,
  #     pers_n_ipip03,
  #     pers_c_ipip03r,
  #     pers_hon_hum03r,
  #     pers_narc01r,
  #     pers_a_ipip01,
  #     pers_a_ipip03,
  #     pers_a_ipip04r,
  #     pers_a_ipip02r
  #   )
  # ) |>
  ungroup() 


push_mods

here_save_arrow(dat_2, "dat_2-hedwig")
dat_2 <- here_read_arrow("dat_2-hedwig")


hist(dat_2$agreeableness_reversed)
hist(dat_2$coldheartedness_sans_sdo)


# assess scale ------------------------------------------------------------


#psychopathy scale ---------------------------------------------------------------

dt_scale_19 <- dat_2 |>
  filter(wave == 2019  & year_measured == 1)

dt_only_items <- dt_scale_19 |> select(
  pers_e_ipip01,
  pers_e_ipip03r,
  pers_e_ipip04,
#  pers_n_ipip02r_reversed,#  # Am relaxed most of the time.
#  pers_n_ipip03_reversed,   #  Get upset easily.
 # pers_c_ipip03r_reversed, # make a mess of things
#  pers_hon_hum03r_reversed, # Would like to be seen driving around in a very expensive car.
 # pers_narc01r_reversed, #  # Feel entitled to more of everything.
#  pers_n_ipip01,  # have frequent mood swings 
  sdo03,#
  pers_a_ipip01_reversed,#
  pers_a_ipip03_reversed,#
 # sdo06r,# do what we can to equalise 
  pers_a_ipip04r_reversed,#
  pers_a_ipip02r_reversed#
)

# dt_only_items <- dt_scale_19 |> select(
#   pers_a_ipip01_reversed, # Sympathise with others' feelings.
#   pers_a_ipip03_reversed, # Feel others' emotions.
#   #  sdo06r, #  We should do what we can to equalise conditions for different groups.
#   pers_a_ipip04r_reversed, # Am not really interested in others.
#   pers_a_ipip02r_reversed, # Am not interested in other people's problems..
# )
# 
# 
# 
# # check factor structure
performance::check_factorstructure(dt_only_items)

# explore a factor structure made of 3 latent variables
efa <- psych::fa(dt_only_items, nfactors = 1) %>%
  model_parameters(sort = TRUE, threshold = "max")

efa


n <- n_factors(dt_only_items)

# plot
plot(n) + theme_classic()
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
# 



# a little better 
hist(dat_2$psychopathy_personality_scale)

hist(dat_2$psychopathy_scale)
hist(dat_2$agreeableness)
hist(dat_2$coldheartedness_sans_sdo)
hist(dat_2$extraversion)



# check dyads by wave 10


# dyads in all waves ------------------------------------------------------


# # checks
# initial_filter <- dat_2 %>%
#   filter((wave %in% c(2018, 2019) & year_measured == 1) | 
#            (wave == 2020)) %>%
#   filter(!is.na(psychopathy_scale))





# Filter the data for years 2018 and 2019 where #year_measured == 1.
dat_18_19 <- dat_2 |>  
  filter(wave %in% c(2018, 2019) & year_measured == 1 & !is.na(psychopathy_scale)  & employed == 1)

#Group by id and find the number of unique waves they are part of.
id_count <- dat_18_19 |> 
  group_by(id) |> 
  summarise(n_unique_waves = n_distinct(wave), .groups = 'drop') |> 
  filter(n_unique_waves == 2)

valid_ids_1 <- id_count$id

dat_filtered <- dat_2 |> 
  filter(id %in% valid_ids_1 & wave %in% c(2018, 2019, 2020))

n_unique(dat_filtered$id)

# select variables and prepare data
dat_long  <- dat_filtered %>%
  dplyr::select(
    id,
    wave,
    year_measured,
    sample_origin_names_combined,
  #  rel_num,
    age,
    male,
    eth_cat,
    employed,
    born_nz,
    children_num,
    sexual_orientation,
    education_level_coarsen,
    political_conservative,
    religion_identification_level,
    nz_dep2018,
    agreeableness_reversed,
    # pers_a_ipip01,
    # pers_e_ipip01,
    # pers_e_ipip03r,
    # pers_e_ipip04,
    # pers_n_ipip02r,
    # pers_n_ipip03,
    # pers_c_ipip03r,
    # pers_hon_hum03r,
    # pers_narc01r,
    # pers_n_ipip01,
    # sdo03,
    # pers_a_ipip03,
    # sdo06r,
    # pers_a_ipip04r,
    # pers_a_ipip02r,
    # sdo06r,
    nzsei13,
    sat_relationship,
    emp_current_job_years,
    # question
    emp_job_sat,
    conflict_in_relationship,
    charity_donate,
    hours_charity,
    household_inc,
    hours_children,
    hours_work,
    hours_housework,
    hours_exercise,
    w_gend_age_euro,
    sample_origin_names_combined,
    alert_level_combined_lead,
    kessler_depressed,
    kessler_hopeless,
    kessler_worthless,
    kessler_effort,
    kessler_nervous,
    kessler_restless,
    hlth_disability,
    urban,
    psychopathy_scale,
    kessler_latent_depression,
    kessler_latent_anxiety,
    coldheartedness,
    sc_impulsivity,
    fearless_dominance,
  psychopathy_personality_scale,
  coldheartedness_sans_sdo
  )  %>%
  # mutate(
  #   pers_n_ipip02r_reversed = 8 - pers_n_ipip02r,
  #   pers_n_ipip03_reversed = 8 - pers_n_ipip03,
  #   pers_c_ipip03r_reversed = 8 - pers_c_ipip03r,
  #   pers_hon_hum03r_reversed = 8 - pers_hon_hum03r,
  #   pers_narc01r_reversed = 8 - pers_narc01r,
  #   pers_a_ipip01_reversed = 8 - pers_a_ipip01,
  #   pers_a_ipip03_reversed = 8 - pers_a_ipip03,
  #   pers_a_ipip04r_reversed = 8 - pers_a_ipip04r,
  #   pers_a_ipip02r_reversed = 8 - pers_a_ipip02r
  # ) |>
# rowwise(wave) |>
# select(
#   -c(
#     pers_n_ipip02r,
#     pers_n_ipip03,
#     pers_c_ipip03r,
#     pers_hon_hum03r,
#     pers_narc01r,
#     pers_a_ipip01,
#     pers_a_ipip03,
#     pers_a_ipip04r,
#     pers_a_ipip02r
#   )
# ) |>
# mutate(kessler_latent_depression =  mean(
#   c(kessler_depressed, kessler_hopeless, kessler_worthless),
#   na.rm = TRUE
# )) |>
# mutate(kessler_latent_anxiety  = mean(c(
#   kessler_effort, kessler_nervous, kessler_restless
# ), na.rm = TRUE)) |>
# dplyr::mutate(
#   fearless_dominance = mean(
#     c(
#       pers_e_ipip01,
#       pers_e_ipip03r,
#       pers_e_ipip04,
#       pers_n_ipip02r_reversed,
#       pers_n_ipip03_reversed,
#       na.rm = TRUE
#     )
#   ),
#   sc_impulsivity = mean(
#     c(
#       pers_c_ipip03r_reversed,
#       pers_hon_hum03r_reversed,
#       pers_narc01r_reversed,
#       pers_n_ipip01,
#       sdo03,
#       na.rm = TRUE
#     )
#   ),
#   coldheartedness = mean(
#     c(
#       pers_a_ipip01_reversed,
#       pers_a_ipip03_reversed,
#       sdo06r,
#       pers_a_ipip04r_reversed,
#       pers_a_ipip02r_reversed,
#       na.rm = TRUE
#     )
#   ),
#   psychopathy_scale = mean(c(
#     coldheartedness,
#     sc_impulsivity,
#     fearless_dominance
#   ))
# ) |>
# #  ungroup() |>
#   arrange(id, wave) |>  # individual criteria 
#   group_by(rel_num, wave) |>
#   mutate(num_in_rel_wave = n()) |>
#   ungroup() |>
#   group_by(id) |>
#   mutate(k_18 = ifelse(wave == 2018 &
#                          !is.na(psychopathy_scale) & employed == 1 & num_in_rel_wave == 2, 1, 0)) %>% # selection criteria
#   mutate(k_19 = ifelse(wave == 2019 &
#                          !is.na(psychopathy_scale) &  num_in_rel_wave == 2, 1, 0)) %>% # selection criteria
#   mutate(h_18 = mean(k_18, na.rm = TRUE)) %>%
#   mutate(h_19 = mean(k_19, na.rm = TRUE)) %>%
#   dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
#   dplyr::filter(h_18 > 0) |>  # hack to enable repeat of baseline
#   ungroup() %>%
droplevels() |>
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
    household_inc_log = log(household_inc + 1),
    hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_volunteering_log = log(hours_charity + 1),
    charity_donate_log = log(charity_donate + 1),
    sample_origin = sample_origin_names_combined
  ) |>  #
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  # select(-h_18, -k_18, -h_19, -k_19) |>
  droplevels() |>
  ungroup() %>%
  mutate(wave = as.numeric(wave)) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  #  select(-c(sample_origin, year_measured)) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()
dat_long$coldheartedness_sans_sdo


N <- skimr::n_unique(dat_long$id)
N

dat_19 <- dat_long |>
  filter(wave == 2) |>
  mutate(psychopathy_scale_z = scale(psychopathy_scale),
         coldheartedness_sans_sdo_z = scale(coldheartedness_sans_sdo))

# check
table(is.na( dat_19$coldheartedness_sans_sdo))

n_unique(dat_19$id) # 640

hist(dat_19$psychopathy_scale)
table(dat_19$psychopathy_scale_z)
hist(dat_19$coldheartedness_sans_sdo_z)

mean_exposure <- mean(dat_19$psychopathy_scale,
                      na.rm = TRUE)
mean_exposure

# just for interst
max(dat_19$psychopathy_scale,
    na.rm = TRUE)

min(dat_19$psychopathy_scale,
    na.rm = TRUE)

# just for interst
max(dat_19$coldheartedness,
    na.rm = TRUE)

min(dat_19$coldheartedness,
    na.rm = TRUE)


max(dat_19$fearless_dominance,
    na.rm = TRUE)

min(dat_19$fearless_dominance,
    na.rm = TRUE)


max(dat_19$sc_impulsivity,
    na.rm = TRUE)

min(dat_19$sc_impulsivity,
    na.rm = TRUE)

min(dat_19$coldheartedness_sans_sdo,
    na.rm = TRUE)

max(dat_19$coldheartedness_sans_sdo,
    na.rm = TRUE)

max(dat_19$coldheartedness_sans_sdo,
    na.rm = TRUE)


# for coldheartedness
# make sure to use the sd
max_score <- max(dat_19$psychopathy_scale_z, na.rm = TRUE)
max_score # 2.812498

min_score <- min(dat_19$psychopathy_scale_z, na.rm = TRUE)
min_score #  -2.874288



sd_exposure <- sd(dat_19$psychopathy_scale,
                  na.rm = TRUE)
sd_exposure

one_point_in_sd_units <- 1 / sd_exposure
one_point_in_sd_units
0 +  one_point_in_sd_units # in bound

# half a standard deviation if needed
half_sd <- sd_exposure / 2
half_sd

one_point_in_sd_units

one_point_in_sd_units <- 1 / sd_exposure
one_point_in_sd_units





# make sure to use the sd
max_score_c <- max(dat_19$coldheartedness_sans_sdo_z, na.rm = TRUE)
max_score_c #

min_score_c <- min(dat_19$coldheartedness_sans_sdo_z, na.rm = TRUE)
min_score_c #  -2.874288


sd_exposure_c<- sd(dat_19$coldheartedness_sans_sdo,
                   na.rm = TRUE)
sd_exposure_c

one_point_in_sd_units_c <- 1 / sd_exposure_c
one_point_in_sd_units_c




#  increase everyone by one point, contrasted with what they would be anyway.
#  only use this function for raw scores

f_up <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - sd_exposure,
         data[[trt]] + sd_exposure,
         max_score)
}

# decrease everyone by  - half_sd
f_down <- function(data, trt) {
  ifelse(data[[trt]] >= min_score  + sd_exposure,
         data[[trt]] - sd_exposure,
         min_score)
}

# coldheartedness
f_up_c <- function(data, trt) {
  ifelse(data[[trt]] <= max_score_c - sd_exposure_c,
         data[[trt]] + sd_exposure_c,
         max_score_c)
}

# decrease everyone by  - half_sd
f_down_c <- function(data, trt) {
  ifelse(data[[trt]] >= min_score_c  + sd_exposure_c,
         data[[trt]] - sd_exposure_c,
         min_score_c)
}






#  increase everyone by one point, contrasted with what they would be anyway.
#  only use this function for raw scores

f_up <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - sd_exposure,
         data[[trt]] + sd_exposure,
         max_score)
}

# decrease everyone by  - half_sd
f_down <- function(data, trt) {
  ifelse(data[[trt]] >= min_score  + sd_exposure,
         data[[trt]] - sd_exposure,
         min_score)
}
min_score



dt_check_exposure <- dat_long|> filter(wave == 1 | wave == 2)

# makes sure all is false
table (is.na(dt_check_exposure$psychopathy_scale))

table(dt_check_exposure$psychopathy_scale)

dt_positivity_full <- dat_long |>
  filter(wave == 1 | wave == 2) |>
  select(wave, id, psychopathy_scale, psychopathy_personality_scale,coldheartedness_sans_sdo) |>
  mutate(psychopathy_scale_round = round(psychopathy_scale, 0), 
         psychopathy_personality_scale_round = round(psychopathy_personality_scale, 0),
         coldheartedness_sans_sdo_round = round(coldheartedness_sans_sdo, 0))


table ((dt_positivity_full$psychopathy_scale)) #
table ((dt_positivity_full$psychopathy_personality_scale_round)) #

table ((dt_positivity_full$psychopathy_scale)) #
table ((dt_positivity_full$coldheartedness_sans_sdo_round)) #

# test positivity
out <-
  msm::statetable.msm(psychopathy_scale_round, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab


# test positivity
out <-
  msm::statetable.msm(psychopathy_personality_scale_round, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab



# test positivity
out <-
  msm::statetable.msm(coldheartedness_sans_sdo_round, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab



baseline_vars = c(
  # "id",
  "male",
  #  "partner_male",
 # "sexual_orientation",
  #  "partner_sexual_orientation",
  "age",
  # "partner_age",
  # "education_level_coarsen",
  #  "partner_education_level_coarsen",
  # factors
  "eth_cat",
  # "partner_eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  #  "sample_origin",
  "nz_dep2018",
  "nzsei13",
  #  "partner_nzsei13",
  "born_nz",
  #  "partner_born_nz",
  "hlth_disability",
  #  "partner_hlth_disability",
  "kessler_latent_depression",
  # "partner_kessler_latent_depression",
  "kessler_latent_anxiety",
  #"partner_kessler_latent_anxiety",
  "household_inc_log",
  # added: measured with error but OK for imputations
  # "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  #  "partner_political_conservative",
  "religion_identification_level",
  #  "partner_religion_identification_level",
  # Sample origin names combined
  "urban",
  "children_num",
  # "partner_children_num",
  "hours_children_log",
  # "partner_hours_children_log",
  # new
  "hours_work_log",
  #  "partner_hours_work_log",
  # new
  # "partner_hours_housework_log",
  "hours_housework_log",
  #new
  # "hours_exercise_log",
  # "agreeableness",
  # "conscientiousness",
  # "extraversion",
  # "honesty_humility",
  # "openness",
  # "neuroticism",
  # "modesty",
  "sample_weights",
  "alert_level_combined_lead"
)

colnames(dat_long)

# check
baseline_vars

# check
exposure_var

# outcomes
outcome_vars = c(
#  "sat_relationship",
#  "conflict_in_relationship",
  "nzsei13",
  "hours_volunteering_log",
  "charity_donate_log")


# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome



# make data wide and impute baseline missing values -----------------------
exposure_var



# custom function
# check data
mice:::find.collinear(dat_long)  # note that mice will ignore collinear data. However a better approach would be to use passive

#check missing
naniar::vis_miss(dat_long, warn_large_data = FALSE)
dev.off()


baseline_vars
exposure_var

dat_long <- data.frame(dat_long)

margot_wide_impute_baseline

dat_long_couples <- dat_long |> droplevels()

prep_coop_all <- margot_wide_impute_baseline(
  dat_long_couples,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)

colnames(prep_coop_all)

# library(magrittr)
# prep_coop_all_hedwig <- prep_coop_all %>%
#   group_by(id, t0_rel_num) %>%
#   mutate_all(rev) %>%
#   ungroup() %>%
#   select(-id, -t0_rel_num) %>%
#   set_colnames(paste0('partner_', colnames(.)))

prep_coop_all_hedwig <- prep_coop_all

colnames(prep_coop_all_hedwig)

#
n_unique(prep_coop_all_hedwig$id)

# save function -- will save to your "push_mod" directory
here_save(prep_coop_all_hedwig, "prep_coop_all_hedwig")

# spit and shine:

# read function
prep_coop_all_hedwig <- here_read("prep_coop_all_hedwig")

colnames(prep_coop_all_hedwig)
naniar::vis_miss(prep_coop_all_hedwig, warn_large_data = FALSE)
dev.off()



#check must be a dataframe


prep_coop_all_hedwig <- as.data.frame(prep_coop_all_hedwig)

# arrange data for analysis -----------------------------------------------
# spit and shine
df_wide_censored <-
  prep_coop_all_hedwig |>
  mutate(t0_eth_cat = as.factor(t0_eth_cat)) |>
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
        !t0_sample_weights,
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    t0_not_lost,
    t0_sample_weights,
    #  t1_permeability_individual, # make sure to change for each study
    t1_not_lost,
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
dev.off()


# again check path
push_mods
# save
here_save(df_clean, "df_clean_hedwig")

# read if needed
df_clean <- here_read("df_clean_hedwig")

str(df_clean)
nrow(df_clean)
df_clean <- as.data.frame(df_clean)

#check n
nrow(df_clean)

colnames(df_clean)
# get names


# low n so can't use this
# names_base <-
#   df_clean |> select(starts_with("t0"),
#                      -t0_sample_weights,
#                      -t0_not_lost,
#                      -t0_partner_not_lost) |> colnames()
# names_base



names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost,
                     -t0_coldheartedness_sans_sdo_z)|> colnames()




names_base

names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

# set variables for models ------------------------------------------------

#### SET VARIABLE NAMES: Customise for each outcomewide model
#  model
A

C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))

# check
print(W)


f_up
f_down
# models

#"nzsei"

t2_nzsei13_z_up <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_nzsei13_z",
  cens = C,
  shift = f_up,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_nzsei13_z_up
here_save(t2_nzsei13_z_up, "t2_nzsei13_z_up-hedwig")


t2_nzsei13_z_down <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_nzsei13_z",
  cens = C,
  shift = f_down,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_nzsei13_z_down
here_save(t2_nzsei13_z_down, "t2_nzsei13_z_down-hedwig")


t2_nzsei13_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_nzsei13_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_nzsei13_z_null
here_save(t2_nzsei13_z_null, "t2_nzsei13_z_null-hedwig")




# chartiy ----------------------------------------------------------------


# t2_charity_donate_z
t2_charity_donate_z_up <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate_log_z",
  cens = C,
  shift = f_up,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_charity_donate_z_up
here_save(t2_charity_donate_z_up , "t2_charity_donate_z_up-hedwig")


t2_charity_donate_z_down  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate_log_z",
  cens = C,
  shift = f_down,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)



t2_charity_donate_z_down
here_save(t2_charity_donate_z_down , "t2_charity_donate_z_down-hedwig")

t2_charity_donate_z_null  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate_log_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_charity_donate_z_null
here_save(t2_charity_donate_z_null, "t2_charity_donate_z_null-hedwig")


#t2_hours_charity_z

t2_hours_charity_z_up <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t0_hours_volunteering_log_z",
  cens = C,
  shift = f_up,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_hours_charity_z_up
here_save(t2_hours_charity_z_up , "t2_hours_charity_z_up-hedwig")


t2_hours_charity_z_down<- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t0_hours_volunteering_log_z",
  cens = C,
  shift = f_down,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_hours_charity_z_down
here_save(t2_hours_charity_z_down, "t2_hours_charity_z_down-hedwig")


t2_hours_charity_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t0_hours_volunteering_log_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_hours_charity_z_null
here_save(t2_hours_charity_z_null, "t2_hours_charity_z_null-hedwig")


# contrasts
# nzsei
t2_nzsei13_z_up<-
  here_read("t2_nzsei13_z_up-hedwig")

t2_nzsei13_z_down <-
  here_read("t2_nzsei13_z_down-hedwig")

t2_nzsei13_z_null<-
  here_read("t2_nzsei13_z_null-hedwig")

# first contrast
contrast_t2_nzsei13_z_up <-
  lmtp_contrast(t2_nzsei13_z_up,
                ref = t2_nzsei13_z_null,
                type = "additive")

tab_contrast_t2_nzsei13_z_up<-
  margot_tab_lmtp(contrast_t2_nzsei13_z_up ,
                  scale = "RD",
                  new_name = "Objective job success (SD)")


out_tab_contrast_t2_nzsei13_z_up  <-
  lmtp_evalue_tab(tab_contrast_t2_nzsei13_z_up ,
                  scale = c("RD"))

out_tab_contrast_t2_nzsei13_z_up 


# second contrast
contrast_t2_nzsei13_z_down <-
  lmtp_contrast(t2_nzsei13_z_down,
                ref = t2_nzsei13_z_null,
                type = "additive")

tab_contrast_t2_nzsei13_z_down  <-
  margot_tab_lmtp(contrast_t2_nzsei13_z_down,
                  scale = "RD",
                  new_name = "Objective job success (SD)")


out_tab_contrast_t2_nzsei13_z_down  <-
  lmtp_evalue_tab(tab_contrast_t2_nzsei13_z_down,
                  scale = c("RD"))

out_tab_contrast_t2_nzsei13_z_down


# t2_charity_donate_z
t2_charity_donate_z_up <-
  here_read("t2_charity_donate_z_up-hedwig")

t2_charity_donate_z_down <-
  here_read("t2_charity_donate_z_down-hedwig")

t2_charity_donate_z_down_null <-
  here_read("t2_charity_donate_z_null-hedwig")

# first contrast
contrast_t2_charity_donate_z_up<-
  lmtp_contrast(t2_charity_donate_z_up,
                ref = t2_charity_donate_z_null,
                type = "additive")

tab_contrast_t2_charity_donate_z_up<-
  margot_tab_lmtp(contrast_t2_charity_donate_z_up,
                  scale = "RD",
                  new_name = "Charity donations (SD)")


out_tab_contrast_t2_charity_donate_z_up<-
  lmtp_evalue_tab(tab_contrast_t2_charity_donate_z_up,
                  scale = c("RD"))

out_tab_contrast_t2_charity_donate_z_up


# second contrast
contrast_t2_charity_donate_z_down <-
  lmtp_contrast(t2_charity_donate_z_down,
                ref = t2_charity_donate_z_null,
                type = "additive")

tab_contrast_t2_charity_donate_z_down <-
  margot_tab_lmtp(contrast_t2_charity_donate_z_down,
                  scale = "RD",
                  new_name = "Charity donations (SD)")


out_tab_contrast_t2_charity_donate_z_down<-
  lmtp_evalue_tab(tab_contrast_t2_charity_donate_z_down,
                  scale = c("RD"))

out_tab_contrast_t2_charity_donate_z_down



# volunteering
t2_hours_charity_z_up<-
  here_read("t2_hours_charity_z_up-hedwig")

t2_hours_charity_z_down<-
  here_read("t2_hours_charity_z_down-hedwig")

t2_hours_charity_z_null <-
  here_read("t2_hours_charity_z_null-hedwig")

# first contrast
contrast_t2_hours_charity_z_up <-
  lmtp_contrast(t2_hours_charity_z_up,
                ref = t2_hours_charity_z_null,
                type = "additive")

tab_contrast_t2_hours_charity_z_up <-
  margot_tab_lmtp(contrast_t2_hours_charity_z_up,
                  scale = "RD",
                  new_name = "Weekly hours volunteering(SD)")


out_tab_contrast_t2_hours_charity_z_up<-
  lmtp_evalue_tab(tab_contrast_t2_hours_charity_z_up,
                  scale = c("RD"))

out_tab_contrast_t2_hours_charity_z_up


# second contrast
contrast_t2_hours_charity_z_down<-
  lmtp_contrast(t2_hours_charity_z_down,
                ref = t2_hours_charity_z_null,
                type = "additive")

contrast_t2_hours_charity_z_down

tab_contrast_t2_hours_charity_z_down <-
  margot_tab_lmtp(contrast_t2_hours_charity_z_down,
                  scale = "RD",
                  new_name = "Weekly hours volunteering (SD)")

tab_contrast_t2_hours_charity_z_down


out_tab_contrast_t2_hours_charity_z_down <-
  lmtp_evalue_tab(tab_contrast_t2_hours_charity_z_down,
                  scale = c("RD"))

out_tab_contrast_t2_hours_charity_z_down



# graphs ------------------------------------------------------------------

tab_outcomes_up <- rbind(
  out_tab_contrast_t2_nzsei13_z_up,
  out_tab_contrast_t2_charity_donate_z_up#,
 # out_tab_contrast_t2_hours_charity_z_up
  )

tab_outcomes_up

tab_outcomes_down <- rbind(
  out_tab_contrast_t2_nzsei13_z_down,
  out_tab_contrast_t2_charity_donate_z_down#,
 # out_tab_contrast_t2_hours_charity_z_down
)


# # make group table
group_tab_outcomes_up <-
  group_tab(tab_outcomes_up , type = "RD")

# save
here_save(group_tab_outcomes_down, "group_tab_outcomes_down-hedwig")

group_tab_outcomes_down



# # make group table
group_tab_outcomes_down <-
  group_tab(tab_outcomes_down , type = "RD")

# save
here_save(group_tab_outcomes_down, "group_tab_outcomes_down-hedwig")

group_tab_outcomes_down


# graph -------------------------------------------------------------------

# create plots -------------------------------------------------------------

# check N
N <- n_unique(dat_long$id)
N

sub_title = "Shift up 1 x point in psychopathy if less than max response, else max response, N = 22450"

one_point_in_sd_units

# graph health
plot_group_tab_outcomes_up<- margot_plot(
  group_tab_outcomes_up,
  type = "RD",
  title = "Psychopathy effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
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

plot_group_tab_outcomes_up
dev.off()

plot_group_tab_outcomes_up
# save graph
ggsave(
  plot_group_tab_outcomes_up,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_outcomes_up-hedwig.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# plot 2 ------------------------------------------------------------------




sub_title_1 = "Shift down 1 x point in psychopathy if less than max response, else max response, N = 22450"
# graph health
plot_group_tab_outcomes_down <- margot_plot(
  group_tab_outcomes_down,
  type = "RD",
  title = "Psychopathy effects",
  subtitle = sub_title_1,
  xlab = "",
  ylab = "",
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
plot_group_tab_outcomes_down
dev.off()



# save graph
ggsave(
  plot_group_tab_outcomes_down,
  path = here::here(here::here(push_mods, "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_outcomes_down-hedwig.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()






plot_compare_up_down <-plot_group_tab_outcomes_down + plot_group_tab_outcomes_up+ plot_annotation(title = "Shift Intervention Comparisions: -1 down/+1pt up", tag_level = "A")


plot_compare_up_down


ggsave(
  plot_compare_up_down,
  path = here::here(here::here(push_mods, "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_compare_up_down.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()









# ANALYSIS OF ONLY COLDHEARTEDNESS ----------------------------------------



names_base_c <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost,
                     -t0_psychopathy_scale_z)|> colnames()




names_base_c

names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

# set variables for models ------------------------------------------------

#### SET VARIABLE NAMES: Customise for each outcomewide model
#  model
A_c <- "t1_coldheartedness_sans_sdo_z"

C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))

# check
print(W)


f_up_c
f_down_c
# models

#"nzsei"

t2_nzsei13_z_up_c <- lmtp_tmle(
  data = df_clean,
  trt = A_c,
  baseline = names_base_c,
  outcome = "t2_nzsei13_z",
  cens = C,
  shift = f_up_c,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_nzsei13_z_up_c 
t2_charity_donate_z_up_c

here_save(t2_nzsei13_z_up_c , "t2_nzsei13_z_up-hedwig_c")


t2_nzsei13_z_down_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c ,
  baseline = names_base_c,
  outcome = "t2_nzsei13_z",
  cens = C,
  shift = f_down_c ,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_nzsei13_z_down_c 
here_save(t2_nzsei13_z_down_c , "t2_nzsei13_z_down-hedwig_c ")


t2_nzsei13_z_null_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c,
  baseline = names_base_c,
  outcome = "t2_nzsei13_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_nzsei13_z_null_c 
here_save(t2_nzsei13_z_null_c , "t2_nzsei13_z_null-hedwig_c ")





# chartiy ----------------------------------------------------------------


# t2_charity_donate_z
t2_charity_donate_z_up_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c,
  baseline = names_base_c,
  outcome = "t2_charity_donate_log_z",
  cens = C,
  shift = f_up_c,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_charity_donate_z_up_c 
here_save(t2_charity_donate_z_up_c , "t2_charity_donate_z_up-hedwig_c")


t2_charity_donate_z_down_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c,
  baseline = names_base_c,
  outcome = "t2_charity_donate_log_z",
  cens = C,
  shift = f_down_c,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)



t2_charity_donate_z_down_c
here_save(t2_charity_donate_z_down_c , "t2_charity_donate_z_down-hedwig_c ")

t2_charity_donate_z_null_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c,
  baseline = names_base_c,
  outcome = "t2_charity_donate_log_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_charity_donate_z_null_c 
here_save(t2_charity_donate_z_null_c , "t2_charity_donate_z_null-hedwig_c ")


#t2_hours_charity_z

t2_hours_charity_z_up_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c ,
  baseline = names_base_c,
  outcome = "t0_hours_volunteering_log_z",
  cens = C,
  shift = f_up_c ,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_hours_charity_z_up_c 
here_save(t2_hours_charity_z_up_c , "t2_hours_charity_z_up-hedwig_c ")


t2_hours_charity_z_down_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c ,
  baseline = names_base_c,
  outcome = "t0_hours_volunteering_log_z",
  cens = C,
  shift = f_down,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_hours_charity_z_down_c 
here_save(t2_hours_charity_z_down_c , "t2_hours_charity_z_down-hedwig_c")


t2_hours_charity_z_null_c  <- lmtp_tmle(
  data = df_clean,
  trt = A_c,
  baseline = names_base_c,
  outcome = "t0_hours_volunteering_log_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_hours_charity_z_null_c 
here_save(t2_hours_charity_z_null_c, "t2_hours_charity_z_null-hedwig_c")


# contrasts
# nzsei
t2_nzsei13_z_up_c  <-
  here_read("t2_nzsei13_z_up-hedwig_c")

t2_nzsei13_z_down_c  <-
  here_read("t2_nzsei13_z_down-hedwig_c")

t2_hours_charity_z_null_c <-
  here_read("t2_nzsei13_z_null-hedwig_c")

# first contrast
contrast_t2_nzsei13_z_up_c  <-
  lmtp_contrast(t2_nzsei13_z_up_c ,
                ref = t2_nzsei13_z_null_c ,
                type = "additive")

tab_contrast_t2_nzsei13_z_up_c  <-
  margot_tab_lmtp(contrast_t2_nzsei13_z_up_c ,
                  scale = "RD",
                  new_name = "Objective job success (SD)")


out_tab_contrast_t2_nzsei13_z_up_c  <-
  lmtp_evalue_tab(tab_contrast_t2_nzsei13_z_up_c ,
                  scale = c("RD"))

out_tab_contrast_t2_nzsei13_z_up_c 


# second contrast
contrast_t2_nzsei13_z_down_c  <-
  lmtp_contrast(t2_nzsei13_z_down_c ,
                ref = t2_nzsei13_z_null_c ,
                type = "additive")

tab_contrast_t2_nzsei13_z_down_c  <-
  margot_tab_lmtp(contrast_t2_nzsei13_z_down_c,
                  scale = "RD",
                  new_name = "Objective job success (SD)")


out_tab_contrast_t2_nzsei13_z_down_c  <-
  lmtp_evalue_tab(tab_contrast_t2_nzsei13_z_down_c,
                  scale = c("RD"))

out_tab_contrast_t2_nzsei13_z_down_c 


# t2_charity_donate_z
t2_charity_donate_z_up_c  <-
  here_read("t2_charity_donate_z_up-hedwig_c")

t2_charity_donate_z_down_c <-
  here_read("t2_charity_donate_z_down-hedwig_c")

t2_charity_donate_z_down_null_c <-
  here_read("t2_charity_donate_z_null-hedwig_c")

# first contrast
contrast_t2_charity_donate_z_up_c <-
  lmtp_contrast(t2_charity_donate_z_up_c,
                ref = t2_charity_donate_z_null_c,
                type = "additive")

tab_contrast_t2_charity_donate_z_up_c <-
  margot_tab_lmtp(contrast_t2_charity_donate_z_up_c,
                  scale = "RD",
                  new_name = "Charity donations (SD)")


out_tab_contrast_t2_charity_donate_z_up_c <-
  lmtp_evalue_tab(tab_contrast_t2_charity_donate_z_up_c,
                  scale = c("RD"))

out_tab_contrast_t2_charity_donate_z_up_c


# second contrast
contrast_t2_charity_donate_z_down_c <-
  lmtp_contrast(t2_charity_donate_z_down_c,
                ref = t2_charity_donate_z_null_c,
                type = "additive")

tab_contrast_t2_charity_donate_z_down_c <-
  margot_tab_lmtp(contrast_t2_charity_donate_z_down_c,
                  scale = "RD",
                  new_name = "Charity donations (SD)")


out_contrast_t2_charity_donate_z_down_c<-
  lmtp_evalue_tab(tab_contrast_t2_charity_donate_z_down_c,
                  scale = c("RD"))

out_contrast_t2_charity_donate_z_down_c



# volunteering
t2_hours_charity_z_up_c <-
  here_read("t2_hours_charity_z_up-hedwig_c")

t2_hours_charity_z_down_c <-
  here_read("t2_hours_charity_z_down-hedwig_c")

t2_hours_charity_z_null_c <-
  here_read("t2_hours_charity_z_null-hedwig_c")

# first contrast
contrast_t2_hours_charity_z_up_c <-
  lmtp_contrast(t2_hours_charity_z_up_c,
                ref = t2_hours_charity_z_null_c,
                type = "additive")

tab_contrast_t2_hours_charity_z_up_c <-
  margot_tab_lmtp(contrast_t2_hours_charity_z_up_c,
                  scale = "RD",
                  new_name = "Weekly hours volunteering(SD)")


out_tab_contrast_t2_hours_charity_z_up_c<-
  lmtp_evalue_tab(tab_contrast_t2_hours_charity_z_up_c,
                  scale = c("RD"))

out_tab_contrast_t2_hours_charity_z_up_c


# second contrast
contrast_t2_hours_charity_z_down_c <-
  lmtp_contrast(t2_hours_charity_z_down_c,
                ref = t2_hours_charity_z_null_c,
                type = "additive")

contrast_t2_hours_charity_z_down_c

tab_contrast_t2_hours_charity_z_down_c <-
  margot_tab_lmtp(contrast_t2_hours_charity_z_down_c,
                  scale = "RD",
                  new_name = "Weekly hours volunteering (SD)")

tab_contrast_t2_hours_charity_z_down_c
out_tab_contrast_t2_hours_charity_z_down_c<-
  lmtp_evalue_tab(tab_contrast_t2_hours_charity_z_down_c,
                  scale = c("RD"))

out_tab_contrast_t2_hours_charity_z_down_c



# graphs ------------------------------------------------------------------

tab_outcomes_up_c <- rbind(
  out_tab_contrast_t2_nzsei13_z_up_c,
  out_tab_contrast_t2_charity_donate_z_up_c#,
 # out_tab_contrast_t2_hours_charity_z_up_c
)

tab_outcomes_up_c

tab_outcomes_down_c <- rbind(
  out_tab_contrast_t2_nzsei13_z_down_c,
  out_contrast_t2_charity_donate_z_down_c#,
 # out_tab_contrast_t2_hours_charity_z_down_c
)


# make group table
group_tab_outcomes_up_c <- group_tab(tab_outcomes_up_c  , type = "RD")

# save
here_save(group_tab_outcomes_up_c, "group_tab_outcomes_up-hedwig_c")


# # make group table
group_tab_outcomes_down_c <-
  group_tab(tab_outcomes_down_c , type = "RD")

# save
here_save(group_tab_outcomes_down_c, "group_tab_outcomes_down-hedwig_c")

group_tab_outcomes_down_c
# graph -------------------------------------------------------------------



# create plots -------------------------------------------------------------

# check N
N <- n_unique(dat_long$id)
N

sub_title_c = "Shift up 1 x point in coldheartedness if less than max response, else max response, N = 22450"

one_point_in_sd_units_c

# graph health
plot_group_tab_outcomes_up_c <- margot_plot(
  group_tab_outcomes_up_c,
  type = "RD",
  title = "Psychopathy effects",
  subtitle = sub_title_c,
  xlab = "",
  ylab = "",
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

plot_group_tab_outcomes_up_c
dev.off()

plot_group_tab_outcomes_up_c
# save graph
ggsave(
  plot_group_tab_outcomes_up_c,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_outcomes_up-hedwig_c.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# plot 2 ------------------------------------------------------------------




sub_title_1_c = "Shift down 1 x point in coldheartedness if less than max response, else max response, N = 22450"
# graph health
plot_group_tab_outcomes_down_c <- margot_plot(
  group_tab_outcomes_down_c,
  type = "RD",
  title = "Coldheartedness effects",
  subtitle = sub_title_1_c,
  xlab = "",
  ylab = "",
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
plot_group_tab_outcomes_down_c
dev.off()



# save graph
ggsave(
  plot_group_tab_outcomes_down_c,
  path = here::here(here::here(push_mods, "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_outcomes_down-hedwig_c.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()








# group plot --------------------------------------------------------------

plot_group_tab_outcomes_up_c + plot_group_tab_outcomes_down_c

# comparative intervention graphs -----------------------------------------

# combo graphs

plot_compare_up_down_c <-plot_group_tab_outcomes_up_c + plot_group_tab_outcomes_down_c + plot_annotation(title = 
                                                                                           "Shift Intervention Comparisions: -1 down/+1pt up", tag_level = "A")



ggsave(
  plot_compare_up_down_c,
  path = here::here(here::here(push_mods, "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_compare_up_down_c.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()

plot_compare_up_down_c
