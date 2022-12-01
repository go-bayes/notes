# revision conducted on 24 Nov 2022.
# joseph.bulbulia


# bayesian approach
# impute religious identification
# see this https://github.com/paul-buerkner/brms/issues/1385
# perfect impute
# note:
# potentially helpful for brms predict with added noise: https://github.com/paul-buerkner/brms/issues/544
# see also: https://discourse.mc-stan.org/t/predict-brms-in-multivariate-model-with-imputation/6334


options(scipen = 999)
#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for saving models (bulbulia only - use own paths for simulated data)
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/attacks/mods")
push_figs <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/attacks/figs")

# install.packages("arrow", repos = c(arrow = "https://nightlies.apache.org/arrow/r", getOption("repos")))
# read data (again bulbulia only) If replicating use the jittered data in the data folder
pull_path <-
  fs::path_expand(
    "/Users/joseph/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

pull_path
#arrow::write_parquet(time13, (here::here("data", "time13")))
# wow this is fast
#time13 <- read_parquet( (here::here("data", "time13")))

dat <- arrow::read_parquet(pull_path)

max(dat$SampleOriginYear)

df <- dat |>
  filter(YearMeasured == 1)
table1::table1(~ as.factor(BornTerritorialAuthority) + Warm.Muslims |
                 Wave,
               data = df)
# wrangle data
# create basic outcomewide dataframe from which we will select the a small dataframe.
dat_bayes <- dat |>
  arrange(Id, Wave) |>
  mutate(Male = if_else(GendAll == 1, 1, 0)) |>
  dplyr::select(
    Id,
    Wave,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    HLTH.BMI,
    REGC_2022,
    Rural_GCH2018,
    SampleOriginYear,
    #  BornTerritorialAuthority,
    REGC_2022,
    Age,
    Male,
    EthCat,
    BornNZ,
    Employed,
    Edu,
    Pol.Orient,
    SDO,
    RWA,
    NZSEI06,
    NZSEI13,
    NZDep2013,
    Relid,
    Partner,
    Parent,
    TSCORE,
    Warm.Asians,
    Warm.Chinese,
    #Warm.Disabled, #only in wave12
    #Warm.Elderly,
    Warm.Immigrants,
    Warm.Indians,
    Warm.Maori,
    #  Warm.MentalIllness,
    # not in 8
    Warm.Muslims,
    Warm.NZEuro,
    Warm.Overweight,
    Warm.Pacific,
    RaceRejAnx,
    #   Warm.Refugees,
    # not in 8
    TSCORE,
    YearMeasured
  ) |>
  dplyr::mutate(Employed = as.numeric(Employed)) |>
  dplyr::filter(
    (Wave ==   2012 & YearMeasured == 1) |
      (Wave ==   2013 & YearMeasured == 1) |
      (Wave ==   2014 & YearMeasured == 1) |
      (Wave ==   2015 & YearMeasured == 1) |
      (Wave ==   2016 & YearMeasured == 1) |
      (Wave ==  2017 & YearMeasured != -1) |
      (Wave ==  2018 & YearMeasured != -1) |
      (Wave ==  2019 & YearMeasured != -1) |
      (Wave ==  2020 & YearMeasured != -1) |
      (Wave == 2021 & YearMeasured != -1)
  ) %>%
  droplevels() |>
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  dplyr::mutate(
    lag_warm.muslims = dplyr::lag(Warm.Muslims),
    lag_overweight = dplyr::lag(Warm.Overweight)
  ) |>
  #  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
  #                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2013 =  ifelse(Wave == 2013 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2014 =  ifelse(Wave == 2014 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
                                    YearMeasured == 1, 1, 0)) %>%
  # dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
  #                                   YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  # dplyr::mutate(hold12 = mean(org2012, na.rm = TRUE)) %>%  # Hack
  # dplyr::filter(hold12 > 0) %>%
  dplyr::mutate(hold13 = mean(org2013, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold13 > 0) %>%
  dplyr::mutate(hold14 = mean(org2014, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold14 > 0) %>%
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold > 0) %>%
  dplyr::mutate(hold2 = mean(org2017, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold2 > 0) %>%
  # dplyr::mutate(hold3 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  # dplyr::filter(hold3 > 0) %>%
  ungroup(Id) |>
  dplyr::mutate(Edu = as.numeric(Edu)) |>
  arrange(Id, Wave) %>%
  group_by(Id) |>
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2016", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b,  .direction = "downup") %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2017,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2018,
      TSCORE_b + 730,
      ifelse(
        YearMeasured == 0 & Wave == 2019,
        TSCORE_b + 1094,
        # leap
        ifelse(
          YearMeasured == 0 &
            Wave == 2020,
          TSCORE_b + 1459,
          if_else(YearMeasured == 0 &
                    Wave == 2021,   TSCORE_b + 1824,
                  TSCORE)
        )
      )
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020 |
         Wave == 2021),
    1,
    0
  )))) %>% # All 2019s even if NA need to be 1
  #dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(
    Y_Warm.Asians = Warm.Asians,
    Y_Warm.Chinese = Warm.Chinese,
    # Warm.Disabled, only in wave12
    # Y_Warm.Elderly = Warm.Elderly,
    Y_Warm.Immigrants = Warm.Immigrants,
    Y_Warm.Indians = Warm.Indians,
    Y_Warm.Maori = Warm.Maori,
    #  Y_Warm.MentalIllness = Warm.MentalIllness,
    # not in 8
    Y_Warm.Muslims = Warm.Muslims,
    Y_Warm.NZEuro = Warm.NZEuro,
    Y_Warm.Overweight = Warm.Overweight,
    Y_Warm.Pacific = Warm.Pacific,
    #  Y_Warm.Refugees = Warm.Refugees,
    As = Attack
  ) %>%
  dplyr::mutate(Warm.Muslims_b = if_else(Wave == "2016", (Warm.Muslims), NA_real_)) %>%
  fill(Warm.Muslims_b, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_b = if_else(Wave == "2016", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_b, .direction = "downup") %>%
  dplyr::mutate(Warm.Muslims_c = if_else(Wave == "2017", (Warm.Muslims), NA_real_)) %>%
  fill(Warm.Muslims_c, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_c = if_else(Wave == "2017", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_c, .direction = "downup") %>%
  dplyr::mutate(SampleOriginYear_b = if_else(Wave == "2016", (SampleOriginYear - 1), NA_real_)) %>%
  fill(SampleOriginYear_b, .direction = "downup") %>%
  dplyr::mutate(REGC_2022_c = if_else(Wave == "2017", as.numeric(REGC_2022), NA_real_)) %>%
  fill(REGC_2022_c, .direction = "downup") %>%
  dplyr::mutate(Rural_GCH2018_c = if_else(Wave == "2017", as.numeric(Rural_GCH2018), NA_real_)) %>%
  fill(Rural_GCH2018_c, .direction = "downup") |>
  dplyr::mutate(Age_c = if_else(Wave == "2017", (Age), NA_real_)) %>%
  fill(Age_c, .direction = "downup") %>%
  dplyr::mutate(HLTH.BMI_c = if_else(Wave == "2017", (HLTH.BMI), NA_real_)) %>%
  fill(HLTH.BMI_c, .direction = "downup") %>%
  dplyr::mutate(CONSCIENTIOUSNESS_c = if_else(Wave == "2017", (CONSCIENTIOUSNESS), NA_real_)) %>%
  fill(CONSCIENTIOUSNESS_c,  .direction = "downup") %>%
  dplyr::mutate(OPENNESS_c = if_else(Wave == "2017", (OPENNESS), NA_real_)) %>%
  fill(OPENNESS_c,  .direction = "downup") |>
  dplyr::mutate(HONESTY_HUMILITY_c = if_else(Wave == "2017", (HONESTY_HUMILITY), NA_real_)) %>%
  fill(HONESTY_HUMILITY_c,  .direction = "downup") |>
  dplyr::mutate(EXTRAVERSION_c = if_else(Wave == "2017", (EXTRAVERSION), NA_real_)) %>%
  fill(EXTRAVERSION_c,  .direction = "downup") |>
  dplyr::mutate(NEUROTICISM_c = if_else(Wave == "2017", (NEUROTICISM), NA_real_)) %>%
  fill(NEUROTICISM_c,  .direction = "downup") |>
  dplyr::mutate(AGREEABLENESS_c = if_else(Wave == "2017", (AGREEABLENESS), NA_real_)) %>%
  fill(AGREEABLENESS_c,  .direction = "downup") |>
  dplyr::mutate(Male_c = if_else(Wave == "2017", as.numeric(Male), NA_real_)) %>%
  fill(Male_c,  .direction = "downup") %>%
  dplyr::mutate(NZDep2013_c = if_else(Wave == "2017", as.numeric(NZDep2013), NA_real_)) %>%
  fill(NZDep2013_c,  .direction = "downup")  %>%
  dplyr::mutate(RaceRejAnx_c = if_else(Wave == "2017", as.numeric(RaceRejAnx), NA_real_)) %>%
  fill(RaceRejAnx_c,  .direction = "downup")  %>%
  dplyr::mutate(EthCat_c = if_else(Wave == "2017", as.numeric(EthCat), NA_real_)) %>%
  fill(EthCat_c,  .direction = "downup") %>%
  dplyr::mutate(BornNZ_c = if_else(Wave == "2017", as.numeric(BornNZ), NA_real_)) %>%
  fill(BornNZ_c,  .direction = "downup")  %>%
  dplyr::mutate(Pol.Orient_c = if_else(Wave == "2017", (Pol.Orient), NA_real_)) %>%
  fill(Pol.Orient_c,  .direction = "downup") %>%
  dplyr::mutate(Relid_c = if_else(Wave == "2017", as.numeric(Relid), NA_real_)) %>%
  fill(Relid_c,  .direction = "downup") %>%
  dplyr::mutate(Partner_c = if_else(Wave == "2017", (as.numeric(Partner)), NA_real_)) %>%
  fill(Partner_c,  .direction = "downup") %>%
  dplyr::mutate(Parent_c = if_else(Wave == "2017", (as.numeric(Parent)), NA_real_)) %>%
  fill(Parent_c,  .direction = "downup") %>%
  dplyr::mutate(Employed_c = if_else(Wave == "2017", (as.numeric(Employed)), NA_real_)) %>%
  fill(Employed_c,  .direction = "downup") %>%
  dplyr::mutate(Edu_c = if_else(Wave == "2017", (Edu), NA_real_)) %>%
  fill(Edu_c,  .direction = "downup") %>%
  dplyr::mutate(SDO_c = if_else(Wave == "2017", (as.numeric(SDO)), NA_real_)) %>%
  fill(SDO_c,  .direction = "downup") %>%
  dplyr::mutate(RWA_c = if_else(Wave == "2017", (as.numeric(RWA)), NA_real_)) %>%
  fill(RWA_c,  .direction = "downup") %>%
  dplyr::mutate(NZSEI13_c = if_else(Wave == "2017", (as.numeric(NZSEI13)), NA_real_)) %>%
  fill(NZSEI13_c,  .direction = "downup") |>
  ungroup() %>%
  # select(
  #   -c(
  #     Employed,
  #     Urban,
  #     Edu,
  #     HLTH.BMI,
  #     Pol.Orient,
  #     SDO,
  #     RWA,
  #     NZSEI13,
  #     NZDep.2013,
#     Age,
#     Relid,
#     RaceRejAnx,
#     Partner,
#     Parent,
#     hold,
#     Age,
#     EthCat,
#     BornNZ,
#     TSCORE,
#     org2016,
#     hold,
#     CONSCIENTIOUSNESS,
#     OPENNESS,
#     HONESTY_HUMILITY,
#     EXTRAVERSION,
#     NEUROTICISM,
#     AGREEABLENESS
#   )
# ) |>
dplyr::mutate(EthCat_c = as.factor(EthCat_c)) |>
  dplyr::filter(#   !is.na(Warm.Muslims_b),!is.na(Warm.Overweight_b),!is.na(Age_c),!is.na(HLTH.BMI_c),!is.na(BornNZ_c),!is.na(Male_c),!is.na(Edu_c),!is.na(Employed_c),!is.na(EthCat_c),!is.na(Parent_c),!is.na(Partner_c),!is.na(Relid_c),!is.na(RaceRejAnx_c),!is.na(Pol.Orient_c),!is.na(REGC_2022),!is.na(Rural_GCH2018),!is.na(NZDep2013),
    !is.na(NZSEI13),
    !is.na(EthCat),
    !is.na(Edu_c)) |>
  dplyr::mutate(
    Rural_GCH2018_c = as.factor(Rural_GCH2018_c),
    #SampleOriginYear = as.factor(SampleOriginYear),
    #    BornTerritorialAuthority =  as.factor(BornTerritorialAuthority),
    REGC_2022_c = as.factor(REGC_2022_c),
    Age_cZ = scale(Age_c),
    HLTH.BMI_cZ = scale(HLTH.BMI_c),
    BornNZ_cZ = scale(BornNZ_c),
    Male_cZ = scale (Male_c),
    Edu_cZ = scale(Edu_c),
    Employed_cZ = scale(Employed_c),
    # EthCat_c = EthCat_c,
    Parent_cZ = scale(Parent_c),
    Partner_cZ = scale(Partner_c),
    Relid_cZ = scale(Relid_c),
    RaceRejAnx_cZ = scale(RaceRejAnx_c),
    Pol.Orient_cZ = scale(Pol.Orient_c),
    # Urban_cZ = scale(Urban_c),
    SDO_cZ = scale(SDO_c),
    RWA_cZ = scale(RWA_c),
    NZDep2013_cZ = scale(NZDep2013_c),
    NZSEI13_cZ = scale(NZSEI13_c),
    AGREEABLENESS_cZ = scale(AGREEABLENESS_c),
    CONSCIENTIOUSNESS_cZ = scale(CONSCIENTIOUSNESS_c),
    OPENNESS_cZ = scale(OPENNESS_c),
    HONESTY_HUMILITY_cZ = scale(HONESTY_HUMILITY_c),
    EXTRAVERSION_cZ = scale(EXTRAVERSION_c),
    NEUROTICISM_cZ = scale(NEUROTICISM_c)
  ) %>%
  dplyr::filter(Wave != 2012) |>
  droplevels() |>
  mutate(Sample = as.factor(if_else(
    SampleOriginYear < 2,
    0,
    if_else(
      SampleOriginYear >= 2  &
        SampleOriginYear < 4,
      1,
      if_else(SampleOriginYear == 4, 2, 3)
    )
  ))) |>
  dplyr::arrange(Id, Wave)

#"Time4"
table(dat_bayes$Sample)
# relabel wave
levels(dat_bayes$Wave) <-
  c("Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10",
    "Time11",
    "Time12",
    "Time13")

table(dat_bayes$Sample)


! is.na(Pol.Orient_c), !is.na(REGC_2022), !is.na(Rural_GCH2018),!is.na(NZDep2013), !is.na(NZSEI13)

table1(
  ~ Sample + Warm.Muslims + lag_warm.muslims + Male + Age + EthCat +
    REGC_2022  + Rural_GCH2018 +  EthCat + Edu_cZ + Pol.Orient_cZ +  NZDep2013 + NZSEI13 |
    Wave,
  data = dat_bayes
)

summary(lm (
  Warm.Muslims ~ as.numeric(Wave) * (
    Sample + Age + Male + EthCat_c + REGC_2022 + Rural_GCH2018 +
      Edu_cZ + Pol.Orient_cZ +  NZDep2013 + NZSEI13
  ),
  data = dat_bayes
))

length(unique(dat_bayes$Id)) #

dat_bayes[, c("lag_warm.muslims", "Warm.Muslims")]

# image
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = dat_bayes)


# check
x <- table1::table1(
  ~ Y_Warm.Muslims + Y_Warm.Overweight |
    factor(Wave) * factor(As),
  data = dat_bayes,
  overall = F
)
x

kable(x, format = "latex", booktabs = TRUE)
t1kable(x, format = "latex")

# Missing data problem
t2 <- table1::table1( ~ Y_Warm.Muslims |
                        Wave * as.factor(As),
                      data = dat_bayes,
                      overall = F)

# data prep

# create new data set
dt_prep <- dat_bayes %>%
  group_by(Id) %>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      0,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        1,
        Attack
      )
    )
  )) %>%
  mutate(
    Y_Warm.Asians = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.Asians
      )
    )
  ) %>%
  mutate(
    Y_Warm.Overweight = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.Overweight
      )
    )
  ) %>%
  mutate(
    Y_Warm.Chinese = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.Chinese
      )
    )
  ) %>%
  # mutate(
  #   Y_Warm.Elderly = ifelse(
  #     Wave == "Time10" & Attack == 1 |
  #       Wave == "Time11" |
  #       Wave == "Time12",
  #     NA,
  #     ifelse(
  #       Wave == "Time10" & Attack == 0 |
  #         Wave == "Time9" |
  #         Wave == "Time8",
  #       NA,
#       Warm.Elderly
#     )
#   )
# ) %>%
mutate(
  Y_Warm.Immigrants = ifelse(
    Wave == "Time10" & Attack == 1 |
      Wave == "Time11" |
      Wave == "Time12" | Wave == "Time13",
    NA,
    ifelse(
      Wave == "Time10" & Attack == 0 |
        Wave == "Time9" |
        Wave == "Time8" |
        Wave == "Time7" |
        Wave == "Time6" |
        Wave == "Time5",
      NA,
      Warm.Immigrants
    )
  )
) %>%
  mutate(
    Y_Warm.Indians = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.Indians
      )
    )
  ) %>%
  mutate(
    Y_Warm.Maori = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.Maori
      )
    )
  ) %>%
  # mutate(
  #   Y_Warm.MentalIllness = ifelse(
  #     Wave == "Time10" & Attack == 1 |
  #       Wave == "Time11" |
  #       Wave == "Time12",
  #     NA,
  #     ifelse(
  #       Wave == "Time10" & Attack == 0 |
  #         Wave == "Time9" |
  #         Wave == "Time8",
  #       NA,
#       Warm.MentalIllness
#     )
#   )
# ) %>%
mutate(
  Y_Warm.Muslims = ifelse(
    Wave == "Time10" & Attack == 1 |
      Wave == "Time11" |
      Wave == "Time12" | Wave == "Time13",
    NA,
    ifelse(
      Wave == "Time10" & Attack == 0 |
        Wave == "Time9" |
        Wave == "Time8" |
        Wave == "Time7" |
        Wave == "Time6" |
        Wave == "Time5",
      NA,
      Warm.Muslims
    )
  )
) %>%
  mutate(
    Y_Warm.NZEuro = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.NZEuro
      )
    )
  ) |>
  mutate(
    Y_Warm.Pacific = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5",
        NA,
        Warm.Pacific
      )
    )
  ) |>
  ungroup() %>%
  arrange(Id, Wave)

# check
length(unique(dt_prep$Id))


skimr::skim(dt_prep) %>%
  arrange(n_missing)

head(dt_prep)
colnames(dt_prep)

str(dt_prep)
hist(dt_prep$Age_cZ)

head(dt_prep)



# bind data
dt_bind <-  dat_bayes %>%
  bind_rows(dt_prep) %>%
  arrange(Id, Wave)


# Test NAs = Correct
table1::table1(
  ~ Y_Warm.Muslims +
    # Y_Warm.Chinese +
    # # Warm.Disabled, only in wave12
    # #  Y_Warm.Elderly +
    # Y_Warm.Immigrants +
    # Y_Warm.Indians +
    # Y_Warm.Maori +
    # #   Y_Warm.MentalIllness +  # not in 8
    # Y_Warm.Muslims +
    # Y_Warm.NZEuro +
    Y_Warm.Overweight #+
  #  Y_Warm.Pacific# +
  #   Y_Warm.Refugees
  | Wave * as.factor(As),
  data = dt_bind,
  overall = F
)


#  data wrangle
dt_bind
dt_bind$YearMeasured
# link dfs for zero estimate -----------------------------------------

# five data
head()

dt_temp <- dt_bind |>
  dplyr::filter((As == 0 & YearMeasured != -1)) |>
  # filter(Wave != "Time11" & Wave != "Time12") |>
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_ni <- dt_temp

dt_ni$wave

dt_ni$wave  =  dt_temp$wave - 6

table(dt_ni$wave)

## no impute but with the years



# one data
dt_temp1 <- dt_bind |>
  dplyr::filter((As == 1 & Wave == "Time10") |
                  (As == 1 & Wave == "Time11") |
                  (As == 1 & Wave == "Time12") |
                  (As == 1 & Wave == "Time13")
  ) %>%
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_ni1 <- dt_temp1
dt_ni1$Wave
dt_ni1$wave  <- dt_temp1$wave - 6

table(dt_ni1$wave) # Correct


str(dt_ni1)

# Check missing
library(naniar)
naniar::gg_miss_var(dt_ni)
naniar::gg_miss_var(dt_ni1)


## save data

arrow::write_parquet(dt_ni,
                     here::here(push_mods, "dt_ni-attacks.rds"))
arrow::write_parquet(dt_ni1,
                     here::here(push_mods, "dt_ni1-attacks.rds"))




# read-prepared-data ------------------------------------------------------

dt_ni <-
  arrow::read_parquet(here::here(push_mods, "dt_ni-attacks.rds"))
dt_ni1 <-
  arrow::read_parquet(here::here(push_mods, "dt_ni1-attacks.rds"))

# check
table(dt_ni$wave)
table(dt_ni1$wave)

# bayes models ------------------------------------------------------------


prior = c(
  set_prior('normal(0, 1)', class = "b"),
  set_prior(
    "student_t(3, 4, 1)",
    class = "Intercept",
    lb = 1,
    ub = 7
  )
)

## impute muslim
# ensure parrallel computations -------------------------------------------


library("brms")
library("rstan")

rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
library(cmdstanr)


bform_mus <-
  bf(
    Y_Warm.Muslims | mi()  ~ wave +
      Sample +
      Age +
      Male +
      EthCat_c +
      REGC_2022 +
      Rural_GCH2018 +
      Edu_cZ +
      Pol.Orient_cZ +
      NZDep2013 +
      NZSEI13 +
      wave:Pol.Orient_cZ
    + (1 | Id)
  )




bform_mus_b <-
  bf(
    Y_Warm.Muslims | mi()  ~ mi(lag_warm.muslims) + wave +
      Sample +
      Age +
      Male +
      EthCat_c +
      REGC_2022 +
      Rural_GCH2018 +
      Edu_cZ +
      Pol.Orient_cZ +
      NZDep2013 +
      NZSEI13 +
      wave:Pol.Orient_cZ  + (1 | Id)
  ) +
  bf(lag_warm.muslims | mi() ~ 
       Sample +
       Age +
       Male +
       EthCat_c +
       REGC_2022 +
       Rural_GCH2018 +
       Edu_cZ +
       Pol.Orient_cZ +
       NZDep2013 +
       NZSEI13 +
       wave:Pol.Orient_cZ + (1 | Id)) + set_rescor(rescor = FALSE)
       



m_0 <- brm(
  backend = "cmdstanr",
  data = dt_ni,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file =  here::here(push_mods, "impute-2012-zero-MUS-attacks-use.rds")
)


summary(m_0)


# FAIL! 
# m_0b <- brm(
#   backend = "cmdstanr",
#   data = dt_ni,
#   family = "gaussian",
#   bform_mus_b,
#   prior = prior,
#   init = 0,
#   file =  here::here(push_mods, "impute-2012-zero-MUS-attacks-use-b.rds")
# )



# M2
m_1 <- brm(
  backend = "cmdstanr",
  data = dt_ni1,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file = here::here(push_mods, "impute-2012-one-MUS-attacks-use.rds")
)
summary(m_1)




bform_mus <-
  bf(
    Y_Warm.Muslims | mi()  ~ wave +
      Sample +
      Age +
      Male +
      EthCat_c +
      REGC_2022 +
      Rural_GCH2018 +
      Edu_cZ +
      Pol.Orient_cZ +
      NZDep2013 +
      NZSEI13 +
      wave:Pol.Orient_cZ
    + (1 | Id)
  )


m_0a <- brm(
  backend = "cmdstanr",
  data = dt_ni,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file =  here::here(push_mods, "impute-2012-zero-MUS-attacks-use.rds")
)



# M2
m_1a <- brm(
  backend = "cmdstanr",
  data = dt_ni1,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file = here::here(push_mods, "impute-2012-one-MUS-attacks-use.rds")
)
summary(m_1)



# preliminary-inspection-imputations --------------------------------------


p0 <-
  plot(ggeffects::ggpredict(m_0, terms = c("wave [0:3]", "Pol.Orient_cZ"))) + scale_y_continuous(limits = c(3, 5))


p0b <-
  plot(ggeffects::ggpredict(m_0b, terms = c("wave [0:3]", "Pol.Orient_cZ"))) + scale_y_continuous(limits = c(3, 5))

p0b
p0

p1 <-
  plot(ggeffects::ggpredict(m_1, terms = c("wave [0:3]", "Pol.Orient_cZ"))) + scale_y_continuous(limits = c(3, 5))

summary(m_0)

p0a <-
  plot(ggeffects::ggpredict(m_0, terms = c("wave [0:3]"))) + scale_y_continuous(limits = c(3, 5))
p1b <-
  plot(ggeffects::ggpredict(m_1, terms = c("wave [0:3]"))) + scale_y_continuous(limits = c(3, 5))


p0a + p1b



# save-imputations --------------------------------------------------------
name_error = "sd"

# set N for id counts
id_0<- m_0$data$Id
id_1 <- m_1$data$Id

standata(m_0)

length(m_0$data$Id)

# analysis
name <- "yfit_muslim"

fitted_values_0 <- predict(m_0, ndraws = 50)
fitted_values_0


fitted_values_1 <- predict(m_1,  ndraws = 50)


# make df
fitted_values_0 <- data.frame(fitted_values_0)
head(fitted_values_0)
head(fitted_values_0)
mean(fitted_values_0$Est.Error)

nrow(fitted_values_0)

# needs to be df
yfit <- as.data.frame(fitted_values_0$Estimate)
sd <- as.data.frame(fitted_values_0$Est.Error)
# rename
colnames(yfit) <- name
colnames(sd) <- name_error

length(id_0)



# data frame
dat_0 <-
  as.data.frame(cbind(Y_orig = standata(m_0)$Y, standata(m_0)$X, yfit,id_0, sd)) |>
  mutate(id = as.factor(id_0)) |>
  arrange(id, wave)

dat_0 <- dat_0 |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(0, nrow(dat_0)))) |>
  select(-id_0)


dat_0_wide = dat_0 |>
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  mutate(yimpute_muslim_lag = dplyr::lag(yimpute_muslim))


head(dat_0_wide$yimpute_muslim_lag)

table(dat_0$wave)
## Same for 1s

# make df
fitted_values_1 <- data.frame(fitted_values_1)

# needs to be df
yfit1 <- as.data.frame(fitted_values_1$Estimate)
sd <- as.data.frame(fitted_values_1$Est.Error)

# rename
colnames(yfit1) <- name
colnames(sd) <- name_error

# data frame
dat_1 <-
  as.data.frame(cbind(Y_orig = standata(m_1)$Y, standata(m_1)$X, yfit1, id_1, sd)) |>
  mutate(id = as.factor(id_1))

dat_1 <- dat_1 |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(1, nrow(dat_1)))) |>
  select(-id_1)

# combine data

dat_combined <- rbind(dat_0, dat_1) |>
  filter(wave == 0 | wave == 1 | wave == 2 | wave == 3) |>
  mutate(Wave = as.factor(wave),
         Condition = as)

str(dat_combined)

# # save processed data
# saveRDS(dat_combined,
#         here::here(push_mods, "g-comp-processed-muslims-attack"))
#
# # read processed data
# dat_combined_muslim <-
#   readRDS(here::here(push_mods, "g-comp-processed-muslims-attack"))
# table(dat_combined_muslim$wave)
#
# # read processed data
# head(dat_combined_muslim)

# wrangle for imputed values with errors
dat_combined_imputed_muslim_2012 <- dat_combined  |>
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  group_by(id) |>
  mutate(se = if_else(Y_orig != Inf, 0, sd)) |>
  ungroup() |>
  mutate(se = if_else(se <= 0, .01, se)) |>
  mutate(Wave = as.factor(Wave)) |>
  data.frame()

head(dat_combined_imputed_muslim_2012)

#save imputed values
arrow::write_parquet(
  dat_combined_imputed_muslim_2012,
  here::here(push_mods, "dat_combined_imputed_muslim-attack-2012")
)

dat_combined_imputed_muslim_2012 <- arrow::read_parquet(
  here::here(push_mods, "dat_combined_imputed_muslim-attack-2012")
)


# gee ---------------------------------------------------------------------



library(geepack)



model_gee_muslim <-
  geeglm(
    data = dat_combined_imputed_muslim_2012,
    formula = yimpute_muslim ~  as.factor(as) * Wave * Pol.Orient_cZ,
    id = id,
    corstr = "ar1"
  )

model_parameters(model_gee_muslim) |>
  print_md()

gee_muslim <-
  plot (ggeffects::ggpredict(
    model_gee_muslim,
    terms = c('Wave', "as", "Pol.Orient_cZ[-1.86, 0, 2.45]")
  )) + scale_y_continuous(limits = c(3, 5.5))
gee_muslim

options(scipen = 999)

comparisons(
  model_gee_muslim ,
  newdata = datagrid(
    as = c(0, 1),
    Wave = 0:3,
    Pol.Orient_cZ = c(-1.86, 0, 2.45)
  ),
  variables = c("as", "Pol.Orient_cZ")
)


plot_cme(
  model_gee_muslim,
   effect = "as",
  condition = c("Wave", "Pol.Orient_cZ"),
  conf_level = 0.95
)


plot_cme(
  model_gee_muslim,
  effect = "as",
  condition = c("Wave"),
  conf_level = 0.95
)

plot_cco(
  model_gee_muslim,
  effect = "as",
  condition = c("Wave", "Pol.Orient_cZ"),
  conf_level = 0.95,
  transform_pre = "ratio"
)

plot_cco(
  model_gee_muslim,
  effect = "as",
  condition = c("Wave", "Pol.Orient_cZ"),
  conf_level = 0.95,
  transform_pre = "difference"
)


# prepare data -------------------------------------------------------------

d_muslim <- dat_combined_imputed_muslim_2012 |>
  mutate(se = if_else(se <= 0, .01, se)) |>
  mutate(Wave = as.factor(Wave)) |>
  mutate(Attack = as.factor(as))



# #  gcomp ------------------------------------------------------------
#

prior_muslim = c(
  set_prior("normal(0,.5)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 2)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")  # only for raneffs
)




bform_mus_marg  =   bf(yimpute_muslim |
                         mi(se) ~  as  *  wave  + (1 | id),
                       sigma ~ 0 + as,
                       set_rescor(rescor = FALSE))

prior_re  = c(
  set_prior("normal(0,.5)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 2)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")  # only for raneffs
)


system.time(
  m_marg_mus <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim,
    family = "gaussian",
    bform_mus_marg,
    prior_re,
    init = 0,
    file = here::here(push_mods, "m_marg_mus-2012.rds")
  )
)


# Images to use
muslim_marg  <- plot(conditional_effects(
  m_marg_mus,
  "wave:as",
  spaghetti = TRUE,
  ndraws = 200,
  plot = F
))[[1]]




plot_muslim <- muslim_marg +
  labs(subtitle = "Muslim Warmth",
       y = "Muslim Warmth (1-7) ",
       x = "Years: 2018-2022; N = 13,409") +
  scale_colour_okabe_ito(alpha = .1) +
  theme_classic()   +
  scale_y_continuous(limits = c(4.0, 4.5))

plot_muslim

# effect modification -----------------------------------------------------
summary(m_cond_mus)

bform_mus_cond  =   bf(
  yimpute_muslim | mi(se) ~  Attack  *  Wave *  Pol.Orient_cZ + (1 |
                                                                   id),
  sigma ~ 0 + as,
  set_rescor(rescor = FALSE)
)

prior_mus_cond  = c(
  set_prior("normal(0,.5)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 2)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")  # only for raneffs
)





system.time(
  m_cond_mus <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim,
    family = "gaussian",
    bform_mus_cond,
    prior_mus_cond,
    init = 0,
    file = here::here(push_mods, "m_cond_mus-2012.rds")
  )
)


# Images to use
# muslim_cond  <- plot(
#   conditional_effects(
#     m_cond_mus,
#     "Wave:As",
#     spaghetti = TRUE,
#     ndraws =200,
#     plot = F))[[1]]
#

# brm_muslim <- plot (ggeffects::ggemmeans(m_cond_mus, terms = c('Wave', "Attack", "Pol.Orient_cZ[-1.86, 0, 2.45]"))) +
#   scale_y_continuous(limits = c(3,5.5))
#
# brm_muslim


plot_cme(
  m_cond_mus,
  effect = "Attack",
  condition = c("Wave", "Pol.Orient_cZ"),
  conf_level = 0.95,
  ndraws = 1000
)


plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = c("Wave", "Pol.Orient_cZ"),
  conf_level = 0.95,
  # transform_pre = "ratio",
  ndraws = 1000
)


plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = c("Wave", "Pol.Orient_cZ"),
  conf_level = 0.95,
  transform_pre = "ratio",
  ndraws = 1000
)



plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  #  transform_pre = "ratio",
  ndraws = 1000
)

pred_marg <- predictions(
  m_cond_mus,
  type = "response",
  newdata = datagrid(
    "Attack" = 0:1,
    "Wave" = 0:3
    #  "Pol.Orient_cZ" = c(-1.86, 0, 2.45)),
    ndraws = 1000,
    re_formula = NA
  ) |>
    posteriordraws()
  
  
  
  pred_cond <- predictions(
    m_cond_mus,
    type = "response",
    newdata = datagrid(
      "Attack" = 0:1,
      "Wave" = 0:3,
      "Pol.Orient_cZ" = c(-1.86, 0, 2.45)
    ),
    ndraws = 1000,
    re_formula = NA
  ) |>
    posteriordraws()
  
  
  
  library(ggdist)
  
  ggplot(pred_marg, aes(
    x = Wave,
    y = draw,
    fill = factor(Attack)
  )) +
    stat_halfeye(slab_alpha = 1) +
    labs(
      title = "Marginal effect of Attack on Muslim Warmth",
      x = "Political Right Orientation (SD)",
      y = "Predicted Response",
      fill = "Attack"
    ) +
    #  facet_grid(.~Wave,   shrink = TRUE) +
    scale_fill_okabe_ito(alpha = 1)
  
  
  ggplot(pred_cond, aes(
    x = Pol.Orient_cZ,
    y = draw,
    fill = factor(Attack)
  )) +
    stat_halfeye(slab_alpha = 1) +
    labs(
      title = "Conditional effect of Attack on Muslim Warmth",
      x = "Political Right Orientation (SD)",
      y = "Predicted Response",
      fill = "Attack"
    ) +
    facet_grid(. ~ Wave,   shrink = TRUE) +
    scale_fill_okabe_ito(alpha = 1)
  
  
  ggplot(pred_cond, aes(
    x = Pol.Orient_cZ, y = draw, fill = Attack
  )) +
    stat_halfeye(slab_alpha = .9) +
    #  stat_dotsinterval(quantiles = 1000, point_interval = mode_hdci) +
    # stat_dotsinterval(slab_alpha = .9) +
    labs(
      title = "Conditional effect of Attack on Muslim Warmth",
      x = "Political Right Orientation (SD)",
      y = "Predicted Response",
      fill = "Attack"
    ) +
    facet_grid(. ~ Wave,   shrink = TRUE) +
    scale_fill_okabe_ito()
  
  
  
  cmp <- comparisons(
    m_cond_mus,
    newdata = datagrid(
      Wave = 0:3,
      Attack = 0:1,
      Pol.Orient_cZ = c(-1.86, 0, 2.45)
    ),
    ndraws = 10,
    re_formula = NA
  )
  cmp
  summary(cmp)
  
  summary(m_cond_mus)
  out_cond_0 <- comparisons(
    m_cond_mus,
    type = "response",
    newdata = datagrid("Attack"),
    ndraws = 100,
    re_formula = NULL
  ) |>
    dplyr::mutate_if(is.numeric, round, 3)
  
  
  # |>
  #   slice(1:6) |>
  #   select(contrast:conf.high) #|>
  #   dplyr::mutate_if(is.numeric, round, 3)
  
  out_cond_0
  
  out_cond_1 <- comparisons(
    m_cond_mus,
    type = "response",
    newdata = datagrid(
      "Attack" = 0:1,
      "Wave" = 1,
      "Pol.Orient_cZ" = c(-1.86, 0, 2.45)
    ),
    ndraws = 10,
    re_formula = NULL
  ) |>
    tidy()
  
  out_cond_1
  
  out_cond_1 <- comparisons(
    m_cond_mus,
    type = "response",
    newdata = datagrid(
      "Attack" = 0:1,
      "Wave" = 1,
      "Pol.Orient_cZ" = c(-1.86, 0, 2.45)
    ),
    ndraws = 10,
    re_formula = NULL
  ) |>
    tidy()
  
  out_cond_0
  
  
  
  # compare
  
  model_parameters(model_gee_muslim) |>
    print_md()
  
  
  model_parameters(m_cond_mus) |>
    print_md()
  
  
  gee_comp <- comparisons(model_gee_muslim,
                          type = "response",
                          newdata = datagrid("as")) |>
    dplyr::mutate_if(is.numeric, round, 3)
  
  
  gee_comp
  
  plot_cco(effect = "as",
           cp)
  
  
  # d-muslims-wide ----------------------------------------------------------
  
  # ------------------------------------------------------------------
  # set name for error
  name_error = "sd"
  
  # set N for id counts
  id_0 <- dt_five_zero_noimpute$Id
  id_1 <- dt_five_one_noimpute$Id
  
  
  
  # analysis
  name <- "yfit_muslim"
  
  fitted_values_0 <- predict(m_0, ndraws = 50)
  fitted_values_0
  
  
  fitted_values_1 <- predict(m_1,  ndraws = 50)
  
  
  # make df
  fitted_values_0 <- data.frame(fitted_values_0)
  head(fitted_values_0)
  head(fitted_values_0)
  mean(fitted_values_0$Est.Error)
  
  # needs to be df
  yfit <- as.data.frame(fitted_values_0$Estimate)
  sd <- as.data.frame(fitted_values_0$Est.Error)
  # rename
  colnames(yfit) <- name
  colnames(sd) <- name_error
  
  # data frame
  dat_0 <-
    as.data.frame(cbind(
      Y_orig = standata(m_0)$Y, standata(m_0)$X, yfit, id_0, sd
    )) |>
    mutate(id = as.factor(id_0)) |>
    arrange(id, wave)
  
  dat_0_wide <- dat_0 |>
    # note changes
    mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                    yfit_muslim,
                                    Y_orig)) |>
    mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
    mutate(as = as.factor(rep(0, nrow(
      dat_0
    )))) |>
    arrange(id_0, wave) |>
    mutate(yimpute_muslim_lag = dplyr::lag(yimpute_muslim))
  
  
  yimpute_muslim_lag <- dat_0_wide |>
    filter(wave == 0) |>
    select(yimpute_muslim_lag, id_0)
  
  
  
  dat_0_wide_u <- dat_0_wide |>
    select(-yimpute_muslim_lag) |>
    select(-id_0)
  
  ## Same for 1s
  
  # make df
  fitted_values_1 <- data.frame(fitted_values_1)
  
  # needs to be df
  yfit1 <- as.data.frame(fitted_values_1$Estimate)
  sd <- as.data.frame(fitted_values_1$Est.Error)
  
  # rename
  colnames(yfit1) <- name
  colnames(sd) <- name_error
  
  # data frame
  dat_1 <-
    as.data.frame(cbind(
      Y_orig = standata(m_1)$Y, standata(m_1)$X, yfit1, id_1, sd
    )) |>
    mutate(id = as.factor(id_1))
  
  dat_1_wide <- dat_1 |>
    mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                    yfit_muslim,
                                    Y_orig)) |>
    mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
    mutate(as = as.factor(rep(1, nrow(
      dat_1
    )))) |>
    arrange(id_0, wave) |>
    select(-id_1)
  
  
  dat_0_wide_u
  # combine data
  
  dat_combined_u  <- rbind(dat_0_wide_u, dat_1_wide) |>
    filter(wave == 0 |
             wave == 1 | wave == 2 | wave == 3) |>
    mutate(Wave = as.factor(wave),
           Condition = as)
  
  str(dat_combined_u)
  
  # save processed data
  saveRDS(
    dat_combined_u,
    here::here(push_mods, "dat_combined_u-muslims-attack")
  )
  
  # read processed data
  dat_combined_u <-
    readRDS(here::here(
      push_mods,  "dat_combined_u-muslims-attack"
    ))
  
  
  
  
  test1 <- dat_combined_u |>
    select(wave, as, yimpute_muslim, Pol.Orient_cZ,   id)
  
  
  shown <- test1 |>
    pivot_wider(
      names_from = wave,
      values_from = c(yimpute_muslim),
      names_glue = "{.value}_{wave}"
    )
  
  
  nbind <- rbind(yimpute_muslim_lag, yimpute_muslim_lag)
  
  shown_df <- cbind(nbind, shown)
  
  shown_df <- shown_df |>
    mutate(
      yimpute_muslim_lag_c = scale(yimpute_muslim_lag, center = TRUE, scale = FALSE)
    )
  
  head(shown_df)
  tail(shown_df)
  
  summary(glm(
    yimpute_muslim_0 ~ as  +  yimpute_muslim_lag, data = shown_df
  ))
  
  
  # p <- predictions(fit, newdata = datagrid(qsmk = 0:1, grid_type = "counterfactual"))
  # aggregate(predicted ~ qsmk, data = p, FUN = mean)
  
  
  
  # overweight --------------------------------------------------------------
  #  Not run
  # prior_ow = c(
  #   set_prior("normal(0,.5)",  class = "b"),
  #   set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  #   set_prior(
  #     "student_t(3, 4, 2)",
  #     class = "Intercept",
  #     lb = 1,
  #     ub = 7
  #   ),
  #   set_prior("exponential(1)", class = "sd")  # only for raneffs
  # )
  #
  #
  #
  # bform_marg_ow  =   bf(yimpute_overweight |mi(se) ~  as  *  wave  + (1|id),
  #                        sigma ~ 0 + as, set_rescor(rescor = FALSE))
  #
  # prior_re  = c(
  #   set_prior("normal(0,.5)",  class = "b"),
  #   set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  #   set_prior(
  #     "student_t(3, 4, 2)",
  #     class = "Intercept",
  #     lb = 1,
  #     ub = 7
  #   ),
  #   set_prior("exponential(1)", class = "sd")  # only for raneffs
  # )
  #
  #
  # system.time(
  #   m_marg_ow <- brms::brm(
  #     backend = "cmdstanr",
  #     data = d_overweight,
  #     family = "gaussian",
  #     bform_marg_ow,
  #     prior_re,
  #     init = 0,
  #     file = here::here(push_mods,"m_marg_ow.rds")
  #   )
  # )
  #
  #
  # # Images to use
  # m_marg_ow  <- plot(
  #   conditional_effects(
  #     m_marg_ow,
  #     "wave:as",
  #     spaghetti = TRUE,
  #     ndraws =200,
  #     plot = F))[[1]]
  #
  #
  
  # comparison marginal graph -----------------------------------------------
  # plot_muslim <- muslim_gr +
  #   labs(subtitle = "Muslim Warmth",
  #        y = "Muslim Warmth (1-7) ",
  #        x = "Years: 2018-2022; N = XXX") +
  #   scale_colour_okabe_ito(alpha =1) +
  #   theme_classic()   +
  #   scale_y_continuous(limits = c(4.0, 6))
  #
  #
  # plot_overweight <- ov_gr +
  #   labs(subtitle = "Overweight Warmth",
  #        y = "Overweight Warmth (1-7) ",
  #        x = "Years: 2018-2020/21; N = 19814") +
  #   #scale_colour_viridis_d(alpha =.4, name = "attack condition") +
  #   scale_colour_okabe_ito(alpha =.4) +
  #   # scale_colour_viridis_d(alpha =.4) +
  #   theme_classic()   +
  #   scale_y_continuous(limits = c(4.0, 6))
  #
  # plot_overweight
  #
  # combined_plot <- plot_muslim + plot_overweight + plot_annotation(tag_levels = "A",
  #                                                                  title = "Comparison of Warm trajectories: (A) Muslims; (B) Overweight") + plot_layout(guides = 'collect')
  #
  # combined_plot
  # dev.off()
  #
  # plot_muslim + plot_overweight
  