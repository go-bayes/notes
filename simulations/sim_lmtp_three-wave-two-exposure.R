# lmpt simulation
# 7 April 2024
# between two causal models for three wave study
library(lmtp)
library(margot)

#devtools::install_github("go-bayes/margot")
library(future)
library(SuperLearner)
library(xgboost)
library(ranger)

# set file to save
push_mods <-   fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/notes/sims"
  )

# multi-cores
plan(multisession)

# 10 cores on my machine
n_cores <- parallel::detectCores() - 2

# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger", #
            "SL.xgboost") #

# set seed for reproducability
set.seed(123)

# set N
n =1000
# baseline confounders
W0 <- rnorm(n, 1)

# time-vary confounder at baseline
L0 <- rnorm(n, 1)

# A
A0 <- rnorm(n, L0 + W0)

# A causes Y here
Y0 <- rnorm(n, L0 * .1 + (A0 *.5)  + W0 * .1 )

# time-vary-confounder  not affected by A0
L1 <- rnorm(n, .5 * L0 )

# A1 
A1 = rnorm(n, A0 + (W0 * .1) + .1 * L1)

# simulate out come with 
Y2 = rnorm (n, Y0 *.5 + (A1 * .2) + (L0 * .05)  + (A0 * .1)  + L1 *.01 + W0 * .1)

# bind data
dat <- cbind.data.frame(W0,L0,A0,Y0,L1,A1,Y2)

# view
skimr::skim( dat) 

head(dat)


# try ordinary regressions
mod_regression <- lm( Y2 ~ A1 + L1 + A0 + Y0 + W0 + L0, data = dat)
parameters::model_parameters(mod_regression)

mod_regression_no_y  <- lm( Y2 ~ A1 + A0 + W0 + L0, data = dat)
parameters::model_parameters(mod_regression_no_y)


mod_regression_no_x  <- lm( Y2 ~ A1 + Y0 + W0 + L0 + L1, data = dat)
parameters::model_parameters(mod_regression_no_x)

mod_regression_no_x_y <- lm( Y2 ~ A1 + W0 + L0 + L1,data = dat)
parameters::model_parameters(mod_regression_no_x_y)

mod_regression_only_A <- lm( Y2 ~ A0 )
parameters::model_parameters(mod_regression_only_A)

mod_regression_only_A <- lm( Y2 ~ A1 )
parameters::model_parameters(mod_regression_only_A)


# functions
gain_A <- function(data, trt) {
  ifelse(data[[trt]] != 1, 1, data[[trt]])
}
# 
zero_A <- function(data, trt){
  ifelse(data[[trt]] != 0, 0, data[[trt]])
}


L_alpha <- list(NULL, c("L1"))
A <- c("A0", "A1")
head(dat)



# wrong because y is effect a
model_y0_mediator_gain <- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0","L0","Y0"),
  shift = gain_A,
  data = dat,
  trt = A,
  time_vary = L_alpha,
 #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
#  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_gain

here_save(model_y0_mediator_gain, "model_y0_mediator_gain")


model_y0_mediator_gain_2 <- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0","L0","Y0"),
  shift = gain_A,
  data = dat,
  trt = A,
  time_vary = L_alpha,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_gain

lmtp_contrast(model_y0_mediator_gain, ref = model_y0_mediator_gain_2, type = "additive")

here_save(model_y0_mediator_gain, "model_y0_mediator_gain")

model_y0_mediator_zero <- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0","L0","Y0"),
  shift = zero_A,
  data = dat,
  trt = A,
  time_vary = L_alpha,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt="SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_zero

here_save(model_y0_mediator_zero, "model_y0_mediator_zero")

# false
lmtp_contrast(model_y0_mediator_gain, ref = model_y0_mediator_zero, type = "additive")





# 
# 
# model_y0_mediator_NULL <- lmtp_sdr(
#   outcome = "Y2",
#   baseline = "W0",
#   shift = NULL,
#   data = dat,
#   trt = A,
#   time_vary = L_alpha,
#   #cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   #  weights = df_clean_slice$t0_sample_weights,
#   learners_trt= "SL.glmnet",
#   # ranger much faster
#   learners_outcome=  "SL.glmnet",
#   parallel = n_cores
# )
# 
# model_y0_mediator_NULL
# 
# here_save(model_y0_mediator_NULL, "model_y0_mediator_NULL")
# 
# mean(dat$Y2)

# corrred-specified
L_beta <- list("L0", c("L1","Y0"))

model_y0_confounder_gain <- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0"),
  shift = gain_A,
  data = dat,
  trt = A,
  time_vary = L_beta,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_gain
here_save(model_y0_confounder_gain, "model_y0_confounder_gain")

model_y0_confounder_zero <- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0"),
  shift = zero_A,
  data = dat,
  trt = A,
  time_vary = L_beta,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
head(dat)
model_y0_confounder_zero
here_save(model_y0_confounder_zero, "model_y0_confounder_zero")

lmtp_contrast(model_y0_confounder_gain, ref = model_y0_confounder_zero, type = "additive")


# control -----------------------------------------------------------------

model_y0_confounder_gain_control <- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0","L0", "L1", "Y0"),
  shift = gain_A,
  data = dat,
  trt = "A1",
 # time_vary = L_beta,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_gain_control
here_save(model_y0_confounder_gain_control, "model_y0_confounder_gain_control")

model_y0_confounder_zero_control<- lmtp_sdr(
  outcome = "Y2",
  baseline = c("W0","L0", "L1", "Y0"),
  shift = zero_A,
  data = dat,
  trt = "A1",
  # time_vary = L_beta,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_zero_control
here_save(model_y0_confounder_zero_control, "model_y0_confounder_zero_control")

lmtp_contrast(model_y0_confounder_gain, ref = model_y0_confounder_zero, type = "additive")




lmtp_contrast(model_y0_confounder_gain_control, ref = model_y0_confounder_zero_control, type = "additive")




# models with different order ----------------------------------------------------------
# set N
n = 5000
# baseline confounders
W0_b <- rnorm(n, 1)

# time-vary confounder at baseline
L0_b  <- rnorm(n, 1)

# Y causes A here
Y0_b  <- rnorm(n, L0_b + W0_b  )


# Y causes A
A0_b  <- rnorm(n, L0_b * .2 + W0_b * .2 + Y0_b * .5 )



# time-vary-confounder  affected by A0
L1_b  <- rnorm(n, .5 * L0_b )

# A1 
A1_b  = rnorm(n, A0_b  * .9  + (W0_b) + .2 * L1_b + Y0_b * .1)

# simulate out come with 
Y2_b = rnorm (n, Y0_b  *.8 + (A1_b  * .2) + (L0_b  * .01)  + (A0_b  * 0)  + L1_b  *.05 )

# bind data
dat_b  <- cbind.data.frame(W0_b ,L0_b,L1_b ,A0_b ,Y0_b ,A1_b ,Y2_b )

# view
skimr::skim( dat_b ) 


# try ordinary regressions
mod_regression_b  <- lm( Y2_b  ~ A1_b  + L1_b  + A0_b  + Y0_b  + W0_b  + L0_b )
parameters::model_parameters(mod_regression_b )

mod_regression_no_y_b   <- lm( Y2_b  ~ A1_b  + A0_b  + W0_b  + L0_b )
parameters::model_parameters(mod_regression_no_y_b )


mod_regression_no_x_b   <- lm( Y2_b  ~ A1_b  + Y0_b  + W0_b  + L0_b  + L1_b )
parameters::model_parameters(mod_regression_no_x_b )

mod_regression_no_x_y_b  <- lm( Y2_b  ~ A1_b  + W0_b + L0_b  + L1_b  )
parameters::model_parameters(mod_regression_no_x_y_b )

mod_regression_only_A_b  <- lm( Y2_b  ~ A1_b  )
parameters::model_parameters(mod_regression_only_A_b )

# functions -correct
L_alpha_b <- list(c("Y0_b", "L0_b"), c("L1_b"))

# incorrect
L_beta_b <- list(c("L0_b"), c("L1_b","Y0_b"))


A_b  <- c("A0_b", "A1_b")
head(dat_b)


# specified correctly
#Y -> A
model_y0_mediator_gain_b  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = gain_A,
  data = dat_b,
  trt = A_b ,
  time_vary = L_alpha_b,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_gain_b

here_save(model_y0_mediator_gain_b , "model_y0_mediator_gain_b")


model_y0_mediator_zero_b  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = zero_A,
  data = dat_b,
  trt = A_b,
  time_vary = L_alpha_b,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_zero_b 

here_save(model_y0_mediator_zero_b, "model_y0_mediator_zero_b")

# truth Y -> A but controlled
lmtp_contrast(model_y0_mediator_gain_b, ref = model_y0_mediator_zero_b, type = "additive")


# incorrred-specified
L_beta_b <- list(c("L0_b"), c("L1_b","Y0_b"))

model_y0_confounder_gain_b  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = gain_A,
  data = dat_b,
  trt = A_b,
  time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome= "SL.ranger",
  parallel = n_cores
)
# model_y0_confounder_gain_b 
here_save(model_y0_confounder_gain_b , "model_y0_confounder_gain_b ")

#
model_y0_confounder_zero_b  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = zero_A,
  data = dat_b,
  trt = A_b,
  time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_zero_b 
here_save(model_y0_confounder_zero_b , "model_y0_confounder_zero_b ")

# false Y -> A but not controlled
lmtp_contrast(model_y0_confounder_gain_b, ref = model_y0_confounder_zero_b, type = "additive")



## Just control
model_y0_confounder_gain_b_control  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = gain_A,
  data = dat_b,
  trt = "A1_b",
 # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_gain_b_control 
here_save(model_y0_confounder_gain_b_control , "model_y0_confounder_gain_b_control")


model_y0_confounder_zero_b_control  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = zero_A,
  data = dat_b,
  trt = "A1_b",
  # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome= "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_zero_b_control 
here_save(model_y0_confounder_zero_b_control , "model_y0_confounder_zero_b_control")

# nails it. Y -> A but we control and assess only A1
lmtp_contrast(model_y0_confounder_gain_b_control, ref = model_y0_confounder_zero_b_control, type = "additive")




## Just control
model_y0_confounder_gain_b_control  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = gain_A,
  data = dat_b,
  trt = "A1_b",
 # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_gain_b_control 
here_save(model_y0_confounder_gain_b_control , "model_y0_confounder_gain_b_control")


model_y0_confounder_zero_b_control  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = zero_A,
  data = dat_b,
  trt = "A1_b",
  # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome= "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_zero_b_control 
here_save(model_y0_confounder_zero_b_control , "model_y0_confounder_zero_b_control")

# nails it. Y -> A but we control and assess only A1
lmtp_contrast(model_y0_confounder_gain_b_control, ref = model_y0_confounder_zero_b_control, type = "additive")





# another coeff A ---------------------------------------------------------


# models with different order ----------------------------------------------------------
# set N
rm(dat_b)
n = 5000
# baseline confounders
W0_b <- rnorm(n, 1)

# time-vary confounder at baseline
L0_b  <- rnorm(n, 1)

# Y causes A here
Y0_b  <- rnorm(n, L0_b + W0_b  )


# Y causes A
A0_b  <- rnorm(n, L0_b * .2 + W0_b * .2 + Y0_b * .5 )



# time-vary-confounder  affected by A0
L1_b  <- rnorm(n, .5 * L0_b )

# A1 
A1_b  = rnorm(n, A0_b  * .9  + (W0_b) + .2 * L1_b + Y0_b * .1)

# simulate (A strong effect on Y2)
Y2_b = rnorm (n, Y0_b  *.8 + (A1_b  * .1) + (L0_b  * .01)  + (A0_b * .5)  + L1_b  *.05  + W0_b *.8)

# bind data
dat_b  <- cbind.data.frame(W0_b ,L0_b,L1_b ,A0_b ,Y0_b ,A1_b ,Y2_b )

# view
skimr::skim( dat_b ) 


# try ordinary regressions
mod_regression_b  <- lm( Y2_b  ~ A1_b  + L1_b  + A0_b  + Y0_b  + W0_b  + L0_b )
parameters::model_parameters(mod_regression_b )

mod_regression_no_y_b   <- lm( Y2_b  ~ A1_b  + A0_b  + W0_b  + L0_b )
parameters::model_parameters(mod_regression_no_y_b )


mod_regression_no_x_b   <- lm( Y2_b  ~ A1_b  + Y0_b  + W0_b  + L0_b  + L1_b )
parameters::model_parameters(mod_regression_no_x_b )

mod_regression_no_x_y_b  <- lm( Y2_b  ~ A1_b  + W0_b + L0_b  + L1_b  )
parameters::model_parameters(mod_regression_no_x_y_b )

mod_regression_only_A_b  <- lm( Y2_b  ~ A1_b  )
parameters::model_parameters(mod_regression_only_A_b )

mod_regression_only_A_b  <- lm( Y2_b  ~ A0_b  +  W0_b + L0_b   + Y0_b )
parameters::model_parameters(mod_regression_only_A_b )

# functions -correct
L_alpha_b <- list(c("Y0_b", "L0_b"), c("L1_b"))

# incorrect
L_beta_b <- list(c("L0_b"), c("L1_b","Y0_b"))


A_b  <- c("A0_b", "A1_b")
head(dat_b)


# specified correctly
#Y -> A
model_y0_mediator_gain_bb  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = gain_A,
  data = dat_b,
  trt = A_b ,
  time_vary = L_alpha_b,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_gain_bb <- model_y0_mediator_gain_b

here_save(model_y0_mediator_gain_bb , "model_y0_mediator_gain_bb")


model_y0_mediator_zero_bb  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = zero_A,
  data = dat_b,
  trt = A_b,
  time_vary = L_alpha_b,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)

model_y0_mediator_zero_bb 

here_save(model_y0_mediator_zero_bb, "model_y0_mediator_zero_bb")

# truth Y -> A but controlled
lmtp_contrast(model_y0_mediator_gain_bb, ref = model_y0_mediator_zero_bb, type = "additive")


# incorrred-specified
L_beta_b <- list(c("L0_b"), c("L1_b","Y0_b"))

model_y0_confounder_gain_bb  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = gain_A,
  data = dat_b,
  trt = A_b,
  time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome= "SL.ranger",
  parallel = n_cores
)
# model_y0_confounder_gain_b 
here_save(model_y0_confounder_gain_bb , "model_y0_confounder_gain_bb")

#
model_y0_confounder_zero_bb  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = "W0_b",
  shift = zero_A,
  data = dat_b,
  trt = A_b,
  time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
here_save(model_y0_confounder_zero_bb , "model_y0_confounder_zero_bb")

# false Y -> A but not controlled
lmtp_contrast(model_y0_confounder_gain_bb, ref = model_y0_confounder_zero_bb, type = "additive")



## Just control
model_y0_confounder_gain_b_controlb  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = gain_A,
  data = dat_b,
  trt = "A1_b",
  # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome=  "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_gain_b_controlb 
here_save(model_y0_confounder_gain_b_controlb, "model_y0_confounder_gain_b_controlb")


model_y0_confounder_zero_b_controlb  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = zero_A,
  data = dat_b,
  trt = "A1_b",
  # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome= "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_zero_b_controlb 
here_save(model_y0_confounder_zero_b_controlb , "model_y0_confounder_zero_b_controlb")

# nails it. Y -> A but we control and assess only A1
lmtp_contrast(model_y0_confounder_gain_b_controlb, ref = model_y0_confounder_zero_b_controlb, type = "additive")





## Just control
model_y0_confounder_gain_b_controlb_sub  <- lmtp_sub(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = gain_A,
  data = dat_b,
  trt = "A1_b",
 # time_vary = L_beta_b,
  #cens = C,
 # mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners= "SL.ranger"#,
  # ranger much faster
  #parallel = n_cores
)
model_y0_confounder_gain_b_controlb_sub 
here_save(model_y0_confounder_gain_b_controlb_sub, "model_y0_confounder_gain_b_controlb_sub")


model_y0_confounder_zero_b_controlb_sub  <- lmtp_sdr(
  outcome = "Y2_b",
  baseline = c("W0_b","L0_b", "A0_b", "L1_b", "Y0_b"),
  shift = zero_A,
  data = dat_b,
  trt = "A1_b",
  # time_vary = L_beta_b ,
  #cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  #  weights = df_clean_slice$t0_sample_weights,
  learners_trt= "SL.ranger",
  # ranger much faster
  learners_outcome= "SL.ranger",
  parallel = n_cores
)
model_y0_confounder_zero_b_controlb_sub 
here_save(model_y0_confounder_zero_b_controlb_sub , "model_y0_confounder_zero_b_controlb_sub")

# nails it. Y -> A but we control and assess only A1
lmtp_contrast(model_y0_confounder_gain_b_controlb_sub, ref = model_y0_confounder_zero_b_controlb_sub, type = "additive")

