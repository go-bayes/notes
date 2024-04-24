
# simulate radim fertility
# joseph.bulbulia@gmail.com 23 april 2023
# for radim

set.seed(123)  # reproduce

# number of observations
n <- 10000

# simulate binary predictors for religion in 2005 and 2003
religion_2005 <- rbinom(n, 1, 0.6)  # treatment
religion_2003 <- rbinom(n, 1, 0.8)  # slightly higher probability in 2003, say

#
male <- rbinom(n, 1, 0.3)  # 30% probability for males, 70% for females


# simulate other covariates (e.g., socioeconomic status, which can be approximated by income level)
income_2003_log <- log( rnorm(n, mean = 50000, sd = 10000) ) # income_log

# generate the number of children in 2013, influenced by religion & other vars

# assuming direct effects and interaction between religion variables
lambda <- exp(
  -.2 +
    0.2 * religion_2005 +
    0.01 * religion_2003 -
    0.05 * income_2003_log+-.05 * male +
    0.1 * (male * religion_2005) # gender effect only if male
)

# simulate poisson outcome
children_2013 <- rpois(n, lambda)  # Poisson-distributed number of children

# check
children_2013

# combine into a data frame
data <- data.frame(children_2013, religion_2005, religion_2003, income_2003_log, male)


# fit the poisson regression model 
fit_1 <- glm(children_2013 ~ religion_2005 + religion_2003 + income_2003_log + male + male:religion_2005,
             family = poisson(link = "log"), data = data)



# check the summary of the model to interpret coefficients
summary(fit_1)


# conditional rate ratios from the coefficients
rate_ratios <- exp(coef(fit_1))

# this gives us *conditional* rate ratios 
rate_ratios 


# to get standard errors and compute *marginal* effects
# use this package
library(clarify)

# use clarify to simulate and calculate the ATE
sim_coefs <- sim(fit_1,  n = 1000, vcov = "HC3")


# simulate outcome
sim_outcome<- sim_ame(
  sim_coefs,
  var = "religion_2005",
  cl = 3,
  verbose = TRUE
)

# view
sim_outcome

# compute rate ratio
RR_ate <-transform(sim_outcome, 
                   RR= `E[Y(1)]` / `E[Y(0)]`)

# result
RR_ate

# compute risk difference 
ATE_rr_rd <-transform(sim_outcome, 
                      RR= `E[Y(1)]` / `E[Y(0)]`,
                      RD = `E[Y(1)]` - `E[Y(0)]`)
# result
ATE_rr_rd

# obtain confidence intervals -- on rate ratio and rate difference scale

# result
summary(ATE_rr_rd, null = c(RR = 1,RD = 0))



## EFFECT MODIFICATION BY MALE

sim_outcome_male_contrast <- sim_ame(
  sim_coefs,
  var = "religion_2005",
  by = "male",
  cl = 3,
  verbose = TRUE
)


# view
summary(sim_outcome_male_contrast)

# contrast males who are religious (say church) with females
CATE_rr_rd <-transform(sim_outcome_male_contrast, 
                       RR= `E[Y(1)|1]` / `E[Y(1)|0]`, # men who are religous, women who religious
                       RD = `E[Y(1)|0]` - `E[Y(0)|0]`)# men who are religous, women who religious


# result
summary(CATE_rr_rd,null = c(RR = 1,RD = 0))



# notice that contrasts are sensitive to scale -- best to use RD scale
