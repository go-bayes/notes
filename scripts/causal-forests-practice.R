# Causal Random Forests
Dear grf Package Developers,

I am using the grf package for a causal forest analysis involving longitudinal data and facing the challenge of missing Y values due to panel attrition.
I write for your advice on handling these missing outcomes within your package.
Note a further complication is that we have survey weights for the sample, which we hope to use when generalising.
Below is a simplified example highlighting my issue, and proposed workaround.  My concerns are:(1) censoring model misspecification (can we do better ?
)
(2) invalidating inference by multiplying the censoring and sampling weights.

Thank you for your assistance and for developing this really great software !
  
  Joe


# 3-wavel panel design,
# gist: Y_time2 ~ tau * W1_time1 + beta * X_time0 (see:  https://doi.org/10.1214/19-STS728 )
# problem: loss-to-follow-up leads to missing responses in Y_time2

# load grf
library(grf)
# simulate data for the three-wave panel
set.seed(123)
n <- 1000 # n of observations
p <- 5   # n of covariates

# wave 0 (baseline) data
X0 <- matrix(rnorm(n * p), n, p) # Covariates
W0 <- rbinom(n, 1, 0.5)          # Treatment indicator at baseline
Y0 <- rnorm(n)                   # Outcome at baseline

# wave 1 data (post-baseline) exposure
# assume W is the treatment and is measured again without missing values
W1 <- rbinom(n, 1, 0.5) # Treatment indicator at baseline + 1

# construct the combined covariates matrix (X)
# here, we assume X, W, and Y observed at baseline define X for simplicity
X <- cbind(X0, W0, Y0)

# wave 2 data (baseline + 2), the outcome wave
# sim attrition leading to missing Y values
# we arbitrarily assume 20% attrition
Y2 <- rnorm(n)                     # outcome at baseline + 2
missing_indices <- sample(1:n, n * 0.2) # indices of missing data
Y2[missing_indices] <-
  NA           # NA values to simulate missing data

# fit the causal forest model
# as expected, we return an error because Y2 cannot contain missing values.
cf <- grf::causal_forest(X, Y2, W1)

# possible work around to verify...

#  1:  fill out the example by simulating survey weights
survey_weights <- runif(nrow(X), min = 0.5, max = 2)

#  2: create binary indicator for missing Y2 values
Y2_missing <- as.integer(is.na(Y2))

#  3: fit a censoring model: ** this seems like a hack, would like ML/superlearner here **
censoring_model <- glm(
  Y2_missing ~ ., data = as.data.frame(cbind(X, W1)), family = "binomial"
  )

#  4: compute inverse probability of censoring weights
uncensored_probs <- predict(
  censoring_model, type = "response", newdata = data.frame(X, W1)
  )
censoring_weights <- 1 / uncensored_probs

#  5: consider trimming extreme weights (depending on context/question etc.)
weights_cap <- quantile(censoring_weights, 0.99)
censoring_weights_capped  <- pmin(censoring_weights, weights_cap)

#  6: combine censoring weights with survey weights (use "censoring_weights" if preferable)
combined_weights <- censoring_weights_capped * survey_weights

#  7: fit the causal forest model with combined survey & censoring weights. 
# concern: are sample.weights working as we hope? 

cf_model <- causal_forest(
  X = X[!is.na(Y2), ],
  Y = Y2[!is.na(Y2)],
  W = W1[!is.na(Y2)],
  sample.weights = combined_weights[!is.na(Y2)]
)

version(pkg = "grf")
#[1] "grf 2.3.1"

