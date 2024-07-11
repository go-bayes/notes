causal_contrast_marginal <- function(df, Y, X, baseline_vars = "1", treat_0 = treat_0,
                                     treat_1 = treat_1, estimand = c("ATE", "ATT"), type = c("RR", "RD"),
                                     nsims = 200, cores = parallel::detectCores(), family = "gaussian",
                                     weights = NULL, continuous_X = FALSE, splines = FALSE, vcov = "HC2",
                                     verbose = FALSE) {
  
  # Validate family
  if (is.character(family)) {
    if (!family %in% c("gaussian", "binomial", "Gamma", "inverse.gaussian", "poisson", "quasibinomial", "quasipoisson", "quasi")) {
      stop("Invalid 'family' argument. Please specify a valid family function.")
    }
    family_fun <- get(family, mode = "function", envir = parent.frame())
  } else if (inherits(family, "family")) {
    family_fun <- family
  } else {
    stop("Invalid 'family' argument. Please specify a valid family function or character string.")
  }
  
  # Build formula
  build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
    if (baseline_vars == "1") {
      if (continuous_X && splines) {
        return(paste(Y, "~ bs(", X, ")"))
      } else {
        return(paste(Y, "~", X))
      }
    } else {
      if (continuous_X && splines) {
        return(paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")"))
      } else {
        return(paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")"))
      }
    }
  }
  
  # Apply model
  weight_var <- if (!is.null(weights) && weights %in% names(df)) df[[weights]] else NULL
  formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
  fit <- glm(as.formula(formula_str), weights = weight_var, family = family_fun, data = df)
  sim_imp <- sim(fit, n = nsims, vcov = vcov)
  
  # Output processing and return
  sim_estimand <- sim_ame(sim_imp, var = X, cl = cores, verbose = verbose)
  summary(sim_estimand)
}