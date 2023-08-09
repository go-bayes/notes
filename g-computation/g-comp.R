## g-computation approach
## 25 Sept 2022
# recover 1 and 2 year ATE using g-compution


# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# read data
imps_bind<- readRDS(here::here("mods", "imps_bind"))
head(imps_bind)

# Data for testing model priors
# make list
imp1 <- as.data.frame(imps_bind$imputations$imp[[1]])
imp2 <- as.data.frame(imps_bind$imputations$imp[[2]])
imp3 <- as.data.frame(imps_bind$imputations$imp[[3]])
imp4 <- as.data.frame(imps_bind$imputations$imp[[4]])
imp5 <- as.data.frame(imps_bind$imputations$imp[[5]])
imp6 <- as.data.frame(imps_bind$imputations$imp[[6]])
imp7 <- as.data.frame(imps_bind$imputations$imp[[7]])
imp8 <- as.data.frame(imps_bind$imputations$imp[[8]])
imp9 <- as.data.frame(imps_bind$imputations$imp[[9]])
imp10 <- as.data.frame(imps_bind$imputations$imp[[10]])


ameliadata <-
  list(imp1, imp2, imp3, imp4, imp5, imp6, imp7, imp8, imp9, imp10)

library(miceadds)
#imputed_mlist <- list(imputed_m$imputations)
acmice <- miceadds::datlist2mids( ameliadata )
# #head(amice$loggedEvents, 10)
# a_mice<- mice::complete(acmice, action = 'long', include = TRUE)
# skim(a_mice)
# # inspect data
# skim(dt_fivewaves) |>
#   arrange(n_missing)



###############  RENAME YOUR IMPUTED DATASET  'df"  ###############  ###############  ###############
###############   IMPORANT DO THIS   ###############  ###############  ###############  ###############

df <- acmice


############### SET YOUR EXPOSURE VARIABLE, ###############  ###############  ###############

## HERE WE USE THE EXAMPLE OF HOURS WORK / 10
###############   IMPORTANT SET YOUR EXPOSURE VARIABLE



X = "Wave"


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

cvars = c("As", "As:Wave")


# SET THE RANGE OF religious service FROM ZERO TO 80
min = 0
max = 20


# set full range of X
x =  min:max
x

# range for some graphs
minmax <- paste(c(x), sep = ",")


# baseline condition here is 20 hours of work.  We could make it different
r = 0

# focal contrast for X  Someone who goes from 20 to 60 hours of work.
f = 18  # when the curves converge

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = not used
delta = abs(r - f)

ylim = c(4, 6)  

# mice imputed data
## THIS IS KEY, NAME THE DATA I GAVE YOU "DF"

# n imputations
m = 10

#sd = 1.612229


  
# gcomp   ------------------------------------------------------------------
Y = "Ys"
main = "Counterfactual 20-year Muslim Warmth trajectory"
#ylab = "Warmth Muslims"
sub = "Warmth Muslims"
xlab = "years"


# regression
# mice_generalised_lin = function(df, X, Y, cvars, family) {
#   require("splines")
#   require("mice")
#   out <- with(df, glm(as.formula(paste(
#     paste(Y,"~", X,"+"),
#     paste(cvars,collapse = "+")
#   )), family = family))
#   out
# }
conflict_prefer("cbind", "base")


out_ct <- mice_generalised_lin(df = df,
                              X = X,
                              Y = Y,
                              family = "gaussian",
                              cvars = cvars)

summary(pool(out_ct))

# where do models cross --------------------------------------------------------------


pool_stglm_a1 <- function(models, df, m, x, X) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = models$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x, 
        subset =  As==1
      )
    est.all[, i] <- g.comp$est
    var.all[, i] <- diag(g.comp$vcov)
  }
  #estimate
  est <- rowMeans(est.all)
  
  #within-variance
  W <- rowMeans(var.all)
  
  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)
  
  #total variance
  var <- W + (1 + 1 / m) * B
  
  #total standard error
  se <- sqrt(var)
  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)
  # lower interval
  ui <- est + (1.96 * se)
  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}



pool_stglm_a0 <- function(models, df, m, x, X) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = models$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x, 
        subsetnew = As == 0
      )
    est.all[, i] <- g.comp$est
    var.all[, i] <- diag(g.comp$vcov)
  }
  #estimate
  est <- rowMeans(est.all)
  
  #within-variance
  W <- rowMeans(var.all)
  
  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)
  
  #total variance
  var <- W + (1 + 1 / m) * B
  
  #total standard error
  se <- sqrt(var)
  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)
  # lower interval
  ui <- est + (1.96 * se)
  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}


pool_stglm_a1 <- function(models, df, m, x, X) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = models$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x, 
        subsetnew = As == 1
      )
    est.all[, i] <- g.comp$est
    var.all[, i] <- diag(g.comp$vcov)
  }
  #estimate
  est <- rowMeans(est.all)
  
  #within-variance
  W <- rowMeans(var.all)
  
  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)
  
  #total variance
  var <- W + (1 + 1 / m) * B
  
  #total standard error
  se <- sqrt(var)
  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)
  # lower interval
  ui <- est + (1.96 * se)
  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}



m0 <- pool_stglm_a0(out_ct, df = df,  m = m, x = x, X = X)


m1 <- pool_stglm_a1(out_ct, df = df,  m = m, x = x, X = X)

m1
m0
p0 <- ggplot_stglm(m0, ylim = c(4,6), main, xlab = "years", ylab = "Warmth to Muslims counterfactual no attack", min, p, sub)
p1 <- ggplot_stglm(m1, ylim = c(4,6), main, xlab = "years", ylab = "Warmth to Muslims counterfactual attack", min, p, sub)

fig_trend<- p0 + p1 + plot_annotation(title = "Comparison of long term counterfactual outcomes",
                                      subtitle = "It would take 18 years (or until 2037) for the pre-attack counterfactual Muslim Warmth trajectory to catch the post-attack counterfactual trajectory",
                                      tag_levels = "A")

fig_trend

ggsave(
  fig_trend,
  path = here::here("figs"),
  width = 16,
  height = 9,
  units = "in",
  filename = "fig_trend.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)





# coef + estimate for the contrast of interest # We  will combine the coeffients
#  into a large table, later.
accept_c <-  vanderweelevalue_ols(out_w2, f - min, delta, sd)
accept_c

out_a
## table for all contrasts (exploratory )
accept_t <- out_w2 %>%
  # #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)

accept_t

# graph
accept_p <-
  ggplot_stglm(
    out_w2,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


accept_p



## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r,
    subset = subset
  )
out_ct

# coef + estimate for the contrast of interest # We  will combine the coeffients
#  into a large table, later.
attack_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
attack_c




### EXTRA 
# g-computation using only zero data
fit <- glm(Ys ~ wave, data=km_zero5)
fit


gc0<- stdGlm(fit=fit,
       data=km_zero5,
       X = "wave",
       x = c(3:4))



plot(gc0)


## g-computation using only ones data
fit1 <- glm(Ys ~ wave, data=km_one5)

gc1<- stdGlm(fit=fit1,
             data=km_one5,
             X = "wave",
             x = c(3:4))

plot(gc1)


## both
df_five_all <- rbind(km_one5, km_zero5)
df_five_all<- df_five_all |>
  arrange(Id, Wave)



table(df_five_all_time8$As)

max(df_five_all$wave)
# model
m1 <- glm(Ys ~ As * wave, df_five_all, family = "gaussian")
summary(m1)

gc_0<- stdGlm(fit=m1,
             data=df_five_all,
             X = "wave",
             x = c(2,3,4),
             subsetnew = As==0)

gc_1<- stdGlm(fit=m1,
              data=df_five_all,
              X = "wave",
              x = c(3,4),
              subsetnew = As==1)



plot(gc_0)
plot(gc_1)



