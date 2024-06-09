## Measurement error

# does mismeasured confounder lead to bias under the null. 

# reproducability 
set.seed(123)
n = 1000
L = rnorm(n)
L_p = rnorm(n, L)
A = rnorm(n, L)
Y = rnorm(n, L)


# fit

parameters::model_parameters( lm(Y ~ A + L) )
parameters::model_parameters( lm(Y ~ A + L_p ) )

