# test using causalTree

# documentation
https://github.com/susanathey/causalTree

# install
devtools::install_github("susanathey/causalTree")

# call library
library(causalTree)

tree <- causalTree(y~ x1 + x2 + x3 + x4, data = simulation.1, treatment = simulation.1$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)

head(tree)

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

opfit <- prune(tree, opcp)

rpart.plot(opfit)


