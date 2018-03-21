source("BRM/utils.R")
set.seed(1234)

# Test univariate Minimizer:
objective = function(x){
    return(0.008*x^4 + 2)
}
curve(objective(x), xlim=c(-1, 1), main = "Minimizing a numerically challenging function")
res = univariateMinimizer(obj        = objective,
                          max_iter   = 1000000,
                          precision  = 1e-7,
                          learn_rate = 0.1,
                          verbose    = TRUE)

points(res, objective(res), col = "steelblue", pch=4, lwd=2, cex=2)
text(-0.5, y = objective(-0.75), labels = "Starting Point")
text(0, 2.0003, labels="End Point")
