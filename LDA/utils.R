objective = function(x){
  return(0.008*x^4 + 2)
}

univariateMinimizer = function(obj, learn_rate = 1, epsilon = 1e-3,
                               max_iter = 100, precision = 1e-6, verbose = FALSE,
                               report_freq = 100){
  
  # Approximate gradient by taking the difference of an epsilon-step
  d = function(x, fun = obj, e = epsilon){
    gradient = (fun(x + e) - fun(x)) / e
    # Sometimes the approximate gradient turns out to be too steep.
    # Thus we bound it which stabilizes the algorithm by introducing
    # some inefficiency.
    gradient = min(abs(gradient), 10) * sign(gradient) # Correct the sign
    return(gradient)
  }
  curve(obj(x), from = -10, to = 10, main = "Objective Function", asp = 1)
  
  # Draw multiple random starting points.
  # Take the combination that provides the lowest value of the objective function.
  a = runif(1000, min = -1000, max = 1000)
  a = a[which.min(sapply(a, obj))]
  i = 0
  
  gradient = d(a)
  if(verbose){
    print("Starting Point:")
    print(paste("a:", a, "f(a):", obj(a), "df(a):", d(a)))
  }
  
  while (i < max_iter & abs(gradient) > precision) {
    i = i + 1
    
    gradient = d(a)
    a = a - learn_rate * gradient
    
    if(i %% report_freq == 0 & verbose) {
      cat("Iteration:", i, "x:", a, "f(x):", obj(a), "df(x):", d(a),
          sep = c("\t", "\n"))
      points(a, obj(a), col = "red")}
  }
  cat("\nFinal", "Results:\n","Iteration:", i, "x:", a, "f(x):", obj(a), "df(x):", d(a),
      sep = c("\t", "\n"))
  
  points(a, obj(a), col = "blue", pch = 4, cex = 2)
  if(i >= max_iter){
    warning("Maximum number of iterations reached.")
  }
}

univariateMinimizer(obj = objective, max_iter = 1000000, precision = 1e-9, learn_rate = 0.75, verbose = F)

