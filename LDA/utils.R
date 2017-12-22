# Define a Gradient Descent Routine

objective = function(x){
  return((0.8*x^2 + 4*x + 2)/(x^2 + 4))
}

gradientDescentMinimizer = function(obj, n_var = 1, learn_rate = 1, epsilon = 10e-3,
                                    max_iter = 100, precision = 10e-6){
  # Approximate gradient by taking the difference of an epsilon-step
  d = function(x, fun = obj, e = epsilon){
    gradient = fun(x + e) - fun(x)
    return(gradient)
  }
  curve(obj(x), from = -10, to = 10, main = "Objective Function")
  
  # Draw multiple random starting points.
  # Take the combination that provides the lowest value of the objective function.
  a = runif(n_var * 1000, min = -1000, max = 1000)
  a = matrix(a, nrow = n_var)
  a = a[, which.min(apply(a, 2, obj))]
  i = 0
  
  gradient = d(a)
  print(a)
  while (i<=max_iter & abs(gradient) >= precision) {
    i = i + 1
    
    gradient = d(a)
    a = a - learn_rate * gradient
    
    if(i %% 10 == 0) {
      print(
        paste("Iteration:", i, "x:", a, "f(x):", obj(a)))
      points(a, obj(a), col = "red")}
  }
  print(
    paste("Iteration:", i, "x:", a, "f(x):", obj(a), "Gradient:", gradient, collapse = "\n"))
  points(a, obj(a), col = "blue", pch = 4, cex = 2)
}

gradientDescentMinimizer(obj = objective, max_iter = 10000, precision = 10e-6)
