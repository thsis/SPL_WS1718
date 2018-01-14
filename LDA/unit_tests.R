source("utils.R")

# Test univariate Minimizer:
objective = function(x){
  return(0.008*x^4 + 2)
}
univariateMinimizer(obj = objective,
                    max_iter = 1000000,
                    precision = 1e-9,
                    learn_rate = 0.75,
                    verbose = FALSE)


# Test bivariate Minimizer:

# Bivariate Case:
bivariate_objective = function(A){
  fun = function(x) return(t(x) %*% A %*% x)
  return(fun)
}

# Test
A_2d_test = matrix(data=c(1, 0, 0, 4), nrow = 2, ncol = 2)
F_2d_test = bivariate_objective(A_2d_test)
F_2d_test(c(1,1))

x = seq(-300, 300, length.out = 100)
y = seq(-300, 300, length.out = 100)

# FUN is equivalent to the form above.
Z = outer(x, y, FUN = function(x, y) x^2  + 4*y^2)
persp(x, y, Z)

# Approximation of Bivariate Gradient
get_2D_gradient = function(x, objective, epsilon){
  init = matrix(data = x, nrow = 2, ncol=2, byrow = TRUE)
  steps = init + diag(x = epsilon, ncol = 2, nrow = 2)
  
  f_steps = apply(steps, 2, objective)
  f_comp =  apply(init, 2, objective)
  
  return((f_steps - f_comp) / epsilon)
}

# Bivariate Gradient Descent:
bivariateMinimizer = function(obj,
                              epsilon_step = 0.001,
                              max_iter = 10,
                              precision = 10^-6,
                              learn = 0.5){
  # Approximate Gradient
  get_2D_gradient = function(x, objective = obj, epsilon = epsilon_step){
    init = matrix(data = x, nrow = 2, ncol=2, byrow = TRUE)
    steps = init + diag(x = epsilon, ncol = 2, nrow = 2)
    
    f_steps = apply(steps, 2, objective)
    f_comp =  apply(init, 2, objective)
    
    D = (f_steps - f_comp) / epsilon
    # Trim D to limit the gradient:
    D_trimmed = ifelse(abs(D) <= 5, abs(D), 5) * sign(D)
    return(D_trimmed)
  }
  # Draw multiple random starting points.
  # Take the combination that provides the lowest value of the objective function.
  a = matrix(data = runif(10000, min = -100, max = 100),
             ncol = 2)
  # DEBUG:
  f_a = apply(a, 1, obj)
  a = a[which.min(f_a), ]
  gradient = get_2D_gradient(a)
  
  i = 0
  
  learn_rates = seq(from = learn, to = 0, length.out = max_iter + 1)
  
  while(i <= max_iter & any(abs(gradient) >= precision)){
    # DEBUG
    cat("\nStep:\t\t", i, "\nx:\t\t", a, "\ngradient:\t", gradient, "\nlearn:\t", learn_rates[i],  "\n---------")
    
    i = i + 1
    a = a - learn_rates[i] * gradient
    gradient = get_2D_gradient(a)
  }
  
  # DEBUG:
  print(a)
  print(gradient)
  print(i)
}

bivariateMinimizer(obj = F_2d_test, max_iter = 100)
