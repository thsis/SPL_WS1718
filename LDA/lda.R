# Linear Discriminant Analysis
gradientDescentOptimizer = function(expr, learn_rate, step = 10e-3,
                                    precision = 10e-3, max_iter = 10e6){
  print("--------------------Initialized-----------------")
  i = 0
  sexpr = substitute(expr)
  start = runif(1, -100, 100)
  x_list = list(x = c(start, start + step))
  gradient = diff(eval(sexpr, envir = x_list))
  
  print(x_list)
  print(eval(sexpr, envir = x_list))
  print(gradient)
  
  while(i < max_iter & abs(gradient) > precision){
    i = i + 1
    x_0 = x_list[['x']][2]
    x_1 = x_0 - sign(gradient) * learn_rate
    x_list[["x"]] = c(x_0, x_1)
    gradient = diff(eval(sexpr, envir = x_list))
    
    print(paste("i:", i))
    print(paste0("x_", 0:(length(x_list[['x']])-1), ": ", x_list[["x"]]))
    print(paste("f(x):", eval(sexpr, envir = x_list)))
    print(paste("df(x)/dx:", gradient))
  }
}

gradientDescentOptimizer(x^2 + 2, 0.3)
