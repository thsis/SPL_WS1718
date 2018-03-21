# Define Optimization Routines:
#     - univariate Case.
#     - bivariate Case.
#     - multivariate Case.

# Define Other helper-functions:
#     - get all numeric columns of a dataframe
#     - get mean vector, grouped by class label
#     - get covariance matrix, grouped by class label

# ==============================================================================
## Optimization Routines
# ==============================================================================
# Univariate Case:
univariateMinimizer = function(obj, learn_rate = 1, epsilon         = 1e-3,
                               max_iter        = 100, precision     = 1e-6,
                               verbose         = FALSE, report_freq = 100){

    # Approximate gradient by taking the difference of an epsilon-step
    d = function(x, fun = obj, e = epsilon){
        gradient = (fun(x + e) - fun(x)) / e
        # Sometimes the approximate gradient turns out to be too steep.
        # Thus we bound it which stabilizes the algorithm by introducing
        # some inefficiency.
        gradient = min(abs(gradient), 10) * sign(gradient) # Correct the sign
        return(gradient)
    }

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
    cat("\t", "Results:\n","Iteration:", i, "x        :", a, "f(x)     :", obj(a), "df(x)    :", d(a),
        sep = c("\t", "\n"))

    if(i >= max_iter){
        warning("Maximum number of iterations reached.")}
    return(a)
}

# ==============================================================================
# Bivariate Case:
bivariateMinimizer = function(obj, epsilon_step = 0.001, max_iter    = 10,
                              precision         = 1e-6, learn        = 0.5,
                              verbose           = FALSE, report_freq = 100){
      # Approximate Gradient
      get_2D_gradient = function(x, objective = obj, epsilon = epsilon_step){
          init      = matrix(data = x, nrow = 2, ncol=2, byrow = TRUE)
          steps     = init + diag(x = epsilon, ncol = 2, nrow = 2)

          f_steps   = apply(steps, 2, objective)
          f_comp    =  apply(init, 2, objective)

          D         = (f_steps - f_comp) / epsilon
          # Trim D to limit the gradient:
          D_trimmed = ifelse(abs(D) <= 5, abs(D), 5) * sign(D)
          return(D_trimmed)
      }
      # Draw multiple random starting points.
      # Take the combination that provides the lowest value of the objective function.
      a        = matrix(data = runif(10000, min = -100, max = 100),
                        ncol = 2)
      # DEBUG:
      f_a      = apply(a, 1, obj)
      a        = a[which.min(f_a), ]
      gradient = get_2D_gradient(a)

      i = 0

      learn_rates = seq(from = learn, to = 0, length.out = max_iter + 1)

      while(i <= max_iter & any(abs(gradient) >= precision)){
          if(i %% report_freq == 0 & verbose) {
            cat("\nStep:\t\t", i,
                "\nx:\t\t", a,
                "\ngradient:\t", gradient,
                "\nlearn:\t", learn_rates[i],
                "\n---------")}

          i = i + 1
          a = a - learn_rates[i] * gradient
          gradient = get_2D_gradient(a)
    }

    cat("\nResults\n",
        "\nIteration:\t", i,
        "\nx:\t\t", a,
        "\nf(x):\t\t", obj(a),
        "\ndf(x):\t\t", gradient)

    if(i >= max_iter){
        warning("Maximum number of iterations reached.")}
    return(a)
}

# ==============================================================================
# Multivariate Case:
gradientDescentMinimizer = function(
  obj, n_pars, epsilon_step   = 0.001,
  max_iter    = 10, precision = 1e-6,
  learn       = 0.5, verbose  = FALSE,
  report_freq = 100){
    # Check for invalid parameters:
    stopifnot(epsilon_step > 0,
              max_iter > 0,
              precision > 0,
              learn > 0,
              report_freq > 0)

    # Approximate Gradient
    get_gradient = function(x, d      = n_pars,
                            objective = obj,
                            epsilon   = epsilon_step){
        init      = matrix(data = x, nrow = d, ncol = d, byrow = TRUE)
        steps     = init + diag(x = epsilon, ncol = d, nrow = d)
        f_steps   = apply(steps, 1, objective)
        f_comp    =  apply(init, 1, objective)
        D         = (f_steps - f_comp) / epsilon
        D_trimmed = ifelse(abs(D) <= 100, abs(D), 100) * sign(D)
        return(D_trimmed)}

    # Draw multiple random starting points.
    # Use the one that provides the lowest value of the objective function.
    a = matrix(data = runif(1000 * n_pars,
                            min = -10,
                            max = 10),
               ncol = n_pars)
    f_a = apply(a, 1, obj)
    a = a[which.min(f_a), ]
    gradient = get_gradient(a)

    # Set up values for loop
    i = 0
    learn_rates = seq(from = learn, to = 0, length.out = max_iter + 1)

    while(any(abs(gradient) >= precision) & i <= max_iter){
        if(i %% report_freq == 0 & verbose) {
            cat("\nStep:\t\t", i,
                "\nx:\t\t", a,
                "\ngradient:\t", gradient,
                "\nlearn:\t", learn_rates[i],
                "\n------------------------------------")}

        i = i + 1
        a = a - learn_rates[i] * gradient
        gradient = get_gradient(a)
  }

    cat("\nResults\n",
        "\nIteration:\t", i,
        "\nx:\t\t", a,
        "\nf(x):\t\t", obj(a),
        "\ndf(x):\t\t", gradient,
        "\n")

    if(i >= max_iter){
        warning("Maximum number of iterations reached.")}
    return(a)
}

# ==============================================================================
## Other helpers
# ==============================================================================

get_numeric_cols = function(data){
    numerics = sapply(data, is.numeric)
    return(numerics)
}

get_group = function(data, by, group){
    num = get_numeric_cols(data)
    x = subset(data, data[, by] == group,
               select = num)
    return(x)
}

get_class_means = function(Data, By, ...){
    groups = unique(Data[, By])
    grp = as.list(groups)
    sub_data = lapply(X = grp, FUN = get_group,
                      by = By, data = Data)
    # Get list of means.
    means = sapply(X = sub_data, FUN = colMeans, ...)
    colnames(means) = as.character(groups)
    return(means)
}

get_class_cov = function(Data, By, ...){
    groups = unique(Data[, By])
    grp = as.list(groups)
    sub_data = lapply(X = grp, FUN = get_group,
                      by = By, data = Data)
    # Get list of covariance matrices.
    covs = lapply(X = sub_data, FUN = cov, ...)
    names(covs) = as.character(groups)
    return(covs)
}
