source("BRM/utils.R")

# Define a function that returns a log-likelihood function.
get_loglikelihood = function(data, y, distr){
  grp = unique(data[, y])
  nums = get_numeric_cols(data)
  
  Xy_mat = as.matrix(cbind(
    data[, which(nums)],
    y = ifelse(data[, y] == grp[1], 1, 0)))
  
  # y is always the last column
  y_pos = dim(Xy_mat)[2]
  
  l = function(x){
    - sum(apply(Xy_mat, 1,
                function(X){
                  X[y_pos] * distr(t(X[-y_pos]) %*% x,
                                   lower.tail = TRUE,
                                   log.p = TRUE) + 
                    (1-X[y_pos]) * distr(t(X[-y_pos]) %*% x,
                                         lower.tail = FALSE,
                                         log.p = TRUE)}))}
  # Return loglikelihood as a function of x (here 'x' stands for the weights)
  return(l)
}

# Get logit-weights:
brm = function(data, y, mode, ...){
  distr = switch (mode,
                  "logit" = plogis,
                  "probit" = pnorm
  )
  
  # Add column of ones for the intercept:
  Data = cbind(constant = 1,
               data)
  
  llog = get_loglikelihood(Data, y, distr)
  params = get_numeric_cols(Data)
  
  # We use -obj because we want to maximize obj (i.e. the likelihood).
  beta = gradientDescentMinimizer(obj = llog,
                                  sum(params),
                                  ...)
  names(beta) = names(which(params))
  # Get fitted probabilities:
  Xb = as.matrix(Data[, params]) %*% beta
  Data$fitted_values = sapply(X = Xb, FUN = distr)
  Data$true_values = Data[, y]
  
  out = list(X = Data[, params],
             weights = beta,
             likelihood_val = llog(beta),
             type = mode,
             distribution = distr)
  
  return(structure(out, class = "brm"))
}

predict.brm = function(model, data){
  Xb = as.matrix(cbind(constant = 1, data)) %*% model$weights
  predictions = sapply(X = Xb,
                       FUN = model$distribution,
                       log.p = FALSE,
                       lower.tail = TRUE)
  return(predictions)
}
