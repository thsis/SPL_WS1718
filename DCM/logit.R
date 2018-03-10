source("DCM/utils.R")

# Define a function that returns a log-likelihood function.
get_loglikelihood = function(data, y, distr){
  grp = unique(data[, y])
  y_col = which(colnames(data) == y)
  nums = get_numeric_cols(data)
  
  Xy_mat = as.matrix(cbind(
    data[, -y_col],
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
dcm = function(data, y, mode, ...){
  distr = switch (mode,
    "logit" = plogis,
    "probit" = pnorm
  )
  llog = get_loglikelihood(data, y, distr)
  params = get_numeric_cols(data)
  
  # We use -obj because we want to maximize obj (i.e. the likelihood).
  beta = gradientDescentMinimizer(obj = llog,
                                  length(params) - 1,
                                  ...)
  
  # Get fitted probabilities:
  Xb = as.matrix(data[, params]) %*% beta
  data$fitted_values = sapply(X = Xb, FUN = distr)
  data$true_values = data[, y]
   
  out = list(X = data[, params],
             weights = beta,
             likelihood_val = llog(beta),
             type = mode,
             distribution = distr)
  
  return(structure(out, class = "dcm"))
}

predict.dcm = function(model, data){
  Xb = as.matrix(data) %*% model$weights
  predictions = sapply(X = Xb,
                       FUN = model$distribution,
                       log.p = FALSE,
                       lower.tail = TRUE)
  return(predictions)
}
