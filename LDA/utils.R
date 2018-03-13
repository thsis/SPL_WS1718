# Define helper-functions:
#     - get all numeric columns of a dataframe
#     - get mean vector, grouped by class label
#     - get covariance matrix, grouped by class label
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
  # Get matrix of means.
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
