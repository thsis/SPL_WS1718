library("geigen")

# Import helpers
source("LDA/utils.R")

lda = function(data, by){
  # Closed form solution of LDA: 
  # w = S_b^-0.5 * largest_eigenvector(S_b^0.5 * S_w^-1 * S_b^0.5) 
   
  # Check prerequisites
  num = get_numeric_cols(data = data)
  classes = unique(data[, by])
  
  stopifnot(
    length(num) > 0,
    length(classes) == 2)
  
  mu = get_class_means(Data = data, By = by, na.rm = TRUE)
  S = get_class_cov(Data = data, By = by, use = "complete.obs")
  n = sapply(classes, function(x){sum(data[, by] == x)})
  
  # Compute overall mean:
  x_bar = colMeans(data[, num], na.rm = TRUE)
  
  # Compute between class scatter matrix:
  Sb1 = (mu[, 1] - x_bar) %*% t(mu[, 1] - x_bar)
  Sb2 = (mu[, 2] - x_bar) %*% t(mu[, 2] - x_bar)
  S_b = Sb1 + Sb2
  
  # Compute within class scatter matrix:
  S_w = (n[1]-1)*S[[1]] + (n[2]-1)*S[[2]]

  # Extract eigenvector corresponding to largest eigenvalue
  # geigen solves the generalized eigenvalue problem of:
  # A*x = lambda B*x
  V = geigen(S_b, S_w, symmetric = TRUE)
  
  # Extract (absolutely) largest eigenvalue
  ev_order = order(abs(V[["values"]]), decreasing = TRUE)
  v = V[["vectors"]][ev_order[1:2], ]
  
  # Percent of variance explained:
  inertia = (V[["values"]][ev_order[1:2]])^2 / sum(V[["values"]]^2)
  
  # Compute rotations
  lda1 = apply(X = data[, num], MARGIN = 1, FUN = function(x){sum(x * v[1, ])})
  lda2 = apply(X = data[, num], MARGIN = 1, FUN = function(x){sum(x * v[2, ])})
  
  out = list(
    X = data.frame("lda1" = lda1,
                   "lda2" = lda2,
                   "labels" = data[, by]),
    classes = classes,
    class_means = mu,
    scalings = v,
    explained_var = inertia)
  
  return(structure(out, class = "flda"))
}

plot.flda = function(model){
  p = ggplot2::ggplot(data = model$X, ggplot2::aes_string(x = "lda1", y = "lda2", color = "labels")) +
    ggplot2::geom_point(alpha = 0.5, shape=21) +
    ggplot2::ggtitle("Projection on the first 2 linear discriminants") +
    ggplot2::xlab("First linear discriminant") +
    ggplot2::ylab("Second linear discriminant") +
    ggplot2::theme_bw()
  return(p)
}

predict.flda = function(model, data){
  v = model$scalings[1, ]
  num = get_numeric_cols(data)
  
  Data = data[, num]
  dims = dim(Data)
  stopifnot(dims[2] == length(v))
  
  # Compute discriminant as the dot product of every observation
  # with the scaling vector v:
  discr = apply(X = Data, MARGIN = 1, FUN = function(x){sum(x * v)})
  
  # Compute cutoff threshold:
  c = 0.5 * t(v) %*% (rowSums(model$class_means))
  predictions = ifelse(discr <= as.numeric(c), model$classes[1], model$classes[2])
  
  return(predictions)
}
