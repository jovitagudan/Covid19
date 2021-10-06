

#Estimation of negative exponential model
neg_expo_model <- function(dependent_var, start_id, end_id){
  
  y <- dependent_var[start_id:end_id]
  x <- 1:length(y) 
  theta.0 <- max(y) * 1.1
  model.0 <- lm(log(- y + theta.0) ~ x)
  alpha.0 <- -exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  # Fit the model
  model <- nls(y ~ alpha * exp(beta * x) + theta , start = start)
  return(model)
}