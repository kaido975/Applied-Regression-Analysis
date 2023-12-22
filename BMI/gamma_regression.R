################################################################
# Part (b): Estimation
################################################################

#' Estimate parameters of a gamma GLM
#'
#' @param formula A formula object
#' @param data A data frame containing response and predictors
#'
#' @return An object containing fields beta_hat and phi_hat for the
#'         fitted coefficient vector and dispersion parameter, along with 
#'         fields X and y for the inputted data.
fit_gamma_glm <- function(formula, data = NULL) {
  # extract model matrix X and response vector y based on formula and data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(mf, data)
  # estimate beta
  beta_hat <- estimate_beta(y, X)
  # estimate phi
  phi_hat <- estimate_phi(y, X, beta_hat)
  # output estimates, along with the inputted data
  list(beta_hat = beta_hat, phi_hat = phi_hat, X = X, y = y)
}

#' Estimate coefficient vector in a gamma regression
#'
#' This function should repeatedly update the coefficient vector via calls to `lm()` 
#' until the first iteration where each coordinate changes by less than 1e-10.
#'
#' @param y A length n response vector
#' @param X An nxp model matrix
#'
#' @return A length p fitted coefficient vector
estimate_beta <- function(y, X) {
  p <- dim(X)[2]
  beta <- rep(0, p)
  tol <- 1.0
  while(tol > 1e-10){
    #Mean prediction with log link
    mu <- exp(X %*% beta)
    
    #M matrix
    M <- 1/mu
    #Denationalization of M matrix
    M <- matrix(diag(as.vector(M)),ncol=length(y))
    
    #Z vector 
    z <- M %*% (y - mu) +  X %*% beta
    
    #Regressing Z vector on X to get estimates
    beta_est <- coef(lm(z ~ X-1))
    
    #Change in estimates
    err <- abs(beta_est - beta)
    
    #maximum of change should be below tolerance
    tol <- max(err)
    
    #Update estimates
    beta <- beta_est
  }
  return (beta)
}

#' Estimate dispersion parameter in a gamma regression
#'
#' @param y An length n response vector
#' @param X An nxp model matrix
#' @param beta_hat A length p fitted coefficient vector
#'
#' @return An estimate for the dispersion parameter
estimate_phi <- function(y, X, beta_hat) {
  dims <- dim(X)
  n <- dims[1]
  p <- dims[2]
  #Mean prediction with log link
  mu <- exp(X %*% beta_hat)
  
  #Pearson's score
  score <- ((y-mu)/mu)^2
  
  #Dispersion Estimate
  phi <- sum(score)/(n-p)
}

################################################################
# Part (c): Wald inference
################################################################

#' Carry out Wald inference for a gamma GLM
#'
#' @param gamma_glm_fit Fit object returned by fit_gamma_glm
#'
#' @return A matrix with one row per variable, and columns `Variable`, 
#' `Estimate`, `Std. Error`, `t value`, and `Pr(>|t|)`
wald_inference <- function(gamma_glm_fit) {
  #Extracting results from GLM fit
  beta <- gamma_glm_fit$beta_hat
  phi <- gamma_glm_fit$phi_hat
  X <- gamma_glm_fit$X
  
  dims <- dim(X)
  n <- dims[1]
  p <- dims[2]
  
  #Significance level and t quantile
  alpha <- 0.05
  t_quant <- qt(1-alpha/2, df=n-p)
  
  #Standard error calculation 
  SE <- sqrt(phi*diag(inv(t(X) %*% X)))

  #Confidence interval
  lower <- beta - SE*t_quant
  upper <- beta + SE*t_quant
  
  #Test statistics
  t_value <- beta/SE

  #Pvalues of the test statistic
  p_value <- 2 * pt(abs(t_value), df = n-p, lower.tail = FALSE)
  Estimates <- beta
  #Adding results to data frame
  df <- data.frame(Estimates, SE, t_value, p_value)
  
  #Row name. format
  rownames(df) <- sub('.', '', rownames(df))
  
  return (df)
}
  
################################################################
# Part (d): Likelihood ratio test
################################################################

#' Carry out likelihood ratio test for two nested gamma GLM models
#'
#' @param gamma_glm_fit_partial Fit object from partial model
#' @param gamma_glm_fit_full Fit object from full model
#'
#' @return Likelihood ratio test p-value
likelihood_ratio_test <- function(gamma_glm_fit_partial, gamma_glm_fit_full) {
  alpha <- 0.05
  
  #Extract partial fit results
  beta_partial <- gamma_glm_fit_partial$beta_hat
  phi_partial <- gamma_glm_fit_partial$phi_hat
  X_partial <- gamma_glm_fit_partial$X
  dims <- dim(X_partial)
  p1 <- dims[2] 
  y <- gamma_glm_fit_partial$y
  mu_partial <- exp(X_partial %*% beta_partial)
  
  #Compute Deviance for partial model
  score1 <- sum(log(y/mu_partial))
  
  #Extract full model results
  beta_full <- gamma_glm_fit_full$beta_hat
  phi_full <- gamma_glm_fit_full$phi_hat
  X_full <- gamma_glm_fit_full$X
  dims <- dim(X_full)
  n <- dims[1]
  p2 <- dims[2] 
  S <- p2 - p1
  mu_full <- exp(X_full %*% beta_full)
  
  #Compute Deviance for partial model
  score2 <- sum(log(y/mu_full))
  
  #Compute the F-statistic (Scaled by dispersion of full model)
  F_stat <-  2*(score2 - score1)/S/phi_full
  
  #Compute P value based on F distribution
  p_value <- pf(F_stat, S, n-p2, lower.tail = FALSE)
  
  return (c(F_stat, p_value))
  
}