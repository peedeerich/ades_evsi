ades_parameters <- function(n){
  
  L         <- 30
  logit_Q_E <- rnorm(n,0.6,1/6)
  Q_SE      <- 1
  C_E       <- 200000
  C_T       <- 15000
  C_SE      <- 100000
  P_C       <- rbeta(n,15,85)
  P_SE      <- rbeta(n,3,9)
  log_OR    <- rnorm(n,-1.5,1/3)
  lambda    <- 75000
  
  Q_E <- (exp(logit_Q_E)/(1 + exp(logit_Q_E)))
  logit_P_T <- log(P_C/(1-P_C)) + log_OR
  P_T <- (exp(logit_P_T)/(1 + exp(logit_P_T)))
  
    
  pars      <- cbind(L, Q_E, Q_SE, C_E, C_T, C_SE, P_C, P_SE, log_OR, P_T, lambda)
  
  invisible(pars)
}

