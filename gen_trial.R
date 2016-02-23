# function to simulate a simple OR trial

trial_sim <- function(P_C, P_T, n1, n2=n1){
  mylength <- length(P_C)
  x1 <- rbinom(mylength,n1,P_C)
  x2 <- rbinom(mylength,n2,P_T)
  x1 <- ifelse(x1 == 0 | x1 == n1, x1 + 0.5, x1)
  n1 <- ifelse(x1 == 0 | x1 == n1, n1 + 1, n1)
  x2 <- ifelse(x2 == 0 | x2 == n2, x2 + 0.5, x2)
  n2 <- ifelse(x2 == 0 | x2 == n2, n2 + 1, n2)
  Tx <- log((x2/(n2-x2))/(x1/(n1-x1)))
  Tx
}

