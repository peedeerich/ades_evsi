# Code to calculate net benefits from Ades et al cost effectiveness model
# An exemplar decision tree

# No need to code as a decision tree as such, just do it directly

source("gen_params.R")



ades_model <- function(pars){
  
  pars <- as.data.frame(pars)
  attach(pars)
  NB_1 <- P_C * (lambda * L * ((1+Q_E)/2) - C_E) + (1 - P_C) * lambda * L
  NB_2_a <- P_SE * P_T * (lambda * (L*((1 + Q_E)/2) - Q_SE) - (C_T + C_SE + C_E))
  NB_2_b <- P_SE * (1 - P_T) * (lambda*(L - Q_SE) - (C_T + C_SE))
  NB_2_c <- (1 - P_SE) * P_T * (lambda * L * ((1 + Q_E)/2) - (C_T + C_E))
  NB_2_d <- (1 - P_SE) * (1 - P_T) * (lambda*L - C_T)
  NB_2 <- NB_2_a + NB_2_b + NB_2_c + NB_2_d
  detach(pars)
  
  PSA <- NB_2 - NB_1
  invisible(PSA)
}