# Pulling it all together for trial sims

source("gen_params.R")
source("gen_model.R")
source("gen_trial.R")

require(mgcv)

n_runs <- 20000
n_arm <- 1000

my_pars <- as.data.frame(ades_parameters(n_runs))
my_PSA <- ades_model(my_pars)
my_sumstats <- trial_sim(my_pars$P_C, my_pars$P_T, n_arm)

gam_mod <- gam(my_PSA ~ te(my_sumstats))
ghat <- data.frame(g1 = 0, g2 = fitted(gam_mod))

evsi_trial <- mean(do.call(pmax, ghat)) - max(colMeans(ghat))