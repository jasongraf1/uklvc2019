# -------------------------------------------------------------------------
# File: 01_brms_analysis.R
# Run Bayesian regression models for the first 2 lines of the VADIS analysis
# -------------------------------------------------------------------------

source("R/00_load_data.R")

# Set the rstan options.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# model formula
fmla <- Type ~ (1 | Speaker_ID) +
  (0 + Possessor.Animacy3 | Speaker_ID) +
  Possessor.Animacy3 +
  Possessor.Length +
  Possessum.Length +
  Final.Sibilant +
  TTR +
  Possessor.Givenness +
  Possessor.Expression.Type +
  PersistenceBinary +
  ProtoSemanticRelation +
  Possessor.Thematicity


# Fit models --------------------------------------------------------------

# Set priors. Gelman (2008) recommends default Cauchy priors, but there are
# potential near separation issues, so we'll use a prior with narrower tails
# (see Ghosh et al. 2018)
priors <- c(
  set_prior("normal(0, 2.5)", "Intercept"),
  set_prior("normal(0, 2.5)", class = "b")
)

t <- proc.time()
gen_brms_list <- vector("list")
for (i in seq_along(gen_data_list)){
  d <- gen_data_list[[i]]
  # standardize the model inputs, excluding the response and random effects
  d_std <- stand(d, cols = fmla) # use the fitting formula for convenience
  # fit the model
  gen_brms_list[[i]] <- brm(
    fmla,
    data = d_std,
    family = "bernoulli",
    prior = priors,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    seed = 43214,
    control = list(adapt_delta = 0.99),
    refresh = 0
  )
  rm(d, d_std) # remove datasets
}
names(gen_brms_list) <- names(gen_data_list)
saveRDS(gen_brms_list, "output/gen_brms_list.rds")
elapsed <- proc.time() - t

# Models can be evaluated individually with e.g., shinystan
# shinystan::launch_shinystan(gen_brms_list[[1]])

# References --------------------------------------------------------------

# Gelman, Andrew, Aleks Jakulin, Maria Grazia Pittau & Yu-Sung Su. 2008. A weakly informative default prior distribution for logistic and other regression models. The Annals of Applied Statistics 2(4). 1360–1383. doi:10.1214/08-AOAS191.
# Ghosh, Joyee, Yingbo Li & Robin Mitra. 2018. On the use of Cauchy prior distributions for Bayesian logistic regression. Bayesian Analysis 13(2). 359–383. doi:10/gdfv6g.



