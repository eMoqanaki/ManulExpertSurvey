# -------------------------------------------------------------------------
# Bayesian Ordinal Regression Modeling
# doi: https://doi.org/10.1007/s13364-026-00859-9
# -------------------------------------------------------------------------

# Library
library(brms)

# Download survey data from Github:
# https://github.com/eMoqanaki/ManulExpertSurvey/blob/main/manul_survey_anonymized.rds

# Load data
survey_data <- readRDS(file.path(dir_data, "manul_survey_anonymized.rds"))

# Constants
n_chains <- 4
n_iter   <- 10^4
n_core   <- 6

# Priors
priors <- c(
  set_prior("normal(0, 4)", class = "Intercept"),
  set_prior("normal(0, 4)", class = "b"), 
  set_prior("student_t(3, 0, 2.5)", class = "sd")
)

# Prepare variables
resp_patterns <- c("Threat", "Conservation", "Research", "Value", "ResearchLimits")
all_cols <- names(survey_data)

# Five models
mod_list <- list()
for ( i in seq_along(resp_patterns) ) {
  
  resp_patterns[i]
  
  pattern <- paste0("^", resp_patterns[i])
  group_resp_vars <- all_cols[grepl(pattern, all_cols)]
  pred_vars <- all_cols[!grepl(paste(resp_patterns, collapse="|"), all_cols)]
  pred_vars <- pred_vars[!(pred_vars %in% c("ID", "CountryID"))]
  
  # Reshape data 
  long_cols <- c("ID", "CountryID", group_resp_vars)
  this_dat_wide <- survey_data[, long_cols]
  
  manul_dat_long <- reshape(
    this_dat_wide, 
    varying = group_resp_vars, 
    v.names = "resp",
    timevar = "order",
    times = group_resp_vars,
    direction = "long"
  )
  
  # Merge predictors back in
  manul_dat <- merge(
    manul_dat_long, 
    survey_data[, c("ID", "CountryID", pred_vars)], 
    by = c("ID", "CountryID")
  )
  
  # Construct formula
  rhs_fixed <- paste(pred_vars, collapse = " + ")
  random_effects <- "(1 | ID/CountryID) + (1 | order)"
  formula_str <- paste("resp ~ 1 +", rhs_fixed, "+", random_effects)
  myformula <- as.formula(formula_str)
  
  print(myformula)

  # Determine family
  family_func <- if ( resp_patterns[i] == "ResearchLimits" ) {
    bernoulli("logit") 
  } else { 
    cumulative("probit") 
  }
  
  # Fit model
  mod_list[[resp_patterns[i]]] <- brm(
    formula = myformula,
    data = manul_dat, 
    family = family_func,
    prior = priors, 
    chains = n_chains, 
    iter = n_iter, 
    cores = n_core,
    init = "0"
  )
  
  # Clean up memory
  rm(manul_dat, manul_dat_long, this_dat_wide)
  gc()

}#mod

# Example of accessing results
mod <- "Threat"

print(mcmc_plot(mod_list[[mod]], type = "trace"), ask = FALSE)
summary(mod_list[[mod]])
posterior_summary(mod_list[[mod]])
