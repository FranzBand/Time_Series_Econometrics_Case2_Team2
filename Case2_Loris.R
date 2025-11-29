#-------- PART: LORIS ---------

# --- Granger causality tests using vars::causality() ---

# Does INDPRO Granger-cause FEDFUNDS?
causality(var_model, cause = "trans_indpro")

# Does FEDFUNDS Granger-cause INDPRO?
causality(var_model, cause = "trans_fedfunds")

# Does CPI Granger-cause INDPRO?
causality(var_model, cause = "trans_cpiaucsl")

# You can repeat for each variable as the "cause":
causality(var_model, cause = "trans_indpro")
causality(var_model, cause = "trans_fedfunds")
causality(var_model, cause = "trans_cpiaucsl")

cat("H0: The lags of the variable named in 'cause' DO NOT help predict the others (no Granger causality).\n")
cat("H1: At least one lag of the 'cause' variable DOES help predict the others (there IS Granger causality).\n")

# --- Function for bivariate Granger causality test ---

# --------- Helper: bivariate Granger test (X -> Y) ---------
# y_name : name of dependent variable (string)
# x_name : name of "cause" variable (string)
# p      : number of lags
# data   : matrix or data.frame with columns y_name and x_name (here: var_data)

biv_granger <- function(y_name, x_name, p, data) {
  
  y <- as.numeric(data[, y_name])
  x <- as.numeric(data[, x_name])
  
  T <- length(y)
  
  # build lagged data frame
  df <- data.frame(y = y[(p + 1):T])
  
  for (i in 1:p) {
    df[[paste0("y_l", i)]] <- y[(p + 1 - i):(T - i)]
    df[[paste0("x_l", i)]] <- x[(p + 1 - i):(T - i)]
  }
  
  # unrestricted: y on its own lags + lags of x
  form_unres <- as.formula(
    paste("y ~", 
          paste(c(paste0("y_l", 1:p), paste0("x_l", 1:p)), collapse = " + "))
  )
  
  # restricted: y only on its own lags (no x lags)
  form_res <- as.formula(
    paste("y ~", paste(paste0("y_l", 1:p), collapse = " + "))
  )
  
  unres <- lm(form_unres, data = df)
  res   <- lm(form_res,   data = df)
  
  # F-test comparing restricted vs unrestricted
  anova(res, unres)
}

# All six bivariate Granger tests:
biv_granger("trans_fedfunds", "trans_indpro", 3, var_data)     # INDPRO → FEDFUNDS
biv_granger("trans_indpro", "trans_fedfunds", 3, var_data)     # FEDFUNDS → INDPRO
biv_granger("trans_indpro", "trans_cpiaucsl", 3, var_data)     # CPI → INDPRO
biv_granger("trans_cpiaucsl", "trans_indpro", 3, var_data)     # INDPRO → CPI
biv_granger("trans_cpiaucsl", "trans_fedfunds", 3, var_data)   # FEDFUNDS → CPI
biv_granger("trans_fedfunds", "trans_cpiaucsl", 3, var_data)   # CPI → FEDFUNDS


# ---- Impulse Response Functions for reduced-form VARs ----

library(vars)

# IRFs for all three shocks and all three responses, no CI yet
irf_rf_point <- irf(
  var_model,
  impulse  = c("trans_indpro", "trans_fedfunds", "trans_cpiaucsl"),
  response = c("trans_indpro", "trans_fedfunds", "trans_cpiaucsl"),
  n.ahead  = 24,
  boot     = FALSE,     # point estimates only
  ortho    = FALSE      # reduced-form shocks, NOT orthogonalized
)

# Plot all IRFs (reduced-form, in transformed units)
plot(irf_rf_point)

# IRFs with 95% bootstrap CIs
set.seed(123)   # for reproducibility

irf_rf_boot <- irf(
  var_model,
  impulse  = c("trans_indpro", "trans_fedfunds", "trans_cpiaucsl"),
  response = c("trans_indpro", "trans_fedfunds", "trans_cpiaucsl"),
  n.ahead  = 24,
  boot     = TRUE,
  ci       = 0.95,
  runs     = 500,       # you can increase to 1000 if time allows
  ortho    = FALSE
)

plot(irf_rf_boot)

# Helper functions for cumulation
cum_diff1 <- function(x) apply(x, 2, cumsum)                 # for Δ and Δ log
cum_diff2 <- function(x) apply(apply(x, 2, cumsum), 2, cumsum) # for Δ² log

# Start from the bootstrap IRFs
irf_levels <- irf_rf_boot

# Transform point estimates
irf_levels$irf <- lapply(irf_rf_boot$irf, function(m) {
  out <- m
  # INDPRO in log-levels (cum of Δ log)
  out[, "trans_indpro"]   <- cumsum(m[, "trans_indpro"])
  # FEDFUNDS in levels (cum of Δ)
  out[, "trans_fedfunds"] <- cumsum(m[, "trans_fedfunds"])
  # CPI in log-levels (double cum of Δ² log)
  out[, "trans_cpiaucsl"] <- cumsum(cumsum(m[, "trans_cpiaucsl"]))
  out
})

# Transform lower CI
irf_levels$Lower <- lapply(irf_rf_boot$Lower, function(m) {
  out <- m
  out[, "trans_indpro"]   <- cumsum(m[, "trans_indpro"])
  out[, "trans_fedfunds"] <- cumsum(m[, "trans_fedfunds"])
  out[, "trans_cpiaucsl"] <- cumsum(cumsum(m[, "trans_cpiaucsl"]))
  out
})

# Transform upper CI
irf_levels$Upper <- lapply(irf_rf_boot$Upper, function(m) {
  out <- m
  out[, "trans_indpro"]   <- cumsum(m[, "trans_indpro"])
  out[, "trans_fedfunds"] <- cumsum(m[, "trans_fedfunds"])
  out[, "trans_cpiaucsl"] <- cumsum(cumsum(m[, "trans_cpiaucsl"]))
  out
})

# Plot IRFs interpreted as responses of the original series in (log-)levels
plot(irf_levels)

