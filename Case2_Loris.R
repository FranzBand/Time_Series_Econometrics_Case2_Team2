#-------- PART: LORIS ---------

# --- Granger causality tests using vars::causality() ---

# Does INDPRO Granger-cause FEDFUNDS?
causality(var_model, cause = "trans_indpro")

# Does FEDFUNDS Granger-cause INDPRO?
causality(var_model, cause = "trans_fedfunds")

# Does CPI Granger-cause INDPRO?
causality(var_model, cause = "trans_cpi")

# You can repeat for each variable as the "cause":
causality(var_model, cause = "trans_indpro")
causality(var_model, cause = "trans_fedfunds")
causality(var_model, cause = "trans_cpi")

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
biv_granger("trans_fedfunds", "trans_indpro", 3, var_data)      # INDPRO -> FEDFUNDS
biv_granger("trans_indpro", "trans_fedfunds", 3, var_data)      # FEDFUNDS -> INDPRO
biv_granger("trans_indpro", "trans_cpi", 3, var_data)      # CPI -> INDPRO
biv_granger("trans_cpi", "trans_indpro", 3, var_data)      # INDPRO -> CPI
biv_granger("trans_cpi", "trans_fedfunds", 3, var_data)    # FEDFUNDS -> CPI
biv_granger("trans_fedfunds", "trans_cpi", 3, var_data)    # CPI -> FEDFUNDS


# ---- Impulse Response Functions for reduced-form VARs ----

# IRFs for all three shocks and all three responses, no CI yet
irf_rf_point <- irf(
  var_model,
  impulse  = c("trans_indpro", "trans_fedfunds", "trans_cpi"),
  response = c("trans_indpro", "trans_fedfunds", "trans_cpi"),
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
  impulse  = c("trans_indpro", "trans_fedfunds", "trans_cpi"),
  response = c("trans_indpro", "trans_fedfunds", "trans_cpi"),
  n.ahead  = 24,
  boot     = TRUE,
  ci       = 0.95,
  runs     = 500,       # you can increase to 1000 if time allows
  ortho    = FALSE
)

plot(irf_rf_boot)

# Helper functions for cumulation
cum_diff1 <- function(x) apply(x, 2, cumsum)                 # for diff and diff log
cum_diff2 <- function(x) apply(apply(x, 2, cumsum), 2, cumsum) # for diff diff log

# Start from the bootstrap IRFs
irf_levels <- irf_rf_boot

# Transform point estimates
irf_levels$irf <- lapply(irf_rf_boot$irf, function(m) {
  out <- m
  # INDPRO in log-levels (cum of diff log)
  out[, "trans_indpro"]   <- cumsum(m[, "trans_indpro"])
  # FEDFUNDS in levels (cum of diff)
  out[, "trans_fedfunds"] <- cumsum(m[, "trans_fedfunds"])
  # CPI in log-levels (double cum of diff diff log)
  out[, "trans_cpi"] <- cumsum(cumsum(m[, "trans_cpi"]))
  return(out)
})

# Transform lower CI
irf_levels$Lower <- lapply(irf_rf_boot$Lower, function(m) {
  out <- m
  out[, "trans_indpro"]   <- cumsum(m[, "trans_indpro"])
  out[, "trans_fedfunds"] <- cumsum(m[, "trans_fedfunds"])
  out[, "trans_cpi"] <- cumsum(cumsum(m[, "trans_cpi"]))
  return(out)
})

# Transform upper CI
irf_levels$Upper <- lapply(irf_rf_boot$Upper, function(m) {
  out <- m
  out[, "trans_indpro"]   <- cumsum(m[, "trans_indpro"])
  out[, "trans_fedfunds"] <- cumsum(m[, "trans_fedfunds"])
  out[, "trans_cpi"] <- cumsum(cumsum(m[, "trans_cpi"]))
  return(out)
})

# Plot IRFs interpreted as responses of the original series in (log-)levels
plot(irf_levels)

# ==============================================================================
# PLOT FOR REDUCED-FORM POINT ESTIMATES (NO CI)
# ==============================================================================

# 1. Helper Function: Convert Point Estimate Object to Data Frame
extract_point_irf <- function(irf_object) {
  impulse_names <- names(irf_object$irf)
  df_list <- list()
  
  for (imp in impulse_names) {
    # Extract only the Point Estimate matrix
    pe <- as.data.frame(irf_object$irf[[imp]])
    
    # Add Horizon and Impulse Name columns
    pe$horizon <- 0:(nrow(pe)-1)
    pe$impulse <- imp
    
    # Reshape to Long Format for ggplot
    pe_long <- melt(pe, id.vars = c("horizon", "impulse"), 
                    variable.name = "response", value.name = "irf")
    
    df_list[[imp]] <- pe_long
  }
  return(do.call(rbind, df_list))
}

# 2. Extract Data from your existing object 'irf_rf_point'
# (Make sure you ran the 'irf_rf_point' block from earlier!)
plot_data_point <- extract_point_irf(irf_rf_point)

# 3. Create Nice Labels (Updated for cpi)
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_fedfunds" = "Fed Funds Rate", 
  "trans_cpi"      = "CPI (Inflation)"
)

# 4. Generate the Plot
ggplot(plot_data_point, aes(x = horizon, y = irf)) +
  # Main Line (Black)
  geom_line(color = "black", linewidth = 0.8) +
  
  # Add Zero Reference Line (Red)
  geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 0.5) +
  
  # Grid Layout: Rows = Responses, Cols = Impulses
  facet_grid(response ~ impulse, scales = "free_y", 
             labeller = labeller(impulse = label_map, response = label_map)) +
  
  # Clean Theme
  theme_bw() +
  labs(
    title = "Impulse Response Functions (Reduced Form)",
    subtitle = "Point Estimates (Differences / Growth Rates)",
    x = "Horizon (Months)",
    y = "Response"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "#f0f0f0", color = "black")
  )


# ==============================================================================
# PLOT FOR BOOTSTRAPPED IRFs (WITH 95% CI)
# ==============================================================================

# 1. Helper Function: Convert Bootstrapped IRF Object to Data Frame
extract_boot_irf <- function(irf_object) {
  impulse_names <- names(irf_object$irf)
  df_list <- list()
  
  for (imp in impulse_names) {
    # Extract Point Estimates
    pe <- as.data.frame(irf_object$irf[[imp]])
    # Extract Lower Confidence Interval
    lo <- as.data.frame(irf_object$Lower[[imp]])
    # Extract Upper Confidence Interval
    up <- as.data.frame(irf_object$Upper[[imp]])
    
    # Add Horizon and Impulse columns
    pe$horizon <- lo$horizon <- up$horizon <- 0:(nrow(pe)-1)
    pe$impulse <- lo$impulse <- up$impulse <- imp
    
    # Reshape to Long Format
    pe_long <- melt(pe, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "irf")
    lo_long <- melt(lo, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "lower")
    up_long <- melt(up, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "upper")
    
    # Merge all into one dataframe
    merged <- merge(pe_long, lo_long, by=c("horizon", "impulse", "response"))
    merged <- merge(merged, up_long, by=c("horizon", "impulse", "response"))
    
    df_list[[imp]] <- merged
  }
  return(do.call(rbind, df_list))
}

# 2. Extract Data from your existing object 'irf_rf_boot'
# (Make sure you ran the 'irf_rf_boot' block with boot=TRUE!)
plot_data_boot <- extract_boot_irf(irf_rf_boot)

# 3. Create Nice Labels (Updated for cpi)
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_fedfunds" = "Fed Funds Rate", 
  "trans_cpi"      = "CPI (Inflation)"
)

# 4. Generate the Plot
ggplot(plot_data_boot, aes(x = horizon, y = irf)) +
  # Confidence Interval Ribbon (Shaded)
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) +
  
  # Main Line (Black)
  geom_line(color = "black", linewidth = 0.8) +
  
  # Add Zero Reference Line (Red)
  geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 0.5) +
  
  # Grid Layout: Rows = Responses, Cols = Impulses
  facet_grid(response ~ impulse, scales = "free_y", 
             labeller = labeller(impulse = label_map, response = label_map)) +
  
  # Clean Theme
  theme_bw() +
  labs(
    title = "Impulse Response Functions (Reduced Form)",
    subtitle = "With 95% Bootstrap Confidence Intervals",
    caption = "Shaded area represents 95% Confidence Interval",
    x = "Horizon (Months)",
    y = "Response"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "#f0f0f0", color = "black")
  )


# ==============================================================================
# 7. Impulse Response Functions (Cumulative Levels)
# ==============================================================================

# --- Helper Function: Convert IRF Object to Tidy Data Frame ---
extract_irf_df <- function(irf_object) {
  impulse_names <- names(irf_object$irf)
  df_list <- list()
  
  for (imp in impulse_names) {
    # Extract Point Estimates, Lower CI, and Upper CI
    pe <- as.data.frame(irf_object$irf[[imp]])
    lo <- as.data.frame(irf_object$Lower[[imp]])
    up <- as.data.frame(irf_object$Upper[[imp]])
    
    # Add Horizon and Impulse Name
    pe$horizon <- lo$horizon <- up$horizon <- 0:(nrow(pe)-1)
    pe$impulse <- lo$impulse <- up$impulse <- imp
    
    # Melt to Long Format
    pe_long <- melt(pe, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "irf")
    lo_long <- melt(lo, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "lower")
    up_long <- melt(up, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "upper")
    
    # Merge into single dataframe
    merged <- merge(pe_long, lo_long, by=c("horizon", "impulse", "response"))
    merged <- merge(merged, up_long, by=c("horizon", "impulse", "response"))
    
    df_list[[imp]] <- merged
  }
  return(do.call(rbind, df_list))
}

# --- Generate the Plot ---

# 1. Get data from your calculated levels object
plot_data <- extract_irf_df(irf_levels) 

# 2. Define nice labels for the graph (Updated for cpi)
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_fedfunds" = "Fed Funds Rate", 
  "trans_cpi"      = "CPI (Inflation)"
)

# 3. Create the Grid Plot (CORRECTED)
ggplot(plot_data, aes(x = horizon, y = irf)) +
  # Confidence Interval (Shaded Area)
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  # Main IRF Line
  geom_line(color = "steelblue", linewidth = 1) +
  # Zero Reference Line
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  # Grid Layout: Rows = Responses, Cols = Impulses
  facet_grid(response ~ impulse, scales = "free_y", 
             labeller = labeller(impulse = label_map, response = label_map)) +
  # Theme and Labels
  theme_bw() +
  labs(
    title = "Impulse Response Functions (Cumulative Levels)",
    subtitle = "Analysis of Ind. Production, Fed Funds Rate, and CPI",
    caption = "Shaded area represents 95% Bootstrap Confidence Interval",
    x = "Horizon (Months)",
    y = "Cumulative Response"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),       
    strip.background = element_rect(fill = "#f0f0f0", color = "black")
  )

