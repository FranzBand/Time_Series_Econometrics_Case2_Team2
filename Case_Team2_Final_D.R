#----Preparing data-----
#----Load data and packages
library(dplyr)
library(ggplot2)
library(vars)
library(bootUR)
library(stats)
library(reshape2)

data_full <- read.csv("2020-01.csv")#we use 2020-01 because
#it has the data for all months until December 2019
View(data_full)

data <- data_full[-1, ] #first row is skipped, so it's easier to work with the data
View(data)

#----get the data/ variables we want----
#Industrial Production Index
indpro <- data$INDPRO   #industrial production, id6 (from appendix of Fred MD)
trans_indpro <- diff(log(indpro)) #transformed indpro according to appendix

plot(indpro, main = "Ind prod Index",
     type = "l", xlab= "Months since Jan 1959", ylab= "index",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(indpro)), by = 60))


plot(trans_indpro, main = "transformed Ind prod Index",
     type= "l", xlab= "Months since Jan 1959", ylab= "index",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(trans_indpro)), by = 60))
#plot transformed according to the transformation code of the appendix

#federal funds rate
fedfunds <- data$FEDFUNDS  #effective federal funds rate, id84
trans_fedfunds <- diff(fedfunds)  #transformed fed rate
plot(fedfunds, main = "fed fund rates",
     type= "l", xlab= "Months since Jan 1959", ylab= "index",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(trans_fedfunds)), by = 60))

plot(trans_fedfunds, main = " trans fed fund rates",
     type ="l", xlab= "Months since Jan 1959", ylab= "index",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(trans_fedfunds)), by = 60))

#consumer price index
cpi <- data$CPIAUCSL  #id113

trans_cpi <- diff(diff(log(cpi))) #transformed according to appendix
trans_cpi_1 <- diff(log(cpi)) #transformed with only one diff

plot(cpi, main = "consumer price index", type= "l",
     xlab= "Months since Jan 1959", ylab= "index",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(cpi)), by = 60))

plot(trans_cpi_1, main = "transformed with one diff, cpi",
     xlab= "Months since Jan 1959", ylab= "index", type= "l",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(trans_cpi_1)), by = 60))

plot(trans_cpi, main = "transformed cpi",
     xlab= "Months since Jan 1959", ylab= "index", type= "l",
     xaxt= "n") #remove x-axis labeling
# Add custom x-axis ticks every 60 months
axis(1, at = seq(0, max(1:length(trans_cpi)), by = 60))

#----preparing data for VAR----
#see length of vectors
l_indpro <- length(trans_indpro)
l_indpro 

l_fedfunds <- length(trans_fedfunds)
l_fedfunds

l_cpi <- length(trans_cpi)
l_cpi

#get the min length of all vectors
min_length <- min(l_cpi, l_fedfunds, l_indpro)

trans_fedfunds <- trans_fedfunds[1:min_length]
trans_indpro <- trans_indpro[1:min_length]

#check new length
l_indpro <- length(trans_indpro)
l_indpro 

l_fedfunds <- length(trans_fedfunds)
l_fedfunds

l_cpi <- length(trans_cpi)
l_cpi

#----Estimating VAR lag order----
#put all data together
var_data <- cbind(trans_indpro, trans_fedfunds, trans_cpi)
View(var_data)
lag_selection <- VARselect(var_data, lag.max= 18,type ="const")  #check for info criteria
print(lag_selection$selection)  #VAR(3) or VAR(4) selected


#display VAR(3)
var_model <- VAR(var_data, p= 3, type = "const")
var_model
summary(var_model)

plot(var_model)

#----check for validity of var(3)-----
#stability check
#If all roots are smaller than 1, the VAR(3) model is stable
roots_test <- roots(var_model)
print(roots_test)
if(all(roots_test <1)){
  print("All roots are smaller than 1, the var(3) model is stable")
}else {
    print("At least one root is bigger than 1, thus the var(3) model is unstable")
}

#serial correlation test (Portmanteau Test)
#HO: No serial correlation 
serial_test <- serial.test(var_model, lags.pt = 12, type= "PT.asymptotic")
print(serial_test)
#we reject the H0 hypothesis, meaning that there is serial correlation.
#having chosen the lag order based on the BIC/ SC, it may happen that there is
#still information which is not detected by the model

#----Unit root test----
#----ur test for indpro-----
indpro_log <- log(indpro)
plot(indpro_log, type="l", main= "ind prod log")#plot without proper labeling of axis,
#because it's just to see if it needs "trend" or "intercept" for the adf

indpro_d2 <- diff(diff(indpro_log))
plot(indpro_d2, type="l", main="ind prod diff^2")
abline(h=0, col= "red")

indpro_d1 <- diff(indpro_log)
plot(indpro_d1, type="l", main="ind prod diff^1")
abline(h=0, col= "blue")
abline(h=0.005, col="red")

adf_indpro_2 <- adf(diff(indpro_log, differences=2), deterministics= "intercept")
print(adf_indpro_2)
#intercept, because no clear trend is visible in the plot.
#Reject H0 hypothesis, time series is stationary

adf_indpro_1 <- adf(diff(indpro_log, differences=1), deterministics= "intercept")
print(adf_indpro_1)
#intercept, because no clear trend is visible in the plot.
#Reject H0 hypothesis, time series is stationary

adf_indpro_0 <- adf(indpro_log, deterministics= "trend")
print(adf_indpro_0)
#trend, because a clear trend is visible in the plot.
#Fail to reject H0 hypothesis, time series has an unit root


#-----fedfunds unit root test-----
plot(fedfunds, type="l", main= "fed funds rate")#plot without proper labeling of axis,
#because it's just to see if it needs "trend" or "intercept" for the adf

fedfunds_d2 <- diff(diff(fedfunds))
plot(fedfunds_d2, type="l", main="fed funds diff^2")
abline(h=0, col= "blue")

fedfunds_d1 <- diff(fedfunds)
plot(fedfunds_d1, type="l", main="fed funds diff^1")
abline(h=0, col= "blue")

adf_fedfunds_2 <- adf(diff(fedfunds, differences=2), deterministics= "intercept")
print(adf_fedfunds_2)
#intercept, because no clear trend is visible in the plot.
#Reject H0 hypothesis, time series is stationary

adf_fedfunds_1 <- adf(diff(fedfunds, differences=1), deterministics= "intercept")
print(adf_fedfunds_1)
#intercept, because no clear trend is visible in the plot.
#Reject H0 hypothesis, time series is stationary

adf_fedfunds_0 <- adf(fedfunds, deterministics= "trend")
print(adf_fedfunds_0)
#trend, because a clear trend is visible in the plot
#Fail to reject H0 hypothesis, time series has an unit root

#----cpi unit root test-----
plot(cpi, type="l", main= "consumer price index")#plot without proper labeling of axis,
#because it's just to see if it needs "trend" or "intercept" for the adf

cpi_d2 <- diff(diff(cpi))
plot(cpi_d2, type="l", main="consumer price index diff^2")
abline(h=0, col= "blue")

cpi_d1 <- diff(cpi)
plot(cpi_d1, type="l", main="consumer price index diff^1")
abline(h=0, col= "blue")
abline(h=0.25, col= "red")
abline(h=0.5, col= "green")

adf_cpi_2 <- adf(diff(cpi, differences=2), deterministics= "intercept")
print(adf_cpi_2)
#intercept, because no clear trend is visible in the plot.
#reject h0 hypothesis, series is stationary of order I(2)

adf_cpi_1 <- adf(diff(cpi, differences=1), deterministics= "intercept")
print(adf_cpi_1)
#intercept, because no clear trend is visible in the plot.
#reject the h0 hypothesis, series is stationary of order I(1)

adf_cpi_0 <- adf(cpi, deterministics= "trend")
print(adf_cpi_0)
#trend, because a clear trend is visible in the plot.
#not possible to reject the H0 hypothesis, time series has a unit root


#----- Granger causality tests using vars::causality() ----
#Multivariate Granger causality tests
# Does the Indrpro cause the others?
causality(var_model, cause = "trans_indpro")

# Does the Federal Funds Rate cause the others?
causality(var_model, cause = "trans_fedfunds")

# Does the CPI cause the others?
causality(var_model, cause = "trans_cpi")

cat("H0: The lags of the variable named in 'cause' DO NOT help predict the others (no Granger causality).\n")
cat("H1: At least one lag of the 'cause' variable DOES help predict the others (there IS Granger causality).\n")

# ---------  Function for bivariate Granger causality test --------- 

# bivariate Granger test (X -> Y) 
# y_name : name of dependent variable (string)
# x_name : name of "cause" variable (string)
# p      : number of lags in our case 3
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

# All six bivariate Granger tests:                              Arrow = Granger causes
biv_granger("trans_fedfunds", "trans_indpro", 3, var_data)      # INDPRO -> FEDFUNDS
biv_granger("trans_cpi", "trans_indpro", 3, var_data)           # INDPRO -> CPI
biv_granger("trans_indpro", "trans_fedfunds", 3, var_data)      # FEDFUNDS -> INDPRO
biv_granger("trans_cpi", "trans_fedfunds", 3, var_data)         # FEDFUNDS -> CPI
biv_granger("trans_indpro", "trans_cpi", 3, var_data)           # CPI -> INDPRO
biv_granger("trans_fedfunds", "trans_cpi", 3, var_data)         # CPI -> FEDFUNDS


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
  runs     = 500,      
  ortho    = FALSE
)

plot(irf_rf_boot)

# Helper functions for cumulation
cum_diff1 <- function(x) apply(x, 2, cumsum)                    # for diff and diff log
cum_diff2 <- function(x) apply(apply(x, 2, cumsum), 2, cumsum)  # for diff diff log

# We start from the bootstrap IRFs
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


# -----PLOT FOR REDUCED-FORM POINT ESTIMATES (NO CI)-----

# 1. Convert Point Estimate Object to Data Frame
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
plot_data_point <- extract_point_irf(irf_rf_point)

# 3. Labels
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_fedfunds" = "Fed Funds Rate", 
  "trans_cpi"      = "CPI"
)

# 4. Generate the Plot
ggplot(plot_data_point, aes(x = horizon, y = irf)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 0.5) +
  
  # Grid Layout: Rows = Responses, Cols = Impulses
  facet_grid(response ~ impulse, scales = "free_y", 
             labeller = labeller(impulse = label_map, response = label_map)) +
  
  # Theme
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


#------ PLOT FOR BOOTSTRAPPED IRFs (WITH 95% CI)-----

# 1. Convert Bootstrapped IRF Object to Data Frame
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

# 2. Extract Data from our existing object 'irf_rf_boot'
plot_data_boot <- extract_boot_irf(irf_rf_boot)

# 3. Labels
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_fedfunds" = "Fed Funds Rate", 
  "trans_cpi"      = "CPI"
)

# 4. Generate the Plot
ggplot(plot_data_boot, aes(x = horizon, y = irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 0.5) +
  
  # Grid Layout: Rows = Responses, Cols = Impulses
  facet_grid(response ~ impulse, scales = "free_y", 
             labeller = labeller(impulse = label_map, response = label_map)) +
  
  # Theme
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



# -----Impulse Response Functions (Cumulative Levels)-----

# Convert IRF Object to Tidy Data Frame
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

# 1. Get data from the calculated levels object
plot_data <- extract_irf_df(irf_levels) 

# 2. Labels
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_fedfunds" = "Fed Funds Rate", 
  "trans_cpi"      = "CPI (Price level)"
)

# 3. Grid Plot
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

#-----SVAR models----
#-----new ordering of data-----
#var_data_new has the order: indpro -> cpi -> fed funds
svar_data <- var_data[, c(1,3,2)]
lag_selection_new <- VARselect(svar_data, lag.max= 18, type ="const")  #check for info criteria
print(lag_selection_new$selection)  #VAR(3) is selected
svar_model<- VAR(svar_data, p=3, type="const")

summary(svar_model)
plot(svar_model)

#----check for validity of svar(3)-----

#stability check
#If all roots are smaller than 1, the VAR(3) model is stable
roots_test_svar <- roots(svar_model)
print(roots_test_svar)
if(all(roots_test_svar <1)){
  print("All roots are smaller than 1, the svar(3)_2 model is stable")
}else {
  print("At least one root is bigger than 1, thus the svar(3)_2 model is unstable")
}

#serial correlation test (Portmanteau Test)
#HO: No serial correlation 
serial_test_svar <- serial.test(svar_model, lags.pt = 12, type= "PT.asymptotic")
print(serial_test_svar)
#we reject the H0 hypothesis, meaning that there is serial correlation.
#having chosen the lag order based on the BIC/ SC, it may happen that there is
#still information which is not detected by the model

#-----impulsive response functions for new order-----
set.seed(123)
irf_svar <- irf(svar_model, n.ahead = 24, boot= TRUE, ci= 0.95, runs=500, ortho = TRUE)
plot(irf_svar)
#ortho= TRUE <--- THIS IS THE KEY DIFFERENCE (Structural Shocks)

#-----new part for svar-----

# 1. Define the Correct Recursive Order
# Theory (Bernanke et al. 2005): 
# Slow-Moving (Prod, Prices) -> Policy (Fed Funds)
# This assumes the Fed reacts instantly to the economy, but the economy reacts with a lag.

# ortho = TRUE enables the Cholesky decomposition based on the column order



#----plotting of irf for svar----
# Helper function to extract data for ggplot
extract_boot_irf <- function(irf_object) {
  impulse_names <- names(irf_object$irf)
  df_list <- list()
  
  for (imp in impulse_names) {
    pe <- as.data.frame(irf_object$irf[[imp]])
    lo <- as.data.frame(irf_object$Lower[[imp]])
    up <- as.data.frame(irf_object$Upper[[imp]])
    
    pe$horizon <- lo$horizon <- up$horizon <- 0:(nrow(pe)-1)
    pe$impulse <- lo$impulse <- up$impulse <- imp
    
    pe_long <- melt(pe, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "irf")
    lo_long <- melt(lo, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "lower")
    up_long <- melt(up, id.vars = c("horizon", "impulse"), variable.name = "response", value.name = "upper")
    
    merged <- merge(pe_long, lo_long, by=c("horizon", "impulse", "response"))
    merged <- merge(merged, up_long, by=c("horizon", "impulse", "response"))
    df_list[[imp]] <- merged
  }
  return(do.call(rbind, df_list))
}

# Extract and Label
plot_data_svar <- extract_boot_irf(irf_svar)

# Create nice labels corresponding to the new order
label_map <- c(
  "trans_indpro"   = "Ind. Production", 
  "trans_cpi"      = "CPI",
  "trans_fedfunds" = "Fed Funds Rate"
)

# Generate Plot
ggplot(plot_data_svar, aes(x = horizon, y = irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "firebrick", alpha = 0.3) + # Different color for SVAR
  geom_line(color = "firebrick", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  
  facet_grid(response ~ impulse, scales = "free_y", 
             labeller = labeller(impulse = label_map, response = label_map)) +
  
  theme_bw() +
  labs(
    title = "Structural Impulse Response Functions (SVAR)",
    subtitle = "Identified via Cholesky Ordering: IndPro -> CPI -> FedFunds",
    caption = "Note: Fed Funds responds instantly to others; Others respond with lag.",
    x = "Horizon (Months)",
    y = "Structural Response"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "#f0f0f0", color = "black")
  )
