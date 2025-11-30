library(dplyr)
library(ggplot2)
library(vars)
#-------------------------
# 1. Load data
#-------------------------
data_full <- read.csv("2020-01.csv")

# first row contains the transformation codes, so drop it
data <- data_full[-1, ]

#-------------------------
# 2. Extract raw series
#-------------------------
indpro   <- as.numeric(data$INDPRO)    # industrial production, tcode = 5
fedfunds <- as.numeric(data$FEDFUNDS)  # federal funds rate, tcode = 2
cpiaucsl <- as.numeric(data$CPIAUCSL)  # CPI, tcode = 6

#-------------------------
# 3. Transformations (McCracken–Ng tcodes)
#   5: diff(log x)
#   2: diff(x)
#   6: diff(diff(log x))
#-------------------------
trans_indpro   <- diff(log(indpro))
trans_fedfunds <- diff(fedfunds)
trans_cpiaucsl <- diff(diff(log(cpiaucsl)))

# simple plots
plot(indpro, main = "Industrial Production Index")
plot(trans_indpro, main = "Transformed INDPRO (diff log)")

plot(fedfunds, main = "Fed Funds Rate")
plot(trans_fedfunds, main = "Transformed FEDFUNDS (diff level)")

plot(cpiaucsl, main = "Consumer Price Index")
plot(trans_cpiaucsl, main = "Transformed CPI (second diff of log)")

boxplot(fedfunds, main ="Boxplot FEDFUNDS")
boxplot(trans_fedfunds, main ="Boxplot transformed FEDFUNDS")

#-------------------------
# 4. Align lengths and build VAR dataset
#-------------------------
min_len <- min(length(trans_indpro),
               length(trans_fedfunds),
               length(trans_cpiaucsl))

trans_indpro_al   <- tail(trans_indpro,   min_len)
trans_fedfunds_al <- tail(trans_fedfunds, min_len)
trans_cpiaucsl_al <- tail(trans_cpiaucsl, min_len)

var_data <- cbind(trans_indpro_al,
                  trans_fedfunds_al,
                  trans_cpiaucsl_al)

colnames(var_data) <- c("trans_indpro",
                        "trans_fedfunds",
                        "trans_cpiaucsl")

#-------------------------
# 5. Lag selection
#-------------------------
lag_selection <- VARselect(var_data, lag.max = 12, type = "const")
print(lag_selection$selection)   # check which p each criterion prefers

#-------------------------
# 6. Estimate VAR (example: p = 3)
#-------------------------
var_model <- VAR(var_data, p = 3, type = "const")
summary(var_model)

# diagnostic plots: fit, residuals, ACF/PACF of residuals
plot(var_model)

#Optional
serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")  # autocorr
arch.test(var_model, lags.multi = 12)                         # ARCH / heterosk.
normality.test(var_model)                                     # residual normality

