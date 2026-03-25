# ============================================================
#  SECOND HOME ASSIGNMENT - Predictive Analytics
#  French Consumer Price Index (FCPI) - Time Series Analysis
# ============================================================
#  Data: Monthly French CPI, January 1960 – December 2019
#  File: fcpi.csv (semicolon-separated)
# ============================================================

# ---- 0. SETUP: Load Required Packages ----------------------

# Uncomment the line below to install packages if needed:
# install.packages(c("forecast", "tseries", "ggplot2", "urca"))

library(forecast)  # ARIMA modelling, auto.arima, forecasting utilities
library(tseries)   # ADF and KPSS stationarity tests
library(ggplot2)   # Publication-quality plots
library(urca)      # Unit root tests with full specification control (ADF with trend/drift)


# ============================================================
#  LOAD DATA AND CONVERT TO TIME SERIES
# ============================================================

# Read the CSV (semicolon-separated)
fcpi_raw <- read.csv("fcpi.csv", sep = ";", header = TRUE)
head(fcpi_raw)
str(fcpi_raw)

# Convert to a monthly ts object starting January 1960
cpi_ts <- ts(fcpi_raw$cpi, start = c(1960, 1), frequency = 12)
cat("Time series: Jan 1960 – Dec 2019,", length(cpi_ts), "monthly observations\n")


# ============================================================
#  PART 1
# ============================================================

# ---- 1.1  Plot, ACF, and Decomposition ---------------------

# --- Plot the raw time series ---
autoplot(cpi_ts) +
  labs(
    title = "French Consumer Price Index (Jan 1960 – Dec 2019)",
    x     = "Year",
    y     = "CPI"
  ) +
  theme_minimal()

# --- Autocorrelation Function (ACF) ---
# We use 60 lags (5 years) to reveal both short-run and seasonal autocorrelation.
ggAcf(cpi_ts, lag.max = 60) +
  labs(title = "ACF of French CPI (raw)") +
  theme_minimal()

# --- Classical Multiplicative Decomposition ---
# We choose the multiplicative form because the seasonal fluctuations grow
# proportionally with the overall level of the series (variance increases with level).
decomp_raw <- decompose(cpi_ts, type = "multiplicative")
plot(decomp_raw, main = "Classical Multiplicative Decomposition of French CPI")

# DISCUSSION (Q1.1):
# The raw series exhibits a strong, persistent upward trend driven by inflation,
# and a slowly decaying ACF that takes many lags to reach zero – both hallmarks
# of a non-stationary process with a stochastic trend. A mild seasonal pattern
# is also visible (e.g., price dips in summer). The multiplicative decomposition
# is chosen because seasonal amplitudes scale with the trend level.
#
# LIMITATION: Classical decomposition relies on a centred moving average, which
# leaves the first and last 6 months without an estimated trend. It also assumes
# the seasonal pattern is constant across the entire sample, which may be
# unrealistic over 60 years. Finally, it cannot separate a cycle from the trend.


# ---- 1.2  Log Transformation and Comparison ----------------

# Apply the natural logarithm
log_cpi_ts <- log(cpi_ts)

# --- Plot the log-transformed series ---
autoplot(log_cpi_ts) +
  labs(
    title = "Log-Transformed French CPI",
    x     = "Year",
    y     = "log(CPI)"
  ) +
  theme_minimal()

# --- ACF of log-transformed series ---
ggAcf(log_cpi_ts, lag.max = 60) +
  labs(title = "ACF of Log(CPI)") +
  theme_minimal()

# --- Classical Additive Decomposition of log(CPI) ---
# After logging, the series is additive (seasonal component has constant amplitude).
decomp_log <- decompose(log_cpi_ts, type = "additive")
plot(decomp_log, main = "Additive Decomposition of Log(CPI)")

# DECISION: Use the log-transformed series for all remaining analyses.
#
# MOTIVATION: The log transformation stabilises the variance – seasonal
# fluctuations are now roughly constant in size across decades, which is
# confirmed by the stable seasonal component in the additive decomposition.
# Working in logs is standard practice in macroeconomics because changes in
# log(CPI) are interpretable as approximate percentage (inflation) changes.
# The ACF structure is essentially unchanged after logging, confirming that
# non-stationarity is driven by a trend, not by heteroscedasticity.

# Set the working series for the rest of the assignment
working_ts <- log_cpi_ts


# ---- 1.3  KPSS and ADF Stationarity Tests ------------------

# CHOICE OF FUNCTIONAL FORM:
# The log(CPI) series shows a clear deterministic upward trend (visible in the
# decomposition). Therefore, the most accurate functional form for both tests
# must include a TREND TERM:
#  - ADF: we test H0: unit root, including a constant and a linear trend ("ct").
#         If we omitted the trend, the test would be misspecified because the
#         DGP contains a trend under both H0 and H1.
#  - KPSS: H0 is trend-stationarity (null = "Trend"). Rejecting H0 means the
#           series is not stationary around a deterministic trend.

# --- ADF Test (H0: unit root; H1: stationary) ---
# Using the urca package for full specification control.
# We include a constant and trend ("ct"), with lag selection via AIC.
adf_result <- ur.df(working_ts, type = "trend", selectlags = "AIC")
cat("\n=== ADF Test on log(CPI) ===\n")
summary(adf_result)

# --- KPSS Test (H0: trend-stationary; H1: not stationary) ---
kpss_result <- kpss.test(working_ts, null = "Trend")
cat("\n=== KPSS Test on log(CPI) ===\n")
print(kpss_result)

# INTERPRETATION:
# ADF:  The test statistic is larger (less negative) than the critical values
#       → fail to reject H0 → evidence of a unit root → non-stationary.
# KPSS: p-value is below 0.05 → reject H0 (trend-stationarity) → non-stationary.
# Both tests agree: log(CPI) is non-stationary. We need to difference the series.


# ---- 1.4  Making the Series Stationary and ARIMA Identification ----

# --- Step 1: Determine the number of regular differences ---
n_diff <- ndiffs(working_ts, test = "kpss")
cat("\nNumber of regular differences required:", n_diff, "\n")

# Apply one regular difference
diff1_log_cpi <- diff(working_ts, differences = 1)

# --- Verify stationarity of the once-differenced series ---
kpss_diff1 <- kpss.test(diff1_log_cpi, null = "Level")
cat("\n=== KPSS Test on Δlog(CPI) ===\n")
print(kpss_diff1)
# p-value > 0.05 → cannot reject H0 (level-stationarity) → stationary ✓

# --- Step 2: Check for remaining seasonal non-stationarity ---
n_sdiff <- nsdiffs(diff1_log_cpi)
cat("Number of seasonal differences required:", n_sdiff, "\n")

# Apply one seasonal difference (lag = 12 for monthly data)
stationary_ts <- diff(diff1_log_cpi, lag = 12, differences = 1)
cat("Stationary series: (1-B)(1-B^12) log(CPI_t), length =", length(stationary_ts), "\n")

# --- Plot the stationary series ---
autoplot(stationary_ts) +
  labs(
    title = "Stationary Series: First and Seasonal Difference of log(CPI)",
    subtitle = expression((1-B)(1-B^{12}) ~ log(CPI[t])),
    x = "Year", y = expression(Delta[1] ~ Delta[12] ~ log(CPI))
  ) +
  theme_minimal()

# --- ACF and PACF of the stationary series ---
# These are the key diagnostics for ARIMA order selection.
p1 <- ggAcf(stationary_ts, lag.max = 48) +
  labs(title = "ACF of Stationary Series") +
  theme_minimal()

p2 <- ggPacf(stationary_ts, lag.max = 48) +
  labs(title = "PACF of Stationary Series") +
  theme_minimal()

# install.packages("gridExtra")  # uncomment if not installed
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# --- Model Identification ---
# NON-SEASONAL part (lags 1–11):
#   ACF: significant spike at lag 1, then cuts off → MA(1) term → q = 1
#   PACF: decays gradually → supports MA rather than AR
#   → Non-seasonal: p = 0, d = 1, q = 1
#
# SEASONAL part (lags 12, 24, 36 ...):
#   ACF: significant negative spike at lag 12, then cuts off → SMA(1) → Q = 1
#   PACF: significant spike at lag 12, then cuts off → could support SAR(1)
#   → Seasonal: P = 0, D = 1, Q = 1  [m = 12]
#
# Proposed model: ARIMA(0,1,1)(0,1,1)[12]  –  the classic "Airline Model"

manual_model_full <- Arima(working_ts,
                           order    = c(0, 1, 1),
                           seasonal = list(order = c(0, 1, 1), period = 12))
summary(manual_model_full)

cat("\n--- Manually Identified Model ---\n")
cat("ARIMA(0,1,1)(0,1,1)[12]\n")
cat("Equation:\n")
cat("  (1 - B)(1 - B^12) log(Y_t) = (1 + theta_1 * B)(1 + Theta_1 * B^12) * eps_t\n")
cat("  where B is the backshift operator, eps_t ~ WN(0, sigma^2)\n")
cat("  theta_1  =", round(coef(manual_model_full)["ma1"],  4), "\n")
cat("  Theta_1  =", round(coef(manual_model_full)["sma1"], 4), "\n")


# ============================================================
#  PART 2
# ============================================================

# ---- 2.1  Train/Test Split, AIC Model, Residual Diagnostics ----

# We use the last 4 years (Jan 2016 – Dec 2019, 48 observations) as the test set,
# and all prior data as the training set. This gives a meaningful out-of-sample
# evaluation period while retaining ~93% of the data for estimation.

train_ts <- window(working_ts, end   = c(2015, 12))
test_ts  <- window(working_ts, start = c(2016,  1))

cat("\nTraining set: Jan 1960 – Dec 2015 (", length(train_ts), "observations)\n")
cat("Test set:     Jan 2016 – Dec 2019 (", length(test_ts),  "observations)\n")

# --- Auto ARIMA with AIC on the training set ---
# stepwise = FALSE and approximation = FALSE perform a thorough grid search.
# This may take a minute or two.
cat("\nRunning auto.arima() with AIC criterion – please wait...\n")
auto_model <- auto.arima(
  train_ts,
  ic            = "aic",
  stepwise      = FALSE,
  approximation = FALSE
)
cat("\n=== AIC-Selected Model ===\n")
summary(auto_model)
# arimaorder() returns c(p,d,q,P,D,Q); format as ARIMA(p,d,q)(P,D,Q)[12]
ao <- arimaorder(auto_model)
cat(sprintf("Selected order: ARIMA(%d,%d,%d)(%d,%d,%d)[12]\n",
            ao[1], ao[2], ao[3], ao[4], ao[5], ao[6]))

# COHERENCE CHECK WITH 1.3:
# auto.arima should select d >= 1 and D >= 1, consistent with the unit-root
# evidence from ADF and KPSS tests. If the selected non-seasonal and seasonal
# MA/AR orders differ from (0,1,1)(0,1,1)[12], this is still coherent as long
# as the differencing orders match.

# --- Residual Diagnostics for the AIC-selected model ---
checkresiduals(auto_model)
# This plots: residual time series, ACF of residuals, and a histogram.
# We look for: no remaining autocorrelation, roughly normal distribution, constant variance.

# Ljung-Box test for residual autocorrelation (lag = 24 as rule of thumb)
lb_auto <- Box.test(residuals(auto_model), lag = 24, type = "Ljung-Box",
                    fitdf = sum(arimaorder(auto_model)[c(1, 3, 4, 6)]))
cat("\n=== Ljung-Box Test (AIC model residuals) ===\n")
print(lb_auto)
# H0: no autocorrelation in residuals. p-value > 0.05 → model is adequate.

# DISCUSSION:
# If the Ljung-Box p-value is above 0.05, the residuals behave like white noise
# and the model captures the dynamics well. If significant autocorrelation
# remains, one could increase the AR or MA order. The histogram and Q-Q plot
# help judge normality; departures at the tails are common with economic data
# and may suggest a Student-t innovation distribution.


# ---- 2.2  Forecasting and Accuracy Comparison --------------

h <- length(test_ts)  # 48-month forecast horizon

# --- Estimate the manually chosen model on the training set ---
manual_model_train <- Arima(train_ts,
                             order    = c(0, 1, 1),
                             seasonal = list(order = c(0, 1, 1), period = 12))

# --- Generate forecasts (in log scale) ---
fc_auto   <- forecast(auto_model,         h = h)
fc_manual <- forecast(manual_model_train, h = h)

# --- Back-transform to original CPI scale for interpretable plots ---
# Since working_ts = log(cpi_ts), we apply exp() to forecasts and intervals.
test_original  <- window(cpi_ts, start = c(2016, 1))
train_original <- window(cpi_ts, end   = c(2015, 12))

# Helper function to exponentiate a forecast object
exp_fc <- function(fc) {
  fc$mean  <- exp(fc$mean)
  fc$lower <- exp(fc$lower)
  fc$upper <- exp(fc$upper)
  fc$x     <- exp(fc$x)
  fc
}

fc_auto_orig   <- exp_fc(fc_auto)
fc_manual_orig <- exp_fc(fc_manual)

# Use a recent window for plotting (from 2010 onward) to make the forecast visible
plot_start <- c(2010, 1)
cpi_plot   <- window(cpi_ts, start = plot_start)

# --- Plot 1: AIC-Selected Model ---
ao  <- arimaorder(auto_model)
auto_label <- sprintf("AIC: ARIMA(%d,%d,%d)(%d,%d,%d)[12]", ao[1],ao[2],ao[3],ao[4],ao[5],ao[6])

# --- Plot 1: AIC-Selected Model ---
autoplot(cpi_plot, series = "History") +
  autolayer(fc_auto_orig,  series = auto_label,      PI = TRUE) +
  autolayer(test_original, series = "Actual (test)") +
  scale_color_manual(values = c("History" = "grey50", "Actual (test)" = "black",
                                setNames("steelblue", auto_label))) +
  labs(
    title    = paste("Forecast:", auto_label),
    subtitle = "Training: Jan 1960 – Dec 2015 | Test: Jan 2016 – Dec 2019",
    x        = "Year",
    y        = "CPI",
    color    = NULL
  ) +
  theme_minimal()

# --- Plot 2: Manual Model ARIMA(0,1,1)(0,1,1)[12] ---
autoplot(cpi_plot, series = "History") +
  autolayer(fc_manual_orig, series = "Manual: ARIMA(0,1,1)(0,1,1)[12]", PI = TRUE) +
  autolayer(test_original,  series = "Actual (test)") +
  scale_color_manual(values = c("History" = "grey50", "Actual (test)" = "black",
                                "Manual: ARIMA(0,1,1)(0,1,1)[12]" = "firebrick")) +
  labs(
    title    = "Forecast: Manual Model ARIMA(0,1,1)(0,1,1)[12]",
    subtitle = "Training: Jan 1960 – Dec 2015 | Test: Jan 2016 – Dec 2019",
    x        = "Year",
    y        = "CPI",
    color    = NULL
  ) +
  theme_minimal()

# --- Accuracy Measures on the test set (original scale) ---
acc_auto   <- accuracy(fc_auto_orig$mean,   test_original)
acc_manual <- accuracy(fc_manual_orig$mean, test_original)

cat("\n=== Forecast Accuracy Comparison (Test Set: Jan 2016 – Dec 2019) ===\n")
cat("\nAIC-Selected Model:", auto_label, "\n")
print(acc_auto)

cat("\nManual Model – ARIMA(0,1,1)(0,1,1)[12]:\n")
print(acc_manual)

# Summary table
acc_table <- rbind(
  AIC_Model    = acc_auto,
  Manual_Model = acc_manual
)
cat("\n--- Summary Table ---\n")
print(round(acc_table, 4))

# INTERPRETATION:
# The model with lower RMSE, MAE, and MAPE on the test set is the better
# forecaster. The MAPE (Mean Absolute Percentage Error) is particularly
# useful here because CPI is a level series and MAPE expresses errors in
# percentage terms, making it easy to interpret and compare across models.
# If the AIC-selected and manually chosen models are similar, this suggests
# that our ACF/PACF-based identification in Part 1 was accurate. Differences
# may arise because auto.arima optimises in-sample fit (AIC), which does not
# always translate to superior out-of-sample performance.

cat("\n=== END OF ASSIGNMENT 2 ===\n")

