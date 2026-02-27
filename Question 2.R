# --- Packages ---
library(AER)
library(ggplot2)
library(forecast)   # ggAcf
library(lmtest)     # dwtest, Box.test
library(tseries)    # adf.test (valgfrit men ofte nyttigt)

# --- Load data ---
data("USMacroG", package = "AER")
df <- USMacroG

# GDP as a ts object
gdp_ts <- df[, "gdp"]

# =========================================================
# 1) Create QoQ growth rate series for GDP
# =========================================================
# Option A (recommended): log-growth (approx. percent growth)
gdp_gr_ts <- 100 * diff(log(gdp_ts))   # QoQ growth in %

# Option B: simple percent change (if you prefer exact %)
# gdp_gr_ts <- 100 * (gdp_ts / stats::lag(gdp_ts, -1) - 1)
# gdp_gr_ts <- na.omit(gdp_gr_ts)

# Make a data frame for ggplot
gdp_gr_df <- data.frame(
  time   = as.numeric(time(gdp_gr_ts)),
  gdp_gr = as.numeric(gdp_gr_ts)
)

# =========================================================
# 2) Plot growth series + ACF, and comment on trend
# =========================================================
# Plot growth series
ggplot(gdp_gr_df, aes(x = time, y = gdp_gr)) +
  geom_line() +
  labs(
    title = "US GDP QoQ growth (log-diff, %)",
    x = "Year",
    y = "Growth rate (%)"
  )

# ACF of growth series
ggAcf(gdp_gr_ts, lag.max = 100) +
  labs(title = "ACF: US GDP QoQ growth")

# (Optional) Also show PACF if you want
ggPacf(gdp_gr_ts, lag.max = 100) + labs(title = "PACF: US GDP QoQ growth")

# =========================================================
# 3) Seasonality / cycle checks
# =========================================================
# With quarterly data, seasonality often shows up at lags 4, 8, 12, ...
# You can inspect ACF peaks at multiples of 4 (already in ACF plot),
# and also do a simple seasonal plot:

# Convert to ts explicitly (freq should already be 4, but just to be safe)
gdp_gr_ts <- ts(gdp_gr_ts, start = start(gdp_ts) + c(0,1), frequency = 4)

# Seasonal plot (quarter-of-year patterns)
ggseasonplot(gdp_gr_ts, year.labels = TRUE) +
  labs(title = "Seasonal plot: US GDP QoQ growth")

# Another quick view: subseries plot
ggsubseriesplot(gdp_gr_ts) +
  labs(title = "Subseries plot: US GDP QoQ growth")

# =========================================================
# 4) Statistical test for autocorrelation
# =========================================================
# (a) Ljung-Box test (common for autocorrelation in a series)
# Test up to lag 8 or 12 (quarterly: 8 or 12 is typical)
Box.test(gdp_gr_ts, lag = 8,  type = "Ljung-Box")
Box.test(gdp_gr_ts, lag = 12, type = "Ljung-Box")

# (b) Durbin-Watson on a mean-only model (tests AR(1)-type autocorrelation)
m0 <- lm(gdp_gr_ts ~ 1)
dwtest(m0)

# (c) Breusch-Godfrey test (more general, tests up to order p)
# Here with p = 4 (one year of quarterly lags) and p = 8 as alternative
bgtest(m0, order = 4)
bgtest(m0, order = 8)

# (Optional but often helpful) Stationarity check on growth rates
adf.test(gdp_gr_ts)
