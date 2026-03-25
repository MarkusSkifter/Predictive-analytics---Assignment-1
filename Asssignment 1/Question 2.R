######## Question 2 #########
# load required packages
library(AER)
library(ggplot2)
library(forecast)   # ggAcf, ggPacf, ggseasonplot, ggsubseriesplot
library(tseries)    # adf.test (optional)

# load data
data("USMacroG", package = "AER")

# define dataframe
df <- USMacroG

# extract GDP (time series object)
gdp_ts <- df[, "gdp"]

# ---------- 2.1: Create QoQ growth rate (log-difference) ----------
# Quarter-on-quarter growth in percent:
# 100 * (log(y_t) - log(y_{t-1}))
gdp_growth_ts <- 100 * diff(log(gdp_ts))

# create dataframe for ggplot
gdp_growth_df <- data.frame(
  time   = as.numeric(time(gdp_growth_ts)),
  growth = as.numeric(gdp_growth_ts)
)

# ---------- 2.2: Plot growth series and ACF ----------
# plot growth series
ggplot(gdp_growth_df, aes(x = time, y = growth)) +
  geom_line() +
  labs(
    title = "US GDP QoQ growth (log-difference, %)",
    x = "Year",
    y = "Growth rate (%)"
  )

# plot ACF
ggAcf(gdp_growth_ts, lag.max = 100) +
  labs(title = "ACF: US GDP QoQ growth")

# (optional) plot PACF if needed
# ggPacf(gdp_growth_ts, lag.max = 100) +
#   labs(title = "PACF: US GDP QoQ growth")

# ---------- 2.3: Check for seasonality and cycles ----------
# seasonal plot (quarterly data)
ggseasonplot(gdp_growth_ts, year.labels = TRUE) +
  labs(title = "Seasonal plot: US GDP QoQ growth")

# subseries plot
ggsubseriesplot(gdp_growth_ts) +
  labs(title = "Subseries plot: US GDP QoQ growth")

# ---------- 2.4: Portmanteau test for autocorrelation ----------
# Ljung-Box test for the first lags (8 or 12 typical for quarterly data)
Box.test(gdp_growth_ts, lag = 8,  type = "Ljung-Box")
Box.test(gdp_growth_ts, lag = 12, type = "Ljung-Box")