############################################################
# Predictive Analytics — Assignment 1
# Group: [Casper Egon Hansen (S161795), Markus Emil Skifter (S160645), Sebastian Brostrup (S161547)]
############################################################

############################
# 0) Setup: packages + data
############################

library(AER)       # USMacroG
library(ggplot2)   # plotting
library(forecast)  # ggAcf, ggPacf, ggseasonplot, ggsubseriesplot
library(tseries)   # adf.test (optional)
library(fpp3)      # olympic_running + model/forecast workflow

# Load macro dataset (Questions 1–2)
data("USMacroG", package = "AER")

# Extract GDP as a time-series
gdp_ts <- USMacroG[, "gdp"]


############################################################
# Question 1
# Create two plots: GDP series and its autocorrelation (ACF).
############################################################

# --- 1.1 GDP time plot ---
gdp_df <- data.frame(
  time = as.numeric(time(gdp_ts)),
  gdp  = as.numeric(gdp_ts)
)

p1_gdp <- ggplot(gdp_df, aes(x = time, y = gdp)) +
  geom_line() +
  labs(
    title = "US Real GDP (quarterly, 1950–2000)",
    x = "Year",
    y = "Real GDP (billion USD)"
  )
print(p1_gdp)

# --- 1.2 GDP autocorrelation plot (ACF) ---
p1_acf <- ggAcf(gdp_ts, lag.max = 100) +
  labs(title = "ACF: US Real GDP (levels)")
print(p1_acf)

# (Write-up guidance for your report, not code):
# - GDP levels typically show strong upward trend and very high persistence.
# - ACF usually decays very slowly (often near 1 at many lags), consistent with non-stationarity.
# - A transformation (log and/or differencing) often makes sense to remove trend and stabilize variance.


############################################################
# Question 2
# Create QoQ GDP growth, plot it + ACF, comment trend/seasonality/cycle.
# Perform a statistical test for autocorrelation.
############################################################

# --- 2.1 QoQ growth rate (log-difference), in percent ---
# growth_t = 100*(log(GDP_t) - log(GDP_{t-1}))
gdp_growth_ts <- 100 * diff(log(gdp_ts))

gdp_growth_df <- data.frame(
  time   = as.numeric(time(gdp_growth_ts)),
  growth = as.numeric(gdp_growth_ts)
)

# --- 2.2 Plot growth series ---
p2_growth <- ggplot(gdp_growth_df, aes(x = time, y = growth)) +
  geom_line() +
  labs(
    title = "US GDP QoQ growth (log-difference, %)",
    x = "Year",
    y = "Growth rate (%)"
  )
print(p2_growth)

# --- 2.3 ACF of growth ---
p2_acf <- ggAcf(gdp_growth_ts, lag.max = 100) +
  labs(title = "ACF: US GDP QoQ growth")
print(p2_acf)

# --- 2.4 (Optional but useful) Seasonality/cycle diagnostics ---
# Quarterly data: if there is seasonality, spikes often show at lags 4, 8, 12, ...
p2_season <- ggseasonplot(gdp_growth_ts, year.labels = TRUE) +
  labs(title = "Seasonal plot: US GDP QoQ growth")
print(p2_season)

p2_subseries <- ggsubseriesplot(gdp_growth_ts) +
  labs(title = "Subseries plot: US GDP QoQ growth")
print(p2_subseries)

# --- 2.5 Statistical test for autocorrelation ---
# Ljung–Box test: H0 = no autocorrelation up to chosen lag
lb_8  <- Box.test(gdp_growth_ts, lag = 8,  type = "Ljung-Box")
lb_12 <- Box.test(gdp_growth_ts, lag = 12, type = "Ljung-Box")

print(lb_8)
print(lb_12)

# (Optional) ADF test for stationarity of growth (often stationary)
# print(adf.test(gdp_growth_ts))

# (Write-up guidance for your report):
# - Growth series should look much less trending than GDP levels.
# - Seasonality: check lags 4, 8, 12 in ACF + seasonal/subseries plots.
# - Ljung-Box p-value < 0.05 => reject H0, evidence of autocorrelation.


############################################################
# Question 3 — Olympic running times (fpp3::olympic_running)
# a) Plot Time vs Year for each event
# b) Fit regression line, report average change per year
# c) Plot residuals vs Year
# d) Forecast 2020 + prediction intervals (and assumptions)
############################################################

# Load olympic data (fpp3)
olymp <- olympic_running

# --- 3a) Plot winning time against year for each event ---
p3a <- ggplot(olymp, aes(x = Year, y = Time, colour = Sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Length, scales = "free_y", ncol = 4) +
  labs(
    title = "Olympic running — Winning times by event",
    x = "Year",
    y = "Winning time (seconds)"
  )
print(p3a)

# --- 3b) Fit linear regression line for each event ---
# TSLM(Time ~ Year) fits a linear trend vs Year for each (Length, Sex) group.
fit <- olymp |> model(trend_model = TSLM(Time ~ Year))

# Average rate per year = coefficient on Year (seconds per year)
# Negative means improving (time decreases each year).
rate_per_year <- fit |> coef() |> filter(term == "Year") |> select(Length, Sex, estimate)
print(rate_per_year)

# Plot with fitted lines
p3b <- fit |>
  augment() |>
  ggplot(aes(x = Year, y = Time, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = .fitted)) +
  facet_wrap(~ Length, scales = "free_y", ncol = 4) +
  labs(
    title = "Olympic running — Linear trend fit (Time ~ Year)",
    x = "Year",
    y = "Winning time (seconds)"
  )
print(p3b)

# --- 3c) Residuals vs Year ---
p3c <- fit |>
  augment() |>
  ggplot(aes(x = Year, y = .resid, colour = Sex)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(Sex ~ Length, scales = "free_y", ncol = 4) +
  labs(
    title = "Residuals from linear trend model",
    x = "Year",
    y = "Residual (seconds)"
  )
print(p3c)

# (Optional) QQ-plot of residuals
p3c_qq <- fit |>
  augment() |>
  ggplot(aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(Sex ~ Length, scales = "free") +
  labs(
    title = "Residual QQ-plots by event",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  )
print(p3c_qq)

# --- 3d) Predict winning time for the 2020 Olympics + prediction intervals ---
# Forecast horizon: next Olympic after 2016 is 2020 -> h = 1 step ahead
fc <- fit |> forecast(h = 1)
print(fc)

# 95% prediction interval (hilo)
fc_95 <- fc |> hilo(level = 95)
print(fc_95)

# Multiple PI levels, unpack into columns
fc_pi <- fc |>
  hilo(level = c(90, 95, 99)) |>
  unpack_hilo(c(`90%`, `95%`, `99%`)) |>
  select(
    Length, Sex, .mean,
    `90%_lower`, `90%_upper`,
    `95%_lower`, `95%_upper`,
    `99%_lower`, `99%_upper`
  )
print(fc_pi)

# (Write-up assumptions for your report):
# 1) Linear trend continues into 2020 (no structural breaks).
# 2) Errors are independent over time (no autocorrelation in residuals).
# 3) Constant variance (homoskedasticity).
# 4) Residuals approximately normal (for the PI construction).
# If residual plots show curvature or changing variance, linear model may be questionable.
