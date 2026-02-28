# --- Packages ---
library(fpp3)
library(ggplot2)
library(dplyr)

# --- Load data ---
data <- olympic_running

# =========================================================
# a) Plot winning time against year for each event
# =========================================================

ggplot(data, aes(x = Year, y = Time, colour = Sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Length, scales = "free_y", ncol = 4) +
  labs(
    title  = "Olympic Running – Winning Times by Event",
    x      = "Year",
    y      = "Winning Time (seconds)"
  ) +
  theme_minimal()

# Main features:
# - All events show a clear downward trend: winning times have been
#   decreasing steadily over the century.
# - Women's events begin later (1928+) and start from higher times,
#   but exhibit steeper declines than men's events.
# - Some years are missing (no Olympics during WWI/WWII: 1916, 1940, 1944).
# - Short sprints (100m, 200m) show very small absolute improvements;
#   long-distance events (5000m, 10000m) show much larger absolute drops.
# - The rate of improvement appears roughly linear for most events
#   over the observed period.


# =========================================================
# b) Fit a regression line for each event; report avg rate per year
# =========================================================

# Fit OLS: Time ~ Year separately for each (Length, Sex) combination
models <- data %>%
  group_by(Length, Sex) %>%
  do(model = lm(Time ~ Year, data = .))

# Extract the Year coefficient (seconds per year) for each event
rates <- models %>%
  summarise(
    Length = Length,
    Sex    = Sex,
    rate_per_year = coef(model)["Year"]
  )

print(rates)
# Negative coefficients confirm decreasing times.
# E.g. for Men's 100m the slope is approximately −0.012 sec/year.
# Longer events have steeper (more negative) slopes in absolute terms.

# Overlay fitted lines on the scatter plot
ggplot(data, aes(x = Year, y = Time, colour = Sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ Length, scales = "free_y", ncol = 4) +
  labs(
    title  = "Olympic Running – Winning Times with Linear Trend",
    x      = "Year",
    y      = "Winning Time (seconds)"
  ) +
  theme_minimal()


# =========================================================
# c) Plot residuals against year
# =========================================================

# Compute residuals for every observation
data_with_resid <- data %>%
  group_by(Length, Sex) %>%
  mutate(
    fitted    = fitted(lm(Time ~ Year)),
    residuals = Time - fitted
  ) %>%
  ungroup()

ggplot(data_with_resid, aes(x = Year, y = residuals, colour = Sex)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Length, scales = "free_y", ncol = 4) +
  labs(
    title  = "Olympic Running – Residuals from Linear Regression",
    x      = "Year",
    y      = "Residual (seconds)"
  ) +
  theme_minimal()

# What the residual plots indicate:
# - For several events the residuals show a curved (non-linear) pattern,
#   suggesting that the true improvement trend is not perfectly linear
#   (improvements have slowed in recent decades – diminishing returns).
# - Some large negative residuals appear in earlier decades (rapid early gains)
#   and residuals drift closer to zero in later years, indicating
#   heteroskedasticity or a non-constant rate of improvement.
# - A few outliers may correspond to exceptional performances or unusual
#   conditions (altitude, weather, doping era etc.).
# - The linear model is a useful first approximation but a curved or
#   piecewise model might fit better.


# =========================================================
# d) Predict winning times for the 2020 Olympics (held in 2021)
#    with 95% prediction intervals
# =========================================================

new_data <- data.frame(Year = 2020)

predictions <- models %>%
  rowwise() %>%
  mutate(
    pred   = predict(model, newdata = new_data, interval = "prediction",
                     level = 0.95)[1, "fit"],
    lower  = predict(model, newdata = new_data, interval = "prediction",
                     level = 0.95)[1, "lwr"],
    upper  = predict(model, newdata = new_data, interval = "prediction",
                     level = 0.95)[1, "upr"]
  )

print(predictions[, c("Length", "Sex", "pred", "lower", "upper")])

# Assumptions made in these calculations:
# 1. Linearity: the winning time decreases at a constant rate per year
#    (the linear model is correctly specified).
# 2. Independence: the residuals across Olympic Games are independent
#    (no serial correlation in improvements over time).
# 3. Constant variance (homoskedasticity): the variability of winning times
#    around the trend is the same throughout the entire period.
# 4. Normality of errors: the residuals are normally distributed,
#    which is required for the prediction intervals to be valid.
# 5. No structural breaks: the historical trend continues unchanged
#    into 2020 (e.g. no sudden rule changes, technological shifts,
#    or physiological limits becoming binding).
# 6. The model is fitted on past Games and extrapolated one step forward;
#    any systematic deviation from linearity inflates the true uncertainty
#    beyond what the formal interval captures.
