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
    title = "Olympic Running – Winning Times by Event",
    x     = "Year",
    y     = "Winning Time (seconds)"
  )

# - All events show a clear downward trend over the century.
# - Women's events begin later (1928+) but show steeper declines.
# - Missing years due to WWI/WWII (1916, 1940, 1944).
# - Short sprints show small absolute improvements; long-distance
#   events show much larger absolute drops.


# =========================================================
# b) Fit a linear trend model for each event
# =========================================================

fit <- data %>%
  model(trend_model = TSLM(Time ~ Year))

# Report the average rate of improvement per year
fit %>%
  coef() %>%
  filter(term == "Year") %>%
  select(Length, Sex, estimate)

# Plot with fitted trend lines

fit %>%
  augment() %>%
  ggplot(aes(x = Year, y = Time, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = .fitted)) +
  facet_wrap(~ Length, scales = "free_y", ncol = 4) +
  labs(
    title = "Olympic Running – Winning Times with Linear Trend",
    x     = "Year",
    y     = "Winning Time (seconds)"
  ) 

# =========================================================
# c) Plot residuals 
# =========================================================
fit %>%
  augment() %>%
  ggplot(aes(x = Year, y = .resid, colour = Sex)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(Sex ~ Length, scales = "free_y", ncol = 4) +
  labs(
    title = "Olympic Running – Residuals from Linear Trend Model",
    x     = "Year",
    y     = "Residual (seconds)"
  ) 


fit %>%
  augment() %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(Sex ~ Length, scales = "free") +
  labs(
    title = "Residual QQ-Plots by Event",
    x     = "Theoretical Quantiles",
    y     = "Sample Quantiles"
  ) +
  theme_minimal()



# - Curved residual patterns suggest improvements have slowed (diminishing returns).
# - Some heteroskedasticity visible: larger spread in earlier years.
# - A curved or piecewise model might fit better than a straight line.


# =========================================================
# d) Predict winning times for the 2020 Olympics
#    with 90%, 95%, and 99% prediction intervals


fc <- fit %>%
  forecast(h = 1)  # one step ahead = 2020

print(fc)


fc %>%
  hilo(level = 95) %>%
  print()


fc %>%
  hilo(level = c(90, 95, 99)) %>%
  unpack_hilo(c(`90%`, `95%`, `99%`)) %>%
  select(Length, Sex, .mean, 
         `90%_lower`, `90%_upper`,
         `95%_lower`, `95%_upper`,
         `99%_lower`, `99%_upper`) %>%
  print()


# Assumptions:
# 1. Linearity: winning times decline at a constant rate per year.
# 2. Independence: residuals across Games are uncorrelated.
# 3. Homoskedasticity: residual variance is constant over time.
# 4. Normality: residuals are normally distributed.
# 5. No structural breaks: the historical trend continues into 2020.



