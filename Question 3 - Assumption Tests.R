# --- Packages ---
library(fpp3)
library(ggplot2)
library(dplyr)
library(lmtest)      # dwtest, bgtest, bptest, resettest
library(tseries)     # jarque.bera.test
library(strucchange) # Chow test, CUSUM, Bai-Perron

# --- Load data and fit models (same as Question 3.R) ---
data <- olympic_running

# Helper: fit one model per (Length, Sex) group and return a named list
fit_models <- function(df) {
  df %>%
    group_by(Length, Sex) %>%
    do(model = lm(Time ~ Year, data = .))
}

models <- fit_models(data)

# Helper: run a set of tests on a single lm object and print results
run_tests <- function(mod, label) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Event:", label, "\n")
  cat(rep("-", 60), "\n", sep = "")

  # -------------------------------------------------------
  # 1. LINEARITY  –  Ramsey RESET test
  #    H0: the linear specification is correct (no omitted
  #        higher-order terms).  Reject H0 => non-linearity.
  # -------------------------------------------------------
  cat("\n[1] Linearity – RESET test\n")
  print(resettest(mod, power = 2:3, type = "fitted"))

  # -------------------------------------------------------
  # 2. INDEPENDENCE  –  Durbin-Watson & Breusch-Godfrey
  #    DW tests AR(1) autocorrelation (DW ≈ 2 => no AC).
  #    BG is more general (tests up to lag p).
  # -------------------------------------------------------
  cat("\n[2] Independence – Durbin-Watson test\n")
  print(dwtest(mod))

  cat("\n[2] Independence – Breusch-Godfrey test (lag = 2)\n")
  print(bgtest(mod, order = 2))

  # ACF plot of residuals (visual check)
  acf(resid(mod), main = paste("ACF of residuals –", label))

  # -------------------------------------------------------
  # 3. HOMOSKEDASTICITY  –  Breusch-Pagan & Goldfeld-Quandt
  #    BP regresses squared residuals on regressors.
  #    H0: constant variance.  Reject H0 => heteroskedasticity.
  #    GQ splits sample in two halves and compares variances.
  # -------------------------------------------------------
  cat("\n[3] Homoskedasticity – Breusch-Pagan test\n")
  print(bptest(mod))

  cat("\n[3] Homoskedasticity – Goldfeld-Quandt test\n")
  print(gqtest(mod, order.by = ~ Year, data = model.frame(mod)))

  # -------------------------------------------------------
  # 4. NORMALITY  –  Shapiro-Wilk & Jarque-Bera
  #    SW is powerful for small samples (n < ~50).
  #    JB tests skewness and kurtosis jointly.
  #    H0: residuals are normally distributed.
  # -------------------------------------------------------
  cat("\n[4] Normality – Shapiro-Wilk test\n")
  print(shapiro.test(resid(mod)))

  cat("\n[4] Normality – Jarque-Bera test\n")
  print(jarque.bera.test(resid(mod)))

  # QQ plot (visual check)
  qqnorm(resid(mod), main = paste("QQ plot –", label))
  qqline(resid(mod), col = "red")

  # -------------------------------------------------------
  # 5. STRUCTURAL BREAK  –  CUSUM & Bai-Perron
  #    CUSUM plots cumulative sum of recursive residuals;
  #    crossing the confidence band => parameter instability.
  #    Bai-Perron tests for unknown breakpoint locations.
  # -------------------------------------------------------
  cat("\n[5] Structural break – CUSUM test\n")
  cs <- efp(Time ~ Year, data = model.frame(mod), type = "OLS-CUSUM")
  plot(cs, main = paste("CUSUM –", label))

  cat("\n[5] Structural break – Bai-Perron breakpoint test\n")
  bp_test <- breakpoints(Time ~ Year, data = model.frame(mod))
  print(summary(bp_test))
}

# =========================================================
# Run all tests for every (Length, Sex) combination
# =========================================================

for (i in seq_len(nrow(models))) {
  label <- paste0(models$Length[i], "m – ", models$Sex[i])
  run_tests(models$model[[i]], label)
}

# =========================================================
# Interpretation guide
# =========================================================
#
# 1. LINEARITY (RESET test)
#    p < 0.05  => reject H0 => evidence of non-linearity.
#    Common fix: add Year^2 term, or use a log/square-root
#    transformation of Time.
#
# 2. INDEPENDENCE (DW / BG)
#    DW < 1.5 or > 2.5  => likely autocorrelation present.
#    p < 0.05 in BG      => reject H0 => serial correlation.
#    With only ~27 observations (one per Games) serial
#    correlation is possible if performance trends cluster.
#
# 3. HOMOSKEDASTICITY (BP / GQ)
#    p < 0.05  => reject H0 => variance is not constant.
#    Typical pattern: higher variance in early years when
#    fewer athletes competed; lower variance in modern era.
#    Fix: WLS or heteroskedasticity-robust standard errors.
#
# 4. NORMALITY (SW / JB)
#    p < 0.05  => reject H0 => residuals not normal.
#    With n ≈ 27 the tests have limited power; the QQ plot
#    is often more informative.  Minor departures are usually
#    acceptable for prediction intervals.
#
# 5. STRUCTURAL BREAK (CUSUM / Bai-Perron)
#    CUSUM band crossed        => parameter instability.
#    Bai-Perron reports the most likely break year(s).
#    Plausible break points: ~1952 (post-WWII boom in sport),
#    ~1980s (professional training, nutrition science).
#    If a break is found, fit separate regressions per period.
