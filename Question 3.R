library(fpp3)
library(ggplot2)

######### Opgave 2 ##########

df <- olympic_running
df$Year <- as.numeric(df$Year)

# Fit linear model separately for each Length and Sex
results <- by(df, list(df$Length, df$Sex), function(subdata) {
  model <- lm(Time ~ Year, data = subdata)
  coef(model)["Year"]   # extract slope
})

results

######## Opgave 3 ###########
df$residuals <- NA

for(l in unique(df$Length)) {
  for(s in unique(df$Sex)) {
    
    # Subset data
    idx <- df$Length == l & df$Sex == s
    subdata <- df[idx, ]
    
    # Fit model
    model <- lm(Time ~ Year, data = subdata)
    
    # Store residuals back in original df
    df$residuals[idx] <- resid(model)
  }
}

ggplot(df, aes(x = Year, y = residuals, colour = Sex)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Length, scales = "free_y") +
  theme_minimal() +
  labs(title = "Residuals from Linear Trend Models",
       x = "Year",
       y = "Residual")


####################################

df$residuals <- NA_real_

for (l in unique(df$Length)) {
  for (s in unique(df$Sex)) {
    
    idx <- df$Length == l & df$Sex == s
    subdata <- df[idx, ]
    
    # skip tiny groups
    if (nrow(subdata) < 2) next
    
    # IMPORTANT: keep residuals aligned with original rows
    model <- lm(Time ~ Year, data = subdata, na.action = na.exclude)
    
    # resid(model) now has length = nrow(subdata) (with NAs where rows were dropped)
    df$residuals[which(idx)] <- resid(model)
  }
}

anyNA(df$residuals)   # TRUE is fine if there are missing Time/Year in the raw data

library(ggplot2)

ggplot(df, aes(x = Year, y = residuals, colour = Sex)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Length, scales = "free_y") +
  theme_minimal() +
  labs(title = "Residuals from Time ~ Year (by Length × Sex)",
       x = "Year",
       y = "Residual")
