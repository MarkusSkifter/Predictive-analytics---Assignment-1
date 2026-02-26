data("USMacroG", package = "AER")

df <- USMacroG

plot(df[,"gdp"],
     main = "Real GDP (US, 1950–2000)",
     ylab = "GDP (Billion USD)",
     xlab = "Time")
