#$ cd "C:\Users\MarkusSkifter\OneDrive - CBS - Copenhagen Business School\8. Semester\Predictive analytics\Opgaver\Assignment1\Predictive-analytics---Assignment-1"

# git add .
# git commit -m "hvad man har gjort"
# git push
    # når man får kode er det bare at trykke "1" og derefter enter

######## Question 1 #########

data("USMacroG", package = "AER")

df <- USMacroG

plot(df[,"gdp"],
     main = "Real GDP (US, 1950–2000)",
     ylab = "GDP (Billion USD)",
     xlab = "Time")

acf(USMacroG[,"gdp"],
    lag.max = 40,
    col = "blue",
    lwd = 2,
    main = "Autocorrelation Function of Real GDP")

