#$ cd "C:\Users\MarkusSkifter\OneDrive - CBS - Copenhagen Business School\8. Semester\Predictive analytics\Opgaver\Assignment1\Predictive-analytics---Assignment-1"

# git add .
# git commit -m "hvad man har gjort"
# git push
    # når man får kode er det bare at trykke "1" og derefter enter

######## Question 1 #########

# loader data"
data("USMacroG", package = "AER")

#definerer df
df <- USMacroG

# because ggplot can not take the values if they are not numeric, we convert them to numeric from the timeseries format it otherwise is.
gdp_df <- data.frame(
  time = as.numeric(time(df)),
  gdp  = as.numeric(df[,"gdp"])
)

# we plot gdp
ggplot(gdp_df, aes(x = time, y = gdp)) + geom_line()

# we plot the acf function
ggAcf(gdp_df["gdp"], lag.max = 100)
