# first we define the dataframe:

df <- olympic_running

ggplot(
  olympic_running %>% 
    filter(Length == 100),
  aes(x = Year, y = Time, colour = Sex)
) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Olympic Winning Times – 100m",
       x = "Year",
       y = "Winning Time (seconds)")


### 200m ###