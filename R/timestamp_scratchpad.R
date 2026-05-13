# May 13, 2026

library(dplyr)
library(lubridate)


# example timestamp data ----------------------------------------------------------

ts <- seq(
  as_datetime("2026-05-12 00:20:54"),
  as_datetime("2026-05-12 04:00:00"),
  by = "15 mins"
)

ts

lubridate::round_date(ts, unit = "10 mins")

lubridate::round_date(ts, unit = "15 mins")

lubridate::round_date(ts, unit = "2 hours")


# as data.frame -----------------------------------------------------------

df <- data.frame(ts = ts, value = rnorm(length(ts)))

plot(df$ts, df$value)

# average
df_avg <- df %>% 
  mutate(ts_round = round_date(ts, unit = "2 hour")) %>% 
  summarise(
    # would need to include depth and/or station in .by
    value_avg = mean(value), .by = ts_round 
  )

# make rounding period flexible -------------------------------------------------

round_unit <- "5 hours"

# average
df_avg_flex <- df %>% 
  mutate(ts_round = round_date(ts, unit = round_unit)) %>% 
  summarise(
    value_avg = mean(value), .by = ts_round 
  )


