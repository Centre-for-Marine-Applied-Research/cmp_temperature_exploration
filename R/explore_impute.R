library(dplyr)
library(ggplot2)
library(imputeTS)
library(lubridate)
library(purrr)
library(tidyr)

A <- 10
per <- 365
phi <- 0
C <- 15

df <- data.frame(t = 1:730) |> 
  mutate(value = A * sin((2 * pi * t) / per  + phi))

ggplot(df, aes(t, value)) +
  geom_point()

# NAs for whole time series -----------------------------------------------

dat <- data.frame(
  timestamp = seq(as_date("2025-01-01"), as_date("2026-12-31")),
  value = NA
) 

dat_imp <- ts(dat)

imp <- na_seadec(dat_imp, algorithm = "interpolation", find_frequency = TRUE)


# single time series ------------------------------------------------------

dat1 <- data.frame(
  timestamp = seq(as_date("2025-01-01"), as_date("2026-12-31"))
) |> 
  mutate(value = A * sin((2 * pi * as.numeric(timestamp)) / per  + phi)) 

dat1[20:45, "value"] <- NA
dat1[471:571, "value"] <- NA

ggplot(dat1, aes(timestamp, value)) +
  geom_line() 

dat_imp1 <- dat1 |> 
  ts() |> 
  na_seadec(algorithm = "interpolation", find_frequency = TRUE)

dat1$value_imp <- imp1[, 2]

dat1 <- dat1 |> 
  mutate(imp = if_else(is.na(value), TRUE, FALSE))

ggplot(dat1, aes(timestamp, value_imp, col = imp)) +
  geom_point() 


# single time series - not in chronological order ------------------------------------------------------

dat1.1 <- data.frame(
  timestamp = seq(as_date("2025-01-01"), as_date("2026-12-31"))
) |> 
  mutate(value = A * sin((2 * pi * as.numeric(timestamp)) / per  + phi)) 

dat1.1[20:45, "value"] <- NA
dat1.1[471:571, "value"] <- NA

dat1.1 <- dat1[sample(1:nrow(dat1.1)), ]

# looks the same as the above because ggplot knows timestamp is a posixct object
ggplot(dat1.1, aes(timestamp, value)) +
  geom_line() 

dat_imp1.1 <- dat1.1 |> 
  ts() |> 
  na_seadec(algorithm = "interpolation", find_frequency = TRUE)

dat1.1$value_imp <- imp1[, 2]

dat1.1 <- dat1.1 |> 
  mutate(imp = if_else(is.na(value), TRUE, FALSE))

ggplot(dat1.1, aes(timestamp, value_imp, col = imp)) +
  geom_point() 


# 2 groups: All NAs for 1 group ---------------------------------------------------------

# silently gives wrong answer!
dat2 <- data.frame(
  timestamp = seq(as_date("2025-01-01"), as_date("2026-12-31")),
  variable_1 = NA
) |> 
  mutate(variable_2 = A * sin((2 * pi * as.numeric(timestamp)) / per  + phi)) |> 
  pivot_longer(cols = contains("variable"), values_to = "value", names_to = "variable") |> 
  arrange(timestamp)
 
dat2[75:125, "value"] <- NA
dat2[1000:1200, "value"] <- NA

ggplot(dat2, aes(timestamp, value)) +
  geom_line() +
  facet_wrap(~variable, ncol = 1)

dat_imp2 <- dat2 |> 
  ts() |> 
  # doesn't give an error or warning!
  # na_secdec does not respect group_by
  na_seadec(algorithm = "interpolation", find_frequency = TRUE) 

dat2$value_imp <- dat_imp2[, 3]

dat2 <- dat2 |> 
  mutate(imp = if_else(is.na(value), TRUE, FALSE)) 

ggplot(dat2, aes(timestamp, value_imp, col = imp)) +
  geom_point() +
  facet_wrap(~variable, ncol = 1)

# maps over 2 time series---------------------------------------------------------------------

dat3 <-  data.frame(
  timestamp = seq(as_date("2025-01-01"), as_date("2026-12-31"))) |> 
  mutate(
    variable_1 = A * sin((2 * pi * as.numeric(timestamp)) / per  + phi),
    variable_2 = 5 * sin((2 * pi * as.numeric(timestamp)) / per  + 25)
  ) |> 
  pivot_longer(cols = contains("variable"), values_to = "value", names_to = "variable") |> 
  arrange(variable, timestamp)

dat3[100:175, "value"] <- NA
dat3[300:335, "value"] <- NA

dat3[750:775, "value"] <- NA
dat3[1200:1300, "value"] <- NA

ggplot(dat3, aes(timestamp, value)) +
  geom_line() +
  facet_wrap(~variable, ncol = 1)

dat3_imp <- dat3 |> 
  group_split(variable) |> 
  purrr::map(
    .f = \(x) ts(x) |> 
      na_seadec(algorithm = "interpolation", find_frequency = TRUE) |> 
      data.frame()
  ) |> 
  list_rbind() 

dat3$value_imp <- dat3_imp$value 

dat3 <- dat3 |> 
  mutate(imp = if_else(is.na(value), TRUE, FALSE))

ggplot(dat3, aes(timestamp, value_imp, col = imp)) +
  geom_point() +
  facet_wrap(~variable, ncol = 1)

# map with nas---------------------------------------------------------------------

dat4 <- data.frame(
  timestamp = seq(as_date("2025-01-01"), as_date("2026-12-31")),
  variable_1 = NA
) |> 
  mutate(variable_2 = A * sin((2 * pi * as.numeric(timestamp)) / per  + phi)) |> 
  pivot_longer(cols = contains("variable"), values_to = "value", names_to = "variable") |> 
  arrange(variable)


dat4[750:775, "value"] <- NA
dat4[1200:1300, "value"] <- NA

ggplot(dat4, aes(timestamp, value)) +
  geom_line() +
  facet_wrap(~variable, ncol = 1)

# gives a warning message!
dat4_imp <- dat4 |> 
  group_split(variable) |> 
  map(
    .f = \(x) ts(x) |> 
      na_seadec(algorithm = "interpolation", find_frequency = TRUE) |> 
      data.frame()
  ) |> 
  list_rbind() 

dat4$value_imp <- dat4_imp$value 

dat4 <- dat4 |> 
  mutate(imp = if_else(is.na(value), TRUE, FALSE))

ggplot(dat4, aes(timestamp, value_imp, col = imp)) +
  geom_point() +
  facet_wrap(~variable, ncol = 1)


