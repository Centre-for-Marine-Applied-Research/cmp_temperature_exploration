


#temperature averages ------------------------------------------------------


#Full set - isolate station at "X" depth 
 %>% 
  filter(sensor_depth_at_low_tide_m == "10") %>%
  ggplot(aes(x=ts_round, y=value_avg, colour=station)) +
  geom_point(size=0.5) +
  ggtitle("10 meters") +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

# individual stations 
 %>% 
  filter(station == "Moose Point 1") %>%
  filter(sensor_depth_at_low_tide_m == "2") %>%
  ggplot(aes(x=ts_round, y=value_avg, colour=sensor_depth_at_low_tide_m)) +
  geom_point(size=0.8) +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

#multiple stations - isolate at a depth 
 %>% 
  filter(sensor_depth_at_low_tide_m == "10") %>%
  filter(station == c("Spry Harbour", "Birchy Head", "Moose Point 1")) %>% 
  ggplot(aes(x=ts_round, y=value_avg, colour=station)) +
  geom_point(size=0.5) +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")


