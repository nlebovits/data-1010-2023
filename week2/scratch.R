library(tidyverse)
library(nycflights13)

flights <- flights

flights <- flights %>%
              mutate(dep_time_lube = lubridate::hms(dep_time))

time <- flights %>%
          mutate(
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100,
          elapsed_time = (hour*60) + minute)


flights_filt <- flights %>% 
                  filter(month == 12,
                         day == 1)


cancel_count <- flights %>% 
  group_by(year, month, day) %>%
  summarise(n = n(),
           n_cancelled = sum(is.na(air_time)),
           not_cancelled = n - n_cancelled,
           p_cancelled = n_cancelled / n,
           delay = mean(arr_delay, na.rm = TRUE))


delay_x_carrier <- flights %>%
                      group_by(carrier) %>%
                      summarize(avg_delay = mean(arr_delay, na.rm = TRUE))


# proportion (%) of flights cancelled each day
cancelled_flights <- flights %>%
  filter(is.na(dep_delay), is.na(arr_delay))

cancelled_flights %>%
  mutate(perc_cancelled_flights = (cancelled_flights/flights)*100)

flights %>%
  
  group_by(dest) %>%
  summarise(arr_delay = n_distinct(arr_delay)) %>%
  
  arrange((arr_delay))

avg_delay_x_dest <- flights %>%
                        group_by(dest) %>%
                        summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
                        arrange(avg_delay)





