library(tidyverse)
install.packages("nycflights13")
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
