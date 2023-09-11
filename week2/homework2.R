library(tidyverse)
library(nycflights13)
library(ggthemr)

ggthemr('fresh')

flights <- flights

### Question #1

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))  # the !is.na here means IS NOT NA, much like != means IS NOT EQUAL

avg_delay_x_day <- flights %>%
                      group_by(month, day) %>%
                      summarize(avg_delay = mean(arr_delay, na.rm = TRUE),
                                tot_flights = n(),
                                tot_cancelled = sum(is.na(dep_delay) & is.na(arr_delay))) %>%
                      mutate(pct_cancelled = tot_cancelled/tot_flights*100)

### Question #2

ggplot(avg_delay_x_day, aes(x = avg_delay, y = pct_cancelled)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  # facet_wrap(~month) +
  labs(title = "Avg. Delay vs. Pct. Cancelled",
       x = "Avg. Delay (Minutes)",
       y = "Pct. Cancelled")


### Question #3

avg_delay_x_hour <- flights %>%
                        group_by(hour) %>%
                        summarize(avg_delay = mean(arr_delay, na.rm = TRUE),
                                  tot_flights = n(),
                                  ontime_or_early = sum(is.na(dep_delay) | dep_delay < 0)) %>%
                        mutate(pct_ontime_or_early = ontime_or_early / tot_flights*100)

ggplot(avg_delay_x_hour, aes(x = hour, y = avg_delay)) +
  geom_col() +
  labs(title = "Avg. Delay per Hour",
       x = "Hour of Day",
       y = "Avg. Delay (Minutes)")


ggplot(avg_delay_x_hour, aes(x = hour, y = pct_ontime_or_early)) +
  geom_col() +
  labs(title = "Pct. on Time or Early per Hour",
       x = "Hour of Day",
       y = "Pct. on Time or Early")

### Question #4

long_delays <- flights %>% 
                  mutate(long_delay = ifelse(is.na(dep_delay) | dep_delay < 30, 0, 1)) %>% # what if dep delay is NA? have to exclude that, too
                  group_by(carrier) %>%
                  summarize(tot_flights = n(),
                            tot_long_delay = sum(long_delay == 1)) %>% # tally total number of depature delays of at least 30 minutes per carrier
                  mutate(pct_long_delays = tot_long_delay / tot_flights * 100) # calculate as pct of total flights

ggplot(long_delays, aes(x = reorder(carrier, -pct_long_delays), y = pct_long_delays)) +
  geom_col() +
  labs(title = "Pct. Long Delays by Carrier",
       x = "Carrier",
       y = "Long Delays (%)")

### Question #5

avg_delay_x_dest <- flights %>%
                      group_by(dest) %>%
                      summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
                      arrange(avg_arr_delay)

ggplot(avg_delay_x_dest, aes(y = reorder(dest, -avg_arr_delay), x = avg_arr_delay)) +
  geom_col() +
  labs(title = "Avg. Arrival Delay by Destination",
       y = "Destination",
       x = "Avg. Arrival Delay (Minutes)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),  # Adjust the size as desired
    panel.grid.major.y = element_blank(),  # Remove major vertical gridlines
    panel.grid.minor.y = element_blank()   # Remove minor vertical gridlines
  )


