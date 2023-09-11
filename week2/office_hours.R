library(tidyverse)
library(nycflights13)
library(ggthemr)

ggthemr('fresh')

flights <- flights

not_cancelled <- flights %>%
                    filter(!is.na(dep_delay), !is.na(arr_delay))

#Shortest departure delays on average by hour.  
delay_x_hour <- not_cancelled %>%
                    group_by(hour) |> #
                    summarize(avg_dep_delay = mean(dep_delay)) |> #figure out the average
                    arrange(avg_dep_delay) # list in ascending order


delays_x_hour <- not_cancelled %>%
                      mutate(hourA = dep_time %/% 100) |>  
                      group_by(hour) |> #
                      summarize(avg_dep_delay = mean(dep_delay)) |> #figure out the average
                      arrange(avg_dep_delay) # list in ascending order

hmm <- not_cancelled |> 
  mutate(hour = dep_time %/% 100,
         on_time_early = dep_delay <=0) |> 
  group_by(hour) |> 
  summarise(on_time_early = 100 * mean(on_time_early))

hmm2 <- not_cancelled |> 
  mutate(hour = dep_time %/% 100,
         on_time_early = dep_delay <=0) |> 
  group_by(hour) |> 
  summarise(on_time_early = 100 * mean(on_time_early))  |> 
  arrange(on_time_early)
  


daily <- group_by(flights, year, month, day)

### OLD CODE

library(ggpubr)

delay <- daily %>%
  summarize(
    avg_delay = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    prop_canceled = mean(is.na(dep_time)))

oldPlot <- delay %>%
  filter(avg_delay > 20) %>%
  ggplot(mapping = aes(x = avg_delay, y = prop_canceled)) +
  geom_point(alpha = 5/10)


### NEW CODE


delay <- daily %>%
  summarize(
    avg_delay = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    prop_canceled = mean(is.na(dep_time)))


newPlot <- delay %>%
  filter(avg_delay > 10,
         prop_canceled < .4) %>%
  ggplot(mapping = aes(x = avg_delay, y = prop_canceled)) +
  geom_point(alpha = 3/10) +
  geom_smooth(se = FALSE)

ggarrange(oldPlot, newPlot, nrow = 2)

### Nissim's code

avg_delay_x_day <- flights %>%
  group_by(month, day) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE),
            tot_flights = n(),
            tot_cancelled = sum(is.na(dep_delay) & is.na(arr_delay))) %>%
  mutate(pct_cancelled = tot_cancelled/tot_flights*100)

ggplot(avg_delay_x_day, aes(x = avg_delay, y = pct_cancelled, fill = as.factor(month))) +
  geom_point() +
  # geom_smooth(method = "loess", se = TRUE) +
  # facet_wrap(~month) +
  scale_fill_viridis_d() +
  labs(title = "Avg. Delay vs. Pct. Cancelled",
       x = "Avg. Delay (Minutes)",
       y = "Pct. Cancelled")



#### structure q's

hmm3 <- flights %>%
          mutate(cancelled = is.na(dep_time)) %>%
          group_by(., dest) %>%
          summarise(pct.cancel = 100*mean(cancelled)) %>%
          arrange(pct.cancel)

hmm5 <- flights %>%
  mutate(cancelled = is.na(dep_time)) %>%
  group_by(dest) %>%
  summarise(pct.cancel = 100*mean(cancelled)) %>%
  arrange(pct.cancel)

hmm4 <- flights %>%
  mutate(cancelled = is.na(dep_time)) %>%
  group_by(cancelled, dest) %>%
  summarise(pct.cancel = 100*mean(cancelled)) %>%
  arrange(pct.cancel)



### 

element <- "a"
vector <- c("a", "b", "c")

dests <- c("ABQ", "ACK", "ANC", "EYW", "LEX", "SBN")

flights %>%
  # filter(dest == "ABQ" | dest=="ACK" | dest == "ANC" | dest ==
  #          "EYW" | dest == "LEX" | dest=="SBN") %>%
  filter(dest %in% dests) %>%
  group_by(dest) %>%
  summarise(n_tailnum = n())





####

library(tidyverse)
library(nycflights13)
library(ggthemr)

ggthemr('fresh')

flights <- flights

cancelled_flights <- flights %>%
  filter(is.na(dep_delay), is.na(arr_delay))

cancelled_flights_percent <- flights %>% 
  mutate(.,cancelled_flights_percent = ifelse((is.na(dep_delay) & is.na(arr_delay)),1,0)) %>%
  group_by(year, month, day) %>%
  summarise(cancelled_flights_percent = 100*mean(cancelled_flights_percent))

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

##finding the mean of the delays
avg_delay <- not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(avg_delay= mean(dep_delay))
