## Transforming and Cleaning Data

# this week, we'll be working through commands that are in the package dplyr
# dplyr (pronounced dee-plyer if you say it out loud) is a package that is part of the tidyverse
# when you install the tidyverse, you get dplyr, as well as ggplot2 and a few other handy tools for wrangling and displaying data
# you'll notice a trend in all these dplyr commands
# in each one, you'll first name the dataset you're working with, then the variables you are working with)

# First, load your libraries. Install packages first if necessary!
#install.packages("tidyverse")
#install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

# Take a look at the data
flights

# If you want to see all rows/columns, you have to use View()
View(flights)

# use the filter command to find all flights that left on December 1st
filter(flights, month==12, day==1)

# if you want to save this output, you can assign it to a variable
dec1 <- filter(flights, month==12, day==1)
# now look at what you just saved
dec1

# use OR or AND operators to make your filter stronger
# or uses this symbol -> | to select things that fit one OR the other parameter
# and uses this symbol -> & to select things that fit BOTH parameters

# select flights from November OR December
filter(flights, month==11 | month==12)

# select flights that left in January AND before 6AM
filter(flights, month==1 & dep_time < 600)

# use the arrange command to reorder your dataset
# similar to sorting by multiple columns in Excel, this will sort by the first thing, then the second, etc.
arrange(flights, year, month, day)

# throw in desc() to sort things in descending order (largest to smallest)
arrange(flights, desc(dep_delay))

# use the select command to choose a subset of variables
# pull out just days and times of the flights data
select(flights, month, day)

# use a minus sign(-) to select everything BUT a variable
select(flights, -dep_time)

# a few more things you can do with select, including selecting variables that start with a particular string of characters
select(flights, starts_with("sch"))

# you can also use select to reorder things - columns will show up in the order you type them here
# in this example, we're putting one variable first and everything else after it
select(flights, air_time, everything())

# how to rename a variable in your dataset using the rename command
# you'll do new_name=old_name
rename(flights, sched_dep=sched_dep_time)

# using the mutate command to make changes to variables
# think of this like calculating across columns in Excel
# let's first make a smaller version of the dataset using select

# highlight this whole thing and run it - the help text to the side is just there to explain each part and won't mess up the command!
flights_sml <- select(flights,   # selecting from the flights dataset
                        year:day,   # selecting everything between the year column and the day column, inclusive (so in this case, it's year, month, day)
                        ends_with("delay"),  # selecting all the fields that end with the string "delay"
                        distance,   # selecting distance
                        air_time)   # selecting air_time

# now take a look at the new dataset
flights_sml

# using mutate to a new variable, speed. Speed is distance divided by time. Air time is in minutes, so we want to multiply by 60 to get mph
mutate(flights_sml, speed = distance/air_time*60)

# while mutate gives you the whole dataset, you can use transmute instead to get just your new variable
transmute(flights_sml, speed = distance/air_time*60)

# you can use modular arithmetic to do some fun math, especially helpful with time variables
# a modular operation gives you the remainder of a division problem
# you may remember this from...elementary school?
# example - if you divide 517 by 100, you get 5 with a remainder of 17. 100 goes evenly into 517 five times, and there are 17 left over
# because of the way the time variables are listed here (517=5:17), you can divide by 100 to get the hour and the remainder is your minute!
transmute(flights,
          dep_time,
          hour = dep_time %/% 100, # %/% gives you the quotient
          minute = dep_time %% 100) # %% gives you the remainder

# a few other commands, like min_rank(), can be used here to rank order the values of a particular variable
longest_delay <- mutate(flights_sml,
                        delay_rank = min_rank(arr_delay))
arrange(longest_delay, delay_rank)

# the summarise command helps you do operations to create summaries of data
# yes, it is summarise with an s and not a z - you'll find a handful of things in R using British spellings

# find the average flight departure delay
# adding na.rm = TRUE here to remove any missing data
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# we can use the group_by command to group data together based on common things across the dataset like date
by_day <- group_by(flights, year, month, day)
# now we can use summarise to see the average departure delay by date
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# the pipe (%>%) is a helpful operator to string commands together
# think of it like you are "piping" the results of one command into the next

# here's an example doing a couple of things step by step without pipes
# grouping flights by destination
by_dest <- group_by(flights, dest)
# use grouped data to calculate distance and average delay
delay <- summarise(by_dest, 
            count = n(),  # this is just counting the number of records by destination
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE))
# now use ggplot to display the data
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +  # alpha values under 1 makes the dots a little more transparent
  geom_smooth(se = FALSE) # se = FALSE gets rid of that confidence interval area on the line

# one quick thing - we see here a big outlier out at dist = 5,000, which is throwing off our data a bit since it's so far from everything else
# let's find out what it is
filter(delay, dist > 4500)
# looks like it's a flight to Honolulu, HI
# let's see what the chart looks like if we get rid of this flight to dest="HNL"
# the operator != means NOT EQUAL TO, so what we are saying below is to select destinations that are not "HNL"
# we can also filter out any airports with less than 20 flights per year since these are more specialty, small flights and won't show us many useful trends
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# ok, remember the pipe (%>%) mentioned above? let's try to put a few of these things together using that

delays <- flights %>%  # put all this into a dataset called delays, start with the flights data
  group_by(dest) %>%  # take that flights data and group it by destination
  summarise(   # take that grouped data and summarise it as we did before
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL")  # take that summarised data and get rid of count < 20 and dest = "HNL"
  )


# we used na.rm = TRUE a bunch, but why do we do that?
# let's look at some things without taking out the NA values
flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))
# we see that mean = NA in all the rows previewed here
# if there is missing data in the input, the output will not calculate
# so there must be some missing data where we are trying to take the mean
# let's look for the rows of data where dep_delay is NA
filter(flights, is.na(dep_delay))
# looks like these are cancelled flights that never took off or arrived, so they can't possibly even have a departure delay

# let's look at a subset of flights that were not cancelled
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))  # the !is.na here means IS NOT NA, much like != means IS NOT EQUAL

# and let's group these by tail number, so we can see if delays vary by type of plane
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay))
  )

# plot the data to see what it tells us
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10) #geom_freqpoly is plotting the count of values at each minute of delay

# we see a spike around 0, which is good because it means that most planes have average delays around 0
# but there are quite a few wayyyy out at 300 minutes, which is a pretty big average delay
# let's investigate!
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),  # find the mean delay
    n = n())  # and the number of flights by plane

# we see here that all the planes with crazy delays had very few total flights, so they're probably not very helpful to look at here
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# let's filter out all the planes that took fewer than 25 flights and see how that changes things
delays %>%
  filter(n > 25) %>%   # now see how we can pipe filtered data straight into a ggplot - notice we don't have to define the dataset we're using because we've piped it in
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

# now let's combine some of these functions we've learned together, we'll do a few things here
# 1. create an indicator of whether a flight was cancelled
# 2. count the number of flights and cancelled flights by carrier
# 3. calculate the percentage of cancelled flights by carrier
flights %>%
  mutate(., cancelled = ifelse((is.na(dep_delay) & is.na(arr_delay)), 1, 0)) %>% #using the ifelse command here to set our new variable cancelled = 1 if dep_delay and arr_delay are NA but otherwise make it 0 
  group_by(carrier) %>% 
  summarise(100*mean(cancelled))  # because our values are 1 if cancelled and 0 if not, taking the mean will give us the percent of flights that are cancelled

# note the dot above at the beginning of the mutate command - this just means we're using what was piped in above (the flights data)

# some of the delays data includes negative values, which means the plane took off early. this could be throwing off some of our delay info
# let's see how avg delay differs if we keep these negative delays or remove them
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),   # our basic avg delay measure without taking out negatives
    avg_delay2 = mean(arr_delay[arr_delay > 0])  # what is in the [brackets] is subsetting the data so we're pulling out only arr_delay > 0, or the average positive delay 
  )

# let's try a few more things to explore our data
# we can use the first() or last() function to find the values at the extremes of a set of data
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time))
  
# you can use the n_distinct() command to find the number of unique values for a given variable
# this will show you the number of distinct carriers by destination in descending order
not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

# you can use the count() function to count how many times something is in the dataset
# here we count the number of flights going to each destination
not_cancelled %>%
  count(dest)

# put a few of these things together to get more complex output
# here we find the % of flights each day that are delayed by more than 1 hour
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(hour_perc = 100*mean(arr_delay > 60))

# we've been grouping by day, month, and year but we can do a few layers of grouping to separate this out
daily <- group_by(flights, year, month, day)
per_day <- summarise(daily, flights = n())
per_month <- summarise(per_day, flights = sum(flights))
per_year <- summarise(per_month, flights = sum(flights))

# now let's take a look at what we have
per_day
per_month
per_year

# you can also ungroup data using the ungroup function
daily %>%
  ungroup() %>%  # no longer grouped by date
  summarise(flights = n()) # now you're just counting the ungrouped data

# one more big example putting a lot of functions together!
# how do we answer the question of which destinations have the smallest percentage of cancelled flights?
# 1. see if each flight was cancelled
# 2. group flights by destination
# 3. calculate the % of flights cancelled for each destination
# 4. arrange them to see the destinations with fewest cancelled flights

# note you won't see much different in the first few steps below because you don't see all the variables in the previews shown 

# creating a new variable to indicate whether a flight was cancelled (is the departure time NA?)
flights %>%
  mutate(cancelled = is.na(dep_time))

# build on that and group flights by destination airport
flights %>%
  mutate(cancelled = is.na(dep_time)) %>%
  group_by(., dest)

# add on a line to find the % of cancelled flights for each destination
flights %>%
  mutate(cancelled = is.na(dep_time)) %>%
  group_by(., dest) %>%
  summarise(pct.cancel = 100*mean(cancelled))

# now add a line to arrange by % cancelled
flights %>%
  mutate(cancelled = is.na(dep_time)) %>%
  group_by(., dest) %>%
  summarise(pct.cancel = 100*mean(cancelled)) %>%
  arrange(pct.cancel)

# now you've seen how we can build additional items onto the same code to get more specific with our commands!
