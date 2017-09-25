library(nycflights13)
library(tidyverse)

filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)

filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))

filter(flights, arr_delay >= 120)

flights <- flights

filter(flights, dest %in% c('IAH','HOU'))

filter(flights, carrier %in% c('AA','UA','DL'))
?airlines

filter(flights, month %in% c(7,8,9))

filter(flights, dep_delay <= 0, arr_delay > 120)


filter(flights, is.na(dep_time))


arrange(flights, is.na())

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

select(flights, contains("TIME", ignore.case = FALSE))


by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")



