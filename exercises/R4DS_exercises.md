# R4D Exercises
Martin Kruger  
10 September 2017  


\ 


## 5. Data Transformation

### 5.1 Introduction





### 5.2 Filter rows with `filter()`

#### 5.2.1 Comparisons 

#### 5.2.2 Logical operators 

#### 5.2.3 Missing values 

#### 5.2.4 Exercises 

1. Find all flights that
    1. Had an arrival delay of two or more hours
    2. Flew to Houston (IAH or HOU)
    3. Were operated by United, American, or Delta
    4. Departed in summer (July, August, and September)
    5. Arrived more than two hours late, but didn’t leave late
    6. Were delayed by at least an hour, but made up over 30 minutes in flight
    7. Departed between midnight and 6am (inclusive)



2. Another useful **dplyr** filtering helper is `between()`. What does it do? Can you use it to simplify the code needed to answer the previous challenges?

```
between() inclusive of both start and end values

between(x, left, right) is equivalent to x >= left & x <= right

```

3. How many flights have a missing `dep_time`? What other variables are missing? What might these rows represent?




4. Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)

`NA^0 == 1`, because for all numeric values $x^0 = 1$ 


### 5.3 Arrange rows with `arrange()`

`arrange()` works similarly to `filter()` except that instead of selecting rows, it changes their order. 

#### 5.3.1 Exercises

1. How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
2. Sort flights to find the most delayed flights. Find the flights that left earliest.
3. Sort flights to find the fastest flights.
4. Which flights travelled the longest? Which travelled the shortest?


```r
# 1.
arrange(flights, desc(is.na(dep_time)), dep_time)

# 2. 
arrange(flights, desc(dep_delay), dep_time)
arrange(flights, dep_delay, dep_time)

# 3. 
arrange(flights, air_time)

# 4. 
arrange(flights, desc(distance))
arrange(flights, distance)
```
\ 

### 5.4 Select columns with `select()`

There are a number of helper functions you can use within `select()`:  

 + `starts_with("abc")`: matches names that begin with “abc”.  
 + `ends_with("xyz")`: matches names that end with “xyz”.  
 + `contains("ijk")`: matches names that contain “ijk”.  
 + `matches("(.)\\1")`: selects variables that match a regular expression. This one matches any variables that contain repeated characters.  
 + `num_range("x", 1:3)`: matches x1, x2 and x3.  
\ 


#### 5.4.1 Exercises

1. Brainstorm as many ways as possible to select `dep_time`, `dep_delay`, `arr_time`, and `arr_delay` from `flights`.
2. What happens if you include the name of a variable multiple times in a `select()` call?
3. What does the `one_of()` function do? Why might it be helpful in conjunction with this vector?
`vars <- c("year", "month", "day", "dep_delay", "arr_delay")`
4. Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
`select(flights, contains("TIME"))`


```r
# 1.
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("dep_"), starts_with("arr_"))
select(flights, matches("^(dep|arr)_(time|delay)$"))

# 2. 
select(flights, year, month, day, year, year)

# 3.
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

# 4.
select(flights, contains("TIME"))

select(flights, contains("TIME", ignore.case = FALSE))
```
\ 

### 5.5 Add new variables with `mutate()`

Besides selecting sets of existing columns, it’s often useful to add new columns that are functions of existing columns. That’s the job of `mutate()`. 

If you only want to keep the new variables, use `transmute()` 


#### 5.5.1 Useful creation functions

 + Arithmetic operators: +, -, *, /, ^.  
 + Modular arithmetic: `%/%` (integer division) and `%%` (remainder), where `x == y * (x %/% y) + (x %% y)`.  
 + Logs: `log()`, `log2()`, `log10()`.  
 + Offsets: `lead()` and `lag()`.  
 + Cumulative and rolling aggregates: R provides functions for running sums, products, mins and maxes: `cumsum()`, `cumprod()`, `cummin()`, `cummax()`; and dplyr provides `cummean()` for cumulative means.  
 + Logical comparisons, `<, <=, >, >=, !=`.  
 + Ranking: there are a number of ranking functions, but you should start with `min_rank()`. `row_number()`, `dense_rank()`, `percent_rank()`, `cume_dist()`, `ntile()`.  
 
 
#### 5.5.2 Exercises 

1. Currently `dep_time` and `sched_dep_time` are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
2. Compare `air_time` with `arr_time - dep_time`. What do you expect to see? What do you see? What do you need to do to fix it?
3. Compare `dep_time`, `sched_dep_time`, and `dep_delay`. How would you expect those three numbers to be related?
4. Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for `min_rank()`.
5. What does `1:3 + 1:10` return? Why?
6. What trigonometric functions does R provide?
 




### 5.6 Grouped summaries with `summarise()`

It collapses a data frame to a single row. 
 

#### 5.6.1 Combining multiple operations with the pipe

#### 5.6.2 Missing values

#### 5.6.3 Counts

#### 5.6.4 Useful summary functions

 + Measures of location: we’ve used `mean(x)`, but `median(x)` is also useful. The mean is the sum divided by the length; the median is a value where 50% of x is above it, and 50% is below it.  
 + Measures of spread: `sd(x)`, `IQR(x)`, `mad(x)`. The mean squared deviation, or standard deviation or sd for short, is the standard measure of spread. The interquartile range `IQR()` and median absolute deviation `mad(x)` are robust equivalents that may be more useful if you have outliers.  
 + Measures of rank: `min(x)`, `quantile(x, 0.25)`, `max(x)`. Quantiles are a generalisation of the median. For example, `quantile(x, 0.25)` will find a value of x that is greater than 25% of the values, and less than the remaining 75%.  
 + Measures of position: `first(x)`, `nth(x, 2)`, `last(x)`. These work similarly to `x[1]`, `x[2]`, and `x[length(x)]` but let you set a default value if that position does not exist (i.e. you’re trying to get the 3rd element from a group that only has two elements).  
 + Counts: You’ve seen `n()`, which takes no arguments, and returns the size of the current group. To count the number of non-missing values, use `sum(!is.na(x))`. To count the number of distinct (unique) values, use `n_distinct(x)`.  
 `not_cancelled %>% count(dest)`  
 `not_cancelled %>% count(tailnum, wt = distance)`  
 + Counts and proportions of logical values: `sum(x > 10)`, `mean(y == 0)`. When used with numeric functions, TRUE is converted to 1 and FALSE to 0. This makes `sum()` and `mean()` very useful: `sum(x)` gives the number of TRUEs in x, and `mean(x)` gives the proportion.  


#### 5.6.5 Grouping by multiple variables

#### 5.6.6 Ungrouping

If you need to remove grouping, and return to operations on ungrouped data, use `ungroup()`.  


#### 5.6.7 Exercises

1. Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:
 + A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.  
 + A flight is always 10 minutes late.  
 + A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.  
 + 99% of the time a flight is on time. 1% of the time it’s 2 hours late.  
 Which is more important: arrival delay or departure delay?

2. Come up with another approach that will give you the same output as `not_cancelled %>% count(dest)` and `not_cancelled %>% count(tailnum, wt = distance)` (without using `count()`).  

3. Our definition of cancelled flights `(is.na(dep_delay) | is.na(arr_delay) )` is slightly suboptimal. Why? Which is the most important column?

4. Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?

5. Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about `flights %>% group_by(carrier, dest) %>% summarise(n()))`

6. What does the sort argument to `count()` do. When might you use it?
\ 

```
# 1. 
Arrival delay is more important. Arriving early is nice, but equally as good as arriving late is bad. Variation is worse than consistency; if I know the plane will always arrive 10 minutes late, then I can plan for it arriving as if the actual arrival time was 10 minutes later than the scheduled arrival time.

So I’d try something that calculates the expected time of the flight, and then aggregates over any delays from that time. I would ignore any early arrival times. A better ranking would also consider cancellations, and need a way to convert them to a delay time (perhaps using the arrival time of the next flight to the same destination).

```


```r
# 2.
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

#not_cancelled %>% count(dest)
not_cancelled %>% group_by(dest) %>% tally()

#not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% group_by(tailnum) %>% summarise(n= sum(distance))


# 3.



# 4.

cancelled_delayed <- 
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(prop_cancelled = mean(cancelled),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))

ggplot(cancelled_delayed, aes(x = avg_dep_delay, prop_cancelled)) +
  geom_point() +
  geom_smooth()


# 5.
flights %>% group_by(carrier, dest) %>% summarise(n())


flights %>%
  #filter(arr_delay > 0) %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))


# 6.
```


\ 

### 5.7 Grouped mutates (and filters)

Grouping is most useful in conjunction with `summarise()`, but you can also do convenient operations with `mutate()` and `filter()`:

A grouped filter is a grouped mutate followed by an ungrouped filter.  


##### 5.7.1 Exercises

1. Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.  
2. Which plane (tailnum) has the worst on-time record?  
3. What time of day should you fly if you want to avoid delays as much as possible?  
4. For each destination, compute the total minutes of delay. For each, flight, compute the proportion of the total delay for its destination.  
5. Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. Using `lag()` explore how the delay of a flight is related to the delay of the immediately preceding flight.  
6. Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). Compute the air time a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?  
7. Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.   
8. For each plane, count the number of flights before the first delay of greater than 1 hour.    



\ 


*** 

## 12. Tidy Data 


```r
table1
```

```
## # A tibble: 6 x 4
##       country  year  cases population
##         <chr> <int>  <int>      <int>
## 1 Afghanistan  1999    745   19987071
## 2 Afghanistan  2000   2666   20595360
## 3      Brazil  1999  37737  172006362
## 4      Brazil  2000  80488  174504898
## 5       China  1999 212258 1272915272
## 6       China  2000 213766 1280428583
```
\ 


```r
table2
```

```
## # A tibble: 12 x 4
##        country  year       type      count
##          <chr> <int>      <chr>      <int>
##  1 Afghanistan  1999      cases        745
##  2 Afghanistan  1999 population   19987071
##  3 Afghanistan  2000      cases       2666
##  4 Afghanistan  2000 population   20595360
##  5      Brazil  1999      cases      37737
##  6      Brazil  1999 population  172006362
##  7      Brazil  2000      cases      80488
##  8      Brazil  2000 population  174504898
##  9       China  1999      cases     212258
## 10       China  1999 population 1272915272
## 11       China  2000      cases     213766
## 12       China  2000 population 1280428583
```
\ 


```r
table3
```

```
## # A tibble: 6 x 3
##       country  year              rate
## *       <chr> <int>             <chr>
## 1 Afghanistan  1999      745/19987071
## 2 Afghanistan  2000     2666/20595360
## 3      Brazil  1999   37737/172006362
## 4      Brazil  2000   80488/174504898
## 5       China  1999 212258/1272915272
## 6       China  2000 213766/1280428583
```
\ 


```r
table4a
```

```
## # A tibble: 3 x 3
##       country `1999` `2000`
## *       <chr>  <int>  <int>
## 1 Afghanistan    745   2666
## 2      Brazil  37737  80488
## 3       China 212258 213766
```
\ 


```r
table4b
```

```
## # A tibble: 3 x 3
##       country     `1999`     `2000`
## *       <chr>      <int>      <int>
## 1 Afghanistan   19987071   20595360
## 2      Brazil  172006362  174504898
## 3       China 1272915272 1280428583
```
\ 


```r
table5
```

```
## # A tibble: 6 x 4
##       country century  year              rate
## *       <chr>   <chr> <chr>             <chr>
## 1 Afghanistan      19    99      745/19987071
## 2 Afghanistan      20    00     2666/20595360
## 3      Brazil      19    99   37737/172006362
## 4      Brazil      20    00   80488/174504898
## 5       China      19    99 212258/1272915272
## 6       China      20    00 213766/1280428583
```
\ 

***

### 12.3 Spreading and Gathering 
\ 


#### 12.3.3 Exercises

**Q1.** 

Why are `gather()` and `spread()` not perfectly symmetrical?
Carefully consider the following example:


```r
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
```

```
## # A tibble: 4 x 3
##    year  half return
##   <dbl> <dbl>  <dbl>
## 1  2015     1   1.88
## 2  2015     2   0.59
## 3  2016     1   0.92
## 4  2016     2   0.17
```

```r
stocks %>% spread(year, return)
```

```
## # A tibble: 2 x 3
##    half `2015` `2016`
## * <dbl>  <dbl>  <dbl>
## 1     1   1.88   0.92
## 2     2   0.59   0.17
```

```r
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
```

```
## # A tibble: 4 x 3
##    half  year return
##   <dbl> <chr>  <dbl>
## 1     1  2015   1.88
## 2     2  2015   0.59
## 3     1  2016   0.92
## 4     2  2016   0.17
```

(Hint: look at the variable types and think about column names.)
Both `spread()` and `gather()` have a `convert` argument. What does it do?
\ 

**A:**

The functions `spread` and `gather` are not perfectly symmetrical because column type information is not transferred between them. In the original table the column `year` was `numeric`, but after the spread-gather cyle it is `character`, because with `gather`, variable names are always converted to a character vector.

The convert argument tries to convert character vectors to the appropriate type. In the background this uses the `type.convert` function.


```r
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`, convert = TRUE)
```

```
## # A tibble: 4 x 3
##    half  year return
##   <dbl> <int>  <dbl>
## 1     1  2015   1.88
## 2     2  2015   0.59
## 3     1  2016   0.92
## 4     2  2016   0.17
```

\ 

**Q2.** 

Why does this code fail?


```r
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
```

*<font color='red'>Error in inds_combine(.vars, ind_list) : Position must be between 0 and n</font>*

\ 

**A:** 

The code fails because the column names 1999 and 2000 are not standard and thus needs to be quoted. The tidyverse functions will interpret 1999 and 2000 without quotes as looking for the 1999th and 2000th column of the data frame. 

This will work: 


```r
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
```

```
## # A tibble: 6 x 3
##       country  year  cases
##         <chr> <chr>  <int>
## 1 Afghanistan  1999    745
## 2      Brazil  1999  37737
## 3       China  1999 212258
## 4 Afghanistan  2000   2666
## 5      Brazil  2000  80488
## 6       China  2000 213766
```
\ 

**Q3.**

Why does spreading this tibble fail? How could you add a new column to fix the problem?


```r
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people %>% spread(key, value)
```

*<font color='red'>Error: Duplicate identifiers for rows (1, 3)</font>*

\ 

**A:** 

Spreading the data frame fails because there are two rows with `age` for *“Phillip Woods”*. We would need to add another column with an indicator of the observation number for each row. 


```r
people <- tribble(
  ~name,             ~key,    ~value, ~obs,
  #-----------------|--------|------|------
  "Phillip Woods",   "age",       45, 1,
  "Phillip Woods",   "height",   186, 1,
  "Phillip Woods",   "age",       50, 2,
  "Jessica Cordero", "age",       37, 1,
  "Jessica Cordero", "height",   156, 1
)

people %>% spread(key, value)
```

```
## # A tibble: 3 x 4
##              name   obs   age height
## *           <chr> <dbl> <dbl>  <dbl>
## 1 Jessica Cordero     1    37    156
## 2   Phillip Woods     1    45    186
## 3   Phillip Woods     2    50     NA
```
\ 

**Q4.**

Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?


```r
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg
```

```
## # A tibble: 2 x 3
##   pregnant  male female
##      <chr> <dbl>  <dbl>
## 1      yes    NA     10
## 2       no    20     12
```

\ 

**A:**

You need to gather it. The variables are:

 + pregnant: logical (“yes”, “no”)
 + sex: char
 + count: integer


\ 

***

### 12.4 Separating and Uniting
\ 


#### 12.4.3 Exercises

**Q1.**

What do the `extra` and `fill` arguments do in separate()? Experiment with the various options for the following two toy datasets. 

\ 

**A:**
\ 
The `extra` argument tells separate what to do if there are too many pieces, and the `fill` argument if there aren’t enough. 


**`extra`**	

If `sep` is a character vector, this controls what happens when there are too many pieces. There are three valid options: 

 + *warn* (the default): emit a warning and drop extra values.  
 + *drop*: drop any extra values without a warning.  
 + *merge*: only splits at most length(into) times.   
\ 



```r
sep_tib <- tibble(x = c("a,b,c", "d,e,f,g", "h,i,j"))
sep_tib
```

```
## # A tibble: 3 x 1
##         x
##     <chr>
## 1   a,b,c
## 2 d,e,f,g
## 3   h,i,j
```


```r
sep_tib %>% 
  separate(x, c("one", "two", "three"), sep=c(','), extra="warn")
```

```
## Warning: Too many values at 1 locations: 2
```

```
## # A tibble: 3 x 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e     f
## 3     h     i     j
```


```r
sep_tib %>% 
  separate(x, c("one", "two", "three"), sep=c(','), extra="drop")
```

```
## # A tibble: 3 x 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e     f
## 3     h     i     j
```


```r
sep_tib %>% 
  separate(x, c("one", "two", "three"), sep=c(','), extra="merge")
```

```
## # A tibble: 3 x 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e   f,g
## 3     h     i     j
```
\ 

**`fill`** 

If `sep` is a character vector, this controls what happens when there are not enough pieces. There are three valid options: 

 + *warn* (the default): emit a warning and fill from the right.  
 + *right*: fill with missing values on the right.  
 + *left*: fill with missing values on the left.  
\ 


```r
sep_tib <- tibble(x = c("a,b,c", "d,e", "f,g,i")) 
sep_tib
```

```
## # A tibble: 3 x 1
##       x
##   <chr>
## 1 a,b,c
## 2   d,e
## 3 f,g,i
```


```r
sep_tib %>% 
  separate(x, c("one", "two", "three"), sep=c(','), fill="warn")
```

```
## Warning: Too few values at 1 locations: 2
```

```
## # A tibble: 3 x 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e  <NA>
## 3     f     g     i
```


```r
sep_tib %>% 
  separate(x, c("one", "two", "three"), sep=c(','), fill="right")
```

```
## # A tibble: 3 x 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e  <NA>
## 3     f     g     i
```


```r
sep_tib %>% 
  separate(x, c("one", "two", "three"), sep=c(','), fill="left")
```

```
## # A tibble: 3 x 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2  <NA>     d     e
## 3     f     g     i
```
\ 

**Q2.**

Both `unite()` and `separate()` have a `remove` argument. What does it do? Why would you set it to FALSE?
\ 

**A:** 

*`remove`* If TRUE, remove input column from output data frame. 

One would set it to FALSE to keep the source input column in the output data frame.  
You would set it to FALSE if you want to create a new variable, and also keep the old one. 

\ 

**Q3.**

Compare and contrast `separate()` and `extract()`. Why are there three variations of `separate` (by position, by separator, and with groups), but only one `unite`?
\ 

**A:** 

The function `extract` uses a regular expression to find groups and split into columns. In `unite` it is unambigous since it is many columns to one, and once the columns are specified, there is only one way to do it, the only choice is the `sep`. In `separate`, it is one to many, and there are multiple ways to split the character string.  



## 13. Relational data

### 13.1 Introduction

#### 13.1.1 Prerequisites


\ 

### 13.2 nycflights13

```
flights 
airlines
airports 
planes 
weather

```

#### 13.2.1 Exercises

\ 

### 13.3 Keys

#### 13.3.1 Exercises 


### 13.4 Mutating joins

#### 13.4.1 Understanding joins

#### 13.4.2 Inner join

#### 13.4.3 Outer joins

#### 13.4.4 Duplicate keys

#### 13.4.5 Defining the key columns

#### 13.4.6 Exercises

1. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:  
```
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
    borders("state") +
    geom_point() +
    coord_quickmap()
```
(Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)  
You might want to use the size or colour of the points to display the average delay for each airport.  
2. Add the location of the origin and destination (i.e. the lat and lon) to flights.  
3. Is there a relationship between the age of a plane and its delays?  
4. What weather conditions make it more likely to see a delay?  
5. What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.  




#### 13.4.7 Other implementations 


### 13.5 Filtering joins

* `semi_join(x, y)` keeps all observations in x that have a match in y.  
* `anti_join(x, y)` drops all observations in x that have a match in y.  


#### 13.5.1 Exercises


### 13.6 Join problems


### 13.7 Set operations



