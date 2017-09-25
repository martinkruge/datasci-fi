library(quantmod)
library(purrr)
library(broom)
library(dplyr)

start_date <- "2010-01-01"
end_date <- "2017-07-31"
stocks <- c("AAPL", "GOOG", "INTC", "FB", "MSFT", "TWTR")

stock_data <- stocks %>% 
  map(
    getSymbols, 
    from = start_date, to = end_date, 
    auto.assign = FALSE, src = "google"
  ) %>% 
  map(tidy) %>% 
  setNames(stocks)

save(stock_data, file = "stock_data.RData")


stock_data$GOOG$index

volume_data <- stock_data[['GOOG']] %>% 
  filter(
    between(index, as.Date('2013-12-01'), as.Date('2013-12-10')),
    !grepl("Volume", series)
  )

