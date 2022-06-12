library(Quandl)
library(tibble)
library(ggplot2)

# quandl_api = "6yyZXucUyPxZAy9A49Zg"
# Quandl.api_key(quandl_api)
# doge.q <-  Quandl('BITFINEX/DOGEUSD', start_date='2021-08-24', end_date='2022-05-22')



#### QUANTMOD LIBRARY ####
library(quantmod)
library(tidyquant)
library(dplyr)

tickers = c("TSLA", "BTC", "PYPL", "TWTR", "DOGE-USD")
musk.stocks <- tq_get(tickers, from = "2015-01-26",
                      to = "2022-06-05")

musk.stocks <- musk.stocks %>%
  mutate(
    name = as.factor(symbol),
    change = round((close - open), digits = 3),
    direction = case_when(change < 0 ~ "Down",
                          change >= 0 ~ "Up")) %>% 
  dplyr::select(name, date, open, high, low, close,
         volume, adjusted, direction)


#### Calculate daily returns for TSLA ####
tsla.retrn <- musk.stocks[musk.stocks$name == "TSLA",] %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "daily_return"
  )

tsla.stock <- musk.stocks[musk.stocks$name == "TSLA",] %>%
  full_join(tsla.retrn, by = c("date")) %>%
  mutate(
    daily_return = round(daily_return * 100, digits = 4),
    lag1 = lag(daily_return),
    lag2 = lag(daily_return, n = 2, na.pad = T),
    lag3 = lag(daily_return, n = 3, na.pad = T),
    lag4 = lag(daily_return, n = 4, na.pad = T)
  ) %>%
  na.omit()

# tsla.stock

#### Calculate daily returns for TWTR ####

twtr.retrn <- musk.stocks[musk.stocks$name == "TWTR",] %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "daily_return"
  )

twtr.stock <- musk.stocks[musk.stocks$name == "TWTR", ] %>%
  full_join(twtr.retrn, by = c("date")) %>%
  mutate(
    daily_return = round(daily_return * 100, digits = 4),
    lag1 = lag(daily_return),
    lag2 = lag(daily_return, n = 2, na.pad = T),
    lag3 = lag(daily_return, n = 3, na.pad = T),
    lag4 = lag(daily_return, n = 4, na.pad = T)
  ) %>%
  na.omit()

twtr.stock


data.frame(c(twtr.stock, tsla.stock))
