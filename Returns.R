#Returns
#Chapter 2
library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
prices <- getSymbols(symbols, 
                     src = "yahoo",
                     from = "2012-12-31",
                     to = "2017-12-31",
                     auto.assign = TRUE,
                     warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)
head(prices)

asset_returns_dplyr_byhand <- 
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  data.frame(date = index(.)) %>% 
  remove_rownames() %>% 
  gather(asset, prices, -date) %>% 
  group_by(asset) %>% 
  mutate(returns = (log(prices) - log(lag(prices)))) %>% 
  select(-prices) %>% 
  spread(asset, returns) %>% 
  select(date, symbols)

asset_returns_dplyr_byhand <- 
  asset_returns_dplyr_byhand %>% na.omit()

asset_returns_long <- 
  asset_returns_dplyr_byhand %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset)

asset_returns_long %>% 
  ggplot(aes(x = returns, fill = asset)) + 
  geom_histogram(alpha = 0.45, binwidth = .005) + 
  ggtitle("Monthly Returns Since 2013")

asset_returns_long %>% 
  ggplot(aes(x = returns, fill = asset)) + 
  geom_histogram(alpha = 0.45, binwidth = .005) + 
  ggtitle("Monthly Returns Since 2013") +
  facet_wrap(~asset)

asset_returns_long %>% 
  ggplot(aes(x = returns, color = asset)) + 
  geom_density(alpha = 1) + 
  ggtitle("Monthly Returns Density Since 2013") + 
  xlab("Monthly Returns") + 
  ylab("Distribution")

asset_returns_long %>% 
  ggplot(aes(x = returns)) + 
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = 0.01) + 
  guides(fill = FALSE) + 
  facet_wrap(~asset) + 
  ggtitle("Monthly Returns") +
  xlab("Monthly Returns") + 
  ylab("Distribution")

#Chapter 3
w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)
tibble(w, symbols)
w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]

asset1 <- asset_returns_dplyr_byhand[,2]
asset2 <- asset_returns_dplyr_byhand[,3]
asset3 <- asset_returns_dplyr_byhand[,4]
asset4 <- asset_returns_dplyr_byhand[,5]
asset5 <- asset_returns_dplyr_byhand[,6]
portfolio_returns_byhand <-
  (w_1 * asset1) +
  (w_2 * asset2) +
  (w_3 * asset3) +
  (w_4 * asset4) +
  (w_5 * asset5)
names(portfolio_returns_byhand) <- "returns"

asset_returns_long %>% 
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5])) %>% 
  head()

portfolio_returns_dplyr_byhand <- asset_returns_long %>% 
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>% 
  group_by(date) %>% 
  summarise(returns = sum(weighted_returns))

portfolio_returns_dplyr_byhand %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue")+
  xlab("date") +
  ylab("monthly return") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))  

portfolio_returns_dplyr_byhand %>% 
  ggplot(aes(x = returns)) + 
  geom_histogram(colour = "cornflowerblue",
                 fill = "cornflowerblue", 
                 binwidth = .005) + 
  geom_density(alpha = 1, color = "red") +
  ggtitle("Portfolio Returns Distribution")

asset_returns_long %>% 
  ggplot(aes(x = returns, fill = asset)) + 
  geom_histogram(alpha = 0.15, binwidth = .01) +
  geom_histogram(data = portfolio_returns_dplyr_byhand, 
                 fill = "cornflowerblue", 
                 binwidth = .01) + 
  ggtitle("Portfolio and Asset Monthly Returns")
