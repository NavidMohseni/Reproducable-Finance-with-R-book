#Risks
#Chapter 3
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

portfolio_sd_tidy_builtin_percent <- portfolio_returns_dplyr_byhand %>% 
  summarise(sd = sd(returns),
            sd_by_hand = sqrt(sum((returns - mean(returns))^2)/(nrow(.)-1))) %>% 
  mutate(dplyr = round(sd, 4) * 100)  

asset_returns_long %>% 
  group_by(asset) %>% 
  summarize(sd = 100 * sd(returns)) %>% 
  add_row(asset = "Portfolio", 
          sd = portfolio_sd_tidy_builtin_percent$dplyr) %>% 
  ggplot(aes(x = asset, y = sd, colour = asset)) + 
  geom_point() + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  geom_text(aes(x = "Portfolio",
                y = portfolio_sd_tidy_builtin_percent$dplyr + .2),
                color = "cornflowerblue",
            label = "Portfolio") + 
  labs(y = "Standard Deviation")

