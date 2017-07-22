library(tidyverse)
library(lubridate)

dat <- read_csv("~/Documents/Kaggle/zillow/data/train_2016_v2.csv")


props <- read_csv("~/Documents/Kaggle/zillow/data/properties_2016.csv")

sample_sub <- read_csv("~/Documents/Kaggle/zillow/sample_submission.csv")


props %>% head(1000) %>% inner_join(dat) %>% View

dat <- dat %>% 
  mutate(month = month(transactiondate),
         year = year(transactiondate))

dat %>% mutate(pred = paste0(year, month)) %>% 
  View


dat %>% 
  group_by(parcelid, month, year) %>% 
  count %>%  
  filter(n > 1) %>%
  nrow


log(zest) - log(sale) = log(zest/sale) = 5

zest/sale = exp(4.7)

zest = sale * exp(5)


dat %>% group_by(transactiondate) %>% 
  summarise(avg = mean(logerror),
            max = max(logerror),
            min = min(logerror),
            ub = avg + 2 * sd(logerror),
            lb = avg - 2 * sd(logerror)
            ) %>%
  gather(key = key, val = val, avg:lb) %>% 
  ggplot(aes(x = transactiondate, y = val, color = key)) +
  geom_line() + 
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.5, colour = "#a0a0a0"))

library(fpp)
library(magrittr)
dat %>% group_by(transactiondate) %>% 
  summarise(med = median(logerror)) %>%
  pull(med) %T>% adf.test %>% kpss.test


auto.arima(dat %>% group_by(transactiondate) %>% 
             summarise(med = median(logerror)) %>%
             pull(med))
















