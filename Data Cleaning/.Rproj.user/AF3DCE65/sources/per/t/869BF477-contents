##testing for normality
library(dplyr)
library("ggpubr")
##consuming in data
safdata <- read.csv("safaricom_Prices.csv", header = TRUE)

returns_data <- 
  safdata %>%
  mutate(nsafdate = as.Date(MarketDate,format = "%d/%m/%Y"),
         returnsdata = (NewPrices/MarketPrice)-1) %>%
  filter(nsafdate > "2019/01/01") %>%
  select(returnsdata)

str(returns_data)
##1. create a histogram
hist(returns_data$returnsdata)


##create a q-q plot

qqnorm(returns_data$returnsdata, main="QQ plot of normal data")


##3.Perform a Shapiro- wilk test
shapiro.test(returns_data$returnsdata)
##pvalue is less than alpha

##4. perform kolmogorov smirnov test
ks.test(returns_data$returnsdata, 'pnorm')
##pvalue is less than alpha







