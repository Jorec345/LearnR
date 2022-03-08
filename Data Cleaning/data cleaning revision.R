library(dplyr)


#consume in the data
safdata <- read.csv("safaricom_Prices.csv", header = TRUE)
str(safdata)

#
#create a column for returns
returns_data <- 
safdata%>%
  mutate(nsafdate = as.Date(MarketDate,format = "%d/%m/%Y"),
         returnsdata = (NewPrices/MarketPrice)-1)%>%
  filter(nsafdate > "01/01/2019")%>%
  select(nsafdate,returnsdata)


##data visualization

bardata <- table(safdata$NewPrices)
barplot(bardata)
barplot(bardata, main = "simple bar plot", xlab = "saf prices", 
        ylab = "frequency")


