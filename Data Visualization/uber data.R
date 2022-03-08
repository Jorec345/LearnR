library(dplyr)
myUberData <- read.csv("Uber_Data.csv",skip = 5,header = TRUE)
str(myUberData)
head(myUberData)


NewUberData <- myUberData[,-1]
str(NewUberData)
inputDate <- NewUberData$Request.date..UTC.

dateConverter <-  function(inputDate, date.Format = "%Y/%m%d"){
  if (is.na(as.Date(inputDate[1],date.Format))){
    NewDate = as.Date(inputDate,"%Y-%m-%d")
  }else{
    NewDate = as.Date(inputDate,"%m/%d/%Y")
  }
  
  return(NewDate)
}

dateConverter(inputDate)

NewUberData %>%
  mutate(TransactionTime = dateConverter(Transaction.timestamp..UTC.),
         RequestDate = dateConverter(Request.date..UTC.)),
         RequestTime = strftime(format = "%H/%M"),
         RequestDateLocal = as.Date(Re))


