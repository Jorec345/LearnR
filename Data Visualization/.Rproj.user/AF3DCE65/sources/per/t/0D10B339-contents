library(dplyr)
library(stringr)

##consuming in the data
consumedata <- read.csv("Crime_Data.csv", header = TRUE)

##checking the data
str(consumedata)

##for each crime subcategory how many number of crimes have occurred
noofcrime <- 
  consumedata %>% 
  na.omit()%>%
  group_by(Crime.Subcategory)%>%
  count()%>%
  arrange(desc(n))%>%
  rename(crimecount = n)
  
noofcrime$crimeInPercent <- (noofcrime$crimecount/sum(noofcrime$crimecount)) * 100

##narrowing down into car prowl since it has the largest number of crimes and checking trends
##To Do : The conversion of date to year not picking.
carprowlcrimecounts <- 
  consumedata %>%
  filter(Crime.Subcategory == "CAR PROWL")%>%
  mutate(OccurredDate = as.Date(Occurred.Date,"%m/%d/%y"),
         Year = as.numeric(format(OccurredDate,"%Y"))) %>%
  group_by(Year)%>%
  summarise(count= n())%>%
  arrange(desc(count))


##asst - analysis by neighborhood
###group the data by neighbor and get no of crimes from 2015
neighborhoodcrimecount <- 
  consumedata %>%
  group_by(Neighborhood)%>%
  count()%>%
  arrange(desc(n))%>%
  rename(ncount = n)

##Neigbourhood crime count for the last 5years.From 2015
hoodcrimes <- 
  consumedata %>%
  mutate(hoodDate = as.Date(Occurred.Date, "%m/%d/%Y"), 
         year = as.numeric(format(hoodDate,"%Y")))%>%
  filter(hoodDate >= "2015/01/01") %>%
  group_by(year,Neighborhood)%>%
  count()%>%
  arrange(desc(Neighborhood),desc(n))%>%
  rename(hoodcount = n)

##monthly wise data analysis for the last decade.
monthlyanalytics <- 
  consumedata %>%
  mutate(newDate = as.Date(Occurred.Date, "%m/%d/%Y"), 
         Months = factor(months(as.Date(newDate)),levels = month.name))%>%
  filter(newDate >= "2012/01/01") %>%
  group_by(Months)%>%
  count()%>%
  arrange(Months)%>%
  rename(monthcount = n)

##monthly by yearly analysis
yearlyanalysis <- 
  consumedata %>%
    mutate(newDate = as.Date(Occurred.Date, "%m/%d/%Y"), 
           Months = factor(months(newDate),levels = month.name),
           Year = as.numeric(format(newDate,"%Y")))%>%
    filter(newDate >= "2012/01/01") %>%
    group_by(Year, Months)%>%
    count()%>%
    arrange(Months, Year,desc(n))

##analysis by time
##My To Do - read through summarize and mutate, dplyr
#1.) Analysis by time and crime subcategory
analysisbyTime <- 
  consumedata %>%
    mutate(strtime = str_pad(Occurred.Time, 4, pad = "0"),
           newtime = strptime(strtime, format = "%H%M"),
           hours = as.numeric(format(newtime, "%H"))) %>%
    group_by(Crime.Subcategory,hours) %>%
    summarise(nocrimes = n()) %>%
    arrange(desc(nocrimes))
  
##Analysis of the highest crime  which is car prowl by hour of the day.
higherstCrimeByHr <- 
  consumedata %>%
      filter(Crime.Subcategory == "CAR PROWL")%>%
      mutate(strtime = str_pad(Occurred.Time, 4, pad = "0"),
             newtime = strptime(strtime, format = "%H%M"),
             hours = as.numeric(format(newtime, "%H"))) %>%
      group_by(hours) %>%
      summarise(nocrimesCR = n()) %>%
      arrange(desc(nocrimesCR))

##analysis by day of the week
noOfcrimeByday <- 
  consumedata %>%
      mutate(newdate = as.Date(Occurred.Date, format = "%m/%d/%Y"),
             dayofweek = weekdays(newdate)) %>%
      group_by(dayofweek)%>%
      count()%>%
      arrange(desc(n))

##analysis by neighborhood considering day of the week
neighDayanalysis <- 
  consumedata %>%
    mutate(newdate = as.Date(Occurred.Date, format = "%m/%d/%Y"),
           dayofweek = weekdays(newdate)) %>%
    group_by(Neighborhood,Crime.Subcategory, dayofweek)%>%
    count()%>%
    arrange(desc(n))

##asst
##analyze neighborhoods by time 

neighTimeanalysis <- 
  consumedata %>%
    mutate(strtime = str_pad(Occurred.Time, 4, pad = "0"),
           newtime = strptime(strtime, format = "%H%M"),
           hours = as.numeric(format(newtime, "%H"))) %>%
    group_by(Neighborhood,hours) %>%
    count()%>%
    arrange(desc(n))


#analysis of the day of week for carprowl
carprollAnalysis <- 
  consumedata %>%
  filter(Crime.Subcategory == "CAR PROWL")%>%
    mutate(newdate = as.Date(Occurred.Date, format = "%m/%d/%Y"),
            dayofweek = weekdays(newdate)) %>%
    group_by(Crime.Subcategory, dayofweek)%>%
    count()%>%
    arrange(desc(n))




  








