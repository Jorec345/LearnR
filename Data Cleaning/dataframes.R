library("tibble")
library(dplyr)

quantsData <- data.frame(
  Quants = c("Joy Akinyi","Maggy Maina","Andrew Abok","Calvin Mwange","Brian Ouchoh"),
  Age = c(21,25,30,24,23),
  heights = c(180.0,175.0,183.4,182.0,179.0),
  Gender = c("F","F","M","M","M"))

##adding a column after heights
quantsData <- quantsData %>%
  add_column(status = c("s","m","m","s","s"),.after = "heights" )

quantsData

##rename quants - names, heights - height, 
names(quantsData)[c(1,3,4)] <- c("Names","Height","Status")
quantsData

##vectorization of operations
fullstatus <- function(inputcol){
  teamsreturn <- vector()
  for (i in 1:length(inputcol)) {
    
    if(inputcol[i] == "s"){
      teamsreturn[i] <- "Single"
    }else{
      teamsreturn[i] <- "Married"
    }
  }
  return(teamsreturn)
}
quantsData$Status <- fullstatus(quantsData$Status)
quantsData

##asst
##change gender to be in full.
##make the names- both names

fullnames <- function(inputcol){
  genderfull <- vector()
  for (i in 1:length(inputcol)) {
    
    if(inputcol[i] == "M"){
      genderfull[i] <- "Male"
    }else{
      genderfull[i] <- "Female"
    }
    
  }
  return(genderfull)
}
quantsData$Gender <- fullnames(quantsData$Gender)
quantsData

salaries <- as.numeric(c(100000.0,150000.0,130000.0,148000.0,170000.0))
quantsData <- cbind(quantsData,salaries)


###############running analysis##############

avgSaloGender <- 
   quantsData %>%
   filter(Gender == "Female")%>%
   select(c(salaries,Gender)) %>%
   summarize(mean = mean(salaries))

avgmale <- 
  quantsData %>%
  filter(Gender == "Male")%>%
  select(c(salaries,Gender))%>%
  summarise(mean = mean(salaries))

###Group by operations 
grpBySex <- 
  quantsData %>% 
  group_by(Gender)%>%
  summarize(meanSalary = mean(salaries),meanAge = mean(Age))

grpsum <- 
  quantsData %>%
  group_by(Gender) %>%
  summarise_at(c("Age","Height","salaries"),mean)

               








