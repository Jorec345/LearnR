library("dplyr")
library("ggplot2")
library("reshape2")
VADeaths = as.matrix(VaDeaths)
VadeathsAverage <-  melt(VADeaths)

##filter by age brackets and no of deaths
Age_NoofDeaths <- 
  VadeathsAverage %>%
  rename( Brackets = Var1,Gender.Location = Var2 ,NofDeaths = value)%>%
  filter(Brackets %in% c("60-64","65-69","70-74"), NofDeaths >= 20 )

#filter by age brackets or no of deaths
AgeOrNoofDeaths <- 
  VadeathsAverage %>%
  rename( Brackets = Var1,Gender.Location = Var2 ,NofDeaths = value)%>%
  filter(Brackets %in% c("60-64","65-69","70-74") | NofDeaths >= 20 )




##piecharts
Age_NoofDeaths <- 
  VadeathsAverage %>%
  rename( Brackets = Var1,Gender.Location = Var2 ,NofDeaths = value)%>%
  filter(Brackets %in% c("60-64","65-69","70-74"), NofDeaths >= 20 )%>%
  select(Location = Gender.Location,Deaths = NofDeaths)

Age_NoofDeaths$Location = as.character(Age_NoofDeaths$Location)

aggDf <- Age_NoofDeaths %>% group_by(Location) %>% summarize_all(sum)

pie(aggDf$Deaths,labels = aggDf$Location,main = "Deaths by Gender and Location",
    col = c("khaki","tomato","ivory","lavender"))
legend("bottomleft", aggDf$Location,fill = c("khaki",
                                             "tomato",
                                             "ivory",
                                             "lavender"), cex=.9)



plotrix::pie3D(aggDf$Deaths,labels = aggDf$Location,main = "Deaths by Gender and Location",
               col = c("khaki","tomato","ivory","lavender"), explode = 0.05)
legend("bottomleft", aggDf$Location,fill = c("khaki",
                                             "tomato",
                                             "ivory",
                                             "lavender"), cex=.9)

AgeOrNoofDeaths <- 
  VadeathsAverage %>%
  rename( Brackets = Var1,Gender.Location = Var2 ,NofDeaths = value)%>%
  filter(Brackets %in% c("60-64","65-69","70-74") | NofDeaths >= 40 )%>%
  select(Ages = Brackets, Deaths = NofDeaths)

AgeOrNoofDeaths$Ages = as.character(AgeOrNoofDeaths$Ages)

aggregatedDeaths <- 
  AgeOrNoofDeaths %>%
  group_by(Ages) %>%
  summarize_all(sum)

pie(aggregatedDeaths$Deaths,labels = aggregatedDeaths$Ages, 
    main = "Deaths by Age Bracket", col = c("red","blue","black"))
legend("bottomleft",aggregatedDeaths$Ages,fill = c("red","blue","black"))


pie(aggregatedDeaths$Deaths,labels = aggregatedDeaths$Ages, 
    main = "Deaths by Age Bracket", col = c("red","blue","black"))
legend("bottomleft",aggregatedDeaths$Ages,fill = c("red","blue","black"))
