library("dplyr")
library("ggplot2")
library("reshape2")
VADeaths = as.matrix(VaDeaths)
VadeathsAverage <-  melt(VADeaths)

##filter by age brackets and no of deaths
ggplot(longformart,aes(fill = Var2,x = Var1,y = value))+
  geom_bar(position = "dodge",stat = "identity")+
  ggtitle("Male-Female Deaths",
          subtitle = 'Comparing Male-Female Deaths by Age Group')

##facet wrap
ggplot(VadeathsAverage, aes(fill = Var2, x = Var1, y = value))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Male-Female Deaths By Gender", 
          subtitle = 'comparing Male-Female Deaths by Age Group and Gender')+
  facet_wrap(~Var2)

