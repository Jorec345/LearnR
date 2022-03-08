##write a function that prints the mean, the SD, the skewness and the kurtosis
heights = c(180.0,175.0,183.4,182.0,179.0)
mystats <- function(x){
  if (length(x)== 0){
    stop("failed operation")
  }
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- (sum((x-m)^3/s^3))/n
  kurt <- (sum((x-m)^4/s^4))/n
  excesskurt <- (sum((x-m)^4/s^4))/n-3
  return(c(mean_h = m, len_h = n,standev = s,skewness = skew,
           kurtosis = kurt,excesskurtosis = excesskurt))
}
ourvar <- vector()
mystats(heights)



#stdev = s, skewness = skew, kurtosis =  kurt, 
#excesskurtosis = excesskurt)
#}

##asst
###group the data by neighbor and get no of crimes from 2015



  
  
  
  
