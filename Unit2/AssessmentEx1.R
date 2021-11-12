# Data set Service, model the data using a one parameter Weibull distribution. 
setwd("/home/ajo/gitRepos/inference/Unit2")
data <- read.csv("Service.csv") # He only uploaded Salaries, not service!
plot.ecdf() 
lines(dweibull()) # etc
