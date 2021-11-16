# Data set Service, model the data using a one parameter Weibull distribution. 
setwd("/home/ajo/gitRepos/inference/Unit2")
data <- read.csv("Service.csv") 
str(data)
head(data)
summary(data)

# a) Plot the empirical cdf and the theoretical distribution. What value should be used as scale-parameter?
x <- seq(0, max(data$Times), by = 0.001) 
plot.ecdf(data$Times) 
lines(x, pweibull(x, shape = 2, scale = sqrt(mean(data$Times^2))), lty = 2, col = "blue") # MLE estimator for theta..
lines(x, pweibull(x, shape = 2, scale = 2*mean(data$Times)/sqrt(pi)), lty = 3, col = "red") # MOM-estimator for theta. 
legend("topleft", legend = c("MLE", "MOM"), lty = 3:2, col = c("blue", "red"))

# The fit is alright I suppose. 

# b) Goodness of fit. 