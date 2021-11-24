# Trying to calculate the confidence intervals manually (to match the CIs found with the library "boot").
library(boot)
books <- read.csv("bookprices.csv")
head(books)

only.Math <- books[books$Area == "Math & Science", "Price"] # Only Math book prices. 
only.Social <- books[books$Area == "Social Sciences", "Price"] # Only Social book prices. 
B <- 10000

# Manually find the bootstrap ratio of means.
b.ratio.means <- rep(NA, B)
set.seed(1)
for (i in 1:B){
  Math.resample <- sample(only.Math, replace = T)
  Social.resample <- sample(only.Social, replace = T)
  mean.math <- mean(Math.resample)
  mean.social <- mean(Social.resample)
  b.ratio.means[i] <- mean.math/mean.social 
}
boot.ratio.mean <- mean(b.ratio.means)
boot.ratio.sd <- sd(b.ratio.means)

rbind("Estimation of Bootstrap Ratio of Means" = boot.ratio.mean, "Estimation of Bootstrap Std. Err of Ratio" = boot.ratio.sd)


# First find the bootstrap ratio of means (with library boot). Also estimate the variance of each bootstrap estimate. 
N <- 500 # Iterations for Bootstrap estimate of variance. 
new.book.data <- cbind(c(only.Math, only.Social), c(rep(1,times=27), rep(2, times = 17)))
colnames(new.book.data) <- c("Price", "Area")

theta2 <- function(data, i){
  area <- data[i, 2]
  price <- data[i, 1]
  return(mean(price[area==1])/mean(price[area==2]))
}

theta3 <- function(data, i){
  area <- data[i, 2]
  price <- data[i, 1]
  b <- boot::boot(data[i,], theta2, R = N, strata = data[i,2]) 
  c(mean(price[area==1])/mean(price[area==2]), var(b$t))
}

set.seed(1)
(b <- boot::boot(new.book.data, theta3, R = B, strata = new.book.data[,2]))

###############################################################
# Confidence intervals. 

# Ratio in sample.
sample.ratio <- mean(only.Math)/mean(only.Social) 
(ciPercentile <- quantile(b.ratio.means, c(0.025, 0.975))) # Quantile with manual bootstrap. 
quantile(b$t[,1], c(0.025, 0.975)) # Quantile with boot library. Same as below. 

c(2*sample.ratio - quantile(b.ratio.means, 0.975), 2*sample.ratio - quantile(b.ratio.means, 0.025)) # Basic with manual bootstrap. 
c(2*sample.ratio - quantile(b$t[,1], 0.975), 2*sample.ratio - quantile(b$t[,1], 0.025)) # Basic with boot library. Same as below. 

# Normal, for comparison. None of them are the same as the one found with boot.ci below! Why?
(ciNorm <- qnorm(p=c(.025, .975), mean=sample.ratio, sd=boot.ratio.sd)) # Using estimated standard error manually calculated. 
(ciNorm <- qnorm(p=c(.025, .975), mean=sample.ratio, sd=sd(b$t[,1]))) # Using estimated standard error with library. 


# All confidence intervals found with library.
boot.ci(b)
