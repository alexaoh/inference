# Data on hardcover textbooks from Areas Math & Science and Social Sciences. 
library(dplyr)

setwd("/home/ajo/gitRepos/inference/Unit2")
data <- read.csv("bookprices.csv")
head(data)
str(data)
summary(data)

data$areaFact <- as.factor(data$Area)

# a) Exploratory data analysis on book prices for each of the two disciplinary areas. 

# Data set with only Math & Science prices.
only.Math <- data[data$Area == "Math & Science", "Price"]

# Data set with only Social Sciences prices.
only.Social <- data[data$Area == "Social Sciences", "Price"]

# Math & Science.
m1 <- mean(only.Math)
v1 <- var(only.Math)
s1 <- sum(only.Math)

# Social Sciences. 
m2 <- mean(only.Social)
v2 <- var(only.Social)
s2 <- sum(only.Social)

df <- cbind("Math & Science" = c(m1, v1, s2), "Social Sciences" = c(m2, v2, s2))
rownames(df) = c("Sample Mean", "Sample Variance", "Total Sum")
df

# Different amount of rows from each area. 
data %>% group_by(areaFact) %>% summarise(no = length(areaFact))

# What else can I do (EDA)?

# b) Bootstrap the mean of book price for each group separately. Describe the distributions. 
B <- 10000
b.means.Math <- rep(0, B)
set.seed(1)
for (i in 1:B){
  b.means.Math[i] <- mean(sample(only.Math, replace = T))
}
boot.mean.Math <- mean(b.means.Math)

b.means.Social <- rep(0, B)
set.seed(1)
for (i in 1:B){
  b.means.Social[i] <- mean(sample(only.Social, replace = T))
}
boot.mean.Social <- mean(b.means.Social)

df.means <- cbind("Math & Science" = boot.mean.Math, "Social Sciences" = boot.mean.Social)
rownames(df.means) <- "Bootstrap Means"
df.means
df
# The bootstrap means are very similar to the sample means.

# Also done with a library.
library(boot)
mean.func <- function(data, i){
  return(mean(data))
}
set.seed(1)
boot(only.Math, statistic = mean.func, R = B)
set.seed(1)
boot(only.Social, statistic = mean.func, R = B)

# How can I describe the distributions?

# c) Bootstrap the ratio of means and provide an estimate of the standard error of the ratio.
B <- 10000
b.ratio.means <- rep(0, B)
set.seed(1)
for (i in 1:B){
  mean.math <- mean(sample(only.Math, replace = T))
  mean.social <- mean(sample(only.Social, replace = T))
  b.ratio.means[i] <- mean.math/mean.social 
}
boot.ratio.mean <- mean(b.ratio.means)
boot.ratio.sd <- sd(b.ratio.means)
boot.ratio.mean
boot.mean.Math/boot.mean.Social
boot.ratio.sd

# d) 95% confidence intervals for the ratio of means using the "percentile" and the "studentized" methods. 
# Interpret the intervals. 
