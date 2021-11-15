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
summary(only.Math)
hist(only.Math, breaks = 20)

# Data set with only Social Sciences prices.
only.Social <- data[data$Area == "Social Sciences", "Price"]
summary(only.Social)
hist(only.Social, breaks = 20)

# Math & Science.
m1 <- mean(only.Math)
med1 <- median(only.Math)
v1 <- var(only.Math)
s1 <- sum(only.Math)

# Social Sciences. 
m2 <- mean(only.Social)
med2 <- median(only.Social)
v2 <- var(only.Social)
s2 <- sum(only.Social)

df <- cbind("Math & Science" = c(m1, v1, med1, s1), "Social Sciences" = c(m2, v2, med2, s2))
rownames(df) = c("Sample Mean", "Sample Variance", "Sample Median", "Total Sum")
df

# Different amount of rows from each area. 
data %>% group_by(areaFact) %>% summarise(no = length(areaFact))

# Can see from the summaries that the data looks very different in the two groups. 
# Suggests that the prices in each group are different. 

# b) Bootstrap the mean of book price for each group separately. Describe the distributions. 
B <- 10000
b.means.Math <- rep(0, B)
set.seed(1)
for (i in 1:B){
  b.means.Math[i] <- mean(sample(only.Math, replace = T))
}

(boot.mean.Math <- mean(b.means.Math))
(boot.sd.Math <- sd(b.means.Math))
hist(b.means.Math, breaks = 100, probability = T, main = "Bootstrap Math & Science")
x <- seq(130, 180, by = 1)
lines(x, dnorm(x, mean = boot.mean.Math, sd = boot.sd.Math), col = "blue", lty = 2)
abline(v = boot.mean.Math, col = "red", lty = 1)
legend("topright", legend = c("N(Mean Boot, sd = 7.3)", "Mean Boot"), col = c("blue", "red"), lty = 2:1)

b.means.Social <- rep(0, B)
set.seed(1)
for (i in 1:B){
  b.means.Social[i] <- mean(sample(only.Social, replace = T))
}
(boot.mean.Social <- mean(b.means.Social))
(boot.sd.Social <- sd(b.means.Social))
hist(b.means.Social, breaks = 100, probability = T, main = "Bootstrap Social Sciences")
x <- seq(30, 160, by = 1)
lines(x, dnorm(x, mean = boot.mean.Social, sd = boot.sd.Social), col = "blue", lty = 2)
abline(v = boot.mean.Social, col = "red", lty = 1)
legend("topright", legend = c("N(Mean Boot, sd = 16.9)", "Mean Boot"), col = c("blue", "red"), lty = 2:1)

df.means <- cbind("Math & Science" = boot.mean.Math, "Social Sciences" = boot.mean.Social)
rownames(df.means) <- "Bootstrap Means"
df.means
df
# The bootstrap means are very similar to the sample means.

# Also done with a library.
library(bootstrap)
mean.func <- function(data){
  return(mean(data))
}
set.seed(1)
bMath <- bootstrap::bootstrap(only.Math, nboot = B, theta = mean.func)
mean(bMath$thetastar)
bSoc <- bootstrap::bootstrap(only.Social, nboot = B, theta = mean.func)
mean(bSoc$thetastar)


# How can I describe the distributions? See histograms. 

# c) Bootstrap the ratio of means and provide an estimate of the standard error of the ratio.
B <- 10000
b.ratio.means <- rep(0, B)
set.seed(1)
for (i in 1:B){
  Math.resample <- sample(only.Math, replace = T)
  Social.resample <- sample(only.Social, replace = T)
  mean.math <- mean(Math.resample)
  mean.social <- mean(Social.resample)
  b.ratio.means[i] <- mean.math/mean.social 
}
(boot.ratio.mean <- mean(b.ratio.means))
(boot.ratio.sd <- sd(b.ratio.means))
boot.mean.Math/boot.mean.Social
hist(b.ratio.means, breaks = 100)
abline(v=c(1.17, 2.4))
abline(v=boot.ratio.mean, col = "blue")
abline(v=c(1.58, 1.87))

# d) 95% confidence intervals for the ratio of means using the "percentile" and the "studentized" methods. 
# Interpret the intervals. 

# First (for comparison) use the normal distrbution approximation (should be almost the same).
(ciNorm <- qnorm(p=c(.025, .975), mean=boot.ratio.mean, sd=boot.ratio.sd))

# Confidence interval via the percentile method.
(ciPercentile <- quantile(b.ratio.means, c(0.025, 0.975)))

# Confidence interval via the studentized method. Need to estimate the quantiles. 
set.seed(1)
B <- 1000
N <- 100
z <- rep(NA, B)
for (i in 1:B){
  Math.resample <- sample(only.Math, replace = T)
  Social.resample <- sample(only.Social, replace = T) 
  mean.math <- mean(Math.resample)
  mean.social <- mean(Social.resample)
  b.ratio.means[i] <- mean.math/mean.social  
  se.resample <- rep(NA, N)
  for (j in 1:N){
    se.resample[j] <- mean(sample(Math.resample, replace = T))/mean(sample(Social.resample, replace = T))
  }
  z[i] <- sd(se.resample)
  #z[i] <- (b.ratio.means[i]-boot.ratio.mean)/sd(se.resample)
}
(mean(b.ratio.means))
(sd(b.ratio.means))
hist(z)
(ciStudentized <- c(boot.ratio.mean - quantile(z, 0.025)*boot.ratio.sd, boot.ratio.mean + quantile(z, 0.975)*boot.ratio.sd))
# Tror ikke dette er rett! Virker veldig smalt!

# With a package. Får ikke disse til å fungere!? Usikker på hvordan APIet fungerer.
library(boot)
theta <- function(data, i){
  mean(data[data$Area == "Math & Science", "Price"][i]/data[data$Area == "Social Sciences", "Price"][i])
}
boot(data, theta, R = B)
theta2 <- function(i){
  mean(data[data$Area == "Math & Science", "Price"][i]/data[data$Area == "Social Sciences", "Price"][i])
}
bootstrap::bootstrap(data, nboot = B, theta = theta2)

# e) Permutation test to compare the two bootstrap means. 
(diff.boot.means <- boot.mean.Math - boot.mean.Social) # First find the difference of means. 
perms <- 10000
diffs <- rep(1, perms)
for (i in 1:perms){
  s <- sample(data[, "Price"]) # Sample from the combined population.
  diffs[i] <- mean(s[1:27]) - mean(s[28:44]) # Keep 27 in Math and 17 in Social. 
  # Burde også bytte ut 27 med length(area = Math & Science).
}

# The null hypothesis looks to not hold that well.
hist(diffs, breaks = 100)
abline(v=diff.boot.means, col = "red", lty = 2)

# Calculate the p-value. Evidence against null hypothesis I would say, which means that the ratios are different. 
(p_value <- (sum(diffs >= diff.boot.means) + sum(diffs <= -diff.boot.means))/perms)

# Compare the result from the permutation test to an alternative based on a non-parametric test (bootstrap tests?)
