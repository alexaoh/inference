X <- c(47, 34, 28, 44, 23, 32, 39, 27, 36, 33,
       28, 32, 29, 35, 30, 37, 41, 26, 52, 31)

n <- length(X)

S2 <- var(X)
X.bar <- mean(X)

beta.MOM <- (log(S2 + X.bar^2) - 2*log(X.bar))^1/2
alpha.MOM <- log(X.bar) - beta.MOM^2/2

alpha.MLE <- 1/n*sum(log(X))
beta.MLE <- (1/n*sum(log(X) - alpha.MLE)^2)^1/2

estimations <- cbind("MOM" = c(alpha.MOM, beta.MOM), "MLE" = c(alpha.MLE, beta.MLE))
rownames(estimations) <- c("Alpha", "Beta")
estimations

# Interpretations
X.log <- log(X)
mean(X.log) # Can see that this is very similar to estimations from above. 
sd(X.log) # A lot larger than estimations from above, probably because of small sample. 
