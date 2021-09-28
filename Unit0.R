# Simulation of the properties of Problem 9.


# Setting some of the parameters to some arbitrary values.
sigma <- 1
mu_1 <- 1
mu_2 <- 2 # To show that both expectations do not have to be equal. 

# (a)
V <- rep(0,1000)
for (n in 1:1000){
  # Sampling X and Y from their respective distributions. 
  X <- rnorm(n, mu_1, sigma^2)
  Y <- rnorm(n, mu_2, 3*sigma^2)
  V[n] <- 1/(2*sigma)*sqrt(n)*(mean(X)-mean(Y)-mu_1+mu_2)
}

par(mfrow=c(1,1))
plot(V, col = 1, lty = 1, main = "Scatter plot of simulated V and N(0,1) (n=1000)")
points(rnorm(1000), col = 2, lty = 1)
legend(0, 4, c("V_n", "N(0,1)"), col = c(1,2), lty = 1)

par(mfrow=c(1,2))
hist(V, breaks = 100)
hist(rnorm(1000), col = 2, breaks = 100)

# (b)
N <- 1000
Tn <- rep(0,N)
for (n in 1:N){
  # Sampling X and Y from their respective distributions. 
  X <- rnorm(n+1, mu_1, sigma^2)
  Y <- rnorm(n+1, mu_2, 3*sigma^2)
  Tn[n] <- var(X) + 1/3*var(Y)
}

chi_sim <- (N-1)*Tn/sigma^2
par(mfrow=c(1,2))
hist(chi_sim, breaks = 100, main = "Histogram of (n-1)Tn/sigma^2 (N=1000)")
hist(rchisq(N, 2*(N-1)), col = 2, breaks = 100)

par(mfrow=c(1,1))
plot(chi_sim, col = 1, lty = 1, main = "Scatter plot of simulated Tn and rqchisq (n=1000)")
points(rchisq(N, 2*(N-1)), col = 2, lty = 1)
legend(0, 1200, c("T_n", "rchisq(1000,2*(1000-1))"), col = c(1,2), lty = 1)

# Cannot figure out what is wrong here!? Used the same code in (d) and the results seem to make sense there.


# (c)
# Simulate if Vn and Tn are independent? How?

# (d)
N <- 1000
Tn <- rep(0,N)
Un <- rep(0,N)
for (n in 1:N){
  # Sampling X and Y from their respective distributions. 
  X <- rnorm(n+1, mu_1, sigma^2)
  Y <- rnorm(n+1, mu_2, 3*sigma^2)
  Tn[n] <- var(X) + 1/3*var(Y)
  Un[n] <- sqrt(n)*(mean(X)-mean(Y)-mu_1+mu_2)/sqrt(Tn[n])
}

par(mfrow=c(1,2))
hist(Un, breaks = 100, main = "Histogram of Un (N=1000)")
hist(rt(N, 2*(N-1)), col = 2, breaks = 100)

par(mfrow=c(1,1))
plot(Un, col = 1, lty = 1, main = "Scatter plot of simulated Un and rt (n=1000)")
points(rt(N, 2*(N-1)), col = 2, lty = 1)
legend(0, 1200, c("T_n", "rchisq(1000,2*(1000-1))"), col = c(1,2), lty = 1)

# (e)
# Simulate convergence in probability. Possible?

# (f)
# Simulate convergence in law. Possible?
