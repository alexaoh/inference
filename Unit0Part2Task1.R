# Plotting the power function here, for sake of understanding. Looks good!
n <- 100
p <- seq(from=0, to=1,by=0.01)

power <- function(p){
  # Power function with continuous correction. 
  pnorm((40-n*p)/(sqrt(n*p*(1-p)))-0.5)+1-pnorm((60-n*p)/(sqrt(n*p*(1-p)))+0.5)
}

plot(NULL, NULL, xlim = c(0,1), ylim = c(0,2), main = "Power Function", xlab = "p", ylab = "Densities")
lines(p, pnorm((40-n*p)/(sqrt(n*p*(1-p)))), lty = 1)
lines(p, 1-pnorm((60-n*p)/(sqrt(n*p*(1-p)))), lty = 2)

lines(p, power(p), lty = 1, col = 2)
