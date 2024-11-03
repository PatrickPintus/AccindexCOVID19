rm(list=ls())

## Load deSolve package
library(deSolve)

### ONE REGION + TESTS

sir.national.4.tests <- function(time, state, parms) {
    with(as.list(c(state, parms)), {
    # Détection
    epsilon <- 1/100    # share of test in population
    alpha <- 1          # convexity of number of tests
    mu <- 1             # convexity of test share
    pi <- 1              # share of positives who are isolated
    # Effet de la détection
    if ( ((epsilon*NN)^alpha)*((I1/(I1+S1+R1))^mu )  > epsilon * NN ) {
    dS1 <- -beta1 * S1 * (I1+(1-pi)*P1)
    dI1 <-  beta1 * S1 * (I1+(1-pi)*P1) - gamma * I1 - epsilon * NN 
    dR1 <-                      gamma * (I1+P1) 
    dP1 <- epsilon*NN -  gamma*P1
    } else {
    dS1 <- -beta1 * S1 * (I1+(1-pi)*P1)   
    dI1 <-  beta1 * S1 * (I1+(1-pi)*P1) - gamma * I1 - ((epsilon*NN)^alpha)*((I1/(I1+S1+R1))^mu)
    dR1 <-                      gamma * (I1+P1)    
    dP1 <- ((epsilon*NN)^alpha)*((I1/(I1+S1+R1))^mu) - gamma*P1
    }
### alternative test technology 1: ((epsilon*NN)^alpha)*((I1/(I1+S1+R1))^(2-alpha))
### alternative test technology 2: epsilon*NN*(I1^alpha)/(I1^alpha+S1^alpha+R1^alpha) 
    return(list(c(dS1, dI1, dR1, dP1)))
  })
}

# Population size
NN <- 10000

# Proportion in each compartment:
init1 <- c(S1 = NN*0.995, I1 = NN*0.005, R1 = NN*0, P1 = NN*0)

## beta: infection parameter; gamma: recovery parameter

# Infection duration
DD <- 28
# Number of contacts 
kappa <- 4/DD
# Fraction that result in transmission
tau   <- 1
# Disease transmission rate

beta <- kappa*tau/NN

parameters <- c(beta1 = beta,
                gamma = 1/DD,
                delta1=.0)

## Time frame
times      <- seq(0, 200, by = 1)

## Simulation

out <- ode(y = init1, times = times, func = sir.national.4.tests,
           parms = parameters)

par(mfrow=c(2,2))

## Plot Region 1: S I R
matplot(x = times, y = out[,2:4]*100/NN, type = "l",
        xlab = "Time", ylab = "S I R (% pop)",
        lwd = 3, lty = 1, bty = "l", col = 2:5,
        cex.lab=1.5,cex.axis=1.5,cex.main=1.5)

## Plot Region 1: I + P 
matplot(x = times, y = (out[,3]+out[,5])*100/NN, type = "l",
        xlab = "Time", ylab = "I + P (% pop)",
        lwd = 3, lty = 1, bty = "l", col = 6,
        cex.lab=1.5,cex.axis=1.5,cex.main=2)

## Plot Region 1: P
matplot(x = times, y = out[,5]*100/NN, type = "l",
        xlab = "Time", ylab = "Tested Positives (% pop)",
        lwd = 3, lty = 1, bty = "l", col = 7,
        cex.lab=1.5,cex.axis=1.5,cex.main=2)

## Plot Region 1: I
matplot(x = times, y = out[,3]*100/NN, type = "l",
                  #(out[,2]+out[,3]+out[,4]+out[,5])*100/NN, type = "l",
			#(out[,3]^alpha+out[,2]^alpha+out[,4]^alpha)
        xlab = "Time", ylab = "Infected (% pop)",
        lwd = 3, lty = 1, bty = "l", col = 3,
        cex.lab=1.5,cex.axis=1.5,cex.main=2)