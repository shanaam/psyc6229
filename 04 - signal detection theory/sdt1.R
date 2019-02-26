# sdt1.R  Simulation of a signal detection model of a yes-no task

# clear workspace
rm( list=ls() )

# set model observer parameters
mu1 <- 0
mu2 <- 3
sigma <- 2
criterion <- 1.5

# calculate d' and c from observer parameters
dprimecalc <- (mu2 - mu1)/sigma
ccalc <- (criterion - (mu1 + mu2)/2)/sigma

# set experiment parameters
ntrials <- 10000

# initialize data frame
init <- rep(NA, ntrials)
trials <- data.frame(signal = init, response = init, dvar = init)

# run trials; we could do this more efficiently as a matrix operation,
# but here we mimic the procedure in an experiment with human observers
for (t in 1:ntrials) {
    
    # choose a signal
    trials$signal[t] <- 1 + (runif(1) < 0.5)
    
    # get the decision variable
    if (trials$signal[t] == 1) 
        trials$dvar[t] <- rnorm(1, mean = mu1, sd = sigma)
    else
        trials$dvar[t] <- rnorm(1, mean = mu2, sd = sigma)
    
    # more concisely:  trials$dvar[t] <- rnorm( 1, mean=ifelse( trials$signal[t]==1, mu1, mu2 ), sd=sigma )
    
    # make a response
    trials$response[t] <- 1 + (trials$dvar[t] > criterion)
    
}

# find the hit rate and false alarm rate
hit <- mean(trials$response[trials$signal == 2] == 2)
fa  <- mean(trials$response[trials$signal == 1] == 2)

# calculate d' and c from data
dprimehat <- qnorm(hit) - qnorm(fa)
chat <- -0.5 * (qnorm(hit) + qnorm(fa))

# compare true and empirical d'
cat("dprimecalc =", dprimecalc, "\n")
cat("dprimehat  =", dprimehat, "\n\n")

# compare true and empirical c
cat("ccalc =", ccalc, "\n")
cat("chat  =", chat, "\n\n")

# plot decision variable distributions
hist1 <- hist( trials$dvar[ trials$signal==1 ], breaks=20, plot=FALSE )
hist2 <- hist( trials$dvar[ trials$signal==2 ], breaks=20, plot=FALSE )
plot(  hist1$mid, hist1$density, type='o', col='red', xlab='decision variable', ylab='density' )
lines( hist2$mid, hist2$density, type='o', col='green' )
abline( v=criterion, col='blue' )
