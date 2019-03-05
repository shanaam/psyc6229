# ccdemo.R  Simulation of cue combination experiments; shows mean and standard deviation
#           of optimally combined cues in several cue conflict conditions

rm( list = ls() )

# create a function to simulate many optimal cue combination trials
ccsim <- function( mu1, sigma1, mu2, sigma2, ntrials=10000 ) {
	
	# get samples from the two cues
	x1 <- rnorm( ntrials, mu1, sigma1 )
	x2 <- rnorm( ntrials, mu2, sigma2 )
	
	# combine the samples optimally
	w1 = (1/sigma1^2) / ( (1/sigma1^2) + (1/sigma2^2) )
	w2 = (1/sigma2^2) / ( (1/sigma1^2) + (1/sigma2^2) )
	x <- w1*x1 + w2*x2
	
	# return the mean and standard deviation of the optimal combination
	v = list( mu=mean(x), sigma=sd(x) )
	return( v )
	
}

# find PSEs and cue standard deviations in a simulated cue conflict experiment

# define the single cue distributions
muA <- 10
sigmaA <- 1.5
muBlist <- 5:15
sigmaB <- 2

# get PSE and SD in all cue conflict conditions
pse   <- rep(NaN,length(muBlist))
cuesd <- rep(NaN,length(muBlist))
for( i in 1:length(muBlist) ) {
	v <- ccsim( muA, sigmaA, muBlist[i], sigmaB )
	pse[i] <- v$mu
	cuesd[i] <- v$sigma
}

# plot PSEs
plot( muBlist, pse, type='o', col='red', xlab='mean of cue B', ylab='PSE' )
abline(h=muA,lty=2)
text(muBlist[3],muA,'mean of cue A',pos=3)

# plot combined cue SDs
plot( muBlist, cuesd, type='o', col='red', ylim=c(0,1.2*pmax(sigmaA,sigmaB)), xlab='mean of cue B', ylab='stdev of combined cue' )
abline(h=c(sigmaA,sigmaB),lty=2)
text(muBlist[3],sigmaA,'sigmaA',pos=3)
text(muBlist[3],sigmaB,'sigmaB',pos=3)
