# problem2.R  Solution to topic 7, problem 2

rm( list=ls() )

# (a) make up some data with lapse rate lambda = 0.05

# make the theoretical psychometric function
psymet <- function( x ) 0.05 + 0.95*pnorm( x, 0.40, 0.20 )

# choose some stimulus levels and the number of trials at each level
stimlev <- seq( 0.05, 0.90, by=0.10 )
ntrials <- rep( 40, length(stimlev) )
df <- data.frame( stimlev, ntrials )

# sample a number of correct trials at each stimulus level from the binomial distribution
ptheory <- psymet( stimlev )
df$ncorrect <- rbinom( length(ntrials), ntrials, ptheory )
df$pcorrect <- df$ncorrect / df$ntrials

# plot the data
par( mfrow=c(1,2) )
plot( df$stimlev, df$pcorrect, ylim=c(-0.1,1.1), xlab='stimulus level', ylab='proportion correct' )


# (b) fit a cumulative normal psychometric function without a lapse rate parameter and find
#     the deviance of the fit

# find the maximum likelihood fit
fitfn <- function( x, mu, sigma ) pnorm( x, mean=mu, sd=sigma )
errfn <- function( p ) -sum(log( dbinom( df$ncorrect, df$ntrials, fitfn( df$stimlev, p[1], p[2] ) ) ))
param <- optim( c(0.30,0.20), errfn )$par

# plot the fit
curve( fitfn( x, param[1], param[2] ), col='red', add=TRUE )

# find the deviance between the data and the fit
pfit <- fitfn( stimlev, param[1], param[2] )
deviance <- 2*( sum(log(dbinom( df$ncorrect, df$ntrials, df$pcorrect )))  # saturated fit
              - sum(log(dbinom( df$ncorrect, df$ntrials, pfit ))) )       # actual fit


# (c) find the deviance distribution

# initialize vector for deviance
nrepeat <- 10000
deviance_star <- rep( NaN, nrepeat )

# find distribution of deviance
for (i in 1:nrepeat) {
    
	# make up data for a new psychometric function, based on the fitted function
	ncorrect_star <- rbinom( length(df$ntrials), df$ntrials, pfit )
	pcorrect_star <- ncorrect_star / df$ntrials
	
	# find deviance of simulated data
	deviance_star[i] <- 2*( sum(log(dbinom( ncorrect_star, df$ntrials, pcorrect_star )))
	                      - sum(log(dbinom( ncorrect_star, df$ntrials, pfit ))) )
	
}

# show distribution of deviance
hist( deviance_star, breaks=25, probability=TRUE )


# (d) is the deviance from part (b) implausibly far out in the distribution?

# show the deviance we measured on the simulated distribution
abline( v=deviance, col='green' )

# find the p level of the deviance we measured
plevel = mean( deviance_star > deviance )
print(sprintf('plevel = %.4f',plevel))

# typically the p level is not very low, so we do not reject the goodness of fit.
# apparently it is difficult to tell whether we need a lapse rate parameter just
# by checking the deviance -- at least with this dataset.
