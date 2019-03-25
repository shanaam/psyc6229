# deviance.R  Illustrate a goodness of fit test for a psychometric function

rm( list=ls() )

# make up some behavioural data for a discrimination task
df <- data.frame( stimlev=seq( 0.1, 1.0, by=0.1 ),                  # stimulus levels
                  ntrials=rep( 20, times=10 ),                      # number of trials at each stimulus level
                  nhigher=c( 1, 0, 4, 5, 8, 10, 12, 14, 19, 20 ) )  # number of trials where the observer responded "higher"
df$phigher <- df$nhigher / df$ntrials                               # 'nhigher' expressed as a proportion

# choose the form of the psychometric function
psyfn <- function( x, mu, sigma )
    pnorm( x, mu, sigma )

# define a function to calculate the log likelihood of a dataset
loglike <- function( nhigher, ntrials, phigher )
    sum(log( dbinom( nhigher, ntrials, phigher ) ))

# define a maximum likelihood objective function
# - note that this is the same objective function we used in previous scripts,
#   formulated using the loglike() function
obj <- function( p, data )
    -loglike( data$nhigher, data$ntrials, psyfn( data$stimlev, p[1], p[2] ) )

# fit the objective function
pinit <- c( 0.5, 0.2 )                          # initial guess
phat <- optim( pinit, obj, NULL, data=df )$par  # find parameters of best fit

# show data and fitted function
par( mfrow=c(1,2) )
plot( df$stimlev, df$phigher, col='black', xlim=c(0,1), ylim=c(0,1) )
curve( psyfn( x, phat[1], phat[2] ), col='red', add=TRUE )

# get fitted probabilities
pfit <- psyfn( df$stimlev, phat[1], phat[2] )

# define a function that gives the deviance of a dataset
devfn <- function( nhigher )
    2*( loglike( nhigher, df$ntrials, nhigher/df$ntrials ) -   # log likelihood of perfect fit
        loglike( nhigher, df$ntrials, pfit ) )                 # log likelihood of actual fit

# find the deviance of the actual data
dev <- devfn( df$nhigher )

# find distribution of deviance
devn <- 10000
devstar <- rep( NaN, devn )
for (i in 1:devn) {
    
    # resample data
    dfstar <- df
    dfstar$nhigher <- rbinom( nrow(dfstar), dfstar$ntrials, pfit )
    dfstar$phigher <- dfstar$nhigher / dfstar$ntrials
    
    # find deviance of resampled data
    devstar[i] <- devfn( dfstar$nhigher )
    
}

# show distribution of deviance
hist( devstar, breaks=25, probability=TRUE )
abline( v=dev, col='red' )

# find the p level of the deviance of the actual data
plevel = mean( devstar > dev )
print(sprintf('plevel = %.4f',plevel))

# show asymptotic distribution of deviance; limit as number of stimulus levels --> Inf  (Wichmann & Hill, 2001a, p. 1303)
curve( dchisq( x, df=nrow(df) ), col='green', add=TRUE )
