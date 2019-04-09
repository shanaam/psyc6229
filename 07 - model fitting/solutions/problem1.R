# problem1.R  Solution to topic 7, problem 1

rm( list=ls() )

# make up some behavioural data for a discrimination task
df <- data.frame( stimlev=seq( 0.1, 1.0, by=0.1 ),                  # stimulus levels
                  ntrials=rep( 20, times=10 ),                      # number of trials at each stimulus level
                  nhigher=c( 1, 0, 4, 5, 8, 10, 12, 14, 19, 20 ) )  # number of trials where the observer responded "higher"
df$phigher <- df$nhigher / df$ntrials                               # 'nhigher' expressed as a proportion

# make a psychometric function with a lapse parameter, lambda
psymet <- function( x, mu, sigma, lambda ) lambda + (1-2*lambda)*pnorm( x, mean=mu, sd=sigma )

# find the maximum likelihood fit
errfn <- function( p ) -sum(log( dbinom( df$nhigher, df$ntrials, psymet( df$stimlev, p[1], p[2], p[3] ) ) ))
fit <- optim( c( 0.20, 0.20, 0.05 ), errfn )
param <- fit$par

# plot the data and fit
plot( df$stimlev, df$phigher, ylim=c(-0.1,1.1), xlab='stimulus level', ylab='proportion correct' )
curve( psymet( x, param[1], param[2], param[3] ), col='red', add=TRUE )

