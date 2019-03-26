# steps.R  Maximum likelihood fit of a curve to a psychometric function, in steps

rm( list=ls() )

# make up some behavioural data for a discrimination task
df <- data.frame( stimlev=seq( 0.1, 1.0, by=0.1 ),                  # stimulus levels
                  ntrials=rep( 20, times=10 ),                      # number of trials at each stimulus level
                  nhigher=c( 1, 0, 4, 5, 8, 10, 12, 14, 19, 20 ) )  # number of trials where the observer responded "higher"
df$phigher <- df$nhigher / df$ntrials                               # 'nhigher' expressed as a proportion

# choose the form of the psychometric function
psyfn <- function( x, mu, sigma )
    pnorm( x, mu, sigma )

# here's a guess for mu and sigma
mu <- 0.10
sigma <- 0.20

# what's the probability of a correct response at the lowest stimulus
# level, according to these values of mu and sigma?
psyfn( df$stimlev[1], mu, sigma )

# what's the probability of getting the observed number of correct
# responses at the lowest stimulus level, according to these values
# of mu and sigma?
dbinom( df$nhigher[1], df$ntrials[1], psyfn( df$stimlev[1], mu, sigma ) )

# what are the probabilities of correct responses at *all* the stimulus
# levels, according to these values of mu and sigma?
psyfn( df$stimlev, mu, sigma )

# what are the probabilities of getting the observed numbers of correct
# responses at *all* the stimulus levels, according to these values of mu
# and sigma?
dbinom( df$nhigher, df$ntrials, psyfn( df$stimlev, mu, sigma ) )

# what's the joint probability of all these observed numbers of correct
# responses, according to these values of mu and sigma?
prod( dbinom( df$nhigher, df$ntrials, psyfn( df$stimlev, mu, sigma ) ) )

# convert this probability into a negative log likelihood
-sum(log( dbinom( df$nhigher, df$ntrials, psyfn( df$stimlev, mu, sigma ) ) ))

# forget about our guesses for mu and sigma, and make the line above
# into a function of a variable p (which is an atomic vector of length 2)
obj <- function( p )
    -sum(log( dbinom( df$nhigher, df$ntrials, psyfn( df$stimlev, p[1], p[2] ) ) ))

# find the parameters that minimize the objective function
pinit <- c( 0.5, 0.2 )             # initial guess
phat <- optim( pinit, obj )$par    # find the parameters of the best fit

# plot data and fitted PSE function
plot( df$stimlev, df$phigher, col='red', ylim=c(-0.1,1.1), xlab='stimulus level', ylab='proportion judged higher', main='PSE function' )
curve( psyfn( x, phat[1], phat[2] ), from=0, to=1, col='green', add=TRUE )
