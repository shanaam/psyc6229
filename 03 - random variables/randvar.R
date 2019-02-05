# randvar.R  Random variables

### the cumulative distribution function (cdf)

# the normal cdf (continuous)
curve( pnorm( x, mean=0, sd=1 ), -5, 5, main='normal cdf' )

# the uniform cdf (continuous)
curve( punif( x, min=0, max=1 ), -1, 2, main='uniform cdf' )

# the binomial cdf (discrete)
curve( pbinom( x, size=20, prob=0.5 ), 0, 20, n=1000, main='binomial cdf' )

# see ?distributions for more


### the probability density function (pdf), probability mass function (pmf)

# the normal pdf (continuous)
curve( dnorm( x, mean=0, sd=1 ), -5, 5, main='normal pdf' )

# the uniform pdf (continuous)
curve( dunif( x, min=0, max=1 ), -0.5, 1.5, main='uniform pdf' )

# the binomial pmf (discrete)
x <- seq( 0, 20, 0.5 )
y <- dbinom( x, size=20, prob=0.5 )
plot( x, y, type='h', main='binomial pmf' )
# curve( dbinom( x, size=20, prob=0.5 ), 0, 20, n=1000 )

# see ?distributions for more


### the cdf is the integral of the pdf

# define our own standard normal cdf as the integral of R's standard normal pdf
normcdf <- function( x ) {
    p <- rep( NaN, length(x) )
    for( i in 1:length(x) )
        p[i] <- integrate( dnorm, -Inf, x[i] )$value
    return( p )
}

# plot it
curve( normcdf, -3, 3 )

# compare it to R's standard normal cdf
x <- seq( -3, 3, by=0.25 )
points( x, pnorm(x), col='red' )


### the pdf is the derivative of the cdf

# define our own standard normal pdf as the derivative of R's standard normal cdf
normpdf <- function( x ) {
    h <- 1e-6
    p <- rep( NaN, length(x) )
    for( i in 1:length(x) )
        p[i] <- (pnorm(x[i]+h)-pnorm(x[i]))/h
    return( p )
}

# plot it
curve( normpdf, -3, 3 )

# compare it to R's standard normal pdf
x <- seq( -3, 3, by=0.25 )
points( x, dnorm(x), col='red' )


### some common pdf's are location-scale families

# normal distribution
mu <- 10
sigma <- 2
curve(           dnorm(  x, mean=mu, sd=sigma ), 5, 15 )
curve( (1/sigma)*dnorm( (x-mu)/sigma          ), 5, 15, type='p', col='red', add=TRUE )


### empirical pdf and cdf

x = rnorm( 1000 )
hist( x, probability=TRUE )  # empirical pdf; probability=TRUE ensures area = 1
plot( ecdf( x ) )            # empirical cdf; ecdf returns a function


### random number generators (rng's) for some common random variables

# the normal rng (continuous)
x <- rnorm( 10000, mean=10, sd=4 )
hist( x, xlim=c(0,20), main='normally distributed samples' )

# the uniform rng (continuous)
x <- runif( 10000, min=0, max=1 )
hist( x, xlim=c(-0.5,1.5), main='uniformly distributed samples' )

# the binomial rng (discrete)
x <- rbinom( 1000, size=20, prob=0.5 )
hist( x, breaks=seq(-0.25,20.25,0.5), main='binomially distributed samples' )

# see ?distributions for more


### a linear congruential rng

s <- 1  # the state of the rng

rand <- function( ) {
	
	# constants
	m <- 4294967296  # = 2^32
	A <- 1664525
	B <- 1013904223
	
	# update
	s <<- ( A*s + B ) %% m
	# Q:  what if we used <- instead of <<- ?
	
	# return a number in range [0,1)
	return( s/m )
	
}

# what if we reset the state?
rand()
rand()
s <- 1
rand()
rand()


### rng seeds

set.seed( 1000 )   # set the seed
rnorm( 5 )         # get some random numbers
rnorm( 5 )
set.seed( 1000 )   # reset the seed to the same value
rnorm( 5 )         # same random numbers!
rnorm( 5 )

# this is why numbers from an rng are sometimes called "pseudo-random"


### inverse transform sampling

# get a normally distributed sample using the uniform rng
r <- qnorm( runif(10000) )
hist( r, main='normally distributed!', probability=TRUE )
curve( dnorm(x), col='red', add=TRUE )
