# fitdemo.R  Illustrate how to fit a function to behavioural data

rm( list=ls() )

### part 1.  fit a psychometric function

# make up some behavioural data for a discrimination task
stimlev  <- seq( 0.1, 1.0, 0.1 )                        # stimulus levels:  0.1, 0.2, ..., 1.0
ntrials  <- rep( 20, 10 )                               # number of trials at each stimulus level:  20
nhigher  <- c( 1, 0, 4, 5, 8, 10, 12, 14, 19, 20 )      # number of trials where the stimulus was judged higher in value than a reference stimulus
phigher <- nhigher/ntrials                              # 'nhigher' expressed as a proportion

# choose the form of the psychometric function
psyfn <- function( x, mu, sigma )
	pnorm( x, mu, sigma )
	
# define a maximum likelihood objective function        # 'objective function' is a function youre trying to minimize
obj <- function( p )
	-sum(log( dbinom( nhigher, ntrials, psyfn( stimlev, p[1], p[2] ) ) ))

# find the parameters that minimize the objective function
pinit <- c( 0.5, 0.2 )             # initial guess: this is just to get a sense of the MAGNITUDE you expect (e.g. 1 vs 1 billion)
phat <- optim( pinit, obj )$par    # find the parameters of the best fit

#NOTE: Optim expects an obj function with ONE argument, but the one argument can be a vector with multiple parts (like we have here)


### part 2.  report the fit and plot it

# report the fit
muhat <- phat[1]
sigmahat <- phat[2]
cat(sprintf('muhat = %.2f\nsigmahat = %.2f',muhat,sigmahat))

# plot data and fitted PSE function
plot( stimlev, phigher, col='red', ylim=c(-0.1,1.1), xlab='stimulus level', ylab='proportion judged higher', main='PSE function' )
curve( psyfn( x, muhat, sigmahat ), from=0, to=1, col='green', add=TRUE )

# add a legend
legend( 'topleft', inset=0.05, legend=c('data','fit'), col=c('red','green'), pch=c(1,NA), lty=c(NA,1) )

# add error bars
stderr <- sqrt( phigher*(1-phigher)/ntrials )  # standard error of proportion judged higher (assumes a binomial distribution)
stderr <- pmax(stderr,0.001)  # avoid zero-length error bars
plow  <- phigher-stderr;     # lower end of error bars
phigh <- phigher+stderr;     # upper end of error bars
arrows( stimlev, plow, stimlev, phigh, code=3, angle=90, length=0.05, col='red' )

# in some add-on graphics packages there are functions that will add error bars in a single line.
# this code shows how to add error bars manually with the basic graphics package.  often it's
# useful to know how to do things manually anyway, as it gives you more control over how the
# plot will appear.
