# bootstrap2.R  Bootstrap the parameters of a fitted function

rm( list=ls() )

# make up some behavioural data for a discrimination task
df <- data.frame( stimlev=seq( 0.1, 1.0, by=0.1 ),                  # stimulus levels
                  ntrials=rep( 20, times=10 ),                      # number of trials at each stimulus level
                  nhigher=c( 1, 0, 4, 5, 8, 10, 12, 14, 19, 20 ) )  # number of trials where the observer responded "higher"
df$phigher <- df$nhigher / df$ntrials                               # 'nhigher' expressed as a proportion

# choose the form of the psychometric function
psyfn <- function( x, mu, sigma )
    pnorm( x, mu, sigma )

# define a maximum likelihood objective function
obj <- function( p, data )
    -sum(log( dbinom( data$nhigher, data$ntrials, psyfn( data$stimlev, p[1], p[2] ) ) ))

# make bootstrap function
bootfn <- function( resample=TRUE ) {
    
    # resample data
    dfstar <- df
    if( resample ) {
        dfstar$nhigher <- rbinom( nrow(df), df$ntrials, df$phigher )
        dfstar$phigher <- dfstar$nhigher / dfstar$ntrials
    }
    
    # fit resampled data
    pinit <- c( 0.5, 0.2 )                       # initial guess
    m <- optim( pinit, obj, NULL, data=dfstar )  # find parameters of best fit
    return( m$par )

}

# get fit to original data
phat <- bootfn( resample=FALSE )

# bootstrap
phatstar <- replicate( 1000, bootfn() )

# histogram bootstrapped PSEs
hist( phatstar[1,], main='bootstrapped PSEs' )
abline( v=phat[1], col='red' )

# report maximum likelihood fit and bootstrapped 95% confidence interval
pse_ml <- phat[1]
pse_sd <- sd( phatstar[1,] )
cat( sprintf('\npse = %.4f +- %.4f\n\n',pse_ml,1.96*pse_sd) )

# or find the 95% confidence interval nonparametrically
cint <- quantile( phatstar[1,], c(0.025,0.975) )
cat( sprintf('pse = %.4f, 95%% confidence interval = (%.4f, %.4f)\n\n',pse_ml,cint[1],cint[2]) )
abline( v=cint, col='green' )
