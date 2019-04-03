# xval2.R  Illustrate model selection via cross validation

rm( list=ls() )

# load cross validation function
source('xval1.R')

# set simulation parameters
p <- 5
plist <- 1:(2*p)
sigma <- 0.2
nsamp <- 50


# part one.  do a single cross validation test, as we would in a real experiment

# run test
err1 <- xval( p=p, plist=plist, sigma=sigma, nsamp=nsamp )

# see which model had smallest validation error
phat <- which.min( err1$logerrv )


# part two.  repeat the cross validation test many times, to see how well
# it performs on average

# set number of repetitions
nrep <- 100

# initialize matrix for training and validation error
errt <- matrix( NA, nrow=length(plist), ncol=nrep )
errv <- errt

# make many cross validation runs
for( i in 1:nrep ) {
  print(sprintf('trial %d of %d',i,nrep))
  r <- xval( p=p, plist=plist, sigma=sigma, nsamp=nsamp, plotit=FALSE )
  errt[,i] <- r$logerrt
  errv[,i] <- r$logerrv
}

# find mean training and validation error for each model
errmeant <- apply( errt, 1, mean )
errmeanv <- apply( errv, 1, mean )

# plot errors
par( mfrow=c(1,1) )
plot( plist, errmeant, type='o', col='red', xlab='model degree', ylab='log error',
      main='mean training and validation error',
      ylim=c(min(errmeant,errmeanv),max(errmeant,errmeanv)) )
lines( plist, errmeanv, type='o', col='green' )

# on each repetition, find the model with the lowest cross validation error
pmin <- apply( errv, 2, which.min )

# how often was it correct?
pcorrect <- mean( pmin==p )
print(sprintf('chose the correct model on proportion %.2f of trials',pcorrect))

# how often was it close?
pclose <- mean( pmin>=p-1 & pmin<=p+1 )
print(sprintf('chose a model in [p-1,p+1] on proportion %.2f of trials',pclose))

# histogram chosen models
# hist( pmin, breaks=seq(0.5,max(plist)+0.5), probability=TRUE )

