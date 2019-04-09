# xval1.R  Illustrate model selection via cross validation

xval <- function( p=5, plist=1:(2*p), sigma=0.20, nsamp=50, plotit=TRUE ) {

	# make a model matrix with all independent variables
	x <- seq(-10,10,length.out=nsamp)
	pmax <- max(plist)
	k <- matrix( 0:pmax, nrow=nsamp, ncol=pmax+1, byrow=TRUE )
	X <- matrix( x, nrow=nsamp, ncol=pmax+1 )^k
	X <- X/matrix( apply(X,2,max), nrow=nsamp, ncol=pmax+1, byrow=TRUE )

	# choose regression coefficients
	beta <- rnorm( p+1, mean=0, sd=2 )

	# make up some dependent variables for the model of order p
	y <- X[,1:(p+1)] %*% beta + rnorm( nsamp, sd=sigma )

	# show data
	if( plotit ) {
		par( mfrow=c(1,2) )
		plot( x, y, type='o' )
	}

	# divide the data into blocks; trial i belongs to block blocki(i)	
	nblocks <- 10
	blocki <- ( ( 0:(nsamp-1) ) %% nblocks ) + 1
	blocki <- sample( blocki, nsamp, replace=FALSE )

	# initialize training and validation error
	errt <- matrix( NaN, nrow=length(plist), ncol=nblocks )
	errv <- matrix( NaN, nrow=length(plist), ncol=nblocks )
	
	# try polynomial models of various degrees
	for( i in 1:length(plist) ) {

		# get the part of the model matrix that we need for this model
		Xsub <- X[,1:(plist[i]+1)]
	
		# step through cross validation blocks
		for( b in 1:nblocks ) {
		
			# get the training data
			it <- blocki != b
			Xt <- Xsub[it,]
			yt <- y[it]
		
			# get the validation data
			iv <- blocki == b
			Xv <- Xsub[iv,]
			yv <- y[iv]
		
			# fit the linear model to the training data
			fit <- lm( yt ~ Xt - 1 )
			betahat <- fit$coefficients
		    
			# find mean squared prediction error on training trials
			yhatt <- Xt %*% betahat
			errt[i,b] <- mean( (yt-yhatt)^2 )
			
			# find mean squared prediction error on validation trials
			yhatv <- Xv %*% betahat
			errv[i,b] <- mean( (yv-yhatv)^2 )
		
		}
	
	}

	# find log of average training and validation error for each model
	logerrt <- log( apply( errt, 1, mean ) )
	logerrv <- log( apply( errv, 1, mean ) )
	
	# plot results
	if( plotit ) {
		plot( plist, logerrt, type='o', col='red', xlab='model order', ylab='log( ss error )' )
	    lines( plist, logerrv, type='o', col='green' )
	}

	# return average log error
	return( list( logerrt=logerrt, logerrv=logerrv ) )

}
