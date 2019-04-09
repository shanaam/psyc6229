# nndemo.R  Illustrate a neural network that learns the MNIST dataset

# in practice we would do this in Python, using PyTorch or TensorFlow, which
# are add-on modules that take care of many of the details of neural network
# programming.  but we're using R for this course, and this is also a useful
# R exercise.

rm( list=ls() )

# load MNIST dataset
load( 'mnist.Rdata' )

# find data set size
ntrain <- ncol( traini )
ntest  <- ncol( testi )

# set network architecture
nn <- list()
nn$dimi <- nrow( traini )      # training stimulus size
nn$dimo <- nrow( traino )      # training target size
nn$nnode <- c( nn$dimi, nn$dimi, nn$dimi, round(sqrt( nn$dimi) ), nn$dimo ) # number of nodes in each layer
nn$nlev <- length( nn$nnode )  # number of layers

# set activation function and its derivative
nn$f  <- function( x ) pmax( x, 0 )
nn$fp <- function( x ) as.double( x > 0 )

# set error function
nn$errfn <- function( o, y ) 0.5*sum( (y-o)^2 )

# initialize lists for weights and states
nn$w     <- rep( list( NA ), nn$nlev-1 )  # weights
nn$z     <- rep( list( NA ), nn$nlev )    # layer inputs
nn$y     <- rep( list( NA ), nn$nlev )    # layer outputs
nn$wp    <- rep( list( NA ), nn$nlev-1 )  # derivatives of error with respect to weights
nn$delta <- rep( list( NA ), nn$nlev )    # derivatives of error with respect to layer inputs

# initialize weights
for( i in 1:(nn$nlev-1) ) {
    
    # assign random values
    nn$w[[i]] <- matrix( rnorm(nn$nnode[i]*nn$nnode[i+1]), nrow=nn$nnode[i+1] )

    # normalize weights
    # (this is not usually part of back propagation, but I find that it
    #  improves performance in this problem)
    m <- sqrt( apply( nn$w[[i]]^2, 1, sum ) )
    nn$w[[i]] <- nn$w[[i]] / matrix( m, nrow=nrow(nn$w[[i]]), ncol=ncol(nn$w[[i]]) )
    
}

# set training and test batch size
ktrain <- 10
ktest <- 1000

# set learning rate
k <- 0.01

# load forward and backward pass functions
source( 'fbpass.R' )

# train network via stochastic gradient descent
err <- NA
i <- 0
repeat {
    i <- i + 1

    # choose a batch of training stimuli
    trainf <- ceiling( runif( ktrain, min=0, max=ntrain ) )
    
    # forward pass
    nn <- fpass( nn, traini[,trainf] )
    
    # backward pass
    nn <- bpass( nn, traino[,trainf] )
    
    # adjust weights in each layer
    for( j in 1:(nn$nlev-1) ) {
     
        # adjust weights
        nn$w[[j]] <- nn$w[[j]] - k * nn$wp[[j]]
        
        # normalize weights (as noted above, not usually part of backpropagation)
        m <- sqrt( apply( nn$w[[j]]^2, 1, sum ) )
        nn$w[[j]] <- nn$w[[j]] / matrix( m, nrow=nrow(nn$w[[j]]), ncol=ncol(nn$w[[j]]) )
        
    }
    
    # show progress
    if( i %% 10 == 0 ) {
      
        # choose a batch of test stimuli
        testf <- ceiling( runif( ktest, min=0, max=ntest ) )
        
        # forward pass
        nn <- fpass( nn, testi[,testf] )

        # check responses
        responsek <- apply( nn$y[[nn$nlev]], 2, which.max ) - 1
        correctk  <- apply( testo[,testf],   2, which.max ) - 1
        ii <- i %/% 10
        err[ ii ] <- mean( responsek != correctk )
        
        # show error
        plot( err, type='o', xlim=c(1,ii), ylim=c(0,1), xlab='iteration', ylab='error rate' )
        cat( sprintf( '%d %.6f\n', i, err[ ii ] ) )
        Sys.sleep( 0.1 )
        
    }
    
}
