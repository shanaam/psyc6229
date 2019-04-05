# fbpass.R  Forward and backward pass functions 

# forward pass
fpass <- function( nn, batchin ) {
    
    # set input layer's outputs to stimulus
    nn$y[[1]] <- batchin
    
    # step through remaining layers  
    for( i in 2:nn$nlev) {
        
        # linear transformation
        nn$z[[i]] <- nn$w[[i-1]] %*% nn$y[[i-1]]
        
        # activation function
        nn$y[[i]] <- nn$f( nn$z[[i]] )
        
    }
    
    return( nn )
    
}

# backward pass
bpass <- function( nn, batchout ) {
    
    # find deltas of output layer (assumes sum-of-squares error function)
    nn$delta[[nn$nlev]] <- ( nn$y[[nn$nlev]] - batchout ) * nn$fp( nn$z[[nn$nlev]] )
    
    # step through earlier layers in reverse order
    for( i in seq( nn$nlev-1, 1, by=-1 ) ) {
        
        # back-propagate deltas
        if( i>1 )
            nn$delta[[i]] <- nn$fp( nn$z[[i]] ) * ( t(nn$w[[i]]) %*% nn$delta[[i+1]] )
        
        # find derivative of output error with respect to weights at this layer,
        # summed over all samples in the batch
        nn$wp[[i]] <- nn$delta[[i+1]] %*% t( nn$y[[i]] )
        
    }
    
    return( nn )
    
}
