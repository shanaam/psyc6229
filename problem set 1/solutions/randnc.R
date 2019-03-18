# randnc.R  Clipped normal random number generators

rm( list=ls() )

# here's a straightforward solution
randnc1 <- function( nsamp, mean=0, sd=1, nclip=2 ) {
    
    # get an initial sample
    x <- rnorm( nsamp, mean, sd )
    
    # find the outliers
    k <- ( x < mean - nclip*sd ) | ( x > mean + nclip*sd )

    # loop until there are no outliers
    while( any(k) ) {
        x[k] <- rnorm( sum(k), mean, sd )                       # resample the outliers
        k <- ( x < mean - nclip*sd ) | ( x > mean + nclip*sd )  # find the outliers again
    }
    
    return( x )

}

# check a large sample
v <- randnc1( 1000, mean=10, sd=2, nclip=2 )
hist( v )                # show a histogram
any( v < 6 | v > 14 )    # see whether there are any outliers

# this solution works, but every time through the loop we check every element
# to see if it's an outlier.  but if an element was not an outlier on pass n, it's not
# going to be an outlier on pass n+1, because we don't resample elements that are not
# outliers.  so this solution is inefficient.

# we can make a more efficient solution by making more careful use of indices
randnc2 <- function( nsamp, mean=0, sd=1, nclip=2 ) {
    
    # get an initial sample
    x <- rnorm( nsamp, mean, sd )
    
    # make a vector of outlier indices
    k <- which( ( x < mean - nclip*sd ) | ( x > mean + nclip*sd ) )
    
    # loop until there are no outliers
    while( length(k)>0 ) {
        
        # resample the outliers
        x[k] <- rnorm( length(k), mean, sd )
        
        # see which previous outliers are still outliers
        k2 <- ( x[k] < mean - nclip*sd ) | ( x[k] > mean + nclip*sd )
        
        # just keep the previous outliers that are still outliers
        k <- k[k2]
        
    }
    
    return( x )
    
}

# check a large sample
v <- randnc2( 1000, mean=10, sd=2, nclip=2 )
hist( v )                # show a histogram
any( v < 6 | v > 14 )    # see whether there are any outliers
