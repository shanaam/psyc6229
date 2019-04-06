# mnistdemo.R

rm( list=ls() )

# load MNIST dataset
load( 'mnist.Rdata' )

# show some digits
for( i in 1:100 ) {
    
    # show image
    im <- matrix( traini[,i], nrow=28, byrow=TRUE )
    im <- im[,seq(ncol(im),1,-1)]
    image( im, col=grey.colors(256) )
    
    # show label
    k <- which.max( traino[,i] ) - 1
    print( k )

    # pause    
    Sys.sleep( 1 )
    
}
