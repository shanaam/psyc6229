# atomic_vector.R

rm( list=ls() )

# atomic vector
x <- 1:5
is.atomic( x )  # true
is.vector( x )  # true
is.matrix( x )  # false

# matrix
m <- matrix( 1:5, nrow=1 )
is.atomic( m )  # true
is.vector( m )  # false
is.matrix( m )  # true

# a matrix is an atomic data type, but it is not a vector
