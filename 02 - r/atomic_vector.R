# atomic_vector.R

rm( list=ls() )

# atomic vector
x <- 1:5
is.atomic( x )  # true
is.vector( x )  # true
is.matrix( x )  # false 
is.array ( x )  # false because dim(x) is NULL

# matrix
m <- matrix( 1:5, nrow=1 ) # R infers what the columns might be. we have 5 values and 1 row. Therefore, ncol = 5
is.atomic( m )  # true
is.vector( m )  # false
is.matrix( m )  # true
is.array( m ) # true because dim(x) has some values

# is.matrix (m) is true because
dim(m) # != NULL

# an atomic vector is both (a) an atomic data type and (b) a vector

# a matrix is an atomic data type, but it is not a vector
