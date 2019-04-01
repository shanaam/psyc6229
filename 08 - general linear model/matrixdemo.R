# matrixdemo.R  Illustrate some matrix operations

rm( list=ls() )

# matrix arithmetic
x <- matrix( c(1,4,2,3,1,5,4,1,4),          nrow=3 )
y <- matrix( c(11,14,19,18,17,17,12,13,11), nrow=3 )
a <- x + y
b <- x - y
c <- 10*x

# matrix multiplication
z1 <- x %*% y
z2 <- y %*% x  # not commutative

# matrix inversion
w <- solve( x )  # invert matrix x
z1 <- w %*% x
z2 <- x %*% w

# in principle z1 and z2 should be the 3 x 3 identity matrix:
# 
#   1 0 0
#   0 1 0
#   0 0 1
# 
# but notice that the elements of z1 and z2 are typically not exactly zero or one.
# this is a byproduct of finite-precision arithmetic.


# solve a system of linear equations, X*b = y
X <- matrix( rnorm(9), nrow=3 )           # make up an X
b <- matrix( rnorm(3), nrow=3 )           # make up a b
y <- X %*% b                              # make up a y
# bhat <- solve(X) %*% y                  # solve for b using the inverse of X
bhat <- solve( X, y )                     # solve for b in a more numerically stable way
print( cbind( b, bhat ) )                 # check the solution
