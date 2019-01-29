# topic2solutions.R

### chapter 2 ###

# 1(a)
z <- x^(a^b)

# 1(b)
z <- (x^a)^b

# 1(c)
z <- 3*x^3 + 2*x^2 + 6*x + 1
z <- (3*x + 2)*(x^2 + 2) - 3  # fewer operations

# 1(d)
z <- floor( 100*abs(x) ) %% 10

# 1(e)
z <- z + 1

# 2(a)
c( 1:8, 7:1 )

# 2(b)
rep( 1:5, 1:5 )

# 2(c)
1 - diag(3)

# 2(d)
matrix( c(0,2,3,0,5,0,7,0,0), nrow=3, byrow=TRUE )

# 3
phi <- atan( vec[2]/vec[1] )
r <- sqrt( sum( vec^2 ) )

# 4
x <- 1:100
x <- x [ x %% 2 != 0 ]
x <- x [ x %% 3 != 0 ]
x <- x [ x %% 7 != 0 ]

# 5(a)
queue <- c( "Steve", "Russell", "Alison", "Liam" )
queue <- c( queue, "Barry" )

# 5(b)
queue <- queue[-1]

# 5(c)
queue <- c( "Pam", queue )

# 5(d)
queue <- queue[ 1:(length(queue)-1) ]

# 5(e)
queue <- queue[ queue != "Alison" ]
which( queue=="Russell" )

# 6
rm(list = ls())
x <- 1        # ok
x[3] <- 3     # ok; vector is extended, with NA in place 2
y <- c()      # ok; value is NULL
y[2] <- 2     # ok; as above, vector is extended, with NA filled in
y[3] <- y[1]  # ok
y[2] <- y[4]  # ok
z[1] <- 0     # not ok; z does not exist

# 7

# version 1
x <- diag( 10 )
x[ x != 0 ] = 5

# version 2
x <- diag( 10 )
x[ which( x != 0 ) ] = 5

# version 3
x <- diag( 10 )
diag( x ) <- 5

# version 4
x <- diag( 10 )
x[ seq( 1, 100, by=11 ) ] <- 5

### chapter 3 ###

# 1, part one:  calculate y as a function of x

if( x <= 0 ) {
    y <- -x^3
} else if( x <= 1 ) {
    y <- x^2
} else
    y <- sqrt( x )

# 1, part two:  incorporate part one into given program

# input
x.values <- seq(-2, 2, by = 0.1)

# for each x calculate y
n <- length(x.values)

y.values <- rep(0, n)
for (i in 1:n) {
    x <- x.values[i]
    # your expression for y goes here
    if( x <= 0 ) {
        y <- -x^3
    } else if( x <= 1 ) {
        y <- x^2
    } else
        y <- sqrt( x )
    y.values[i] <- y
}

# output
plot(x.values, y.values, type = "l")

# 1, part three:  derivatives?  (don't worry about this part if
# you don't know calculus)

# Does f have a derivative at x = 0?
#   (d/dx) -x^3 = -3*x^2, so we we approach x = 0 from the left,
#   the slope tends towards 0.  (d/dx) x^2 = 2*x, so as we approach
#   x = 0 from the right, the slope also tends towards 0.  thus f
#   has a well-defined derivative at x = 0, namely 0.

# Does f have a derivative at x = 1?
#   Following similar reasoning, we see that the slope of f tends
#   towards different values at x = 1, as we approach from the left and
#   the right, so f does not have a well-defined derivative at that point.

# 2

h <- function( x, n ) {
    y <- 0
    for( i in 0:n )
        y <- y + x^i
    return( y )
}

# 3

hexact <- function( x, n )
    ( 1 - x^(n+1) ) / ( 1 - x )

# seems to work ... we get the same answer both ways
h( 0.3, 55 )
hexact( 0.3, 55 )
h( 6.6, 8 )
hexact( 6.6, 8 )

# 4

# part one:  while loop
hwhile <- function( x, n ) {
    y <- 0
    i <- 0
    while( i <= n ) {
        y <- y + x^i
        i <- i + 1
    }
    return( y )
}

# part two:  vector operations
hvec <- function( x, n )
    sum( x^(0:n) )

# 6

# part one:  for loop
gmean <- function( x ) {
    n <- length( x )
    y <- 1
    for( i in 1:n )
        y <- y * x[i]
    y <- y^(1/n)
}

# part two:  vector operations
gmeanvec <- function( x )
    prod( x ) ^ (1/length(x))

# part three:  harmonic mean
hmean <- function( x )
    1 / sum( 1/x )

# 7
sum( x[ seq( 3, length(x), by=3 ) ] )

# 10
x.min <- x[1]
for( i in 2:length(x) )
    if( x[i] < x.min )
        x.min <- x[i]

# 11

# start with two sorted vectors
x1 <- sort( rnorm( 10 ) )
x2 <- sort( rnorm( 8 ) )

# merge them
n1 <- length( x1 )  # get vector lengths
n2 <- length( x2 )
n <- n1 + n2
i1 <- 1  # start with first elements of vectors
i2 <- 1
x <- rep( NA, n )
for( i in 1:n ) {
    # if current element in first vector is smaller, choose it
    if( x1[i1] < x2[i2] ) {
        x[i] <- x1[i1]
        i1 <- i1 + 1
        # if we've used up the first vector, then use the rest of the second vector, and we're done
        if( i1 > n1 ) {
            x[(i+1):n] <- x2[i2:n2]
            break
        }
    # if current element in second vector is smaller, choose it
    } else {
        x[i] <- x2[i2]
        i2 <- i2 + 1
        # if we've used up the second vector, then use the rest of the first vector, and we're done
        if( i2 > n2 ) {
            x[(i+1):n] <- x1[i1:n1]
            break
        }
    }
}

# 12

# find sum of roll of two dice
roll <- function()
    sum( sample( 1:6, 2, replace=TRUE ) )

# simulate a single game of craps
cgame <- function() {
    
    # first roll
    r1 <- roll()
    if( r1==7 || r1==11 )  # win
        return( TRUE )
    if( r1==2 || r1==3 || r1==12 )  # lose
        return( FALSE )
    
    # subsequent rolls
    repeat {
        r <- roll()
        if( r==r1 )  # win
            return( TRUE )
        if( r==7 )  # lose
            return( FALSE )
    }
    
}

# find empirical probability of winning
pwin <- mean( replicate( 100000, cgame() ) )
cat( 'pwin =', pwin )


# 15

# we'll let TRUE represent ON, and FALSE represent OFF

# start with all switches ON
x <- rep( TRUE, 100 )

# toggle switches
for( i in 1:100 ) {
    k <- seq( i, 100, by=i )
    x[k] = ! x[k]
}

# see which switches are on
print( x )
which( !x )
# the last command shows that the switches labelled by square numbers (1, 4, 9, ...)
# are turned off, and the others are turned on
