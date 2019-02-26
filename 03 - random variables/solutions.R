# solutions.R

### chapter 15 ###

# problem 1.  we could solve this problem manually, but let's do it in R
# to avoid doing a lot of number crunching by hand, and also to see how to
# use some new R functions.

# first we'll make a data frame that has all possible rolls of the two dice
df <- expand.grid( d1=1:6, d2=1:6 )  # the expand.grid function is the cartesian product

# (a) X = minimum value showing

olist <- pmin( df$d1, df$d2 )     # get the outcomes

sspace <- sort( unique( olist ) ) # eliminate duplicates to get the sample space
n <- as.vector( table( olist ) )  # count number of occurrences of each outcome
p <- n / sum(n)                   # convert counts to probabilities
plot( sspace, p, type='h', ylim=c(0,1.1*max(p)) )  # plot the pmf
e <- sum( sspace*p )              # find the expected value

# (b) X = absolute difference between two values showing

olist <- abs( df$d1 - df$d2 )     # get the outcomes

# notice that the next five lines are the same as the lines we used in part (a),
# so really we should put them into a function instead of typing them out again.
# here I'll keep the duplicated lines to make it clearer what's going on, but as we
# become more familiar with R code I'll make more extensive use of functions.
# also, below (part (abc') I show how to put this code into a function.
sspace <- sort( unique( olist ) ) # eliminate duplicates to get the sample space
n <- as.vector( table( olist ) )  # count number of occurrences of each outcome
p <- n / sum(n)                   # convert counts to probabilities
plot( sspace, p, type='h', ylim=c(0,1.1*max(p)) )  # plot the pmf
e <- sum( sspace*p )              # find the expected value

# (c) X = minimum value showing divided by the other value showing

olist <- pmin( df$d1, df$d2 ) / pmax( df$d1, df$d2 )  # get the outcomes

# here again, same five lines as above.
sspace <- sort( unique( olist ) ) # eliminate duplicates to get the sample space
n <- as.vector( table( olist ) )  # count number of occurrences of each outcome
p <- n / sum(n)                   # convert counts to probabilities
plot( sspace, p, type='h', ylim=c(0,1.1*max(p)) )  # plot the pmf
e <- sum( sspace*p )              # find the expected value

# (abc')  let's do all of the above again, this time putting the analysis code
#         into a function

getpmf <- function( result_list ) {
    sspace <- sort( unique( result_list ) ) # eliminate duplicates to get the sample space
    n <- as.vector( table( result_list ) )  # count number of occurrences of each outcome
    p <- n / sum(n)                         # convert counts to probabilities
    plot( sspace, p, type='h', ylim=c(0,1.1*max(p)) )  # plot the pmf
    e <- sum( sspace*p )                    # find the expected value
}

olist <- pmin( df$d1, df$d2 )  # part (a)
getpmf( olist )

olist <- abs( df$d1 - df$d2 )  # part (b)
getpmf( olist )

olist <- pmin( df$d1, df$d2 ) / pmax( df$d1, df$d2 )  # part (c)
getpmf( olist )



# problem 9

# # code from problem statement
# maxheads <- function(n.toss) {
#   # returns the length of the longest sequence of heads # in a sequence of n.toss coin tosses
#   n_heads = 0 # length of current head sequence
#   max_heads = 0 # length of longest head sequence so far
#   for (i in 1:n.toss) {
#     # toss a coin and work out length of current head sequence
#     if (runif(1) < 0.5) { # a head, sequence of heads increases by 1
#       n_heads <- n_heads + 1
#     } else { # a tail, sequence of heads goes back to 0
#       n_heads <- 0
#     }
#     # see if current sequence of heads is the longest
#     if (n_heads > max_heads) {
#       max_heads <- n_heads
#     }
#   }
#   return(max_heads)
# }

# supplementary exercise:  rewrite maxheads using rle
maxheads <- function( n.toss ) {
    
    # make a sequence of coin tosses; 1 = heads, 0 = tails
    c.toss <- sample( c( 0, 1 ), n.toss, replace=TRUE )
    
    # encode lengths of sequences of heads and tails
    r <- rle( c.toss )
    
    # find length of longest sequence of heads
    return( max( r$lengths[ r$values==1 ] ) )

}

# get a large sample
len <- 20        # number of coin flips
nsamp <- 10000   # number of elements in the sample
hsamp <- replicate( nsamp, maxheads(len) )  # get the sample

# get a histogram of the sample
h <- hist( hsamp, breaks=seq( -0.5, len+0.5, by=1 ), plot=FALSE )

# estimate the probability mass function (pmf) using the histogram
x <- 0:len
p_hat <- h$count / nsamp

# print the pmf
cat( '   x  p_hat(x)\n' )
cat( '--------------\n')
for( i in 1:len )
    cat( sprintf( '  %2d    %.4f\n', x[i], p_hat[i] ) )


# problem 15(a)

# define the cdf
F_X <- function( x ) {
    y <- rep( 0, length( x ) )
    k <- x>0
    y[k] = 1 - exp(-x[k])
    return( y )
}

# plot the cdf
curve( F_X( x ), -1, 5 )


# problem 18

# (a)

# define the pdfs
f_X <- function ( x ) {
    y <- rep( 0, length( x ) )
    k <- x >= 0 & x <= 1
    y[k] <- 4 * x[k]^3
    return( y )
}

f_Y <- function ( x ) {
    y <- rep( 0, length( x ) )
    y[ x >= 0 & x <= 1 ] <- 1
    return( y )
}

# plot the pdfs
par( mfrow=c(1,2) )  # put the plots in a 1 x 2 grid
curve( f_X( x ), -0.5, 1.5 )
curve( f_Y( x ), -0.5, 1.5 )

# calculate the mean of X
f <- function( x ) f_X( x ) * x
mu_X <- integrate( f, 0, 1 )$value
cat( 'mu_X =', mu_X, '\n' )

# calculate the mean of Y
f <- function( x ) f_Y( x ) * x
mu_Y <- integrate( f, 0, 1 )$value
cat( 'mu_Y =', mu_Y, '\n' )

# the pdfs are polynomials, so we could also find the means with
# a closed-form integral instead of using numerical integration.

# (b)

# calculate the variance of X
f <- function( x ) f_X( x ) * ( x - mu_X )^2
sigma_X2 <- integrate( f, 0, 1 )$value
cat( 'sigma_X^2 =', sigma_X2, '\n' )

# calculate the variance of Y
f <- function( x ) f_Y( x ) * ( x - mu_Y )^2
sigma_Y2 <- integrate( f, 0, 1 )$value
cat( 'sigma_Y^2 =', sigma_Y2, '\n' )

# again, we could also use closed-form integrals.
