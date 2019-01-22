# datastruc.R  data structures (notes by Shanaa)

# useful keyboard shortcuts           os x                 windows
#   run current line or selection     command-return       control-R
#   run whole script (.R file)        command-E
#   halt a script                     control-C
#   clear console                     command-option-L     control-L
#   (see menus for others)

### atomic vector:  1D sequence of elements, all of the same type (numbers, strings, etc.)

# creating atomic vectors
x <- 10                  # make a vector with one element
x <- c( 1, 2, 3, 4, 5 )  # make a vector with several elements
print( x )               # show the vector in the console
?c                       # get help on the function c()
??combine                # look for functions that have 'combine' in their help text

x <- 1:10                # make a vector that is a sequence of numbers
x <- seq( 10, 20, 2 )
x <- seq( from=10, to=20, by=2 )
x <- seq( from=10, to=20, length.out=10 )
?seq

x <- rnorm( 10, mean=0, sd=1 )  # make a vector of normally distributed random numbers
x <- rnorm( 10 )                # use default values for mean and sd

# other types of atomic vectors
x <- c( 'this', 'that', 'here', 'there' )  # character vector
x <- c( TRUE, FALSE, TRUE, FALSE )         # logical vector
typeof( x )                                # find the type of a vector

x <- 1
typeof( x )

x <- 1L
typeof( x )                                #this is an INTEGER!, not double
# getting elements of vectors
x <- rnorm( 10 )
x[1]                     # get an element of a vector, does NOT start at 0 in R
                         # the 1 you pass IS AN ATOMIC VECTOR (of length 1), therefore, you can pass any other vector as well!
x[ c(1,3,5) ]            # get several elements of a vector
x[ 1:5 ]                 # get a range of elements of a vector (this is itself a vector)
x>0                      # get a vector indicating whether each element of x is greater than zero -- VERY SIMILAR to x + 1    
x[ x>0 ]                 # get a vector of the elements of x that are greater than zero

# setting elements of vectors
x[1] <- 10
x[ c(1,3,5) ] <- 10
x[ c(1,3,5) ] <- c(10,20,30)
x[ 1:5 ] <- 10
x[ x>0 ] <- 0

x[ c(1,3,5,7) ] <- c(10,20)

# applying functions to vectors
sum( x )                 # find the sum of all the elements in a vector
mean( x )                # find the mean of all the elements in a vector
sin( x )                 # find the sine of all the elements in a vector
is.atomic( x )           # see whether x is an atomic vector ( not the same as is.vector() )
length( x )              # find the number of elements in a vector

# special values
x <- Inf                 # infinity, e.g., 1/0
x <- -Inf                # negative infinity, e.g., -1/0
x <- NaN                 # not a number, e.g., 0/0
x <- NA                  # not available, e.g., missing data


### matrix:  2D grid of elements, all of the same type (numbers, strings, etc.)

# creating matrices
x <- 1:12
m <- matrix( x, ncol=4 )  # make a 3 x 4 matrix of numbers 1 to 12 (filled in column-wise), numbers come from the atomic vector
# by default, R will fill in columns first! i.e. NOT how you write

# non-numeric matrices
m <- matrix( c('abc','def','ghi','jkl'), nrow=2 )  # character matrix
m <- matrix( c(TRUE,FALSE,TRUE,FALSE), nrow=2 )    # logical matrix
typeof( m )                                        # find the mode of a matrix (R will automatically convert all elements to the same mode)

# getting parts of matrices
m <- matrix( rnorm(50), nrow=10 )
m[2,1]                   # get an element of m, row 2 then column 1
m[2,2:4]                 # get several elements of m
m[2,]                    # get a row of m
m[,2]                    # get a column of m --> remember these are returned as an atomic vector, not a matrix
m[ m>0 ]                 # get a vector of the elements of m that are greater than zero

# applying functions to matrices
sum( m )                 # find the sum of all the elements in a matrix
sin( m )                 # find the sine of all the elements in a matrix
dim( m )                 # find the dimension of a matrix
length( m )              # find the number of elements in a matrix
as.vector( m )           # convert a matrix to a vector
is.matrix( m )           # see whether m is a matrix

# combining matrices
x <- matrix( rnorm(9), nrow=3 )
y <- matrix( rnorm(9), nrow=3 )
z <- rbind( x, y )       # combine matrices vertically (appending rows; add to the bottom)
z <- cbind( x, y )       # combine matrices horizontally (appending columns; add to the right)

# compare x and y
a <- 1:4
b <- 11:14
x <- cbind( a, b )
y <- rbind( a, b )      # R is trying to be helpful here. BUT this can make the code a little unpredicatable so be careful.

# useful fact:  a matrix is just an atomic vector with a "dim" property that specifies
# the number of rows and columns
x <- seq( 1, 12, by=1 )  # atomic vector
is.atomic( x )
is.matrix( x )
dim( x )
dim( x ) <- c( 3, 4 )    # alternative way of making x into a matrix
x
is.atomic( x )
is.matrix( x )
dim( x )
# consequences
# - we can have logical matrices, character matrices, etc.
# - most things you can do with an atomic vector can be done with a matrix too
#   e.g., x[ 1 ], sum( x ), length( x )


### array:  n-dimensional block of elements, all of the same type (numbers, strings, etc.)

# creating arrays
x <- rnorm( 24 )
a <- array( data=x, dim=c(2,3,4) )  # make a 2 x 3 x 4 array of random numbers
a

# here too, an array is just an atomic vector with a "dim" property that specifies
# more than two dimensions, e.g., rows, columns, and slices
# 
# review relationship between is.atomic(), is.matrix(), is.array()


### list:  1D sequence of elements, not necessarily all of the same type

# creating lists
x <- list( a=1, initials='rfm', scores=c(10,20,30) )  # make a list
str( x )  # show contents of x

# getting elements of lists
x$a
x$initials
x$scores
x$scores[2]

# subscripts to get elements of a list
x[[1]]
x[[3]][2]
# x[[1:2]]  # error; why?

# subscripts to get a sublist
x[1]
x[1:2]

# setting elements of lists
x$a <- 2
x$initials <- 'jfk'

# applying functions to lists
is.list( x )             # see whether x is a list
length( x )              # get the number of elements in x
names( x )               # get a vector of the names of the elements of x

# note:  lists are vectors, but not atomic vectors!
x <- list( a=1, b=2 )
is.vector( x )
is.atomic( x )
# although we usually call the atomic vectors that we examined in the first section
# "vectors" for short, sometimes it's important to know that, strictly speaking,
# a vector can be an atomic vector or a list


### data frame:  a list of equal-length vectors

# creating data frames
df <- data.frame( trial=1:5,
                  signal=c('a','b','b','a','b'), 
                  response=c('a','b','a','a','a'),
                  rt=c(0.11,0.21,0.12,0.21,0.21) )  # make a data frame

# getting parts of data frames
df$trial
df$signal
df$response[1:3]

# applying functions to data frames
is.list( df )            # see whether x is a list
is.data.frame( df )      # see whether x is a data frame
length( df )             # get number of columns in x
names( df )              # get a vector of the names of the columns of df


### miscellaneous

help.start()                   # open help homepage

getwd()                        # get working directory
setwd( '/Users/rfm/Desktop' )  # set working directory

ls()                           # list all variables
rm( x )                        # remove variable x
rm( list=ls() )                # remove all variables

x <- 1
y <- 2
save( x, y, file='datafile.Rdata' )  # save variables
load( 'datafile.Rdata' )             # load variables
unlink( 'datafile.Rdata' )           # delete a file


### functions

# create a function to calculate sine in degrees
sind <- function(theta)
	sin((pi/180) * theta)

sind(0)
sind(90)

# create a function to calculate sine in degrees (default) or radians
sinv <- function(theta, deg = TRUE) {
	if (deg) 
		return(sin((pi/180) * theta))
	else
		return(sin(theta))
}

sinv(90)
sinv(pi/2, deg = FALSE)
sinv(deg = FALSE, theta = pi/2)  # order of arguments doesn't matter if we use argument names

# create a function to calculate sine and cosine
sincos <- function(theta)
	list(sine = sin(theta), cosine = cos(theta))

v <- sincos(0)
v$sine
v$cosine
str(v)

# create a function that uses a variable in the workspace
r <- rnorm(100)
sumsquare <- function(x)
	sum((r - x)^2)

sumsquare(0)
sumsquare(1)

# change the value of r and call sumsquare again
r <- rnorm(100)
sumsquare(0)
sumsquare(1)


### plots

# make some data
x <- seq( 0, 2*pi, by=0.1 )
y <- sin( x )
y <- y + rnorm( length(y), sd=0.1 )

# basic plot
plot( x, y )

# plot with some options
plot( x, y, type='o', pch=2, lwd=2, col='red', xlab='x', ylab='sin(x)', main='a sinusoid',
	xlim=c(-0.1,2*pi+0.1), ylim=c(-1.2,1.2) )
# type = points (p), lines (l), both (b), overlay (o), etc.
# pch  = point character (circle, square, etc.), a code 1-25
# lwd  = line width
# col  = colour (red, green, blue, etc.)
# xlab = x label
# ylab = y label
# main = title
# xlim = limits of x axis
# ylim = limits of y axis

# add to an existing plot
z <- sin(x)
lines( x, z, col='blue' )               # add lines connecting points
abline( h=0 )                           # add a straight line
curve( sin(2*x), c(0,2*pi), col='green', add=TRUE )  # plot a function

# histogram
r <- rnorm( 1000, mean=10, sd=3 )
hist(r)

# histogram with probability units on y-axis
r <- rnorm( 1000, mean=10, sd=3 )
hist(r, probability=TRUE, ylim=c(0,0.15))
curve( dnorm(x,mean=10,sd=3), c(-3,3), col='red', add=TRUE )
