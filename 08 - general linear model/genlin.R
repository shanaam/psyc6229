# genlin.R  The general linear model

rm( list=ls() )

# set parameters
p <- 3    # number of independent variables in each sample
n <- 10   # number of samples

# make up some data using the linear model:  y = X*beta + eps
X <- matrix( rnorm(n*p), nrow=n )         # make up independent variables (X)
beta <- matrix( rnorm(p), nrow=p )        # make up regression coefficients (beta)
y <- X %*% beta + rnorm( n, sd=0.1 )      # compute dependent variables (y)

# now suppose we only have X and y.  we can solve the over-determined system
# of linear equations to estimate beta.  we implicitly use a sum-of-squares
# error measure.
betahat <- solve( t(X) %*% X ) %*% t(X) %*% y  # estimate beta
print( cbind( beta, betahat ) )           # compare true and estimated beta
# notice that we don't recover beta exactly.  this is OK, because the data
# are noisy -- we used rnorm() to generate y.

# plot actual and fitted y values
yhat <- X %*% betahat
mn <- min(y,yhat) ; mx <- max(y,yhat)
plot( y, yhat, type='p', col='red', xlim=c(mn,mx), ylim=c(mn,mx) )
lines( c(mn,mx), c(mn,mx), type='l', lty=2 )

# repeat the fit using R's lm() function, which is numerically more stable
fit <- lm( y ~ X - 1 )                    # fit a linear model with y-intercept set to zero
betahat <- fit$coefficients               # get the regression coefficients
print( cbind( beta, betahat ) )           # check the solution

# lm() also returns confidence intervals for the coefficients, and much more
# information as well -- see ?lm and str( fit )
cint <- confint( fit, level=0.95 )

# note that even with the general linear model, we can model the dependent variable
# as a nonlinear function of the independent variables.  we just add new independent
# variables that are nonlinear functions of the initial independent variables.
# for example, we can model y as a quadratic function of x, as follows.

# make up some data
x <- seq( -1, 1, length.out=10 )
X <- cbind( rep( 1, length(x) ), x, x^2 )     # independent variables:  1, x, x^2
beta <- c( 0.5, 1, 2 )                        # coefficients
y <- X %*% beta + rnorm( length(x), sd=0.1 )  # dependent variables

# find regression coefficients
fit <- lm( y ~ X - 1 )
betahat <- fit$coefficients

# plot data and fitted curve
plot( x, y, type='p', col='red' )
curve( betahat[1] + betahat[2]*x + betahat[3]*x^2, col='green', xlim=c(-1,1), add=TRUE )
