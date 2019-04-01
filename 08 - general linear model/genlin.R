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

# repeat the above, using R's lm() function, which is numerically more stable
fit <- lm( y ~ X - 1 )                    # fit a linear model with y-intercept set to zero
betahat <- fit$coefficients               # get the regression coefficients
print( cbind( beta, betahat ) )           # check the solution

# lm() also returns confidence intervals for the coefficients, and much more
# information as well -- see ?lm and str( fit )
cint <- confint( fit, level=0.95 )
