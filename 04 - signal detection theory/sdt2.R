# sdt2.R  A signal detection model of a yes-no task

# clear workspace
rm(list = ls())

# set model observer parameters
mu1 <- 0
mu2 <- 3
sigma <- 2
criterion <- 1

# calculate d' and c from observer parameters
dprime1 <- (mu2 - mu1)/sigma
c1 <- (criterion - (mu1 + mu2)/2)/sigma

# get hit and false alarm rates using normal cumulative distribution function (cdf)
hit        <- 1 - pnorm( criterion, mean=mu2, sd=sigma )
falsealarm <- 1 - pnorm( criterion, mean=mu1, sd=sigma )

# or, to highlight the fact that the calculation is based on the distance
# between the criterion and the means, as measured in standard deviations:
# hit        <- 1 - pnorm( ( criterion - mu2 )/sigma )
# falsealarm <- 1 - pnorm( ( criterion - mu1 )/sigma )

# find d' and c from hit and false alarm rates
dprime2 <- qnorm( hit ) - qnorm( falsealarm )
c2 <- -0.5*( qnorm( hit ) + qnorm( falsealarm ) )

# compare values of d' (should be the same)
cat( 'dprime 1: ', dprime1, '\n' )
cat( 'dprime 2: ', dprime2, '\n' )

# compare values of c (should be the same)
cat( 'c 1: ', c1, '\n' )
cat( 'c 2: ', c2, '\n' )
