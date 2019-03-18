# keeling.R

rm( list=ls() )

# (a) read the data

df <- read.table( 'keeling.txt', sep=',', header=TRUE )


# (b) plot the CO2 concentration over time

# eliminate missing data
df <- df[ df$co2_fit > 0, ]

# plot keeling curve
plot( df$date_float, df$co2_fit, type='l', xlab='time', ylab='CO2 concentration (ppm)', main='Keeling curve' )


# (c) plot average CO2 concentration by year

# one way of doing this is to manually find the concentration for each year

# get average concentration in available years
yearvec <- unique( df$year )
co2vec <- rep( NA, length(yearvec) )
for( i in 1:length(yearvec) )
    co2vec[i] <- mean( df$co2_fit[ df$year==yearvec[i] ] )

# plot it
plot( yearvec, co2vec, type='p', xlab='year', ylab='CO2 concentration (ppm)', main='CO2 by year' )

# but R has specialized functions available that make many common data processing operations
# like this much easier.  here we'll use the aggregate() function.

dfy <- aggregate( df$co2_fit, by=list( df$year ), FUN=mean )

plot( dfy$Group.1, dfy$x, type='p', xlab='year', ylab='CO2 concentration (ppm)', main='CO2 by year' )


# (d) plot average CO2 concentration by month

# we'll use aggregate() again, as that's easier, and also the more R-like way of solving
# this problem.

dfm <- aggregate( df$co2_fit, by=list( df$month ), FUN=mean )

plot( dfm$Group.1, dfm$x, type='o', xlab='month', ylab='CO2 concentration (ppm)', main='CO2 by month' )

