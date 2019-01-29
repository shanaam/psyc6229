# solutions.R

### chapter 14, problem 26

rm( list=ls() )

# probability of rolling a value x with two dice
proll <- function( x )
    pmax( pmin( x-1, 13-x ), 0 ) / 36

# results that make us win, lose, or continue on roll 1
rwin1  <- c( 7, 11 )
rlose1 <- c( 2, 3, 12 )
rcont1 <- setdiff( 2:12, c( rwin1, rlose1 ) )

# probability of winning on first roll
p1 <- sum( proll( rwin1 ) )

# probability of winning on a subsequent roll
# - step through results on roll 1 that do not make us win or lose on that roll
pk <- 0
for( r1 in rcont1 ) {

    # list the results that do not make us win or lose on rolls 2, 3, 4, ...
    rcontk <- setdiff( 2:12, c(7,r1 ) )

    # find the probability of winning on rolls 2, 3, 4, ..., given result r1 on roll 1
    # (see derivation of this formula in solutions.pdf, problem 26)
    pk <- pk + ( proll(r1)^2 ) / ( 1 - sum( proll( rcontk ) ) )
    
}

# total probability of winning
pwin <- p1 + pk
cat( 'pwin =', pwin )
