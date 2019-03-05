# model.R

# model observer
bob <- function( depthD1=NaN, depthT1=NaN, depthD2=NaN, depthT2=NaN ) {
	
	# set cue standard deviations in centimeters
	sigmaD = 1;
	sigmaM = 3;
	
	# set cue weights
	wD = 0.25
	wT = 0.75

	# get cue samples
	if( !is.nan(depthD1) )  xD1 <- rnorm( 1, depthD1, sigmaD )
	if( !is.nan(depthT1) )  xT1 <- rnorm( 1, depthT1, sigmaM )
	if( !is.nan(depthD2) )  xD2 <- rnorm( 1, depthD2 ,sigmaD )
	if( !is.nan(depthT2) )  xT2 <- rnorm( 1, depthT2, sigmaM )
	
	# get combined cue for stimulus 1
	if( !is.nan(depthD1) & !is.nan(depthT1) ) {
		x1 <- wD*xD1 + wT*xT1
	} else if( !is.nan(depthD1) ) {
		x1 <- xD1
	} else if( !is.nan(depthT1) ) {
		x1 <- xT1
	} else
		stop('both depthD1 and depthT1 are NaN')
		
	# get combined cue for stimulus B
	if( !is.nan(depthD2) & !is.nan(depthT2) ) {
		x2 <- wD*xD2 + wT*xT2
	} else if( !is.nan(depthD2) ) {
		x2 <- xD2
	} else if( !is.nan(depthT2) ) {
		x2 <- xT2
	} else
		stop('both depthD2 and depthT2 are NaN')
	
	# make response
	return( ifelse( x1>=x2, 1, 2 ) )
		
}
