# problem3.R  Simulation of a signal detection model of a yes-no task

sdtsim <- function(mu1 = 0, mu2 = 3, sigma = 2, criterion = 1.5, ntrials = 1000) {

	# calculate d' and c from observer parameters
	dprimecalc <- (mu2 - mu1)/sigma
	ccalc <- (criterion - (mu1 + mu2)/2)/sigma

	# initialize data frame
	init <- rep(NA, ntrials)
	trials <- data.frame(signal = init, response = init, dvar = init)
	
	# choose signals
	trials$signal <- ifelse( runif(ntrials)<0.5, 1, 2 )
	
	# get decision variables
	trials$dvar <- rnorm( ntrials, mean=ifelse( trials$signal==1, mu1, mu2 ), sd=sigma )
	
	# make responses
	trials$response <- ifelse( trials$dvar<criterion, 1, 2 )
	trials$correct <- (trials$response == trials$signal)
	
	# find the hit rate
	hit <- mean(trials$response[trials$signal == 2] == 2)
	fa <- mean(trials$response[trials$signal == 1] == 2)

	# calculate d' and c from data
	dprimehat <- qnorm(hit) - qnorm(fa)
	chat <- -0.5 * (qnorm(hit) + qnorm(fa))

	# find proportion correct
	pcorrect = mean(trials$correct)

	# make return argument
	v <- list(trials = trials, dprimecalc = dprimecalc, dprimehat = dprimehat, 
		ccalc = ccalc, chat = chat, pcorrect = pcorrect)
	return(v)

}
