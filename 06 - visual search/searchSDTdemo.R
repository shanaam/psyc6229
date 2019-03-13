# searchSDTdemo.R
# Simulation of a search task using the parallel signal detection theory (SDT) model.

library(ggplot2)

################################################
# Parameters
################################################

# dprime for single target vs. single distractor discrimination
# use a high dprime to simulate an efficient "parallel" search, and lower dprime for inefficient "serial" search
dprime <- 2

# set sizes: number of items in the simulated displays
setsize <- c(3,6,12,18)

# number of trials to simulate
nTrials <- 500

################################################
# Simulate a series of trials at each set size
################################################

# Matrices to store the responses for target-present and target-absent displays
targs <- matrix(ncol=length(setsize), nrow=nTrials)
dists <- matrix(ncol=4, nrow=nTrials)

# Loop over set size
for (i in 1:length(setsize)) {
    nitems <- setsize[i] # number of items in the display

    # Simulate trials
    counter <- 1
    while (counter <= nrow(targs)) {

        # Simulate a target display (1 sample from target distribution and N-1 from distractor distribution)
        tsample <- c(rnorm(1, mean=(dprime/2), sd=1), rnorm(nitems-1, mean=(-dprime/2), sd=1))

        # Simulate a distractor display (N samples from distractor distribution)
        dsample <- rnorm(nitems, mean=(-dprime/2), sd=1)

        # Observer's response is based on the maximum of each distribution
        targs[counter,i] <- max(tsample)
        dists[counter,i] <- max(dsample)

        counter <- counter+1
    }

    # Combine the target and distractor response data into a single data frame (for plot)
    targtmp <- data.frame(response=targs[,i], label="Tpresent")
    disttmp <- data.frame(response=dists[,i], label="Tabsent")
    stimuli <- rbind(targtmp,disttmp)

    # Draw the histograms showing the Maximum Rule response for this set size
    # Note that you need to print() the plot when calling ggplot() inside a loop
    ggp <- ggplot(stimuli, aes(response, fill=label)) + geom_density(alpha=0.5) + ggtitle(paste("Setsize = ",nitems))
    dev.new()
    print(ggp)

}

################################################
# Compute observer's dprime for each set size
################################################

# Difference in means of response distributions
diffMeans <- colMeans(targs) - colMeans(dists)

# Combined standard deviations of the response distributions
sds <- sqrt(( (apply(targs,2,sd)^2) + (apply(dists,2,sd)^2) ) / 2)

# dprime = difference in means / sd
dSetsize <- diffMeans / sds

# Plot dprime for each set size
dev.new()
plot(setsize, dSetsize, xlab="Set size", ylab="Simulated d prime", main=paste("d prime = ",dprime))

################################################
# Simulate response time (RT) from dprime
################################################
# Loosely based on speed-accuracy trade-off formula from McElree & Carrasco (1999)

# Parameters
nonsearchRT <- 400 # time (in msec) for non-search processes (sets minimum possible RT)
beta <- 500 # scaling factor
maxDprime <- 6.2 # maximum possible dprime for this task (dprime when RT = Inf ms)

# Simulated RT from dprime
simulatedRT <- nonsearchRT + (beta * (1 - (log(dSetsize)/log(maxDprime))) )

# Plot simulated RT for each set size with a linear regression line
dev.new()
plot(setsize, simulatedRT, xlab="Set size", ylab="RT (msec)", main=paste("Simulated RT over set size, d prime = ",dprime))
regressline <- lm(simulatedRT ~ setsize)
abline(regressline)

# Print info about the linear fit
summary(regressline)

################################################
# Suggested experiments:
#
# 1. Try to find dprime values which will recreate the RT/set size slopes
# observed for feature (R_vs_G), conjunction (RV_vs_GVRH), and configuration
# (2_vs_5) search in the experiment.
#
# 2. See what happens when the target and distractor distributions have
# unequal standard deviation. (In the rnorm() function, change sd=1 to another
# value, like sd=3.)
################################################