# clear the workspace
rm(list = ls())

# read in bob
source('problem set 2/model.R')

# a function that takes a vector 'depths' with 2 values and
# runs bob assuming the first value is depth from either D1 or T1 (or both), 
# and the second value is depth from either D2 or T2 (or both)
applyBob <- function(depths, depthCue = 'disparity'){
  if (depthCue == 'disparity') {
    return (bob(depthD1 = depths[1], depthD2 = depths[2]))
  }
  
  else if (depthCue == 'touch') {
    return (bob(depthT1 = depths[1], depthT2 = depths[2]))
  }
  
  else if (depthCue == 'both') {
    return (bob(depthD1 = depths[1], depthT1 = depths[1], depthD2 = depths[2], depthT2 = depths[2]))
  }
}

# function to record bob's responses given some stimulus depths
# dethCue can be 'dispariity', 'touch', or 'both'
makeBobDF <- function(stim1Depth, stim2Depth, depthCue){
  # create a dataframe with stimuli and empty columns to fill out
  df <- data.frame('stim1Depth' = stim1Depth,
                   'stim2Depth' = stim2Depth,
                   'bobResponse' = NaN,
                   'verdict' = NaN)
  
  # apply bob to each row and add a verdict column
  # we say bob scores a 'Hit' when bob says the deeper stimulus is 1 when that is true
  # when bob says the deeper stimulus is 2 when that is true, we say that is a 'Correct Rejection'
  df$bobResponse <- apply(df, 1, applyBob, depthCue = depthCue)
  df$verdict <- ifelse(df$bobResponse == 1 & 
                         df$stim1Depth >= df$stim2Depth, 'H', 
                       ifelse (df$bobResponse == 2 & 
                                 df$stim1Depth >= df$stim2Depth, 'M', 
                               ifelse(df$bobResponse == 1 & 
                                        df$stim1Depth < df$stim2Depth, 'FA', 
                                      'CR')))
  return(df)
}


# Q1: d'
disparityDF <- makeBobDF(stim1Depth = rep(seq(from = 10, to = 12, length.out = 10), each = 20),
                         stim2Depth = 11,
                         depthCue = 'disparity')

touchDF <- makeBobDF(stim1Depth = rep(seq(from = 10, to = 12, length.out = 10), each = 20),
                     stim2Depth = 11,
                     depthCue = 'touch')

# obtain hit and false alarm rates
disparity_verdicts <- table(disparityDF$verdict)
touch_verdicts <- table(touchDF$verdict)

#calculate d' (divide by sqrt(2) since its a 2AFC task)
dPrime_disparity <-  (qnorm(disparity_verdicts['H'] / 200) - qnorm(disparity_verdicts['FA'] / 200)) / sqrt(2)
dPrime_touch <-  (qnorm(touch_verdicts['H'] / 200) - qnorm(touch_verdicts['FA'] / 200)) / sqrt(2)

#### PRINT THE dprimes

# Q2
# function used to determine the proporion of times bob says '1 was closer'
applyn1Closer <- function(stim1Depth, df)
  return (sum(df[df$stim1Depth == stim1Depth[1], ]$bobResponse == 1))

# form of the cdf
cdf <- function(x, mu, sigma)
  pnorm( x, mu, sigma )

findPhat <- function(stim1Depth, stim2Depth, depthCue, pinit){
  
  # create a dataframe where bob does a lot of trials of this task with disparity as the cue
  expDF <- makeBobDF(stim1Depth = stim1Depth,
                     stim2Depth = stim2Depth,
                     depthCue =  depthCue)
  
  # intialize a dataframe where first column is the unique stim1s
  forEachDF <- data.frame('stim1Depth' = unique(expDF$stim1Depth),
                          'nTrials' = 20,
                          'n1Closer' = NaN,
                          'p1Closer' = NaN)
  
  forEachDF$n1Closer <- apply(forEachDF, 1, applyn1Closer, df = expDF) 
  forEachDF$p1Closer <- forEachDF$n1Closer / 20
  
  # plot the data
  plot(forEachDF$stim1Depth, forEachDF$p1Closer, type = 'p')
  
  # fit a cdf to this data
  # a maximum likelihood objective function --> we will minimize this
  # we are looking for mu (p[1]) and sigma (p[2])
  obj <- function(p)
    -sum(log(dbinom(forEachDF$n1Closer, 
                    forEachDF$nTrials, 
                    cdf(forEachDF$stim1Depth, p[1], p[2]))))
  
  pinit <- pinit
  return (optim(pinit, obj)$par)
}

# set the stimulus depths and a plausibile pinit
stim1Depth <- rep(seq(from = 0, to = 20, length.out = 100), each = 20)
stim2Depth <- 10
pinit <- c(10,3)

# calculate an estimate of sigma when judging depth using binocular disparity, touch, and both
sigma_disparity <- findPhat(stim1Depth = stim1Depth, 
                            stim2Depth = stim2Depth, 
                            depthCue = 'disparity', 
                            pinit = pinit)[2] /sqrt(2)

sigma_touch <- findPhat(stim1Depth = stim1Depth, 
                        stim2Depth = stim2Depth, 
                        depthCue = 'touch', 
                        pinit = pinit)[2] /sqrt(2)

sigma_both <- findPhat(stim1Depth = stim1Depth, 
                       stim2Depth = stim2Depth, 
                       depthCue = 'both', 
                       pinit = pinit)[2] /sqrt(2)
