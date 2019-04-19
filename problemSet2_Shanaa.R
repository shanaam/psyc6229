# clear the workspace
rm(list = ls())

# read in bob
source('problem set 2/model.R')

# set up a function that can be used by the apply() function.
# a function that takes a vector 'depths' with 2 values and
# runs bob assuming the first value is depth from either D1 or T1 (or both), 
# and the second value is depth from either D2 or T2 (or both)
applyBob <- function(depths, depthCue = 'disparity'){
  # this first part is used for question 3
  if (length(depths) == 4){
    return(bob(depthD1 = depths[1], depthT1 = depths[2], depthD2 = depths[3], depthT2 = depths[4]))
  }
  
  # for questions 1 and 2
  else {
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
}

# function to record bob's responses, in a dataframe, given some stimulus depths (used for questions 1 and 2)
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
  df$bobResponse <- apply(df[, 1:2], 1, applyBob, depthCue = depthCue)
  df$verdict <- ifelse(df$bobResponse == 1 & 
                         df$stim1Depth >= df$stim2Depth, 'H', 
                       ifelse (df$bobResponse == 2 & 
                                 df$stim1Depth >= df$stim2Depth, 'M', 
                               ifelse(df$bobResponse == 1 & 
                                        df$stim1Depth < df$stim2Depth, 'FA', 
                                      'CR')))
  return(df)
}

### Q1: d'

# we will present 2 stimuli, one with 10cm depth and another with 12 cm depth

# the two possible depths
Q1_depths <- c(10,12)

# make a vector of which stimulus gets shown first
stim1Depth <- sample(Q1_depths, 200, replace = TRUE)

# make a vector of the second stimulus, given the first
otherStim <- function(stim1)
  return(ifelse(stim1 == 10, 12, 10))

stim2Depth <- sapply(stim1Depth, FUN = otherStim)

# create a dataframe with the stimuli presented to bob, and bob's responses
# with disparity as the depth que
disparityDF <- makeBobDF(stim1Depth = stim1Depth,
                         stim2Depth = stim2Depth,
                         depthCue = 'disparity')

# repeat for touch
touchDF <- makeBobDF(stim1Depth = stim1Depth,
                     stim2Depth = stim2Depth,
                     depthCue = 'touch')

# calculate d' (divide by sqrt(2) since its a 2AFC task)
# sum() works for counting the 'H' and 'FA' since our TRUE is treated as a 1, and FALSE is treated as a 0
# when doing arithmetic on a vector of booleans
dPrime_disparity <-  (qnorm(sum(disparityDF$verdict == 'H') / 200) - qnorm(sum(disparityDF$verdict == 'FA') / 200)) / sqrt(2)
dPrime_touch <-  (qnorm(sum(touchDF$verdict == 'H') / 200) - qnorm(sum(touchDF$verdict == 'FA') / 200)) / sqrt(2)

# conclusion on dprimes
sprintf("When bob discriminates between depths of 10 cm and 12 cm, his d' when using only binocular disparity is %.*f, whereas his d' when using only touch is %.*f.", 3, dPrime_disparity, 3, dPrime_touch)



# Q2
# set the stimulus depths and a plausibile pinit
stim1Depth <- rep(seq(from = 0, to = 20, length.out = 100), each = 20)
stim2Depth <- 10
pinit <- c(10,3)

# below are some functions used in findPhat
# function used to determine the proporion of times bob says '1 was closer'
applyn1Closer <- function(stim1Depth, df)
  return (sum(df[df$stim1Depth == stim1Depth[1], ]$bobResponse == 1))

# form of the cdf
cdf <- function(x, mu, sigma)
  pnorm(x, mu, sigma)

# findPhat finds plausible values for p (a parameter of length 2 we are optimizing)
# given depths of stimuli presented to bob and the depth cue bob is to use
# note that in an experiment stim1 does not have to be presented first
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
  
  # fit a cdf to this data
  # a maximum likelihood objective function --> we will minimize this
  # we are looking for mu (p[1]) and sigma (p[2])
  obj <- function(p)
    -sum(log(dbinom(forEachDF$n1Closer, 
                    forEachDF$nTrials, 
                    cdf(forEachDF$stim1Depth, p[1], p[2]))))
  
  pinit <- pinit
  phat <- optim(pinit, obj)$par
  
  # plot the data
  plot(forEachDF$stim1Depth, forEachDF$p1Closer, type = 'p', 
       xlab = 'depth of stimulus 1 (cm)', ylab = 'proportion stimulus 1 judged deeper', 
       main = 'PSE function', bty="n")
  
  # add the fitted curve
  curve(cdf(x, phat[1], phat[2]), from = min(stim1Depth), to = max(stim1Depth), col='blue', add=TRUE)
  
  # add error bars
  stderr <- sqrt(forEachDF$p1Closer * (1 - forEachDF$p1Closer) / 20)
  stderr <- pmax(stderr, 0.001)             # replace error bars with 0 length
  plow  <- forEachDF$p1Closer - stderr;     # lower end of error bars
  phigh <- forEachDF$p1Closer + stderr;     # upper end of error bars
  arrows(forEachDF$stim1Depth, plow, forEachDF$stim1Depth, phigh, code = 3, angle = 90, length = 0.03, col = 'black')
  
  # add a legend
  legend('bottomright', inset = 0.05, legend = c('data','fit'), 
         col = c('black','blue'), pch = c(1,NA), lty = c(NA,1))
  
  return (phat)
}


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

# conclusion
print('If bob is combining binocular disparity and touch cues optimally, we expect: 1/sigma_both^2 = 1/sigma_disparity^2 + 1/sigma_touch^2')
sprintf("The calculated value of sigma_both, given sigma_disparity and sigma_touch is %.*f. However, bob's actual sigma_both is %.*f. Bob is likely not optimally combining both cues", 3, 1/sigma_disparity^2 + 1/sigma_touch^2, 3, sigma_both)





# Q3

# set the stimulus depths and a plausibile pinit
stim1Depth_disparity<- rep(seq(from = 2, to = 22, length.out = 100), each = 20)
stim1Depth_touch <- rep(seq(from = 2, to = 22, length.out = 100), each = 20)
stim2Depth_disparity <- 10
stim2Depth_touch <- 14
pinit <- c(10,3)

# create a dataframe with stimuli and empty columns to fill out
bobDF_conflict <- data.frame('stim1Depth_disparity' = stim1Depth_disparity,
                 'stim1Depth_touch' = stim1Depth_touch,
                 'stim2Depth_disparity' = stim2Depth_disparity,
                 'stim2Depth_touch' = stim2Depth_touch)
# apply bob 
bobDF_conflict$bobResponse <- apply(bobDF_conflict[, 1:4], 1, applyBob)

# intialize a dataframe where first column is the unique stim1s
forEachDF_conflict <- data.frame('stim1Depth' = unique(stim1Depth_disparity),
                        'nTrials' = 20,
                        'n1Closer' = NaN,
                        'p1Closer' = NaN)

# slight change to the apply function
applyn1Closer <- function(stim1Depth, df)
  return (sum(df[df$stim1Depth_disparity == stim1Depth[1], ]$bobResponse == 1))

# calculate proportion of times stimulus 1 is judged deeper
forEachDF_conflict$n1Closer <- apply(forEachDF_conflict, 1, applyn1Closer, df = bobDF_conflict) 
forEachDF_conflict$p1Closer <- forEachDF_conflict$n1Closer / 20

# objective function
obj <- function(p)
  -sum(log(dbinom(forEachDF_conflict$n1Closer, 
                  forEachDF_conflict$nTrials, 
                  cdf(forEachDF_conflict$stim1Depth, p[1], p[2]))))

# optimize
phat <- optim(pinit, obj)$par

# plot the data
plot(forEachDF_conflict$stim1Depth, forEachDF_conflict$p1Closer, type = 'p', 
     xlab = 'depth of stimulus 1 (cm)', ylab = 'proportion stimulus 1 judged deeper', 
     main = 'PSE function for cue conflict', bty="n")

# add the fitted curve
curve(cdf(x, phat[1], phat[2]), from = min(stim1Depth_disparity), to = max(stim1Depth_disparity), col='blue', add=TRUE)

# add error bars
stderr <- sqrt(forEachDF_conflict$p1Closer * (1 - forEachDF_conflict$p1Closer) / 20)
stderr <- pmax(stderr, 0.001)             # replace error bars with 0 length
plow  <- forEachDF_conflict$p1Closer - stderr;     # lower end of error bars
phigh <- forEachDF_conflict$p1Closer + stderr;     # upper end of error bars
arrows(forEachDF_conflict$stim1Depth, plow, forEachDF_conflict$stim1Depth, phigh, code = 3, angle = 90, length = 0.03, col = 'black')

# add a legend
legend('bottomright', inset = 0.05, legend = c('data','fit'), 
       col = c('black','blue'), pch = c(1,NA), lty = c(NA,1))


# conclusion
print('Given our sigmas from part 2, if bob is combining binocular disparity and touch cues optimally, we expect the percieved depth of the conflicting stimulus to be a weighted sum of the depths provided by disparity and touch')
print('The weight assigned to a cue, c, is given by the formula w_c = (1/sigma_c^2) / ((1/sigma_disparity^2) + (1/sigma_touch^2)')

# calculate the theoretical weights
w_disparity <- (1/sigma_disparity^2) / ((1/sigma_disparity^2) + (1/sigma_touch^2))
w_touch <- (1/sigma_touch^2) / ((1/sigma_disparity^2) + (1/sigma_touch^2))

# calculate the theoretical mu
mu_pred <- w_disparity * stim2Depth_disparity + w_touch * stim2Depth_touch

sprintf('Given the calculated weights for the depth from binocular disparity (%.*f), and touch (%.*f), we expect an optimal bob to percieve stimulus 2 to be at a depth of %.*f. However, our model indicates that the point of subjective equality, that is the mean of the fitted cdf, is %.*f. This again indicates that bob is not likely combining the cues optimally.', 
        3, w_disparity, 3, w_touch, 3, mu_pred, 3, phat[1])
