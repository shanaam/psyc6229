# searchDatademo.R
# Analyze data from a search experiment. Data can be found in the file: PSYC6229_search_data.csv
# 
# Search experiment data downloaded from:
# http://search.bwh.harvard.edu/new/data_set_files.html

################################################
# Load and check the data
################################################

# Read data into data frame (change the filepath to the correct data location):
data <- read.table("C:/Users/krist/OneDrive/Desktop/PSYC6229_search_data.csv", header=TRUE, sep=",")

# View the data frame
View(data)

# Check which search conditions are included in this data
unique(data$Condition)

# Check which set sizes were used in these experiments
unique(data$Setsize)

# Get a quick summary of the data in each condition
summary(subset(data, Condition == "R_vs_G"))
summary(subset(data, Condition == "RV_vs_RHGV"))
summary(subset(data, Condition == "2_vs_5"))

################################################
# Plot RT distributions
################################################

# Set up a 3x4 subplot figure (3 rows of 4 subplots)
par(mfrow=c(3,4))

# For each condition
for (i in unique(data$Condition)) {

    # For each set size (note the sort(): this ensures the set sizes are in increasing order)
    for (j in sort(unique(data$Setsize))) {

        # Plot a histogram of the RTs in this condition X set size
        hist(data$RT[data$Condition==i & data$Setsize==j],
        xlab="RT in msec",
        main=paste(i,", set size = ",j),
        col="blue")
    }
}

# Note that some RTs are huge (>10 seconds). To get a better sense of the
# shape of the distributions, you might try plotting only the faster RTs,
# e.g., RTs < 2000 msec:
# data$RT[data$Condition==i & data$Setsize==j & data$RT < 2000]

################################################
# Investigate the error rates
################################################

# Quick summary of the average %error in each condition X set size
aggregate(data$Error, list(data$Condition,data$Setsize), mean)

# Make a new 3x4 subplot figure 
dev.new()
par(mfrow=c(3,4))

# Plot error types (miss and false alarms) for each condition X set size
for (i in unique(data$Condition)) {
    for (j in sort(unique(data$Setsize))) {

        # "Type" data from this condition X set size
        # Type labels each trial as one of: HIT, MISS, FA, TNEG
        tmp <- data$Type[data$Condition==i & data$Setsize==j]

        # Number of each type of trial
        counts <- table(tmp)

        # Plot the two error types (False Alarm and Miss) as percentages
        barplot(100*counts[c("FA","MISS")]/sum(counts),
            ylim = c(0,5),
            main=paste(i,", set size = ",j))
    }
}

# The preceeding graphs are okay for exploring the data, but for a
# publication you would want to plot %error by subject, with standard
# error bars. Here's how to do that:

# Make a new 3x4 subplot figure 
dev.new()
par(mfrow=c(3,4))

# For each condition
for (i in unique(data$Condition)) {

    # For each set size
    for (j in sort(unique(data$Setsize))) {

        # Extract data from this condition X set size
        tmp <- data[data$Condition==i & data$Setsize==j,]

        # Compute mean error for each subject X trial type (target present or absent)
        bySub <- aggregate(tmp$Error, list(tmp$Subject,tmp$Tpresent), mean)

        # Average over subjects (and multiply by 100 to get a whole number percentage)
        meansBySub <- aggregate(bySub$x, list(bySub$Group.2), mean)
        means <- 100 * meansBySub$x

        # Standard deviation of subject means
        sds <- aggregate(bySub$x, list(bySub$Group.2), sd)

        # Standard error is standard devation divided by square root number of subjects
        sderr <- 100 * sds$x / sqrt(length(unique(bySub$Group.1)))

        # Plot the means as bars
        barHeights <- barplot(means, names.arg = list("FA","MISS"),
            ylim = c(0,12), main=paste(i,", set size = ",j))

        # Add error bars to represent standard error
        arrows(barHeights, means - sderr, barHeights, means + sderr,
            lwd = 1.5, angle = 90, code = 3, length = 0.05)
    }
}

################################################
# Compute dprime for each subject
################################################

# Make a new 3x4 subplot figure 
dev.new()
par(mfrow=c(3,4))

# For each condition
for (i in unique(data$Condition)) {

    # For each set size
    for (j in sort(unique(data$Setsize))) {

        # Extract data from this condition X set size
        tmp <- data[data$Condition==i & data$Setsize==j,]

        # Compute mean error for each subject X trial type (ground truth target present or absent)
        bySub <- aggregate(tmp$Error, list(tmp$Subject,tmp$Tpresent), mean)

        # If a subject has 100% hit rate or 0% false alarm rate, their dprime will be infinity.
        # To avoid this problem, replace values near 0 or 1 with values slightly above 0 / below 1.
        bySub$x[bySub$x < 0.005] <- 0.005
        bySub$x[bySub$x > 0.995] <- 0.995

        # Compute z-score of hit rate (1 - % errors on target-present trials)
        zHIT <- qnorm(1-bySub$x[bySub$Group.2==1])

        # Compute z-score of false alarm rate (% errors on target-absent trials)
        zFA <- qnorm(bySub$x[bySub$Group.2==0])

        # Compute dprime: z(Hit) - z(FA)
        dprimeBySub <- zHIT - zFA

        # Plot each subject's dprime as a bar plot
        barHeights <- barplot(dprimeBySub, names.arg =
            unique(bySub$Group.1), ylim = c(0,7),
            main=paste(i,", set size = ",j))
    }
}

################################################
# Check if RT distributions are normal
################################################

# Matrix to store test results
p <- matrix(ncol=5, nrow=4*29*2)
colnames(p) <- list("cond", "setsize", "subject", "tpres", "pval")

# Run a test of normality for the RTs in each condition X set size X subject X trial type
counter <- 1
# For each condition
for (i in unique(data$Condition)) {

    # For each set size
    for (j in sort(unique(data$Setsize))) {

        # For each subject in this condition X set size
        for (ii in unique(data$Subject[data$Condition==i & data$Setsize==j])) {

            # For each trial type (target present or absent)
            for (jj in unique(data$Tpresent)) {

                # Extract the data for this condition X set size X subject X trial type
                tmp <- data[data$Condition==i & data$Setsize==j & data$Subject==ii & data$Tpresent==jj,]

                # Run the Shapiro-Wilks test of normality
                result <- shapiro.test(tmp$RT)

                # Save the p value of the test in the matrix
                p[counter,] <- c(i, j, ii, jj, result$p.value)

                counter <- counter + 1
            }
        }
    }
}

# Convert the matrix of test results a data frame and view it
testresults = as.data.frame(p)
View(testresults)

# The Shapiro-Wilks test's null hypothesis is that the data is normally-distributed.
# Significant p values means you can reject the null hypothesis -- the data is not normal.

################################################
# Trim RTs to remove outliers
################################################

# Trimming is a standar technique to make RT distributions closer to normal.
# However, it can introduce biases, e.g.:
# Miller, J. (1991). Reaction time analysis with outlier exclusion:
# Bias varies with sample size. The Quarterly Journal of Experimental
# Psychology, 43A(4), 907-912.

# Loop through data, identify bad RTs for each condition X set size X subject X trial type and save trimmed RTs in a new data frame
flag <- 1
# For each condition
for (i in unique(data$Condition)) {

    # For each set size
    for (j in sort(unique(data$Setsize))) {

        # For each subject in this condition X set size
        for (ii in unique(data$Subject[data$Condition==i & data$Setsize==j])) {

            # For each trial type (target present or absent)
            for (jj in unique(data$Tpresent)) {

                # Extract the data for this condition X set size X subject X trial type
                tmp <- data[data$Condition==i & data$Setsize==j & data$Subject==ii & data$Tpresent==jj,]

                # Only keep RTs within a few standard deviations (2.5 or 3) of the mean
                thHigh <- mean(tmp$RT) + (2.5*sd(tmp$RT))
                thLow <- mean(tmp$RT) - (2.5*sd(tmp$RT))

                # Reduce data to just trials where the RTs were within the two thresholds
                new <- tmp[tmp$RT > thLow & tmp$RT < thHigh,]

                # Build a new data frame (data2) with the trimmed RT data
                if (flag==1) {
                    data2 <- new
                    flag <- 0
                } else {
                    data2 <- rbind(data2, new)
                }
            }
        }
    }
}

# Save a backup of the original data and replace with the trimmed data
dataOLD <- data
data <- data2

# Plot the RT histograms again: the RT ranges should look more reasonable now
dev.new()
par(mfrow=c(3,4))
for (i in unique(data$Condition)) {
    for (j in sort(unique(data$Setsize))) {
        hist(data$RT[data$Condition==i & data$Setsize==j],
        xlab="RT in msec",
        main=paste(i,", set size = ",j),
        col="blue")
    }
}

################################################
# Plot search slopes
################################################

# The x values for the plots: set size
x <- sort(unique(data$Setsize))

# For each condition
for (i in unique(data$Condition)) {

    # Make a new figure for each condition
    dev.new()

    # Extract the data from this condition
    tmp <- data[data$Condition==i,]

    # Compute the mean RT for each subject X trial type X set size
    bySub <- aggregate(tmp$RT, list(tmp$Subject,tmp$Tpresent,tmp$Setsize), mean)

    # Average over subjects (to get means over trial type X set size)
    means <- aggregate(bySub$x, list(bySub$Group.2, bySub$Group.3), mean)

    # Make sure means are ordered by set size
    means <- means[order(means$Group.2),]

    # Extract the means for target-absent trials
    y0 = means$x[means$Group.1==0]

    # Extract the means for target-present trials
    y1 = means$x[means$Group.1==1]

    # Plot the target-absent and target-present mean RTs as points
    plot(x, y0, col="blue", ylim=c(0,2500), xlab="Set size", ylab="RT (ms)", main=i)
    points(x, y1, col="red")

    # Show a linear regression line for target-absent points
    regressline <- lm(y0 ~ x)
    abline(regressline)

    # Show a linear regression line for target-present points
    regressline <- lm(y1 ~ x)
    abline(regressline)

}