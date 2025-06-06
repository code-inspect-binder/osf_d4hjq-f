# Author: Ivan Bielik
# I analysed the data in late February 2020 and wrote the article about it
# in the journal Politics of Central Europe.
# The script employs base functions of R and thus it should work
# on every computer that has a base version of R installed.

# results from the analysis of electoral change for CSSD
# k <- c(0.85, 1.08, 0.94, 1.34, 1.05, 1.66)



# PRIOR DISTRIBUTION

# visualisation based on the analysis of vote share change of CSSD using normal
# distribution and principal exponent equation from Taagepera
xs <- seq(0, 2.5, by = 0.01)
plot(xs, dnorm(xs, mean = 1.15, sd = 0.27), t = "l", lwd = 2, 
     main = "Normal distribution of k exponent, N(1.15, 0.27)",
     xlab = "Possible values of k",
     ylab = "Density")

# create vector of probabilities
hypos <- seq(0, 1, by = 0.01)

# plots of prior distributions
par(mfrow = c(3, 1))
plot(hypos, dbeta(hypos, 3, 7), type = "l", lwd = 2,
     main = "Prior distribution of H1: beta(3, 7)",
     xlab = "Probability of x",
     ylab = "Density")
plot(hypos, dbeta(hypos, 7, 3), type = "l", lwd = 2,
     main = "Prior distribution of H2: beta(7, 3)",
     xlab = "Probability of x",
     ylab = "Density")
plot(hypos, dbeta(hypos, 1, 5), type = "l", lwd = 2,
     main = "Prior distribution of H3: beta(1, 5)",
     xlab = "Probability of x",
     ylab = "Density")



# POSTERIOR DISTRIBUTION


# load the dataset with election polls
df <- read.csv("data/cssd_polls_data.csv")

# how many polls happened during the last term?
(length(df$value))   # 71 observations

# how many polls have measured the result to be more than 7.27%?
(length(which(df$value > 0.0727)))   # 43 successes
# it also follows that if we want to find out df$value <= 0.0727
# we need to just substract 43 from 71 observations

# how many polls have measured the result to be more than 10%?
(length(which(df$value >= 0.1)))   # 14 successes

# statement of hypotheses and their translation into beta distribution

# H1: CSSD will increase their vote share in next elections - beta(43+3, 28+7)
# H2: CSSD will decrease their vote share in next elections - beta(28+7, 43+3)
# H3: CSSD will gain double digits in vote share in next elections - beta(14+1, 57+5)

# plots of posterior distributions
par(mfrow = c(3, 1))
plot(hypos, dbeta(hypos, 43+3, 28+7), type = "l", lwd = 2,
     main = "Posterior distribution of H1: beta(43+3, 28+7)",
     xlab = "Probability of x",
     ylab = "Density")
plot(hypos, dbeta(hypos, 28+7, 43+3), type = "l", lwd = 2,
     main = "Posterior distribution of H2: beta(28+7, 43+3)",
     xlab = "Probability of x",
     ylab = "Density")
plot(hypos, dbeta(hypos, 14+1, 57+5), type = "l", lwd = 2,
     main = "Posterior distribution of H3: beta(14+1, 57+5)",
     xlab = "Probability of x",
     ylab = "Density")

# Example: finding 90 % confidence intervals for beta distributions
params <- c(0.05, 0.95)
ci_h1 <- qbeta(params, 43+3, 28+7)

# it is evident that we need to decide between H1 and H2 from the visualisation
# of posterior distributions 
# H3 is not very likely out of observed data and thus we will focus on H1 and H2


# MONTE CARLO SIMULATION


# Which hypothesis better explains the observed data? 
# A/B test of H1 and H2
# assumption: H1 (variant A) perform better than H2 (variant B)

# for reproducibility of analysis
set.seed(17)

# set number of draws from distribution
trials <- 100000

A <- rbeta(trials, 43+3, 28+7)
B <- rbeta(trials, 28+7, 43+3)

# which hypothesis better explains probabilities
sum(A > B) / trials

par(mfrow = c(1, 2))
hist(A/B, main = "Histogram of H1/H2",
     xlab = "Improvement of H1 over H2")
plot(ecdf(A/B), main = "ECDF of H1/H2",
     xlab = "Improvement of H1 over H2",
     ylab = "Probability")

# summary statistics of A/B
summary(A/B)
