# Author: Ivan Bielik
# I analysed the data in late February 2020 and wrote the article about it
# in the journal Politics of Central Europe.
# The script employs tidyverse package. If you do not have it installed,
# use install.packages("tidyverse") in your R console


# load necessary packages
library(tidyverse)

# load data and compute their number of rows
polls <- read_csv("data/cssd_polls_data.csv")
(polls_len <- nrow(polls))

# set hypotheses and probabilities from normal distribution
# based on the analysis in second R script "01_hypo_testing.R"
hypo_k <- seq(0.89, 0.99, by = 0.01)
prob_k <- pnorm(hypo_k, mean = 1.15, sd = 0.27)

# set number of trials for random sampling from a distribution
trials <- 1000

# set seed for a reproducible analysis
set.seed(17)

# create dataset with possible k values, their prior and likelihood
# probabilities 
# likelihood is drawn from 1000 random samples from beta distibution
df <- tibble(k = hypo_k) %>% 
  mutate(model_vote = 0.0727^k) %>%
  mutate(occur = map_int(.$model_vote, 
                         ~ length(which(polls$value >= .x))),
         prior = prob_k) %>%
  mutate(like = map(.$occur,~ rbeta(trials, .x, polls_len - .x)))


# add posterior mean and normalised posterior to dataframe
df <- df %>%
  mutate(posterior = map_dbl(1:nrow(.), 
                             ~ mean(df$prior[.x] * df$like[[.x]]))) %>%
  mutate(norm_posterior = posterior / sum(posterior)) 

# check that norm_posterior is 1
sum(df$norm_posterior)

# visualise normalised posterior on a graph
p <- df %>% ggplot(aes(k, norm_posterior)) +
    geom_point() +
    geom_line() 

p + labs(
  title = "Probability of k exponent in the interval <0.89; 0.99>",
  x = "Value o k exponent",
  y = "Normalised probability"
) +
  theme_minimal()
