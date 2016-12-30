library(tidyverse)
library(Lahman)

bat <- Batting %>% 
  mutate(PA = AB + BB + HBP + SF,
         BB_perc = BB / PA) %>%
  filter(yearID >= 2008,
         PA > 0) %>% 
  anti_join(Pitching, by = "playerID") 
  
g <- ggplot(bat %>% filter(PA >= 250), aes(BB_perc)) 

g + geom_histogram()

m.beta <- MASS::fitdistr(bat$BB_perc[bat$PA >= 250], "beta",
                         start = list(shape1 = 1, shape2 = 10))

g + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dbeta,
                color = "red",
                args = list(coef(m.beta)[[1]], coef(m.beta)[[2]]))

qqplot(bat$BB_perc[bat$PA >= 250], rbeta(5000, coef(m.beta)[[1]], coef(m.beta)[[2]]))
abline(0,1)

library(VGAM)

# negative log likelihood of data given alpha; beta
ll <- function(alpha, beta) {
  -sum(dbetabinom.ab(bat$BB, bat$PA, alpha, beta, log = TRUE))
}

m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B")
coef(m)

g + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dbeta,
                color = "red",
                args = list(coef(m)[[1]], coef(m)[[2]]))

qqplot(bat$BB_perc[bat$PA >= 250], rbeta(5000, coef(m)[[1]], coef(m)[[2]]))
abline(0,1)

## Shows not the best fit, need to transform the data?

m.gamma <- MASS::fitdistr(bat$BB_perc[bat$PA >= 250], "gamma")

qqplot(bat$BB_perc[bat$PA >= 250], rgamma(5000, m.gamma$estimate[[1]], m.gamma$estimate[[2]]))
abline(0,1)

g + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dgamma,
                color = "red",
                args = list(m.gamma$estimate[[1]], m.gamma$estimate[[2]]))

## Much better fit, can we transform the data before calculation to get a
## beta distribution