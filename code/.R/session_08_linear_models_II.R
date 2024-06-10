# load packages
pacman::p_load(tidyverse, rethinking)

# load dark theme
source("code/.R/dark_theme.R")

# introductory example ----------------------------------------------------

data(WaffleDivorce)
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

ggplot(d, aes(x = M, y = D)) + 
  geom_point(size = 3, color = "#ff02ff") + 
  geom_smooth(method = "lm", color = "#ff02ff") +
  labs(x = "Marriage Rate", 
       y = "Divorce Rate") + 
  dark_theme()


m1_intro <- quap(
  alist( D ~ dnorm(mu, sigma), 
         mu ~ a + bM * M, 
         a ~ dnorm(0, .2), 
         bM ~ dnorm(0, .5), 
         sigma ~ dexp(1)), 
  data = d
)
precis(m1_intro)

ggplot(d, aes(x = M, y = A)) + 
  geom_point(size = 3, color = "#ff02ff") + 
  geom_smooth(method = "lm", color = "#ff02ff") +
  labs(x = "Marriage Rate", 
       y = "Median Age Marriage") + 
  dark_theme()

ggplot(d, aes(x = D, y = A)) + 
  geom_point(size = 3, color = "#ff02ff") + 
  geom_smooth(method = "lm", color = "#ff02ff") +
  labs(x = "Divorce Rate", 
       y = "Median Age Marriage") + 
  dark_theme()

m2_intro <- quap(
  alist( D ~ dnorm(mu, sigma), 
         mu ~ a + bM * M + bA * A, 
         a ~ dnorm(0, .2), 
         bM ~ dnorm(0, .5),
         bA ~ dnorm(0, .5), 
         sigma ~ dexp(1)), 
  data = d
)
precis(m2_intro)


# Exercie: Multiple Regression Model --------------------------------------------------------------------

## generative simulation 

sim_pts <- function(FGA, FTA, hFG, hFT){ 
  
  # FTA
  FT <- rbinom(1, FTA, prob = hFT)
  
  # 2PA  
  FG <- rbinom(1, FGA, prob = hFG)*2
  
  FT + FG
  
}    

N_games <- 1e3
hFG <-  .7
hFT <-  .4
FGA <- round(rgamma(N_games, 20, 1),0)
FTA <- round(rgamma(N_games, 5, 1),0)
pts <- vector("numeric", length = N_games)

for (i in seq_along(1:N_games)) { 
  
  pts[i] <-  sim_pts(FGA[i], FTA[i], hFG, hFT)
  
  }

dat <- tibble(FGA, FTA, pts)  
  

ggplot(dat, aes(x = pts)) + 
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) + 
  labs(x = "PTS", 
       y = "Frequency") + 
  dark_theme()

m1_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd), 
    mu <- a + b_1 * FGA + b_2 * FTA,
    a ~ dunif(0,10),
    b_1 ~ dunif(0, 3),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,10)
  ),
  data = dat)
precis(m1_shaq)

## mean-centering

FGA_bar <- round(mean(dat$FGA),0)
FTA_bar <- round(mean(dat$FTA),0)

m2_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd), 
    mu <- a + b_1 * (FGA-FGA_bar) + b_2 * (FTA-FTA_bar), 
    a ~ dnorm(20,8), 
    b_1 ~ dunif(0, 3),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,10) 
  ),
  data = dat)
precis(m2_shaq)

m3_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd),
    mu <- a + b_1 * (FGA-FGA_bar) * 2 + b_2 * (FTA-FTA_bar), 
    a ~ dnorm(20,8),
    b_1 ~ dunif(0, 1),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,10)
  ),
  data = dat)
precis(m3_shaq)



# load and prepare data 

shaq <- read_csv("data/shaq.csv")
dat <- list(FGA = shaq$FGA,
            FTA = shaq$FTA, 
            PTS = shaq$PTS,
            Min = shaq$Minutes)


# mediation / pipe

# without mediator

ggplot(shaq, aes(x = Minutes, y = PTS)) + 
  geom_point(size = 3, color =  "#ff02ff", alpha = .2) +
  geom_smooth(method = "lm", color = "#FDB927") +
  labs(x = "Minutes", 
       y = "Points") + 
  dark_theme()

Min_bar <- round(mean(dat$Min),0)
m5_shaq <- quap(
  alist(
    PTS ~ dnorm(mu, sd),
    mu <- a + b_1 * (Min - Min_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dunif(0, 2), 
    sd ~ dunif(0,10) 
  ),
  data = dat)
precis(m5_shaq)

# with mediator

m6_shaq <- quap(
  alist(
    PTS ~ dnorm(mu, sd), 
    mu <- a + b_1 * (Min - Min_bar) + b_2 * (FGA - FGA_bar) + b_3 * (FTA - FTA_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dnorm(0, 2), 
    b_2 ~ dunif(0, 2), 
    b_3 ~ dunif(0, 1), 
    sd ~ dunif(0,10)
  ),
  data = dat)
precis(m6_shaq)

# Fork
ggplot(diamonds, aes(y = price, x = clarity, color = clarity)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(diamonds, aes(y = price, x = cut, color = cut)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(diamonds, aes(y = price, x = color, color = color)) + 
  geom_boxplot() +
  theme_minimal()

diamonds2 <- diamonds
diamonds2$clarity <- fct_recode(factor(diamonds2$clarity),
                           "1" = "I1", 
                           "2" = "SI2", 
                          "3" = "SI1", 
                          "4" = "VS2", 
                          "5" = "VS1", 
                          "6" = "VVS2", 
                          "7" = "VVS1", 
                          "8" = "IF")
diamonds2$clarity <- as.numeric(diamonds2$clarity)
cor(diamonds2$price, diamonds2$clarity)

dat <- list(
  p = standardize(diamonds2$price), 
  cl = standardize(diamonds2$clarity),
  ca = standardize(diamonds2$carat)
)

m1 <- quap(
  alist(
    p ~ dnorm(mu, sd),
    mu <- a + b * cl, 
    a ~ dnorm(0, .1), 
    b ~ dnorm(0, .5), 
    sd ~ dexp(1) 
  ),
  data = dat)
precis(m1)

ggplot(diamonds, aes(y = price, x = carat, color = clarity)) + 
  geom_point(alpha = .5) +
  theme_minimal()

m2 <- quap(
  alist(
    p ~ dnorm(mu, sd),
    mu <- a + b * ca,
    a ~ dnorm(0, .1), 
    b ~ dnorm(0, .5), 
    sd ~ dexp(1)
  ),
  data = dat)
precis(m2)


ggplot(diamonds, aes(y = carat, x = clarity, color = clarity)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(diamonds, aes(y = carat, x = cut, color = cut)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(diamonds, aes(y = carat, x = color, color = color)) + 
  geom_boxplot() +
  theme_minimal()


m3 <- quap(
  alist(
    cl ~ dnorm(mu, sd),
    mu <- a + b * ca,
    a ~ dnorm(0, .1), 
    b ~ dnorm(0, .5), 
    sd ~ dexp(1)),
  data = dat)
precis(m3)


m4 <- quap(
  alist(
    p ~ dnorm(mu, sd),
    mu <- a + b1 * cl + b2 * ca,
    a ~ dnorm(0, .1), 
    b1 ~ dnorm(0, .5), 
    b2 ~ dnorm(0, .5), 
    sd ~ dexp(1)
  ),
  data = dat)
precis(m4)
