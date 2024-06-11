# load packages
pacman::p_load(tidyverse, rethinking)

# load dark theme
source("code/.R/dark_theme.R")


# Exercise: Multiple Regression Model --------------------------------------------------------------------

## generative simulation 

sim_pts <- function(FGA, FTA, hFG, hFT){ 
  
  # FTA
  FT <- rbinom(1, FTA, prob = hFT)
  
  # 2PA  
  FG <- rbinom(1, FGA, prob = hFG) * 2
  
  FT + FG + rnorm(1, mean=0, sd=4)
  
}    

N_games <- 1e3
hFG <-  .7
hFT <-  .4
FGA <- round(rgamma(N_games, 20, 1),0)
FTA <- round(rgamma(N_games, 5, 1),0)
pts <- vector("numeric", length = N_games)

set.seed(1471)
for (i in seq_along(1:N_games)) { 
  
  pts[i] <-  sim_pts(FGA[i], FTA[i], hFG, hFT)
  
  }

dat <- tibble(FGA, FTA, pts)  

ggplot(dat, aes(x = pts)) + 
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) + 
  labs(x = "PTS", 
       y = "Frequency") #+ 
#  dark_theme()
#ggsave("materials/plots/session_08_simulation_shaq_points.png", height = 5, width = 6)


m1_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd), 
    mu <- a + b_1 * FGA * 2 + b_2 * FTA,
    a ~ dunif(0,10),
    b_1 ~ dunif(0, 1),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,8)
  ),
  data = dat)
precis(m1_shaq)

plot(m1_shaq)
pairs(m1_shaq, pars = c("a", "b_1", "b_2", "sd"))

## mean-centering

FGA_bar <- round(mean(dat$FGA),0)
FTA_bar <- round(mean(dat$FTA),0)

m2_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd), 
    mu <- a + b_1 * (FGA-FGA_bar) * 2 + b_2 * (FTA-FTA_bar), 
    a ~ dnorm(20,8), 
    b_1 ~ dunif(0, 3),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,8) 
  ),
  data = dat)
precis(m2_shaq)

plot(m2_shaq)
pairs(m2_shaq, pars = c("a", "b_1", "b_2", "sd"))


# load and prepare data 


# Pipe --------------------------------------------------------------------

shaq <- read_csv("data/shaq.csv")
dat <- list(FGA = shaq$FGA ,
            FTA = shaq$FTA , 
            PTS = shaq$PTS ,
            MIN = shaq$Minutes ,
            FTA_bar = round(mean(shaq$FTA),0) , 
            FGA_bar = round(mean(shaq$FGA),0) , 
            MIN_bar = round(mean(shaq$FGA),0))

# without mediator

ggplot(shaq, aes(x = Minutes, y = PTS)) + 
  geom_point(size = 2, color =  "#ff02ff", alpha = .5) +
  labs(x = "Minutes", 
       y = "Points") # + 
#  dark_theme()
#ggsave("materials/plots/session_08_data_shaq_points_min.png", height = 5, width = 6)

m3_shaq <- quap(
  alist(
    PTS ~ dnorm(mu, sd),
    mu <- a + b_1 * (MIN - MIN_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dunif(0, 2), 
    sd ~ dunif(0,8) 
  ),
  data = dat)
precis(m3_shaq)

# with mediator

m4_shaq <- quap(
  alist(
    PTS ~ dnorm(mu, sd), 
    mu <- a + b_1 * (MIN- MIN_bar) + b_2 * (FGA - FGA_bar) + b_3 * (FTA - FTA_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dnorm(0, 2), 
    b_2 ~ dunif(0, 2), 
    b_3 ~ dunif(0, 1), 
    sd ~ dunif(0,8)
  ),
  data = dat)
precis(m4_shaq)



# Fork --------------------------------------------------------------------

diamonds$clarity <- as.numeric(diamonds$clarity)
diamonds$cut <- as.numeric(diamonds$cut)
diamonds$color <- as.numeric(diamonds$color)

diamonds
ggplot(diamonds, aes(y = price, x = as.factor(clarity))) + 
  geom_violin(width=1.4, color = "#ff02ff", fill = "#ff02ff", alpha = .5) +
  geom_boxplot(width=0.05, color="white", alpha=0.2) #+
#  dark_theme()
#ggsave("materials/plots/session_08_data_diamonds_price_clarity.png", height = 5, width = 8)

ggplot(diamonds, aes(y = price, x = as.factor(cut))) + 
  geom_violin(width=1.4, color = "green", fill = "green", alpha = .5) +
  geom_boxplot(width=0.05, color="white", alpha=0.2) #+
#  dark_theme()
#ggsave("materials/plots/session_08_data_diamonds_price_cut.png", height = 5, width = 8)

ggplot(diamonds, aes(y = price, x = as.factor(color*-1))) + 
  geom_violin(width=1.4, color = "white", fill = "white", alpha = .5) +
  geom_boxplot(width=0.05, color="white", alpha=0.2) # +
#  dark_theme()
#ggsave("materials/plots/session_08_data_diamonds_price_color.png", height = 5, width = 8)

ggplot(diamonds, aes(y = price, x = carat)) + 
  geom_jitter(color = "#ff02ff", fill = "#ff02ff", alpha = .1) # +
#  dark_theme()
#ggsave("materials/plots/session_08_data_diamonds_price_carat.png", height = 5, width = 8)


dat <- list(
  p = standardize(diamonds$price), 
  cl = standardize(diamonds$clarity),
  ca = standardize(diamonds$carat)
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
