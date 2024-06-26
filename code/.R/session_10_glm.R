# preparation -------------------------------------------------------------

pacman::p_load(rethinking, tidyverse)
source("code/.R/dark_theme.R")


# logistic regression ----------------------------------------------

## Interpretation

dat <- tibble(logit = seq(-6,6, length.out = 100) , 
              logistic = inv_logit(logit)) 

dat %>% 
  ggplot(aes(logit, logistic)) +
  geom_line(size = 2, color = "#ff02ff") + 
  labs(x = "Logit",
       y = "Logistic") #+ 
  #dark_theme()
#ggsave("materials/plots/session_10_logit_link.png", height = 5, width = 6)


## intercept prior

dat <-  tibble(a1 = rnorm(1e4, 0,5) ,
               a2 = rnorm(1e4, 0,1) , 
               a3 = rnorm(1e4, 0,.1) , 
               p1 = inv_logit(a1) , 
               p2 = inv_logit(a2) ,
               p3 = inv_logit(a3)) %>% 
  pivot_longer(cols = a1:p3, names_to = "scale", values_to = "value")

dat %>% 
  ggplot(aes(x=value)) +
  geom_density(size = 1, color = "#ff02ff") + 
  facet_wrap(~scale, scales = "free") + 
  labs(x = "Logit Prediction",
       y = "Density") #+ 
#  dark_theme()
#ggsave("materials/plots/session_10_logit_prior_wide.png", height = 5, width = 6)

## slope and intercept prior

#### wide

N <- 1e2

prior <-  tibble(index = 1:(2*N) , 
                 prior = c(rep("wide",N), rep("narrow",N)) , 
                 a = c(rnorm(N, 0, 5), rnorm(N, 0, .5)) , 
                 b = c(rnorm(N, 0, 5), rnorm(N, 0, .5)))


dat <- expand_grid(prior, x = seq(-3,3, len=100)) %>% 
  mutate(p = inv_logit(a+b*x))

dat %>% 
  ggplot(aes(x, p, group = index)) +
  geom_line(color = "#ff02ff") + 
  facet_wrap(~factor(prior, levels = c("wide", "narrow"))) +
  labs(x = "Logit",
       y = "Logistic") #+ 
#  dark_theme()
#ggsave("materials/plots/session_10_logistic_prior_ab_wide.png", height = 5, width = 6)


# Berkeley admission rates ------------------------------------------------

# simulation without direct discrimination

N <- 1000 # number of applicants 
G <- sample(1:2, size = N, replace = TRUE)
D <- rbern(N, ifelse(G==1, .3, .8)) + 1 
accept_rate <- matrix( c(.1, .3, .1, .3), nrow = 2) 
A <- rbern(N, accept_rate[D, G])

table(G,D)
table(G,A)

## total effect of G

dat <- list(A=A, D=D, G=G)

m1 <- ulam(
  alist(
    A ~ dbern(p) , 
    logit(p) <- a[G]  , 
    a[G] ~ dnorm(0,1) 
  ) , data=dat, chains = 4, cores = 4
)

traceplot(m1)
precis(m1, depth = 2) # coefficients on log odds scale
inv_logit(coef(m1)) # interpret as probability 


## direct effect of G

m2 <- ulam(
  alist(
    A ~ dbern(p) , 
    logit(p) <- a[G, D] , 
    matrix[G, D]:a ~ dnorm(0,1) 
  ) , data = dat, chains = 4, cores = 4
)

precis(m2, depth = 3)  
inv_logit(coef(m2))
traceplot(m2)


# simulation with direct discrimination


N <- 1000 # number of applicants 
accept_rate <- matrix( c(.1, .3, .2, .5), nrow = 2) 
A <- rbern(N, accept_rate[D, G])
dat <- list(A=A, D=D, G=G)

table(G,D)
table(G,A)


## total effect of G

dat <- list(A=A, D=D, G=G)


m3 <- ulam(
  alist(
    A ~ dbern(p) , 
    logit(p) <- a[G]  , 
    a[G] ~ dnorm(0,1) 
  ) , data=dat, chains = 4, cores = 4
)

precis(m3, depth = 2) # coefficients on log odds scale
inv_logit(coef(m3)) # interpret as probability 
traceplot(m3)


## direct effect of G

m4 <- ulam(
  alist(
    A ~ dbern(p) , 
    logit(p) <- a[G, D] , 
    matrix[G, D]:a ~ dnorm(0,1) 
  ) , data = dat, chains = 4, cores = 4
)

precis(m4, depth = 3)  
inv_logit(coef(m4))
traceplot(m4)


# analyze real data

# load data

library(rethinking)
data("UCBadmit", "UCBadmit_long")
dat_wide <- UCBadmit
dat_long <- UCBadmit_long


dat <- list(
  A = dat_wide$admit , 
  N = dat_wide$applications , 
  G = ifelse(dat_wide$applicant.gender=="female", 1, 2) , 
  D = as.integer(dat_wide$dept)
  )

### total effect
m5 <- ulam(
  alist(
    A ~ dbinom(N, p) ,
    logit(p) <- a[G] ,
    a[G] ~ dnorm(0,1) 
  ) , data = dat, chains = 4, cores = 4
)

precis(m5, depth = 2)
inv_logit(coef(m5))
traceplot(m5)

#### posterior distribution of differences
post1 <- extract.samples(m5)

posterior <-  tibble(p1 = inv_logit(post1$a[,1]) , 
                     p2 = inv_logit(post1$a[,2]) , 
                     diff = p1 - p2)


ggplot(posterior, aes(diff)) + 
  geom_density(color = "#ff02ff", linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density") +
  dark_theme() 
ggsave("materials/plots/session_10_Berkeley_total.png", height = 5, width = 6)
  
  

### direct effect 

m6 <- ulam(
  alist(
    A ~ dbinom(N, p) ,
    logit(p) <- a[G, D] ,
    matrix[G, D]:a ~ dnorm(0,1) 
  ) , data = dat, chains = 4, cores = 4
)

precis(m6, depth = 3)
inv_logit(coef(m6))
traceplot(m6)

#### posterior distribution of differences
post2 <- extract.samples(m6)

PrA <- inv_logit(post2$a)

diff_prob_D <- sapply(1:6, function(i) PrA[, 1, i] - PrA[, 2, i])
diffs <-  tibble(D1 = PrA[, 1, 1] - PrA[, 2, 1] , 
                 D2 = PrA[, 1, 2] - PrA[, 2, 2] , 
                 D3 = PrA[, 1, 3] - PrA[, 2, 3] , 
                 D4 = PrA[, 1, 4] - PrA[, 2, 4] , 
                 D5 = PrA[, 1, 5] - PrA[, 2, 5] ,
                 D6 = PrA[, 1, 6] - PrA[, 2, 6]) %>% 
  pivot_longer(cols = D1:D6, names_to = "Department", values_to = "Diff")
  

diffs %>% 
  ggplot(aes(Diff, color = Department)) + 
  geom_density(alpha = .3, linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density") +
  scale_color_viridis_d(option = "C") +
  dark_theme()
ggsave("materials/plots/session_10_Berkeley_direct.png", height = 5, width = 6)




