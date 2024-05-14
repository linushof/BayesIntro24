# install and load packages
pacman::p_load(tidyverse, HDInterval)

dark_theme <- function(base_size = 20, base_family = "mono") {
  theme_void() +
    theme(
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y =  element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      plot.title = element_text(color = "white"),
      plot.subtitle = element_text(color = "white"),
      plot.caption = element_text(color = "white"),
      strip.text = element_text(color = "white")
    )
}

# Globe tossing 
sim_tosses <- function(n, p){
  sample(c("L", "W"), size=n, replace=TRUE, prob=c(p, 1-p))
}

data <- sim_tosses(1000, 0)

# counting

counter <- function(data, cp){ 
  
  L <- sum(data=="L")
  W <- sum(data=="W")
  ways <- sapply( cp , function(cp) (cp*4)^L * ((1-cp)*4)^W ) 
  #post <- ways/sum(ways) # relative number
  data.frame(cp, ways) # summary
  
}

counter <- function(data, cp){ 
  
  sides <- length(cp)-1
  
  L <- sum(data=="L")
  W <- sum(data=="W")
  ways <- sapply( cp , function(cp) (cp*sides)^L * ((1-cp)*sides)^W ) 
  #post <- ways/sum(ways) # relative number
  data.frame(cp, ways) # summary
  
}


counter(c("L", "W", "L"), cp=seq(0,1,.25))
counter(sim_tosses(10, .1), cp=seq(0,1,.1))



#  Integrate prior knowledge

old_data <- c("L", "W", "L")
old_ways <- counter(old_data, cp=seq(0,1,.25))

new_data <- "W"
new_ways <- counter(new_data, cp=seq(0,1,.25))
  
data.frame(cp=old_ways$cp, old=old_ways$ways, new=new_ways$ways, total=old_ways$ways*new_ways$ways)




#prior$ways * new$ways # absolute ways
#round((prior$post * new$post)/sum(prior$post * new$post), 2) # relative 



# R 3.2.4 Bayesian updating with grid approximation


compute_post <- function(data, candidates, n){
  
  L <- sum(data=="L") # data 
  relative_ways <- dbinom(L, n, prob = candidates$cp) # aka the 'likelihood'
  posterior <- relative_ways * candidates$prior # updating 
  posterior_norm <- posterior/sum(posterior) # standardization
  data.frame(candidates, lh=round(relative_ways, 3), post=round(posterior_norm,3))
}


candidates <- tibble(cp = seq(0,1,.1) , prior = rep(1/length(cp), length(cp)))

p <- .5
n <- 9

# estimation 


set.seed(1671)
data <- sim_tosses(n,p)
estimation <- compute_post(data, candidates, n)
estimation

# Check results

estimation %>% 
  pivot_longer(cols = c(prior,post), names_to = "type", values_to = "probability") %>% # transform tata
  ggplot(aes(x=cp, y = probability, color = type, linetype = type)) + 
  geom_line(linewidth = 2) + 
  labs(x = "Candidates", 
       y = "Probability", 
       color = "Probability Type",
       linetype = "Probability Type") + 
  dark_theme() +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  theme(panel.grid.major.y =  element_line(color = "white"))
ggsave("materials/plots/session_04_bayesian_updating.png", height = 5, width = 6)



# Step-wise updating and the value of more data 



samples <- vector("numeric", n)
results <- vector("list", n)

for (i in seq_along(1:n)){
  
  estimation <- compute_post(data[i], candidates, n=1)
  results[[i]] <- expand_grid(n = i, estimation)
  
  candidates$prior <- estimation$post
  
}
results

# examine updating process
label <- tibble(n = 1:n,  data)
plot <- results %>% 
  bind_rows() %>% 
  pivot_longer(cols = c(prior, post), names_to = "type", values_to = "probability") %>% 
  ggplot(aes(x=cp, y = probability, color = type)) + 
  facet_wrap(~n) +
  geom_line(linewidth = 2, aes(linetype = type)) + 
  labs(x = "Candidates", 
       y = "Probability", 
       color = "Probability Type",
       linetype = "Probability Type") + 
  dark_theme() 
  #scale_y_continuous(breaks = seq(0, 1, .05)) +
  #theme(panel.grid.major.y =  element_line(color = "white"))

plot + geom_text(
  data    = label,
  mapping = aes(x = -Inf, y = -Inf, label = data),
  hjust   = -1,
  vjust   = -12, 
  color = 'white'
)

ggsave("materials/plots/session_04_bayesian_updating_stepwise.png", height = 7, width = 12)




candidates <- tibble(cp = seq(0,1,.01) , prior = rep(1/length(cp), length(cp)))

p <- .5
n <- 100

# estimation 


set.seed(12123)

estimation <- compute_post(sim_tosses(n,p), candidates, n)



# posterior 
p <- estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", size = 1) + 
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") +
  dark_theme()
p
ggsave("materials/plots/session_04_posterior.png", height = 5, width = 6)

# area under the curve
p + geom_area(fill = "#F8766D", alpha = 0.4)
ggsave("materials/plots/session_04_posterior_fill.png", height = 5, width = 6)


# Posterior intervals
posterior_samples <- sample(estimation$cp, prob = estimation$post, size = 1e4, replace = TRUE) 

# percentile interval (middle 50%)
PI_lower <- quantile(posterior_samples, probs = .25)
PI_upper <- quantile(posterior_samples, probs = .75)

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", size = 1) + 
  geom_ribbon(data = subset(estimation, cp >= PI_lower & cp <= PI_upper),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") + 
  dark_theme()
ggsave("materials/plots/session_04_pi50.png", height = 5, width = 6)

PI_lower <- quantile(post_samples, probs = .10)
PI_upper <- quantile(post_samples, probs = .90)

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", size = 1) + 
  geom_ribbon(data = subset(estimation, cp >= PI_lower & cp <= PI_upper),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") +
  dark_theme()
ggsave("materials/plots/session_04_pi80.png", height = 5, width = 6)

# highest posterior density interval | 50 % 
HPDI_lower <- hdi(post_samples, .5)[1]
HPDI_upper <- hdi(post_samples, .5)[2]


estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", size = 1) + 
  geom_ribbon(data = subset(estimation, cp >= HPDI_lower & cp <= HPDI_upper),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") + 
  dark_theme()
ggsave("materials/plots/session_04_hpid50.png", height = 5, width = 6)

# highest posterior density interval | 50 % 
HPDI_lower <- HPDI(post_samples, .80)[1]
HPDI_upper <- HPDI(post_samples, .80)[2]

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", size = 1) + 
  geom_ribbon(data = subset(estimation, cp >= HPDI_lower & cp <= HPDI_upper),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") + 
  dark_theme()
ggsave("materials/plots/session_04_hpid80.png", height = 5, width = 6)
