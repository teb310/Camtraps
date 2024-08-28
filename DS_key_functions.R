
unif <- function(x) {
  1
}

halfnorm <- function(x, s) {
  exp((-x ^ 2) / (2s ^ 2))
}

hasrate <- function(x, s, b) {
  1 - exp(-(x / s) ^ (-b))
}

ggplot() +
  xlim(c(0,15)) +
  ylim(c(0,1)) +
  ylab("Detection probability g(y)") +
  xlab("Distance y from camera (meters)") +
  geom_function(fun = ~unif(x=.x)) +
  geom_function(fun = ~halfnorm(x=.x, s=5), linetype = 2) +
  geom_function(fun = ~hasrate(x=.x, s=5, b=3), linetype = 3) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(10,0,0,0, "pt"), size = 10),
        axis.title.y = element_text(margin = margin(0,10,0,0, "pt"), size = 10))
  
