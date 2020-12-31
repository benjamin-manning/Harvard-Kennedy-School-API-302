library(tidyverse)
library(ggplot2)
library(gtable)
library("ggpubr")

n <- 7
B <- 10000
S1 <- replicate(B, {
  X <- sample(c(1,0), n, replace=TRUE, prob = c(.9, .1))
  sum(X)
})
table(S1)
#s apply n for every one!

test <- function(s){
  X <- sample(c(1,0), s, replace=TRUE, prob = c(.8, .2))
  sum(X)
}

S2 <- sapply(S1, test)
  
calculator <- function (num1, num2) {
  final = 0
  if(num2 < 5){
    final = num1*10 + num2*5 + (5-num2)*120
  }
  else{
    final = num1*10 + num2*5
  }
}

final <- (mapply(calculator, S1, S2))
summary(final)
total_data <- data.frame(S1, S2, final)
total_data$five_check <- total_data$S2 >= 5
g3 <- ggplot(total_data, aes(final, color = five_check)) +
  geom_density(adjust = 3) +
  ylim(c(0,.03)) +
  xlim(c(0, 500))

g <- rbind(g1, g2, g3, size = "first")
ggarrange(g1, g2, g3)

