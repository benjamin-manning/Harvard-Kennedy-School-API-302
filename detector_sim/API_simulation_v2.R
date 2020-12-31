install.packages('tidyverse')
install.packages('ggpubr')
library(tidyverse)
library(ggplot2)
library(gtable)
library("ggpubr")

set.seed((1))
simulation_func <- function(n, B){
  detectors_bought <- replicate(B, {
    X <- sample(c(1,0), n, replace=TRUE, prob = c(.9, .1))
    sum(X)
  })
  first_test <- function(s){
    X <- sample(c(1,0), s, replace=TRUE, prob = c(.8, .2))
    sum(X)
  }
  detectors_final <- sapply(detectors_bought, first_test)
  calculator <- function (num1, num2) {
    final_cost = 0
    if(num2 < 5){
      final_cost = num1*10 + num2*5 + (5-num2)*120
    }
    else{
      final_cost = num1*10 + num2*5
    }
  }
  
  final_cost <- mapply(calculator, detectors_bought, detectors_final)
  five_or_more_detectors <- detectors_final >= 5
  total_data <- data.frame(detectors_bought, detectors_final, five_or_more_detectors, final_cost)
}

sd_func <- function(x){
  sd(x$final_cost)
}

x1 <- simulation_func(1, 10000)
x2 <- simulation_func(2, 10000)
x3 <- simulation_func(3, 10000)
x4 <- simulation_func(4, 10000)
x5 <- simulation_func(5, 10000)
x6 <- simulation_func(6, 10000)
x7 <- simulation_func(7, 10000)
x8 <- simulation_func(8, 10000)
x9 <- simulation_func(9, 10000)
x10 <- simulation_func(10, 10000)
x11 <- simulation_func(11, 10000)
x12 <- simulation_func(12, 10000)
x20 <- simulation_func(20, 10000)
x30 <- simulation_func(30, 10000)
x50 <- simulation_func(50, 10000)

sd1 <- sd(x1$final_cost)
sd2 <- sd(x2$final_cost)
sd3 <- sd(x3$final_cost)
sd4 <- sd(x4$final_cost)
sd5 <- sd(x5$final_cost)
sd6 <- sd(x6$final_cost)
sd7 <- sd(x7$final_cost)
sd8 <- sd(x8$final_cost)
sd9 <- sd(x9$final_cost)
sd10 <- sd(x10$final_cost)
sd11 <- sd(x11$final_cost)
sd12 <- sd(x12$final_cost)
sd20 <- sd(x20$final_cost)
sd30 <- sd(x30$final_cost)
sd50 <- sd(x50$final_cost)

bar1 <- mean(x1$final_cost)
bar2 <- mean(x2$final_cost)
bar3 <- mean(x3$final_cost)
bar4 <- mean(x4$final_cost)
bar5 <- mean(x5$final_cost)
bar6 <- mean(x6$final_cost)
bar7 <- mean(x7$final_cost)
bar8 <- mean(x8$final_cost)
bar9 <- mean(x9$final_cost)
bar10 <- mean(x10$final_cost)
bar11 <- mean(x11$final_cost)
bar12 <- mean(x12$final_cost)
bar20 <- mean(x20$final_cost)
bar30 <- mean(x30$final_cost)
bar50 <- mean(x50$final_cost)
mean_values

x_values <- c(1:12, 20, 30, 50)
sd_values <- c(sd1, sd2, sd3, sd4, sd5, sd6, sd7, sd8, sd9, sd10, sd11, sd12, sd20, sd30, sd50)
mean_values <- c(bar1, bar2, bar3, bar4, bar5, bar6, bar7, bar8, bar9, bar10, bar11, bar12, bar20, bar30, bar50)
sd_chart <- data.frame(x_values, sd_values, mean_values)
sd_chart$mean_values
ggplot(sd_chart, aes(x=x_values, y=sd_values))+
  geom_point(aes(size = mean_values), show.legend = FALSE)+ 
  geom_line()+
  geom_text(label = round(mean_values), nudge_x = -1, nudge_y = -2) +
  scale_x_continuous(name="Number of Detectors", limits=c(0, 50), breaks = c(1:12, 20, 30, 50)) +
  ylab('Standard Deviation') +
  ggtitle("Standard Deviation of Cost compared to Number of Detectors Initially Acquired", subtitle = 'Means listed and are proportional to size of point') 

g4 <- ggplot(x4, aes(final_cost)) +
  geom_density(adjust = 3, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("4 Detectors", subtitle = 'Mean: $305.7, Median: $295.0, SD: 100.5')

g5 <- ggplot(x5, aes(final_cost)) +
  geom_density(adjust = 3, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("5 Detectors", subtitle = 'Mean: $213.2, Median: $190.0, SD: 111.5')

g6 <- ggplot(x6, aes(final_cost)) +
  geom_density(adjust = 3, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("6 Detectors", subtitle = 'Mean: $174.9, Median: $180.0, SD: 104.6')
g7 <- ggplot(x7, aes(final_cost)) +
  geom_density(adjust = 13, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("7 Detectors", subtitle = 'Mean: $138.4, Median: $100.0, SD: 78.6')

g8 <- ggplot(x8, aes(final_cost)) +
  geom_density(adjust = 13, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("8 Detectors", subtitle = 'Mean: $126.8, Median: $110.0, SD: 57.7')

g9 <- ggplot(x8, aes(final_cost)) +
  geom_density(adjust = 13, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("9 Detectors", subtitle = 'Mean: $125.1, Median: $120.0, SD: 39.07')

g10 <- ggplot(x10, aes(final_cost)) +
  geom_density(adjust = 13, alpha = .1) +
  ylim(c(0, .015)) +
  xlim(c(0, 300)) +
  ggtitle("10 Detectors", subtitle = 'Mean: $130.7, Median: $130.0, SD: 26.8')


summary(x4$final_cost)
sd(x4$final_cost)
summary(x5$final_cost)
sd(x5$final_cost)
summary(x6$final_cost)
sd(x6$final_cost)
summary(x7$final_cost)
sd(x7$final_cost)
summary(x8$final_cost)
sd(x8$final_cost)
summary(x9$final_cost)
sd(x9$final_cost)
summary(x10$final_cost)
sd(x10$final_cost)

ggarrange(g5, g6, g7, g8, g9, g10, common.legend = TRUE, legend = 'bottom')

6*.8*.9
10*6+5*.9*6 + (5-4.32)*120
(5-4.32)*120

7*.9*.8
10*7+5*.9*7

8.9*.8
10*8+5*.9*7
