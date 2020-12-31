library(tidyverse)

pi = .3

np <- c(1-pi,0,0,0,0,0,0,0,0,0,0)
p0 <- c(pi,0,0,0,0,0,0,0,0,0,0)
p1 <- c(0,1,0,0,0,0,0,0,0,0,0)
p2 <- c(0,0,1,0,0,0,0,0,0,0,0)
p3 <- c(0,0,0,1,0,0,0,0,0,0,0)
p4 <- c(0,0,0,0,1,0,0,0,0,0,0)
p5 <- c(0,0,0,0,0,1,0,0,0,0,0)
p6 <- c(0,0,0,0,0,0,1,0,0,0,0)
p7 <- c(0,0,0,0,0,0,0,1,0,0,0)
p8 <- c(0,0,0,0,0,0,0,0,1,0,0)
Born <- c(0,0,0,0,0,0,0,0,0,1,1)

population0 <-c(100,0,0,0,0,0,0,0,0,0,0)
population1 <-c(65,35,0,0,0,0,0,0,0,0,0)
population2 <-c(27.4625,7.9625,35,0,0,0,0,0,0,0,0)
population4 <-c(65,35,0,0,0,0,0,0,0,0,0)
population5 <-c(65,35,0,0,0,0,0,0,0,0,0)
population6 <-c(65,35,0,0,0,0,0,0,0,0,0)
population7 <-c(65,35,0,0,0,0,0,0,0,0,0)
population8 <-c(65,35,0,0,0,0,0,0,0,0,0)
population9 <-c(65,35,0,0,0,0,0,0,0,0,0)
population10 <-c(65,35,0,0,0,0,0,0,0,0,0)
length(population)

pregnancy <- data.frame(np, p0, p1,p2,p3,p4,p5,p6,p7,p8,Born)


rownames(pregnancy)[rownames(pregnancy) == 1] = 'np'
rownames(pregnancy)[rownames(pregnancy) == 2] = 'p0'
rownames(pregnancy)[rownames(pregnancy) == 3] = 'p1'
rownames(pregnancy)[rownames(pregnancy) == 4] = 'p2'
rownames(pregnancy)[rownames(pregnancy) == 5] = 'p3'
rownames(pregnancy)[rownames(pregnancy) == 6] = 'p4'
rownames(pregnancy)[rownames(pregnancy) == 7] = 'p5'
rownames(pregnancy)[rownames(pregnancy) == 8] = 'p6'
rownames(pregnancy)[rownames(pregnancy) == 9] = 'p7'
rownames(pregnancy)[rownames(pregnancy) == 10] = 'p8'
rownames(pregnancy)[rownames(pregnancy) == 11] = 'Born'

pregUpdate<-(pregnancy)*population0
popupdate <- colSums(pregUpdate)
pregUpdate
for (i in 1:10){
  pregUpdate<-(pregnancy)*popupdate
  popupdate <- colSums(pregUpdate)
}
pregUpdate
popupdate

