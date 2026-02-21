# Assignment 1
options(digits=3)

# Exercise 1
clouds = read.table("clouds.txt", header=TRUE)

t1=sqrt(sqrt(clouds$seeded))
mean(t1)
mu3 <- t.test(t1, alternative = "greater", mu=3)
mu4 <- t.test(t1, alternative = "greater", mu=4)
mu3_5 <- t.test(t1, alternative = "greater", mu=3.5)

# Sign test 
sum(t1>3) 
binom.test(20,26,alt="g",p=0.5)

wilcox.test(t1,mu=3,alt="g", exact = FALSE)

results_1c <- data.frame(
  Transformation = c("P-Value"),
  `Mu = 3` = c(mu3$p.value),
  `Mu = 4` = c(mu4$p.value),
  `M = 3.5` = c(mu3_5$p.value)
)

results_1c
