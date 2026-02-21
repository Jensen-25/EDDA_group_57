options(digits=3)

clouds = read.table("clouds.txt", header=TRUE)
seeded <- clouds$seeded

# Calculate the estimator
mean_estimator <- mean(seeded)

# Calculate the standard error 
se <- mean_estimator / sqrt(length(seeded))

# Finding the z-score 
z_score = qnorm(0.975)

# Lower CI Boundary
lower <- mean_estimator - z_score*se
lower

# Upper CI Boundary
upper <- mean_estimator + z_score*se
upper

# Bootstrap CI

B=1000
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(seeded,replace=TRUE)
  Tstar[i]=median(Xstar) 
}

Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
c(2*mean(seeded)-Tstar975,2*mean(seeded)-Tstar25)

t <- mean_estimator
pl=sum(Tstar<t)/B;pr=sum(Tstar>t)/B
p=2*min(pl,pr); p
## p-value is 0.004 < 0.05 so H0 is rejected

hist(Tstar,prob=T, main="Histogram of tstar")
lines(rep(t,2),c(0,0.03), col="red",lwd=2)
axis(1,t,expression(paste("t")))

ks.test(jitter(Tstar), "pexp", t)
# p-value small so H0 is rejected