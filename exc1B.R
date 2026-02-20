options(digits = 3)

# Load data
clouds <- read.table("clouds.txt", header = TRUE)
seeded <- clouds$seeded
unseeded <- clouds$unseeded

# Transformations
seeded_sqrt  <- sqrt(seeded)
unseeded_sqrt <- sqrt(unseeded)

seeded_4rt  <- sqrt(seeded_sqrt)
unseeded_4rt <- sqrt(unseeded_sqrt)

# Visual check
par(mfrow=c(2,2))

qqnorm(seeded_sqrt, main="QQ-plot sqrt(seeded)")
qqline(seeded_sqrt, col="red")

qqnorm(seeded_4rt, main="QQ-plot fourth-root(seeded)")
qqline(seeded_4rt, col="red")

boxplot(seeded_sqrt, unseeded_sqrt,
        names=c("Seeded","Unseeded"),
        main="Sqrt rainfall")

boxplot(seeded_4rt, unseeded_4rt,
        names=c("Seeded","Unseeded"),
        main="Fourth-root rainfall")

par(mfrow=c(1,1))

# ----------------------
# Tests: sqrt data
# ----------------------

tt_sqrt  <- t.test(seeded_sqrt, unseeded_sqrt, alternative="greater")
mw_sqrt  <- wilcox.test(seeded_sqrt, unseeded_sqrt, alternative="greater", exact=FALSE)
ks_sqrt  <- ks.test(seeded_sqrt, unseeded_sqrt)

# ----------------------
# Tests: fourth-root data
# ----------------------

tt_4rt  <- t.test(seeded_4rt, unseeded_4rt, alternative="greater")
mw_4rt  <- wilcox.test(seeded_4rt, unseeded_4rt, alternative="greater", exact=FALSE)
ks_4rt  <- ks.test(seeded_4rt, unseeded_4rt)

# Short results table
results_1b <- data.frame(
  Transformation = c("Sqrt", "Fourth-root"),
  `Welch p` = c(tt_sqrt$p.value, tt_4rt$p.value),
  `Mann-Whitney p` = c(mw_sqrt$p.value, mw_4rt$p.value),
  `KS (two-sided) p` = c(ks_sqrt$p.value, ks_4rt$p.value)
)

results_1b
