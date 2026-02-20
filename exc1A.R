rm(list = ls())
options(digits = 3)

## 1) Read data
clouds <- read.table("clouds.txt", header = TRUE)
seeded <- clouds$seeded
unseeded <- clouds$unseeded

cat("n_seeded =", length(seeded), " n_unseeded =", length(unseeded), "\n\n")

## 2) Quick summaries (helps you comment on skew/outliers)
cat("Summary (seeded):\n"); print(summary(seeded)); cat("\n")
cat("Summary (unseeded):\n"); print(summary(unseeded)); cat("\n")

## 3) Basic visuals (assumptions + “effect” intuition)
par(mfrow = c(2,2))

boxplot(seeded, unseeded,
        names = c("Seeded","Unseeded"),
        main = "Boxplots: rainfall by group",
        ylab = "Rainfall (feet per acre)")

hist(seeded, breaks = 10, main = "Histogram: seeded", xlab = "Rainfall")
hist(unseeded, breaks = 10, main = "Histogram: unseeded", xlab = "Rainfall")

qqnorm(seeded, main = "QQ-plot: seeded"); qqline(seeded, col = "red")
qqnorm(unseeded, main = "QQ-plot: unseeded"); qqline(unseeded, col = "red")

par(mfrow = c(1,1))

## 4) Decide paired or not (comment in report)
cat("Paired? No. Two independent samples (different clouds in two groups).\n\n")

## 5) TEST 1: Two-sample t-test (Welch is default and preferred here)
cat("=== Two-sample t-test (Welch) ===\n")
tt_two <- t.test(seeded, unseeded, alternative = "two.sided", var.equal = FALSE)
tt_greater <- t.test(seeded, unseeded, alternative = "greater", var.equal = FALSE)
cat("Two-sided p-value:", tt_two$p.value, "\n")
cat("One-sided (seeded > unseeded) p-value:", tt_greater$p.value, "\n\n")

## 6) TEST 2: Mann–Whitney (Wilcoxon rank-sum)
cat("=== Mann–Whitney / Wilcoxon rank-sum ===\n")
mw_two <- wilcox.test(seeded, unseeded, alternative = "two.sided", exact = FALSE)
mw_greater <- wilcox.test(seeded, unseeded, alternative = "greater", exact = FALSE)
cat("Two-sided p-value:", mw_two$p.value, "\n")
cat("One-sided (seeded > unseeded) p-value:", mw_greater$p.value, "\n\n")

## 7) TEST 3: Kolmogorov–Smirnov (distributional difference)
cat("=== Kolmogorov–Smirnov test ===\n")
ks_two <- ks.test(seeded, unseeded, alternative = "two.sided")
ks_greater <- ks.test(seeded, unseeded, alternative = "greater")
cat("Two-sided p-value:", ks_two$p.value, "\n")
cat("One-sided (seeded > unseeded) p-value:", ks_greater$p.value, "\n\n")

## 8) Applicability notes (printed as reminders)
cat("Notes for report:\n")
cat("- t-test targets difference in MEANS; normality/variance assumptions are questionable due to skew/outliers.\n")
cat("- Wilcoxon is more robust; targets location shift (often interpreted as median shift under assumptions).\n")
cat("- KS compares ENTIRE distributions, not just mean/median.\n")
cat("- Permutation test is applicable because assignment to seeded/unseeded was randomized.\n")
