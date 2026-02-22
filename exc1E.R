## Tests:
## (i) median(seeded) < 300  via sign test (binomial)
## (ii) P(seeded < 30) <= 0.25  via exact binomial test

options(digits = 3)

# Load data (expects clouds.txt in the same folder as the project)
clouds <- read.table("clouds.txt", header = TRUE)
seeded <- clouds$seeded
n <- length(seeded)

cat("Exercise 1e — Seeded clouds (n =", n, ")\n\n")

# ------------------------------------------------------------
# (i) Test whether median precipitation for seeded clouds < 300
# Sign test: H0 median = 300 (P(X<300)=0.5), H1 median < 300 (P(X<300)>0.5)
# ------------------------------------------------------------
k_less_300 <- sum(seeded < 300)
k_equal_300 <- sum(seeded == 300)
n_eff <- n - k_equal_300  # remove ties if any

median_test <- binom.test(k_less_300, n_eff, p = 0.5, alternative = "greater")

cat("1) Sign test for median < 300:\n")
cat("   k = #(<300) =", k_less_300, "out of", n_eff, "(ties removed =", k_equal_300, ")\n")
cat(sprintf("   p-value = %.4f\n\n", median_test$p.value))

# ------------------------------------------------------------
# (ii) Test whether fraction with precipitation < 30 is at most 25%
# H0: p <= 0.25 vs H1: p > 0.25
# ------------------------------------------------------------
x_less_30 <- sum(seeded < 30)
p_hat <- x_less_30 / n

frac_test <- binom.test(x_less_30, n, p = 0.25, alternative = "greater")
ci_p <- binom.test(x_less_30, n)$conf.int

cat("2) Binomial test for p = P(X<30) <= 0.25:\n")
cat("   x = #(<30) =", x_less_30, "out of", n, "\n")
cat(sprintf("   p-hat = %.3f\n", p_hat))
cat(sprintf("   95%% CI for p: [%.3f, %.3f]\n", ci_p[1], ci_p[2]))
cat(sprintf("   p-value (H1: p > 0.25) = %.4f\n\n", frac_test$p.value))

# Optional: store results in objects (handy if you source() this from Rmd)
ex1e_results <- list(
  n = n,
  median_test = list(k = k_less_300, n_eff = n_eff, p_value = median_test$p.value),
  frac_test = list(x = x_less_30, n = n, p_hat = p_hat, ci = ci_p, p_value = frac_test$p.value)
)

ex1e_results