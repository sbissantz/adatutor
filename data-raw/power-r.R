##########################################
# Power of the Original Study (power.o) #
##########################################
#
# Sourced by the 1026 scripts. Altmejd et al.'s power.o is the power of a
# two-sided test of a correlation at alpha = .05, from the original's r and n
# (Fisher-z approximation, as pwr::pwr.r.test). This function reproduces it to
# machine precision on all 131 rows of altmejd723.

calculate_power_r <- function(n, r, alpha = 0.05) {
  # Too small for the Fisher-z approximation
  n[!is.na(n) & n <= 3] <- NA

  # Critical correlation under the null
  t_crit <- stats::qt(alpha / 2, df = n - 2, lower.tail = FALSE)
  r_crit <- sqrt(t_crit^2 / (t_crit^2 + n - 2))

  # Bias-corrected Fisher-z of the observed and the critical correlation
  z_r <- atanh(r) + r / (2 * (n - 1))
  z_crit <- atanh(r_crit)

  # Both tails under the alternative
  stats::pnorm((z_r - z_crit) * sqrt(n - 3)) +
    stats::pnorm((-z_r - z_crit) * sqrt(n - 3))
}
