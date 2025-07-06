install.packages("tibble")
install.packages("dplyr")
library(tibble)  
library(dplyr) 

plot_df <- tibble(
  horizon = 1:24,
  slope = sapply(regression_results$coefs, function(x) x["VRP"]),
  se    = sapply(regression_results$se,    function(x) x["VRP"])
) %>%
  mutate(
    lower = slope - 1.96 * se,
    upper = slope + 1.96 * se
  )

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))  


plot(plot_df$horizon, plot_df$slope, type = "l", xlim = range(plot_df$horizon),
     ylim = c(-0.2, 0.9),
     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "Horizon (months)", ylab = "", lwd = 2)

axis(2, at = seq(-0.2, 0.8, by = 0.2), las = 1, tck = 0.02)
axis(4, at = seq(-0.2, 0.8, by = 0.2), las = 1, tck = 0.02, labels = FALSE)
axis(1, at = seq(2, 24, by = 2), las = 1, tck = 0.02)
axis(3, at = seq(2, 24, by = 2), tck = 0.02, labels = FALSE)
box()

lines(plot_df$upper, lwd = 2, lty = 2)
lines(plot_df$lower, lwd = 2, lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 1)

legend("topright",
       legend = c("Slope", "Upper bound", "Lower bound"),
       col = c("black", "black", "black"),
       lty = c(1, 2, 3),
       lwd = 2,
       bty = "o",
       seg.len = 4,
       cex =0.8
)

mtext("Estimated slope coefficient and 95% confidence band", side = 3, line = 1, cex = 1, font = 1)

r2 <- regression_results$r2*100

plot(plot_df$horizon, r2, type = "l",
     xlim = range(plot_df$horizon), ylim = c(-0.5, 8),
     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "Horizon (months)", ylab = "", lwd = 2)

axis(2, at = seq(0, 8, by = 1), las = 1, tck = 0.02)
axis(4, at = seq(0, 8, by = 1), las = 1, tck = 0.02, labels = FALSE)
axis(1, at = seq(2, 24, by = 2), las = 1, tck = 0.02)
axis(3, at = seq(2, 24, by = 2), tck = 0.02, labels = FALSE)
box()
abline(h = 0, col = "black", lwd = 2, lty = 1)

mtext("Estimated adjusted R-square (%)", side = 3, line = 1, cex = 1, font = 1)

