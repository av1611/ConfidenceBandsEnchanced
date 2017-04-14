confint.norm.sigma <- function (super.replication.count = 10,
                           replication.count = 10,
                           sample.size = 10,
                           mu = 0,
                           sigma = 1,
                           alpha = 0.2) {
# scalars
sigma.sq = sigma^2
target.coverage.prob = 1 - alpha
left.quantile  = qchisq (p = alpha/2, df = sample.size - 1)
right.quantile = qchisq (p = 1 - alpha/2, df = sample.size - 1)

x = replicate(n = super.replication.count, 
              expr = replicate(n = sample.size, 
                               expr = rnorm(replication.count, mu, sigma)))
x.bar = apply (x, c(1, 3), mean)
x.centered = sweep(x, c(1, 3), x.bar) # subtract bar from x
x.centered.by.mu = sweep(x, c(1, 3), mu) # subtract mu
x.centered.sq = x.centered^2 
# create a column vector with row sums of x.centered.sq
ss = apply(x.centered.sq, c(1, 3), sum) # 
# testing - should be close to s2.
sigma.sq.hat = ss / sample.size
confint.lower  = ss / right.quantile
confint.upper  = ss / left.quantile
confint.width  = confint.upper - confint.lower
is.sigma.sq.covered = (sigma.sq < confint.upper & sigma.sq > confint.lower)
sum.covered = apply(is.sigma.sq.covered, 2, sum)
coverage.ratio = sum.covered / replication.count 
coverage.ratio.error = coverage.ratio - target.coverage.prob

final.info <- cbind(as.vector(confint.lower),
                    as.vector(confint.upper),
                    as.vector(is.sigma.sq.covered))
final.info.length <- length(final.info)
colnames(final.info) <- c("Confint.Lower", "Confint.Upper", "IsSigmaCovered")

# Exporting final.info as CSV into a dedicated dir
params.info <- paste("src", super.replication.count,
                     "rc",  replication.count,
                     "ss", sample.size,
                     "a", alpha,
                     "sigma", sigma,
                     "mu", mu, sep = "_")
dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
final.info.csv.full.name = paste0("confint.norm.sigma_", params.info, ".csv")
final.info.csv.path <- file.path("./", "csv_output", final.info.csv.full.name)
write.csv(final.info, final.info.csv.path)

# Plotting confints
dir.create(file.path("./", "plots"), showWarnings = FALSE)
full.name = paste0("confint.norm.sigma_", params.info, ".jpeg")
plot.path<- file.path("./", "plots", full.name)

jpeg(plot.path)
plot (final.info[, 1], col = "dark blue", 
      type = "l",  
      ylab = "intervals", ylim = range(min(final.info[, 1]), max(final.info[, 2])))
lines (final.info[, 2], col = "blue")
abline(h = sigma, col = "dark gray", lty = 5)
cl = c("red", "green")
arrows(1:final.info.length, confint.lower[1:final.info.length],
       1:final.info.length, confint.upper[1:final.info.length],
       # length = par("din")[1]/replication.count * 0.5,
       angle = 90, code = 0, lwd = 0.1,
       col = cl[is.sigma.sq.covered[1:final.info.length] + 1])
sub.title = paste0("src = ", super.replication.count, ", ",
                  "rc = ", replication.count,  ", ",
                  "ss = ", sample.size,  ", ", 
                  "a = ", alpha,  ", ",
                  "sigma = ",  sigma,  ", ")
title(main = "Confidence intervals", sub = sub.title,  cex.main = 1, cex.sub = 0.7)
dev.off()

# Plotting coverage.ratio.error
# Exporting the plot to a file stored in a "./plots" folder
dir.create(file.path("./", "plots"), showWarnings = FALSE)
full.name <- paste0("coverage.ratio.error.norm.sigma_", params.info, ".jpeg")
plot.path<- file.path("./", "plots", full.name)
jpeg(plot.path)
plot (coverage.ratio.error, ylab = "difference",
      ylim = range(min(coverage.ratio.error), max(coverage.ratio.error)))
title(main = "Difference between supposed and actual coverage probability 
              in a confidence interval for a variance of normal distribution", 
      sub = sub.title,  cex.main = 1, cex.sub = 1)
dev.off()
}
