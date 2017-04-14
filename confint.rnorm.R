confint.rnorm.mu <- function (super.replication.count = 100,
                           replication.count = 100,
                           sample.size = 100,
                           mu = 0,
                           sigma = 7,
                           alpha = 0.05) {
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

pic.name.info <- paste("confintrnorm", 
                       "src", super.replication.count,
                       "rc",  replication.count,
                       "ssize", sample.size,
                       "a", alpha,
                       sep = "_")
# Exporting the plot to a file stored in a "./plots" folder
dir.create(file.path("./", "plots"), showWarnings = FALSE)
full.name = paste0(pic.name.info, ".jpeg")
plot.path<- file.path("./", "plots", full.name)
jpeg(plot.path)
plot (coverage.ratio.error, main = "Difference between supposed and actual
      coverage probability in a confidence interval 
      for a variance of normal distribution",
      ylab = "difference", 
      ylim = range(min(coverage.ratio.error), max(coverage.ratio.error)))
dev.off()
# error.range = range (coverage.ratio.error)
# range.width = error.range[2] -  error.range[1]
# range.width
}

# 

# We can see that these errors sum up to about zero. 
# For small replication count there is asymmetry.
# There are more small positive errors and there are less negative bigger errors. 
# If we increase replication count, we will have more values of errors. 
# We can guess that the range of errors is defined by alpha. 








