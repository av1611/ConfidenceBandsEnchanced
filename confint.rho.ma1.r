confint.rho.ma1 <- function(super.replication.count = 10,
                              replication.count = 10,
                              sample.size = 10,
                              theta = .5,
                              sigma = 5,
                              mu = 0,
                              alpha = 0.6)
{
  target.coverage.prob = 1 - alpha
  #derived
  gamma.0 = sigma^2 * (1 + theta^2)
  gamma.1 = sigma^2 * theta
  rho.1 = theta / (1 + theta^2)
  # one more z 
  z = replicate(n = super.replication.count, 
                expr = replicate(n = sample.size + 1, 
                                 expr = rnorm(n = replication.count, 
                                              mean = mu, sd = sigma)))
  x = array(0, dim = c(replication.count, sample.size, super.replication.count))
  # 3 nested loops to make x
  for (super.replication.index in 1 : super.replication.count)
  {
    for (replication.index in 1 : replication.count)
    {
      for (sample.index in 2 : sample.size)
      {
        # make x out of z recursively
        x [replication.index, sample.index, super.replication.index] = 
          z [replication.index, sample.index, super.replication.index]  +
          z [replication.index, sample.index - 1, super.replication.index] * theta
      }
    }
  }
  # check by ARIMA
  x.bar = apply (x, c(1, 3), mean)
  x.centered = sweep(x, c(1, 3), x.bar)
  gamma.0.hat = apply (x.centered^2, c(1, 3), mean)
  # for testing. Difference between the true and estimated variance of X
  # gamma.0.hat
  # gamma.0
  # gamma.0.error = gamma.0.hat - gamma.0
  # gamma.0.error
  # mean (gamma.0.error)

  # gamma 1
  gamma.1.hat = array(0, dim = c(replication.count, super.replication.count))
  
  for (super.replication.index in 1 : super.replication.count)
  {
    for (replication.index in 1 : replication.count)
    {
      s.scalar = 0
        
      for (sample.index in 1 : (sample.size - 1))
      {
        # make x out of z recursively
        s.scalar = s.scalar + 
          x.centered [replication.index, sample.index,     super.replication.index] * 
          x.centered [replication.index, sample.index + 1, super.replication.index]
      }
      gamma.1.hat [replication.index, super.replication.index] = s.scalar / sample.size
    }
  }

  gamma.1.error = gamma.1.hat - gamma.1
  rho.1.hat = gamma.1.hat / gamma.0.hat
  z.quantile = - qnorm(alpha/2)
  margin.or.error = z.quantile * sqrt((1 - 3 * rho.1.hat^2 + 4 * rho.1.hat^4) / sample.size)
  
  # compute margin of error
  confint.lower  = rho.1.hat - margin.or.error
  confint.upper  = rho.1.hat + margin.or.error
  confint.width  = confint.upper - confint.lower
  is.rho.1.covered = (rho.1 < confint.upper & rho.1 > confint.lower)
  sum.covered = apply(is.rho.1.covered, 2, sum)
  coverage.ratio = sum.covered / replication.count
  # should be of length = super.replication.count
  # vector minus scalar
  coverage.ratio.error = coverage.ratio - target.coverage.prob
  
  final.info <- cbind(as.vector(confint.lower),
                      as.vector(confint.upper),
                      as.vector(is.rho.1.covered))
  final.info.length <- length(final.info)
  
  colnames(final.info) <- c("Confint.Lower", "Confint.Upper", "IsSigmaCovered")
  
  # Exporting final.info as CSV into a dedicated dir
  params.info <- paste("src", super.replication.count,
                       "rc",  replication.count,
                       "ss", sample.size,
                       "theta", theta,
                       "sigma", sigma,
                       "mu", mu,
                       "a", alpha, sep = "_")
  dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
  final.info.csv.full.name = paste0("confint.rho.ma1_", params.info, ".csv")
  final.info.csv.path <- file.path("./", "csv_output", final.info.csv.full.name)
  write.csv(final.info, final.info.csv.path)
  
  # Plotting confints
  dir.create(file.path("./", "plots"), showWarnings = FALSE)
  full.name = paste0("confint.rho.ma1_", params.info, ".jpeg")
  plot.path<- file.path("./", "plots", full.name)
  
  jpeg(plot.path)
  plot (final.info[ , 1], col = "dark blue", 
        type = "l",  
        ylab = "intervals", ylim = range(min(final.info[, 1]), max(final.info[, 2])))
  lines (final.info[ , 2], col = "blue")
  abline(h = sigma, col = "dark gray", lty = 5)
  cl = c("red", "green")
  arrows(1:final.info.length, confint.lower[1:final.info.length],
         1:final.info.length, confint.upper[1:final.info.length],
         # length = par("din")[1]/replication.count * 0.5,
         angle = 90, code = 0, lwd = 0.1,
         col = cl[is.rho.1.covered[1:final.info.length] + 1])
  
  main.string = "Confidence intervals for correlation of MA(1) 
  based on Gaussian white noise, using Bartlett's formula"
  sub.title = paste0("src = ", super.replication.count, ", ",
                     "rc = ", replication.count,  ", ",
                     "ss = ", sample.size,  ", ", 
                     "theta = ", theta,  ", ",
                     "sigma = ",  sigma,  ", ",
                     "mu = ", mu, ", ",
                     "alpha = ", alpha, ", ")
  title(main = main.string, sub = sub.title, cex.main = 1, cex.sub = 0.7)
  dev.off()
  
  # Plotting coverage.ratio.error
  # Exporting the plot to a file stored in a "./plots" folder
  dir.create(file.path("./", "plots"), showWarnings = FALSE)
  full.name = paste0("coverage.ratio.error.rho.ma1_", params.info, ".jpeg")
  plot.path<- file.path("./", "plots", full.name)
  jpeg(plot.path)
  plot (coverage.ratio.error, ylab = "difference",
        ylim = range(min(coverage.ratio.error), max(coverage.ratio.error)))
  title(main = "Difference between supposed and actual coverage probability 
        in a confidence interval for a variance of normal distribution", 
        sub = sub.title,  cex.main = 1, cex.sub = 1)
  dev.off()

  # error.range = range (coverage.ratio.error)
  # range.width = error.range[2] -  error.range[1]
  # range.width
}
  
  