confint.bernoulli <- function(super.replication.count = 10,
                              # greater than 1
                              replication.count = 10,
                              # greater than 1,
                              sample.size = 10,
                              p = .5,
                              alpha = 0.05,
                              # scalars
                              target.coverage.prob = 1 - alpha) {
  x = replicate(n = super.replication.count, 
                expr = replicate(n = sample.size, 
                                 expr = rbinom(n = replication.count, 
                                               size = 1, prob = p)))
  
  x.bar = apply (x, c(1, 3), mean)
  dim(x.bar) #  - rep.count by sup.rep.count
  dim (x.bar) ==  c (replication.count, super.replication.count)
  margin.or.error = 1 / (2 * sqrt (sample.size * alpha))
  # smaller, but still guarantees probability
  margin.or.error = sqrt (- log(alpha/2)/ (2*sample.size))
  # compute margin of error
  confint.lower  = x.bar - margin.or.error
  confint.upper  = x.bar + margin.or.error
  confint.width  = confint.upper - confint.lower
  is.p.covered = (p < confint.upper & p > confint.lower)
  sum.covered = apply(is.p.covered, 2, sum)
  coverage.ratio = sum.covered / replication.count 
  # should be of length = super.replication.count
  # vector minus scalar
  coverage.ratio.error = coverage.ratio - target.coverage.prob
  coverage.ratio.error
  
  final.info <- cbind(as.vector(confint.lower),
                      as.vector(confint.upper),
                      as.vector(is.p.covered),
                      rep(p, super.replication.count * replication.count))
  final.info.length <- length(final.info)
  colnames(final.info) <- c("Confint.Lower", "Confint.Upper", "IsPCovered", "P")
  
  # Exporting final.info as CSV into a dedicated dir
  params.info <- paste("src", super.replication.count,
                       "rc",  replication.count,
                       "ss", sample.size,
                       "p", p,
                       "a", alpha, sep = "_")
  dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
  final.info.csv.full.name <- paste0("confint.bernoulli_", params.info, ".csv")
  final.info.csv.path <- file.path("./", "csv_output", final.info.csv.full.name)
  write.csv(final.info, final.info.csv.path)
  
  # Plotting confints
  dir.create(file.path("./", "plots"), showWarnings = FALSE)
  full.name = paste0("confint.bernoulli_", params.info, ".jpeg")
  plot.path<- file.path("./", "plots", full.name)
  jpeg(plot.path)
  plot (final.info[, 1], col = "dark blue", type = "l", 
        ylab = "intervals", ylim = range(min(final.info[, 1]), max(final.info[, 2])))
  lines (final.info[, 2], col = "dark red")
  
  abline(h = p, col = "dark gray", lty = 5)
  cl = c("red", "green")
  arrows(1:final.info.length, confint.lower[1:final.info.length],
         1:final.info.length, confint.upper[1:final.info.length],
         # length = par("din")[1]/replication.count * 0.5,
         angle = 90, code = 0, lwd = 0.1,
         col = cl[is.p.covered[1:final.info.length] + 1])
  sub.title = paste0("src = ", super.replication.count, ", ",
                    "rc = ", replication.count,  ", ",
                    "ss = ", sample.size,  ", ", 
                    "a = ", alpha,  ", ",
                    "p = ",  p,  ", ")
  title(main = "Confidence intervals", sub = sub.title, cex.main = 1, cex.sub = 0.7)
  mtext(sub.title, side = 1, cex = 0.75)
  dev.off()
  
  # Exporting the plot to a file stored in a "./plots" folder
  dir.create(file.path("./", "plots"), showWarnings = FALSE)
  full.name = paste0("coverage.ratio.error.bernoulli_", params.info, ".jpeg")
  plot.path<- file.path("./", "plots", full.name)
  jpeg(plot.path)
  plot(coverage.ratio.error, ylab = "difference", 
        ylim = range(min(coverage.ratio.error), max(coverage.ratio.error)))
  title(main = "Difference between supposed and actual coverage probability 
        in a confidence interval for a mean of Bernoulli distribution", 
        sub = sub.title,  cex.main = 1, cex.sub = 1)
  # error.range = range (coverage.ratio.error)
  # range.width = error.range[2] -  error.range[1]
  # range.width
  dev.off()
}