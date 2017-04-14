confint.draw.bounds <- function (final.info) {
  final.info.length <- length(final.info)
  dir.create(file.path("./", "plots"), showWarnings = FALSE)
  full.name = paste0("confint.norm.mu_", params.info, ".jpeg")
  plot.path <- file.path("./", "plots", full.name)
  jpeg(plot.path)
  plot (final.info[, 1], col = "dark blue", 
        type = "l",  
        ylab = "intervals", ylim = range(min(final.info[, 1]), max(final.info[, 2])))
  lines (final.info[, 2], col = "blue")
  abline(h = mu, col = "dark gray", lty = 5)
  cl = c("red", "green")
  arrows(1:final.info.length, confint.lower[1:final.info.length],
         1:final.info.length, confint.upper[1:final.info.length],
         # length = par("din")[1]/replication.count * 0.5,
         angle = 90, code = 0, lwd = 0.1,
         col = cl[is.mu.covered[1:final.info.length] + 1])
  sub.title = paste0("src = ", super.replication.count, ", ",
                     "rc = ", replication.count,  ", ",
                     "ss = ", sample.size,  ", ", 
                     "a = ", alpha,  ", ",
                     "sigma = ",  sigma,  ", ",
                     "mu = ",  mu)
  main.string = "Confidence intervals for a mean of i.i.d. normal population"
  title(main = main.string, sub = sub.title,  cex.main = 1, cex.sub = 0.7)
  dev.off()
}
