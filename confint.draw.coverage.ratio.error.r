confint.draw.coverage.ratio.error <- function (final.info) {
  final.info.length <- length(final.info)
  dir.create(file.path("./", "plots"), showWarnings = FALSE)
  full.name <- paste0("coverage.ratio.error.norm.mu_", params.info, ".jpeg")
  plot.path<- file.path("./", "plots", full.name)
  jpeg(plot.path)
  plot (coverage.ratio.error, ylab = "difference",
        ylim = range(min(coverage.ratio.error), max(coverage.ratio.error)))
  title(main = "Difference between supposed and actual coverage probability 
        in a confidence interval for a mean of normal distribution", 
        sub = sub.title,  cex.main = 1, cex.sub = 1)
  dev.off()
}
