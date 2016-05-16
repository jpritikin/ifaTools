plotTwoFactors <- function(slope) {
  lvm <- varimax(toFactorLoading(slope))$loadings   ## \label{e4:varimax}
  if (any(abs(lvm[lvm < 0]) > .001)) stop("Got negative loadings")
  lvm[lvm<0] <- 0
  df <- as.data.frame(lvm[, 1:2])
  df$name <- rownames(df)
  pl <- ggplot(df, aes_string(x = rownames(slope)[1],
    y = rownames(slope)[2], label = "name")) + geom_text(size = 3)
  pm <- promax(lvm[, 1:2])$rotmat   ## \label{e4:promax}
  for (dx in 1:ncol(pm)) {
    d1 <- .5 * pm[, dx] / sqrt(sum(pm[, dx]^2))
    pl <- pl + geom_segment(x = .5, y = .5, xend = d1[1] + .5,
      yend = d1[2] + .5, arrow = arrow(length = unit(.5, "cm")))
  }
  pl + xlim(0, 1) + ylim(0, 1)
}
