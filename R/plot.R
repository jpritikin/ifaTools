#' Plot expected and observed table from SitemFit
#'
#' @param sout output from SitemFit
#' @param itemName name of item to plot
#' @param ...  Not used.  Forces remaining arguments to be specified by name.
#' @param showSampleSize whether to show the sample size at the top of the plot
#' @export
SitemPlot <- function(sout, itemName, ..., showSampleSize=TRUE) {
    garbageArguments <- list(...)
    if (length(garbageArguments) > 0) {
        stop("Values for the '...' argument are invalid; use named arguments")
    }

    s1 <- sout[[itemName]]
    obs <- s1$orig.observed
    ex <- s1$orig.expected
    rowTotal <- apply(obs, 1, sum)
    mask <- rowTotal > 0
    obs <- (obs / rowTotal)[mask,]
    ex <- (ex / rowTotal)[mask,]
    ss <- data.frame(sscore=as.numeric(names(rowTotal)), n=rowTotal)
    both <- rbind(cbind(type="expected", melt(ex)),
                  cbind(type="observed", melt(obs)))
    both$outcome <- factor(both$outcome, colnames(obs))
    plot <- ggplot(both, aes(x=sumScore, y=value)) + facet_wrap(~type) + ylim(0,1) +
        labs(y="probability", title=itemName)
    guide.style <- guide_legend(keywidth=.1, keyheight=.5, direction = "horizontal", title.position = "top",
                                label.position="bottom", label.hjust = 0.5, label.vjust = .5,
                                label.theme = element_text(angle = 90, size=8))
    plot <- plot + geom_line(aes(color=outcome)) + guides(color = guide.style)
    if (showSampleSize) {
        plot <- plot + geom_text(data=ss, aes(label=n, x=sscore), y = 1, size=2, angle=90)
    }
    plot
}

#' Create item response map table
#'
#' Categories are placed at the mean score of the examinees who picked
#' that category.
#'
#' @param grp an IFA group
#' @param ...  Not used.  Forces remaining arguments to be specified by name.
#' @param factor which factor to plot (defaults to 1)
#' @export
itemResponseMap <- function(grp, ..., factor=1) {
    garbageArguments <- list(...)
    if (length(garbageArguments) > 0) {
        stop("Values for the '...' argument are invalid; use named arguments")
    }

  item.mask <- grp$param[factor,] > 0
  result <- NULL
  for (ix in rev(colnames(grp$param)[item.mask])) {
    lev <- levels(grp$data[,ix])
    for (ox in 1:length(lev)) {
      mask <- grp$data[,ix]==lev[ox]
      mask <- !is.na(mask) & mask
      if (all(!mask)) next
      result <- rbind(result, data.frame(item=ix,
                                         outcome=ox, outcome.name=lev[ox],
                                         score=mean(grp$score[mask, factor], na.rm=TRUE)))
    }
  }
  result
}

#' Plot item information in the latent distribution
#'
#' For multidimensional items, you will need to supply a basis
#' vector. This vector is normalized to unit length.
#' 
#' @param grp an IFA group
#' @param ...  Not used.  Forces remaining arguments to be specified by name.
#' @param width the plot will span from -width to width
#' @param showTotal whether to plot the total item information
#' @param basis the basis vector (for multidimensional items)
#' @export
plotInformation <- function(grp, ..., width=3, showTotal=FALSE, basis=c(1)) {
    garbageArguments <- list(...)
    if (length(garbageArguments) > 0) {
        stop("Values for the '...' argument are invalid; use named arguments")
    }

    basis <- basis / sqrt(sum(basis^2))
  spec <- grp$spec
  param <- grp$param
  i.name <- colnames(grp$param)
  loc <- seq(-width, width, .1)
  grid <- basis %*% t(loc)
  df <- list(score=loc)
  total <- numeric(length(loc))
  for (ix in 1:length(spec)) {
    id <- i.name[ix]
    s <- spec[[ix]]
    df[[id]] <- rpf.info(s, param[1:rpf.numParam(s),ix], grid, basis)
    total <- total + df[[id]]
  }
  if (showTotal) df$total <- total
  df <- as.data.frame(df)
  long<- melt(df, id.vars=c('score'), variable.name="item")
  long$item <- factor(long$item)
  ggplot(long, aes(score, value, group=item)) +
    geom_line(size=1.1,aes(color=item)) + ylab("information")
}
