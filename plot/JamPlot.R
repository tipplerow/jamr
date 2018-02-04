
JamPlot.col <- function(x) {
  ifelse(x < 7, x, x + 1) ## Avoid hard-to-see yellow...
}

JamPlot.pch <- function(x) {
  x - 1 ## Start with squares...
}

JamPlot.by <- function(dframe, xkey, ykey, bykey, byval = NULL, overlay = FALSE, type = "b",
                       xlab = NULL, ylab = NULL, col = NULL, pch = NULL, cex = NULL, 
                       lty = 1, lwd = 1, legend.loc = "topleft", legend.text = NULL, ...) {
  if (is.null(byval))
    byval <- sort(unique(dframe[,bykey]))

  if (is.null(xlab))
    xlab <- xkey

  if (is.null(ylab))
    ylab <- ykey

  if (is.null(col))
    col <- 1:length(byval)

  if (is.null(pch))
    pch <- 0:(length(byval) - 1)

  if (is.null(cex))
    cex <- rep(1, length(pch)) + ifelse(pch == 18, 0.35, 0.0)

  if (is.null(legend.text))
    legend.text <- byval

  dframe <- dframe[order(dframe[,bykey], dframe[,xkey]),]

  if (!overlay) {
    par(las = 1)
    plot(dframe[,xkey], dframe[,ykey], type = "n", xlab = xlab, ylab = ylab, ...)
  }

  for (k in seq_along(byval)) {
    keep <- which(dframe[,bykey] == byval[k])
    x <- dframe[keep, xkey]
    y <- dframe[keep, ykey]

    points(x, y, type = type, col = col[k], pch = pch[k], lty = lty, lwd = lwd, cex = cex[k])
  }

  if (!is.null(legend.loc))
    legend(legend.loc, bty = "n", legend = legend.text, col = col, pch = pch)
}

JamPlot.err <- function(x, y, dy, type = "b", pch = 1, col = 1, ...) {
  stopifnot(length(y) == length(dy))
  points(x, y, type = type, pch = pch, col = col, ...)

  for (k in seq_along(y))
    lines(c(x[k], x[k]), c(y[k] - 2.0 * dy[k], y[k] + 2.0 * dy[k]), col = col)
}

JamPlot.logAxis <- function(side, tick.power, tick.labels, ...) {
  .jamPlot.logAxis(side, tick.power, tick.labels, outer = FALSE, ...)
}

JamPlot.logX <- function(dframe, xkey, ykey, tick.power = NULL, tick.labels = TRUE, outer = FALSE, ...) {
  x <- dframe[,xkey]
  y <- dframe[,ykey]
              
  if (is.null(tick.power))
    tick.power <- .jamPlot.imputeTickPowers(x)

  plot(x, y, log = "x", type = "n", axes = FALSE, xlim = .jamPlot.logAxisLimit(tick.power), ...)

  .jamPlot.logAxis(1, tick.power, tick.labels, outer, ...)
  axis(2)
  box()
}

JamPlot.logY <- function(xlim, ylim, xlab, ylab, tick.power = NULL, tick.labels = TRUE, outer = FALSE, x.axis = TRUE, ...) {
  if (is.null(tick.power))
    tick.power <- .jamPlot.imputeTickPowers(ylim)

  if (!x.axis)
    xlab <- ""

  plot(xlim, ylim, log = "y", type = "n", axes = FALSE, ylim = .jamPlot.logAxisLimit(tick.power), xlab = xlab, ylab = ylab, ...)

  if (x.axis)
    axis(1)
  
  .jamPlot.logAxis(2, tick.power, tick.labels, outer)
  box()
}

.jamPlot.logAxis <- function(side, tick.power, tick.labels, outer, ...) {
  if (isTRUE(tick.labels))
    labels <- .jamPlot.tickLabels(tick.power)
  else 
    labels <- tick.labels
  
  axis(side, at = .jamPlot.tickValues(tick.power), labels = labels, outer = outer, ...)
}

.jamPlot.logAxisLimit <- function(tick.power) {
  c(10 ^ min(tick.power), 10 ^ max(tick.power))
}

.jamPlot.imputeTickPowers <- function(x) {
  minpow <- floor(log10(min(x)))
  maxpow <- ceiling(log10(max(x)))

  minpow:maxpow
}

.jamPlot.tickLabels <- function(tick.power) {
  eval(parse(text = sprintf("expression(%s)", paste(sprintf("10 ^ %d", tick.power), collapse = ", "))))
}

.jamPlot.tickValues <- function(tick.power) {       
  10 ^ tick.power
}

JamPlot.scatter <- function(dframe, xkey, ykey, bykey, byval = NULL, col = NULL, pch = NULL,
                            legend.loc = "topleft", legend.text = NULL, ...) {
  if (is.null(byval))
    byval <- sort(unique(dframe[,bykey]))

  if (is.null(col))
    col <- 1:length(byval)

  if (is.null(pch))
    pch <- 0:(length(byval) - 1)

  if (is.null(legend.text))
    legend.text <- byval

  par(las = 1)
  plot(dframe[,xkey], dframe[,ykey], type = "n", ...)

  for (k in seq_along(byval)) {
    keep <- which(dframe[,bykey] == byval[k])
    x <- dframe[keep, xkey]
    y <- dframe[keep, ykey]

    points(x, y, col = col[k], pch = pch[k])
  }

  if (!is.null(legend.loc))
    legend(legend.loc, bty = "n", legend = legend.text, col = col, pch = pch)
}

JamPlot.arrows <- function(x, y, col, pch, space, length = 0.08, angle = 30) {
  stopifnot(length(x) == length(y))
  points(x, y, type = "p", col = col, pch = pch)

  for (k in 2:length(x)) {
    x0 <- x[k - 1]
    y0 <- y[k - 1]

    x1 <- x[k]
    y1 <- y[k]

    dx <- x1 - x0
    dy <- y1 - y0

    gamma <- space / abs(dx)

    if (gamma < 0.5) {
      x0 <- x0 + gamma * dx
      x1 <- x1 - gamma * dx

      y0 <- y0 + gamma * dy
      y1 <- y1 - gamma * dy

      arrows(x0, y0, x1, y1, length = length, angle = angle, col = col)
    }
  }
}
