
setClass("Layout3",
         representation(top    = "function",
                        middle = "function",
                        bottom = "function"))

Layout3.create <- function(width,
                           height,
                           yspace,
                           xlab,
                           xlim,
                           ylab.Top,
                           ylim.Top,
                           ylab.Mid,
                           ylim.Mid,
                           ylab.Bot,
                           ylim.Bot,
                           cex = 1.0,
                           cex.lab = 0.9,
                           cex.axis = 0.9,
                           xTickPow = NULL,
                           xTickLab = TRUE,
                           yTickPow.Top = NULL,
                           yTickLab.Top = TRUE,
                           yTickPow.Mid = NULL,
                           yTickLab.Mid = TRUE,
                           yTickPow.Bot = NULL,
                           yTickLab.Bot = TRUE) {
  this.xlab <- xlab
  this.xlim <- xlim
  
  this.ylab.Top <- ylab.Top
  this.ylim.Top <- ylim.Top
  this.ylab.Mid <- ylab.Mid
  this.ylim.Mid <- ylim.Mid
  this.ylab.Bot <- ylab.Bot
  this.ylim.Bot <- ylim.Bot
  
  this.cex <- cex
  this.cex.lab <- cex.lab
  this.cex.axis <- cex.axis
  
  this.xTickPow <- xTickPow
  this.xTickLab <- xTickLab

  this.yTickPow.Top <- yTickPow.Top
  this.yTickLab.Top <- yTickLab.Top
  this.yTickPow.Mid <- yTickPow.Mid
  this.yTickLab.Mid <- yTickLab.Mid
  this.yTickPow.Bot <- yTickPow.Bot
  this.yTickLab.Bot <- yTickLab.Bot
  
  this.isActive  <- FALSE

  this.x1 <- 0.5 * (1.0 - width)
  this.x2 <- 1.0 - this.x1

  this.y1.Bot <- yspace
  this.y2.Bot <- yspace + height

  this.y1.Mid <- 0.5 - 0.5 * height
  this.y2.Mid <- 0.5 + 0.5 * height

  this.y2.Top <- 1.0 - yspace
  this.y1.Top <- 1.0 - (yspace + height)

  logArg <- function(xTickPow, yTickPow) {
    result <- ""

    if (!is.null(xTickPow))
      result <- paste(result, "x", sep = "")

    if (!is.null(yTickPow))
      result <- paste(result, "y", sep = "")

    result
  }

  drawAxis <- function(side, tickPow, tickLab, ...) {
    if (is.null(tickPow))
      axis(side, labels = tickLab, ...)
    else
      JamPlot.logAxis(side, tick.power = tickPow, tick.labels = tickLab, ...)
  }

  panel <- function(x1, x2, y1, y2, xlab, xlim, ylab, ylim, xTickPow, xTickLab, yTickPow, yTickLab, ...) {
    par(fig = c(x1, x2, y1, y2), las = 1, new = this.isActive)

    plot(x    = xlim, y    = ylim,
         xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = ylim,
         type = "n",  axes = FALSE,
         log  = logArg(xTickPow, yTickPow), cex = this.cex, cex.lab = cex.lab, ...)

    drawAxis(1, xTickPow, xTickLab, cex = this.cex, cex.axis = this.cex.axis, cex.lab = this.cex.lab)
    drawAxis(2, yTickPow, yTickLab, cex = this.cex, cex.axis = this.cex.axis, cex.lab = this.cex.lab)
    box()

    this.isActive <<- TRUE
  }

  bottom <- function(...) {
    panel(x1 = this.x1,
          x2 = this.x2,
          y1 = this.y1.Bot,
          y2 = this.y2.Bot,
          xlab = this.xlab,
          xlim = this.xlim,
          ylab = this.ylab.Bot,
          ylim = this.ylim.Bot,
          xTickPow = this.xTickPow,
          xTickLab = this.xTickLab,
          yTickPow = this.yTickPow.Bot,
          yTickLab = this.yTickLab.Bot,
          ...)
  }

  middle <- function(...) {
    panel(x1 = this.x1,
          x2 = this.x2,
          y1 = this.y1.Mid,
          y2 = this.y2.Mid,
          xlab = "",
          xlim = this.xlim,
          ylab = this.ylab.Mid,
          ylim = this.ylim.Mid,
          xTickPow = this.xTickPow,
          xTickLab = FALSE,
          yTickPow = this.yTickPow.Mid,
          yTickLab = this.yTickLab.Mid,
          ...)
  }

  top <- function(...) {
    panel(x1 = this.x1,
          x2 = this.x2,
          y1 = this.y1.Top,
          y2 = this.y2.Top,
          xlab = "",
          xlim = this.xlim,
          ylab = this.ylab.Top,
          ylim = this.ylim.Top,
          xTickPow = this.xTickPow,
          xTickLab = FALSE,
          yTickPow = this.yTickPow.Top,
          yTickLab = this.yTickLab.Top,
          ...)
  }

  new("Layout3", top = top, middle = middle, bottom = bottom)
}
