
setClass("Layout2",
         representation(top = "function",
                        bottom = "function"))

Layout2.create <- function(width,
                           height,
                           yspace,
                           xlab,
                           xlim,
                           ylabTop,
                           ylimTop,
                           ylabBot,
                           ylimBot) {
  this.xlab     <- xlab
  this.xlim     <- xlim
  this.ylabTop  <- ylabTop
  this.ylimTop  <- ylimTop
  this.ylabBot  <- ylabBot
  this.ylimBot  <- ylimBot
  this.isActive <- FALSE

  this.x1 <- 0.5 * (1.0 - width)
  this.x2 <- 1.0 - this.x1

  this.y1Bot <- yspace
  this.y2Bot <- this.y1Bot + height

  this.y2Top <- 1.0 - this.y1Bot
  this.y1Top <- this.y2Top - height

  bottom <- function(doPlot = TRUE, ...) {
    x1 <- this.x1
    x2 <- this.x2
    y1 <- this.y1Bot
    y2 <- this.y2Bot

    xlab <- this.xlab
    xlim <- this.xlim
    ylim <- this.ylimBot
    ylab <- this.ylabBot
    
    par(fig = c(x1, x2, y1, y2), las = 1, new = this.isActive)

    if (doPlot) {
      plot(c(x1, x2), c(y1, y2),
           type = "n",
           xlab = xlab,
           xlim = xlim,
           ylab = ylab,
           ylim = ylim,
           ...)
    }

    this.isActive <<- TRUE
  }

  top <- function(doPlot = TRUE, ...) {
    x1 <- this.x1
    x2 <- this.x2
    y1 <- this.y1Top
    y2 <- this.y2Top

    xlim <- this.xlim
    ylim <- this.ylimTop
    ylab <- this.ylabTop
    
    par(fig = c(x1, x2, y1, y2), las = 1, new = this.isActive)

    if (doPlot) {
      plot(c(x1, x2), c(y1, y2),
           type = "n",
           xlab = "",
           xlim = xlim,
           ylab = ylab,
           ylim = ylim,
           axes = FALSE,
           ...)

      axis(1, labels = FALSE)
      axis(2)
      box()
    }
         
    this.isActive <<- TRUE
  }

  new("Layout2", top = top, bottom = bottom)
}
