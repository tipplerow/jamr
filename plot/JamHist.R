
JamHist.plot2 <- function(x1, x2, h,
                          xlab, xlim, ymax,
                          legend.loc  = "topright",
                          legend.text = NULL) {
    k1 <- which(xlim[1] < x1 & x1 < xlim[2])
    k2 <- which(xlim[1] < x2 & x2 < xlim[2])

    col1 <- rgb(0.0, 0.0, 1.0, 0.25)
    col2 <- rgb(1.0, 0.0, 0.0, 0.25)

    breaks <- seq(xlim[1], xlim[2], h)

    hist(x1[k1],
         breaks = breaks,
         freq   = FALSE,
         main   = NULL,
         xlab   = xlab,
         xlim   = xlim,
         ylim   = c(0.0, ymax),
         col    = col1)

    hist(x2[k2],
         breaks = breaks,
         freq   = FALSE,
         xlab   = "",
         xlim   = xlim,
         ylim   = c(0.0, ymax),
         col    = col2,
         add    = TRUE)

    if (!is.null(legend.text)) {
        legend(legend.loc, bty = "n",
               legend = legend.text,
               col = c(col1, col2),
               pch = c(15, 15))

        legend(legend.loc, bty = "n",
               legend = legend.text,
               col = c(1, 1),
               pch = c(0, 0))
    }
}

JamHist.plot3 <- function(x1, x2, x3, h,
                          xlab, xlim, ymax,
                          legend.loc  = "topright",
                          legend.text = NULL) {
    k1 <- which(xlim[1] < x1 & x1 < xlim[2])
    k2 <- which(xlim[1] < x2 & x2 < xlim[2])
    k3 <- which(xlim[1] < x3 & x3 < xlim[2])

    col1 <- rgb(0.0, 0.0, 1.0, 0.25)
    col2 <- rgb(1.0, 0.0, 0.0, 0.25)
    col3 <- rgb(0.0, 1.0, 0.0, 0.25)

    breaks <- seq(xlim[1], xlim[2], h)

    hist(x1[k1],
         breaks = breaks,
         freq   = FALSE,
         main   = NULL,
         xlab   = xlab,
         xlim   = xlim,
         ylim   = c(0.0, ymax),
         col    = col1)

    hist(x2[k2],
         breaks = breaks,
         freq   = FALSE,
         xlab   = "",
         xlim   = xlim,
         ylim   = c(0.0, ymax),
         col    = col2,
         add    = TRUE)

    hist(x3[k3],
         breaks = breaks,
         freq   = FALSE,
         xlab   = "",
         xlim   = xlim,
         ylim   = c(0.0, ymax),
         col    = col3,
         add    = TRUE)

    if (!is.null(legend.text)) {
        legend(legend.loc, bty = "n",
               legend = legend.text,
               col = c(col1, col2, col3),
               pch = c(15, 15, 15))

        legend(legend.loc, bty = "n",
               legend = legend.text,
               col = c(1, 1, 1),
               pch = c(0, 0, 0))
    }
}
