
FooledByRandomness.plot <- function(N = 250) {
    xwd <- 0.44
    yht <- 0.495
    new <- FALSE

    par(las = 1)

    plotOne <- function(fig) {
        par(fig = fig, new = new)
        new <<- TRUE

        plot(cumsum(rnorm(N)), type = "l", axes = FALSE, xlab = "", ylab = "")
        box()
    }

    xf1 <- c(0.0, xwd)
    xf2 <- 0.5 * c(1.0 - xwd, 1.0 + xwd)
    xf3 <- c(1.0 - xwd, 1.0)

    yf1 <- c(0.0, yht)
    yf2 <- 0.5 * c(1.0 - yht, 1.0 + yht)
    yf3 <- c(1.0 - yht, 1.0)

    plotOne(c(xf1, yf3))
    plotOne(c(xf2, yf3))
    plotOne(c(xf3, yf3))

    plotOne(c(xf1, yf2))
    plotOne(c(xf2, yf2))
    plotOne(c(xf3, yf2))

    plotOne(c(xf1, yf1))
    plotOne(c(xf2, yf1))
    plotOne(c(xf3, yf1))
}
