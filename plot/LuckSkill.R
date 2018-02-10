
LuckSkill.plot <- function() {
    par(las = 1)
    
    x <- rnorm(100)
    y <- rnorm(100)

    x0 <- 3.5
    y0 <- 3.5
    xt <- 2.0
    lim <- c(-x0, x0)
    
    plot(x, y, type = "p", xlab = "", ylab = "", xlim = lim, ylim = lim, axes = FALSE)
    
    arrows(-x0, 0, x0, 0, length = 0.20)
    arrows(0, -x0, 0, x0, length = 0.20)
    
    text(x0 - 0.1, -0.4, "Skill", cex = 1.5, font = 2)
    text(-0.6, x0 - 0.2, "Luck",  cex = 1.5, font = 2)

    text( xt,  xt, "BLESSED", font = 2)
    text( xt, -xt, "FORLORN", font = 2)
    text(-xt,  xt, "INSUFFERABLE", font = 2)
    text(-xt, -xt, "DOOMED", font = 2)
}
