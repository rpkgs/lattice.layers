
es <- function(T) {
    0.6108 * exp(17.27*T/(T+237.3))
}

draw_line <- function(x, y, col = "blue", lty = 2) {
    min = -10
    # lines(rep(x[1], 2), c(0, y[1]), col = "blue", lty = 2)
    # lines(c(0, x[1]), c(y[1], y[1]), col = "blue", lty = 2)
    lines(c(x, x)  , c(min, y), col = col, lty = lty)
    lines(c(min, x), c(y, y), col = col, lty = lty)
    text(x - 0.2, y, sprintf("(%d℃, %.1f kPa)", x, y), col = col, adj = c(1, -0.3))
}

write_fig({
    T <- seq(0, 35, 0.01)

    par(mar = c(3, 3, 2, 1), mgp = c(1.7, 0.6, 0), cex.lab = 1.2)
    plot(T, es(T), type = "l", lwd = 1.5,
         xlab = "Temperature (T, ℃)",
         ylab = "Saturated vapor pressure (es(T), kPa)")
    grid()
    x = c(20, 30)
    y = es(x)
    draw_line(x[1], y[1], "blue")
    draw_line(x[2], y[2], "red")

    abline(lm(y~x), col = "yellow3")

    points(25, mean(y), pch = 20)
    points(25, es(25), pch = 20, col = "red")
    # lines(rep(x[2], 2), c(0, y[2]), col = "red", lty = 2)
    # abline(v = , )
}, "es.pdf", 6, 6)
