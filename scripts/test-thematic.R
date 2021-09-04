# library(gridExtra)
# g = do.call(arrangeGrob, ps)

library(thematic)
{
    library(grid)
    library(gridExtra)
    library(lattice)
    thematic_on(font = "Times")

    x <- seq(pi / 4, 5 * pi, length.out = 100)
    y <- seq(pi / 4, 5 * pi, length.out = 100)
    r <- as.vector(sqrt(outer(x^2, y^2, "+")))
    grid <- expand.grid(x = x, y = y)
    grid$z <- cos(r^2) * exp(-r / (pi^3))
    p <- levelplot(z ~ x * y, grid,
        cuts = 50, scales = list(log = "e"), xlab = "",
        ylab = "", main = "Weird Function", sub = "with log scales",
        colorkey = FALSE, region = TRUE
    )

    # write_fig(plot_grid(p, p), "b.pdf")
    g <- gridExtra::arrangeGrob(p, p, nrow = 1)
    write_fig(g, "b.pdf")
    # grid.newpage()
    # grid.draw(g)
}
