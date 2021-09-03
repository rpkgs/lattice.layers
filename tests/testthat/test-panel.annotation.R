test_that("panel.annotation and panel.sign works", {
    library(lattice)

    panel <- function(x, y, z, subscripts, at, ...) {
        panel.levelplot2(x, y, z, subscripts, ...)
        panel.annotation(grid.circle(draw = FALSE))
        panel.annotation(grid.rect(), bbox = c(0.7, 1, 0, 1))
        # panel.annotation(~{
        #     grid_plot(d$y, d$value)
        # }, c(0.7, 1, 0, 1))
        panel.annotation(~{
            # par(family = family)
            par(mar = c(0, 0, 0, 0), mgp = c(0, -1, 0), oma = c(0, 0, 0, 0))
            # plot(y~value, d)
            plot(rep(1, 151), -60:90, tck = 0.01, xlab = "", ylab = "", yaxs = "i")
            grid()
        }, c(0.7, 1, 0, 1))
        # panel.barchart2(z, subscripts, at = at, ...)
    }

    # data <- data.table(x, y, r)
    # d <- data[, .(value = mean(r)), .(y)]
    expect_silent({
        p <- levelplot(z ~ x * y, dvolcano, col.regions = topo.colors(10),
                       panel = panel,
                       at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf)) +
            layer_signPerc() + 
            theme_lattice(c(0, 4, 0, 0))
        print(p)
    })
})
