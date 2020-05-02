# test_that("panel.annotation and panel.sign works",
panel <- function(x, y, z, subscripts, ...) {
    dots <- list(...)

    panel.levelplot(x, y, z, subscripts, ...)
    bbox = c(0, 0.5, 0, 0.5)
    panel.annotation(grid.rect(), bbox)
    bbox[3] <- 0.07
    panel.barchartFreq(z, subscripts, bbox, ..., yscale = c(0, 20))
}

library(lattice)

test_that("panel.barchartFreq works", {
    expect_silent({
        write_fig(
            levelplot(z ~ x * y, volcano, col.regions = topo.colors(10),
                      panel = panel
                      # at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf)
            ) +
                theme_lattice(c(0, 4, 0, 0)), 
            show = FALSE
        )
    })
})
