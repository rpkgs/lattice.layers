library(lattice)

# test_that("panel.barchartFreq works", {
expect_silent({
    levelplot(z ~ x * y, dvolcano, col.regions = topo.colors(10)
                # panel = panel.latFreq
                # at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf)
    ) + layer_signPerc() + 
        layer_latFreq() + 
        theme_lattice(c(0, 4, 0, 0))
})
