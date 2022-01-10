
expect_silent({
    # at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf)
    library(rcolors)
    cols = rcolors$RdPu
    cols = topo.colors(10)
    p <- lattice(z ~ x * y, dvolcano, col.regions = cols) +
        layer_signPerc() +
        layer_latFreq(col.regions = NULL) +
        theme_lattice(c(0, 4, 0, 0))
    print(p)
})
