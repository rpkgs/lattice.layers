
expect_silent({
    # at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf)
    p <- lattice(z ~ x * y, dvolcano, col.regions = topo.colors(10)) + 
        layer_signPerc() +
        layer_latFreq() +
        theme_lattice(c(0, 4, 0, 0))
    print(p)
})
