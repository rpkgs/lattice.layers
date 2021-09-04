
test_that("layer_barchart works", {
    expect_silent({
        p <- levelplot(z ~ x * y, dvolcano, col.regions = topo.colors(10)) + 
            layer_barchart() + 
            theme_lattice(c(0, 4, 0, 0))
        print(p)
    })
})
