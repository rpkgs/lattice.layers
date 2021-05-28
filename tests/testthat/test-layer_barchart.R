# test_that("panel.annotation and panel.sign works",

library(lattice)
test_that("layer_barchart works", {
    expect_true({
        write_fig(
            levelplot(z ~ x * y, dvolcano, col.regions = topo.colors(10)) + 
                layer_barchart() + 
                theme_lattice(c(0, 4, 0, 0)),
            show = FALSE
        )
        TRUE
    })
})
