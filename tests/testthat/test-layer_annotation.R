test_that("panel.annotation and panel.sign works", {
    library(grid)
    # data <- data.table(x, y, r)
    # d <- data[, .(value = mean(r)), .(y)]
    expect_silent({
        p <- lattice(z ~ x * y, dvolcano) +
            layer_annotation(grid.circle(), bbox = c(0, 1, 0, 1)) +
            layer_annotation(grid.rect(gp = gpar(fill = "red")), bbox = c(0.7, 1, 0, 1)) +
            layer_annotation(~ {
                par(mar = c(0, 0, 0, 0), mgp = c(0, -1, 0), oma = c(0, 0, 0, 0))
                # plot(y~value, d)
                plot(rep(1, 151), -60:90, tck = 0.01, xlab = "", ylab = "", yaxs = "i")
                grid()
            })
        print(p)
    })
})
