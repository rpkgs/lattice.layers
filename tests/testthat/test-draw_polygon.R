test_that("draw_polygon works", {
    set.seed(100)
    vals <- rnorm(100)
    x <- seq_along(vals)

    par(mgp = c(-2, -1, 0), mar = c(0, 0, 0, 0))
    draw_polygon(vals, x, type = "vertical", tck = 0.02, xlab = "", )
    draw_polygon(vals, x, type = "horizontal")
    expect_equal(2 * 2, 4)
})
