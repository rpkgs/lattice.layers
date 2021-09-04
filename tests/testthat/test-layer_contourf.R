test_that("layer_contourf works", {

    expect_silent({
        cols = get_color("Blues")

        print(lattice(z ~ x * y, dvolcano) +
                  layer_contourf(labels = TRUE, format = "%.0f",
                                 col = "black", lwd = 0.2, lty = 2, region = FALSE))
        print(lattice(z ~ x * y, dvolcano) +
            layer_contourf(labels = TRUE, format = "%.0f", col = cols, region = FALSE))

        print(lattice(z ~ x * y, dvolcano) +
            layer_contourf(labels = TRUE, format = "%.0f", region = FALSE))

        print(lattice(z ~ x * y, dvolcano) +
            layer_contourf(labels = TRUE, format = "%.1f", region = TRUE))
    })
})

