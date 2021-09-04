test_that("layer_contourf works", {

    cols = get_color("Blues")

    expect_silent({
        print(lattice(z ~ x * y, dvolcano) +
            layer_contourf(labels = TRUE, format = "%.0f", col = cols, region = FALSE))

        print(lattice(z ~ x * y, dvolcano) +
            layer_contourf(labels = TRUE, format = "%.0f", region = FALSE))

        print(lattice(z ~ x * y, dvolcano) +
            layer_contourf(labels = TRUE, format = "%.1f", region = TRUE))
    })
})

