test_that("theme_lattice works", {
    library(lattice)

    expect_true({
        p <- levelplot(z~x+y, dvolcano)
        print(p + theme_lattice(c(-4, 0, 1, 1)))
        TRUE
    })
    # file.remove("Rplots.pdf")
    # expect_equal(2 * 2, 4)
})
