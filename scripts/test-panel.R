

# load("l_shade.rda")
# load("params.shade.rda")
# grid.newpage()
# write_fig({
#     do.call(panel.poly_grid, params.shade)
# }, "a1.pdf")
library(lattice)

{
    # load_all("I:/Research/phenology/latticeGrob.R")
    load("params.rda")
    polys_sign <- params[[1]]

    # use_data(polys_sign, overwrite = TRUE)
    expect_silent({
        p <- xyplot(y~x, data.frame(x = c(-180, 180), y = c(-90, 90)),
                    panel =function(x, y, ...) {
                        # panel.xyplot(x, y, ...)
                        panel.poly_grid(polys, density = 0.1, lwd = 1)
                        # do.call(, params.shade)
                    })
        write_fig(p, "a.pdf")
        # print(p)
    })
}
