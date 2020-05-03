library(lattice)

# test_that("panel.barchartFreq works", {
#     expect_silent({
        # write_fig(
{
    levelplot(z ~ x * y, volcano, col.regions = topo.colors(10),
                panel = panel.verticalFreq
                # at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf)
    ) +
        theme_lattice(c(0, 4, 0, 0))
    # show = FALSE
}
#     )
# })
