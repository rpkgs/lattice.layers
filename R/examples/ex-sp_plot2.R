{
    pars = list(title = list(x=-180, y=87, cex=1.5))
    stat = list(show = TRUE, name="mean", loc = c(-40, -50), digit = 2, include.sd = TRUE)

    # spplot style works
    grid2 = grid_slope
    p <- sp_plot(grid2,
                 # grid2@data[, .(mask = pvalue <= 0.05)],
                 ylim = c(-60, 95),
                 yticks = seq(0, 0.3, 0.1),
                 pars = pars,
                 stat = stat,
                 area.weighted = TRUE,
                 density = 0.1
    )
    write_fig(p, "a.pdf", 7.3, 6)
}
