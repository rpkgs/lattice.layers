{
    library(Ipaper)
    data("grid_avhrr")
    pars = list(title = list(x=77, y=39, cex=1.5),
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
    stat = list(show = TRUE, name="mean", loc = c(82.5, 38), digit = 1, include.sd = TRUE)

    # spplot stype
    p <- sp_plot(grid_avhrr,
                     stat = stat,
                     pars = pars,
                     # yticks = seq(0, 0.3, 0.1),
                     interpolate = FALSE)
    write_fig(p, "ex-sp_plot1.pdf", 9.8, 5)

    # levelplot stype
    df = grid_avhrr@data %>% data.frame() %>%
        list(a = ., b = .) %>% melt_list("type")
    p2 <- sp_plot(grid_avhrr, df, formula = X1982 ~ lon+lat | type, aspect = 0.5, pars = pars)
    write_fig(p2 , "ex-sp_plot2.pdf", 6, 5)

    # show strip
    p3 <- sp_plot(grid_avhrr, df, formula = X1982 ~ lon+lat | type,
                  strip = TRUE,
                  par.settings2 = list(axis.line = list(col = "black")),
                  aspect = 0.5, pars = pars)
    write_fig(p3 , "ex-sp_plot3.pdf", 6, 5)
    # write_fig(p2, "ex-spplot_grid2.png", 9.8, 5)
}
