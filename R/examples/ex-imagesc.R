load("R/examples/Tem_anorm.rda")

data = abind::abind(arr, arr, arr, arr, along = 3)

f = path.mnt("i:/GitHub/shapefiles/China/origin/bou1_4l.shp")
china = sf::read_sf(f) %>% as_Spatial()
shp = list("sp.lines", china)

{
    at_x = seq(70, 140, 20)
    at_y = seq(10, 55, 10)
    scales = list(
        x = list(at = at_x, labels = at_x),
        y = list(at = at_y, labels = at_y))

    p <- imagesc(data,
        lon, lat, sp = T,
        sp.layout = shp,
        scales = scales
    ) + theme_lattice(
        # plot.margin = c(1, 2, 1, -2),
        axis.margin = c(1, 0.5, 1, 0),
        key.margin = c(0, 2.3, 0, -0.2)
    )
    # p = p
    write_fig(p, "a.pdf", 11.2, 3.3*2)
    # write_fig(p, "a.pdf", 10, 3.1)
}

