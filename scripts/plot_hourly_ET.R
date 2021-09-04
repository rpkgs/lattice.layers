library(rcolors)
library(cowplot)

# load("Flux_DangXiong.rda")
# Flux_DangXiong = d[date <= "2004-12-31" & month(date) %in% 6:8,
#         .(date, hour = hour2, doy, ET, RH, Tair, Rn, Rs, HS, ea = `2m_ea`)] %>%
#     mutate(hour = as.factor(hour))
df = melt(Flux_DangXiong, c("hour", "date", "doy"))
varnames = df$variable %>% unique() %>% as.character()

l_brks = list(
    ET = c(-Inf, 0, 1, 2, 5, 10, 15, Inf) ,
    RH = c(seq(50, 90, 5)) %>% c(-Inf, ., Inf),
    Rn = c(-Inf, seq(0, 800, 100), Inf),
    Rs = c(-Inf, seq(0, 1000, 100), Inf),
    Tair = c(-Inf, seq(0, 20,  2), Inf),
    HS = c(seq(0, 200, 50), Inf) %>% c(-rev(.), 0, .) %>% unique()
)


unit = expression("W/"~m^2)
varnames = c("ET", "RH", "Tair", "Rn", "Rs", "HS", "ea")
units = c("mm/d", "%", "℃", unit, "W/m2", "W/m2", "kPa")
titles = sprintf("%s (%s)", varnames, units) %>% set_names(varnames)

# how to paste expression?

# titles[4] = expression("Rn (W/"~m^2~")")
# titles[5] = expression("Rs (W/"~m^2~")")
# titles[6] = expression("HS (W/"~m^2~")")

# par(fontfamily = "Times")
{
    # trellis.par.set(grid.pars = list(fontfamily = "Times"), fontsize = list(text = 20))
    # splom(iris[1:4], grousps = iris[[5]], grid = TRUE, auto.key = list(space = "right"))
    ps = foreach(varname = varnames, i = icount(5)) %do% {
        dati = df[variable == varname & date <= "2004-12-31" &
                       month(date) %in% c(6:8)]

        brks2 <- NULL
        brks <- l_brks[[varname]]
        origin <- ifelse(varname %in% c("Tair"), 10, 0)
        if (varname == "RH") origin = 60
        cols <- get_color_symmetry(rcolors$amwg256, brks, origin)
        if (varname == "RH") cols %<>% rev()

        param = list(
            value ~ doy + hour | variable, dati,
            panel = function(x, y, z, subscripts, ...) {
                panel.levelplot(x, y, z, subscripts, ...)
            },
            xlab = NULL, ylab = NULL,
            # xlab = "Day of Year (DOY)",
            scales = list(y = list(at = seq(1, 48, 6)), tck = c(1,0)),
            col.regions = cols
        )
        if (!is.null(brks)) {
            param$at = brks
            brks2 = sample_seq(brks, 2)
        }

        p <- do.call(levelplot, param) %>%
            set_strip(cex = 1, height = 1.1, title = titles[i])
        p + theme_lattice(
            font_family = "Times", font_size = 16,
            plot.margin = c(-3, 3, -3, -1),
                          key.margin = c(0, 0.5, 0, 0.2))
            # layer_contourf(at = brks2, col = "black", lty = 1, lwd = 0.2)
    }
    write_fig(plot_grid(plotlist = ps), "FluxMet_DangXiong.pdf", 14, 5, show = T)
}



# dots = list(...)
# DATA = dots$DATA
# at = seq(30, 100, 10) %>% c(-Inf, ., Inf)
# col = get_color("Blues", length(at) - 1)
# panel.levelplot2(x, y, DATA$RH, subscripts,
#                 at = at, labels = at, col = col,
#                 border = col,
#                 contour = TRUE, region = FALSE)
