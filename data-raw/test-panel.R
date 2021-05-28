library(lattice)
{
    panel <- function(x, y, z, subscripts, at, ...) {
        panel.levelplot(x, y, z, subscripts, ...)
        panel.annotation(grid.circle(draw = FALSE))
        panel.annotation(grid.rect(), bbox = c(0.7, 1, 0, 1))
        # panel.annotation(~{
        #     grid_plot(d$y, d$value)
        # }, c(0.7, 1, 0, 1))
        panel.annotation(~{
            # par(family = family)
            par(mar = c(0, 0, 0, 0), mgp = c(0, -1, 0), oma = c(0, 0, 0, 0))
            # plot(y~value, d)
            plot(rep(1, 151), -60:90, tck = 0.01, xlab = "", ylab = "", yaxs = "i")
            grid()
        }, c(0.7, 1, 0, 1))
        # browser()
        panel.barchart2(z, subscripts, at = at, ...)
        panel.sign(z)
    }
    # data <- data.table(x, y, r)
    d <- data[, .(value = mean(r)), .(y)]
    p <- levelplot(z ~ x * y, grid, col.regions = topo.colors(10),
              panel = panel,
              at = c(-Inf, seq(-0.8, 0.8, by = 0.2), Inf))
    print(p)
        # grid.edit("label", gp = gpar(fontfamily = family), global = TRUE, grep = TRUE)
    # }, "a.pdf", use.cairo_pdf = TRUE)
    # grid.edit(gPath = c("xa", "tick"), y = unit(-1, "lines"))
}

# write_fig({
# par(family = family)
# trellis.par.set(list(axis.text=set_font,
#                      par.strip.text=set_font,
#                      par.xlab.text = set_font,
#                      par.ylab.text = set_font))

family <- "Times"
set_font <- list(fontfamily=family)
trellis.par.set(list(axis.text=set_font,
                     par.strip.text=set_font,
                     par.xlab.text = set_font,
                     par.ylab.text = set_font))

# grid.edit("label_perc", gp = gpar(fontface = 2), grep = TRUE, global = TRUE)

{
    d <- data.table(x = 1:10,y = 1:10)
    panel = function(x, y, ...){
        panel.xyplot(x, y, ...)
        component_axis(ticks = 2:10, origin = 1)
        component_axis(ticks = 1:10, origin = 2, type = "yaxis")
    }
    xyplot(x~y, d,
           panel = panel) %>% print()
}

