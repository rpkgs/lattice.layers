#' Panel of horizontal and vertical polygons
#'
#' @inheritParams lattice::panel.levelplot
#'
#' @param tck The length of tick marks as a fraction of the smaller of the width
#' or height of the plotting region. If tck >= 0.5 it is interpreted as a fraction
#' of the relevant side, so if tck = 1 grid lines are drawn. The default setting
#' (tck = NA) is to use tcl = -0.5.
#' @param xlab,ylab the title of xaxis and yaxis.
#'
#' @export
panel.horizontalFreq <- function(x, y, z, subscripts, ...,
    tck = 0.02, xlab = "", ylab = "")
{
    g <- as.grob(function() {
        # tck = 0
        par(mar = c(0, 0, 0, 0), mgp = c(-2, -1, 0), oma = c(0, 0, 0, 0))
        draw_polygon(z, y, length.out = 1e4, type = "vertical", tck = tck,
                     xaxs = "i", yaxs = "i",
                     xlab = xlab, ylab = ylab
                     # xaxt = "n", yaxt = "n"
                     )
        # usr <- par('usr')
        axis(side = 2, tck = tck)
    })
    panel.annotation(grid.rect(), bbox)
    panel.annotation(g, bbox, clip = "on")
}

#' @export
#' @rdname panel.horizontalFreq
panel.verticalFreq <- function(x, y, z, bbox = c(0.5, 1, 0, 1), subscripts, ...,
    tcl = 0.4, xlab = "", ylab = "",
    xlabels = NULL, ylabels = NULL, digit = 1)
{
    family <- get_family()
    g <- as.grob(function() {
        par(mar = c(0, 0, 0, 0), mgp = c(-2, -1 - tcl-0.2, 0), oma = c(0, 0, 0, 0), family = family)
        draw_polygon(z[subscripts], y[subscripts], length.out = 1e4, type = "vertical",
                     tcl = tcl,
                     xaxs = "i", yaxs = "i",
                     xlab = "", ylab = "",
                     xaxt = "n", yaxt = "n"
                     )
        xticks = pretty(z[subscripts])
        yticks = pretty(y[subscripts])
        axis(side = 1, tcl = tcl, at = xticks, labels = xticks)
        axis(side = 2, tcl = tcl, at = yticks, labels = yticks)
    })
    panel.annotation(grid.rect(), bbox)
    panel.annotation(g, bbox, clip = "on")
    browser()
}
