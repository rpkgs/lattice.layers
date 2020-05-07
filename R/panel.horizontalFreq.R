#' Panel of horizontal and vertical polygons
#'
#' @inheritParams lattice::panel.levelplot
#' @inheritParams panel.annotation
#' 
#' @param tck The length of tick marks as a fraction of the smaller of the width
#' or height of the plotting region. If tck >= 0.5 it is interpreted as a fraction
#' of the relevant side, so if tck = 1 grid lines are drawn. The default setting
#' (tck = NA) is to use tcl = -0.5.
#' @param xlab,ylab the title of xaxis and yaxis.
#' @param zlim the limits of `z` value. If not specified, it's `c(-1, 1)*quantile(abs(z), 0.9)`.
#' 
#' @examples
#' \dontrun{
#' panel.horizontalFreq(x, y, z, subscripts, bbox = c(0.7, 1, 0, 1), unit = "npc")
#' }
#' @export
panel.horizontalFreq <- function(x, y, z, subscripts, 
    bbox = c(0.7, 1, 0, 1), unit = "npc", 
    length.out = 1e4, ...,
    tcl = 0.4, xlab = "", ylab = "", 
    zlim = NULL, 
    zticks = NULL,
    is_spatial = FALSE,
    xlabels = NULL, ylabels = NULL, digit = 1)
{
    family <- get_family()
    g <- as.grob(function() {
        yaxt = "s"
        if (is_spatial) {
            yaxt = "n"
            yticks = if (diff(range(y)) > 60) seq(-60, 90, 30) else pretty(y[subscripts])
        }
        
        if (is.null(zlim)) {
            zmax <- z[subscripts] %>% abs %>% quantile(0.90, na.rm = TRUE) 
            zmax <- if (zmax > 0.5) round(zmax) else round(zmax, 1)
            zlim <- c(-1, 1)*zmax
        } else zmax = max(zlim)
        
        par(mar = c(0, 0, 0, 0), mgp = c(1, 0, 0), oma = c(0, 0, 0, 0), family = family)
        draw_polygon(z[subscripts], y[subscripts], length.out = 1e4, type = "vertical",
                     tcl = tcl,
                     ...,
                     zlim = zlim, 
                     xaxs = "i", yaxs = "i",
                     xlab = "", ylab = "",
                     xaxt = "n", yaxt = yaxt)
        if (is_spatial) {
            at = seq(-60, 90, 10)
            abline(h = seq(-60, 90, 30), lty = 3, col = "grey", lwd  = 0.5)
            axis(side = 2, tcl = tcl, at = yticks, labels = yticks) # label_sp(yticks)
            axis(side = 2, tcl = tcl/2, at = seq(-60, 90, 10), labels = rep("", length(at)), lwd = 0.5)
            # browser()
            if (is.null(zticks)) {
                xticks_major = c(-1, 0, 1)*zmax
                xticks_minor = c(-1, 1)*zmax/2
                axis(side = 1, tcl = tcl, at = xticks_major, labels = xticks_major) # label_sp(yticks)
                axis(side = 1, tcl = tcl/2, at = xticks_minor, labels = rep("", length(xticks_minor)), lwd = 0.5)
            } else {
                xticks_major = zticks
                axis(side = 1, tcl = tcl, at = xticks_major, labels = xticks_major) # label_sp(yticks)
            }
        }
        # usr <- par('usr')
        # axis(side = 2, tck = tck)
    })
    panel.annotation(grid.rect(), bbox, unit)
    panel.annotation(g, bbox, unit, clip = "off")
}

# if (is_spatial) labels <- label_sp(ticks)
# 
label_sp <- function(x = seq(-60, 90, 30)) {
    res <- NULL
    for(i in seq_along(x)) {
        val <- x[[i]]
        res[[i]] <- if (val > 0) {
            substitute(expression(x*degree*N), list(x = val))
        } else if (val == 0) {
            substitute(expression(x*degree), list(x = val))
        } else {
            substitute(expression(x*degree*S), list(x = -val))
        }
    }
    do.call(c, res)
}

