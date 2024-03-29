#' layer_annotation
#' 
#' @inheritParams grid::viewport
#' @param grob grob object
#' @param bbox The region to plot `grob`, `[xmin, xmax, ymin, ymax]` in the unit
#' of npc.
#' @param unit A character vector specifying the units for the corresponding numeric values.
#' @param ... ignored
#'
#' @export
layer_annotation <- function(
    grob, bbox = c(0.5, 1, 0, 1), 
    unit = "npc",
    xscale = FALSE,
    yscale = FALSE,
    clip = "on", ...)
{
    dots_pa = mget(ls()) %>% c(...)

    layer({
        do.call(panel.annotation, dots_pa)
    }, data = listk(dots_pa))
}

#' @inherit grid::unit details
#' @rdname layer_latFreq
#' @export
panel.annotation <- function (grob, bbox = c(0.5, 1, 0, 1), 
    unit = "npc",
    xscale = FALSE,
    yscale = FALSE,
    clip = "on",
    ...)
{
    width <- diff(bbox[1:2])
    height <- diff(bbox[3:4])
    x <- {bbox[1] + width/2} %>% unit(unit)
    y <- {bbox[3] + height/2} %>% unit(unit)
    width %<>% unit(unit)
    height %<>% unit(unit)

    v <- current.viewport()
    ylim <- v$yscale
    xlim <- v$xscale

    xxscale = set_xscale(xscale, xlim)
    yyscale = set_xscale(yscale, ylim)
    # browser()
    # print(yyscale)
    pushViewport(viewport(x, y, width, height, name = "panel.annotation",
        xscale = xxscale, yscale = yyscale, clip = clip))
    grid.draw2(grob)
    popViewport()
}

set_xscale <- function(xscale = FALSE, lims) {
    # xscale can't be unit
    xxscale <- c(0, 1)
    if (is.logical(xscale) && xscale) xxscale <- lims
    if (is.numeric(xscale)) xxscale <- xscale
    xxscale
}

panel_scales <- function() {
    v <- current.viewport()
    ylim <- v$yscale
    xlim <- v$xscale
    v
}

#' convert plot into grob and draw grob
#'
#' @param p one of the supported figure
#' @keywords internal
#'
#' @import grid
#' @import ggplotify
#' @export
grid.draw2 <- function(p) {
    suppressWarnings({
        g <- as.grob(p)
        grid.draw(g)
    })
}

# grid_plot <- function(x, y) {
#     # grid.newpage()
#     pushViewport(plotViewport(c(4, 4, 2, 2))) # begin from bottom
#     pushViewport(dataViewport(x, y,
#                               name="plotRegion"))
#     range_x <- range(x, na.rm = TRUE)
#     xticks = x %>% pretty()
#     xticks <- xticks[xticks >= range_x[1] & xticks <= range_x[2]]

#     grid.rect()
#     grid.xaxis(name = "xa", at = xticks)
#     grid.edit("xa::ticks", y1 = unit(0.5, "lines")) #  + unit(0.5, "lines")
#     # grid.xaxis(name = "xa")
#     grid.yaxis()
#     grid.points(x, y, name="dataSymbols")
#     grid.text("temperature", y=unit(-3, "lines"))
#     grid.text("pressure", x=unit(-3, "lines"), rot=90)

#     # upViewport()
#     upViewport(2)
#     # popViewport()
# }

# trace2 <- function(func) {
#     eval(substitute(trace(func, edit = TRUE)), list(func = func))
# }

# untrace2 <- function(func) {
#     eval(substitute(untrace(func)), list(func = func))
# }
