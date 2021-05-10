#' panel.patch
#'
#' @inheritParams graphics::polygon
#' @param ...
#' - `panel.patch`     : other parameters to `polygon.fullhatch`;
#' - `panel.patch.list`: other parameters to [panel.patch()]
#'
#' @details
#' shadePattern can be set by `set_options(list(col, lty, lwd))`
#' @author XiHui Gu
#'
#' @example R/examples/ex-panel.patch.R
#'
#' @seealso [graphics::polygon]
#' @export
panel.patch <- function(
    x, y = NULL,
    density = NULL, angle = 45, border = NULL,
    col = NA, lty = par("lty"), fill = NA, fillOddEven = FALSE, ...)
{
    ..debug.hatch <- FALSE
    xy <- xy.coords(x, y, setLab = FALSE)
    if (is.numeric(density) && all(is.na(density) | density < 0))
        density <- NULL
    if (!is.null(angle) && !is.null(density)) {

        if (missing(col) || is.null(col) || is.na(col))
            col <- par("fg")
        if (is.null(border))
            border <- col
        if (is.logical(border)) {
            if (!is.na(border) && border)
                border <- col else border <- NA
        }
        start <- 1
        ends <- c(seq_along(xy$x)[is.na(xy$x) | is.na(xy$y)],
            length(xy$x) + 1)
        num.polygons <- length(ends)
        col <- rep_len(col, num.polygons)
        if (length(border))
            border <- rep_len(border, num.polygons)
        if (length(lty))
            lty <- rep_len(lty, num.polygons)
        if (length(density))
            density <- rep_len(density, num.polygons)
        angle <- rep_len(angle, num.polygons)
        i <- 1L
        for (end in ends) {
            if (end > start) {
                if (is.null(density) || is.na(density[i]) || density[i] < 0)
                    .External.graphics(C_polygon,
                        xy$x[start:(end - 1)],
                        xy$y[start:(end - 1)],
                        col[i], NA, lty[i], ...)
                else if (density[i] > 0) {
                    polygon.fullhatch(
                        xy$x[start:(end - 1)],
                        xy$y[start:(end - 1)],
                        col = col[i], lty = lty[i], density = density[i],
                        angle = angle[i], ..debug.hatch = ..debug.hatch, fillOddEven, ...)
                }
                i <- i + 1
            }
            start <- end + 1
        }
        # .External.graphics(C_polygon, xy$x, xy$y, NA, border, lty,
        # ...)
    } else {
        if (is.logical(border)) {
            if (!is.na(border) && border)
                border <- par("fg") else border <- NA
        }
        # .External.graphics(C_polygon, xy$x, xy$y, col, border, lty,
        # ...)
    }
    invisible()
}

#' @param lst List of coordinates matrix `[x, y]`
#' @rdname panel.patch
#' @export
panel.patch.list <- function(lst, ...) {
    for (x in lst) panel.patch(x[, 1], x[, 2], ...)
}

#' @rdname panel.patch
#' @export
panel.poly_grid <- function(s,
    density = 0.1, angle = 45,
    # col = "black", lwd = 1, lty = 1,
    border = NULL, ...)
{
    par.shade <- .options$shadePattern
    lst.lonlat  <- coord.polygons(s)
    params <- listk(lst = lst.lonlat, density, angle, border, ...) %>%
        c(par.shade)
    do.call(panel.patch.list, params)
}

#' @rdname panel.patch
#' @export
panel.polygonsplot2 <- function (
    x, y, z, subscripts, ...,
    density = 0.2, angle = 45, col = "grey65",lwd = 0.5, lty = 1,
    poly_shade, sp.layout)
{
    # sppanel(list(sp.layout), panel.number(), first = TRUE)
    # panel.gridplot, panel.polygonsplot
    panel.polygonsplot(x, y, z, subscripts, ...,
                       col = col, lwd = lwd, lty = lty, sp.layout = sp.layout)
    panel.poly_grid(poly_shade, density, angle,
                    # col = col, lwd = lwd, lty = lty,
                    sp.layout = NULL, ...)
    # sppanel(list(sp.layout), panel.number(), first = FALSE)
}

#' @rdname panel.patch
#' @export
panel.gridplot2 <- function (
    x, y, z, subscripts, ...,
    density = 0.2, angle = 45, col = "grey65", lwd = 0.5, lty = 1,
    poly_shade, sp.layout)
{
    # sppanel(list(sp.layout), panel.number(), first = TRUE)
    # panel.gridplot, panel.polygonsplot
    panel.gridplot(x, y, z, subscripts, ...,
        col = col, lwd = lwd, lty = lty, sp.layout = sp.layout)
    panel.poly_grid(poly_shade, density, angle,
        # col = col, lwd = lwd, lty = lty,
        sp.layout = NULL, ...)
    # sppanel(list(sp.layout), panel.number(), first = FALSE)
}
