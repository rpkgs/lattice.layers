#' lattice arrow style axis
#'
#' @param ticks numeric vector
#' @param labels boolean or character vector with the same length as `ticks`. If
#' `labels = FALSE`, no lables will be displayed.
#' @param angle axis text angle
#' @param origin begin point of axis
#' @param tck tck length in the unit of lines
#' @param type one of "xaxis" and "yaxis"
#'
#' @export
component_axis <- function(ticks, labels = TRUE, origin = 0, tck = 0.4, angle = 0,
    type = c("xaxis", "yaxis")[1])
{
    tck_half = unit(tck/2, "lines")
    tck    = unit(tck, "lines")
    origin = unit(origin, "native")

    tick_max = unit(max(ticks), "native") + unit(0.8, "lines") # 1.2
    tick_min = unit(ticks[1], "native")
    # x1 = max(ticks)
    param_arrow <- listk(
        x0 = tick_min, y0 = origin, x1 = tick_max, y1 = origin,
        col.line = "black", type = "closed", length = unit(0.1, "lines"),
        col = "black", fill = "black", identifier = paste0(type, ".arrow"))

    if (type == "xaxis") {
        do.call(panel.arrows, param_arrow)
        I <- seq(2, length(ticks), 2)
        panel.segments(
            x0 = ticks[I], x1 = ticks[I], y0 = origin, y1 = origin - tck_half,
            identifier = "xaxis.tick.minor")
        if (length(ticks) <= 6) {
            I <- seq(1, length(ticks) - 1)[-1]
        } else {
            I <- seq(1, length(ticks) - 1, 2)[-1]
        }
        panel.segments(
            x0 = ticks[I], x1 = ticks[I], y0 = origin, y1 = origin - tck,
            identifier = paste0(type, ".tick.major"))
    } else {
        # yaxis
        names(param_arrow)[1:4] <- c("y0", "x0", "y1", "x1")
        do.call(panel.arrows, param_arrow)

        I <- seq_along(ticks)
        panel.segments(
            y0 = ticks[I], y1 = ticks[I], x0 = origin, x1 = origin - tck,
            identifier = paste0(type, ".tick.major"))
    }

    family <- get_family()
    if (length(labels) > 1 || labels) {
        if (is.logical(labels)) labels <- ticks
        adj <- c(0.5, 1)
        if (angle == 90)
            adj <- c(1, 0.5)
        if (type == "yaxis") {
            adj = c(1, 0.5)
            title <- if (.options$style == "CH") "频率 (%)" else "Fraction (%)"
            grid.text(x = origin - unit(1.2, "lines") - tck, y = unit(median(ticks), "native"), title,
                gp = gpar(font = 2, fontfamily = family),
                just = c(0, 0.5) %>% rev(),
                rot = 90,
                name = "ylab.title"
            )
            # browser()
            grid.text(
                y = unit(ticks[I], "native"), x = origin - (tck + unit(0.1, "lines")),
                labels[I],
                gp = gpar(fontfamily = .options$family, srt = angle, font = 2),
                just = adj, name = paste0(type, ".text")
            )
        } else {
            grid.text(x = unit(ticks[I], "native"), y = origin - (tck + unit(0.1, "lines")),
                labels[I], rot = angle,
                gp = gpar(fontfamily = .options$family, font = 2),
                just = adj, name = paste0(type, ".text"))
        }
    }
}

grid.text <- function(...) {
    suppressWarnings(grid::grid.text(...))
}
