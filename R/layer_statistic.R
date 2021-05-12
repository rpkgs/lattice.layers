#' layer of spatial statistic label
#' 
#' @inheritParams grid::grid.text
#' @param FUN one of [matrixStats::weightedMedian()], [matrixStats::weightedMean()]
#' @param ... other parameters to [grid.text()]
#' @export
layer_statistic <- function(
    x = 0.1, y = 0.9,
    digit = 2, include.sd = TRUE, 
    name = "u", unit = "", FUN = weightedMedian,
    hjust = 0, vjust = 1, cex = 1,
    # cex = 1.2, adj = c(0, 0),
    ...)
{
    gp = gpar(fontfamily = get_family(), cex = cex)
    dots = listk(digit, include.sd, unit, FUN)
    layer({
        params <- listk(z = z[subscripts]) %>% c(dots)
        label <- do.call(spatial_meansd, params)
        text.params$label = label
        do.call(grid.text, text.params)
        # grid.text(label, dots3)
    }, data = listk(dots,
                    text.params = listk(x, y, hjust, vjust, gp)))
}

#' Statistic of median±sd or median
#' @export
spatial_meansd <- function(z, area = NULL, 
    digit = 2, include.sd = TRUE, 
    name = "u", unit = "", FUN = weightedMedian)
{
    # mu <- median(z, na.rm = TRUE)
    fmt = sprintf("%%.%df", digit)

    z[is.infinite(z)] <- NA
    mu <- FUN(z, area, na.rm = TRUE) %>% sprintf(fmt, .)
    # weightedMedian, weightedMean
    sd <- weightedSd(z, area, na.rm = TRUE) %>% sprintf(fmt, .)

    unit2 = unit
    if(!(is.null(unit) || unit == "")) {
        unit2 <- unit
        # unit2 <- sprintf(" (%s)", unit)
    }

    lst.env = listk(mu, sd, unit = unit2, name)
    label <- if ( include.sd ) {
        eval(substitute(expression(bar(italic(name)) == mu * "±" * sd * " " * unit), lst.env)) # bolditalic
    } else {
        eval(substitute(expression(bar(bold(name)) == mu * " " * unit), lst.env))
    }
    label
}
