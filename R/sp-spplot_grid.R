check_brks <- function(brks){
    nbrk  <- length(brks)
    delta <- median(diff(brks))
    if (is.infinite(brks[1])) {
        brks[1] <- brks[2] - delta
    }

    if (is.infinite(brks[nbrk])) {
        brks[nbrk] <- brks[nbrk - 1] + delta
    }
    brks
}

# xlim <- c(73.5049, 104.9725)
# ylim <- c(25.99376, 40.12632)

#' Plot methods for spatial data with attributes
#'
#' Lattice (trellis) plot methods for spatial data with attributes
#'
#' @inheritParams levelplot2
#' @inheritParams sp::spplot
#' @param ... other parameters to spplot, for example:
#' - xlim
#' - ylim
#' - ...
#'
#' @example R/examples/ex-spplot_grid.R
#'
#' @seealso [sp::spplot()], [lattice::levelplot()]
#'
#' @importFrom matrixStats weightedMedian weightedSd
#' @importFrom sp spplot
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob polygonGrob
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#'
#' @importFrom raster plot
#' @export
spplot_grid <- function(
    grid, zcols,

    brks, colors,
    strip = FALSE,
    strip.factors = NULL,
    toFactor = FALSE,

    panel.titles_full = NULL,
    panel = panel.spatial,
    xlim = NULL, ylim = NULL,
    unit = "", unit.adj = 0.3,

    pars = NULL,
    stat = list(show = FALSE, name = "RC", loc = c(81.5, 26.5), digit = 1,
        include.sd = FALSE, FUN = weightedMedian),
    area.weighted = FALSE,
    legend.space = "right",
    legend.num2factor = FALSE,

    colorkey = TRUE,
    interpolate = FALSE,
    lgd.title = NULL,
    sp.layout = NULL,
    layout = NULL,
    NO_begin = 1,
    cex.lgd = 1.3,
    par.settings = opt_trellis_default,
    par.settings2 = list(axis.line = list(col = "transparent")),
    ...)
{
    par.settings %<>% modifyList(par.settings2)

    if (missing(zcols)) zcols <- names(grid)
    if (is.numeric(zcols)) zcols <- names(grid)[zcols] 
    zcols %<>% intersect(names(grid@data))
    df <- grid@data %>% select(zcols) #[, zcols, drop = FALSE]

    if (missing(colors)) colors <- c("red", "grey80", "blue4")
    is_factor <- is.factor(df[[1]])
    
    # statistic mean value
    data.stat <-
        if (stat$show && !is.null(stat$loc)) {
            area = sp_area(grid, area.weighted)
            labels <- df %>% lapply(spatial_meansd, area, stat, unit)
            list(loc = stat$loc, label = labels)
        } else NULL

    if (missing(brks)) {
        vals <- df[[1]]
        range <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
        vals %<>% clamp(range)
        brks <- pretty(vals, n = 10) %>% c(-Inf, ., Inf)
        cols <- get_break_colors(colors, brks)
    } else {
        cols <- get_break_colors(colors, brks)
        if (toFactor) {
            # drawkey can't support factor well
            df <- lapply(df, cut, brks) %>% as.data.frame()
        }
        levels <- cut(1, brks) %>% levels()
        grid@data <- df
    }
    
    class  <- class(grid)
    params <- list(
        grid, zcols,
        ...,
        col.regions       = cols,
        panel.titles      = zcols,
        panel.titles_full = panel.titles_full,
        panel             = panel,
        NO_begin          = NO_begin,
        brks              = brks,
        strip             = strip,
        as.table          = TRUE,
        sp.layout         = sp.layout,
        layout            = layout,

        xlab              = NULL,
        ylab              = NULL,
        interpolate       = interpolate,
        par.settings      = par.settings,

        scales            = list(draw = FALSE),
        pars              = pars,
        data.stat         = data.stat,
        class             = class
    )
    if (!is.null(xlim)) params$xlim <- xlim
    if (!is.null(ylim)) params$ylim <- ylim

    nbrk <- length(brks)
    params$at <- if (!is_factor) brks else seq(0.5, nbrk + 1)
    
    if (is.list(colorkey) || colorkey) {
        is_factor2 <- legend.num2factor || is_factor
        colorkey.param <- get_colorkey(brks, NULL, legend.space, lgd.title, is_factor2, cex = cex.lgd)
        colorkey.param$unit <- unit
        colorkey.param$unit.adj <- unit.adj
    
        if (is.list(colorkey)) colorkey.param %<>% updateList(colorkey)
        params$colorkey <- colorkey.param
    } else {
        params$colorkey <- FALSE
    }
    do.call(spplot, params)
}
