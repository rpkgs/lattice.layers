#' Plot methods for spatial data with attributes
#'
#' Lattice (trellis) plot methods for spatial data with attributes
#'
#' @inheritParams lattice::levelplot
#' @inheritParams sp::spplot
#'
#' @param brks either a numeric vector of two or more unique cut points to convert
#' values in df into factor.
#' @param colors corresponding colors of every break interval
#' @param xlim,ylim The limits of x and y
#' @param pars parameters controlling the position of histogram and panel titles,
#' e.g. `list(title = list(x=77, y=39, cex=1.5), hist = list(origin.x=77, origin.y=28))`.
#'
#' @param panel.titles_full string or expression vectors of panel titles, default NULL.
#' If provided, group names in `df` will be overwritten.
#' @param formula a formula of the form z ~ s1 + s2 | g1 * g2 * ..., where z is a
#' numeric response, and x, y are numeric values evaluated on a rectangular grid.
#' g1, g2, ... are optional conditional variables, and must be either factors or
#' shingles if present.
#' @param df data.table object, with columns e.g. lon, lat, and others
#' @param df.mask NULL or same length data.table as df, with the columns of `mask`
#' and same group variabes as `df`.
#' `mask` is a boolean vector, which is used to distinguish significant pixels.
#' Note that factor levels should be same for grouped variables in `df` and `df.mask`.
#' @param SpatialPixel corresponding SpatialPixel object of `df`
#'
#' If `mask` present in `df`, `df.mask` will be ignored.
#' @param colorkey Boolean or list returned by [get_colorkey()].
#' @param NO_begin beginning NO of the first panel
#'
#' @example R/examples/ex-spplot_grid.R
#'
#' @seealso [spplot_grid()], [sp::spplot()], [lattice::levelplot()]
#' @note parameter `panel.title` change to `panel.titles_full`
#' - `panel.titles_full` is for tags.
#' - `strip.factors` is for strip factors
#'
#' @importFrom matrixStats weightedMedian weightedSd
#' @importFrom sp spplot coordinates
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob polygonGrob
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#' @importFrom data.table as.data.table
#' @export
levelplot2 <- function(
    formula,
    df,
    SpatialPixel,
    df.mask = NULL,

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
#
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
    info.formula <- parse.formula(formula)
    value.var <- info.formula$value.var
    groups <- info.formula$groups

    if (!is.data.table(df)) df = data.table(df)
    # zcols only for one group
    d_grp = NULL
    if (length(groups) > 0) {
        d_grp = df %>% select(groups) %>% unique()
        if (is.null(panel.titles_full)) {
            panel.titles_full = d_grp[, do.call(paste, c(.SD, list(sep = " ")))] %>%
                label_tag()
        }
    }

    zcols <- if (length(groups) == 1) {
        vals_unique <- d_grp[[1]]
        levels <- levels(vals_unique)
        levels <- if (is.null(levels)) vals_unique else  intersect(levels, vals_unique) # rm no-value levels
    } else {
        NULL
    }

    npixel <- nrow(SpatialPixel)
    par.settings <- modifyList(par.settings, par.settings2)

    if (is.null(df.mask) && "mask" %in% colnames(df)) df.mask <- df
    
    list.mask <- NULL
    labels_sign <- NULL
    if (!is.null(df.mask)) {
        ## make sure factor is the same
        for (i in seq_along(groups)) {
            varname <- groups[i]
            levels <- levels(df[[varname]])
            if (is.null(levels)) levels <- unique(df[[varname]])
            df.mask[[varname]] %<>% factor(levels = levels)
        }
        list.mask <- dlply(df.mask, rev(groups), function(d) d$mask)
    }

    # statistic mean value
    data.stat <-
        if (stat$show && !is.null(stat$loc)) {
            area <- sp_area(SpatialPixel, area.weighted)
            # need to debug for two variables group
            labels <- dlply(df, rev(groups), function(d) {
                  spatial_meansd(d[[value.var]], area, stat, unit, FUN = stat$FUN)
              })
            list(loc = stat$loc, label = labels)
        } else NULL

    is_factor <- is.factor(df[[value.var]])
    if (missing(colors)) colors <- c("red", "grey80", "blue4")

    if (missing(brks)) {
        if (!is_factor) {
            vals <- df[[value.var]]
            range <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
            vals %<>% clamp(range)
            brks <- pretty(vals, n = 10) %>% c(-Inf, ., Inf)
        } else {
            brks <- levels(df[[value.var]])
        }
        cols <- get_break_colors2(colors, brks, is_factor)
    } else {
        cols <- get_break_colors2(colors, brks, is_factor)
        if (toFactor) df[[value.var]] %<>% cut(brks) # cut into factor
        levels <- cut(1, brks) %>% levels()
    }

    class <- class(SpatialPixel)
    data <- coordinates(SpatialPixel) %>%
        as.data.table() %>%
        cbind(df)

    if (strip == TRUE) {
        n <- length(zcols)
        if (is.null(strip.factors)) strip.factors <- zcols
        # names <- if (is.null(strip.factors)) zcols else strip.factors
        strip_levels <- label_tag(strip.factors)
        strip <- strip.custom(factor.levels = strip_levels)
        zcols <- NULL
    }

    params <- list(
        formula, data,
        list.mask = list.mask,
        SpatialPixel = SpatialPixel,
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

    do.call(levelplot, params)
}
