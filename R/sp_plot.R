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
#' @param colorkey Boolean or list returned by [get_colorkey()]. `list` object can
#' be passed to [draw.colorkey()] directly.
#' @param NO_begin beginning NO of the first panel
#'
#' @example R/examples/ex-sp_plot.R
#'
#' @seealso [sp::spplot()], [lattice::levelplot()]
#' @note parameter `panel.title` change to `panel.titles_full`
#' - `panel.titles_full` is for tags.
#' - `strip.factors` is for strip factors
#'
#' @importFrom raster plot
#' @importFrom matrixStats weightedMedian weightedSd
#' @importFrom sp spplot coordinates
#' @importFrom grid frameGrob placeGrob rectGrob segmentsGrob polygonGrob
#' @importFrom lattice panel.number panel.segments panel.points panel.arrows
#' @importFrom data.table as.data.table
#' @export
sp_plot <- function(
    grid,
    df = grid@data,
    zcols,
    formula = NULL,
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
    area.weighted = FALSE,

    colorkey = TRUE,
    key.space = "right",
    key.height = 0.98,
    key.num2factor = FALSE,

    # aspect = 0.5,
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
    if (!is.null(layout)) layout %<>% rev()
    if (missing(zcols)) zcols <- colnames(df)
    if (is.numeric(zcols)) zcols <- colnames(df)[zcols]
    if (!is.data.table(df)) df <- data.table(df)
    df = df %>% select(zcols)

    list.mask = NULL
    if (!is.null(formula)) {
        info.formula <- parse.formula(formula)
        value.vars <- info.formula$value.var
        groups <- info.formula$groups
        # zcols only for one group
        d_grp = NULL
        if (!is_empty(groups)) {
            d_grp = df %>% select(groups) %>% unique()
            if (is.null(panel.titles_full)) {
                panel.titles_full =
                    d_grp[, do.call(paste, c(.SD, list(sep = " ")))] %>% label_tag()
            }
        }
        zcols <- if (length(groups) == 1) {
            vals_unique <- d_grp[[1]]
            levels <- levels(vals_unique)
            levels <- if (is.null(levels)) vals_unique else intersect(levels, vals_unique) # rm no-value levels
            levels
        } else {
            NULL
        }
        FUN = levelplot
    } else {
        value.vars = zcols
        groups = NULL
        FUN = spplot
    }

    # 1. significant patch
    if (is.null(df.mask) && "mask" %in% colnames(df)) df.mask <- df
    if (!is.null(df.mask)) {
        for (i in seq_along(groups)) {
            varname <- groups[i]
            levels <- levels(df[[varname]])
            if (is.null(levels)) levels <- unique(df[[varname]])
            df.mask[[varname]] %<>% factor(levels = levels)
        }
        list.mask <- dlply(df.mask, rev(groups), function(d) d$mask)
    }

    npixel <- nrow(grid)
    par.settings <- modifyList(par.settings, par.settings2)

    # 2. statistic mean value
    grid_area = sp_area(grid, area.weighted)

    vals_1st <- df[[value.vars[1]]]
    is_factor <- is.factor(vals_1st)
    if (missing(colors)) colors <- c("red", "grey80", "blue4")
    if (missing(brks)) {
        brks <- if (!is_factor) {
            range <- quantile(vals_1st, c(0.05, 0.95), na.rm = TRUE)
            vals_1st %<>% clamp(range)
            pretty(vals_1st, n = 10) %>% c(-Inf, ., Inf)
        } else {
            levels(vals_1st)
        }
    } else {
        if (toFactor) {
            for(var in value.vars) df[[value.vars]] %<>% cut(brks)
        }
        levels <- cut(1, brks) %>% levels()
        grid@data <- df
    }

    cols <- get_break_colors2(colors, brks, is_factor)
    class <- class(grid)
    data <- coordinates(grid) %>% as.data.table() %>%
        set_colnames(c("lon", "lat")) %>% cbind(df)

    if (strip == TRUE) {
        n <- length(zcols)
        if (is.null(strip.factors)) strip.factors <- zcols
        # names <- if (is.null(strip.factors)) zcols else strip.factors
        strip_levels <- label_tag(strip.factors)
        strip <- strip.custom(factor.levels = strip_levels)
    }

    params <- listk(
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
        # aspect            = aspect,

        xlab              = NULL,
        ylab              = NULL,
        interpolate       = interpolate,
        par.settings      = par.settings,

        scales            = list(draw = FALSE),
        pars              = pars,
        class             = class
    )

    params = if (is.null(formula)) {
        list(grid, zcols) %>% c(params)
    } else {
        list(formula, data, list.mask = list.mask, SpatialPixel = grid) %>% c(params)
    }

    if (!is.null(xlim)) params$xlim <- xlim
    if (!is.null(ylim)) params$ylim <- ylim

    nbrk <- length(brks)
    if (!is_factor) params$at <- brks #else seq(0.5, nbrk + 1)

    if (is.list(colorkey) || colorkey) {
        is_factor2 <- key.num2factor || is_factor
        colorkey.param <- get_colorkey(brks, NULL, key.space, lgd.title,
                                       is_factor2, cex = cex.lgd)
        colorkey.param$unit <- unit
        colorkey.param$unit.adj <- unit.adj
        colorkey.param$height = key.height

        if (is.list(colorkey)) colorkey.param %<>% updateList(colorkey)
        if (is_factor) colorkey.param$at <- seq_len(length(colorkey.param$labels$labels) + 1) - 0.5
        params$colorkey <- colorkey.param
    } else {
        params$colorkey <- FALSE
    }
    do.call(FUN, params)
    # +
    #     theme_lattice(
    #         key.margin = c(0, 1.5, 0, 0),
    #         plot.margin = c(0, 3, -1.5, 1)
    #     )
}

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


#' Calculate area of spatial object
#'
#' @param grid SpatialPolygonsDataFrame or SpatialGridDataFrame
#' @param weighted if not, ones vector will be return
#'
#' @seealso [raster::area()]
#' @importFrom raster values area
#' @export
sp_area <- function(grid, weighted = TRUE) {
    sp_area_grid <- function(grid) {
        grid2 <- grid[, 1] # SpatialGridDataFrame
        grid2@data <- data.frame(id = 1:nrow(grid2))
        r <- raster::raster(grid2)
        I <- values(r) %>% which.notna() # pixel becomes data.frame
        area <- values(area(r))[I]
        area
    }

    if (weighted) {
        if (class(grid) == "SpatialPolygonsDataFrame") {
            area <- raster::area(grid)
        } else {
            area <- sp_area_grid(grid)
        }
    } else {
        area <- rep(1, nrow(grid))
    }
    area
}
