#' convertTri
#'
#' Setting 'open.lower' and 'open.upper' to non-zero makes colorkey end with
#' triangular extensions, indicating open-ended intervals. Set to non-zero by
#' default only if first/last intervals are unbounded (-Inf / +Inf).
#' (NOTE: default should perhaps be 0 for back-compatibility, but currently
#' these are simply not shown in the legend, so probably new behaviour is no
#' worse).
#' When non-zero, controls fraction of key$height to be used for triangles at ends.
#'
#' @param x height of triangle. If x in the range of `[0, 0.25]`, `height` will
#' be ignored.
#' @param inf boolean
#' @param height height of triangle
#'
#' @keywords internal
#' @export
convertTri <- function(x, inf = FALSE, height = 0.05)
{
    if (length(x) == 1) {
        if (is.numeric(x) && (x >= 0 && x <= 0.25)) {
            return(x)
        } else if (is.na(x)) {
            # return(0.05)
            return(height*inf)
        } else if (isTRUE(x)) {
            return(height)
        } else {
            return(0)
        }
    }
    warning("Invalid value of 'tri.upper/tri.lower' ignored.")
    0
}

#' fix non-equispaced colorkey
#'
#' @description Not processed if `key$equispaced` is not true.
#'
#' @param key A `colorkey` object (list), at least with the element of `at`.
#'
#' @example R/examples/ex-draw.colorkey_equispaced.R
#'
#' @keywords internal
#' @export
equispaced_colorkey <- function(key) {
    if (!isTRUE(key$equispaced)) return(key) # not processed

    at <- key$at
    is_equispaced <- length(unique(diff(at[is.finite(at)]))) == 1
    if (!is_equispaced) {
        key$at <- seq_along(at)
        labels_at <- seq_along(at)
        labels <- at
        if (first(at) == -Inf) {
            key$at[1] <- -Inf
            labels_at <- labels_at[-1]
            labels <- labels[-1]
        }
        if (last(at) == Inf) {
            key$at[length(key$at)] <- Inf
            n <- length(labels_at)
            labels_at <- labels_at[-n]
            labels <- labels[-n]
        }
        names = setdiff(names(key$labels), c("at", "labels"))
        key$labels <- list(at = labels_at, labels = labels) %>%
            c(key$labels[names])
    }
    key
}

#' @keywords internal
#' @export
process.colorkey <- function(
    col = regions$col,
    alpha = regions$alpha,
    at,
    pretty = FALSE, equispaced = TRUE, format = "%f",
    tick.number = 7,
    tck = 1,
    width = 2,
    height = 1,
    space = "right",
    raster = FALSE,
    interpolate = FALSE,
    tri.upper = NA,
    tri.lower = NA,
    title = NULL,
    unit = NULL,
    unit.adj = 0.3,
    cex.title = 1,
    axis.line = list(),
    axis.text = list(),
    key.padding = c(0, 0),
    rect = list(col = "black", lwd = 0.3), # rect of legend
    ...)
{
    regions <- trellis.par.get("regions")
    listk(
        col, alpha, at,
        tick.number, tck,
        width, height,
        space,
        raster,
        interpolate,
        pretty, equispaced, format,
        tri.upper, tri.lower,
        unit, unit.adj,
        title, cex.title,
        axis.line, axis.text,
        key.padding,
        rect,
        ...
    )
}

# Note: there are two 'at'-s here, one is key$at, which specifies
# the breakpoints of the rectangles, and the other is key$lab$at
# (optional) which is the positions of the ticks. We will use the
# 'at' variable for the latter, 'atrange' for the range of the
# former, and key$at explicitly when needed

#' draw.colorkey
#'
#' @inheritParams lattice::draw.colorkey
#'
#' @example R/examples/ex-draw.colorkey.R
#' @import lattice
#' @export
draw.colorkey <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    key <- do.call(process.colorkey, key)
    key %<>% equispaced_colorkey()

    axis.line <- updateList(trellis.par.get("axis.line"), key$axis.line)
    axis.text <- updateList(trellis.par.get("axis.text"), key$axis.text)

    key$axis.line <- axis.line
    # layout_name <- ifelse(key$space %in% c("top", "bottom"), "layout.heights", "layout.widths")
    # colorkey.title.padding   <- lattice.options()[[layout_name]]$colorkey.title.padding
    # colorkey.title.padding$x <- colorkey.title.padding$x *
    #     trellis.par.get(layout_name)$colorkey.title.padding
    ## made FALSE later if labels explicitly specified
    check.overlap <- TRUE

    # Getting the locations/dimensions/centers of the rectangles
    key$at <- sort(key$at) ## should check if ordered
    numcol <- length(key$at)-1
    #     numcol.r <- length(key$col)
    #     key$col <-
    #         if (is.function(key$col)) key$col(numcol)
    #         else if (numcol.r <= numcol) rep(key$col, length.out = numcol)
    #         else key$col[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]
    key$col <- level.colors(x = seq_len(numcol) - 0.5,
                     at = seq_len(numcol + 1) - 1,
                     col.regions = key$col,
                     colors = TRUE)

    ## FIXME: need to handle DateTime classes properly
    atrange <- range(key$at, finite = TRUE)
    scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)

    if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 0)))
        warning("'at' values are not equispaced; output may be wrong")
    # browser()

    ## recnum <- length(scat)-1
    reccentre <- (scat[-1] + scat[-length(scat)]) / 2
    recdim <- diff(scat)

    cex  <- axis.text$cex
    col  <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface   <- axis.text$fontface
    lineheight <- axis.text$lineheight
    rot <- 0

    # The following code assumes names key$lab and key$lab$lab (which
    # may have been used in user code), whereas documentation says
    # key$labels and key$labels$labels.  To make both work without
    # 'partial matching' warnings, we rename key$labels to key$lab
    # etc.
    if (!is.null(key[["labels"]])) {
        key[["lab"]] <- key[["labels"]]
        key[["labels"]] <- NULL
        if (is.list(key[["lab"]]) && !is.null(key[["lab"]][["labels"]])) {
            key[["lab"]][["lab"]] <- key[["lab"]][["labels"]]
            key[["lab"]][["labels"]] <- NULL
        }
    }

    lab = key$lab
    if (is.null(lab)) {
        if (key$pretty) {
            at <- lpretty(atrange, key$tick.number)
            at <- at[at >= atrange[1] & at <= atrange[2]]
        } else {
            # scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)
            at <- as.numeric(key$at)
        }
        labels <- format(at, trim = TRUE)
    } else if (is.characterOrExpression(lab) && length(lab)==length(key$at)) {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    } else if (is.list(key$lab)) {
        at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
        } else format(at, trim = TRUE)
        if (!is.null(key$lab$cex))  cex  <- key$lab$cex
        if (!is.null(key$lab$col))  col  <- key$lab$col
        if (!is.null(key$lab$rot))  rot  <- key$lab$rot
        if (!is.null(key$lab$font)) font <- key$lab$font
        if (!is.null(key$lab$fontface))   fontface   <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) fontfamily <- key$lab$fontfamily
        if (!is.null(key$lab$lineheight)) lineheight <- key$lab$lineheight
    } else stop("malformed colorkey")

    labscat <- at
    do.labels <- (length(labscat) > 0)

    ## Tri
    height.Tri <- key$height/numcol
    open.lower <- convertTri(key$tri.lower, scat[1] == -Inf, height = height.Tri)
    open.upper <- convertTri(key$tri.upper, scat[length(scat)] == Inf, height.Tri)
    key.rect   <- 1 - open.lower - open.upper

    # legend
    just = switch(key$space,
        right  = if (rot == -90) c("center", "bottom") else c("left", "center"),
        left   = if (rot == 90) c("center", "bottom") else c("right", "center"),
        top    = if (rot == 0) c("center","bottom") else c("left", "center"),
        bottom = if (rot == 0) c("center", "top") else c("right", "center"))

    # add unit label, 20190924
    if (!(is.null(key$unit) || key$unit == "")){
        nlab <- length(labels)
        delta <- labscat[nlab] - labscat[nlab - 1]
        labscat[nlab+1] <- labscat[nlab] + delta*key$unit.adj
        labels[nlab+1]  <- sprintf(" %s", key$unit)
    }

    if (key$space %in% c('right', 'left')) {
        vp_label <- viewport(yscale = atrange)
        x_lab = rep(0, length(labscat))
        y_lab = labscat
    } else {
        vp_label <- viewport(xscale = atrange)
        y_lab = rep(0, length(labscat))
        x_lab = labscat
    }

    labelsGrob <-
        if (do.labels)
            textGrob(label = labels,
                     x = x_lab, y = y_lab, vp = vp_label,
                     default.units = "native",
                     check.overlap = check.overlap,
                     just = just, rot = rot,
                     name = trellis.grobname("labels", type="colorkey"),
                     gp = gpar(col = col, cex = cex,
                              fontfamily = fontfamily,
                              fontface = chooseFace(fontface, font),
                              lineheight = lineheight))
        else nullGrob()

    # layout
    grobwidth <- ifelse(key$space %in% c("top", "bottom"), "grobheight", "grobwidth")
    width_lab <- do.labels/length(labels)
    # For bottom and top, `lgd_width` is height
    widths.x    <- c(0.6*key$width, do.labels*(0.3 + key$tck*0.3), width_lab)
    widths_unit <- c("lines", "lines", grobwidth)
    widths_data <- list(NULL, NULL, labelsGrob)

    lgd_width   <- unit(widths.x, widths_unit, data = widths_data) # for 'right' and 'bottom'
    if (key$space %in% c('left', 'top')) lgd_width <- rev(lgd_width)

    heights.x <- c(0.5*(1 - key$height) + key$key.padding[1],
                 key$height*c(open.upper, key.rect, open.lower),
                 0.5*(1 - key$height) + key$key.padding[2])

    lgd_height <- unit(heights.x, rep("null", 5))

    if (key$space %in% c("right", "left")) {
        key.layout <- grid.layout(nrow = 5, ncol = 3, respect = TRUE,
                        heights = lgd_height,
                        widths = lgd_width)
    } else if (key$space %in% c("top", "bottom")) {
        key.layout <- grid.layout(nrow = 3, ncol = 5, respect = TRUE,
                        heights = lgd_width,
                        widths  = lgd_height)
    }

    key.gf <- key_gf(key, key.layout, vp, vp_label, reccentre, recdim, FALSE)
    key.gf <- key_triangle(key.gf, key, open.lower, open.upper)

    key.gf <- key_border(key.gf, key, open.lower, open.upper)
    key.gf <- key_label(key.gf, key, labscat, labelsGrob, vp_label)

    if (draw) {
        grid.newpage()
        grid.draw(key.gf)
    }
    key.gf
}

#' @export
#' @rdname draw.colorkey
draw.colorkey2 <- draw.colorkey

updateList <- function(x, val)
{
    if (is.null(x)) x <- list()
    modifyList(x, val)
}

is.characterOrExpression <- function(x){
    is.character(x) || is.expression(x) || is.call(x) || is.symbol(x)
}

lpretty <- function(x, ...){
    eps <- 1e-10
    at <- pretty(x[is.finite(x)], ...)
    ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
}

chooseFace <- function(fontface = NULL, font = 1) {
    if (is.null(fontface)) font else fontface
}
