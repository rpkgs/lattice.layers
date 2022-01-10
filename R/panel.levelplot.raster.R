#' @importFrom grDevices as.raster
panel.levelplot.raster <- function(x, y, z,
                                   subscripts,
                                   at = pretty(z),
                                   ...,
                                   col.regions = regions$col,
                                   alpha.regions = regions$alpha,
                                   interpolate = FALSE,
                                   identifier = "levelplot") {
    if (length(subscripts) == 0) {
        return()
    }
    regions <- trellis.par.get("regions")
    x <- x[subscripts]
    y <- y[subscripts]
    z <- z[subscripts]

    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)

    if (hasGroupNumber()) {
        group <- list(...)$group.number
    } else {
        group <- 0
    }

    ## create a suitable matrix of colors
    suppressWarnings(l <- pixel2grid(x, y, zcol)) # warning might here
    range = l$range
    # zmat <- rep("transparent", )
    grid.raster(as.raster(l$mat),
        interpolate = interpolate,
        x = range[1], y = range[3],
        width = range[2] - range[1], height = range[4] - range[3],
        just = c("left", "bottom"),
        default.units = "native",
        name = trellis.grobname(paste(identifier, "raster", sep = "."),
            type = "panel", group = group
        )
    )
}

#' @export
pixel2grid <- function(x, y, z, returnId = FALSE) {
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)

    x <- as.numeric(x)
    y <- as.numeric(y)

    if (x.is.factor) {
        ## unique values (we want to keep missing levels in between)
        ux <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE))
        xwid <- 1L
    } else {
        ## sorted unique values of x
        ux <- sort(unique(x[!is.na(x)]))
        ## complain if all ux are not equidistant
        ## if             (length(unique(diff(ux))) != 1) -- too strict
        if (!isTRUE(all.equal(diff(range(diff(ux))), 0))) {
            warning("'x' values are not equispaced; output may be wrong")
        }
        # xwid <- mean(diff(ux))
        xwid <- collapse::fmode(diff(ux))
        ux <- seq(min(ux), max(ux), xwid)
    }
    ## same things for y
    if (y.is.factor) {
        ux <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE))
        ywid <- 1L
    } else {
        uy <- sort(unique(y[!is.na(y)]))
        if (!isTRUE(all.equal(diff(range(diff(uy))), 0))) {
            warning("'y' values are not equispaced; output may be wrong")
        }
        # ywid <- mean(diff(uy))
        ywid <- collapse::fmode(diff(uy))
        uy <- seq(min(uy), max(uy), ywid)
    }

    ncolumns <- length(ux)
    nrows    <- length(uy)
    xlow     <- ux[1] - 0.5 * xwid
    xhigh    <- ux[ncolumns] + 0.5 * xwid
    ylow     <- uy[1] - 0.5 * ywid
    yhigh    <- uy[nrows] + 0.5 * ywid


    idx <- match_tolerance(x, ux)
    idy <- match_tolerance(y, rev(uy)) # image goes top to bottom
    id <- idy + nrows * (idx - 1L)     # `id` might have Na values
    range <- c(xlow, xhigh, ylow, yhigh)
    if (returnId) {
        id
    } else {
        init = ifelse(is.character(z[1]), NA_character_, NA_real_)
        zmat <- matrix(init, nrows, ncolumns)
        ind_valid = !is.na(id)    
        zmat[id[ind_valid]] <- z[ind_valid]
        # zmat[id] <- z
        # dim(zmat) <- c(nrows, ncolumns)
        list(mat = zmat, range = range)
    }
}

match_tolerance <- function(x, y, tolerance = 1e-6) {
    digits = -log10(tolerance)
    match(round(x, digits), round(y, digits))
}
