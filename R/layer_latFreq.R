#' layer of latitude frequency barchart
#'
#' @param cex axis text size
#' @param col.regions If is null, it will use the default `col.regions` in the panel.
#' Note: Previous default of `c("blue", "red")` has been deprecated. The latest version
#' get `col.regions` from previous layers.
#' @param ... other parameters to [panel.latFreq()]
#'
#' @export
layer_latFreq <- function(
    bbox = c(0.7, 1, 0, 1),
    unit = "npc",
    length.out = 1e4,
    tcl = 0.4, cex = 1,
    xlab = "", ylab = "",
    xlabels = TRUE, ylabels = TRUE,
    ylim = NULL, zlim = NULL, zlim_ratio = c(-1, 1),
    prob_z = 0.9,
    col.regions = NULL,
    is_spatial = FALSE,
    zticks = NULL,
    digit = 1,
    ...) {
  dots <- mget(ls()) %>% c(...)
  layer(
    {
      params <- listk(x, y, z, subscripts)
      if (is.null(.dots$col.regions)) .dots$col.regions <- col.regions
      params %<>% c(.dots)
      # print(str(params, 1))
      do.call(panel.latFreq, params)
    },
    data = listk(.dots = dots)
  )
}

make_latFreq <- function(
    y, z,
    length.out = 1e4,
    tcl = 0.4, cex = 1,
    xlab = "", ylab = "",
    xlabels = TRUE, ylabels = TRUE,
    ylim = NULL, zlim = NULL, zlim_ratio = c(-1, 1),
    prob_z = 0.9,
    is_spatial = FALSE,
    zticks = NULL,
    digit = 1,
    debug = FALSE,
    ...) {
  # save(list = ls(), file = "data-raw/debug_latFreq.rda")
  family <- get_family()
  if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)

  yaxt <- "s"
  if (is_spatial) {
    yaxt <- "n"
    yticks <- if (diff(range(y)) > 60) seq(-60, 90, 30) else pretty(y)
  }

  if (is.null(zlim)) {
    zmax <- quantile(abs(z), prob_z, na.rm = TRUE)
    zmax <- if (zmax > 0.5) round_decade(zmax) else round(zmax, 1)
    zlim <- zlim_ratio * zmax
  } else {
    zmax <- max(zlim)
  }

  d <- data.table(vals = z, x = y)
  d2 <- d[, .(value = mean(vals, na.rm = TRUE)), .(x)] %>%
    .[x <= ylim[2] & x >= ylim[1]]
  d2[is.na(value), value := 0]

  # g <- as.grob(function() {
  if (!debug) {
    old.par = par(mar = c(0, 0, 0, 0), mgp = c(1, 0, 0), 
      oma = c(0, 0, 0, 0), family = family)
    on.exit(par(old.par))
  }
  draw_polygon(d2$value, d2$x,
    length.out = nrow(d2), type = "vertical",
    tcl = tcl, ...,
    ylim = ylim, zlim = zlim,
    xaxs = "i", yaxs = "i",
    xlab = "", ylab = "",
    xaxt = "n", 
    yaxt = yaxt
  )

  if (is_spatial) {
    at <- seq(-60, 90, 10)
    abline(h = seq(-60, 60, 30), lty = 3, col = "grey", lwd = 0.5)
    if (ylabels) {
      ylabels <- as.character(yticks)
      ylabels[c(1, length(ylabels))] <- " "
    }
    axis(side = 2, tcl = tcl, at = yticks, labels = ylabels, cex.axis = cex) # label_sp(yticks)
    axis(
      side = 2, tcl = tcl / 2, at = seq(-60, 90, 10), labels = rep("", length(at)),
      lwd = 0.5, cex.axis = cex
    )

    if (is.null(zticks)) {
      xticks_major <- c(-1, 0, 1) * zmax
      xticks_minor <- c(-1, 1) * zmax / 2
      axis(
        side = 1, tcl = tcl / 2, at = xticks_minor, labels = rep("", length(xticks_minor)),
        lwd = 0.5, cex.axis = cex
      )
    } else {
      xticks_major <- zticks
    }
    axis(side = 1, tcl = tcl, at = xticks_major, labels = xticks_major, cex.axis = cex)
  }
  # })
}


#' @inheritParams lattice::panel.levelplot
#' @inheritParams panel.annotation
#'
#' @param tcl The length of tick marks as a fraction of the smaller of the width
#' or height of the plotting region. If tck >= 0.5 it is interpreted as a fraction
#' of the relevant side, so if tck = 1 grid lines are drawn. The default setting
#' (tck = NA) is to use tcl = -0.5.
#' @param xlab,ylab the title of xaxis and yaxis.
#' @param zlim the limits of `z` value. If not specified, it's `c(-1, 1)*quantile(abs(z), 0.9)`.
#' @param zlim_ratio If `zlim` not provided, `zlim = zlim_ratio * zmax`
#' @param prob_z default 0.9, the probability of z quantile, which used to determine the zlim.
#'
#' @examples
#' \dontrun{
#' panel.latFreq(x, y, z, subscripts, bbox = c(0.7, 1, 0, 1), unit = "npc")
#' }
#' @rdname layer_latFreq
#' @export
panel.latFreq <- function(
    x, y, z, subscripts,
    bbox = c(0.7, 1, 0, 1),
    unit = "npc",
    length.out = 1e4,
    tcl = 0.4, cex = 1,
    xlab = "", ylab = "",
    xlabels = TRUE, ylabels = TRUE,
    ylim = NULL, zlim = NULL, zlim_ratio = c(-1, 1),
    prob_z = 0.9,
    is_spatial = FALSE, zticks = NULL, digit = 1,
    ...) {
  # ylim = rescale_npc2real(bbox[3:4], v$yscale)
  # xlim = rescale_npc2real(bbox[1:2], v$xscale)
  # bbox <- c(xlim, ylim)
  # unit = "native"
  v <- current.viewport()
  if (is.null(ylim)) ylim <- v$yscale

  g <- as.grob(function() {
    make_latFreq(
      y[subscripts], z[subscripts],
      length.out,
      tcl, cex,
      xlab, ylab, xlabels, ylabels,
      ylim, zlim, zlim_ratio,
      prob_z,
      is_spatial, zticks, digit, ...
    )
  })

  panel.annotation(grid.rect(), bbox, unit)
  panel.annotation(g, bbox, unit, clip = "off")
}

rescale_npc2real <- function(range_npc, range) {
  lm(y ~ x, data.frame(x = c(0, 1), y = range)) %>%
    predict(data.frame(x = range_npc))
}

label_sp <- function(x = seq(-60, 90, 30)) {
  res <- NULL
  for (i in seq_along(x)) {
    val <- x[[i]]
    res[[i]] <- if (val > 0) {
      substitute(expression(x * degree * N), list(x = val))
    } else if (val == 0) {
      substitute(expression(x * degree), list(x = val))
    } else {
      substitute(expression(x * degree * S), list(x = -val))
    }
  }
  do.call(c, res)
}
