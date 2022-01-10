#' @export
lattice <- function(x, data = NULL, col.regions, ..., panel = panel_blank) {
    levelplot(x, data, col.regions = col.regions, ..., panel = panel)
}

#' @importFrom latticeExtra layer
layer <- layer

#' @export
layers <- function(...) {
    Reduce(latticeExtra:::`+.trellis`, list(...))
}

#' @export
#' @keywords internal
panel_blank <- function(x, y, z, ...) {}
