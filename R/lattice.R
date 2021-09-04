#' @export
#' @keywords internal
panel_blank <- function(x, y, z, ...) {}

#' @export
lattice <- function(x, data = NULL, col.regions, ..., panel = panel_blank) {
    levelplot(x, data, col.regions = col.regions, ..., panel = panel)
}
