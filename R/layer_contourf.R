#' layer of contourf
#'
#' @inheritParams lattice::panel.levelplot
#' @inheritParams base::sprintf
#' @param ... other parameters to [lattice::panel.levelplot()]
#'
#' @seealso [lattice::panel.levelplot()]
#'
#' @note
#' - the parameter `brks` was renamed as `at`
#'
#' @example R/examples/ex-layer_contourf.R
#' @export
layer_contourf <- function(at = NULL,
                           col = NULL, col.regions = NULL,
                           contour = TRUE, region = FALSE,
                           labels = FALSE,
                           label.style = c("mixed", "flat", "align"),
                           format = "%f", ...) {
  dots <- mget(ls()) %>% c(...)

  layer({
    env <- parent.frame(n = 2) # parent Environment
    dots$at %<>% `%||%`(at)
    nbrk <- length(dots$at) - 1
    cols_default <- get_color("amwg256", nbrk)
    dots$col %<>% `%||%`(env$col) %>%
      `%||%`(cols_default)

    dots$col.regions %<>% `%||%`(env$col.regions) %>%
      `%||%`(cols_default)

    nbrk <- length(dots$at)
    dots$col %<>% get_color(nbrk)
    dots$col.regions %<>% get_color(nbrk)

    if (isTRUE(dots$labels)) {
      dots$labels <- sprintf(dots$format, dots$at)
    }

    param <- listk(x, y, z, subscripts) %>% c(dots)
    do.call(panel.levelplot2, param)
  }, data = listk(dots))
}
