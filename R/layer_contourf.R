#' layer of contourf
#' 
#' @inheritParams lattice::panel.levelplot
#' @inheritParams base::sprintf
#' @param ... other parameters to [lattice::panel.levelplot()]
#' 
#' @seealso [lattice::panel.levelplot()]
#' 
#' @example R/examples/ex-layer_contourf.R
#' @export
layer_contourf <- function(at = NULL,
    col = NULL, col.regions = NULL, 
    contour = TRUE, region = FALSE,
    labels = FALSE,
    label.style = c("mixed", "flat", "align"),
    format = "%f", ...)
{
    dots_pa = mget(ls()) %>% c(...)

    layer({
        env = parent.frame(n = 2) # parent Environment
        dots_pa$at %<>% option_null_default(at)
        nbrk <- length(dots_pa$at) - 1
        cols_default = get_color("amwg256", nbrk)
        dots_pa$col %<>% option_null_default(env$col) %>%
            option_null_default(cols_default)

        dots_pa$col.regions %<>% option_null_default(env$col.regions) %>% 
            option_null_default(cols_default)

        nbrk = length(dots_pa$at)
        dots_pa$col %<>% get_color(nbrk)
        dots_pa$col.regions %<>% get_color(nbrk)

        if (isTRUE(dots_pa$labels)) 
            dots_pa$labels = sprintf(dots_pa$format, dots_pa$at)

        param = listk(x, y, z, subscripts) %>% c(dots_pa)
        do.call(panel.levelplot2, param)
    }, data = listk(dots_pa))
}

#' @keywords internal
#' @export
option_null_default <- function(x, default = NULL) {
    if (is.null(x)) x = default
    x
}
