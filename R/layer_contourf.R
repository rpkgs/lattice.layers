#' layer of contourf
#' 
#' @inheritParams lattice::panel.levelplot
#' @inheritParams base::sprintf
#' @param ... other parameters to [lattice::panel.levelplot()]
#' 
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

        dots_pa$col %<>% option_null_default(env$col) %>%
            option_null_default(get_color_at(at = dots_pa$at))

        dots_pa$col.regions %<>% option_null_default(env$col.regions) %>% 
            option_null_default(get_color_at(at = dots_pa$at))

        nbrk = length(dots_pa$at)
        dots_pa$col %<>% get_color(nbrk)
        dots_pa$col.regions %<>% get_color(nbrk)

        if (isTRUE(dots_pa$labels)) 
            dots_pa$labels = sprintf(dots_pa$format, dots_pa$at)

        param = listk(x, y, z, subscripts) %>% c(dots_pa)
        do.call(panel.levelplot2, param)
    }, data = listk(dots_pa))
}

option_null_default <- function(x, default = NULL) {
    if (is.null(x)) x = default
    x
}

#' @import rcolors
get_color_at <- function(x = "amwg256", at) {
    get_color(x, length(at) - 1)
}
