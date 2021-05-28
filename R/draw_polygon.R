# polygon_ind <- function(x, y, ind = NULL, ...) {
#     if (!is.null(ind)) {
#         x <- x[ind]
#         y <- y[ind]
#     }
#     x <- c(x, rev(x))
#     y <- c(y, y*0)
#     polygon(x, y, ...)
# }

#' draw horizontal and vertical polygons
#'
#' @inheritParams lattice::panel.levelplot
#' @param vals numeric vector
#' @param x The corresponding x position of `vals`
#' @param type one of "horizontal" or "vertical"
#' @param length.out the length of interpolated `vals` and `x`
#' @param alpha the alpha of polygon's fill color
#' @param zlim limits of `vals`
#' 
#' @examples
#' set.seed(1)
#' y <- rnorm(10)
#' x <- seq_along(y)
#' @importFrom dplyr nth
#' @export
draw_polygon <- function(vals, x = NULL, type = "horizontal", length.out = 1e4,
    col.regions = c("blue", "red"), alpha = 0.6,
    zlim = c(-Inf, Inf),
    ...)
{
    n    <- length(vals)
    vals <- vals %>% c(0, ., 0)
    x    <- x %>% c(.[1], ., .[n])

    if (is.null(x)) x <- seq_along(vals)
    xx <- seq(min(x), max(x), length.out = length.out)
    suppressWarnings(yy <- approx(x, vals, xx)$y)

    I_good <- which(!is.na(yy))
    ind <- first(I_good):last(I_good)
    yy <- yy[ind]
    xx <- xx[ind]

    # polygon(x = xx, y = clamp_min(yy, 0), col = "red")
    col.neg = alpha(nth(col.regions, 2), alpha = alpha)
    col.pos = alpha(nth(col.regions, -2), alpha = alpha)

    params <- listk(type = "n", ...)
    if (all(is.finite(zlim))) params$xlim <- zlim

    xxx = xx %>% c(., rev(.))
    if (type == "horizontal") {
        params %<>% c(list(x, vals), .)
        do.call(plot, params)
        # grid(nx = NULL, ny = NA)
        # {
        #     y <- vals
        #     plot(x, y)
        #     xxx = x %>% c(., rev(.))
        #     polygon(xxx, clamp(y, c(0, zlim[2])) %>% c(., .*0), col = col.pos)
        #     polygon(xxx, clamp(y, c(zlim[1], 0)) %>% c(., .*0), col = col.neg)
        # }
        polygon(xxx, clamp(yy, c(0, zlim[2])) %>% c(., .*0), col = col.pos)
        polygon(xxx, clamp(yy, c(zlim[1], 0)) %>% c(., .*0), col = col.neg)
    } else {
        params %<>% c(list(vals, x), .)
        do.call(plot, params)
        # grid(nx = NULL, ny = NA)
        polygon(clamp(yy, c(0, zlim[2])) %>% c(., .*0), xxx, col = col.pos)
        polygon(clamp(yy, c(zlim[1], 0)) %>% c(., .*0), xxx, col = col.neg)
    }
}

# r <- rle(yy >= 0)
# inds <- r$lengths %>% cumsum() %>% c(0, .)
# I_pos <- which(r$values)
# I_neg <- which(!r$values)

# temp <- purrr::map(I_pos, function(i){
#     ind <- (inds[i]+1):(inds[i+1])
#     polygon_ind(xx, yy, ind, col = "red")
#     ind
# })
# temp <- purrr::map(I_neg, function(i){
#     ind <- (inds[i]+1):(inds[i+1])
#     polygon_ind(xx, yy, ind, col = "blue")
#     ind
# })
