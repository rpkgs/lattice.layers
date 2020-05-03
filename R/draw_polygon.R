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
#' @param vals numeric vector
#' @param x The corresponding x position of `vals`
#' @param type one of "horizontal" or "vertical"
#' @param length.out the length of interpolated `vals` and `x`
#' @param alpha the alpha of polygon's fill color
#'
#' @examples
#' set.seed(1)
#' y <- rnorm(10)
#' x <- seq_along(y)
#' @export
draw_polygon <- function(vals, x = NULL, type = "horizontal", length.out = 1e4, alpha = 0.6, ...) {
    if (is.null(x)) x <- seq_along(vals)
    xx <- seq(min(x), max(x), length.out = 10000)
    suppressWarnings(yy <- approx(x, vals, xx)$y)

    # polygon(x = xx, y = clamp_min(yy, 0), col = "red")
    col.pos = alpha("red", alpha = alpha)
    col.neg = alpha("blue", alpha = alpha)

    if (type == "horizontal") {
        plot(x, vals, type = "n", ...)
        polygon(x = xx %>% c(., rev(.)), y = clamp(yy, c(0, Inf)) %>% c(., .*0), col = col.pos)
        polygon(x = xx %>% c(., rev(.)), y = clamp(yy, c(-Inf, 0)) %>% c(., .*0), col = col.neg)
    } else {
        plot(vals, x, type = "n", ...)
        polygon(clamp(yy, c(0, Inf)) %>% c(., .*0), xx %>% c(., rev(.)), col = col.pos)
        polygon(clamp(yy, c(-Inf, 0)) %>% c(., .*0), xx %>% c(., rev(.)), col = col.neg)
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
