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
#' @export
draw_polygon <- function(vals, x = NULL, type = "horizontal", length.out = 1e4, 
    col.regions = c("blue", "red"), alpha = 0.6, 
    zlim = c(-Inf, Inf),
    ...) 
{
    if (is.null(x)) x <- seq_along(vals)
    xx <- seq(min(x), max(x), length.out = length.out)
    suppressWarnings(yy <- approx(x, vals, xx)$y)
    
    I_good <- which(!is.na(yy))
    ind <- first(I_good):last(I_good)
    yy <- yy[ind]
    xx <- xx[ind]

    # polygon(x = xx, y = clamp_min(yy, 0), col = "red")
    col.neg = alpha(col.regions[1], alpha = alpha)
    col.pos = alpha(last(col.regions), alpha = alpha)
    
    params <- listk(type = "n", ...)
    if (all(is.finite(zlim))) params$xlim <- zlim
    
    if (type == "horizontal") {
        params %<>% c(list(x, vals), .)
        do.call(plot, params)
        # grid(nx = NULL, ny = NA)
        polygon(x = xx %>% c(., rev(.)), y = clamp(yy, c(0, zlim[2])) %>% c(., .*0), col = col.pos)
        polygon(x = xx %>% c(., rev(.)), y = clamp(yy, c(zlim[1], 0)) %>% c(., .*0), col = col.neg)
    } else {
        params %<>% c(list(vals, x), .)
        do.call(plot, params)
        # grid(nx = NULL, ny = NA)
        # d <- data.table(yy, xx)
        # a <- clamp(yy, c(0, Inf)) #%>% c(., 0, .*0)
        polygon(clamp(yy, c(0, zlim[2])) %>% c(., .*0), xx %>% c(., rev(.)), col = col.pos)
        polygon(clamp(yy, c(zlim[1], 0)) %>% c(., .*0), xx %>% c(., rev(.)), col = col.neg)
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
