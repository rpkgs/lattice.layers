polygon_ind <- function(x, y, ind = NULL, ...) {
    if (!is.null(ind)) {
        x <- x[ind]
        y <- y[ind]
    }
    x <- c(x, rev(x))
    y <- c(y, y*0)
    polygon(x, y, ...)
}

#' draw_polygon
#'
#' @examples
#' set.seed(1)
#' y <- rnorm(10)
#' x <- seq_along(y)
#' @export
draw_polygon <- function(y, x = NULL, length.out = 1e4) {
    if (is.null(x)) x <- seq_along(y)
    xx <- seq(min(x), max(x), length.out = 10000)
    yy <- approx(x, y, xx)$y

    plot(x, y, type = "n")
    # polygon(x = xx, y = clamp_min(yy, 0), col = "red")
    polygon(x = xx %>% c(., rev(.)), y = clamp(yy, c(0, Inf)) %>% c(., .*0), col = alpha("red", alpha = 0.2))
    polygon(x = xx %>% c(., rev(.)), y = clamp(yy, c(-Inf, 0)) %>% c(., .*0), col = "blue")
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
}
