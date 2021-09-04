#' @importFrom matrixStats weightedMedian
#' @export
matrixStats::weightedMedian

#' @export
grid::grid.newpage

#' @export
grid::grid.draw

#' @export
grid::grid.rect

#' @export
grid::grid.circle


#' @importFrom lattice levelplot
#' @export
lattice::levelplot

# ' @importFrom rcolors get_color
# # rcolors::get_color

#' @name get_color
#' @title Get and interpolate colors
#' @export
get_color <- function (col, n = NULL, show = FALSE) {
    if (length(col) == 1) {
        temp = rcolors::rcolors[[col]]
        if (!is.null(temp)) col = temp
    }
    
    if (is.null(n)) n = length(col)
    cols = colorRampPalette(col)(n)
    if (show) show_col(cols)
    cols
}

#' @export
#' @rdname get_color
get_color_symmetry <- function(x = "amwg256", at, origin = 0, dark = TRUE) {
    npos <- sum(at > origin)
    nneg <- sum(at < origin)
    n <- max(npos, nneg)
    cols <- get_color(x, 2 * n)
    # return in if statement
    if (npos > nneg) {
        if (!dark) {
            cols[seq(n - nneg + 1, 2 * n)]
        } else {
            c(get_color(cols[1:n], nneg), cols[(n + 1):(2 * n)])
            # cols[c(1:nneg, (n + 1):(2 * n))]
        }
    } else {
        if (!dark) {
            cols[seq(1, n + npos)]
        } else {
            c(cols[1:n], get_color(cols[seq(n+1, 2*n)], npos))
            # cols[c(1:n, (2*n - npos + 1):(2 * n))]
        }
    }
}

#' @export
sample_seq <- function(x, step = 2) x[seq(1, length(x), step)]
