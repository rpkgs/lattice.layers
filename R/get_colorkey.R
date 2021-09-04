#' pal
#' show colors in figure device
#' 
#' @param col colors to be visualize.
#' @param border rect border for each color
#' 
#' @export
pal <- function(col, border = "light gray")
{
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), 
         axes = FALSE, xlab = "", ylab = "")
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

#' Construct lattice colorkey
#' 
#' @import RColorBrewer
#' @export
get_colorkey <- function(breaks, cols = NULL, space = "bottom", lgd.title = NULL, is_factor = FALSE, 
    unit = NULL, unit.adj = 0.3,  
    cex = 1.4, fontfamily = "Times", fontface = 2)
{
    ncolor <- length(breaks) - 1

    colorkey <- list(
        title = lgd.title,
        labels = list(cex = cex, fontfamily = fontfamily, fontface = fontface),
        axis.line = list(col = 'black'),
        rect = list(col = "black", lwd = 0.4), 
        # tri.upper = TRUE,  tri.lower = TRUE, 
        height = 1, space = space, tck = 1, 
        unit = unit, unit.adj = unit.adj
    )

    ## TESTS
    if (!is_factor) {
        colorkey$labels$at <- breaks[-c(1, length(breaks))]
    }
    if (!is.null(cols)) colorkey$col = cols
    colorkey
}

get_break_colors <- function(cols, brks) {
    nbrk <- length(brks) - 1
    get_color(cols, nbrk)
} 

get_break_colors2 <- function(cols, brks, is_factor = FALSE) {
    nbrk = length(brks)
    ncolor <- ifelse(is_factor, nbrk, nbrk - 1)
    get_color(cols, nbrk)
} 
