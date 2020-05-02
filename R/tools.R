get_family <- function() {
    family <- par("family")
    if (family == "") family <- "Times"
    # fontfamily = "rTimes"
    family
}

#' check_dir
#' @param path character vectors 
#' 
#' @keywords internal
#' @export
check_dir <- function(path){
    for (i in seq_along(path)) {
        path_i <- path[[i]]
        if (!dir.exists(path_i)){
            dir.create(path_i, recursive = TRUE)
        }
    }
    path
}

listk <- function (...) 
{
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) 
        seq_along(cols)
    else which(vars == "")
    if (length(Id_noname) > 0) 
        vars[Id_noname] <- sapply(cols[Id_noname], deparse)
    x <- setNames(list(...), vars)
    return(x)
}

#' help of gpar
#' 
#' @keywords internal
#' @export 
help_gpar <- function() {
    list(col       = "Colour for lines and borders.",
        fill       = "Colour for filling rectangles, polygons, ...",
        alpha      = "Alpha channel for transparency",
        cex        = "Multiplier applied to fontsize",
        lty        = "Line type",
        lwd        = "Line width",
        lex        = "Multiplier applied to line width",
        lineend    = "Line end style (round, butt, square",
        linejoin   = "Line join style (round, mitre, bevel",
        linemitre  = "Line mitre limit (number greater than 1",
        lineheight = "The height of a line as a multiple of the size of text",
        fontsize   = "The size of text (in points",
        fontfamily = "The font family",
        fontface   = "The font face (bold, italic, ...",
        font       = "Font face (alias for fontface; for backward compatibility") %>% str()
}
