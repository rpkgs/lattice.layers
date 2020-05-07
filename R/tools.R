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

alpha <- function (colour, alpha = NA) {
    if (length(colour) != length(alpha)) {
        if (length(colour) > 1 && length(alpha) > 1) {
            stop("Only one of colour and alpha can be vectorised")
        }
        if (length(colour) > 1) {
            alpha <- rep(alpha, length.out = length(colour))
        }
        else {
            colour <- rep(colour, length.out = length(alpha))
        }
    }
    rgb <- farver::decode_colour(colour, alpha = TRUE)
    rgb[!is.na(alpha), 4] <- alpha[!is.na(alpha)]
    farver::encode_colour(rgb, rgb[, 4])
}

clamp <- function(x, lims = c(0, 1), fill.na = FALSE){
    if (fill.na) {
        x[x < lims[1]] <- NA_real_
        x[x > lims[2]] <- NA_real_
    } else {
        x[x < lims[1]] <- lims[1]
        x[x > lims[2]] <- lims[2]
    }
    x
}

# for levelplot2
parse.formula <- function(formula = x~s1+s2) {
    str_formula <- gsub("s1 \\+ s2 *\\|*| ", "", as.character(formula))
    value.var = str_formula[2]
    groups    = strsplit(str_formula[3], "\\+|\\*")[[1]]
    list(value.var = value.var, groups = groups)    
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

#' fprintf
#' Print sprintf result into console, just like C style fprintf function
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param ... other parameters will be passed to `sprintf`
#'
#' @examples
#' cat(fprintf("%s\n", "Hello phenofit!"))
#' @export
fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))
