#' @keywords internal
#' @import magrittr
#' @importFrom stats median approx setNames
#' @importFrom utils str assignInNamespace
#' @importFrom grDevices cairo_pdf jpeg dev.off svg tiff colorRampPalette xy.coords
#' @importFrom graphics par polygon 
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
    # suppressMessages
    # suppressWarnings
    # suppressMessages({
    #     library(magrittr)
    #     # library(lattice)
    #     library(devtools)
    # })
    # init_lattice()
    # set_font()
    invisible()
}
