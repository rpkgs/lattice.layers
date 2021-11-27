#' @keywords internal
#' @import magrittr
#' @importFrom stats median approx setNames quantile lm predict
#' @importFrom utils str assignInNamespace
#' @importFrom grDevices cairo_pdf jpeg dev.off svg tiff colorRampPalette xy.coords contourLines
#' @importFrom graphics par polygon abline axis rect
#' @importFrom methods as
#' @importFrom data.table data.table is.data.table `:=`
#' @importFrom Ipaper write_fig melt_list listk is_empty which.notna
#' @importFrom plyr dlply
#' @importFrom dplyr select first last
#' @import rcolors
#' 
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
    if (getRversion() >= "2.15.1") {
        utils::globalVariables(
            c(
                "x", "y", "z", "subscripts", "dots", "dots2", "at", "col.regions",
                "vals", "value",
                ".", ".SD", ".N", "..vars"
            )
        )
    }

    sysname <- tolower(Sys.info()[["sysname"]])
    # if ("windows" %in% sysname) 
    init_lattice()
    # set_font()
    invisible()
}


asign_func <- function(func_old, func_new) {
    name <- deparse(substitute(func_new))
    pkg <- str_extract(name, ".*(?=:)") %>% gsub(":", "", .)
    funcname <- str_extract(name, "(?<=:).*") %>% gsub(":", "", .)

    temp <- func_new
    cmd1 <- sprintf("environment(%s) <- environment(%s:::%s)", name, pkg, funcname)
    cmd2 <- sprintf('assignInNamespace("%s", %s, ns="%s")', funcname, name, pkg)
    eval(parse(text = cmd1))
    eval(parse(text = cmd2))
}


#' initialize the new lattice functions in arrangeGrob 
#' @keywords internal
#' @export 
init_lattice <- function() {
    # lattice.layers:::`+.trellis`
    # environment(latticeExtra:::`+.trellis`)
    suppressWarnings({
        eval(parse(text = 'environment(draw.colorkey) <- environment(lattice::xyplot)'))
        eval(parse(text = 'assignInNamespace("draw.colorkey", draw.colorkey, ns="lattice")'))
    })
    # asign_func(draw.colorkey, lattice::draw.colorkey)
    # invisible()
}

# suppressWarnings({
#     environment(draw.colorkey) <- environment(lattice::xyplot)
#     assignInNamespace("draw.colorkey", draw.colorkey, ns="lattice")
# })
