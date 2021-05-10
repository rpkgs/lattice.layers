

.options <- list2env(list(
    style = c("EN", "CH")[2],
    family = "Times",
    family_CH = c("TimesSimSun", "rTimes")[2], # TimesSimSun, rTimes
    shadePattern = list(col = "black", lty = 1, lwd = 1) # 
))

#' set latticeGrob options
#' 
#' @param options
#' - style: font style, one of "EN" and "CH"
#' - family: font family name, e.g. "Times"
#' - family_CH: Chinese fontname for panel.barchartFreq yaxis title
#' 
#' @export
set_options <- function(options) {
    modifyList(.options, options)
    invisible()
}

#' @rdname set_options
#' @export 
get_options <- function(verbose = TRUE) {
    print(str(.options))
    as.list(.options)
}

get_family <- function() {
    family <- par("family")
    if (family == "") {
        family <- ifelse(.options$style == "EN", "Times", .options$family_CH)
    }
    family
}

modifyList <- function(x, val, keep.null = FALSE) {
    # stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[nzchar(vnames)]
    if (keep.null) {
        for (v in vnames) {
            x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) {
                  list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
              } else {
                val[v]
            }
        }
    } else {
        for (v in vnames) {
            x[[v]] <- if (v %in% xnames && is.list(x[[v]]) &&
                is.list(val[[v]])) {
                  modifyList(x[[v]], val[[v]], keep.null = keep.null)
              } else {
                val[[v]]
            }
        }
    }
    x
}
