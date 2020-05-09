

.options <- list(
    style = c("EN", "CH")[2],
    family = "Times",
    family_CH = c("TimesSimSun", "rTimes")[2], # TimesSimSun, rTimes
    shadePattern = list(col = "black", lwd = 1, lty = 1))

#' set latticeGrob options
#' 
#' @param options
#' - style: font style, one of "EN" and "CH"
#' - family: font family name, e.g. "Times"
#' - family_CH: Chinese fontname for panel.barchartFreq yaxis title
#' 
#' @export
set_options <- function(options) {
    .options %<>% modifyList(options)
}

#' @rdname set_options
#' @export 
get_options <- function(verbose = TRUE) {
    print(str(.options))
    .options
}

# get_family <- function(){
# }
get_family <- function() {
    family <- par("family")
    if (family == "") {
        family <- ifelse(.options$style == "EN", "Times", .options$family_CH)
    }
    family
}
