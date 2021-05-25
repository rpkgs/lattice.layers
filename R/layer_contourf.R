#' layer of contourf
#' 
#' @param ... other parameters ignored
#' @export
layer_contourf <- function(brks = NULL, ...)
{
    # dots = mget(ls()) #%>% c(...)
    layer({
        if (is.null(brks)) brks = at
        panel.contourplot(x, y, z, subscripts,
            at = brks, col.regions = col.regions,
            contour = TRUE, region = FALSE
        )
        # params <- listk(x, y, z, subscripts)
        # params %<>% c(dots2)
        # do.call(panel.latFreq, params)
    }, data = listk(brks))
}
