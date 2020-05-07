#' panel.spatial
#' 
#' @inheritParams lattice::panel.levelplot
#' @inheritParams levelplot2
#' @param par position of title, `list(title = list(x, y))`
#' @param list.mask NULL or list of masks. `mask` is a boolean vector, `TRUE` 
#' indicates significant.
#' 
#' @importFrom lattice panel.number panel.text 
#' @importFrom sp sppanel panel.gridplot panel.polygonsplot
#' @importFrom grid viewport popViewport grid.layout rasterGrob nullGrob
#' @export
panel.spatial <- function(x, y, z, subscripts, 
    ...,  
    contour = FALSE, 
    sp.layout, 
    pars, 
    class = NULL, 
    interpolate = TRUE, 
    list.mask = NULL, 
    SpatialPixel = NULL,
    data.stat = NULL)
{
    NO_panel = panel.number()
    fontfamily = get_family()
    dot <- list(...)

    panel.spatialBasic(x, y, z, subscripts, class, contour, interpolate, ..., sp.layout = sp.layout)
    # mask lat <= -60
    panel.annotation(grid.rect(gp = gpar(col = "transparent", fill = "white")), 
        bbox = c(-180, 180, -75, -60), "native", clip = "off")
    
    # 2. stands out significant part
    # density = 1, angle = 45
    panel.signDist(list.mask, SpatialPixel, ...) # spatial polygon pattern
    panel.signPerc(z, subscripts, mask = list.mask[[NO_panel]], xpos = 0.03, ypos = 0.67, ...)

    ## 4. add panel title
    panel.text2(pars$title$x, pars$title$y, dot$panel.titles_full, dot$panel.titles, 
        NO_begin, ...)

    v <- current.viewport()
    xlim <- v$xscale
    bbox <- c(190, max(xlim), -60, 90)
    # panel.annotation(grid.rect(), bbox = bbox, "native")
    panel.horizontalFreq(x, y, z, subscripts, bbox = bbox + c(0, -10, 0, 0), "native", 
        # xlim = c(-1, 1)*5, 
        ylim = bbox[3:4], 
        col.regions = dot$col.regions, length.out = 1e3, is_spatial = TRUE)

    ## 5. panel.text statistic values
    if (!is.null(data.stat)) {
        loc   <- data.stat$loc # 81.5, 26.5
        label <- data.stat$label[[NO_panel]]
        panel.text(loc[[1]], loc[[2]], label, fontfamily = fontfamily, cex = 1.2, adj = c(0, 0))
    }

    ## 7. panel.hist
    panel.barchartFreq(z, subscripts, bbox = c(0.04, 0.26, 0.15, 0.65), ..., 
        yticks = seq(0, 0.6, 0.2), yscale = c(0, 100))
}

#' @rdname panel.spatial
#' @export 
panel.spatialBasic <- function(x, y, z, subscripts, class, contour, interpolate, 
    ..., sp.layout = NULL) 
{
    sppanel(list(sp.layout), panel.number(), first = TRUE)
    if (is.null(class) || class %in% c("SpatialPixelsDataFrame", "SpatialGridDataFrame")) {
        panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = interpolate)  
        # panel.levelplot.raster, panel.levelplot
    } else {
        panel.polygonsplot(x, y, z, subscripts, ..., sp.layout = sp.layout)    
    }
    if (contour) {
        panel.levelplot(x, y, z, subscripts, 
            region = TRUE, contour = TRUE, labels = TRUE, interpolate = FALSE)  
    }
    sppanel(list(sp.layout), panel.number(), first = FALSE)
}

#' @export
dev_off <- function() {
    tryCatch({
        while(TRUE) {
            print("hello")
            dev.off()    
        }
    }, error = function(e) {
        return(invisible())
        # message(sprintf('%s', e))
    })
}
