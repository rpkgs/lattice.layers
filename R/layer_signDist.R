#' layer_signDist
#' 
#' @param density A small (big) value corresponds to a low (high) contour density.
#' 
#' @export
layer_signDist <- function(
    col = "black", lty = 1, lwd = 1, 
    density = 1, angle = 45, ...) 
{
    dots <- mget(ls())
    layer(
        {
            ij <- panel.number()
            mask <- parent.frame(n = 2)$list.mask[[ij]]
            SpatialPixel = parent.frame(n = 2)$SpatialPixel

            params <- listk(mask, SpatialPixel) %>% c(dots)
            do.call(panel.signDist, params)
            # grid.text(label, dots3)
        }, 
        data = listk(dots = dots)
    )
}

#' @importFrom stars st_as_stars
#' @importFrom sf st_as_sf as_Spatial
#' @importFrom raster raster
#' @rdname layer_signDist
#' @export
panel.signDist <- function(mask, SpatialPixel, 
    col = "black", lty = 1, lwd = 1, 
    density = 1, angle = 45, ...) 
{
    if (!is.null(mask) && !is.null(SpatialPixel)) {
        I_sign <- which(mask)

        if (length(I_sign) > 0) {
            grid <- SpatialPixel[I_sign, ]
            grid@data <- data.frame(mask = rep(TRUE, length(grid)))
            # weird bug, st_as_stars works for raster, but not for `SpatialGridDataFrame`
            # or `SpatialPixelsDataFrame` (20210929)
            r = raster(grid)
            poly_shade <- st_as_stars(r) %>%
                st_as_sf(as_points = FALSE, merge = TRUE) %>%
                as_Spatial()
            # save(poly_shade, file = "poly_shade.rda")
            # poly_shade = st_as_sf(as_SpatialGridDataFrame(grid), as_points = FALSE, merge = TRUE) %>% as_Spatial()
            # poly_shade <- raster2poly(grid)
            params <- listk(density, angle, sp.layout = NULL) %>%
                c(., list(...))
            do.call(panel.poly_grid, c(list(poly_shade), params))
        }
    }
}

raster2poly <- function(r, I_grid = NULL) {
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialPolygonsDataFrame")
}

# raster2SpatialGrid <- function(r, I_grid = NULL){
#     if (is.null(I_grid)) I_grid <- 1:nrow(r)
#     as(r, "SpatialGridDataFrame")
# }
