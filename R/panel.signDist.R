#' @importFrom stars st_as_stars
#' @importFrom sf st_as_sf as_Spatial
#' @rdname panel.signPerc
#' @export
panel.signDist <- function(list.mask, SpatialPixel, par.shade = NULL, density = 1, angle = 45, ...) {
    NO_panel <- panel.number()
    if (!is.null(list.mask) && !is.null(SpatialPixel)) {
        mask <- list.mask[[NO_panel]]
        I_sign <- which(mask)

        if (length(I_sign) > 0) {
            grid <- SpatialPixel[I_sign, ]
            grid@data <- data.frame(mask = rep(TRUE, length(grid)))

            poly_shade <- st_as_stars(grid) %>%
                st_as_sf(as_points = FALSE, merge = TRUE) %>%
                as_Spatial()
            # poly_shade = st_as_sf(as_SpatialGridDataFrame(grid), as_points = FALSE, merge = TRUE) %>% as_Spatial()
            # poly_shade <- raster2poly(grid)
            params <- listk(union = FALSE, density, angle, sp.layout = NULL) %>%
                c(., list(...))
            do.call(panel.poly_grid, c(list(poly_shade), params))
        }
    }
}
