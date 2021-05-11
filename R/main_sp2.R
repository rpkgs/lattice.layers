
raster2poly <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialPolygonsDataFrame")
}

raster2SpatialGrid <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialGridDataFrame")
}


#' Calculate area of spatial object
#' 
#' @param grid SpatialPolygonsDataFrame or SpatialGridDataFrame
#' @param weighted if not, ones vector will be return
#' 
#' @seealso [raster::area()]
#' @importFrom raster values area
#' @export
sp_area <- function(grid, weighted = TRUE){

    sp_area_grid <- function(grid) {
        grid2 <- grid[, 1] # SpatialGridDataFrame
        grid2@data <- data.frame(id = 1:nrow(grid2))
        r <- raster::raster(grid2)
        I <- values(r) %>% which.notna() # pixel becomes data.frame
        area <- values(area(r))[I]
        area
    }
    
    if (weighted) {
        if (class(grid) == "SpatialPolygonsDataFrame") {
            area <- raster::area(grid)
        } else {
            area <- sp_area_grid(grid)
        }
    } else area <- rep(1, nrow(grid))
    area
}
