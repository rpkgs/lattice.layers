
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

# Statistic of median±sd or median
spatial_meansd <- function(x, area, stat, unit, FUN = weightedMedian){
    # mu <- median(x, na.rm = TRUE)
    fmt = "%.1f"
    if (!is.null(stat$digit)) fmt = sprintf("%%.%df", stat$digit)    
    if (is.null(FUN)) FUN = weightedMedian
    
    x[is.infinite(x)] <- NA
    mu <- FUN(x, area, na.rm = TRUE) %>% sprintf(fmt, .)
    # weightedMedian, weightedMean
    sd <- weightedSd(x, area, na.rm = TRUE) %>% sprintf(fmt, .)

    unit2   = unit
    if(!(is.null(unit) || unit == "")) {
        unit2 <- unit
        # unit2 <- sprintf(" (%s)", unit)
    }

    lst.env = c(list(mu=mu, sd=sd, unit = unit2), stat)
    label <- if ( !is.null(stat$include.sd) && stat$include.sd ) {
        eval(substitute(expression(bar(italic(name)) == mu * "±" * sd * " " * unit), lst.env)) # bolditalic
    } else {
        eval(substitute(expression(bar(bold(name)) == mu * " " * unit), lst.env))
    }
    label
}
