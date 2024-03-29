#' Get coordinates of spatialPolygons object
#' 
#' @param x Polygons class
#' @export
coord.polygon <- function(x){
    res = list()
    xs  = x@Polygons
    for(i in seq_along(xs)) {
        res[[i]] = xs[[i]]@coords
    }
    do.call(rbind, res)
}

#' coord.polygons
#' 
#' @param x spatialPolygons object
#' 
#' @return `lst_loc`
#' 
#' @rdname coord.polygon
#' @export
coord.polygons <- function(x) {
    polys <- x@polygons
    lapply(polys, coord.polygon)
}

#' @export
union_polygon <- function(x){
  message("union_polygon: please rewrite this function with sf")
  # maptools::unionSpatialPolygons(x, rep(1, length(x)))
}
