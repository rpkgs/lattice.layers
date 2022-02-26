#' @name labeller
#' @title labeller
#'
#' @return function(x) sprintf(fmt, x)
#' @export
labeller_num <- function(fmt = "%6.1f") {
    function(x) sprintf(fmt, x)
}

#' imagesc
#' 
#' @importFrom data.table melt
#' @export
imagesc <- function(arr, x = NULL, y = NULL, sp = TRUE,
    cols, brks, 
    xlab = "", ylab = "",
    sp.layout = NULL,
    colorkey = NULL,
    # scales = NULL, 
    labeller = labeller_num(),
    par.settings = NULL, ...)
{
    if (is.null(x)) x <- 1:dim(arr)[1]
    if (is.null(y)) y <- 1:dim(arr)[2]

    colorkey = colorkey %>% updateList(list(labeller = labeller), .)
    colorkey = colorkey %>% updateList(list(labeller = labeller), .)
    if (is.null(par.settings)) par.settings = opt_trellis_default
    
    params <- c(list(...), listk(
        xlab, ylab,
        sp.layout,
        # scales = list(y = list(relation="free")),
        colorkey, par.settings, as.table = TRUE))
    if (!missing(cols)) params$col.region = cols
    if (!missing(brks)) params$at = brks

    d_coord <- expand.grid(x = x, y = y) %>% as.data.table()
    df <- array_3dTo2d(arr) %>% cbind(d_coord)

    if (sp) {
      # SpatialPixelsDataFrame
      sp::coordinates(df) <- ~ x + y
      sp::gridded(df) <- TRUE

      params %<>% c(list(df), .)
      p = do.call(sp::spplot, params) 
      p = p + theme_lattice(
          plot.margin = c(0, 0, -2, 2),
          key.margin = c(0, 2, 0, 0)
      )
    } else {
      df <- df %>% melt(c("x", "y"), variable.name = "g")
      
      params %<>% c(list(value ~ x + y | g, df), .)
      p = do.call(levelplot, params)
      p = p + theme_lattice(
        plot.margin = c(0, 2, -2, 5),
        key.margin = c(0, 2, 0, 0)
      )
    }
    p 
}
