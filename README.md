
<!-- README.md is generated from README.Rmd. Please edit that file -->

# latticeMap

<!-- badges: start -->
[![R-CMD-check](https://github.com/rpkgs/latticeMap/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/latticeMap/actions)
[![codecov](https://codecov.io/gh/rpkgs/latticeMap/branch/master/graph/badge.svg)](https://codecov.io/gh/rpkgs/latticeMap)
[![CRAN](http://www.r-pkg.org/badges/version/latticeMap)](https://cran.r-project.org/package=latticeMap)<!-- badges: end -->
<!-- badges: end -->

``` r
install.packages("latticeMap")
# covr::package_coverage()
```

``` r
library(latticeMap)
library(Ipaper) # install_githubrpkgs/Ipaper
library(sp)
```

## spplot style

``` r
str(grid_slope)
# Formal class 'SpatialPixelsDataFrame' [package "sp"] with 7 slots
#   ..@ data       :Classes 'data.table' and 'data.frame':  328964 obs. of  2 variables:
#   .. ..$ slope : num [1:328964] -0.012915 -0.000275 -0.000275 -0.000275 -0.000275 ...
#   .. ..$ pvalue: num [1:328964] 0.424 0.944 0.944 0.944 0.944 ...
#   .. ..- attr(*, ".internal.selfref")=<externalptr> 
#   ..@ coords.nrs : num(0) 
#   ..@ grid       :Formal class 'GridTopology' [package "sp"] with 3 slots
#   .. .. ..@ cellcentre.offset: Named num [1:2] -179.9 -89.9
#   .. .. .. ..- attr(*, "names")= chr [1:2] "Var1" "Var2"
#   .. .. ..@ cellsize         : Named num [1:2] 0.25 0.25
#   .. .. .. ..- attr(*, "names")= chr [1:2] "Var1" "Var2"
#   .. .. ..@ cells.dim        : Named int [1:2] 1440 720
#   .. .. .. ..- attr(*, "names")= chr [1:2] "Var1" "Var2"
#   ..@ grid.index : int [1:328964] 1035361 1035362 1035363 1035364 1035365 1035366 1035367 1035368 1035369 1035370 ...
#   ..@ coords     : num [1:328964, 1:2] -180 -180 -179 -179 -179 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:2] "Var1" "Var2"
#   ..@ bbox       : num [1:2, 1:2] -180 -90 180 83.8
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : chr [1:2] "Var1" "Var2"
#   .. .. ..$ : chr [1:2] "min" "max"
#   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#   .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs"
spplot(grid_slope, as.table = TRUE)
```

<img src="man/Figure/sp-1.png" width="100%" />

-   `pars`中`title`控制的是标题位置及标题属性
-   `stat`控制的是计算的统计指标

如果不提供formula, 默认采用的是`sp::spplot`的绘图方法；
如果提供formula，采用的是`lattice::levelplot`的绘图方法。

如果不提供

``` r
pars = list(title = list(x=-180, y=87, cex=1.5))
stat = list(show = TRUE, name="mean", loc = c(-40, -50), digit = 2, include.sd = TRUE)

grid = grid_slope
p <- sp_plot(grid,
             ylim = c(-60, 95),
             yticks = seq(0, 0.3, 0.1),
             pars = pars,
             stat = stat, area.weighted = TRUE
)
p
# Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
# PostScript字体数据库里找不到'rTimes'这个字体系列

# Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
# PostScript字体数据库里找不到'rTimes'这个字体系列

# Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
# PostScript字体数据库里找不到'rTimes'这个字体系列

# Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
# PostScript字体数据库里找不到'rTimes'这个字体系列
```

<img src="man/Figure/sp_plot-1.svg" width="100%" />

``` r
# write_fig(p, "sp_grid.pdf", 7.3, 6)
```

formula的书写方式与`lattice::levelplot`的语法格式一致，只是这里的横轴和纵轴坐标是固定`lon+lat`。以`formula = slope ~ lon + lat | type`为例：`~`前的变量slope是绘图变量z值，
`|`之后的变量type表示group。

`sp_plot`提供了给显著性的格点打patch的功能。需要`df`中含有mask变量，TRUE表示显著。

``` r
set_options(list(style = "EN"))
# levelplot style works
grid2 = grid_slope
df = grid2@data %>% mutate(mask = pvalue <= 0.05)
df = list(a = df, b = df %>% mutate(slope = slope + 0.1)) %>%
    melt_list("type")
p <- sp_plot(grid2, df, #[type == "a"],
             formula = slope ~ lon + lat | type,
             # grid2@data[, .(mask = pvalue <= 0.05)],
             ylim = c(-60, 95),
             xlim = c(-180, 180),
             yticks = seq(0, 0.3, 0.1),
             pars = pars,
             stat = stat,
             layout = c(1, 2),
             # colorkey = F,
             # colorkey = list(space = "bottom"),
             area.weighted = TRUE,
             density = 0.1
) 
p
# Warning in panel.levelplot.raster(x, y, z, subscripts, ..., interpolate =
# interpolate): 'y' values are not equispaced; output may be wrong

# Warning in panel.levelplot.raster(x, y, z, subscripts, ..., interpolate =
# interpolate): 'y' values are not equispaced; output may be wrong
```

<img src="man/Figure/sp_levelplot-1.svg" width="100%" />

``` r
# +  theme_lattice(
#     # key.margin = c(0, 1.5, 0, 0),
    # plot.margin = c(0, 3, -1.5, 1))
# write_fig(p, "sp_levelplot.pdf", 7.3, 6)
```

``` r
library(Ipaper)
set_font()
data("grid_avhrr")
pars = list(title = list(x=77, y=39, cex=1.5),
            hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
stat = list(show = TRUE, name="mean", loc = c(82.5, 38), digit = 1, include.sd = TRUE)

# spplot stype
p <- sp_plot(grid_avhrr,
                 stat = stat,
                 pars = pars,
                 # yticks = seq(0, 0.3, 0.1),
                 interpolate = FALSE)
# Warning in wkt(obj): CRS object has no comment
p
```

<img src="man/Figure/unnamed-chunk-3-1.svg" width="100%" />

``` r
# write_fig(p, "ex-sp_plot1.pdf", 9.8, 5)
```

``` r
# levelplot stype
df = grid_avhrr@data %>% data.frame() %>%
    list(a = ., b = .) %>% melt_list("type")
p2 <- sp_plot(grid_avhrr, df, formula = X1982 ~ lon+lat | type, aspect = 0.5, pars = pars)
p2
```

<img src="man/Figure/unnamed-chunk-4-1.svg" width="100%" />

``` r
# write_fig(p2 , "ex-sp_plot2.pdf", 6, 5)

# show strip
p3 <- sp_plot(grid_avhrr, df, formula = X1982 ~ lon+lat | type,
              strip = TRUE,
              par.settings2 = list(axis.line = list(col = "black")),
              aspect = 0.5, pars = pars)
p3
```

<img src="man/Figure/unnamed-chunk-4-2.svg" width="100%" />

``` r
# write_fig(p3 , "ex-sp_plot3.pdf", 6, 5)
# write_fig(p2, "ex-spplot_grid2.png", 9.8, 5)
```
