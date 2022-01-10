
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lattice.layers

<!-- badges: start -->

[![R-CMD-check](https://github.com/rpkgs/lattice.layers/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/lattice.layers/actions)
[![codecov](https://codecov.io/gh/rpkgs/lattice.layers/branch/master/graph/badge.svg)](https://codecov.io/gh/rpkgs/lattice.layers)
[![CRAN](http://www.r-pkg.org/badges/version/lattice.layers)](https://cran.r-project.org/package=lattice.layers)<!-- badges: end -->
<!-- badges: end -->

``` r
install.packages("lattice.layers")
# covr::package_coverage()
```

``` r
library(lattice.layers)
# Registered S3 method overwritten by 'lattice.layers':
#   method    from        
#   +.trellis latticeExtra
# 
# 载入程辑包：'lattice.layers'
# The following object is masked from 'package:Ipaper':
# 
#     dev_off
library(Ipaper) # install_githubrpkgs/Ipaper
library(sp)
```

`sp_plot`两种绘图语法：

-   如果不提供formula, 采用的是`sp::spplot`的绘图方法；
-   如果提供formula，采用的是`lattice::levelplot`的绘图方法。

> formula的书写方式与`lattice::levelplot`的语法格式一致，只是这里的横轴和纵轴坐标是固定`lon+lat`。以`formula = slope ~ lon + lat | type`为例：`~`前的变量slope是绘图变量z值，
> `|`之后的变量type表示group。

`sp_plot`提供了给显著性的格点打patch的功能。需要`df`中含有mask变量，TRUE表示显著。

## spplot style

``` r
grid2 = grid_slope
df = grid2@data %>% mutate(mask = pvalue <= 0.05)
df = list(a = df, b = df %>% mutate(slope = slope + 0.1)) %>%
    melt_list("type")

p <- sp_plot(grid2, 
             df, df.mask = df[, .(type, mask)],
             formula = slope ~ lon + lat | type,
             ylim = c(-60, 95), xlim = c(-180, 180) + c(-1, 1)*10,
             layout = c(2, 1),
             colorkey = list(space = "bottom"),
             density = 0.1) + 
    layer_signDist(density  = 0.1, lwd = 0.2) +  
    layer_signPerc(x = 0.01, y = 0.72, fill = "white") +
    layer_barchart(y = 0.12) +  
    theme_lattice(
        key.margin = c(1, 1, 1, 1),
        plot.margin = c(0, 1, 2, 1))
p
# Warning in pixel2grid(x, y, zcol): 'y' values are not equispaced; output may be
# wrong

# Warning in pixel2grid(x, y, zcol): 'y' values are not equispaced; output may be
# wrong
```

<img src="man/Figure/sp_levelplot-1.svg" width="100%" />

``` r
# write_fig(p, "a.pdf", 7, 6)
```
