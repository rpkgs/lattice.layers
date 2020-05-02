
<!-- README.md is generated from README.Rmd. Please edit that file -->

# latticeGrob

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/kongdd/latticeGrob.svg?branch=master)](https://travis-ci.com/kongdd/latticeGrob)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/kongdd/latticeGrob?branch=master&svg=true)](https://ci.appveyor.com/project/kongdd/latticeGrob)
[![Codecov test
coverage](https://codecov.io/gh/kongdd/latticeGrob/branch/master/graph/badge.svg)](https://codecov.io/gh/kongdd/latticeGrob?branch=master)
<!-- badges: end -->

The goal of latticeGrob is to …

## Installation

You can install the released version of latticeGrob from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("latticeGrob")

covr::package_coverage()
# latticeGrob Coverage: 59.67%
# R/component_axis.R: 0.00%
# R/latitude_bar.R: 0.00%
# R/panel.annotation.R: 0.00%
# R/panel.barchart.R: 0.00%
# R/tools.R: 17.39%
# R/theme_lattice.R: 37.93%
# R/write_fig.R: 68.12%
# R/draw.colorkey.R: 83.77%
# R/file_name.R: 84.62%
# R/key_triangle.R: 94.27%
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(latticeGrob)
#> 
#> Attaching package: 'latticeGrob'
#> The following object is masked from 'package:datasets':
#> 
#>     volcano
## basic example code
```

## Visualization

  - `draw.colorkey`: modified from lattice, add triangle head and tail
